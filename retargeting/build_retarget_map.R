# build_retarget_map.R — phase 3, the ONE-OFF crunch. DESIGN §6.6.
#
# Builds the source of truth for retargeting: one row per ON-PREM qvd, saying
# which Cloud view replaces it and what is missing if it cannot. Run ONCE for
# the estate, not once per app. Nothing here reads a load script.
#
#     Rscript retargeting/build_retarget_map.R
#
# Outputs `retarget_map.csv` (greppable, diffable, hand-editable) and
# `retarget_payloads.json` (the multi-line material a CSV cell cannot hold:
# view column lists, dropped columns, the original qvd load, suggested SQL).
#
# WHY A MAP AND NOT A SEARCH AT RUNTIME (Adam, 2026-08-24)
#
# Resolution is expensive and identical for every app that reads the same
# qvd — app-unbuilt and app2 share most of their paths. Doing it once means
# the pipeline does a hash lookup on a path and gets an answer, rather than
# re-deriving the same verdict per app per run. It also means the answer can
# be REVIEWED AND CORRECTED once, in one file, instead of drifting per app.
#
# WHAT THIS DELIBERATELY DOES NOT DO
#
# It does not read the qvd's own field list from `LineageStatement`. Measured
# 2026-08-24: the alias side of the lineage LOAD accounts for only 68% of the
# fields a real script reads, and the source side only 32%. The authority on
# what the app needs is the APP, so the per-app field check stays at runtime
# where the script is — this file supplies the view's column list to check
# against. The bulk work is done once; only the cheap set membership is not.
#
# Base R only. Not sourced by run_pipeline.R.

.bm_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
PROJECT_DIR <- if (length(.bm_file_arg)) {
  tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .bm_file_arg[1])))),
           error = function(e) NA)
} else NA
if (is.na(PROJECT_DIR) || !dir.exists(PROJECT_DIR)) {
  stop("could not determine the project folder from the running script's path. ",
       "Run this via 'Rscript retargeting/build_retarget_map.R'.", call. = FALSE)
}
setwd(PROJECT_DIR)
source("retargeting/resolve_paths.R")

# --- a minimal JSON writer -----------------------------------------------
# Base R has none, and `jsonlite` is a development-machine oracle that must
# not reach shipped tooling (INTERFACES.md). Only the shapes used below are
# supported: string, character vector, and a named list of those.
bm_json_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  # Remaining C0 controls would produce invalid JSON; drop them rather than
  # emit a file that will not parse. Qlik scripts do carry stray control
  # characters, so this is not hypothetical.
  gsub("[\001-\037]", "", x)
}
bm_json_str <- function(x) paste0("\"", bm_json_escape(as.character(x)), "\"")
bm_json_arr <- function(x)
  if (!length(x)) "[]" else paste0("[", paste(bm_json_str(x), collapse = ","), "]")
bm_json_obj <- function(lst) {
  parts <- vapply(names(lst), function(k) {
    v <- lst[[k]]
    paste0(bm_json_str(k), ":", if (length(v) == 1L && !is.list(v)) bm_json_str(v) else bm_json_arr(v))
  }, character(1))
  paste0("{", paste(parts, collapse = ","), "}")
}

# --- inputs ---------------------------------------------------------------

bm_load_fixtures <- function(dir = "fixtures") {
  need <- c(qvd = "qvdlist.csv", views = "views.csv",
            cols = "DBfixture1.csv", lineage = "DBfixture2.csv")
  paths <- file.path(dir, need)
  miss <- need[!file.exists(paths)]
  if (length(miss))
    stop("missing fixture(s): ", paste(miss, collapse = ", "),
         ". All four are required; this tool resolves nothing without them.",
         call. = FALSE)
  lapply(setNames(paths, names(need)), utils::read.csv,
         stringsAsFactors = FALSE, colClasses = "character")
}

# --- indexes --------------------------------------------------------------

#' Source object of an on-prem qvd, from `BestSqlObject` (`server.SCHEMA.OBJECT`).
#'
#' Takes the LAST TWO dot-separated parts, so a server name containing dots
#' cannot shift the answer. Returns a lower-cased "schema\rname" key.
bm_src_key <- function(best) {
  p <- strsplit(best, ".", fixed = TRUE)
  n <- lengths(p)
  tolower(vapply(seq_along(p), function(i)
    if (n[i] >= 2L) paste(p[[i]][n[i] - 1L], p[[i]][n[i]], sep = "\r") else NA_character_,
    character(1)))
}

bm_indexes <- function(fx) {
  ck <- tolower(paste(fx$cols$TABLE_SCHEMA, fx$cols$TABLE_NAME, sep = "\r"))
  list(
    # every database object's columns, TABLE and VIEW alike. NEVER filter by
    # TABLE_TYPE: on-prem loads were free to read either (DESIGN §6.6), and a
    # BASE-TABLE-only filter found zero of 24 real loads.
    obj_cols  = split(fx$cols$COLUMN_NAME, ck),
    obj_type  = tapply(fx$cols$TABLE_TYPE, ck, function(x) x[1]),
    # which Cloud views are built over a given source object
    views_over = split(tolower(paste(fx$lineage$VIEW_SCHEMA, fx$lineage$VIEW_NAME, sep = "\r")),
                       tolower(paste(fx$lineage$TABLE_SCHEMA, fx$lineage$TABLE_NAME, sep = "\r"))),
    # the imported subset, and their columns. views.csv is 1:1 with Cloud
    # qvds by the new design standard, so its columns ARE the qvd's fields.
    imported  = unique(tolower(paste(fx$views$VIEW_SCHEMA, fx$views$VIEW_NAME, sep = "\r"))),
    view_cols = split(fx$views$COLUMN_NAME,
                      tolower(paste(fx$views$VIEW_SCHEMA, fx$views$VIEW_NAME, sep = "\r"))),
    view_disp = { k <- tolower(paste(fx$views$VIEW_SCHEMA, fx$views$VIEW_NAME, sep = "\r"))
                  d <- !duplicated(k)
                  setNames(paste(fx$views$VIEW_SCHEMA[d], fx$views$VIEW_NAME[d], sep = "\r"), k[d]) })
}

bm_is_true <- function(x) tolower(x) %in% c("true", "1", "yes", "y")

#' Does this qvd combine or reshape more than one source?
#'
#' Such a qvd has no single view that can replace it however the names line
#' up — it is a rebuild decision, not a rewrite (DESIGN §6.6 stage 5).
bm_multi_source <- function(row)
  bm_is_true(row$HasJoin) || bm_is_true(row$HasAggregation) ||
  bm_is_true(row$HasUnion) || identical(row$SourceType, "Mixed")

# --- suggested SQL --------------------------------------------------------

#' A real CREATE VIEW for the single-source case; a labelled scaffold
#' otherwise (Adam, 2026-08-24).
#'
#' The line is drawn where a tool would start inventing: projecting the
#' columns of ONE known object is mechanical, and a wrong projection is
#' visible. Translating a Qlik load's joins and transformations into SQL is
#' not, and a wrong join produces a view full of plausible wrong data that
#' nobody re-checks. For those, the source tables and the original load are
#' emitted as raw material for a developer, and nothing is asserted.
#'
#' GOTCHA, caught on the first estate run: the TARGET view must not be named
#' after the SOURCE object. An early version emitted
#' `CREATE VIEW [ess].[esam users] AS SELECT ... FROM [ess].[esam users]`,
#' which is both a self-reference and lower-cased, because it built the name
#' from the lower-cased lookup KEY rather than from `BestSqlObject` itself.
#' Case comes from the raw value; the target name comes from the QVD, which
#' is what the new view has to replicate 1:1.
bm_suggest_sql <- function(best, qvd_name, cols, multi) {
  p <- strsplit(best, ".", fixed = TRUE)[[1]]
  n <- length(p)
  schema <- if (n >= 2L) p[n - 1L] else "<schema>"
  object <- if (n >= 1L) p[n] else "<object>"
  target <- sub("[.][^.]*$", "", qvd_name)            # drop .qvd / .QVD
  if (identical(tolower(target), tolower(object)))
    target <- paste0(target, "_v")                     # never self-reference
  if (multi || !length(cols))
    return(paste0(
      "-- SCAFFOLD ONLY - NOT A WORKING VIEW.\n",
      "-- This qvd joined, aggregated or reshaped more than one source, so the\n",
      "-- join logic below is NOT derived and must be written by a developer.\n",
      "-- The original Qlik load is in this payload's `original_load`.\n",
      "-- Best-guess source object: ", schema, ".", object, "\n",
      "CREATE VIEW [", schema, "].[", target, "] AS\nSELECT\n    /* columns */\nFROM [",
      schema, "].[", object, "]\n/* joins and filters from the original load */;"))
  paste0("CREATE VIEW [", schema, "].[", target, "] AS\nSELECT\n    ",
         paste0("[", cols, "]", collapse = ",\n    "),
         "\nFROM [", schema, "].[", object, "];")
}

# --- the crunch -----------------------------------------------------------

#' One row per on-prem qvd. No script is consulted.
build_retarget_map <- function(fx, idx) {
  q   <- fx$qvd
  src <- bm_src_key(q$BestSqlObject)
  n   <- nrow(q)

  verdict <- character(n); vs <- character(n); vn <- character(n)
  newpath <- character(n); ncol_src <- integer(n); ncol_view <- integer(n)
  ndrop <- integer(n); payload <- vector("list", n)

  for (i in seq_len(n)) {
    row   <- q[i, ]
    multi <- bm_multi_source(row)
    scols <- if (!is.na(src[i])) idx$obj_cols[[src[i]]] else NULL
    cands <- if (!is.na(src[i])) idx$views_over[[src[i]]] else NULL
    imp   <- if (length(cands)) cands[cands %in% idx$imported] else character(0)

    ncol_src[i] <- length(scols)
    pay <- list(rel_path = row$RelPath, source_object = row$BestSqlObject,
                sql_object_match = row$SqlObjectMatch, load_logic = row$LoadLogic)

    if (length(imp)) {
      # Pick the imported view that covers the most of the source object's
      # columns. Ties are not agonised over: every candidate is genuinely
      # built over this object, so the widest one loses the least.
      cover <- vapply(imp, function(k) sum(tolower(scols) %in% tolower(idx$view_cols[[k]])), integer(1))
      k     <- imp[which.max(cover)]
      vcols <- idx$view_cols[[k]]
      disp  <- strsplit(idx$view_disp[[k]], "\r", fixed = TRUE)[[1]]
      vs[i] <- disp[1]; vn[i] <- disp[2]
      newpath[i]  <- rp_new_path(disp[1], disp[2])
      ncol_view[i] <- length(vcols)
      dropped <- scols[!(tolower(scols) %in% tolower(vcols))]
      ndrop[i] <- length(dropped)
      verdict[i] <- if (multi) "multi-source"
                    else if (length(dropped)) "retarget-incomplete" else "retarget"
      pay$view_columns    <- vcols
      pay$dropped_columns <- dropped
      if (multi) pay$original_load <- row$LineageStatement
    } else if (length(cands)) {
      k <- cands[1]
      disp <- strsplit(k, "\r", fixed = TRUE)[[1]]
      vs[i] <- disp[1]; vn[i] <- disp[2]
      verdict[i] <- "needs-import"
      pay$views_available_not_imported <- vapply(cands, function(x)
        sub("\r", ".", x), character(1))
    } else if (length(scols)) {
      verdict[i] <- "needs-creating"
      pay$source_columns <- scols
      pay$suggested_sql  <- bm_suggest_sql(row$BestSqlObject, row$QvdName, scols, multi)
      pay$original_load  <- row$LineageStatement
    } else {
      verdict[i] <- "unmapped"
      pay$why <- "source object not found in the database extract"
      pay$original_load <- row$LineageStatement
    }
    payload[[i]] <- pay
  }

  list(map = data.frame(
        rel_path = q$RelPath, qvd_name = q$QvdName, creator_app = q$CreatorAppName,
        source_object = q$BestSqlObject, sql_object_match = q$SqlObjectMatch,
        load_logic = q$LoadLogic, multi_source = vapply(seq_len(n),
          function(i) bm_multi_source(q[i, ]), logical(1)),
        has_rename = bm_is_true(q$HasRename),
        verdict = verdict, view_schema = vs, view_name = vn, new_path = newpath,
        n_source_columns = ncol_src, n_view_columns = ncol_view,
        n_columns_dropped = ndrop, decided_by = "auto",
        stringsAsFactors = FALSE),
       payloads = payload)
}

# --- CLI ------------------------------------------------------------------

main <- function(args) {
  outd <- if (length(args) >= 2L && args[1] == "--out") args[2] else "retargeting"
  fx  <- bm_load_fixtures()
  idx <- bm_indexes(fx)
  r   <- build_retarget_map(fx, idx)

  cat("build_retarget_map: ", nrow(r$map), " on-prem qvds\n", sep = "")
  v <- sort(table(r$map$verdict), decreasing = TRUE)
  for (k in names(v)) cat(sprintf("  %-22s %5d\n", k, v[[k]]))
  cat("  columns dropped by the chosen view, total: ", sum(r$map$n_columns_dropped), "\n", sep = "")

  mfile <- file.path(outd, "retarget_map.csv")
  pfile <- file.path(outd, "retarget_payloads.json")
  old <- rp_read_if(mfile)
  utils::write.csv(rp_merge(old, r$map, "rel_path"), mfile, row.names = FALSE)

  con <- file(pfile, open = "wt", encoding = "UTF-8")
  on.exit(close(con))
  writeLines("{", con)
  for (i in seq_along(r$payloads)) {
    writeLines(paste0("  ", bm_json_str(r$map$rel_path[i]), ": ",
                      bm_json_obj(r$payloads[[i]]),
                      if (i < length(r$payloads)) "," else ""), con)
  }
  writeLines("}", con)
  cat("wrote ", mfile, " and ", pfile, "\n", sep = "")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
