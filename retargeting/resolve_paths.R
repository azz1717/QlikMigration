# resolve_paths.R — phase 3 stage 4, the RESOLVER. DESIGN §6.6.
#
# Decides, for every on-prem qvd load, WHICH Cloud view replaces it and what
# its fields are called there. It writes two lookup tables and nothing else:
# no script is read for rewriting, no script is written. The rewriter is a
# separate tool that consumes these tables and never consults views.csv
# (DESIGN §6.6) — which is what keeps the trailing-% and prefix logic in one
# place instead of two.
#
# Base R only. Not sourced by run_pipeline.R.
#
# WHY TWO TOOLS AND TWO TABLES
#
# Resolution is a JUDGEMENT that a human may overrule; rewriting is a
# mechanical edit that must not be. Splitting them means the judgement can be
# reviewed, corrected by hand and re-used across apps, while the edit stays
# dumb enough to trust. app-unbuilt holds 96 distinct lib:// paths against
# app2's 15 and they overlap: resolving `PMC Region.qvd` once must serve both.
#
# THE SAFETY ARGUMENT FOR A STRICT FIELD CHECK
#
# Several steps below are heuristics — which prefix to strip, which view a
# name means. Every one of them is made safe by the same property: the field
# check is STRICT (Adam, 2026-08-24). Every field the old load READS must
# exist as a column of the candidate view. A heuristic that guesses wrong
# produces a name the view does not have, so the load fails the check and is
# reported for review. The failure mode of a bad guess is a flag, never a
# silently wrong mapping. Do not loosen the check without re-reading this.

.rp_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
PROJECT_DIR <- if (length(.rp_file_arg)) {
  tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .rp_file_arg[1])))),
           error = function(e) NA)
} else NA
if (is.na(PROJECT_DIR) || !dir.exists(PROJECT_DIR)) {
  stop("could not determine the project folder from the running script's path. ",
       "Run this via 'Rscript retargeting/resolve_paths.R'.", call. = FALSE)
}
setwd(PROJECT_DIR)

source("shared/qlik_tokenizer.R")
source("analysis/script_loads.R")

# --- path vocabulary -----------------------------------------------------

# The canonical on-prem data-lake root. `AppDataProd` and `AppData/PROD` are
# the same location, two spellings of one connection (Adam, 2026-08-24), so
# every path test runs on the canonical form and never on the raw string.
.RP_ROOT      <- "lib://appdata/prod/azuredatalake/"
.RP_GEO       <- "lib://appdata/prod/geospatial/"
.RP_CURATED   <- "lib://curated data store:datafiles/"
.RP_CURATED_D <- "lib://curated data store - dev:datafiles/"
.RP_NEW_ROOT  <- "lib://Curated data Store:DataFiles/10 Landing Area/"

#' Canonical form of a lib:// path: separators normalised, connection folded,
#' lower-cased. Used for EVERY comparison; never shown to the user, who should
#' see the path as it really appears in the script.
rp_canonical_path <- function(x) {
  y <- gsub("\\\\", "/", x)
  y <- tolower(y)
  sub("^lib://appdataprod/", "lib://appdata/prod/", y)
}

#' What kind of load is this, before any lookup is attempted?
#'
#' `in-scope` is the only class the resolver looks up. The rest are recorded
#' so the table is a complete inventory of every `from` load in the app
#' (Adam, 2026-08-24) rather than only the part being changed — an unmapped
#' path is a developer's decision and they cannot make it from a partial list.
rp_classify <- function(path) {
  cp <- rp_canonical_path(path)
  if (startsWith(cp, .RP_ROOT))      return("in-scope")
  if (startsWith(cp, .RP_GEO))       return("geospatial")
  if (startsWith(cp, .RP_CURATED_D)) return("dev-connection")
  if (startsWith(cp, .RP_CURATED))   return("already-mapped")
  "other"
}

#' Schema directory and qvd stem from an in-scope path.
#'
#' Every in-scope path in app-unbuilt matches `AzureDataLake/<SCHEMA>/<name>.qvd`
#' exactly — one directory, never zero, never two (measured 2026-08-24). This
#' returns NA for the schema rather than assuming, because that measurement is
#' one app's and the resolver must survive an app where it does not hold.
#' GOTCHA that cost the first run: DEPTH comes from the canonical path, TEXT
#' comes from the raw one, and the two cannot be indexed by the same character
#' offset. `lib://AppDataProd/` is 18 characters where its canonical form
#' `lib://appdata/prod/azuredatalake/` is 33, so slicing the raw string at the
#' canonical length cut it in the wrong place and lost the schema directory —
#' which showed up as `Grant Activity.qvd` reported ambiguous across FUSION and
#' GPS when its path said FUSION plainly. Split both, take the tail.
rp_qvd_parts <- function(path) {
  rest <- substring(rp_canonical_path(path), nchar(.RP_ROOT) + 1L)
  tail <- strsplit(rest, "/", fixed = TRUE)[[1]]
  rseg <- strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]]
  # The tail is identical in both spellings — only the ROOT differs — so the
  # last length(tail) raw segments are the same segments, with their real case.
  rtail <- rseg[seq.int(length(rseg) - length(tail) + 1L, length(rseg))]
  n <- length(rtail)
  list(schema = if (n >= 2L) rtail[n - 1L] else NA_character_,
       stem   = sub("[.][^.]*$", "", rtail[n]))   # drop the extension, keep case
}

#' The Cloud path a resolved view becomes.
rp_new_path <- function(view_schema, view_name)
  paste0(.RP_NEW_ROOT, view_schema, "/", view_name, ".qvd")

# --- field names ---------------------------------------------------------

#' On-prem qvd field name -> the name the Cloud qvd uses.
#'
#' Two strips, both of which may apply to one name
#' (`"Grant Activity.Source System Id%"`):
#'   * the source table rides on the front as `Prefix.`;
#'   * a GUID id field carries a TRAILING `%`.
#'
#' The trailing `%` is uniform: of the 17 distinct `Id%` fields in
#' `[Grant Managing Region].txt`, ZERO survive as a view column with the `%`
#' still on (DESIGN §6.6). A LEADING `%` is an unrelated naming family
#' (`%Dim_*_Key`, 28 views) which KEEPS its `%` — hence `[%]$`, anchored, and
#' never a positional strip.
#'
#' The prefix strip takes everything to the FIRST dot. That is a heuristic:
#' a column whose own name contained a dot would be truncated. It is safe
#' only because of the strict field check — see the header. It is not safe on
#' its own, so do not reuse this function anywhere the check is absent.
rp_norm_field <- function(x) {
  y <- sub("[%]$", "", x)
  sub("^[^.]*[.]", "", y)
}

#' Every qvd field a LOAD segment READS, from typed tokens.
#'
#' NOT from `script_loads()`'s `source_field`, which is a collapsed string: an
#' aliased expression yields `IF(LEN(PMC Region.PMC Region Code)>1,1,0)` with
#' its delimiters already stripped, so the references inside it can no longer
#' be told from the function names around them. Reading the tokens instead
#' gets plain references and nested ones by the same rule, with no parsing of
#' collapsed text (Adam, 2026-08-24).
#'
#' BRACKET and DQUOTE only. SQUOTE is a literal, and every WORD on the source
#' side of a `from` load in both styled apps is a function or keyword — 131
#' in app-unbuilt, 102 in app2, zero bare field references (DESIGN §6.6).
rp_segment_refs <- function(tokens, seg) {
  src <- if (seg$has_as) seg$content_idx[seg$content_idx < seg$as_idx] else seg$content_idx
  src <- src[tokens$type[src] %in% c("BRACKET", "DQUOTE")]
  if (!length(src)) return(character(0))
  vapply(src, function(i) undelimit(tokens$text[i], tokens$type[i]), character(1))
}

# --- the oracle ----------------------------------------------------------

#' views.csv -> per-view column sets. The ONLY authority on what a Cloud qvd
#' contains; nothing is inferred from the old script (DESIGN §6.6).
rp_read_views <- function(path = "fixtures/views.csv") {
  if (!file.exists(path))
    stop("views.csv not found at '", path, "'. Pass --views <path>.", call. = FALSE)
  v <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  need <- c("VIEW_SCHEMA", "VIEW_NAME", "COLUMN_NAME")
  miss <- setdiff(need, names(v))
  if (length(miss))
    stop("views.csv is missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  key <- paste(v$VIEW_SCHEMA, v$VIEW_NAME, sep = "\r")
  sp  <- split(v$COLUMN_NAME, key)
  parts <- strsplit(names(sp), "\r", fixed = TRUE)
  data.frame(view_schema = vapply(parts, `[`, character(1), 1L),
             view_name   = vapply(parts, `[`, character(1), 2L),
             n_columns   = lengths(sp),
             stringsAsFactors = FALSE)[order(names(sp)), ] -> idx
  list(index = idx, columns = sp)
}

# --- resolution ----------------------------------------------------------

#' One path -> one verdict.
#'
#' Order is DESIGN §6.6's: name, then schema only where the collision needs
#' breaking, then the field check ALWAYS — including on a unique name match,
#' because a unique name is not evidence that the qvd is the same shape.
#'
#' A cross-schema tie the path cannot break is NEVER resolved automatically
#' (Adam, 2026-08-20), even where one candidate's field overlap leads.
#' Guessing a schema silently points a load at the wrong data.
rp_resolve_one <- function(stem, schema, refs, views) {
  hit <- which(tolower(views$index$view_name) == tolower(stem))
  if (!length(hit))
    return(list(status = "unmatched", view_schema = NA_character_,
                view_name = NA_character_,
                evidence = "no view of that name"))

  # The old directory is a HINT THAT MAY BE WRONG (Adam, 2026-08-24): schema
  # alignment was never enforced on-prem, so a directory is not a schema.
  # ORIC, NACCHO and AEDC are directories in app-unbuilt that are not
  # VIEW_SCHEMA values at all; GMR uses SSR. Narrow by it only where it names
  # a real schema, and never let a non-matching directory count as evidence.
  known <- !is.na(schema) && tolower(schema) %in% tolower(views$index$view_schema)
  if (length(hit) > 1L && known) {
    narrowed <- hit[tolower(views$index$view_schema[hit]) == tolower(schema)]
    if (length(narrowed) == 1L) hit <- narrowed
  }
  if (length(hit) > 1L)
    return(list(status = "ambiguous", view_schema = NA_character_,
                view_name = NA_character_,
                evidence = paste0(length(hit), " views named that, schemas: ",
                                  paste(views$index$view_schema[hit], collapse = "/"))))

  vs  <- views$index$view_schema[hit]
  vn  <- views$index$view_name[hit]
  col <- views$columns[[paste(vs, vn, sep = "\r")]]

  want <- unique(rp_norm_field(refs))
  want <- want[nzchar(want)]
  missing <- want[!(tolower(want) %in% tolower(col))]

  if (length(missing))
    return(list(status = "field-mismatch", view_schema = vs, view_name = vn,
                evidence = paste0(length(missing), " of ", length(want),
                                  " read fields absent from the view: ",
                                  paste(utils::head(missing, 5L), collapse = "; "))))

  list(status = "resolved", view_schema = vs, view_name = vn,
       evidence = paste0(length(want), " of ", length(want),
                         " read fields present (view has ", length(col), " columns)"))
}

#' Resolve every `from` load in one script.
#'
#' @return list(paths, fields, warnings)
resolve_paths <- function(tokens, views, app = NA_character_) {
  sl   <- script_loads(tokens)
  segs <- find_load_segments(tokens)$segments
  frm  <- sl$loads[sl$loads$source_kind == "from" & !is.na(sl$loads$source), ]

  out <- list(); fmap <- list(); warn <- character(0)

  for (i in seq_len(nrow(frm))) {
    ld    <- frm[i, ]
    class <- rp_classify(ld$source)

    # Segments of THIS load, by line span. `script_loads()` gives line
    # extents, `find_load_segments()` gives a line per field; the two meet
    # there. A load with no segments is a `LOAD *`, which reads no named
    # field and therefore offers the check nothing.
    #
    # `segments` is a LIST, not a data.frame — `content_idx` is a
    # variable-length vector per segment and cannot be a column. Filter it as
    # a list; subsetting it with [i, ] fails with "incorrect number of
    # dimensions", which is how this was found.
    mine <- Filter(function(s) s$line >= ld$line_start && s$line <= ld$line_end, segs)
    refs <- unique(unlist(lapply(mine, function(s) rp_segment_refs(tokens, s))))
    refs <- refs[!is.na(refs) & nzchar(refs)]

    if (class != "in-scope") {
      out[[length(out) + 1L]] <- data.frame(
        app = app, load_id = ld$load_id, table = ld$table, tab = ld$tab,
        line = ld$line_start, old_path = ld$source, status = class,
        view_schema = NA_character_, view_name = NA_character_,
        new_path = NA_character_, n_read_fields = length(refs),
        evidence = "not looked up; out of the rewriter's scope",
        decided_by = "auto", stringsAsFactors = FALSE)
      next
    }

    parts <- rp_qvd_parts(ld$source)
    if (!length(refs))
      warn <- c(warn, sprintf("line %d: `%s` reads no named field (wildcard?); the field check cannot run",
                              ld$line_start, parts$stem))

    v <- rp_resolve_one(parts$stem, parts$schema, refs, views)
    np <- if (v$status == "resolved") rp_new_path(v$view_schema, v$view_name) else NA_character_

    out[[length(out) + 1L]] <- data.frame(
      app = app, load_id = ld$load_id, table = ld$table, tab = ld$tab,
      line = ld$line_start, old_path = ld$source, status = v$status,
      view_schema = v$view_schema, view_name = v$view_name, new_path = np,
      n_read_fields = length(refs), evidence = v$evidence,
      decided_by = "auto", stringsAsFactors = FALSE)

    if (v$status == "resolved" && length(refs))
      fmap[[length(fmap) + 1L]] <- data.frame(
        view_schema = v$view_schema, view_name = v$view_name,
        old_field = refs, new_field = rp_norm_field(refs),
        stringsAsFactors = FALSE)
  }

  paths <- if (length(out)) do.call(rbind, out) else
    data.frame(app = character(0), load_id = integer(0), table = character(0),
               tab = character(0), line = integer(0), old_path = character(0),
               status = character(0), view_schema = character(0),
               view_name = character(0), new_path = character(0),
               n_read_fields = integer(0), evidence = character(0),
               decided_by = character(0), stringsAsFactors = FALSE)

  fields <- if (length(fmap)) unique(do.call(rbind, fmap)) else
    data.frame(view_schema = character(0), view_name = character(0),
               old_field = character(0), new_field = character(0),
               stringsAsFactors = FALSE)

  list(paths = paths, fields = fields, warnings = c(sl$warnings, warn))
}

# --- persistence ---------------------------------------------------------

#' Merge new rows into an existing table WITHOUT overwriting a hand decision.
#'
#' The tables are shared across apps and hand-editable (DESIGN §6.6), so a run
#' for app2 must never silently revert a verdict corrected for app-unbuilt. A
#' row whose `decided_by` is not `auto` is kept exactly as it is; the fresh
#' automatic verdict for that key is discarded, not merged, because a merged
#' half-row is neither the tool's answer nor the human's.
rp_merge <- function(old, new, keys) {
  if (is.null(old) || !nrow(old)) return(new)
  k_old <- do.call(paste, c(old[keys], sep = "\r"))
  k_new <- do.call(paste, c(new[keys], sep = "\r"))
  keep  <- if ("decided_by" %in% names(old)) old$decided_by != "auto" else rep(FALSE, nrow(old))
  rbind(old[keep, , drop = FALSE], new[!(k_new %in% k_old[keep]), , drop = FALSE])
}

rp_read_if <- function(path)
  if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE,
                                         colClasses = "character") else NULL

# --- CLI -----------------------------------------------------------------

main <- function(args) {
  if (!length(args)) {
    cat("usage: Rscript retargeting/resolve_paths.R <script.qvs> [--app NAME]",
        "[--views fixtures/views.csv] [--out retargeting]\n")
    return(invisible(NULL))
  }
  script <- args[1]
  getopt <- function(flag, default) {
    i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[i + 1L]
  }
  app   <- getopt("--app", basename(dirname(normalizePath(script, mustWork = FALSE))))
  vpath <- getopt("--views", "fixtures/views.csv")
  outd  <- getopt("--out", "retargeting")

  views  <- rp_read_views(vpath)
  tokens <- read_qlik_script(script)
  r      <- resolve_paths(tokens, views, app = app)

  cat("resolve_paths: ", script, "\n", sep = "")
  cat("  views:     ", nrow(views$index), " distinct schema/view pairs\n", sep = "")
  cat("  from-loads:", nrow(r$paths), "\n")
  st <- table(r$paths$status)
  for (n in names(st)) cat(sprintf("    %-16s %d\n", n, st[[n]]))
  cat("  field pairs: ", nrow(r$fields), " (",
      sum(r$fields$old_field != r$fields$new_field), " differ)\n", sep = "")
  if (length(r$warnings))
    cat("  warnings: ", length(r$warnings), "\n    ",
        paste(utils::head(unique(r$warnings), 5L), collapse = "\n    "), "\n", sep = "")

  pfile <- file.path(outd, "path_map.csv")
  ffile <- file.path(outd, "field_map.csv")
  utils::write.csv(rp_merge(rp_read_if(pfile), r$paths, c("app", "old_path", "load_id")),
                   pfile, row.names = FALSE)
  utils::write.csv(rp_merge(rp_read_if(ffile), r$fields, c("view_schema", "view_name", "old_field")),
                   ffile, row.names = FALSE)
  cat("wrote ", pfile, " and ", ffile, "\n", sep = "")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
