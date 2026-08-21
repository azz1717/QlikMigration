# usage_report.R — the cross-reference: what the app never uses.
#
# Phase 2, step 4a (DESIGN §6.5). NOT part of the styling pipeline; nothing in
# run_pipeline.R sources this. Base R only. Nothing here deletes anything.
#
# Joins three sources into one reviewable table:
#   app_usage.R    what the app's own definition references
#   script_loads.R what each LOAD produces, and which table holds it
#   script_refs.R  how the script itself uses those tables
#
# It DETECTS and REPORTS. No category carries a recommended action — whether a
# removal or a DROP is automated or left to a developer is undecided
# (DESIGN §6.5, Adam 2026-08-18).

source("analysis/app_usage.R")
source("analysis/script_loads.R")
source("analysis/script_refs.R")

# Every encoding choice here widens or narrows the `unreferenced` list, so each
# is spelled out in DESIGN §6.5 rather than left in the code:
#   - case is folded HERE and only here; steps 2 and 3 preserve it
#   - `quoted` and `bare` matches both count as usage
#   - the unit is a TABLE, not a load
#   - multi-table loads get no verdict; nor do field-level findings on a table
#     whose field list is incomplete
.ur_fold <- function(x) tolower(trimws(x))

#' Cross-reference one app export against its load script.
#'
#' @param app_dir an app*-unbuilt directory.
#' @param script_path the load script. Defaults to the export's own copy;
#'   pass the STYLED output where you have it (DESIGN §5 — phase 1 first).
#' @return list(tables, fields, undetermined, warnings).
usage_report <- function(app_dir, script_path = file.path(app_dir, "script.qvs")) {
  refs <- app_references(app_dir)
  tok  <- read_qlik_script(script_path)
  sl   <- script_loads(tok)
  app  <- basename(app_dir)

  loads  <- sl$loads
  fields <- sl$fields
  names_ <- unique(na.omit(loads$table))

  own   <- unique(loads[!is.na(loads$table), c("table", "line_start", "line_end")])
  sref  <- script_table_refs(tok, names_, own)
  store <- script_disposals(tok, names_)$stores

  # --- field level ---------------------------------------------------------
  # One reference name can be produced by several tables; every one of them
  # counts as referenced. Conservative on purpose.
  ref_key <- .ur_fold(refs$ref)
  idx     <- split(seq_along(ref_key), ref_key)

  fkey <- .ur_fold(fields$field)
  hits <- idx[fkey]

  ev <- function(h) if (is.null(h)) NA_character_ else
    paste(sort(unique(refs$source_file[h])), collapse = "; ")
  kd <- function(h) if (is.null(h)) NA_character_ else
    paste(sort(unique(refs$kind[h])), collapse = "+")

  fld <- data.frame(
    app        = app,
    table      = fields$table,
    field      = fields$field,
    via        = fields$via,
    referenced = !vapply(hits, is.null, logical(1)),
    ref_kinds  = vapply(hits, kd, character(1)),
    n_refs     = vapply(hits, function(h) if (is.null(h)) 0L else sum(refs$n[h]), integer(1)),
    evidence   = vapply(hits, ev, character(1)),
    row.names  = NULL, stringsAsFactors = FALSE)
  fld <- fld[!is.na(fld$table), , drop = FALSE]
  fld <- fld[!duplicated(fld[, c("table", "field")]), , drop = FALSE]

  # --- table level ---------------------------------------------------------
  tbl <- do.call(rbind, lapply(names_, function(nm) {
    mine <- loads[!is.na(loads$table) & loads$table == nm, ]
    f    <- fld[fld$table == nm, ]
    sr   <- sref[sref$table == nm, ]

    app_used    <- any(f$referenced)
    script_used <- any(sr$kind == "use")
    dropped     <- any(sr$kind == "drop")
    complete    <- all(mine$complete_fields)

    # DROP is tested FIRST and beats an app match. A dropped table is not in
    # the final data model, so the app cannot be referencing it — any hit is
    # its field NAMES coinciding with those of the table that survives, which
    # is exactly what a `_Temp` feeding its successor looks like. Testing
    # app_used first put 7 of app2's 9 dropped tables in `referenced`.
    #
    # For a table that is NOT dropped the same name-matching is correct rather
    # than coincidental: Qlik's model is associative, so two live tables
    # sharing a field name share the field, and both genuinely contribute.
    cat_ <- if (dropped) "build-only, dropped"
            else if (app_used) "referenced"
            else if (script_used) "build-only, retained"
            else "unreferenced"

    data.frame(
      app = app, table = nm, category = cat_,
      producers = paste(sort(unique(mine$producer_kind)), collapse = "+"),
      sources   = paste(sort(unique(mine$source_kind)), collapse = "+"),
      n_loads = nrow(mine),
      line_start = min(mine$line_start), line_end = max(mine$line_end),
      n_fields = nrow(f), n_referenced = sum(f$referenced),
      script_reads = sum(sr$kind == "use"), dropped = dropped,
      complete_fields = complete,
      inline_rows = if (all(is.na(mine$inline_rows))) NA_integer_
                    else max(mine$inline_rows, na.rm = TRUE),
      stringsAsFactors = FALSE)
  }))

  # Field-level findings are withheld where the field list is not the whole
  # truth — an unresolved `*` means an absent name proves nothing.
  incomplete <- tbl$table[!tbl$complete_fields]
  fld$field_finding <- !fld$referenced &
    fld$table %in% tbl$table[tbl$category == "referenced"] &
    !(fld$table %in% incomplete)

  und <- loads[loads$producer_kind == "multi-table",
               c("load_id", "source", "line_start", "line_end")]
  if (nrow(und)) und <- data.frame(app = app, und, row.names = NULL)

  list(tables = tbl[order(tbl$category, tbl$table), ], fields = fld,
       undetermined = und, stores = store, warnings = sl$warnings)
}

.ur_show <- function(r) {
  t <- r$tables
  cat("\n=== ", t$app[1L], " — ", nrow(t), " tables ===\n", sep = "")
  print(table(t$category))

  for (cg in c("unreferenced", "build-only, retained")) {
    x <- t[t$category == cg, ]
    if (!nrow(x)) next
    cat("\n-- ", cg, " (", nrow(x), ") --\n", sep = "")
    print(x[order(-x$n_fields),
            c("table", "producers", "sources", "n_fields", "line_start", "line_end")],
          row.names = FALSE)
  }

  ff <- r$fields[r$fields$field_finding, ]
  cat("\n-- unreferenced fields inside referenced tables: ", nrow(ff), " --\n", sep = "")
  if (nrow(ff)) {
    by <- sort(table(ff$table), decreasing = TRUE)
    print(head(data.frame(table = names(by), unreferenced_fields = as.integer(by)), 10),
          row.names = FALSE)
  }

  if (nrow(r$undetermined))
    cat("\n-- undetermined (wildcard qvd paths): ", nrow(r$undetermined), " --\n", sep = "")
  if (nrow(r$stores))
    cat("-- STORE statements (qvd generation inside an app): ", nrow(r$stores), " --\n", sep = "")
}

main <- function(args) {
  stem <- if ("--csv" %in% args) args[match("--csv", args) + 1L] else NA
  args <- setdiff(args, c("--csv", stem))
  scr  <- if ("--script" %in% args) args[match("--script", args) + 1L] else NA
  args <- setdiff(args, c("--script", scr))
  if (length(args) == 0L)
    stop("usage: Rscript usage_report.R <app-dir> [--script <path>] [--csv <stem>]")

  r <- if (is.na(scr)) usage_report(args[1L]) else usage_report(args[1L], scr)
  .ur_show(r)

  if (!is.na(stem)) {
    write.csv(r$tables, paste0(stem, "-tables.csv"), row.names = FALSE)
    write.csv(r$fields, paste0(stem, "-fields.csv"), row.names = FALSE)
    cat("\nwrote ", stem, "-tables.csv and ", stem, "-fields.csv\n", sep = "")
  }
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
