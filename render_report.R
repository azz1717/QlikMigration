# render_report.R - phase 2 step 4b: the submittable document.
#
# NOT part of the styling pipeline; nothing in run_pipeline.R sources this.
# Base R only, self-contained HTML only: no external stylesheet, script, font
# or image, because nothing can be assumed about the machine that opens it.
# No interaction of any kind - this is a document to print, file and read.
#
# ONE question: what and how much? Three drafts failed by answering a
# different one. The rule that governs every line of this file:
#
#   THE REPORT SUPPLIES QUANTITY AND NATURE. THE READER SUPPLIES THE VERDICT.
#
# Severity, priority, effort and risk are the reader's judgement - an
# experienced developer sizes this app in moments from facts, and any label
# this file invents (blocking, safe to delete, needs a decision) takes that
# judgement away from them. So: no severity buckets, no ordering by
# importance, no recommendations, no effort language. Name the actual thing -
# the server, the connector, the count - and stop. DESIGN 7.2.
#
# Nature is carried by naming, not by adjectives: "21 tables loaded from
# AZDB-ZEA-PRD-NIAADL01" tells a developer what the job is; "24 database
# calls block migration" tells them nothing they can act on.

source("usage_report.R")
source("script_debt.R")

.h <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.num <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")

.plural <- function(n, word)
  sprintf("%s %s%s", .num(n), word, if (as.integer(n) == 1L) "" else "s")

# The app's real name, not the directory an export happened to unpack into.
.rr_title <- function(dir) {
  p <- file.path(dir, "app-properties.json")
  if (file.exists(p)) {
    s <- read_json_strings(p)
    i <- which(s$is_key & s$text == "qTitle")
    if (length(i) && i[1L] < nrow(s) && !s$is_key[i[1L] + 1L]) return(s$text[i[1L] + 1L])
  }
  basename(dir)
}

# Connector function -> the connector product it belongs to. `Closest` and
# `TravelAreas` name an operation, not a product, and neither is familiar
# enough to identify itself (Adam 2026-08-18) - the row has to say which
# connector a call belongs to, because that is what determines whether it can
# come to Cloud at all.
#
# Unrecognised functions are NOT guessed at: they still count, and the row
# falls back to a bare "connector calls" with the function names carrying the
# nature. Extend the list rather than widening a pattern.
.RR_CONNECTOR_PRODUCT <- c(
  Closest = "GeoAnalytics", TravelAreas = "GeoAnalytics",
  Intersects = "GeoAnalytics", Dissolve = "GeoAnalytics",
  Cluster = "GeoAnalytics", Simplify = "GeoAnalytics",
  Route = "GeoAnalytics", Distance = "GeoAnalytics")

# One product for every call, or none: a mixed set gets the generic label
# rather than the label of whichever product happened to sort first.
.rr_connector_label <- function(fn) {
  if (!length(fn)) return("connector calls")
  p <- unique(.RR_CONNECTOR_PRODUCT[fn])
  if (length(p) == 1L && !is.na(p)) sprintf("%s connector calls", p) else "connector calls"
}

.rr_ext <- function(x) tolower(vapply(strsplit(x, ".", fixed = TRUE),
  function(p) if (length(p) > 1L) p[length(p)] else "(none)", character(1)))

# One row: figure, what it is, an optional grey qualifier. The asterisk marks
# BOTH columns when the figure and its denominator are equally uncertain -
# they are different unknowns and the reader is entitled to see both flagged
# (Adam 2026-08-18).
.RR_STAR <- '<a class="fn" href="#wild">*</a>'

.rr_row <- function(fig, what, qual = "", star_fig = FALSE, star_qual = FALSE) {
  s <- .RR_STAR
  sprintf('  <tr><td class="n">%s%s</td><td>%s</td><td class="u">%s%s</td></tr>',
          fig, if (star_fig) s else "", .h(what), qual, if (star_qual) s else "")
}

.rr_section <- function(title, rows)
  c("<section>", sprintf("<h2>%s</h2>", .h(title)), "<table>", rows, "</table>",
    "</section>", "")

# A4 is the constraint, not a preference: the page is printed and filed. Fixed
# body width so screen and print wrap identically, break-inside on every
# section and row, break-after on headings so none is orphaned from its table.
.rr_css <- c(
'<style>',
'  @page { size: A4 portrait; margin: 18mm 16mm; }',
'  html, body { background: #fff; color: #000; }',
'  body { font-family: "Segoe UI", Arial, sans-serif; font-size: 10.5pt;',
'         line-height: 1.35; width: 178mm; margin: 16mm auto; }',
'  header { border-bottom: 2px solid #000; padding-bottom: 5px; margin-bottom: 12px; }',
'  h1 { font-size: 13pt; margin: 0; font-weight: 600; }',
'  .date { font-size: 8.5pt; color: #333; margin-top: 2px; }',
'  section { break-inside: avoid; page-break-inside: avoid; }',
'  h2 { font-size: 9pt; font-weight: 700; letter-spacing: .08em;',
'       text-transform: uppercase; margin: 16px 0 4px;',
'       border-bottom: 1px solid #999; padding-bottom: 2px;',
'       break-after: avoid; page-break-after: avoid; }',
'  table { border-collapse: collapse; width: 100%; table-layout: fixed; }',
'  tr { break-inside: avoid; page-break-inside: avoid; }',
'  td { padding: 1.5px 0; vertical-align: baseline; }',
'  td.n { font-family: Consolas, "Courier New", monospace; text-align: right;',
'         width: 18mm; padding-right: 6mm; font-weight: 600; }',
'  td.u { color: #444; font-size: 9pt; width: 62mm; }',
'  footer { margin-top: 16px; border-top: 1px solid #999; padding-top: 5px;',
'           font-size: 8.5pt; color: #333;',
'           break-inside: avoid; page-break-inside: avoid; }',
'  footer p { margin: 0 0 3px; }',
'  a.fn { text-decoration: none; color: #000; font-weight: 700; }',
'  @media print {',
'    body { width: auto; margin: 0; font-size: 10pt; }',
'    header { border-bottom-width: 1.5pt; }',
'  }',
'</style>')

#' The one-page assessment.
#'
#' @param dir an app*-unbuilt directory.
#' @param script_path defaults to the export's own copy; pass the STYLED
#'   script where there is one.
#' @param out output path; defaults to <dir>-report.html.
#' @return the path written, invisibly.
render_report <- function(dir, script_path = NULL, out = NULL) {
  if (is.null(script_path)) script_path <- file.path(dir, "script.qvs")
  if (is.null(out)) out <- paste0(basename(dir), "-report.html")

  tok  <- read_qlik_script(script_path)
  tabs <- .sl_tabs(tok)
  L    <- script_loads(tok)$loads
  ur   <- usage_report(dir, script_path)
  st   <- sql_targets(tok, tabs)
  ast  <- asset_usage(dir, script_path)
  com  <- commented_out_code(tok, tabs)

  # An unresolved wildcard path makes the model a floor rather than a count,
  # so every table and field total is marked - denominators included.
  wild  <- sum(!L$complete_fields)
  w     <- wild > 0L
  title <- .rr_title(dir)

  n_tables <- length(unique(na.omit(L$table)))
  from     <- L[L$source_kind == "from" & !is.na(L$source), ]
  src      <- unique(from$source)
  ext      <- .rr_ext(src)
  ext_note <- if (!length(src)) "" else if (length(unique(ext)) == 1L)
                paste("all", unique(ext))
              else paste(sprintf("%d %s", as.integer(table(ext)), names(table(ext))),
                         collapse = ", ")
  delim    <- sum(.rr_ext(from$source) %in% c("csv", "txt", "tsv", "psv"))

  app <- .rr_section("Application", c(
    .rr_row(.num(max(tok$line)), "script lines",
            sprintf("%s tabs", .num(length(unique(L$tab))))),
    .rr_row(.num(nrow(L)), "load statements",
            sprintf("%s tables produced", .num(n_tables)), star_qual = w),
    .rr_row(.num(length(src)), "distinct source files",
            sprintf("%s file loads; %s", .num(nrow(from)), .h(ext_note)))))

  obj <- st[st$kind == "object", ]
  con <- st[st$kind == "connector", ]
  srv <- if (nrow(obj)) sort(table(obj$server), decreasing = TRUE) else integer(0)
  db  <- .rr_section("Database loads", c(
    if (length(srv))
      vapply(seq_along(srv), function(i)
        .rr_row(.num(srv[i]), sprintf("tables loaded from %s", names(srv)[i])),
        character(1))
    else .rr_row("0", "tables loaded from a database"),
    .rr_row(.num(nrow(con)), .rr_connector_label(con$object),
            if (nrow(con))
              paste(sprintf("%s &times;%d", .h(names(table(con$object))),
                            as.integer(table(con$object))), collapse = ", ")
            else ""),
    .rr_row(.num(delim), "csv / delimited file loads")))

  cat_n <- function(k) sum(ur$tables$category == k)
  n_fld <- nrow(ur$fields)
  n_unu <- sum(ur$fields$field_finding)
  model <- .rr_section("Data model", c(
    .rr_row(.num(cat_n("unreferenced")), "tables unused",
            sprintf("of %s", .num(n_tables)), star_fig = w, star_qual = w),
    .rr_row(.num(cat_n("build-only, retained")), "tables unDROPped",
            "build-only, left in model"),
    .rr_row(.num(n_unu), "fields unused",
            sprintf("%.0f%% of %s%s loaded", 100 * n_unu / max(n_fld, 1L),
                    .num(n_fld), if (w) .RR_STAR else ""),
            star_fig = w),
    .rr_row(.num(wild), "loads unresolved", "wildcard qvd paths")))

  scr <- .rr_section("Script", c(
    .rr_row(.num(com$total_lines), "lines of dead code",
            sprintf("%.1f%% of the script", 100 * com$total_lines / com$script_lines)),
    .rr_row(.num(sum(L$source_kind == "inline")), "inline loads"),
    .rr_row(.num(nrow(duplicate_labels(L))), "duplicate table labels"),
    .rr_row(.num(nrow(guid_literals(tok, tabs))), "hardcoded record ids"),
    .rr_row(.num(nrow(script_disposals(tok, unique(na.omit(L$table)))$stores)),
            "in-app qvd generations", "STORE statements")))

  arow <- function(k, label) {
    all <- sum(ast$kind == k)
    .rr_row(.num(sum(ast$kind == k & !ast$used)), label,
            if (all) sprintf("%s declared", .num(all)) else "none declared")
  }
  assets <- .rr_section("App assets", c(
    arow("variable", "variables unused"),
    arow("dimension", "master dimensions unused"),
    arow("measure", "master measures unused")))

  html <- c(
    sprintf("<title>%s &mdash; Cloud migration assessment</title>", .h(title)),
    .rr_css,
    "<header>",
    sprintf("  <h1>Cloud migration assessment &mdash; %s</h1>", .h(title)),
    sprintf('  <div class="date">%s</div>',
            sub("^0", "", format(Sys.Date(), "%d %B %Y"))),
    "</header>", "",
    app, db, model, scr, assets,
    "<footer>",
    if (w) sprintf('  <p id="wild"><b>*</b> &mdash; %s name no file; their tables and fields are not counted.</p>',
                   .plural(wild, "wildcard load")) else NULL,
    "  <p><b>unused</b> &mdash; loaded and never referenced again, in script or visuals.",
    "     <b>unDROPped</b> &mdash; used to build other tables, not called by visuals, not dropped.</p>",
    "</footer>")

  writeLines(html, out, useBytes = TRUE)
  invisible(out)
}

main <- function(args) {
  scr <- if ("--script" %in% args) args[match("--script", args) + 1L] else NA
  args <- setdiff(args, c("--script", scr))
  out <- if ("--out" %in% args) args[match("--out", args) + 1L] else NA
  args <- setdiff(args, c("--out", out))
  if (!length(args)) stop("usage: Rscript render_report.R <app-dir> [--script <p>] [--out <p>]")
  cat("wrote ", render_report(args[1L],
        if (is.na(scr)) NULL else scr,
        if (is.na(out)) NULL else out), "\n", sep = "")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
