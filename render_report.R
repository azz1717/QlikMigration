# render_report.R — phase 2 step 4b: the submittable document.
#
# NOT part of the styling pipeline; nothing in run_pipeline.R sources this.
# Base R only, self-contained HTML only: no external stylesheet, script, font
# or image, because nothing can be assumed about the machine that opens it.
# Collapsibles are native <details>, so no script is needed.
#
# ONE question: how much work is this app to migrate? (DESIGN §7.2.)
#
# The information design is the deliverable, not the detections. First draft
# gave every detector a section and every section its whole table — a data
# dump with headings, rejected by Adam 2026-08-18 as information overload.
# Three rules came out of that and they govern this file:
#
#   1. THE UNIT IS THE TAB, not the finding. A developer navigates a Qlik
#      script by `///$tab` and works through one at a time. 40 unused tables
#      listed flat hides that 15 of them sit in 10 tabs with nothing used in
#      them at all — which is one cheap decision, not 15 investigations.
#   2. COUNT WORK, NOT FINDINGS. 24 database calls is not 24 jobs: 12 are in
#      a single tab. "6 tabs to visit" is the effort estimate; 24 is trivia.
#   3. NOTHING IS VISIBLE UNTIL ASKED FOR, bar the verdict. Everything else
#      is one click away and closed by default.

source("usage_report.R")
source("script_debt.R")

.h <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.plural <- function(n, one, many = paste0(one, "s"))
  paste0(n, " ", if (n == 1L) one else many)

.tbl <- function(df, cols, labels) {
  if (is.null(df) || !nrow(df)) return("")
  paste0("<div class=scroll><table><thead><tr>",
         paste0("<th>", .h(labels), "</th>", collapse = ""), "</tr></thead><tbody>",
         paste(apply(df[, cols, drop = FALSE], 1L, function(r)
           paste0("<tr>", paste0("<td>", .h(r), "</td>", collapse = ""), "</tr>")),
           collapse = ""), "</tbody></table></div>")
}

.css <- "
:root{--fg:#161616;--dim:#6b6b6b;--line:#e2e2e0;--bg:#fff;--soft:#f7f7f5;
  --fix:#9b2222;--decide:#8a5a12;--del:#2f6b46}
*{box-sizing:border-box}
body{margin:0 auto;padding:2.5rem 1.5rem 5rem;max-width:52rem;background:var(--bg);color:var(--fg);
  font:15px/1.6 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif}
h1{font-size:1.5rem;margin:0}
.sub{color:var(--dim);font-size:.88rem;margin:.15rem 0 2rem}
h2{font-size:.78rem;letter-spacing:.09em;text-transform:uppercase;color:var(--dim);
  margin:2.8rem 0 .7rem;font-weight:600}
.verdict{font-size:1.12rem;line-height:1.5;margin:0 0 1.5rem}
.verdict b{font-weight:600}
.figs{display:flex;gap:1.6rem;flex-wrap:wrap;margin:0 0 1.4rem;padding:1.1rem 0;
  border-top:1px solid var(--line);border-bottom:1px solid var(--line)}
.fig{flex:1 1 9rem}
.fig .n{font-size:2rem;font-weight:600;line-height:1}
.fig .l{font-size:.86rem;font-weight:600;margin-top:.2rem}
.fig .s{font-size:.78rem;color:var(--dim);margin-top:.1rem}
.fig.fix .n{color:var(--fix)} .fig.decide .n{color:var(--decide)} .fig.del .n{color:var(--del)}
.dead{background:var(--soft);border:1px solid var(--line);border-radius:7px;padding:.9rem 1.1rem;margin:.6rem 0}
.dead ul{margin:.5rem 0 0;padding-left:1.1rem;columns:2;font-size:.9rem}
.dead li{margin:.1rem 0;break-inside:avoid}
details{border-bottom:1px solid var(--line)}
details summary{cursor:pointer;padding:.6rem .2rem;display:flex;align-items:baseline;
  gap:.5rem;flex-wrap:wrap;list-style:none}
details summary::-webkit-details-marker{display:none}
summary::before{content:'\\25B8';color:var(--dim);font-size:.75rem;transition:transform .12s}
details[open] summary::before{content:'\\25BE'}
.tabname{font-weight:600;flex:1 1 auto}
.badge{font-size:.72rem;padding:.1rem .45rem;border-radius:99px;white-space:nowrap;
  border:1px solid currentColor;font-weight:600}
.b-fix{color:var(--fix)} .b-decide{color:var(--decide)} .b-del{color:var(--del)}
.body{padding:.2rem 0 1rem 1.1rem}
.body h4{font-size:.82rem;margin:.9rem 0 .25rem;color:var(--dim);text-transform:uppercase;
  letter-spacing:.05em}
.scroll{overflow-x:auto}
table{border-collapse:collapse;width:100%;font-size:.85rem;margin:.2rem 0 .4rem}
th,td{text-align:left;padding:.28rem .55rem;border-bottom:1px solid var(--line);vertical-align:top}
th{color:var(--dim);font-weight:600;white-space:nowrap;font-size:.78rem}
pre{background:var(--soft);border:1px solid var(--line);border-radius:5px;padding:.45rem .65rem;
  overflow-x:auto;font-size:.78rem;margin:.3rem 0}
.q{color:var(--dim);font-size:.85rem;margin:.2rem 0 .6rem}
footer{margin-top:3rem;font-size:.8rem;color:var(--dim)}
footer details{border:0} footer summary{padding:.3rem 0}
footer p{margin:.4rem 0}
@media print{details{break-inside:avoid} details>.body{display:block!important}}
"

.badges <- function(fix, dec, del) paste0(
  if (fix) sprintf("<span class='badge b-fix'>%d to fix</span>", fix) else "",
  if (dec) sprintf("<span class='badge b-decide'>%d to decide</span>", dec) else "",
  if (del) sprintf("<span class='badge b-del'>%d to delete</span>", del) else "")

#' Render one app's migration review.
render_report <- function(app_dir, script_path = file.path(app_dir, "script.qvs"),
                          out = paste0(basename(app_dir), "-report.html")) {
  ur   <- usage_report(app_dir, script_path)
  tok  <- read_qlik_script(script_path)
  sl   <- script_loads(tok)
  tabs <- .sl_tabs(tok)
  L    <- sl$loads

  guid <- guid_literals(tok, tabs)
  com  <- commented_out_code(tok, tabs)
  dup  <- duplicate_labels(L)

  t <- ur$tables
  t$tab <- vapply(t$table, function(nm) {
    x <- L$tab[!is.na(L$table) & L$table == nm]
    if (!length(x)) NA_character_ else x[1L] }, character(1))
  t$lines <- paste0(t$line_start, "–", t$line_end)

  db  <- L[L$source_kind == "select", ]
  inl <- sl$inlines
  all_tabs <- unique(na.omit(L$tab))

  # --- per tab -------------------------------------------------------------
  per <- function(tb) list(
    db     = db[!is.na(db$tab) & db$tab == tb, ],
    guid   = guid[!is.na(guid$tab) & guid$tab == tb, ],
    inl    = inl[!is.na(inl$tab) & inl$tab == tb, ],
    dup    = dup[grepl(tb, dup$tabs, fixed = TRUE), ],
    unused = t[!is.na(t$tab) & t$tab == tb & t$category == "unreferenced", ],
    undrop = t[!is.na(t$tab) & t$tab == tb & t$category == "build-only, retained", ],
    total  = sum(!is.na(t$tab) & t$tab == tb))

  P <- lapply(all_tabs, per); names(P) <- all_tabs
  # Major and minor breaches are NOT the same figure. A database call blocks
  # migration outright; a hardcoded record id keeps the app in `dev` status and
  # must go before production sign-off (Adam 2026-08-18). Folding them together
  # told app2 it had "2 blocking items", which is wrong and alarming.
  nfix <- vapply(P, function(p) nrow(p$db), integer(1))
  nmin <- vapply(P, function(p) nrow(p$guid), integer(1))
  ndec <- vapply(P, function(p) nrow(p$inl) + nrow(p$dup), integer(1))
  ndel <- vapply(P, function(p) nrow(p$unused) + nrow(p$undrop), integer(1))

  # A tab with tables, none of them used: delete the tab, not its tables.
  # One decision instead of N investigations — the cheapest work in the report.
  dead <- all_tabs[vapply(P, function(p)
    p$total > 0L && nrow(p$unused) == p$total, logical(1))]
  rest <- all_tabs[!(all_tabs %in% dead)]

  # Three groups, because a tab needing a rearchitect and a tab with one spare
  # table are not the same errand. Cleanup-only tabs are the long tail and go
  # behind a single expander — 42 sibling rows is the overload being fixed.
  g_block <- rest[nfix[rest] > 0L]
  g_judge <- rest[nfix[rest] == 0L & (ndec[rest] + nmin[rest]) > 0L]
  g_clean <- rest[nfix[rest] == 0L & (ndec[rest] + nmin[rest]) == 0L & ndel[rest] > 0L]
  g_block <- g_block[order(-nfix[g_block])]
  g_judge <- g_judge[order(-ndec[g_judge])]
  g_clean <- g_clean[order(-ndel[g_clean])]

  # --- verdict -------------------------------------------------------------
  worst   <- if (nrow(db)) names(sort(table(db$tab), decreasing = TRUE))[1L] else NA
  worst_n <- if (nrow(db)) max(table(db$tab)) else 0L

  say <- c(
    if (sum(nfix))
      sprintf("<b>%s</b> must be rearchitected before this app can run on Cloud%s.",
              .plural(sum(nfix), "database call"),
              if (!is.na(worst) && worst_n > 1L)
                sprintf(" &mdash; %d of them in <b>%s</b> alone", worst_n, .h(worst)) else "")
    else "Nothing here blocks this app from running on Cloud.",
    if (length(dead))
      sprintf("<b>%s</b> of %d contain nothing the app uses and look deletable outright.",
              .plural(length(dead), "tab"), length(all_tabs)),
    if (sum(ndec))
      sprintf("%s %s a human judgement no tool can make.",
              .plural(sum(ndec), if (sum(nfix)) "further item" else "item"),
              if (sum(ndec) == 1L) "needs" else "need"),
    if (length(g_clean))
      sprintf("The remaining %s need only cleanup.", .plural(length(g_clean), "tab")),
    if (sum(nmin))
      sprintf("%s %s before production sign-off, though %s not block migration.",
              .plural(sum(nmin), "hardcoded record id"),
              if (sum(nmin) == 1L) "must go" else "must go",
              if (sum(nmin) == 1L) "it does" else "they do"))

  figs <- paste0(
    sprintf("<div class='fig fix'><div class=n>%d</div><div class=l>to fix</div><div class=s>blocks migration &middot; %s</div></div>",
            sum(nfix), .plural(sum(nfix > 0L), "tab")),
    sprintf("<div class='fig decide'><div class=n>%d</div><div class=l>to decide</div><div class=s>needs judgement &middot; %s</div></div>",
            sum(ndec), .plural(sum(ndec > 0L), "tab")),
    sprintf("<div class='fig del'><div class=n>%d</div><div class=l>to delete</div><div class=s>unused &middot; %s</div></div>",
            sum(ndel), .plural(sum(ndel > 0L), "tab")))

  # --- per-tab detail ------------------------------------------------------
  block <- function(tb) {
    p <- P[[tb]]; b <- ""
    if (nrow(p$db))
      b <- paste0(b, "<h4>Direct database calls</h4><p class=q>Not permitted on Cloud; each needs moving to a qvd layer.</p>",
                  .tbl(transform(p$db, lines = paste0(line_start, "–", line_end)),
                       c("table", "lines"), c("Table", "Lines")))
    if (nrow(p$guid))
      b <- paste0(b, "<h4>Hardcoded record ids</h4>",
                  .tbl(p$guid, c("line", "literal"), c("Line", "GUID")))
    if (nrow(p$inl)) {
      b <- paste0(b, "<h4>Inline loads</h4><p class=q>Fine for mapping and crosstabs, not for importing data. The content decides.</p>",
        paste0(vapply(seq_len(nrow(p$inl)), function(i) sprintf(
          "<details><summary><span class=tabname>%s</span><span class=q>%s rows &middot; lines %d&ndash;%d</span></summary><div class=body><p class=q><b>Fields:</b> %s</p><pre>%s</pre><p class=q>First 10 of %d rows.</p></div></details>",
          .h(p$inl$table[i]), .h(p$inl$n_rows[i]), p$inl$line_start[i], p$inl$line_end[i],
          .h(p$inl$header[i]), .h(p$inl$sample[i]), p$inl$n_rows[i]), character(1)), collapse = ""))
    }
    if (nrow(p$dup))
      b <- paste0(b, "<h4>Duplicate table labels</h4><p class=q>Built by more than one load. Intentional concatenation or accident cannot be told from the script.</p>",
                  .tbl(p$dup, c("table", "tabs", "lines"), c("Table", "Tabs", "At lines")))
    if (nrow(p$unused))
      b <- paste0(b, "<h4>Tables never used</h4>",
                  .tbl(p$unused, c("table", "n_fields", "sources", "lines"),
                       c("Table", "Fields", "Source", "Lines")))
    if (nrow(p$undrop))
      b <- paste0(b, "<h4>Tables never dropped</h4><p class=q>Built, used to make others, then held in memory for the rest of the reload.</p>",
                  .tbl(p$undrop, c("table", "n_fields", "sources", "lines"),
                       c("Table", "Fields", "Source", "Lines")))
    sprintf("<details><summary><span class=tabname>%s</span>%s</summary><div class=body>%s</div></details>",
            .h(tb), .badges(nfix[[tb]] + nmin[[tb]], ndec[[tb]], ndel[[tb]]), b)
  }

  group <- function(tabs, heading, blurb, wrap = FALSE) {
    if (!length(tabs)) return("")
    inner <- paste(vapply(tabs, block, character(1)), collapse = "")
    if (!wrap)
      return(paste0("<h2>", heading, "</h2><p class=q>", blurb, "</p>", inner))
    paste0("<h2>", heading, "</h2><details><summary><span class=tabname>",
           .plural(length(tabs), "tab"), " with cleanup only</span>",
           .badges(0L, 0L, sum(ndel[tabs])),
           "</summary><div class=body><p class=q>", blurb, "</p>", inner,
           "</div></details>")
  }

  deadblock <- if (length(dead)) paste0(
    "<div class=dead><b>", .plural(length(dead), "tab"),
    "</b> where no table is used by the app &mdash; ",
    sum(vapply(dead, function(d) P[[d]]$total, integer(1))),
    " tables in total. Deleting the tab is one decision rather than a table-by-table review.",
    "<ul>", paste0(vapply(dead, function(d) sprintf("<li>%s <span class=q>(%s)</span></li>",
      .h(d), .plural(P[[d]]$total, "table")), character(1)), collapse = ""), "</ul></div>") else ""

  nfld <- sum(ur$fields$field_finding)

  html <- paste0(
    "<title>", .h(basename(app_dir)), " migration review</title><style>", .css, "</style>",
    "<h1>", .h(basename(app_dir)), "</h1>",
    "<p class=sub>Migration review &middot; ", .h(format(Sys.Date(), "%d %B %Y")),
    " &middot; ", nrow(t), " tables across ", length(all_tabs), " tabs &middot; ",
    com$script_lines, " lines</p>",

    "<p class=verdict>", paste(Filter(nchar, say), collapse = " "), "</p>",
    "<div class=figs>", figs, "</div>",

    if (length(dead)) paste0("<h2>Whole tabs that look dead</h2>", deadblock) else "",

    group(g_block, "Blocking migration",
          "These carry database calls that will not run on Cloud. Expand for the detail."),
    group(g_judge, "Needing a judgement call",
          "Nothing here can be decided by a tool &mdash; someone has to look."),
    group(g_clean, "Cleanup only",
          "Unused or undropped tables, nothing blocking. Safe to leave for later.",
          wrap = TRUE),

    "<footer><h2>Footnotes</h2>",
    sprintf("<details><summary>%s loaded but never used anywhere</summary><div class=body><p>Noted, not urged: this migration carries apps across as they are, and field-level tidying is not the priority. It is also the least certain output here &mdash; a field counts as used if its name appears anywhere in the app, including as a bare word.</p></div></details>",
            .plural(nfld, "field")),
    sprintf("<details><summary>%s of commented-out script (%.1f%% of the file)</summary><div class=body><p>Never executed, so no runtime cost &mdash; purely what a reader has to wade through.</p>%s</div></details>",
            .plural(com$total_lines, "line"),
            100 * com$total_lines / com$script_lines,
            .tbl(head(com$blocks[order(-com$blocks$n_lines), ], 10),
                 c("tab", "line", "n_lines"), c("Tab", "At line", "Lines"))),
    sprintf("<details><summary>What this review covers, and what it cannot</summary><div class=body><p>%sA table counts as used if any of its fields is named anywhere in the app, including as a bare word, so the deletable list is a floor rather than a total.</p><p>Connection validity is out of scope by design &mdash; that is the retargeting phase's job. Hardcoded values are not detected in general, only GUIDs: a broader rule flags far more legitimate code than real problems.</p><p>Nothing here has been changed, and no removal is implied to be safe without testing.</p></div></details>",
            if (nrow(ur$undetermined))
              sprintf("<b>%s could not be assessed at all</b> &mdash; they read every qvd matching a wildcard path, so their tables and fields are outside the script. ",
                      .plural(nrow(ur$undetermined), "load")) else ""),
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
        if (is.na(scr)) file.path(args[1L], "script.qvs") else scr,
        if (is.na(out)) paste0(basename(args[1L]), "-report.html") else out), "\n", sep = "")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
