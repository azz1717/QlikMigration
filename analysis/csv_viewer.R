# csv_viewer.R — turn any csv into a self-contained HTML page you can read.
#
#     Rscript analysis/csv_viewer.R retargeting/path_map.csv
#
# Writes `<name>-view.html` next to nothing in particular (see --out) and
# nothing else. The page needs no server, no network and no spreadsheet: it
# opens from the filesystem, which is the only thing guaranteed to work on the
# VM (DESIGN §8.5, the return channel is a photograph).
#
# WHY THE DATA IS EMBEDDED AS JSON, NOT AS AN HTML TABLE
#
# Sorting and filtering need the values, not their markup. Embedding the rows
# once as JSON and building the table in the browser keeps the page a third of
# the size of the equivalent markup and means a filter never has to re-parse
# what it just rendered.
#
# WHAT IT DELIBERATELY DOES NOT DO
#
# No colour-coding by value, and no verdict column singled out. Which statuses
# are alarming is the reader's judgement, and a tool that paints `unmapped` red
# has made that judgement for them — the same rule the submittable report is
# built on (DESIGN §7.2). Sorting, filtering and hiding columns are navigation;
# they change what you look at, not what it means.
#
# Base R only. Not sourced by run_pipeline.R.

.cv_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
PROJECT_DIR <- if (length(.cv_file_arg)) {
  tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .cv_file_arg[1])))),
           error = function(e) NA)
} else NA
if (is.na(PROJECT_DIR) || !dir.exists(PROJECT_DIR)) {
  stop("could not determine the project folder from the running script's path. ",
       "Run this via 'Rscript analysis/csv_viewer.R'.", call. = FALSE)
}
setwd(PROJECT_DIR)

source("shared/json_write.R")
source("shared/csv_read.R")

# --- the page -------------------------------------------------------------

.cv_css <- '
:root {
  --bg: #ffffff; --fg: #1a1a1a; --muted: #6b6b6b; --line: #e0e0e0;
  --head: #f5f5f5; --zebra: #fafafa; --accent: #2b5797; --chip: #eef1f6;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #16181c; --fg: #e6e6e6; --muted: #9aa0a6; --line: #2c3038;
    --head: #1e2127; --zebra: #191c21; --accent: #8ab4f8; --chip: #232830;
  }
}
* { box-sizing: border-box; }
body {
  margin: 0; background: var(--bg); color: var(--fg);
  font: 14px/1.45 -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
}
header { padding: 14px 18px 10px; border-bottom: 1px solid var(--line); }
h1 { margin: 0 0 2px; font-size: 16px; font-weight: 600; }
h1 span { color: var(--muted); font-weight: 400; }
.meta { color: var(--muted); font-size: 12px; }
.bar { display: flex; gap: 10px; align-items: center; flex-wrap: wrap;
       padding: 10px 18px; border-bottom: 1px solid var(--line); }
input[type=search] {
  flex: 1 1 260px; min-width: 200px; padding: 7px 10px; font-size: 14px;
  color: var(--fg); background: var(--bg);
  border: 1px solid var(--line); border-radius: 6px;
}
input[type=search]:focus { outline: 2px solid var(--accent); outline-offset: -1px; }
button {
  padding: 6px 11px; font-size: 13px; color: var(--fg); background: var(--chip);
  border: 1px solid var(--line); border-radius: 6px; cursor: pointer;
}
button:hover { border-color: var(--accent); }
#count { color: var(--muted); font-size: 12px; white-space: nowrap; }
#cols { display: none; gap: 6px; flex-wrap: wrap; padding: 10px 18px;
        border-bottom: 1px solid var(--line); }
#cols.open { display: flex; }
#cols label { display: inline-flex; gap: 5px; align-items: center;
              padding: 3px 9px; background: var(--chip); border-radius: 12px;
              font-size: 12px; cursor: pointer; }
.wrap { overflow: auto; max-height: calc(100vh - 132px); }
table { border-collapse: collapse; width: 100%; }
th, td {
  text-align: left; vertical-align: top; padding: 6px 10px;
  border-bottom: 1px solid var(--line); font-size: 13px;
}
th {
  position: sticky; top: 0; z-index: 1; background: var(--head);
  cursor: pointer; user-select: none; white-space: nowrap; font-weight: 600;
}
th:hover { color: var(--accent); }
th .dir { color: var(--muted); font-weight: 400; }
tbody tr:nth-child(even) { background: var(--zebra); }
tbody tr:hover { background: var(--chip); }
td { max-width: 460px; overflow-wrap: anywhere; }
td.n { text-align: right; font-variant-numeric: tabular-nums; }
td.empty { color: var(--muted); }
.rownum { color: var(--muted); font-variant-numeric: tabular-nums;
          text-align: right; user-select: none; }
mark { background: #ffe58a; color: #1a1a1a; border-radius: 2px; }
#none { padding: 24px 18px; color: var(--muted); display: none; }
footer { padding: 10px 18px 18px; color: var(--muted); font-size: 12px; }
'

.cv_js <- '
var Q = "", SORT = -1, DIR = 1, HIDDEN = {};
var tbody = document.querySelector("tbody"),
    thead = document.querySelector("thead tr"),
    box   = document.getElementById("q"),
    cnt   = document.getElementById("count"),
    none  = document.getElementById("none");

function isNum(s) { return s !== "" && s !== "NA" && !isNaN(Number(s)); }

function matches(row) {
  if (!Q) return true;
  for (var i = 0; i < row.length; i++)
    if (COLS[i] in HIDDEN === false && row[i].toLowerCase().indexOf(Q) !== -1) return true;
  return false;
}

function mark(text) {
  if (!Q) return document.createTextNode(text);
  var frag = document.createDocumentFragment(), low = text.toLowerCase(), at = 0, hit;
  while ((hit = low.indexOf(Q, at)) !== -1) {
    if (hit > at) frag.appendChild(document.createTextNode(text.slice(at, hit)));
    var m = document.createElement("mark");
    m.textContent = text.slice(hit, hit + Q.length);
    frag.appendChild(m);
    at = hit + Q.length;
  }
  frag.appendChild(document.createTextNode(text.slice(at)));
  return frag;
}

function draw() {
  var rows = ROWS.filter(matches);
  if (SORT >= 0) {
    var numeric = rows.every(function (r) { return r[SORT] === "" || r[SORT] === "NA" || isNum(r[SORT]); });
    rows = rows.slice().sort(function (a, b) {
      var x = a[SORT], y = b[SORT];
      if (numeric) { x = x === "" || x === "NA" ? -Infinity : Number(x);
                     y = y === "" || y === "NA" ? -Infinity : Number(y);
                     return (x - y) * DIR; }
      return x.localeCompare(y, undefined, { numeric: true }) * DIR;
    });
  }
  // One detached build, one insert: rebuilding 2,600 rows inside the live
  // table makes the browser lay the page out on every append.
  var frag = document.createDocumentFragment();
  rows.forEach(function (row, n) {
    var tr = document.createElement("tr");
    var rn = document.createElement("td");
    rn.className = "rownum"; rn.textContent = String(n + 1);
    tr.appendChild(rn);
    row.forEach(function (val, i) {
      if (COLS[i] in HIDDEN) return;
      var td = document.createElement("td");
      if (val === "" || val === "NA") { td.className = "empty"; td.textContent = "-"; }
      else { if (isNum(val)) td.className = "n"; td.appendChild(mark(val)); }
      tr.appendChild(td);
    });
    frag.appendChild(tr);
  });
  tbody.textContent = "";
  tbody.appendChild(frag);
  cnt.textContent = rows.length === ROWS.length
    ? ROWS.length + " rows"
    : rows.length + " of " + ROWS.length + " rows";
  none.style.display = rows.length ? "none" : "block";
  heads();
}

function heads() {
  thead.textContent = "";
  var blank = document.createElement("th"); blank.textContent = "#";
  thead.appendChild(blank);
  COLS.forEach(function (name, i) {
    if (name in HIDDEN) return;
    var th = document.createElement("th");
    th.textContent = name;
    if (SORT === i) {
      var d = document.createElement("span");
      d.className = "dir"; d.textContent = DIR > 0 ? "  ^" : "  v";
      th.appendChild(d);
    }
    th.onclick = function () {
      if (SORT === i) DIR = -DIR; else { SORT = i; DIR = 1; }
      draw();
    };
    thead.appendChild(th);
  });
}

function chips() {
  var box = document.getElementById("cols");
  COLS.forEach(function (name, i) {
    var lab = document.createElement("label");
    var cb = document.createElement("input");
    cb.type = "checkbox"; cb.checked = true;
    cb.onchange = function () {
      if (cb.checked) delete HIDDEN[name]; else HIDDEN[name] = 1;
      draw();
    };
    lab.appendChild(cb);
    lab.appendChild(document.createTextNode(name));
    box.appendChild(lab);
  });
}

box.oninput = function () { Q = box.value.trim().toLowerCase(); draw(); };
document.getElementById("clear").onclick = function () { box.value = ""; Q = ""; draw(); };
document.getElementById("toggle").onclick = function () {
  document.getElementById("cols").classList.toggle("open");
};
document.onkeydown = function (e) {
  if (e.key === "/" && document.activeElement !== box) { e.preventDefault(); box.focus(); }
  if (e.key === "Escape" && document.activeElement === box) { box.value = ""; Q = ""; draw(); box.blur(); }
};
chips();
draw();
'

#' One csv -> one self-contained HTML page.
#'
#' @param path csv to read. @param out html to write, defaulting to
#'   `<stem>-view.html` beside the csv. @param title page heading.
#' @return the path written, invisibly.
csv_viewer <- function(path, out = NULL, title = NULL) {
  if (!file.exists(path))
    stop("no such file: '", path, "'", call. = FALSE)
  d <- read_csv_any(path)
  if (!ncol(d))
    stop("'", path, "' has no columns to show", call. = FALSE)
  if (is.null(out))
    out <- file.path(dirname(path), paste0(sub("[.][^.]*$", "", basename(path)), "-view.html"))
  if (is.null(title)) title <- basename(path)

  # Rows as arrays, not objects: the column names are the same for every row
  # and repeating them 2,600 times is most of the file size.
  rows <- vapply(seq_len(nrow(d)), function(i)
    paste0("[", paste(json_str(unlist(d[i, ], use.names = FALSE)), collapse = ","), "]"),
    character(1))

  html <- c(
    "<!DOCTYPE html>", "<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    paste0("<title>", json_escape(title), "</title>"),
    paste0("<style>", .cv_css, "</style>"), "</head><body>",
    "<header>",
    paste0("<h1>", json_escape(title), " <span>", nrow(d), " rows, ", ncol(d),
           " columns</span></h1>"),
    paste0("<div class=\"meta\">", json_escape(normalizePath(path, winslash = "/")),
           " &middot; rendered ", format(Sys.time(), "%Y-%m-%d %H:%M"), "</div>"),
    "</header>",
    "<div class=\"bar\">",
    "<input type=\"search\" id=\"q\" placeholder=\"Filter rows - type to match any visible column, / to focus\">",
    "<button id=\"clear\">Clear</button>",
    "<button id=\"toggle\">Columns</button>",
    "<span id=\"count\"></span>",
    "</div>",
    "<div id=\"cols\"></div>",
    "<div class=\"wrap\"><table><thead><tr></tr></thead><tbody></tbody></table></div>",
    "<div id=\"none\">Nothing matches that filter.</div>",
    "<footer>Sort by clicking a column. Filtering searches visible columns only.</footer>",
    "<script>",
    paste0("var COLS = [", paste(json_str(names(d)), collapse = ","), "];"),
    paste0("var ROWS = [", paste(rows, collapse = ",\n"), "];"),
    .cv_js,
    "</script>", "</body></html>")

  con <- file(out, open = "wt", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(html, con)
  invisible(out)
}

# --- CLI -------------------------------------------------------------------

main <- function(args) {
  if (!length(args)) {
    cat("usage: Rscript analysis/csv_viewer.R <file.csv> [--out <file.html>] [--title <text>]\n")
    return(invisible(NULL))
  }
  getopt <- function(flag, default) {
    i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[i + 1L]
  }
  path <- args[1]
  out  <- csv_viewer(path, getopt("--out", NULL), getopt("--title", NULL))
  d_rows <- length(readLines(out, warn = FALSE))
  cat("wrote ", out, " (", d_rows, " lines, ",
      format(file.size(out) / 1024, digits = 1, nsmall = 0), " KB)\n", sep = "")
  cat("open it in a browser: file:///", gsub("\\\\", "/", normalizePath(out)), "\n", sep = "")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
