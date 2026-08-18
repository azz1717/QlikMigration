# script_debt.R — the signals the report calls out as debt.
#
# Phase 2 (DESIGN §7.1). NOT part of the styling pipeline; nothing in
# run_pipeline.R sources this. Base R only.
#
# Admission test for anything in here (Adam, 2026-08-18): **if retargeting
# fixes it, it is not report material.** A DEV connection string is fixed by
# pointing it somewhere valid, which is literally phase 3's job, so it is not
# flagged here. A direct database call cannot be retargeted away — it needs
# rearchitecting into a qvd layer — so it is.
#
# What this file does NOT attempt: hardcoded values in general. Adam
# 2026-08-18: `IF([Status] <> 'Completed')` is fine and far commoner than
# `IF([Organisation] = 'ABC Tour Guides')`, and hardcoded dates are legitimate
# as often as not. A detector whose false positives outnumber its true ones
# teaches the reader to skim past the section. Only the one sub-case that can
# be called with confidence is detected — see below.

source("qlik_tokenizer.R")

# A GUID in a load expression is never business logic; it is a patched record
# id (app2 lines 242 and 244, both inside IF()). This is the whole of the
# hardcoded-value detection, deliberately.
.SD_GUID <- "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$"

# Comment text that looks like script rather than prose. Two tests, because
# one line of a commented-out load rarely carries a keyword.
.SD_CODEISH <- "(?i)\\b(load|resident|from|drop\\s+table|store|join|where|inline|autogenerate)\\b"

# SHAPE, for the lines between the keywords: a field list entry ends in a
# comma, a delimited name opens with a bracket or quote, and an expression
# carries parentheses or an `=`. Measured on app-unbuilt: this recovers 1,911
# of the 1,978 comment blocks that keyword-plus-contiguity still called prose,
# and a 20-line sample of the 67 it leaves behind is genuine commentary
# ("Leave out the tiny trips"). Prose carrying a bracket or an `=` is counted
# as code, which is the tolerable direction: the metric is how much disabled
# script a reader has to wade through.
.SD_CODESHAPE <- "[,;][ \t]*$|^[\\s]*[\\\\[\"]|[()=]"

#' Hardcoded record identifiers.
#' @return data.frame(line, tab, literal)
guid_literals <- function(tokens, tabs = NULL) {
  i <- which(tokens$type == "SQUOTE")
  if (!length(i)) return(data.frame(line = integer(0), tab = character(0),
                                    literal = character(0), stringsAsFactors = FALSE))
  body <- substr(tokens$text[i], 2L, nchar(tokens$text[i]) - 1L)
  k <- i[grepl(.SD_GUID, body)]
  if (!length(k)) return(data.frame(line = integer(0), tab = character(0),
                                    literal = character(0), stringsAsFactors = FALSE))
  data.frame(line = tokens$line[k],
             tab = if (is.null(tabs)) NA_character_
                   else vapply(tokens$line[k], function(x) .sl_tab_at(tabs, x), character(1)),
             literal = substr(tokens$text[k], 2L, nchar(tokens$text[k]) - 1L),
             stringsAsFactors = FALSE)
}

#' Commented-out code, measured in lines.
#'
#' Lines rather than blocks because legibility is the cost: a reader wading
#' through 500 lines of disabled script cares about the wading, not how many
#' separate blocks it came in.
#'
#' CONTIGUITY IS THE WHOLE ALGORITHM (Adam 2026-08-18). A `//` comment is one
#' token PER LINE, so a commented-out 60-line LOAD matches a keyword on the
#' two lines carrying LOAD and FROM and on none of the 58 field lines between
#' them. Scoring each line alone reported 513 of 13,869 for app-unbuilt and
#' called the other 2,407 comment lines prose; a 25-line sample of that
#' "prose" was 24 lines of commented-out field lists, inline data rows and
#' expressions. Adjacent comment lines are therefore one block, and a block
#' any part of which looks like script is disabled script entire.
#'
#' The residue is genuine commentary, and on this codebase there is very
#' little of it — which is itself the finding.
#' @return list(blocks = data.frame(line, tab, n_lines), total_lines,
#'   script_lines, prose_lines)
# Comment text with its marker and indentation stripped, so the shape test
# sees the line as it was written rather than the `//` in front of it.
.sd_comment_body <- function(x)
  sub("^[ \t]*(//+|/[*]|--)[ \t]*", "", gsub("[\\r\\n]", " ", x))

commented_out_code <- function(tokens, tabs = NULL) {
  cm <- which(tokens$type == "COMMENT" & !grepl("^///\\$tab", tokens$text))
  if (!length(cm))
    return(list(blocks = data.frame(line = integer(0), tab = character(0),
                                    n_lines = integer(0), stringsAsFactors = FALSE),
                total_lines = 0L, script_lines = max(tokens$line), prose_lines = 0L))

  # Two comment tokens belong to the same block when nothing but trivia sits
  # between them — which for consecutive `//` lines is the newline itself.
  solid <- !tokens$type %in% c("WS", "VOID")
  run   <- cumsum(c(TRUE, vapply(seq_along(cm)[-1L], function(k)
             any(solid[(cm[k - 1L] + 1L):(cm[k] - 1L)]), logical(1))))

  nl    <- vapply(strsplit(tokens$text[cm], "
"), length, integer(1))
  hit   <- grepl(.SD_CODEISH, tokens$text[cm], perl = TRUE) |
           grepl(.SD_CODESHAPE, .sd_comment_body(tokens$text[cm]), perl = TRUE)
  code  <- run %in% unique(run[hit])

  keep  <- cm[code]
  first <- !duplicated(run[code])
  list(blocks = data.frame(
         line = tokens$line[keep][first],
         tab = if (is.null(tabs)) rep(NA_character_, sum(first))
               else vapply(tokens$line[keep][first],
                           function(x) .sl_tab_at(tabs, x), character(1)),
         n_lines = as.integer(tapply(nl[code], run[code], sum)),
         stringsAsFactors = FALSE),
       total_lines = sum(nl[code]), script_lines = max(tokens$line),
       prose_lines = sum(nl[!code]))
}

#' Table names built by more than one independent load.
#'
#' Reported as an observation, NOT a severity. Whether this is intentional
#' concatenation or an accident is not determinable from the script, and
#' Qlik's exact behaviour for a repeated explicit label is not something this
#' repo has verified — see STATE.md.
#' @return data.frame(table, n_loads, tabs, lines)
duplicate_labels <- function(loads) {
  base <- loads[loads$producer_kind == "table" & !is.na(loads$table), ]
  dup  <- names(which(table(base$table) > 1L))
  if (!length(dup)) return(data.frame(table = character(0), n_loads = integer(0),
                                      tabs = character(0), lines = character(0),
                                      stringsAsFactors = FALSE))
  do.call(rbind, lapply(dup, function(nm) {
    x <- base[base$table == nm, ]
    data.frame(table = nm, n_loads = nrow(x),
               tabs  = paste(unique(na.omit(x$tab)), collapse = "; "),
               lines = paste(x$line_start, collapse = ", "),
               stringsAsFactors = FALSE)
  }))
}


#' Where the SQL SELECT statements actually go.
#'
#' Token-based, never raw text: a commented-out SELECT lives entirely inside
#' one COMMENT token, so its FROM never becomes a token and cannot be counted.
#' A raw grep over app-unbuilt's 13,869 lines cannot make that distinction and
#' 3.7% of the file is disabled script.
#'
#' Recognises the two forms a Qlik SQL FROM takes: a qualified object name
#' (`"SERVER".schema."Object"`, parts optionally quoted) and a connector
#' function call (`Closest(...)`, `TravelAreas(...)`). They are different
#' migration problems — an object needs a Cloud data connection, a connector
#' call needs the connector to exist on Cloud at all — so `kind` separates
#' them rather than the reader having to.
#' @return data.frame(line, tab, kind, server, schema, object, target)
sql_targets <- function(tokens, tabs = NULL) {
  empty <- data.frame(line = integer(0), tab = character(0), kind = character(0),
                      server = character(0), schema = character(0),
                      object = character(0), target = character(0),
                      stringsAsFactors = FALSE)
  lower <- tolower(tokens$text)
  insel <- in_select_region(tokens$type, lower)
  from  <- which(insel & tokens$type == "WORD" & lower == "from")
  if (!length(from)) return(empty)

  n <- nrow(tokens)
  out <- lapply(from, function(i) {
    j <- i + 1L
    while (j <= n && tokens$type[j] %in% c("WS", "COMMENT", "VOID")) j <- j + 1L
    parts <- character(0)
    kind <- "object"
    while (j <= n) {
      ty <- tokens$type[j]
      if (ty == "LPAREN") { kind <- "connector"; break }
      if (!ty %in% c("WORD", "DQUOTE", "BRACKET", "OTHER")) break
      if (ty == "OTHER" && tokens$text[j] != ".") break
      parts <- c(parts, tokens$text[j])
      j <- j + 1L
    }
    if (!length(parts)) return(NULL)
    target <- paste0(parts, collapse = "")
    seg <- .sd_split_qualified(target)
    data.frame(line = tokens$line[i],
               tab = if (is.null(tabs)) NA_character_ else .sl_tab_at(tabs, tokens$line[i]),
               kind = kind,
               server = if (kind == "connector") NA_character_ else seg[1L],
               schema = if (kind == "connector") NA_character_ else seg[2L],
               object = if (kind == "connector") target else seg[3L],
               target = target, stringsAsFactors = FALSE)
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(empty)
  do.call(rbind, out)
}

# Split `"SERVER".schema."Object"` into its three trailing parts. The
# tokenizer hands back `abs.` as one WORD (the dot is a word character in the
# token pattern), so the split is on text, after unquoting, and pads from the
# RIGHT: an unqualified `Trip` is an object, not a server.
.sd_split_qualified <- function(x) {
  p <- regmatches(x, gregexpr("\"[^\"]*\"|[^\".]+", x, perl = TRUE))[[1L]]
  p <- p[nzchar(p)]
  p <- sub("^\"(.*)\"$", "\\1", p)
  if (!length(p)) return(c(NA_character_, NA_character_, NA_character_))
  p <- tail(p, 3L)
  c(rep(NA_character_, 3L - length(p)), p)
}

main <- function(args) {
  if (!length(args)) stop("usage: Rscript script_debt.R <script>")
  source("script_loads.R")
  tok  <- read_qlik_script(args[1L])
  tabs <- .sl_tabs(tok)
  l    <- script_loads(tok)$loads

  g <- guid_literals(tok, tabs)
  c_ <- commented_out_code(tok, tabs)
  d <- duplicate_labels(l)
  s <- sql_targets(tok, tabs)

  cat(args[1L], "\n", sep = "")
  cat("  direct database calls: ", sum(l$source_kind == "select"), "\n", sep = "")
  cat("  hardcoded record ids (GUIDs): ", nrow(g), "\n", sep = "")
  if (nrow(g)) print(g, row.names = FALSE)
  cat("  commented-out code: ", c_$total_lines, " lines of ", c_$script_lines,
      " (", round(100 * c_$total_lines / c_$script_lines, 1), "%)\n", sep = "")
  cat("  duplicate table labels: ", nrow(d), "\n", sep = "")
  if (nrow(d)) print(d, row.names = FALSE)
  obj <- s[s$kind == "object", ]
  con <- s[s$kind == "connector", ]
  cat(paste0("  SQL object references: ", nrow(obj), " (",
             length(unique(obj$target)), " distinct) across ",
             length(unique(na.omit(obj$server))), " server(s); connector calls: ",
             nrow(con)), fill = TRUE)
  if (nrow(obj)) {
    byloc <- aggregate(list(refs = obj$target),
                       by = list(server = obj$server, schema = obj$schema), FUN = length)
    print(byloc[order(-byloc$refs), ], row.names = FALSE)
  }
  if (nrow(con)) print(as.data.frame(table(con$object, dnn = "connector")), row.names = FALSE)
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
