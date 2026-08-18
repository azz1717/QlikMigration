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

# Comment text that looks like script rather than prose. Approximate by
# design: the metric is VOLUME of commented-out code, and a few misjudged
# blocks either way do not change what a reader takes from "513 of 13,869".
.SD_CODEISH <- "(?i)\\b(load|resident|from|drop\\s+table|store|join|where|inline|autogenerate)\\b"

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
#' through 513 lines of disabled script cares about the wading, not how many
#' separate blocks it came in.
#' @return list(blocks = data.frame(line, tab, n_lines), total_lines, script_lines)
commented_out_code <- function(tokens, tabs = NULL) {
  i <- which(tokens$type == "COMMENT" & grepl(.SD_CODEISH, tokens$text, perl = TRUE) &
             !grepl("^///\\$tab", tokens$text))
  nl <- if (length(i)) vapply(strsplit(tokens$text[i], "\n"), length, integer(1)) else integer(0)
  list(blocks = data.frame(
         line = tokens$line[i],
         tab = if (is.null(tabs)) rep(NA_character_, length(i))
               else vapply(tokens$line[i], function(x) .sl_tab_at(tabs, x), character(1)),
         n_lines = nl, stringsAsFactors = FALSE),
       total_lines = sum(nl), script_lines = max(tokens$line))
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

main <- function(args) {
  if (!length(args)) stop("usage: Rscript script_debt.R <script>")
  source("script_loads.R")
  tok  <- read_qlik_script(args[1L])
  tabs <- .sl_tabs(tok)
  l    <- script_loads(tok)$loads

  g <- guid_literals(tok, tabs)
  c_ <- commented_out_code(tok, tabs)
  d <- duplicate_labels(l)

  cat(args[1L], "\n", sep = "")
  cat("  direct database calls: ", sum(l$source_kind == "select"), "\n", sep = "")
  cat("  hardcoded record ids (GUIDs): ", nrow(g), "\n", sep = "")
  if (nrow(g)) print(g, row.names = FALSE)
  cat("  commented-out code: ", c_$total_lines, " lines of ", c_$script_lines,
      " (", round(100 * c_$total_lines / c_$script_lines, 1), "%)\n", sep = "")
  cat("  duplicate table labels: ", nrow(d), "\n", sep = "")
  if (nrow(d)) print(d, row.names = FALSE)
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
