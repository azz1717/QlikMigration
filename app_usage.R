# app_usage.R — what the app's own definition references.
#
# Phase 2, step 2 (DESIGN §6.5). NOT part of the styling pipeline; nothing in
# run_pipeline.R sources this. Base R only.
#
# Every string value in app*-unbuilt/**.json is treated as possible expression
# text and run through the tokenizer. Enumerating the keys whose values hold
# expressions is exactly how the first attempt missed half the usage, so no
# key is privileged and no document structure is consulted.
#
# The result over-reports by design: anything that MIGHT be a field counts as
# used. Over-reporting under-prunes, which is the safe failure — the opposite
# error deletes a field a chart is using.
#
# Provenance travels with every reference (DESIGN §6.5). The phase 2 report is
# a submittable document and has to answer "why do you say this is unused?",
# which means naming the file a reference was found in, not just the name.

source("qlik_tokenizer.R")
source("qlik_reserved_words.R")
source("json_strings.R")

# Strip the delimiters off a reference token. Qlik has no escape inside `[ ]`,
# so a bracket body is taken as-is; a doubled `""` inside a quoted reference is
# the one escape that exists.
.au_undelimit <- function(text, type) {
  body <- substr(text, 2L, nchar(text) - 1L)
  ifelse(type == "DQUOTE", gsub('""', '"', body, fixed = TRUE), body)
}

#' Field references in one piece of expression text.
#'
#' @param text a single string, already unescaped out of its JSON.
#' @return data.frame(ref, kind) — `kind` is "bracketed", "quoted" or "bare",
#'   one row per occurrence, in order.
#'
#' DQUOTE gets its own kind rather than counting as a field (Adam 2026-08-18,
#' on the evidence). DESIGN §1.2 — Qlik has no double-quoted string literal —
#' holds for SCRIPT text, but app JSON does not follow it: all 8 unique DQUOTEs
#' in `app-unbuilt/objects` are selection values (`"2022-23"`, `"Yes"`), not
#' fields. Still collected, because a wrong exclusion deletes a live field;
#' separated, because the report must not claim `Yes` is a field.
#'
#' SQUOTE is a literal and is skipped — the `AS`-adjacent alias exception that
#' enforce_bracket_references.R handles cannot arise here, as an app expression
#' has no field list to alias into.
expression_references <- function(text) {
  none <- data.frame(ref = character(0), kind = character(0),
                     stringsAsFactors = FALSE)
  # length 0 is reachable: file_references() seeds its rbind with an empty call
  # so the result keeps its columns when a file yields nothing.
  if (length(text) != 1L || is.na(text) || !nzchar(text)) return(none)

  tok <- tokenize_qlik(text)
  if (nrow(tok) == 0L) return(none)

  delimited <- tok$type %in% c("BRACKET", "DQUOTE")
  dkind     <- ifelse(tok$type[delimited] == "BRACKET", "bracketed", "quoted")

  # Same two guards as the bare-word branch of enforce_bracket_references.R,
  # for the same reasons: a keyword is not a field, and a word in call position
  # is a function even when a field shares its name (Year, Left, Replace).
  nxt  <- next_non_trivia_idx(tok$type)
  call <- !is.na(nxt) & tok$type[nxt] == "LPAREN"
  bare <- tok$type == "WORD" & !call &
          !(tolower(tok$text) %in% QLIK_KEYWORDS)   # the vector is lowercase

  # A string that is not an expression AT ALL is a field name in its entirety.
  # `qFieldDefs` holds unbracketed names, and splitting one on whitespace
  # destroys it: `Latest Funding Financial Year` became four bare words, the
  # field itself was never emitted, and its table was reported unreferenced —
  # the one direction that deletes something live (found 2026-08-18).
  #
  # The test is structural: no brackets, quotes, parens, commas or semicolons
  # anywhere in the string. Words, numbers and hyphens are allowed, because
  # real field names have them (`2021 Census by ILOC`, `Grant Activity-3`).
  whole <- !any(tok$type %in% c("BRACKET", "DQUOTE", "SQUOTE", "LPAREN",
                                "RPAREN", "COMMA", "SEMI")) &&
           any(tok$type == "WORD") && nzchar(trimws(text))

  if (!any(delimited) && !any(bare) && !whole) return(none)

  out <- data.frame(
    ref  = c(.au_undelimit(tok$text[delimited], tok$type[delimited]),
             tok$text[bare],
             if (whole) trimws(text)),
    kind = c(dkind, rep("bare", sum(bare)),
             if (whole) "whole-string"),
    stringsAsFactors = FALSE)

  out[nzchar(out$ref), , drop = FALSE]
}

#' Every reference in one JSON file, aggregated.
#'
#' @return data.frame(source_file, ref, kind, n).
#'
#' Keys are skipped: a JSON member name is structure, never expression text
#' (Adam, 2026-08-18). That is not the same mistake as enumerating which keys'
#' VALUES to read — every value is still scanned, whatever names it.
file_references <- function(path, label = basename(path)) {
  s <- read_json_strings(path)
  vals <- s$text[!s$is_key]

  refs <- do.call(rbind, c(list(expression_references(character(0))),
                           lapply(vals, expression_references)))

  if (nrow(refs) == 0L)
    return(data.frame(source_file = character(0), ref = character(0),
                      kind = character(0), n = integer(0),
                      stringsAsFactors = FALSE))

  # Case is preserved, never folded. Whether Qlik's field matching is
  # case-insensitive is a question for the cross-reference in step 4, and
  # folding here would throw away the evidence it needs to decide.
  agg <- aggregate(list(n = rep(1L, nrow(refs))),
                   by = list(ref = refs$ref, kind = refs$kind), FUN = sum)

  data.frame(source_file = label, agg[order(agg$ref, agg$kind), ],
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Every reference in one app export.
#'
#' @param dir an app*-unbuilt directory.
#' @return data.frame(app, source_file, ref, kind, n), source_file relative to
#'   `dir`. Apps are never merged — the two exports load separate data models
#'   and share nothing (DESIGN §6.5), so a reference in one says nothing about
#'   the other.
app_references <- function(dir) {
  files <- list.files(dir, pattern = "\\.json$", recursive = TRUE,
                      full.names = TRUE)
  if (length(files) == 0L) stop("no .json files under ", dir)

  rel  <- substring(files, nchar(dir) + 2L)
  refs <- do.call(rbind, Map(file_references, files, rel))

  data.frame(app = basename(dir), refs, row.names = NULL,
             stringsAsFactors = FALSE)
}

# Run directly for a summary; `--csv <path>` writes the full table.
main <- function(args) {
  csv  <- if ("--csv" %in% args) args[match("--csv", args) + 1L] else NA
  args <- setdiff(args, c("--csv", csv))

  dirs <- if (length(args)) args else {
    d <- list.dirs(".", recursive = FALSE)
    d[grepl("app[0-9]*-unbuilt$", d)]
  }
  if (length(dirs) == 0L) stop("no app*-unbuilt directory found")

  all <- do.call(rbind, lapply(dirs, app_references))

  for (a in unique(all$app)) {
    x <- all[all$app == a, ]
    u <- function(k) length(unique(x$ref[x$kind == k]))
    cat(sprintf("%s: %d files, %d references (%d unique) — %d bracketed, %d quoted, %d bare, %d whole-string\n",
                a, length(unique(x$source_file)), sum(x$n),
                length(unique(x$ref)), u("bracketed"), u("quoted"), u("bare"),
                u("whole-string")))
  }

  if (!is.na(csv)) {
    write.csv(all, csv, row.names = FALSE)
    cat("wrote ", csv, " (", nrow(all), " rows)\n", sep = "")
  }
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
