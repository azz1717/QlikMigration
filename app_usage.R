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

# --- declared assets: variables and master items --------------------------
#
# A different question from field usage above, and answered differently. A
# master dimension or measure is referenced by its qId — `qLibraryId` in the
# object that uses it — and never by its title, so the id is the only
# reliable key. A variable is referenced by NAME, in chart expressions and in
# the script alike, so both have to be searched.
#
# Over-inclusive on the same principle as the rest of phase 2: a variable
# named in a comment counts as used. A false "used" leaves dead weight
# behind; a false "unused" deletes something live.

#' One metadata file's declared items.
#'
#' Values only, positionally: in document order a key string is immediately
#' followed by its value string, which is all the structure this needs and
#' keeps json_strings.R from having to become a parser.
#' @return data.frame(id, name)
.au_declared <- function(path, name_key) {
  none <- data.frame(id = character(0), name = character(0), stringsAsFactors = FALSE)
  if (!file.exists(path)) return(none)
  s <- read_json_strings(path)
  if (!nrow(s)) return(none)
  val_at <- function(i) if (i < nrow(s) && !s$is_key[i + 1L]) s$text[i + 1L] else NA_character_
  ids <- which(s$is_key & s$text == "qId")
  if (!length(ids)) return(none)
  nms <- which(s$is_key & s$text == name_key)
  data.frame(
    id   = vapply(ids, val_at, character(1)),
    name = vapply(ids, function(i) {
             k <- nms[nms > i]
             if (!length(k)) NA_character_ else val_at(k[1L])
           }, character(1)),
    stringsAsFactors = FALSE)
}

#' Every string VALUE in the app's objects, as one blob.
#'
#' One blob rather than a vector because the search below is per-asset: a
#' hundred variables against ten thousand strings is a hundred passes either
#' way, and one long string is a single gregexpr instead of ten thousand.
.au_object_blob <- function(dir) {
  files <- list.files(file.path(dir, "objects"), pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return("")
  paste(unlist(lapply(files, function(f) {
    x <- read_json_strings(f); x$text[!x$is_key]
  }), use.names = FALSE), collapse = "\n")
}

# Whole-word containment, fixed-string matched so a name carrying a regex
# metacharacter cannot change the pattern's meaning.
.au_word_hit <- function(blob, nm) {
  if (!nzchar(blob) || is.na(nm) || !nzchar(nm)) return(FALSE)
  st <- gregexpr(nm, blob, fixed = TRUE)[[1L]]
  if (st[1L] == -1L) return(FALSE)
  en <- st + nchar(nm) - 1L
  before <- ifelse(st > 1L, substr(rep(blob, length(st)), st - 1L, st - 1L), "")
  after  <- ifelse(en < nchar(blob), substr(rep(blob, length(en)), en + 1L, en + 1L), "")
  any(!grepl("[A-Za-z0-9_]", before) & !grepl("[A-Za-z0-9_]", after))
}

#' Which variables the script mentions, other than to define them.
#'
#' The token right of SET/LET is the definition itself and does not count as
#' a use — otherwise every variable is used by virtue of existing.
.au_var_in_script <- function(tokens, names) {
  if (is.null(tokens) || !nrow(tokens) || !length(names)) return(rep(FALSE, length(names)))
  prev   <- prev_non_trivia_idx(tokens$type)
  lower  <- tolower(tokens$text)
  is_def <- tokens$type == "WORD" & !is.na(prev) & lower[prev] %in% c("set", "let")
  word   <- tokens$type == "WORD" & !is_def
  other  <- paste(tokens$text[tokens$type != "WORD"], collapse = "\n")
  vapply(names, function(nm) {
    if (is.na(nm)) return(FALSE)
    any(tokens$text[word] == nm) || .au_word_hit(other, nm)
  }, logical(1), USE.NAMES = FALSE)
}

#' Declared app assets and whether anything references them.
#'
#' @param dir an app*-unbuilt directory.
#' @param script_path defaults to the export's own copy.
#' @return data.frame(kind, id, name, used)
asset_usage <- function(dir, script_path = NULL) {
  if (is.null(script_path)) script_path <- file.path(dir, "script.qvs")
  blob <- .au_object_blob(dir)
  tok  <- if (file.exists(script_path)) read_qlik_script(script_path) else NULL

  v <- .au_declared(file.path(dir, "variables.json"),  "qName")
  d <- .au_declared(file.path(dir, "dimensions.json"), "title")
  m <- .au_declared(file.path(dir, "measures.json"),   "title")

  # A master item is used when its ID appears; a variable when its NAME does.
  by_id <- function(x, kind) {
    if (!nrow(x)) return(cbind(kind = character(0), x, used = logical(0)))
    data.frame(kind = kind, x,
               used = vapply(x$id, function(i) .au_word_hit(blob, i), logical(1)),
               row.names = NULL, stringsAsFactors = FALSE)
  }
  vu <- if (!nrow(v)) logical(0) else
          vapply(v$name, function(n) .au_word_hit(blob, n), logical(1), USE.NAMES = FALSE) |
          .au_var_in_script(tok, v$name)

  rbind(
    if (nrow(v)) data.frame(kind = "variable", v, used = vu,
                            row.names = NULL, stringsAsFactors = FALSE)
      else by_id(v, "variable"),
    by_id(d, "dimension"),
    by_id(m, "measure"))
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

  for (d in dirs) {
    a <- asset_usage(d)
    n <- function(k) sum(a$kind == k)
    x <- function(k) sum(a$kind == k & !a$used)
    cat(sprintf("%s assets: %d/%d variables unused, %d/%d master dimensions, %d/%d master measures
",
                basename(d), x("variable"), n("variable"), x("dimension"), n("dimension"),
                x("measure"), n("measure")))
  }

  if (!is.na(csv)) {
    write.csv(all, csv, row.names = FALSE)
    cat("wrote ", csv, " (", nrow(all), " rows)\n", sep = "")
  }
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
