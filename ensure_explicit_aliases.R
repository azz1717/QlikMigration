# ensure_explicit_aliases.R
#
# Style-guide pass: ensures every field in a Qlik LOAD statement has an
# explicit AS alias.
#
#   - Fields that already have an alias (top-level "AS") are left untouched.
#   - A field with no alias that is a simple reference (a double-quoted or
#     bracketed name, or a bare dotted identifier) is given an alias equal
#     to its own literal text, e.g.
#         "Grant Activity.Source System Id%",
#     becomes
#         "Grant Activity.Source System Id%" AS "Grant Activity.Source System Id%",
#   - A bare single-quoted token ('...') is NOT treated as a field reference
#     here: on the left of AS, a single-quoted token is a string literal,
#     not a field name (see enforce_bracket_references.R for the one
#     confirmed exception - a single-quoted alias on the RIGHT of AS). A
#     literal with no alias has no well-defined default field name, so it
#     falls through to the "un-aliased expression" case below instead of
#     being silently self-aliased.
#   - The wildcard "*" is left untouched (it cannot take an alias).
#   - A field with no alias that is NOT a simple reference (an un-aliased
#     expression, including a bare string literal) is left untouched and
#     reported back as a warning, since there is no unambiguous default
#     name to assign it.
#   - SELECT ... ; blocks (raw SQL passed to a LIB CONNECT TO source) are
#     left untouched entirely; only Qlik LOAD field lists are processed.
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Vanilla base R only.

.alias_kind <- function(type) {
  switch(type,
         DQUOTE = "double-quoted field",
         BRACKET = "bracketed field",
         WORD = "bare identifier",
         type)
}

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @return a list with:
#'   $tokens   - the token stream with missing aliases spliced in
#'   $warnings - character vector describing any un-aliased expressions
#'               (or unparseable LOAD statements) left untouched
#'   $changes  - data.frame(line, kind, before, after), one row per field
#'               that was given an explicit alias - an exact record of what
#'               changed, for sanity-checking instead of eyeballing a
#'               generic text diff. "kind" identifies the field's own
#'               reference style (double-quoted/bracketed/bare).
ensure_explicit_aliases <- function(tokens) {
  found <- find_load_segments(tokens)
  warn <- found$warnings
  insertions <- list()

  nseg <- length(found$segments)

  # hoist the columns - the stream is not modified until splice_tokens() below
  t_text <- tokens$text
  t_type <- tokens$type
  t_line <- tokens$line

  # change log accumulated as plain vectors, assembled once at the end (a
  # data.frame per iteration plus a final rbind() cost more than the real work)
  ch_line <- integer(nseg);   ch_kind   <- character(nseg)
  ch_before <- character(nseg); ch_after <- character(nseg)
  nch <- 0L

  # stamped per use instead of constructing a data.frame every iteration
  alias_rows <- data.frame(
    text = c(" ", "AS", " ", ""),
    type = c("WS", "WORD", "WS", ""),
    line = 1L,
    stringsAsFactors = FALSE
  )

  for (seg in found$segments) {
    if (seg$has_as) next

    ci <- seg$content_idx

    if (length(ci) == 1 && t_text[ci] == "*") {
      next  # wildcard - no alias possible
    }

    if (length(ci) == 1 && t_type[ci] %in% c("DQUOTE", "BRACKET", "WORD")) {
      before <- t_text[ci]
      new_rows <- alias_rows
      new_rows$text[4] <- before
      new_rows$type[4] <- t_type[ci]
      new_rows$line    <- t_line[ci]
      insertions[[as.character(ci)]] <- new_rows

      nch <- nch + 1L
      ch_line[nch]   <- t_line[ci]
      ch_kind[nch]   <- .alias_kind(t_type[ci])
      ch_before[nch] <- before
      ch_after[nch]  <- paste0(before, " AS ", before)
      next
    }

    seg_text <- paste(t_text[seg$start:seg$end], collapse = "")
    warn <- c(warn, sprintf(
      "Line %d: un-aliased expression left unchanged: %s",
      seg$line, trimws(seg_text)))
  }

  tokens <- splice_tokens(tokens, insertions)

  changes_df <- data.frame(
    line   = ch_line[seq_len(nch)],
    kind   = ch_kind[seq_len(nch)],
    before = ch_before[seq_len(nch)],
    after  = ch_after[seq_len(nch)],
    stringsAsFactors = FALSE
  )

  list(tokens = tokens, warnings = warn, changes = changes_df)
}
