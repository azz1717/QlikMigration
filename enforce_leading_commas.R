# enforce_leading_commas.R
#
# Style-guide pass: moves every field-separator comma in a LOAD field list
# from a trailing position to a leading position, e.g.
#     [FieldA] AS [FieldA],
#     [FieldB] AS [FieldB]
# becomes
#     [FieldA] AS [FieldA]
#     ,[FieldB] AS [FieldB]
#
#   - Only depth-0 field-separator commas move. A comma nested inside a
#     multi-line function call (IF(...), MATCH(...), or any other) is
#     never a candidate in the first place - find_load_segments() only
#     ever splits fields at depth-0 commas, so a comma inside an open
#     paren is structurally part of one field's content and this pass
#     never sees it as something to move.
#   - If comment-only lines sit between the old trailing comma and the
#     next real field, they are left exactly where they are; the new
#     leading comma is placed immediately before the next field's actual
#     content, below the comments.
#   - The last field in a LOAD has no trailing comma to move - left alone.
#   - A comma is only moved when it sits on the SAME source line as the
#     field before it (genuinely trailing). find_load_segments() defines a
#     field's end as "right before the next depth-0 comma" regardless of
#     which line that comma is visually on, so without this check a comma
#     that's already leading (correctly sitting on the next field's line)
#     would look identical to one still needing to move - this is what
#     makes the pass idempotent.
#   - SELECT ... ; blocks are left untouched entirely, same as the other
#     passes (find_load_segments() already skips them).
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Vanilla base R only.

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @return a list with:
#'   $tokens   - the token stream with commas relocated
#'   $warnings - character vector describing any trailing comma that could
#'               not be relocated (no real field found after it)
#'   $changes  - data.frame(from_line, to_line, field_preview), one row per
#'               comma moved - an exact record of what changed, for
#'               sanity-checking instead of eyeballing a generic text diff.
enforce_leading_commas <- function(tokens) {
  found <- find_load_segments(tokens)
  warn <- found$warnings
  insertions <- list()

  n <- nrow(tokens)
  nseg <- length(found$segments)

  # Hoist the columns. The trivia scan below indexes them thousands of times
  # and every tokens$col[i] on a data.frame pays dispatch cost. Safe because
  # the stream is not modified until after the loop.
  t_type <- tokens$type
  t_line <- tokens$line
  t_text <- tokens$text

  # Commas are collected here and voided in ONE call after the loop, rather
  # than one data.frame assignment per comma - see void_token().
  void_idx <- integer(nseg); nvoid <- 0L

  # Change log accumulated as plain vectors and turned into a data.frame once.
  # Building one data.frame per iteration and rbind()-ing them was pure
  # bookkeeping overhead that cost more than the pass's real work.
  ch_from <- integer(nseg); ch_to <- integer(nseg)
  ch_prev <- character(nseg); nch <- 0L

  # stamped per use instead of constructing a data.frame every iteration
  comma_row <- data.frame(text = ",", type = "COMMA", line = 1L,
                          stringsAsFactors = FALSE)

  for (seg in found$segments) {
    comma_idx <- seg$end + 1
    if (comma_idx > n || t_type[comma_idx] != "COMMA") next  # last field in its LOAD
    if (t_line[comma_idx] != t_line[seg$end]) next  # already leading - nothing to do

    # first real (non-trivia) token after the comma
    target_idx <- NA_integer_
    p <- comma_idx + 1
    while (p <= n) {
      if (!(t_type[p] %in% c("WS", "COMMENT"))) { target_idx <- p; break }
      p <- p + 1
    }
    if (is.na(target_idx) || t_type[target_idx] %in% c("COMMA", "SEMI")) {
      warn <- c(warn, sprintf(
        "Line %d: no field found after this comma to attach a leading comma to - left trailing.",
        t_line[comma_idx]))
      next
    }

    nvoid <- nvoid + 1L
    void_idx[nvoid] <- comma_idx

    key <- as.character(target_idx - 1)
    new_comma <- comma_row
    new_comma$line <- t_line[target_idx]
    insertions[[key]] <- if (is.null(insertions[[key]])) new_comma else rbind(insertions[[key]], new_comma)

    preview <- paste(t_text[target_idx:min(target_idx + 4, n)], collapse = "")
    nch <- nch + 1L
    ch_from[nch] <- t_line[comma_idx]
    ch_to[nch]   <- t_line[target_idx]
    ch_prev[nch] <- substr(trimws(preview), 1, 40)
  }

  if (nvoid > 0L) tokens <- void_token(tokens, void_idx[seq_len(nvoid)])
  tokens <- splice_tokens(tokens, insertions)

  changes_df <- data.frame(
    from_line     = ch_from[seq_len(nch)],
    to_line       = ch_to[seq_len(nch)],
    field_preview = ch_prev[seq_len(nch)],
    stringsAsFactors = FALSE
  )

  list(tokens = tokens, warnings = warn, changes = changes_df)
}
