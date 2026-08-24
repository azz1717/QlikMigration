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
#   - A comma is only moved when a LINE BREAK separates it from the field
#     that follows it. That is what "trailing" actually means, and it is what
#     makes the pass idempotent: once the comma has moved down to sit with
#     its field there is no newline between them any more, so a rescan leaves
#     it alone.
#
#     This test used to compare source LINE NUMBERS - move the comma only if
#     it sits on the same line as the field BEFORE it. That failed on a
#     single-line field list:
#         LOAD distinct [Office] AS tmpField_0, [Location] AS tmpField_1 ...
#     where the comma shares a line with both neighbours, so it was
#     "relocated" on every run: output stayed stable, but each pass logged a
#     phantom change and leaked another VOID token.
#
#     Testing for the newline rather than for adjacency also survives the
#     spacing pass putting a space after the comma (", [Field]") - adjacency
#     would break there and start moving commas again.
#   - SELECT ... ; blocks are left untouched entirely, same as the other
#     passes (find_load_segments() already skips them).
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Vanilla base R only.

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @param context optional list. `context$first_field`: is the FIRST segment
#'   of this call's own token stream the true first field of its enclosing
#'   LOAD list? NULL/TRUE (default) derives exactly today's behaviour - no
#'   comma is ever placed before the first segment, since nothing in the
#'   whole-file scan targets it. `FALSE` (for a child stream - PLAN
#'   sections 3/5/9 - whose own first segment is really a CONTINUATION of a
#'   list that starts before this stream) makes the pass ensure a leading comma
#'   sits in front of that first segment too, inserting one if not already
#'   there. Idempotent the same way the rest of the pass is: a second call
#'   finds the comma already immediately before the segment and does nothing.
#' @return a list with:
#'   $tokens   - the token stream with commas relocated
#'   $warnings - character vector describing any trailing comma that could
#'               not be relocated (no real field found after it)
#'   $changes  - data.frame(from_line, to_line, field_preview), one row per
#'               comma moved - an exact record of what changed, for
#'               sanity-checking instead of eyeballing a generic text diff.
#'               A synthesised leading comma (first_field = FALSE case) has
#'               from_line = NA (nothing was relocated - it was inserted).
enforce_leading_commas <- function(tokens, context = NULL) {
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
  # +1L: room for the context$first_field=FALSE synthesised-comma entry
  # below, on top of the at-most-nseg relocations the main loop can log.
  ch_from <- integer(nseg + 1L); ch_to <- integer(nseg + 1L)
  ch_prev <- character(nseg + 1L); nch <- 0L

  # stamped per use instead of constructing a data.frame every iteration
  comma_row <- data.frame(text = ",", type = "COMMA", line = 1L,
                          stringsAsFactors = FALSE)

  for (seg in found$segments) {
    comma_idx <- seg$end + 1
    if (comma_idx > n || t_type[comma_idx] != "COMMA") next  # last field in its LOAD

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

    # Already leading if nothing between the comma and its field breaks the
    # line - see the note at the top of this file.
    if (target_idx > comma_idx + 1) {
      gap <- (comma_idx + 1):(target_idx - 1)
      if (!any(grepl("\n", t_text[gap], fixed = TRUE))) next
    } else {
      next  # comma sits immediately before its field
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

  # context$first_field = FALSE: this call's own first segment is not
  # really the list's first field (PLAN sections 3/5/9) - a live-equivalent field
  # always has a leading separator, so synthesise one if none is there yet.
  # NULL/TRUE derives today's behaviour: never touch the first segment.
  first_field <- TRUE
  if (!is.null(context) && !is.null(context$first_field)) first_field <- isTRUE(context$first_field)

  need_head_comma <- FALSE
  head_idx <- NA_integer_
  if (!first_field && nseg > 0L) {
    head_idx <- found$segments[[1]]$content_idx[1]
    p <- head_idx - 1L
    while (p >= 1L && t_type[p] %in% c("WS", "COMMENT")) p <- p - 1L
    already <- p >= 1L && t_type[p] == "COMMA"
    if (!already) {
      if (head_idx > 1L) {
        key <- as.character(head_idx - 1L)
        new_comma <- comma_row
        new_comma$line <- t_line[head_idx]
        insertions[[key]] <- if (is.null(insertions[[key]])) new_comma else rbind(insertions[[key]], new_comma)
      } else {
        need_head_comma <- TRUE   # nothing precedes token 1 - splice_tokens()
                                   # has no "insert before everything" key;
                                   # prepend by rbind() after splicing, same
                                   # deferred technique enforce_vertical_
                                   # layout.R uses for its own line-1 case.
      }
      preview <- paste(t_text[head_idx:min(head_idx + 4, n)], collapse = "")
      nch <- nch + 1L
      ch_from[nch] <- NA_integer_
      ch_to[nch]   <- t_line[head_idx]
      ch_prev[nch] <- substr(trimws(preview), 1, 40)
    }
  }

  if (nvoid > 0L) tokens <- void_token(tokens, void_idx[seq_len(nvoid)])
  tokens <- splice_tokens(tokens, insertions)

  if (need_head_comma) {
    tokens <- rbind(
      data.frame(text = ",", type = "COMMA", line = t_line[head_idx], stringsAsFactors = FALSE),
      tokens)
  }

  changes_df <- data.frame(
    from_line     = ch_from[seq_len(nch)],
    to_line       = ch_to[seq_len(nch)],
    field_preview = ch_prev[seq_len(nch)],
    stringsAsFactors = FALSE
  )

  list(tokens = tokens, warnings = warn, changes = changes_df)
}
