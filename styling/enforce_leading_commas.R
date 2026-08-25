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
#'   May also be a LOGICAL VECTOR (2026-08-25, batched comment styling): one
#'   element per LOAD block of this stream, in load_tok_idx order, element k
#'   governing the k-th block's own first segment. A scalar is NOT recycled
#'   across blocks - it keeps its single-segment meaning above.
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

  # Comma insertions accumulate in a plain, preallocated list plus a
  # parallel key vector - O(1) per push. Growing a NAMED list by key
  # (insertions[[key]] <- ...) inside the loop forced a linear scan of the
  # existing names on every push, O(n^2) over the segment count (2583
  # segments on script.qvs, ~0.63s in this loop). The named-by-key
  # `insertions` list splice_tokens() expects is built ONCE below, after
  # both loops that push here, via split()+lapply - never by repeated
  # [[<-]] on a growing named list. Same upper bound as the change log
  # above: at most one push per segment from this loop, plus at most one
  # synthesised leading comma per LOAD block from the head-comma loop
  # further down.
  ins_acc <- vector("list", 2L * nseg + 1L)
  ins_key <- character(2L * nseg + 1L)
  n_ins <- 0L

  # Change log accumulated as plain vectors and turned into a data.frame once.
  # Building one data.frame per iteration and rbind()-ing them was pure
  # bookkeeping overhead that cost more than the pass's real work.
  # +nseg+1L: room for the context$first_field=FALSE synthesised-comma
  # entries below (at most one per LOAD block, and a block needs at least one
  # segment, so nseg is an upper bound), on top of the at-most-nseg
  # relocations the main loop can log.
  ch_from <- integer(2L * nseg + 1L); ch_to <- integer(2L * nseg + 1L)
  ch_prev <- character(2L * nseg + 1L); nch <- 0L

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

    new_comma <- comma_row
    new_comma$line <- t_line[target_idx]
    n_ins <- n_ins + 1L
    ins_acc[[n_ins]] <- new_comma
    ins_key[n_ins] <- as.character(target_idx - 1)

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
  # VECTOR form (2026-08-25, perf-sub7 batched styling): one element per LOAD
  # block, in load_tok_idx order - element k governs the k-th block's own
  # first segment. A SCALAR (or NULL) keeps today's meaning exactly: it
  # governs the stream's FIRST segment only, which for a single-LOAD stream
  # is the same thing. Deliberately NOT recycled across blocks, so the
  # top-level whole-file call cannot change behaviour by this edit.
  first_field <- TRUE
  if (!is.null(context) && !is.null(context$first_field)) first_field <- context$first_field
  if (length(first_field) == 0L) first_field <- TRUE

  # Which segment each head comma would go in front of. Scalar: segment 1
  # only (today). Vector: the first segment of each LOAD block whose own
  # element is FALSE.
  head_segs <- integer(0)
  if (nseg > 0L) {
    if (length(first_field) > 1L) {
      seg_load <- vapply(found$segments, function(s) s$load_tok_idx, integer(1))
      blk_first <- which(!duplicated(seg_load))          # ascending token order
      take <- seq_along(blk_first) <= length(first_field)
      head_segs <- blk_first[take][!vapply(first_field[seq_len(sum(take))],
                                           isTRUE, logical(1))]
    } else if (!isTRUE(first_field)) {
      head_segs <- 1L
    }
  }

  need_head_comma <- FALSE
  head_idx <- NA_integer_
  for (hs in head_segs) {
    head_idx <- found$segments[[hs]]$content_idx[1]
    p <- head_idx - 1L
    while (p >= 1L && t_type[p] %in% c("WS", "COMMENT")) p <- p - 1L
    already <- p >= 1L && t_type[p] == "COMMA"
    if (!already) {
      if (head_idx > 1L) {
        new_comma <- comma_row
        new_comma$line <- t_line[head_idx]
        n_ins <- n_ins + 1L
        ins_acc[[n_ins]] <- new_comma
        ins_key[n_ins] <- as.character(head_idx - 1L)
      } else {
        need_head_comma <- TRUE   # only ever block 1's own first segment
        head_line <- t_line[head_idx]
                                   # nothing precedes token 1 - splice_tokens()
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

  insertions <- list()
  if (n_ins > 0L) {
    grp <- split(seq_len(n_ins), ins_key[seq_len(n_ins)])
    insertions <- lapply(grp, function(idxs) {
      if (length(idxs) == 1L) ins_acc[[idxs]] else do.call(rbind, ins_acc[idxs])
    })
  }

  if (nvoid > 0L) tokens <- void_token(tokens, void_idx[seq_len(nvoid)])
  tokens <- splice_tokens(tokens, insertions)

  if (need_head_comma) {
    tokens <- rbind(
      data.frame(text = ",", type = "COMMA", line = head_line, stringsAsFactors = FALSE),
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
