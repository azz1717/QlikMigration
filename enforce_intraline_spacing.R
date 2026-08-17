# enforce_intraline_spacing.R
#
# Style-guide pass: normalises intra-line whitespace. See DESIGN.md §4.7.
#
#   - No space before, exactly one space after every comma. ("No space
#     before" is not in §4.7's original bullets - agreed with Adam
#     2026-08-17 that this pass should own it rather than leaving it for the
#     not-yet-built alignment pass, since it's plain non-newline content
#     spacing (§6.1's stated scope) and alignment - which already has to
#     compute per-block column widths - shouldn't also have to learn to
#     ignore pre-comma noise. The original motivating case (enforce_leading_
#     commas leaving a stray space before a relocated comma) turned out not
#     to actually occur - that pass only relocates a comma when a line break
#     separates it from its field, so the space it left behind is always
#     newline-bearing and out of scope for this pass either way. The real,
#     confirmed justification is plainer: 16 real hand-typed instances in
#     [Grant Managing Region].txt, e.g. line 25's "'GWA','KIM' ,'SA'" -
#     inconsistent spacing before a comma inside MATCH()/IF() argument
#     lists.)
#   - Exactly one space either side of every binary operator: arithmetic
#     (+ - * /), comparison (= <> > < >= <=), concatenation (&).
#   - Exactly one space either side of AS (fixed 2026-08-17 - see the KNOWN
#     GAP note that used to live here: two WORDs can't be lexically adjacent,
#     but AS against a non-WORD token like BRACKET/DQUOTE/SQUOTE/RPAREN can,
#     e.g. unstyled input "field"as[alias]). Case-insensitive match since
#     this pass runs before casing (pass 5).
#   - No space immediately inside parentheses - IF(x, 1, 0), never
#     IF( x, 1, 0 ).
#   - Any other run of 2+ spaces in content collapses to one.
#
# TWO THINGS THE OPERATOR TOKEN TYPE CANNOT SETTLE, SO THIS PASS USES
# CONTEXT (DESIGN §6.1):
#
#   - Unary minus. AddMonths(Today(), -12) - the "-" takes no left space and
#     binds tight to what follows (no right space either). Classified by the
#     PREVIOUS non-trivia token: binary if that token could end a value
#     (NUMBER, BRACKET, DQUOTE, SQUOTE, RPAREN, or a WORD that is not a Qlik
#     keyword - reusing QLIK_KEYWORDS, since AND/OR/THEN/... cannot end a
#     value either); unary otherwise (previous is LPAREN, COMMA, SEMI,
#     another OPERATOR, a keyword-WORD, or there is no previous token at
#     all). Today()-30's "-" has RPAREN before it -> binary. The comma
#     before AddMonths's "-12" -> unary.
#   - The LOAD * wildcard, typed OPERATOR but not one. Classified by
#     POSITION, not content: a "*" is the wildcard iff its previous
#     non-trivia token is the WORD "load", or is "distinct" whose own
#     previous non-trivia token is "load" - the only place Qlik syntax
#     allows a bare "*" there, so it can never collide with a genuine
#     multiplication like n*chunkSize. A wildcard gets no operator-spacing
#     rule at all, in either direction.
#
# SCOPE. Whitespace tokens carrying a newline are the vertical-layout pass's
# job (DESIGN §3.4) and are never touched here - that is most of what
# protects the not-yet-built alignment pass's territory "for free": the
# indentation/pad before a LOAD block's first field is one token together
# with its leading newline, so it is out of scope by construction, no
# special-casing needed.
#
# The one place that isn't automatic: the space after a LOAD field-list
# separator comma (the depth-0 comma enforce_leading_commas relocates) is
# where a future alignment pass's column padding will live. An EXISTING
# whitespace token there is never shrunk by the blanket collapse rule, even
# though a MISSING one (the relocated comma's normal zero-gap state) still
# gets a single space inserted, per the comma rule above. Every other comma
# (function arguments, etc.) gets the full exactly-one/zero-before treatment
# - only the field-separator kind is exempt from shrinking.
#
# SELECT ... ; blocks are skipped, same as every other pass.
#
# Operates on a token stream (see qlik_tokenizer.R - source that first, and
# qlik_reserved_words.R for QLIK_KEYWORDS). Vanilla base R only.

.na_false <- function(x) { x[is.na(x)] <- FALSE; x }

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @return a list with:
#'   $tokens   - the token stream with intra-line whitespace normalised
#'   $warnings - inherited from find_load_segments() (malformed LOAD statements)
#'   $changes  - data.frame(line, kind, detail), one row per site touched.
#'               "kind" is one of "insert", "remove", "collapse".
enforce_intraline_spacing <- function(tokens) {
  n <- nrow(tokens)
  empty <- data.frame(line = integer(0), kind = character(0),
                      detail = character(0), stringsAsFactors = FALSE)
  if (n == 0) return(list(tokens = tokens, warnings = character(0),
                          changes = empty))

  t_text <- tokens$text
  t_type <- tokens$type
  t_line <- tokens$line
  lower  <- tolower(t_text)

  in_select <- in_select_region(t_type, lower)
  PREV <- prev_non_trivia_idx(t_type)

  # ---- classify OPERATOR tokens ------------------------------------------

  is_operator <- t_type == "OPERATOR" & !in_select
  is_star     <- is_operator & t_text == "*"
  is_minus    <- is_operator & t_text == "-"

  prev1_word <- function(idx, w) .na_false(t_type[idx] == "WORD" & lower[idx] == w)
  prev2 <- PREV[PREV]  # "previous of previous", NA propagates through NA indices

  is_wildcard <- is_star &
    (prev1_word(PREV, "load") | (prev1_word(PREV, "distinct") & prev1_word(prev2, "load")))

  value_type <- t_type %in% c("NUMBER", "BRACKET", "DQUOTE", "SQUOTE", "RPAREN")
  value_word <- t_type == "WORD" & !(lower %in% QLIK_KEYWORDS)
  is_value   <- value_type | value_word
  prev_is_value <- .na_false(is_value[PREV])

  is_unary_minus <- is_minus & !prev_is_value

  # every OPERATOR that gets the ordinary "one space either side" treatment
  is_binary_op <- is_operator & !is_wildcard & !is_unary_minus

  is_comma <- t_type == "COMMA" & !in_select

  # ---- field-separator commas: the ones exempt from shrinking -----------
  # Recomputed the same way enforce_leading_commas.R finds them (seg$end+1),
  # since find_load_segments() reports segment bounds, not the separators
  # between them.
  found <- find_load_segments(tokens)
  warn <- found$warnings
  segs <- found$segments
  sep_comma_idx <- integer(0)
  if (length(segs) > 0) {
    ends <- vapply(segs, function(s) s$end, integer(1))
    cand <- ends + 1L
    ok <- cand <= n & t_type[cand] == "COMMA"
    sep_comma_idx <- cand[ok]
  }
  protect_after <- logical(n)
  nxt <- sep_comma_idx + 1L
  ok2 <- nxt <= n & t_type[nxt] == "WS"
  protect_after[nxt[ok2]] <- TRUE

  # ---- decide what happens to each EXISTING whitespace token ------------
  # (splicing new whitespace where none exists is handled separately below)

  has_nl <- grepl("\n", t_text, fixed = TRUE)
  touchable_ws <- t_type == "WS" & !has_nl & !in_select

  type_prev  <- c(NA_character_, t_type[-n])
  type_next  <- c(t_type[-1], NA_character_)
  unary_prev <- c(FALSE, is_unary_minus[-n])

  force_zero <- touchable_ws & (
    (!is.na(type_prev) & type_prev == "LPAREN") |
      (!is.na(type_next) & type_next == "RPAREN") |
      unary_prev |
      (!is.na(type_next) & type_next == "COMMA")
  )
  collapse <- touchable_ws & !force_zero & !protect_after & nchar(t_text) > 1L

  # candidate insertion sites: after every comma, both sides of every binary
  # operator, both sides of every AS. Not every candidate actually needs a
  # splice (most already have their one space) - this is just the upper
  # bound for preallocation.
  comma_idx <- which(is_comma)
  op_idx    <- which(is_binary_op)

  # AS: the "two WORDs can't be lexically adjacent" argument that implicitly
  # spaces every other keyword says nothing about AS sitting directly against
  # a non-WORD token (BRACKET, DQUOTE, SQUOTE, RPAREN...), which tokenizes
  # fine with zero whitespace - DESIGN §4.7's KNOWN GAP, fixed 2026-08-17.
  # Runs before casing (pass 5), so match case-insensitively; AS is a
  # QLIK_KEYWORDS member so any bare WORD spelled "as" outside SELECT is
  # unambiguously this keyword, never a field (keywords win - INTERFACES.md).
  as_idx <- which(t_type == "WORD" & lower == "as" & !in_select)

  # four disjoint categories of change - force_zero/collapse never overlap
  # (mutually exclusive by construction above), and insertions only ever
  # fire where a gap is currently EMPTY, which force_zero/collapse never
  # touch (those only ever act on a WS token that already exists). Size the
  # log to the true upper bound rather than nrow(tokens): each category can
  # independently approach that size, so summing them can exceed it.
  cap <- sum(force_zero) + sum(collapse) + length(comma_idx) + 2L * length(op_idx) + 2L * length(as_idx)
  ch_line <- integer(cap); ch_kind <- character(cap); ch_detail <- character(cap)
  nch <- 0L

  if (any(force_zero)) {
    idx <- which(force_zero)
    k <- length(idx)
    ch_line[nch + seq_len(k)]   <- t_line[idx]
    ch_kind[nch + seq_len(k)]   <- "remove"
    ch_detail[nch + seq_len(k)] <- sprintf("removed %d-space gap", nchar(t_text[idx]))
    nch <- nch + k
    tokens <- void_token(tokens, idx)
  }
  if (any(collapse)) {
    idx <- which(collapse)
    k <- length(idx)
    ch_line[nch + seq_len(k)]   <- t_line[idx]
    ch_kind[nch + seq_len(k)]   <- "collapse"
    ch_detail[nch + seq_len(k)] <- sprintf("%d spaces -> 1", nchar(t_text[idx]))
    nch <- nch + k
    tokens$text[idx] <- " "
  }

  # ---- insert a single space wherever a required gap is currently zero --
  # Zero-gap is immediate physical adjacency (the neighbouring row isn't
  # type WS) - correct whether that neighbour is real content or a VOID
  # leftover from an earlier pass, since either way nothing renders between
  # the anchor and the splice.

  insertions <- list()
  space_row <- data.frame(text = " ", type = "WS", line = 1L,
                          stringsAsFactors = FALSE)

  # A comma directly followed by a binary operator with no gap (e.g. ",+b")
  # makes the comma's "after" anchor and that operator's "before" anchor the
  # SAME position - both want a single space at the same spot. Guard on
  # is.null() so that's one splice and one log entry, not two.
  for (i in comma_idx) {
    if (i >= n || t_type[i + 1L] == "WS") next
    key <- as.character(i)
    if (!is.null(insertions[[key]])) next
    row <- space_row; row$line <- t_line[i]
    insertions[[key]] <- row
    nch <- nch + 1L
    ch_line[nch] <- t_line[i]; ch_kind[nch] <- "insert"; ch_detail[nch] <- "space after comma"
  }

  for (i in op_idx) {
    if (i > 1L && t_type[i - 1L] != "WS") {
      key <- as.character(i - 1L)
      if (is.null(insertions[[key]])) {
        row <- space_row; row$line <- t_line[i]
        insertions[[key]] <- row
        nch <- nch + 1L
        ch_line[nch] <- t_line[i]; ch_kind[nch] <- "insert"; ch_detail[nch] <- "space before operator"
      }
    }
    if (i < n && t_type[i + 1L] != "WS" && is.null(insertions[[as.character(i)]])) {
      key <- as.character(i)
      row <- space_row; row$line <- t_line[i]
      insertions[[key]] <- row
      nch <- nch + 1L
      ch_line[nch] <- t_line[i]; ch_kind[nch] <- "insert"; ch_detail[nch] <- "space after operator"
    }
  }

  for (i in as_idx) {
    if (i > 1L && t_type[i - 1L] != "WS") {
      key <- as.character(i - 1L)
      if (is.null(insertions[[key]])) {
        row <- space_row; row$line <- t_line[i]
        insertions[[key]] <- row
        nch <- nch + 1L
        ch_line[nch] <- t_line[i]; ch_kind[nch] <- "insert"; ch_detail[nch] <- "space before AS"
      }
    }
    if (i < n && t_type[i + 1L] != "WS" && is.null(insertions[[as.character(i)]])) {
      key <- as.character(i)
      row <- space_row; row$line <- t_line[i]
      insertions[[key]] <- row
      nch <- nch + 1L
      ch_line[nch] <- t_line[i]; ch_kind[nch] <- "insert"; ch_detail[nch] <- "space after AS"
    }
  }

  tokens <- splice_tokens(tokens, insertions)

  changes <- data.frame(
    line   = ch_line[seq_len(nch)],
    kind   = ch_kind[seq_len(nch)],
    detail = ch_detail[seq_len(nch)],
    stringsAsFactors = FALSE
  )

  list(tokens = tokens, warnings = warn, changes = changes)
}
