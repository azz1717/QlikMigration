# enforce_alias_alignment.R
#
# Style-guide pass: aligns every field's AS to one column within its own
# LOAD block - DESIGN §4.6, pass 7. Must run LAST, after vertical layout
# (run_pipeline.R already documents this ordering requirement): the column
# this pass computes depends on each field's FINAL indentation.
#
#   - Padding is TABS, always (Adam 2026-08-17, non-negotiable: the scripts
#     are hand-edited afterward, and every other indent/pad in this
#     pipeline is already tabs - not a per-feature choice to revisit). The
#     known consequence - tab-run alignment only holds exactly at one tab
#     width (DESIGN §3.6) - is an accepted tradeoff, not a defect.
#   - The tabs are inserted BEFORE AS's existing single leading space, not
#     instead of it (Adam 2026-08-17): intraline spacing already guarantees
#     exactly one space before AS, and that space stays - this pass only
#     adds tab padding ahead of it, so AS lands one column past every tab
#     stop, never on it.
#   - Scope is per LOAD block (find_load_segments()'s load_tok_idx), not
#     per file: DESIGN §4.6 - "one enormous expression widens only its own
#     block rather than the whole file."
#   - A field is only "eligible" (counts toward the block's column, and
#     gets its own AS rewritten) when TWO things hold (Adam 2026-08-17):
#       1. it starts a source line find_block_structure() classifies as
#          kind "field" - i.e. the common, one-field-per-line shape
#          vertical layout already assumes throughout (matched by LINE
#          NUMBER, not token index - a comma-led field's line-start token
#          is the leading comma, not the field's own first content token).
#          A field sharing a physical line with LOAD or another field
#          (unusual, hand-typed input) is left completely alone rather
#          than guessed at - no established convention for that shape yet.
#       2. its own content through AS sits on that SAME source line - a
#          field whose expression wraps onto further lines is excluded:
#          it's already visually broken from the block's rhythm by
#          wrapping, so forcing every other field to match its width would
#          be absurd, and its own AS (which may itself open a fresh
#          continuation line, already governed by vertical layout) is out
#          of scope here.
#   - The column is the smallest multiple of 4 (tab width, DESIGN §3.6)
#     strictly greater than every eligible field's own column right after
#     its content - guaranteeing at least one tab of gap even for the
#     widest field in the block.
#   - A field whose own column would reach .eaa_max_field_width or beyond
#     is excluded from BOTH the column calculation and the rewrite (Adam
#     2026-08-17): one outlier field forcing every short field in the same
#     block to pad out to match it looks worse than no alignment at all.
#     The excluded field's own AS is left exactly as authored - a human
#     aligns it by hand later if it's worth doing at all. The threshold is
#     the exact character length of a real field Adam pointed to
#     (formatexample.txt's `IF("Grant Activity.Activity Id" = ...)` line,
#     122 chars) - not a round number, a concrete example.
#   - Every field is guaranteed an AS by pass 1 (DESIGN §1.5/§4.3), so
#     "no AS" is not a real case this pass has to handle defensively - it's
#     still checked (has_as) in case this pass is ever run standalone.
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Vanilla base R only.

.eaa_tab_width <- 4L

# A field whose own column (indent + content, right before AS) reaches this
# many characters or more is treated like a wrapped field: excluded from
# the block's column calculation and left untouched. Adam 2026-08-17, tied
# to a concrete real example rather than a round number - see the header
# comment above for which one.
.eaa_max_field_width <- 122L

#' Column (0-based) reached at the end of a string with no embedded
#' newline, expanding tabs at .eaa_tab_width from a given starting column.
.eaa_tab_col <- function(s, start_col = 0L) {
  col <- start_col
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (ch == "\t") col <- (col %/% .eaa_tab_width + 1L) * .eaa_tab_width
    else col <- col + 1L
  }
  col
}

#' Index of the WS token whose text immediately precedes token idx, skipping
#' VOID leftovers - same convention as enforce_vertical_layout.R's
#' .preceding_ws_idx, duplicated locally rather than shared across files
#' (both are a few lines; not worth a new shared-scanner entry for).
.eaa_preceding_ws_idx <- function(type, idx) {
  j <- idx - 1L
  while (j >= 1L && type[j] == "VOID") j <- j - 1L
  if (j >= 1L && type[j] == "WS") j else NA_integer_
}

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#'   Must already have vertical layout applied - see the ordering note above.
#' @return a list with:
#'   $tokens   - the token stream with AS-column whitespace normalised
#'   $warnings - find_load_segments()'s warnings, passed through unchanged
#'   $changes  - data.frame(line, before, after), one row per rewritten gap
enforce_alias_alignment <- function(tokens) {
  seg  <- find_load_segments(tokens)
  segs <- seg$segments
  warn <- seg$warnings

  empty <- data.frame(line = integer(0), before = character(0),
                      after = character(0), stringsAsFactors = FALSE)
  if (length(segs) == 0) return(list(tokens = tokens, warnings = warn, changes = empty))

  bs <- find_block_structure(tokens)
  # Keyed by source LINE NUMBER, not token index: a comma-led field's
  # line-start token (bs$lines$idx) is the leading COMMA, not the field's
  # own content token (first_i below) - same physical line, different
  # token, and the comma+space (or the first field's two-space pad) is
  # part of the column too, so the column walk below must start from the
  # TRUE line-start token, not from first_i.
  field_rows <- bs$lines[bs$lines$kind == "field", c("idx", "line")]
  field_line_idx <- setNames(field_rows$idx, as.character(field_rows$line))

  t_type <- tokens$type
  t_text <- tokens$text
  t_line <- tokens$line

  has_as  <- vapply(segs, function(s) isTRUE(s$has_as), logical(1))
  load_id <- vapply(segs, function(s) s$load_tok_idx, integer(1))
  as_idx  <- vapply(segs, function(s) s$as_idx, integer(1))
  first_i <- vapply(segs, function(s) s$content_idx[1], integer(1))

  ch_line <- integer(0); ch_before <- character(0); ch_after <- character(0)

  for (blk in unique(load_id[has_as])) {
    members <- which(load_id == blk & has_as)

    # A field with NO whitespace at all before AS (real case, ugly hand-
    # typed data in app-unbuilt/script.qvs: `"field"as[alias]`) is excluded
    # rather than crashing on it: this pass's column math assumes
    # intraline spacing already guarantees a WS token there, which mostly
    # holds but isn't universal on unstyled input. Left exactly as
    # authored, same treatment as any other malformed/outlier field.
    has_ws_before_as <- vapply(members, function(m)
      !is.na(.eaa_preceding_ws_idx(t_type, as_idx[m])), logical(1))

    eligible <- (as.character(t_line[first_i[members]]) %in% names(field_line_idx)) &
      (t_line[as_idx[members]] == t_line[first_i[members]]) &
      has_ws_before_as
    if (!any(eligible)) next
    elig <- members[eligible]

    cols <- vapply(elig, function(m) {
      line_start <- field_line_idx[[as.character(t_line[first_i[m]])]]

      # column at the true start of this field's LINE (the leading indent,
      # or the leading comma+space/first-field pad's own preceding indent)
      ws_idx <- .eaa_preceding_ws_idx(t_type, line_start)
      base_col <- 0L
      if (!is.na(ws_idx)) {
        ws <- t_text[ws_idx]
        nl_pos <- gregexpr("\n", ws, fixed = TRUE)[[1]]
        after_nl <- if (nl_pos[1] > 0) substring(ws, max(nl_pos) + 1L) else ws
        base_col <- .eaa_tab_col(after_nl)
      }

      # everything from the line start through the token right before AS -
      # the separator (", " or the first field's pad) plus the field's own
      # content - EXCLUDING the WS token immediately before AS itself: that
      # is the padding THIS pass rewrites, not content. Leaving it in would
      # make a second run measure the first run's tabs as content width,
      # breaking idempotency (found running verify.R against both real
      # fixtures - 220/1583 phantom changes on a second pass). Guaranteed
      # tab-free otherwise (only the leading WS, already accounted for
      # above, can carry tabs), so plain character count is column width
      # for the rest of it.
      ws_before_as <- .eaa_preceding_ws_idx(t_type, as_idx[m])
      pre_as <- line_start:(as_idx[m] - 1L)
      pre_as <- pre_as[t_type[pre_as] != "VOID" & pre_as != ws_before_as]
      base_col + sum(nchar(t_text[pre_as]))
    }, numeric(1))

    # An outlier field (>= .eaa_max_field_width) is excluded from the
    # column entirely, same treatment as a wrapped field - see the header
    # comment. "Move onto the next widest column" (Adam 2026-08-17) means
    # exactly this: the max below is taken over the NARROW fields only.
    narrow <- cols < .eaa_max_field_width
    if (!any(narrow)) next
    target_col <- (max(cols[narrow]) %/% .eaa_tab_width + 1L) * .eaa_tab_width

    for (k in seq_along(elig)) {
      if (!narrow[k]) next   # outlier - left exactly as authored
      m <- elig[k]
      ws_idx <- .eaa_preceding_ws_idx(t_type, as_idx[m])
      if (is.na(ws_idx)) next   # defensive; AS is always preceded by WS

      # Tabs are inserted BEFORE the existing single space, not instead of
      # it (Adam 2026-08-17): intraline spacing already guarantees exactly
      # one space before AS, and that space is kept - this pass only adds
      # tab padding ahead of it. AS itself therefore lands one column past
      # every tab stop, not on it.
      n_tabs <- ceiling((target_col - cols[k]) / .eaa_tab_width)
      new_ws <- paste0(strrep("\t", max(1L, n_tabs)), " ")

      before <- t_text[ws_idx]
      if (identical(before, new_ws)) next

      ch_line   <- c(ch_line, t_line[first_i[m]])
      ch_before <- c(ch_before, before)
      ch_after  <- c(ch_after, new_ws)
      t_text[ws_idx] <- new_ws
    }
  }

  tokens$text <- t_text

  changes <- data.frame(line = ch_line, before = ch_before, after = ch_after,
                        stringsAsFactors = FALSE)
  list(tokens = tokens, warnings = warn, changes = changes)
}
