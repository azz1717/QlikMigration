# comment_style_driver.R
#
# The recursive driver from docs/PLAN-comment-substream.md section 3: for
# each extracted comment-substream child (styling/comment_substream.R), runs
# the REAL styling passes over it, in canonical order (1,2,3,4,5,6,7 -
# DESIGN 2.5: 3 before 4, 3 before 6, all of 1-6 before 7), threading
# context derived from where the run sits in the PARENT, then serializes
# the styled result back into the parent's COMMENT tokens.
#
# NOT wired into run_pipeline.R yet, and pass 8 is untouched - both happen
# together in a later task (wiring this in while pass 8 still runs would
# double-style comments). Base R only.
#
# Caller sources, in order:
#   source("shared/qlik_tokenizer.R")
#   source("shared/qlik_reserved_words.R")
#   source("styling/ensure_explicit_aliases.R")
#   source("styling/enforce_bracket_references.R")
#   source("styling/enforce_leading_commas.R")
#   source("styling/enforce_intraline_spacing.R")
#   source("styling/enforce_reserved_word_case.R")
#   source("styling/enforce_vertical_layout.R")
#   source("styling/enforce_alias_alignment.R")
#   source("styling/comment_substream.R")
#   source("styling/comment_style_driver.R")
#
# ---------------------------------------------------------------------
# THE SCAFFOLD (Adam, 2026-08-24 ruling). A field_run child has no LOAD
# keyword of its own, so find_load_segments() - which passes 3, 6's
# field/continuation classification, and 7 all build on - finds nothing to
# work with there (verified directly against shared/qlik_tokenizer.R: it
# only starts collecting segments after literally seeing the WORD "load").
# Pass 3's own INTERFACES entry anticipates exactly this ("a field_run
# child with no LOAD wrapper at all, wrapped or not").
#
# The fix has in-repo precedent, so it is not the rejected plan-section-4
# alternative (unwrap into the PARENT's live stream): pass 8 already runs
# the real passes over a synthetic one-field LOAD
# (enforce_commented_field_style.R's .scf_style_fragment). This driver does
# the same thing at RUN granularity - "LOAD\n" + the run's own body +
# "\nFROM x;" - entirely inside the isolated child, never touching the
# parent, which is the property that makes it safe (plan section 3's
# argument for the whole child-stream design applies here unchanged: the
# wrapper tokens and the child's own tokens are still never the same
# object as any parent token).
#
# GUARD 1 (Adam): no token that originated in the synthetic wrapper may
# survive into the serialized child. .csd_style_group() asserts this
# directly rather than trusting the slice arithmetic: exactly one LOAD and
# one FROM must be found (otherwise a real field bare-named "load"/"from"
# is indistinguishable from the wrapper and the group is left unstyled,
# not guessed at), AND the sliced-out body text itself must not still
# start with "LOAD" or end with "FROM x;" - an independent, textual
# re-check of the same boundary, not a second read of the same indices.
#
# PROSE (decision 3, plan section 9: carried through unstyled). A run
# whose lines mix field/stmt content with prose is split into maximal
# same-kind groups, using find_block_structure()'s own idx/end line-range
# convention (the exact one .cs_classify_run() already uses, so the
# grouping lines up 1:1 with the run's own line_kind for free). Prose
# groups are carried through byte-identical - not fed through tokenize_qlik
# or any pass, so they cannot be touched even accidentally. Code groups are
# scaffolded and styled independently.
#
# GUARD 2 (Adam): one alignment column per RUN, not per code group. Context
# (target_col, first_field) is derived ONCE per run, from the PARENT, and
# handed unchanged to every code group in that run; only the run's own
# FIRST code group can carry the run's real first_field value - every
# later group in the same run is necessarily a continuation, first_field
# FALSE, same as pass 3's own documented meaning of that flag.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- wrap / style / unwrap one contiguous code fragment ------------------

.CSD_WRAP_HEAD <- "LOAD\n"
.CSD_WRAP_TAIL <- "\nFROM x;"

#' Style one field/stmt fragment (no interior prose) by running the real
#' passes over a synthetic scaffold, per the header comment above.
#'
#' @param body_text character(1) - the fragment's own source text, exactly
#'   as it sits in the child stream (may itself span several lines).
#' @param context list(first_field, target_col, base_depth) for this call -
#'   threaded straight through to passes 3, 6, 7.
#' @return character(1) styled fragment text (no leading/trailing newline),
#'   or NA_character_ if GUARD 1 fails - caller must leave the fragment
#'   unstyled in that case, never guess.
.csd_style_group <- function(body_text, context) {
  # GUARD (idempotence, caught directly on [Grant Managing Region].txt): a
  # depth-0 SEMI inside the body would end the synthetic LOAD statement
  # early, leaving "FROM x;" as a second, ORPHANED top-level statement -
  # pass 6 then inserts DESIGN 4.8's blank-line gap in front of it, which
  # this call's own GUARD 1 does not catch (both LOAD and FROM are still
  # exactly one each) but which is not stable across re-styling. A body
  # that terminates its own statement is not a plain field - left
  # unstylable rather than guessed at.
  bt <- tokenize_qlik(body_text)
  depth0_semi <- bt$type == "SEMI" &
    (cumsum(bt$type == "LPAREN") - cumsum(bt$type == "RPAREN")) == 0L
  if (any(depth0_semi)) return(NA_character_)

  tk <- tokenize_qlik(paste0(.CSD_WRAP_HEAD, body_text, .CSD_WRAP_TAIL))
  tk <- ensure_explicit_aliases(tk)$tokens
  tk <- enforce_bracket_references(tk)$tokens
  tk <- enforce_leading_commas(tk, context)$tokens
  tk <- enforce_intraline_spacing(tk)$tokens
  tk <- enforce_reserved_word_case(tk)$tokens
  tk <- enforce_vertical_layout(tk, context)$tokens
  tk <- enforce_alias_alignment(tk, context)$tokens

  ld <- which(tk$type == "WORD" & tolower(tk$text) == "load")
  fr <- which(tk$type == "WORD" & tolower(tk$text) == "from")
  if (length(ld) != 1L || length(fr) != 1L || fr[1] <= ld[1] + 1L) return(NA_character_)

  body_out <- paste(tk$text[(ld[1] + 1L):(fr[1] - 1L)], collapse = "")
  # Strip the wrapper's OWN leading gap (LOAD's line 1 may or may not have
  # picked up its own inserted indent - irrelevant, it's excluded by ld[1]
  # already) and its trailing gap before FROM - a full newline-plus-indent
  # run (FROM's own statement-kind indent), not just a bare "\n": pass 6
  # writes that gap as "\n" + N tabs for FROM's line, and a plain "\n$"
  # pattern leaves the tabs behind (caught directly, not by inspection -
  # see the synthetic P2 driver-cycle fixed-point failure this fixed).
  body_out <- sub("^\n", "", body_out)
  body_out <- sub("\n[ \t]*$", "", body_out)

  # Second, independent containment check - on the OUTPUT TEXT, not the
  # token indices used to build it - catches a slice off-by-one directly.
  if (grepl("^\\s*LOAD\\b", body_out, ignore.case = TRUE) ||
      grepl("\\bFROM\\s+x\\s*;?\\s*$", body_out, ignore.case = TRUE)) {
    return(NA_character_)
  }
  body_out
}

# ---- splitting a run into prose / code groups -----------------------------

#' Per-line token ranges of a child stream, in the SAME order as its own
#' line_kind vector (extract_comment_runs()'s per-run output) - the exact
#' idx/end convention comment_substream.R's .cs_classify_run() already
#' uses, so the two line up 1:1 with no re-derivation risk.
.csd_line_ranges <- function(child) {
  n <- nrow(child)
  bs <- find_block_structure(child)
  lns <- bs$lines
  if (nrow(lns) == 0) return(data.frame(start = integer(0), end = integer(0)))
  ends <- c(if (nrow(lns) > 1) lns$idx[-1] - 1L else integer(0), n)
  data.frame(start = lns$idx, end = ends)
}

#' Strip a run's OWN TRAILING boundary trivia - a depth-agnostic mirror of
#' .cs_strip_leading_sep(), from the tail instead of the head: trailing
#' whitespace, and a trailing comma (with its own preceding whitespace) if
#' present. comment_substream.R only models a LEADING separator (the tie to
#' whatever precedes a run) - a run can just as easily end mid-field-list,
#' tied to whatever LIVE field follows it, and that trailing comma is NOT a
#' real "next field" for the wrap's own purposes.
#'
#' GOTCHA this exists to fix: without it, a run like `"field", ` (one
#' field, trailing comma+space tying it to a live field after the run) gets
#' wrapped as "LOAD\n\"field\", \nFROM x;" - the wrap's OWN "FROM" reads as
#' the next field to enforce_leading_commas(), which then "relocates" the
#' trailing comma to sit in front of FROM, corrupting the boundary (caught
#' directly on fixtures/formatexample.txt:27, not by inspection). Stripping
#' the trailing bit before wrapping and reattaching it verbatim afterward
#' removes the ambiguity the same way leading_sep already does at the
#' other end.
#'
#' @return list(tokens, trailing_sep) - `trailing_sep` is NA_character_ if
#'   there was none.
.csd_strip_trailing_sep <- function(child) {
  n <- nrow(child)
  j <- n
  if (j >= 1L && child$type[j] == "WS") j <- j - 1L
  if (j >= 1L && child$type[j] == "COMMA") {
    j <- j - 1L
    if (j >= 1L && child$type[j] == "WS") j <- j - 1L
  }
  trailing_sep <- NA_character_
  if (j < n) {
    trailing_sep <- paste(child$text[(j + 1L):n], collapse = "")
    child <- child[seq_len(j), , drop = FALSE]
    rownames(child) <- NULL
  }
  list(tokens = child, trailing_sep = trailing_sep)
}

#' Split a child stream's rows into maximal same-kind (prose / not-prose)
#' groups. Each group: list(start, end, is_prose) - a token index RANGE
#' into `child`, source order.
.csd_make_groups <- function(child, line_kind) {
  ranges <- .csd_line_ranges(child)
  if (nrow(ranges) == 0) return(list())
  is_prose <- line_kind == "prose"
  brk <- c(TRUE, is_prose[-1] != is_prose[-length(is_prose)])
  grp_id <- cumsum(brk)
  groups <- vector("list", max(grp_id))
  for (g in seq_len(max(grp_id))) {
    rows <- which(grp_id == g)
    groups[[g]] <- list(start = ranges$start[rows[1]],
                        end   = ranges$end[rows[length(rows)]],
                        is_prose = is_prose[rows[1]])
  }
  groups
}

# ---- per-run styling --------------------------------------------------

#' Style one extracted run in place, given context derived from the parent.
#'
#' @param run one element of extract_comment_runs()'s $runs, status
#'   "extracted".
#' @param ctx list(target_col, first_field) derived from the PARENT (see
#'   .csd_parent_field_context()) - ignored for a load_block run, which is
#'   a complete commented table with no live block to match (decision 1:
#'   a commented field never WIDENS the live column; a load_block has no
#'   live column to widen in the first place).
#' @return list(run = the (possibly restyled) run, stats = list(bucket,
#'   prose_lines, prose_groups, n_unstylable_groups)) - `bucket` is the
#'   coverage counter key.
.csd_style_run <- function(run, ctx) {
  stats <- list(bucket = NA_character_, prose_lines = 0L,
                prose_groups = 0L, n_unstylable_groups = 0L)

  if (identical(run$kind, "load_block")) {
    # A load_block already carries its own real LOAD keyword - no scaffold
    # needed. Interior prose is out of scope here (deliberately, not an
    # oversight): splitting a REAL LOAD statement's own field list around a
    # prose line the way a field_run's synthetic scaffold can would fragment
    # a genuine single Qlik statement, which is a different and riskier
    # operation than reassembling an already-synthetic wrapper. Left
    # byte-identical and counted, same treatment as a refused run.
    if (any(run$line_kind == "prose")) {
      stats$bucket <- "load_block_prose_skipped"
      stats$prose_lines <- sum(run$line_kind == "prose")
      return(list(run = run, stats = stats))
    }
    lb_ctx <- list(first_field = TRUE, base_depth = 0L, target_col = NULL)
    tk <- run$tokens
    tk <- ensure_explicit_aliases(tk)$tokens
    tk <- enforce_bracket_references(tk)$tokens
    tk <- enforce_leading_commas(tk, lb_ctx)$tokens
    tk <- enforce_intraline_spacing(tk)$tokens
    tk <- enforce_reserved_word_case(tk)$tokens
    tk <- enforce_vertical_layout(tk, lb_ctx)$tokens
    tk <- enforce_alias_alignment(tk, lb_ctx)$tokens
    # Re-tokenize from clean text rather than keep the passes' own row
    # structure: they void trivia in place rather than delete it (VOID
    # rows, empty text, INTERFACES.md's own convention), which is exactly
    # right for a live pipeline but would leave `run$tokens` carrying
    # rows a fresh re-extraction of the same serialized text never would -
    # a spurious mismatch, not a real difference in content. The wrapped
    # field_run path below does the same for the same reason.
    tk <- tokenize_qlik(paste(tk$text, collapse = ""))
    # Pass 6's line-1 special case (no preceding WS token to rewrite, so it
    # INSERTS one) fires here exactly as it would on a real top-level file,
    # since this run's own first token is, from pass 6's point of view,
    # "line 1" - giving `tk` a fresh leading indent WS with nothing in
    # `run$leading_sep` accounting for it. Same normalisation as the
    # field_run path below reconciles the two, and is equally stable under
    # re-extraction (the indent is just as much "leading trivia" by
    # section 7's own definition, whatever put it there).
    stripped <- .cs_strip_leading_sep(tk)
    run$tokens <- stripped$tokens
    run$leading_sep <- stripped$leading_sep
    stats$bucket <- "load_block_styled"
    return(list(run = run, stats = stats))
  }

  if (!identical(run$kind, "field_run")) {
    stats$bucket <- "unknown_kind_skipped"   # defensive - not reachable today
    return(list(run = run, stats = stats))
  }

  # Strip the run's OWN trailing boundary trivia FIRST, before grouping -
  # see .csd_strip_trailing_sep()'s header. line_kind is re-derived from
  # the truncated body rather than reused, since removing trailing rows
  # can (rarely) remove a whole line and desync the two.
  tail_strip <- .csd_strip_trailing_sep(run$tokens)
  body_tokens <- tail_strip$tokens
  trailing_sep <- tail_strip$trailing_sep
  line_kind <- if (is.na(trailing_sep)) run$line_kind else .cs_classify_run(body_tokens)$line_kind

  groups <- .csd_make_groups(body_tokens, line_kind)
  if (length(groups) == 0) {
    stats$bucket <- "field_run_empty"        # defensive - not reachable today
    return(list(run = run, stats = stats))
  }

  n <- length(groups)
  pieces <- character(n)
  # TRUE wherever `pieces[g]` already carries its own trailing boundary
  # text (verbatim prose always does; a scaffolded code group does only
  # when it had a real trailing WS/comma to strip and reattach) - the
  # stitch loop below inserts a join "\n" ONLY where this is FALSE.
  has_own_sep <- logical(n)
  is_prose_vec <- vapply(groups, `[[`, logical(1), "is_prose")
  first_code_seen <- FALSE
  n_scaffolded <- 0L; n_unstylable <- 0L

  for (g in seq_len(n)) {
    grp <- groups[[g]]
    raw <- paste(body_tokens$text[grp$start:grp$end], collapse = "")
    if (grp$is_prose) { pieces[g] <- raw; has_own_sep[g] <- TRUE; next }

    # KNOWN LIMITATION (converged here under budget, not a design choice):
    # only the FIRST code group of a run is scaffolded. A run with prose
    # splitting it into TWO OR MORE separated code groups was found
    # unstable under a second driver application (synthetic fixture run 6
    # - field, prose, field - the second group's own padding grew 2 tabs
    # between cycles; root cause not yet isolated past "not
    # .csd_style_group() itself, which is idempotent standalone - see
    # session notes"). Leaving every code group AFTER the first
    # byte-identical to its authored form is the safe, converged choice:
    # it is never less correct than today's baseline (no styling at all),
    # and it is what real fixtures already exercise (GMR's one
    # prose-carrying run has exactly one code group either side, not two
    # scaffolded groups) - this limitation is UNTESTED against a real
    # multi-code-group prose split because none exists in the mandatory
    # fixtures. Follow-up: isolate why the run-level styler (not
    # .csd_style_group() alone) produces different padding for a NON-first
    # group on a second pass.
    if (first_code_seen) {
      pieces[g] <- raw
      has_own_sep[g] <- TRUE
      n_unstylable <- n_unstylable + 1L
      next
    }

    # A code group's OWN raw slice absorbs the boundary WS leading into
    # whatever follows it (.cs_classify_run()'s line-range convention,
    # same as .csd_strip_trailing_sep()'s target at the whole-run level) -
    # for the LAST group that boundary is the run's own tail, already
    # handled above, but an INTERIOR code group (one followed by a prose
    # group) has the exact same shape: a trailing comma with nothing
    # structurally after it inside the scaffold except the synthetic
    # FROM. Strip it from EVERY group, not just the run's own last one, or
    # it corrupts the same way fixtures/formatexample.txt:27 did (caught
    # directly: synthetic run 6's field-prose-field case was not stable
    # under a second driver application until this was per-group).
    grp_tail <- .csd_strip_trailing_sep(raw_tokens <- body_tokens[grp$start:grp$end, , drop = FALSE])
    raw_body <- paste(grp_tail$tokens$text, collapse = "")
    grp_trailing <- grp_tail$trailing_sep

    # GUARD 2: every code group in this run shares the SAME context -
    # only the first one seen can be the run's own true first field.
    grp_ctx <- list(
      first_field = if (!first_code_seen) isTRUE(ctx$first_field %||% TRUE) else FALSE,
      target_col  = ctx$target_col,
      base_depth  = 0L)
    styled <- .csd_style_group(raw_body, grp_ctx)
    if (is.na(styled)) {
      pieces[g] <- raw
      has_own_sep[g] <- TRUE   # `raw` still carries its own original tail
      n_unstylable <- n_unstylable + 1L
    } else {
      pieces[g] <- paste0(styled, if (!is.na(grp_trailing)) grp_trailing else "")
      has_own_sep[g] <- !is.na(grp_trailing)
      n_scaffolded <- n_scaffolded + 1L
    }
    first_code_seen <- TRUE
  }

  out <- pieces[1]
  if (n > 1L) {
    for (g in 2:n) {
      # Join with "\n" only when the PRECEDING piece does not already
      # carry its own trailing boundary text (see has_own_sep above) -
      # otherwise this would double a newline .csd_strip_trailing_sep()
      # already reattached (caught directly: synthetic run 6 - field,
      # prose, field - was not stable under a second driver application
      # until this joined on has_own_sep instead of is_prose_vec alone).
      sep <- if (has_own_sep[g - 1L]) "" else "\n"
      out <- paste0(out, sep, pieces[g])
    }
  }
  if (!is.na(trailing_sep)) out <- paste0(out, trailing_sep)

  if (nzchar(out)) {
    restyled <- tokenize_qlik(out)
    # A leading comma pass 3 just baked into the front of `restyled` (the
    # first_field=FALSE path) must NOT stay a token - section 7's own
    # invariant (comment_substream.R) is what keeps a second extract from
    # reading a styled-in comma as body text and adding another. Restore
    # canonical shape with the SAME stripper extract_comment_runs() uses,
    # rather than a second copy of that logic.
    # The styled text is now the sole authority on the separator - the
    # ORIGINAL leading_sep (whatever ad hoc spacing preceded an unstyled
    # run) is superseded, not merged: pass 3 already decided, from
    # context$first_field alone, whether a leading comma belongs here.
    stripped <- .cs_strip_leading_sep(restyled)
    run$tokens <- stripped$tokens
    run$leading_sep <- stripped$leading_sep
  }
  stats$bucket <- if (n_scaffolded > 0L) "field_run_scaffolded" else "field_run_unstylable"
  stats$prose_lines <- sum(run$line_kind == "prose")
  stats$prose_groups <- sum(is_prose_vec)
  stats$n_unstylable_groups <- n_unstylable
  list(run = run, stats = stats)
}

# ---- parent-level live context (decision 1: taken as given, never widened) -

#' For every PARENT token index: is it inside a live LOAD's field list
#' (`in_fl`), what is that LOAD's live fields' MODAL AS column (`col_of` -
#' the mode, not the max, same reasoning pass 8 used and the same
#' reason pass 7 excludes outliers), and what is the index of that LOAD's
#' own first live field (`first_live`, so a run can tell whether it sits
#' BEFORE it - the run's true-first-field question).
#'
#' Deliberately does not reuse enforce_commented_field_style.R (pass 8):
#' that file is scheduled for deletion in a later task and this driver must
#' not depend on it surviving. Column arithmetic reuses pass 7's own
#' .eaa_tab_col()/.eaa_preceding_ws_idx() (which DO survive) rather than
#' adding a third twin of that arithmetic (plan section 10 already flags
#' pass 7/pass 8 as a twin pair to resolve when pass 8 goes).
.csd_parent_field_context <- function(tokens) {
  n <- nrow(tokens)
  found <- find_load_segments(tokens)
  segs <- found$segments
  in_fl <- logical(n); col_of <- integer(n); first_live <- integer(n)
  if (length(segs) == 0) return(list(in_fl = in_fl, col_of = col_of, first_live = first_live))

  seg_load <- vapply(segs, function(s) s$load_tok_idx, integer(1))
  seg_end  <- vapply(segs, function(s) s$end, integer(1))
  t_type <- tokens$type; t_text <- tokens$text

  for (L in unique(seg_load)) {
    grp <- which(seg_load == L)
    last <- max(seg_end[grp])
    in_fl[L:last] <- TRUE

    cols <- integer(0)
    for (k in grp) {
      s <- segs[[k]]
      if (!isTRUE(s$has_as)) next
      ws_idx <- .eaa_preceding_ws_idx(t_type, s$as_idx)
      # column of AS's own line: leading indent (from the nearest newline
      # in the preceding WS run) plus everything from there up to AS,
      # excluding the WS immediately before AS itself (that is padding,
      # not content - same exclusion pass 7 applies to its own measurement).
      line_start <- s$as_idx
      j <- s$as_idx - 1L
      while (j >= 1L && !(t_type[j] == "WS" && grepl("\n", t_text[j], fixed = TRUE))) {
        if (j == 1L) { line_start <- 1L; j <- 0L; break }
        j <- j - 1L
      }
      if (j >= 1L) line_start <- j + 1L
      base_col <- 0L
      if (j >= 1L) {
        ws <- t_text[j]
        nlpos <- max(gregexpr("\n", ws, fixed = TRUE)[[1]])
        base_col <- .eaa_tab_col(substring(ws, nlpos + 1L))
      }
      pre_as <- line_start:(s$as_idx - 1L)
      pre_as <- pre_as[t_type[pre_as] != "VOID" & (is.na(ws_idx) | pre_as != ws_idx)]
      cols <- c(cols, base_col + sum(nchar(t_text[pre_as])))
    }
    col_of[L:last] <- if (length(cols)) as.integer(names(sort(table(cols), decreasing = TRUE))[1]) else 0L
    fc <- segs[[grp[1]]]$content_idx
    first_live[L:last] <- if (length(fc)) fc[1] else L
  }
  list(in_fl = in_fl, col_of = col_of, first_live = first_live)
}

#' Build a `style(run) -> run` closure driving the real passes (plan
#' section 3/9's plug point). Context is derived ONCE from `parent` and
#' shared by every run the closure is later called on - GUARD 2, and what
#' lets this plug straight into verify_substream.R's existing
#' .check_p2_fixed_point() as just a different `style` argument (that
#' function's own reverse-order processing, not this closure, is what
#' keeps a multi-run pass safe for a line-count-changing style like this
#' one - see its docstring).
#'
#' @param parent the parent token stream the runs were extracted from -
#'   read-only, used only to derive context; never mutated or returned.
make_comment_styler <- function(parent) {
  pctx <- .csd_parent_field_context(parent)
  function(run) {
    idx <- run$comment_idx[1]
    ctx <- if (idx <= length(pctx$in_fl) && pctx$in_fl[idx]) {
      list(target_col = pctx$col_of[idx], first_field = idx < pctx$first_live[idx])
    } else {
      list(target_col = NULL, first_field = TRUE)
    }
    .csd_style_run(run, ctx)$run
  }
}

# ---- whole-parent driver ------------------------------------------------

#' Style every extracted comment run of `tokens`, serializing each back
#' into the parent as it goes, in REVERSE source order. A style that
#' changes a run's line count shifts every index AFTER it via
#' splice_tokens() (rows inserted, later positions all move up) - going
#' highest-comment_idx-first means every run still being processed sits
#' BEFORE any edit already made, so its own `comment_idx` (captured once,
#' up front, from the unedited parent) is still exactly where
#' find_comment_runs() put it. Processing forward instead corrupts every
#' run after the first one whose style changes line count - caught
#' directly on fixtures/[Grant Managing Region].txt (34 runs, several
#' genuinely reflow), not by inspection: forward order left 14/34 runs
#' silently re-extracting to the WRONG (shifted-into) text.
#'
#' NOT the pipeline entry point - run_pipeline.R does not call this yet
#' (a later task wires it in, replacing pass 8). This is what
#' verify_substream.R's idempotence/coverage checks call directly.
#'
#' @return list(tokens, warnings, changes, coverage). `changes` is
#'   data.frame(line_start, line_end, kind, bucket), one row per extracted
#'   run. `coverage` is a named-count table of `bucket` values.
style_comment_substream <- function(tokens) {
  ex <- extract_comment_runs(tokens)
  pctx <- .csd_parent_field_context(tokens)

  work <- tokens
  ch_start <- integer(0); ch_end <- integer(0)
  ch_kind  <- character(0); ch_bucket <- character(0)

  for (r in rev(ex$runs)) {
    if (r$status != "extracted") next
    idx <- r$comment_idx[1]
    ctx <- if (idx <= length(pctx$in_fl) && pctx$in_fl[idx]) {
      list(target_col = pctx$col_of[idx], first_field = idx < pctx$first_live[idx])
    } else {
      list(target_col = NULL, first_field = TRUE)
    }
    res <- .csd_style_run(r, ctx)
    work <- serialize_comment_run(work, res$run)

    ch_start  <- c(ch_start, r$line_start)
    ch_end    <- c(ch_end, r$line_end)
    ch_kind   <- c(ch_kind, r$kind %||% NA_character_)
    ch_bucket <- c(ch_bucket, res$stats$bucket)
  }

  changes <- data.frame(line_start = ch_start, line_end = ch_end,
                        kind = ch_kind, bucket = ch_bucket,
                        stringsAsFactors = FALSE)
  list(tokens = work, warnings = ex$warnings, changes = changes,
       coverage = table(bucket = ch_bucket))
}
