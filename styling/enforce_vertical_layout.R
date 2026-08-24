# enforce_vertical_layout.R
#
# Style-guide pass: normalises line breaks, indentation and blank lines
# between statements - the "vertical layout" pass, DESIGN §3.4/§4.5/§4.8/§6.2.
# Consumes find_block_structure() (qlik_tokenizer.R).
#
#   - Indentation is FLAT (DESIGN §4.5, decided 2026-08-17): 1 tab for a
#     statement line, 2 for a field line, 3 for a continuation line (a line
#     break before a field's expression finishes), 0 for a true developer
#     comment, 0 for a SET/LET directive (DESIGN §4.5). It does NOT scale
#     with FOR/IF/SUB/DO/SWITCH nesting - see DESIGN §4.5 for why (simpler,
#     avoids compounding §3.6's tab drift).
#   - A LOAD list's true first field (no leading separator comma) gets a
#     two-space pad after its indent (DESIGN §4.4/§4.5), so its content
#     lines up with the ", " in front of every comma-led field below it.
#   - Blank lines (DESIGN §4.8): exactly two between top-level statements,
#     none inside one. A whole control-flow block counts as ONE statement
#     for this rule (its closing NEXT/END IF/etc. included) - that is what
#     find_block_structure()'s stmt_id already encodes.
#   - A comment (or contiguous run of comments) immediately before the next
#     real statement, with no ///$tab section in between, is treated as
#     belonging to THAT statement for blank-line purposes: the two-blank-line
#     gap goes above the comment, not between the comment and the code it
#     describes - e.g. app-unbuilt/script.qvs's
#         //These are the records of the actual cars...
#         [Fleet Cars]:
#     stays glued together. Convention call 2026-08-17, CONFIRMED by Adam
#     the same day - reviewed in output, working, no further work.
#   - `///$tab` section markers (DESIGN §4.8/§6.2, Adam 2026-08-17): the
#     WHOLE line carrying one is left completely untouched. The gap
#     immediately AFTER one now has its indentation fixed like any other
#     line (Adam 2026-08-17 - a table label directly after a section was
#     wrongly left at column 0); only the blank-line COUNT of that gap
#     stays untouched, same protection a SET/LET directive's own gaps get.
#   - SET/LET directives (DESIGN §4.5/§4.8, Adam 2026-08-17): 0 indent, and the
#     blank-line count on EITHER side of one is left exactly as authored -
#     never normalised to the two-blank-line rule, never collapsed to zero.
#   - FROM clause (DESIGN §4.9, Adam 2026-08-17): the format spec's opening
#     paren - "(qvd)" and similar - joins the FROM path on one line, one
#     space between, even when Qlik's own export wrapped it onto its own
#     line (the common case). This is the one rule in this pass that
#     removes a line break rather than just renormalising indent/blanks.
#     Rule confirmed directly by Adam, not inferred from fixture content -
#     app-unbuilt/script.qvs and [Grant Managing Region].txt are raw,
#     unstyled input, never a style reference (see CLAUDE.md).
#   - No new canonical_stream rule needed in verify.R: this pass only
#     rewrites WS token TEXT (never reorders or edits a content token), and
#     WS/COMMENT/VOID are already stripped before canonical_stream compares
#     anything, so blank-line and indentation changes are invisible to the
#     equivalence check by construction.
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Vanilla base R only.

.qvl_indent <- c(statement = "\t", field = "\t\t", continuation = "\t\t\t",
                  comment = "", directive = "")

#' Physically relocate a lone ';' that has one or more comment lines sitting
#' between it and its real target back to directly follow the last real
#' (non-trivia) token - DESIGN §4.9 extension, Adam 2026-08-17. Needed
#' because the normal join technique (collapse the WS between two tokens to
#' "") would, across a comment, land the ';' at the END of the comment's own
#' line - i.e. inside the comment, silently commenting the terminator out
#' forever. This is the one place in this pass that moves a content token
#' rather than only rewriting whitespace text; it runs BEFORE
#' find_block_structure() so every index everything downstream sees is
#' already consistent with the moved token.
#' After this runs, every remaining lone ';' is exactly "one WS token after
#' its target" - the same shape the FROM/(qvd) case already collapses in the
#' main loop below, so that loop needs no separate branch for the moved ones.
.qvl_join_orphan_semicolons <- function(tokens) {
  n <- nrow(tokens)
  if (n == 0) return(tokens)
  t_type <- tokens$type
  t_text <- tokens$text
  lower  <- tolower(t_text)
  in_select <- in_select_region(t_type, lower)
  PREV <- prev_non_trivia_idx(t_type)

  # "Lone" = starts its own physical line: immediately preceded by a
  # newline-bearing WS token.
  is_lone_semi <- t_type == "SEMI" & !in_select &
    c(FALSE, t_type[-n] == "WS") &
    c(FALSE, grepl("\n", t_text[-n], fixed = TRUE))
  candidates <- which(is_lone_semi)
  if (length(candidates) == 0L) return(tokens)

  # Needs moving only when something besides that one WS token sits between
  # the ';' and its target - prev_non_trivia_idx() already skips WS/COMMENT/
  # VOID, so "target isn't two rows back" can only mean a comment (or
  # comment/blank-line chain) is in the way.
  needs_move <- candidates[!is.na(PREV[candidates]) & PREV[candidates] != candidates - 2L]
  if (length(needs_move) == 0L) return(tokens)

  insertions <- list()
  for (k in needs_move) {
    target <- PREV[k]
    insertions[[as.character(target)]] <- data.frame(
      text = ";", type = "SEMI", line = tokens$line[target],
      stringsAsFactors = FALSE)
  }
  tokens <- void_token(tokens, needs_move)
  splice_tokens(tokens, insertions)
}

#' Count of newline characters in a whitespace token's text - used to
#' reproduce a gap's original blank-line count exactly, when that count is
#' being left untouched rather than normalised (DESIGN §4.8).
.qvl_count_newlines <- function(s) nchar(gsub("[^\n]", "", s))

#' Indices of EVERY WS token in the whitespace run immediately preceding a
#' line start - increasing order, integer(0) if there is none (only possible
#' for the very first line of the file, if it has no leading blank
#' lines/whitespace at all).
#'
#' The run, not just its last token. An earlier pass voids tokens IN PLACE
#' rather than deleting rows (INTERFACES.md), so one physical gap can arrive
#' here as WS/VOID/WS - e.g. `WS(" ") VOID WS("\n\t\t")` after pass 4 blanks
#' something mid-gap. Rewriting only the WS nearest the line start left the
#' earlier fragment stranded, and with it any trailing space or extra
#' newline it carried: that was this pass's idempotence bug (2026-08-20), the
#' whole of the 11-line drift on a second run over [Grant Managing Region].txt.
#' Callers must therefore read `before` as the CONCATENATION of the run and
#' blank every fragment but the last when they write.
.preceding_ws_run <- function(type, idx) {
  j <- idx - 1L
  while (j >= 1L && (type[j] == "VOID" || type[j] == "WS")) j <- j - 1L
  run <- seq_len(idx - 1L)
  run <- run[run > j]
  run[type[run] == "WS"]
}

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @param context optional list. `context$base_depth`: extra tab count added
#'   ahead of every line's own flat kind-based indent (PLAN sections 3/5). NULL/0
#'   (default) derives exactly today's behaviour. A child stream's own
#'   `find_block_structure()` has no enclosing LOAD keyword to see, so on its
#'   own it reads as top-level (statement = 1 tab) even when the block it
#'   came from actually sits at field depth - `base_depth` corrects that
#'   without reinterpreting `kind` itself. Applied uniformly, including the
#'   stream's own line 1 (that line's blank-line collapse is a separate
#'   concern from its indent - see the `i == 1L` branch below), so a child
#'   stream is never quietly treated as top-level just because it is line 1
#'   of ITS OWN token stream.
#' @return a list with:
#'   $tokens   - the token stream with vertical whitespace normalised
#'   $warnings - find_block_structure()'s warnings (unbalanced blocks,
#'               ///$tab markers sharing a line), passed through unchanged
#'   $changes  - data.frame(line, kind, before, after), one row per
#'               rewritten gap. "kind" is the line's own kind (statement /
#'               field / continuation / comment / directive) - never
#'               "section", since
#'               those are never rewritten.
enforce_vertical_layout <- function(tokens, context = NULL) {
  base_depth <- 0L
  if (!is.null(context) && !is.null(context$base_depth)) base_depth <- as.integer(context$base_depth)

  tokens <- .qvl_join_orphan_semicolons(tokens)
  bs <- find_block_structure(tokens)
  L <- bs$lines
  warn <- bs$warnings

  empty <- data.frame(line = integer(0), kind = character(0),
                      before = character(0), after = character(0),
                      stringsAsFactors = FALSE)
  if (nrow(L) == 0) return(list(tokens = tokens, warnings = warn, changes = empty))

  nlines <- nrow(L)
  t_type <- tokens$type
  t_text <- tokens$text

  # Effective group for blank-line purposes: a comment inherits the stmt_id
  # of the NEXT non-comment line, so a comment block stays glued to what it
  # describes rather than to what precedes it - UNLESS a ///$tab section
  # sits in between, in which case the comment keeps its own id and that
  # boundary is never crossed.
  eff_id <- L$stmt_id
  i <- nlines
  while (i >= 1L) {
    if (L$kind[i] == "comment") {
      j <- i + 1L
      while (j <= nlines && L$kind[j] == "comment") j <- j + 1L
      if (j <= nlines && L$kind[j] != "section") eff_id[i] <- eff_id[j]
    }
    i <- i - 1L
  }

  ch_line <- integer(nlines);   ch_kind  <- character(nlines)
  ch_before <- character(nlines); ch_after <- character(nlines)
  nch <- 0L

  # Fragments of a multi-row gap that this pass emptied - see
  # .preceding_ws_run(). Blanked in t_text as we go, then voided in ONE call
  # after the loop (INTERFACES.md: never delete a row, batch the edit).
  stranded <- integer(0)

  # Line 1 is handled separately, AFTER this loop: if it needs a leading
  # indent and has no WS token at all to rewrite (true of both fixtures - a
  # real file typically starts directly with content), inserting one would
  # shift every ORIGINAL index this loop still relies on for lines 2..n. So
  # this loop only ever rewrites EXISTING tokens in place - nothing here
  # changes the row count - and the one-time insertion, if needed, happens
  # only once every other index has already been used.
  need_line1_indent <- NA_character_

  for (i in seq_len(nlines)) {
    kind_i <- L$kind[i]
    if (kind_i == "section") next                          # whole line: untouched

    # FROM clause (DESIGN §4.9, Adam 2026-08-17): the format spec's opening
    # paren joins the path on the SAME line as FROM, one space between -
    # even though Qlik's own default export wraps it onto its own line.
    # Detected structurally - the immediately preceding line starts with
    # the WORD "from" - not by kind, so this bypasses the normal
    # indent/blank-line machinery entirely: the gap collapses to a literal
    # single space, no newline.
    if (i > 1L) {
      prev_idx <- L$idx[i - 1L]
      if (t_type[L$idx[i]] == "LPAREN" && t_type[prev_idx] == "WORD" &&
          tolower(t_text[prev_idx]) == "from") {
        run <- .preceding_ws_run(t_type, L$idx[i])
        if (length(run) > 0L) {
          before <- paste0(t_text[run], collapse = "")
          if (!identical(before, " ")) {
            nch <- nch + 1L
            ch_line[nch] <- L$line[i]; ch_kind[nch] <- kind_i
            ch_before[nch] <- before; ch_after[nch] <- " "
            t_text[run] <- ""
            t_text[run[length(run)]] <- " "
            stranded <- c(stranded, run[-length(run)])
          }
        }
        next
      }
    }

    # Trailing ';' (DESIGN §4.9 extension, Adam 2026-08-17): a lone ';' line
    # joins onto whatever precedes it - tight, no space - the same technique
    # as the FROM/(qvd) case above. Any ';' that had a comment in the way was
    # already relocated by .qvl_join_orphan_semicolons() before this loop
    # runs, so every case reaching here is exactly "SEMI immediately after
    # one WS token."
    if (i > 1L && t_type[L$idx[i]] == "SEMI") {
      run <- .preceding_ws_run(t_type, L$idx[i])
      if (length(run) > 0L) {
        before <- paste0(t_text[run], collapse = "")
        if (!identical(before, "")) {
          nch <- nch + 1L
          ch_line[nch] <- L$line[i]; ch_kind[nch] <- kind_i
          ch_before[nch] <- before; ch_after[nch] <- ""
          t_text[run] <- ""
          stranded <- c(stranded, run[-length(run)])
        }
      }
      next
    }

    indent <- paste0(strrep("\t", base_depth), .qvl_indent[[kind_i]])
    if (kind_i == "field" && isTRUE(L$first_field[i])) indent <- paste0(indent, "  ")

    run <- .preceding_ws_run(t_type, L$idx[i])

    if (length(run) == 0L) {
      # Only possible for line 1 with no leading whitespace at all.
      if (nchar(indent) > 0L) need_line1_indent <- indent
      next
    }

    before <- paste0(t_text[run], collapse = "")

    # Blank-line COUNT is left exactly as authored - indentation is still
    # fixed - on either side of a directive (SET/LET) and immediately after
    # a ///$tab section marker (DESIGN §4.8, Adam 2026-08-17). A
    # directive's own incoming gap is protected by kind_i; its outgoing gap,
    # and a section's outgoing gap, are protected by the PREVIOUS line's
    # kind - the section's own line above is already skipped entirely, so
    # only "coming out of one" needs handling here.
    preserve_blanks <- kind_i == "directive" ||
      (i > 1L && L$kind[i - 1L] %in% c("section", "directive"))

    target <- if (i == 1L) {
      indent   # nothing precedes line 1 - collapse any leading blank lines
    } else if (preserve_blanks) {
      paste0(strrep("\n", .qvl_count_newlines(before)), indent)
    } else {
      blanks <- if (eff_id[i] != eff_id[i - 1L]) 2L else 0L
      paste0(strrep("\n", blanks + 1L), indent)
    }

    if (identical(before, target)) next

    nch <- nch + 1L
    ch_line[nch] <- L$line[i]; ch_kind[nch] <- kind_i
    ch_before[nch] <- before; ch_after[nch] <- target
    t_text[run] <- ""
    t_text[run[length(run)]] <- target
    stranded <- c(stranded, run[-length(run)])
  }

  tokens$text <- t_text
  if (length(stranded) > 0L) tokens <- void_token(tokens, stranded)

  if (!is.na(need_line1_indent)) {
    tokens <- rbind(
      data.frame(text = need_line1_indent, type = "WS", line = 1L, stringsAsFactors = FALSE),
      tokens)
    nch <- nch + 1L
    ch_line[nch] <- L$line[1]; ch_kind[nch] <- L$kind[1]
    ch_before[nch] <- ""; ch_after[nch] <- need_line1_indent
  }

  changes <- data.frame(
    line   = ch_line[seq_len(nch)],
    kind   = ch_kind[seq_len(nch)],
    before = ch_before[seq_len(nch)],
    after  = ch_after[seq_len(nch)],
    stringsAsFactors = FALSE
  )

  list(tokens = tokens, warnings = warn, changes = changes)
}
