# enforce_commented_field_style.R - pass 8
#
# Applies the §4 styling rules to commented-out LOAD fields (DESIGN §4.10,
# Adam 2026-08-20). Every rule a live field gets - casing, bracketing,
# explicit aliasing, leading commas, intra-line spacing, alias alignment -
# a commented-out field gets too, so that uncommenting one yields a line
# that reloads without a hand-fix.
#
# THE INVARIANT, and the whole reason this pass has the shape it does:
# it rewrites the TEXT OF COMMENT TOKENS AND NOTHING ELSE. No live token is
# added, removed, moved or altered. Commented script never becomes live
# tokens, so it cannot participate in - or be corrupted by - anything the
# other passes do.
#
# That is not a stylistic preference; it was learned. The first version of
# this feature unwrapped commented fields into real tokens, ran passes 1-7
# over them and re-commented afterwards, which bought shared alias
# alignment for free but put commented script into the live stream. Four
# distinct corruptions followed, each caught by verify.R at stage 3:
#   - pass 3 relocated field separators ACROSS the comment/live boundary,
#     emitting "LOAD , [X]" and merging two live fields into one;
#   - pass 6's orphan-";" rescue re-anchored a statement terminator onto a
#     line that was about to become a comment, so the LOAD lost its
#     terminator and ran on into the following SELECT;
#   - a re-run re-read the styled leading comma as part of the field and
#     added another, growing one comma per run;
#   - multi-line commented expressions were styled per line, so their paren
#     depth never returned to zero.
# All four are impossible here by construction. See git history for the
# unwrap/rewrap implementation if the trade ever needs revisiting.
#
# "Never grow a token's $text to inject syntax" (INTERFACES.md) is about
# syntax that LATER passes must recognise by type. This pass runs last and
# its output stays inside a comment, where no pass and no Qlik parser will
# ever read it as syntax. Rewriting the text is correct precisely here.
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Depends on passes 1, 2, 4 and 5 for the styling itself: it builds a
# synthetic one-field LOAD, runs the REAL passes over it, and reads the
# result back. Nothing about the style rules is reimplemented here.
# Vanilla base R only.

# Words that mean "this comment is a whole statement, not a field". A
# commented-out statement is out of scope (DESIGN §4.10 is field-list only).
.scf_statement_words <- c(
  "load", "select", "from", "resident", "inline", "autogenerate", "where",
  "group", "order", "join", "keep", "concatenate", "store", "drop", "rename",
  "let", "set", "call", "sub", "end", "exit", "for", "next", "do", "loop",
  "switch", "case", "trace", "when", "unless", "qualify", "unqualify",
  "connect", "lib", "directory", "alias", "star", "mapping",
  "noconcatenate", "buffer"
)

# Words that may legitimately sit bare inside a field expression. Anything
# else bare next to another bare word means prose.
.scf_field_words <- c(
  "as", "and", "or", "not", "xor", "like", "distinct",
  "if", "then", "else", "end", "null", "true", "false"
)

.SCF_TAB <- 4L

#' Column a string ends at, expanding tabs to .SCF_TAB stops.
#' Local to this pass by the same reasoning INTERFACES.md records for
#' .eaa_preceding_ws_idx: a few lines, not worth a shared-scanner entry.
.scf_col <- function(s) {
  col <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    col <- if (ch == "\t") (col %/% .SCF_TAB + 1L) * .SCF_TAB else col + 1L
  }
  col
}

#' Does this comment body parse as a LOAD field rather than prose?
#'
#' Conservative on purpose - a false positive rewrites an English sentence
#' as pseudo-code. Three independent tests, all must pass.
#'
#' @param body character(1), comment text with "//" already stripped.
.scf_body_is_field <- function(body) {
  if (!nzchar(trimws(body))) return(FALSE)
  bt <- tokenize_qlik(body)
  solid <- bt[!(bt$type %in% c("WS", "COMMENT", "VOID")), , drop = FALSE]
  ns <- nrow(solid)
  if (ns == 0) return(FALSE)

  # punctuation the tokenizer has no rule for - a bullet, prose apostrophe
  if (any(solid$type == "OTHER")) return(FALSE)

  lower <- tolower(solid$text)
  if (lower[1] %in% .scf_statement_words) return(FALSE)

  # TEST 1 - it must reference something: a bracketed/quoted name, a
  # qualified or system-suffixed bare name ("Table.Field", "Id%"), or a
  # function call. Prose has none of these.
  is_call <- solid$type == "WORD" & c(solid$type[-1] == "LPAREN", FALSE)
  if (!(any(solid$type %in% c("BRACKET", "DQUOTE")) ||
        any(solid$type == "WORD" & grepl("[.%$#]", solid$text)) ||
        any(is_call))) return(FALSE)

  # TEST 2 - prose discriminator. Two adjacent bare non-keyword words is
  # English ("check this first"); in a field expression bare words are
  # always separated by an operator, comma, paren or keyword.
  w <- solid$type == "WORD" & !(lower %in% .scf_field_words)
  if (ns > 1 && any(w[-ns] & w[-1])) return(FALSE)

  # TEST 3 - the fragment must stand alone. A commented-out expression can
  # span several lines, each its own "//" token; the first opens parens it
  # never closes. Styling half an expression is meaningless, and its depth
  # never returns to zero. Real case in "[Grant Managing Region].txt": a
  # commented IF(LEN(...))/IF(MATCH(...)) pair split across two lines.
  depth <- cumsum((solid$type == "LPAREN") - (solid$type == "RPAREN"))
  if (any(depth < 0) || depth[ns] != 0) return(FALSE)

  TRUE
}

#' Style one field fragment by running the REAL passes over it.
#'
#' Builds a synthetic single-field LOAD, applies passes 1, 2, 4 and 5, and
#' reads the field back. Passes 3, 6 and 7 are deliberately NOT used: comma
#' placement, indentation and the alignment column are all supplied by the
#' caller, which knows the comment's context and the synthetic stream does
#' not.
#'
#' @param core character(1), the field with any separator commas removed.
#' @return character(1), the styled field on one line, or NA on failure.
.scf_style_fragment <- function(core) {
  synth <- tokenize_qlik(paste0("[T]:\nLOAD ", core, "\nFROM x;"))
  synth <- ensure_explicit_aliases(synth)$tokens
  synth <- enforce_bracket_references(synth)$tokens
  synth <- enforce_intraline_spacing(synth)$tokens
  synth <- enforce_reserved_word_case(synth)$tokens

  ld <- which(synth$type == "WORD" & tolower(synth$text) == "load")
  fr <- which(synth$type == "WORD" & tolower(synth$text) == "from")
  if (length(ld) == 0 || length(fr) == 0 || fr[1] <= ld[1] + 1L) return(NA_character_)
  out <- paste(synth$text[(ld[1] + 1L):(fr[1] - 1L)], collapse = "")
  # the synthetic wrapper contributed the newline before FROM
  trimws(gsub("[\r\n]+", " ", out))
}

#' Apply §4 styling to commented-out LOAD fields.
#'
#' @param tokens a token stream data.frame, after pass 7.
#' @return a list with:
#'   $tokens   - the stream with eligible COMMENT tokens' text restyled.
#'               No other token is touched in any way.
#'   $warnings - character vector, one per comment left alone for a reason
#'               worth surfacing (currently: block comments in a field list)
#'   $changes  - data.frame(line, before, after), one row per restyled comment
enforce_commented_field_style <- function(tokens) {
  warn <- character(0)
  n <- nrow(tokens)
  empty <- data.frame(line = integer(0), before = character(0),
                      after = character(0), stringsAsFactors = FALSE)
  if (n == 0) return(list(tokens = tokens, warnings = warn, changes = empty))

  found <- find_load_segments(tokens)
  warn <- c(warn, found$warnings)
  segs <- found$segments
  if (length(segs) == 0) return(list(tokens = tokens, warnings = warn, changes = empty))

  seg_load <- vapply(segs, function(s) s$load_tok_idx, integer(1))
  seg_end  <- vapply(segs, function(s) s$end, integer(1))

  # Per LOAD: the token range of its field list, the alignment column its
  # live fields settled on, and where its first live field starts.
  fl_end <- integer(n); in_fl <- logical(n)
  col_of <- integer(n); first_live <- integer(n)
  for (L in unique(seg_load)) {
    grp <- which(seg_load == L)
    last <- max(seg_end[grp])
    in_fl[L:last] <- TRUE
    fl_end[L:last] <- last

    # column each live field's AS sits at, measured from its own line start
    cols <- integer(0)
    for (k in grp) {
      s <- segs[[k]]
      if (!s$has_as) next
      lt <- .scf_line_text_before(tokens, s$as_idx)
      if (is.na(lt)) next
      cols <- c(cols, .scf_col(lt))
    }
    # pass 7 aligns a block to ONE column and excludes outliers, so the
    # modal column is the block's column; max would follow an outlier out
    col_of[L:last] <- if (length(cols)) as.integer(names(sort(table(cols),
                                                     decreasing = TRUE))[1]) else 0L
    fc <- segs[[grp[1]]]$content_idx
    first_live[L:last] <- if (length(fc)) fc[1] else L
  }

  # A comment must OWN its source line: a trailing comment shares its line
  # with live code, and this pass rewrites whole lines.
  solid <- which(!(tokens$type %in% c("WS", "VOID")))
  owns <- logical(n); owns[solid[!duplicated(tokens$line[solid])]] <- TRUE

  cand <- which(tokens$type == "COMMENT" & in_fl & owns)

  ch_line <- integer(0); ch_before <- character(0); ch_after <- character(0)

  for (i in cand) {
    txt <- tokens$text[i]
    if (startsWith(txt, "///$")) next
    if (startsWith(txt, "/*")) {
      warn <- c(warn, sprintf(
        "Line %d: block comment inside a LOAD field list left unstyled (only // comments are styled).",
        tokens$line[i]))
      next
    }

    body <- sub("^//+", "", txt)
    if (!.scf_body_is_field(body)) next

    # Strip separator commas at both ends. A leading one is this pass's own
    # output from an earlier run; a trailing one is the author's. Neither is
    # part of the field, and re-deriving both is what makes the pass
    # idempotent regardless of which form the input is in.
    core <- trimws(body)
    core <- sub("^,[ \t]*", "", core)
    core <- sub("[ \t]*,$", "", core)

    styled <- .scf_style_fragment(core)
    if (is.na(styled)) next

    # §4.5: two tabs of field indent, then either the ", " separator or the
    # two-space pad the first field of a LOAD gets. "//" occupies columns
    # 0-1, which the first tab stop absorbs, so the content lands in exactly
    # the same columns as the live fields beside it.
    is_first <- i < first_live[i]
    prefix <- paste0("//", "\t\t", if (is_first) "  " else ", ")

    # pad to the block's alias column, tabs only (Adam, non-negotiable)
    at <- regexpr(" AS ", styled, fixed = TRUE)
    if (at > 0) {
      head_txt <- substr(styled, 1, at - 1)
      tail_txt <- substr(styled, at + 1, nchar(styled))
      target <- col_of[i]
      pad <- ""
      # Pass 7's convention: tabs go BEFORE the single space it preserves, so
      # AS lands one column PAST a tab stop. Pad to target - 1 accordingly.
      while (.scf_col(paste0(prefix, head_txt, pad, "\t")) <= target - 1L) {
        pad <- paste0(pad, "\t")
        if (nchar(pad) > 64L) break
      }
      new_txt <- paste0(prefix, head_txt, pad, " ", tail_txt)
    } else {
      new_txt <- paste0(prefix, styled)
    }

    if (identical(new_txt, txt)) next
    tokens$text[i] <- new_txt
    ch_line <- c(ch_line, tokens$line[i])
    ch_before <- c(ch_before, txt)
    ch_after <- c(ch_after, new_txt)
  }

  list(tokens = tokens, warnings = warn,
       changes = data.frame(line = ch_line, before = ch_before, after = ch_after,
                            stringsAsFactors = FALSE))
}

#' The printed text of the line containing `idx`, up to but excluding it.
#'
#' GOTCHA: the whitespace token that carries a line break also carries the
#' NEXT line's indentation - enforce_vertical_layout.R rewrites that one gap
#' per line, newline and indent together. So the line does not start at the
#' token after the break; it starts partway INTO it. Taking the following
#' token as the line start silently drops the indent and measured every
#' alias column two tabs (8 columns) short.
.scf_line_text_before <- function(tokens, idx) {
  if (is.na(idx) || idx < 1L) return(NA_character_)
  i <- idx - 1L
  while (i >= 1L) {
    if (tokens$type[i] == "WS" && grepl("\n", tokens$text[i], fixed = TRUE)) {
      gap <- tokens$text[i]
      indent <- substr(gap, max(gregexpr("\n", gap, fixed = TRUE)[[1]]) + 1L,
                       nchar(gap))
      rest <- if (i + 1L <= idx - 1L)
        paste(tokens$text[(i + 1L):(idx - 1L)], collapse = "") else ""
      return(paste0(indent, rest))
    }
    i <- i - 1L
  }
  paste(tokens$text[seq_len(max(idx - 1L, 0L))], collapse = "")
}
