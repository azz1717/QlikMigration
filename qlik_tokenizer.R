# qlik_tokenizer.R
#
# Shared I/O layer for the loadscript style-guide passes. Tokenize a script
# exactly once; every pass operates on the resulting token stream (a
# data.frame) and hands it on to the next pass. Only detokenize() back to
# text when you need to write a file or diff against another stage.
#
#   tokens <- read_qlik_script("script.qvs")
#   tokens <- ensure_explicit_aliases(tokens)$tokens
#   tokens <- some_other_pass(tokens)$tokens
#   writeLines(detokenize(tokens), "script_out.qvs")
#
# Token stream shape: data.frame(text, type, line), one row per token.
#   - text: the token's exact literal substring (concatenating $text in
#     order always reproduces the source exactly - every character of the
#     input belongs to some token).
#   - type: one of COMMENT, DQUOTE, SQUOTE, BRACKET, WS, WORD, COMMA, SEMI,
#     LPAREN, RPAREN, OTHER.
#   - line: the 1-based source line the token starts on.
#
# Vanilla base R only.

# ---- tokenizer ---------------------------------------------------------
# Flat, single-pass tokenizer. Comments, quoted strings, and bracketed
# names do not nest, so one alternation regex is enough to tokenize the
# whole script.
#
# Matching runs in BYTE mode (useBytes = TRUE, in tokenize_qlik below). That
# is a performance requirement, not a style choice. On a string R knows
# contains non-ASCII, PCRE reports BYTE offsets but R reports CHARACTER
# offsets, so gregexpr() converts every match position and regmatches()
# walks the string from the start for every token - both O(n) per match,
# i.e. quadratic overall. Because the whole script is tokenized as a single
# string, ONE smart quote anywhere in the file drags every token onto that
# slow path: a 1MB script took ~38s, and a single 216KB region of prose
# pasted out of Word accounted for 6.3s of it. Byte mode removes the offset
# conversion outright (measured 746x faster on that region).
#
# The catch is that in byte mode "." matches one BYTE, which would shatter
# each multibyte character into fragments that are not valid UTF-8 and that
# error out tolower() in the passes. So the catch-all below matches one
# whole UTF-8 ENCODED CHARACTER instead, and tokenize_qlik() re-marks the
# extracted tokens as UTF-8. Nothing is normalized or rewritten: the token
# stream is identical to the character-mode one, and detokenize() still
# reproduces the input byte for byte.

# one UTF-8 encoded character, expressed as byte ranges
.qlik_utf8_char <- paste0(
  "[\\x00-\\x7F]",                  # ASCII
  "|[\\xC2-\\xDF][\\x80-\\xBF]",    # 2-byte sequence
  "|[\\xE0-\\xEF][\\x80-\\xBF]{2}", # 3-byte sequence
  "|[\\xF0-\\xF4][\\x80-\\xBF]{3}"  # 4-byte sequence
)

# EDITING THIS PATTERN
#
# Alternation is first-match-wins, so ORDER IS SIGNIFICANT. Two rules:
#   - longer alternatives must precede their own prefixes. ">=" has to come
#     before ">", or ">=" tokenizes as two tokens.
#   - the UTF-8 catch-all stays LAST, and stays UTF-8-aware. Replacing it with
#     "." would shatter every multibyte character into invalid fragments and
#     break tolower() in the passes - see the byte-mode note above.
# Matching runs in byte mode, so character classes are byte classes. That is
# fine for the ASCII syntax below; do not add a class expecting it to match a
# multibyte character.

.qlik_token_pattern <- paste0(
  "//[^\n]*",                     # line comment
  "|/\\*[\\s\\S]*?\\*/",          # block comment
  "|\"(?:[^\"]|\"\")*\"",         # double-quoted identifier
  "|'(?:[^']|'')*'",              # single-quoted string literal
  "|\\[[^\\]]*\\]",               # bracketed identifier
  "|[ \t\r\n]+",                  # whitespace
  "|[A-Za-z_][A-Za-z0-9_.$%#]*",  # bare word / identifier / keyword
  "|[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?",  # numeric literal
  "|>=|<=|<>",                    # multi-char operators - BEFORE the single
  "|[-+*/=<>&]",                  # single-char operators
  "|", .qlik_utf8_char            # anything else, one character at a time
)

.qlik_token_type <- function(tok) {
  if (startsWith(tok, "//")) return("COMMENT")
  if (startsWith(tok, "/*")) return("COMMENT")
  ch1 <- substr(tok, 1, 1)
  if (ch1 == "\"") return("DQUOTE")
  if (ch1 == "'")  return("SQUOTE")
  if (ch1 == "[")  return("BRACKET")
  if (grepl("^[ \t\r\n]+$", tok)) return("WS")
  if (grepl("^[A-Za-z_]", tok)) return("WORD")
  if (tok == ",") return("COMMA")
  if (tok == ";") return("SEMI")
  if (tok == "(") return("LPAREN")
  if (tok == ")") return("RPAREN")
  if (grepl("^[0-9]", tok)) return("NUMBER")
  # "*" is typed as an operator but is also the LOAD wildcard, and "-" is both
  # binary and unary minus. The tokenizer cannot tell those apart; a pass that
  # spaces operators has to check context.
  if (tok %in% c(">=", "<=", "<>", "-", "+", "*", "/", "=", "<", ">", "&"))
    return("OPERATOR")
  "OTHER"
}

#' Tokenize a Qlik script.
#'
#' @param text a single string (use paste(lines, collapse = "\n") if you
#'   have a character vector of lines).
#' @return a data.frame(text, type, line) token stream.
tokenize_qlik <- function(text) {
  # byte mode, then re-mark as UTF-8 - see the note above .qlik_token_pattern
  m <- gregexpr(.qlik_token_pattern, text, perl = TRUE, useBytes = TRUE)
  toks <- regmatches(text, m)[[1]]
  Encoding(toks) <- "UTF-8"
  if (length(toks) == 0) {
    return(data.frame(text = character(0), type = character(0),
                       line = integer(0), stringsAsFactors = FALSE))
  }
  types <- vapply(toks, .qlik_token_type, character(1), USE.NAMES = FALSE)

  # line number at the start of each token
  nl_per_tok <- vapply(gregexpr("\n", toks, fixed = TRUE),
                        function(x) sum(x > 0), integer(1))
  cum_nl <- cumsum(nl_per_tok)
  line <- 1L + c(0L, cum_nl[-length(cum_nl)])

  data.frame(text = toks, type = types, line = line, stringsAsFactors = FALSE)
}

#' Read a Qlik loadscript file and tokenize it in one step.
#'
#' @param path file path.
#' @param encoding passed to readLines().
#' @return a token stream data.frame (see tokenize_qlik). The source path
#'   is recorded as an attribute ("source_path") for reference.
read_qlik_script <- function(path, encoding = "UTF-8") {
  lines <- readLines(path, warn = FALSE, encoding = encoding)
  tokens <- tokenize_qlik(paste(lines, collapse = "\n"))
  attr(tokens, "source_path") <- path
  tokens
}

#' Serialize a token stream back to a character vector of lines.
#'
#' Inverse of read_qlik_script()/tokenize_qlik(). Safe to call at any point
#' in a pass pipeline - e.g. to diff one stage against another, or to write
#' the final result with writeLines().
detokenize <- function(tokens) {
  result_text <- paste(tokens$text, collapse = "")
  if (result_text == "") return(character(0))
  result_lines <- strsplit(result_text, "\n", fixed = TRUE)[[1]]
  # strsplit() silently drops a trailing empty field, which loses the last
  # line whenever the script ends in a blank line; restore it.
  if (grepl("\n$", result_text)) result_lines <- c(result_lines, "")
  result_lines
}

#' Splice extra tokens into a token stream immediately after given positions.
#'
#' Passes must not grow a token's $text in place to inject new syntax (e.g.
#' appending " AS X" onto an existing identifier token) - later passes only
#' recognize syntax by token type (a WORD token whose text is "AS"), so
#' anything smuggled into another token's text is structurally invisible
#' and will be missed (or re-added) by the next pass. Use this instead.
#'
#' @param tokens a token stream data.frame.
#' @param insertions a named list keyed by token index (as character, e.g.
#'   "42"), each value a token stream data.frame (text, type, line) to
#'   insert immediately after that index. Indices refer to positions in
#'   the ORIGINAL tokens (not the spliced result).
#' @return the spliced token stream.
splice_tokens <- function(tokens, insertions) {
  n <- nrow(tokens)
  if (length(insertions) == 0 || n == 0) return(tokens)

  ins_idx <- as.integer(names(insertions))
  ins_len <- vapply(insertions, nrow, integer(1))

  # ignore anchors outside the stream, as the old per-row scan did
  keep <- ins_idx >= 1L & ins_idx <= n
  if (!any(keep)) return(tokens)
  ins_idx <- ins_idx[keep]; ins_len <- ins_len[keep]
  insertions <- insertions[keep]

  # add[i] = rows to insert immediately after original row i.
  # Each original row shifts right by the number of rows inserted STRICTLY
  # before it, which is cumsum(add) - add. Computing the output positions
  # arithmetically lets every column be filled with one vectorized assignment
  # instead of extracting and rbind()-ing n single-row data.frames - the
  # latter is what made this the dominant cost of every splicing pass.
  add <- integer(n)
  add[ins_idx] <- ins_len
  pos <- seq_len(n) + (cumsum(add) - add)

  total <- n + sum(add)
  text <- character(total); type <- character(total); line <- integer(total)
  text[pos] <- tokens$text
  type[pos] <- tokens$type
  line[pos] <- tokens$line

  # inserted rows sit immediately after their anchor
  for (k in seq_along(ins_idx)) {
    ins <- insertions[[k]]
    at <- pos[ins_idx[k]] + seq_len(ins_len[k])
    text[at] <- ins$text
    type[at] <- ins$type
    line[at] <- ins$line
  }

  result <- data.frame(text = text, type = type, line = line,
                       stringsAsFactors = FALSE)

  # Carry over any extra attributes the caller hung on the stream - notably
  # "source_path" from read_qlik_script(). Building a fresh data.frame drops
  # them, whereas the row-wise rbind this replaced happened to preserve them.
  extra <- setdiff(names(attributes(tokens)), c("names", "class", "row.names"))
  for (a in extra) attr(result, a) <- attr(tokens, a)

  result
}

#' Remove a token as part of relocating it elsewhere (e.g. moving a comma
#' from a trailing to a leading position).
#'
#' A pass must never delete a row outright - every other index computed
#' from the original tokens (segment bounds, other insertions in the same
#' pass) would silently shift and point at the wrong token. Instead, blank
#' the token in place: empty text contributes nothing on detokenize(), so
#' it vanishes from the output, while every other index stays valid.
#'
#' void_token() is the write half of this convention; find_load_segments()
#' is the read half - it treats type "VOID" as trivia, same as WS/COMMENT,
#' so a later pass that rescans the same token stream (without
#' detokenizing/re-tokenizing first) won't miscount a voided token as a
#' field's content.
#'
#' @param tokens a token stream data.frame.
#' @param idx token index (or vector of indices) to void. Prefer collecting
#'   indices during a pass and voiding them in ONE call: each call assigns
#'   into two data.frame columns, so calling it once per token inside a loop
#'   costs far more than the work it does.
#' @return the token stream with those rows blanked.
void_token <- function(tokens, idx) {
  tokens$text[idx] <- ""
  tokens$type[idx] <- "VOID"
  tokens
}

# ---- shared token-stream scanners ---------------------------------------
# Position-only helpers needed by more than one pass. Promoted here (rather
# than left as a private copy in whichever pass wrote them first) per the
# "shared scanners" principle in DESIGN.md - the SELECT skip used to live
# only inside enforce_reserved_word_case.R; enforce_intraline_spacing.R
# needed the identical logic, so it moved here instead of forking a second
# copy that could drift.

#' TRUE for every token inside a SELECT ... ; region.
#'
#' A WORD "select" opens the region and the next SEMI closes it, with the
#' SEMI itself still inside. This is the same "not ours to touch" boundary
#' find_load_segments() uses internally for LOAD field lists - this version
#' is for passes that scan the WHOLE token stream, not just field lists.
#'
#' @param type token type vector (tokens$type).
#' @param lower lower-cased token text vector (tolower(tokens$text)).
#' @return logical vector, one per token.
in_select_region <- function(type, lower) {
  n <- length(type)
  starts <- which(type == "WORD" & lower == "select")
  semis  <- which(type == "SEMI")
  if (length(starts) == 0) return(logical(n))
  i <- seq_len(n)
  last_sel  <- c(0L, starts)[findInterval(i, starts) + 1L]
  last_semi <- c(0L, semis)[findInterval(i - 1L, semis) + 1L]
  last_sel > last_semi
}

#' Index of the previous non-trivia token for every position, or NA.
#'
#' Trivia = WS, COMMENT, VOID - the same definition find_load_segments() uses
#' for a field segment's content_idx.
#'
#' @param type token type vector.
#' @return integer vector, one per token; NA where there is no previous one.
prev_non_trivia_idx <- function(type) {
  n <- length(type)
  nt <- which(!(type %in% c("WS", "COMMENT", "VOID")))
  k <- findInterval(seq_len(n) - 1L, nt)
  c(NA_integer_, nt)[k + 1L]
}

#' Index of the next non-trivia token for every position, or NA.
#'
#' Symmetric with prev_non_trivia_idx() above. Promoted from a private
#' `.next_content_type` in enforce_reserved_word_case.R (2026-08-17) when
#' enforce_bracket_references.R needed the identical call-position test -
#' both passes must tell a bare word used AS a function/SUB call (skip it)
#' from one used as a field/alias reference (act on it), and both use the
#' same test: is the next non-trivia token an LPAREN.
#'
#' @param type token type vector.
#' @return integer vector, one per token; NA where there is no next one.
next_non_trivia_idx <- function(type) {
  n <- length(type)
  nt <- which(!(type %in% c("WS", "COMMENT", "VOID")))
  nt[findInterval(seq_len(n), nt) + 1L]
}

# ---- shared LOAD field-list scanner ------------------------------------
# Most style-guide passes need to operate per-field within LOAD statements
# while leaving SELECT ... ; (raw SQL passed to LIB CONNECT TO) alone. This
# scan is shared so every pass doesn't reimplement it.

#' Find every field segment across all LOAD field lists in a token stream.
#'
#' @param tokens a token stream data.frame (see tokenize_qlik).
#' @return a list with:
#'   $segments - a list of field segments, each a list with:
#'       start, end   - token index range (inclusive) covering the segment,
#'                      including any leading trivia (whitespace/comments)
#'       content_idx  - integer vector of indices for the segment's
#'                      non-trivia (non WS/COMMENT) tokens
#'       has_as       - TRUE if the segment already has a depth-0 AS clause
#'       as_idx       - token index of that AS keyword, or NA if has_as is
#'                      FALSE (added 2026-08-17 for alias alignment)
#'       line         - source line the segment starts on
#'       load_tok_idx - token index of the "LOAD" keyword that owns this
#'                      segment - groups segments back into their own LOAD
#'                      list (added 2026-08-17 for alias alignment, DESIGN
#'                      §4.6: same value for every field of one LOAD, so
#'                      consecutive segments sharing it are one block)
#'   $warnings - character vector describing any LOAD statement whose end
#'       (FROM/RESIDENT/INLINE/AUTOGENERATE/WHERE/;) could not be found
find_load_segments <- function(tokens) {
  n <- nrow(tokens)
  end_keywords <- c("from", "resident", "inline", "autogenerate", "where")
  segments <- list()
  warn <- character(0)
  if (n == 0) return(list(segments = segments, warnings = warn))

  # Index arithmetic uses INTEGER literals (1L, not 1) throughout. An
  # unsuffixed literal makes the counter a double, which propagates into
  # every index derived from it and out through seg$start / seg$end - where
  # it is invisible until a caller does vapply(..., integer(1)) and gets
  # "values must be type 'integer', but FUN(X[[1]]) result is type 'double'".
  # That cost the spacing pass a debugging cycle. Keep the L suffixes.
  i <- 1L
  in_select <- FALSE

  while (i <= n) {
    ty <- tokens$type[i]

    if (ty == "WORD" && tolower(tokens$text[i]) == "select") {
      in_select <- TRUE
      i <- i + 1L
      next
    }
    if (in_select) {
      if (ty == "SEMI") in_select <- FALSE
      i <- i + 1L
      next
    }

    if (ty == "WORD" && tolower(tokens$text[i]) == "load") {
      load_line <- tokens$line[i]
      start <- i + 1L

      # find end of field list: first depth-0 end-keyword, or a depth-0 ';'
      depth <- 0L
      j <- start
      end <- NA_integer_
      while (j <= n) {
        tj <- tokens$type[j]
        if (tj == "LPAREN") depth <- depth + 1L
        else if (tj == "RPAREN") depth <- depth - 1L
        else if (depth == 0L && tj == "SEMI") { end <- j; break }
        else if (depth == 0L && tj == "WORD" && tolower(tokens$text[j]) %in% end_keywords) { end <- j; break }
        j <- j + 1L
      }
      if (is.na(end)) {
        warn <- c(warn, sprintf(
          "Line %d: reached end of file while parsing LOAD statement - left unchanged.",
          load_line))
        break
      }

      # split [start, end) into depth-0 comma-separated field segments
      seg_start <- start
      depth <- 0L
      k <- start
      bounds <- list()
      while (k < end) {
        tk <- tokens$type[k]
        if (tk == "LPAREN") depth <- depth + 1L
        else if (tk == "RPAREN") depth <- depth - 1L
        else if (tk == "COMMA" && depth == 0L) {
          bounds[[length(bounds) + 1L]] <- c(seg_start, k - 1L)
          seg_start <- k + 1L
        }
        k <- k + 1L
      }
      bounds[[length(bounds) + 1L]] <- c(seg_start, end - 1L)

      for (b in bounds) {
        s_from <- b[1]; s_to <- b[2]
        if (s_from > s_to) next  # empty segment (e.g. trailing comma)

        idxs <- s_from:s_to
        content_idx <- idxs[!(tokens$type[idxs] %in% c("WS", "COMMENT", "VOID"))]
        if (length(content_idx) == 0) next  # comment-only / blank segment

        # look for a depth-0 "AS" among this segment's own tokens
        has_as <- FALSE
        as_idx <- NA_integer_
        d <- 0L
        for (p in idxs) {
          tp <- tokens$type[p]
          if (tp == "LPAREN") d <- d + 1L
          else if (tp == "RPAREN") d <- d - 1L
          else if (d == 0L && tp == "WORD" && tolower(tokens$text[p]) == "as") { has_as <- TRUE; as_idx <- p; break }
        }
        # the alias target's own content tokens (right of AS) - normally a
        # single field/alias reference, but kept general in case of trivia
        alias_content_idx <- if (has_as) content_idx[content_idx > as_idx] else integer(0)

        segments[[length(segments) + 1L]] <- list(
          start = s_from, end = s_to,
          content_idx = content_idx,
          has_as = has_as,
          as_idx = as_idx,
          alias_content_idx = alias_content_idx,
          line = tokens$line[s_from],
          load_tok_idx = i
        )
      }

      i <- end + 1L
      next
    }

    i <- i + 1L
  }

  list(segments = segments, warnings = warn)
}

# ---- shared block-structure scanner -------------------------------------
# The vertical layout pass (DESIGN 6.2) is the first pass needing whole-script
# structure rather than one field segment at a time. This scan answers, for
# every line in the script: what kind of line is it, and which statement does
# it belong to.
#
# It keys off token TYPE, never raw text. That is a correctness requirement,
# not a stylistic one: grepping app-unbuilt/script.qvs for the bare word "for"
# finds 1242 hits, of which roughly 28 are the control-flow keyword - the rest
# sit inside comments and string literals. "sub" scores 75 hits with zero real
# SUB blocks. Typed tokens make comments and literals structurally invisible,
# which is the entire reason this pipeline is a token stream (DESIGN 2.1).
#
# Note how little this scanner has to know. Indentation is FLAT (DESIGN 4.5) -
# a line's indent depends only on what kind of line it is, never on how many
# blocks enclose it - so block tracking exists for exactly one purpose: the
# blank-line rule in DESIGN 4.8 treats a whole FOR/SUB/IF/DO/SWITCH block as a
# single statement, so statements inside one must not be blown apart by two
# blank lines each. That reduces all five block types to one depth counter
# driven by the word list below, with no per-type logic anywhere.

# Openers are recognised ONLY as the first word of a statement. That single
# guard is what stops "Exit for when rowNr >= numRows;" (7 in the stress
# fixture) opening a phantom block, without special-casing exit/exit sub/exit
# do individually.
.qlik_block_opener <- function(w1, has_then) {
  if (w1 %in% c("for", "sub", "do", "switch")) return(w1)
  # DESIGN 1.6: statement IF vs function IF( - the fixture has ~117 of the
  # latter and 15 of the former. Testing for a following LPAREN does NOT
  # separate them, because "IF (x = 1) THEN" is legal; a depth-0 THEN does.
  if (w1 == "if" && has_then) return("if")
  NA_character_
}

.qlik_block_closer <- function(w1, w2) {
  if (w1 == "next") return("for")
  if (w1 == "loop") return("do")
  if (w1 == "end") {
    if (w2 == "sub")    return("sub")
    if (w2 == "if")     return("if")
    if (w2 == "switch") return("switch")
  }
  if (w1 == "endif")     return("if")
  if (w1 == "endsub")    return("sub")
  if (w1 == "endswitch") return("switch")
  NA_character_
}

#' Classify every line of a token stream for the vertical layout pass.
#'
#' A "line" here is a token that starts one: any non-whitespace token whose
#' preceding whitespace carried a newline. Tokens that span lines internally
#' (a BRACKET holding an INLINE table, a block comment) are opaque and are
#' NOT decomposed - they are one line start, and the layout pass must never
#' reach inside them.
#'
#' @param tokens a token stream data.frame (see tokenize_qlik).
#' @return a list with:
#'   $lines - data.frame, one row per line start:
#'       idx         - token index of the line's first token
#'       line        - source line number
#'       kind        - "statement"    1 tab  (table label, LOAD, FROM, FOR...)
#'                     "field"        2 tabs (first line of a LOAD field,
#'                                    plain or comma-led)
#'                     "continuation" 3 tabs (line break before that field's
#'                                    expression is finished - DESIGN 4.5)
#'                     "comment"      column 0
#'                     "directive"    0 tabs (a SET/LET statement, and any of
#'                                    its own continuation lines - DESIGN
#'                                    4.11; blank lines around it are left
#'                                    untouched, same protection as "section")
#'                     "section"      the WHOLE line carrying a ///$tab
#'                                    marker (not just the marker itself) -
#'                                    left ENTIRELY alone
#'       starts_stmt - TRUE if a statement begins here, at any block depth
#'       stmt_id     - the TOP-LEVEL statement this line belongs to; a whole
#'                     block shares one id (DESIGN 4.8, "block = one
#'                     statement"), so two blank lines go where this changes
#'       depth       - block nesting depth; 0 at top level
#'       first_field - TRUE only for the LOAD list's true first field (the
#'                     one sitting right after the LOAD keyword) - the one
#'                     that needs the two-space alignment pad
#'   $warnings - character vector for unbalanced blocks
find_block_structure <- function(tokens) {
  n <- nrow(tokens)
  empty <- data.frame(
    idx = integer(0), line = integer(0), kind = character(0),
    starts_stmt = logical(0), stmt_id = integer(0), depth = integer(0),
    stringsAsFactors = FALSE)
  if (n == 0) return(list(lines = empty, warnings = character(0)))

  ty    <- tokens$type
  lower <- tolower(tokens$text)
  warn  <- character(0)

  # --- line starts -------------------------------------------------------
  # A content token starts a line if a newline fell between it and the
  # previous content token. VOID rows carry no text, so they must be
  # transparent here or a voided token would break the chain and hide a
  # genuine line start.
  has_nl  <- ty == "WS" & grepl("\n", tokens$text, fixed = TRUE)
  content <- which(!(ty %in% c("WS", "VOID")))
  if (length(content) == 0) return(list(lines = empty, warnings = warn))

  cum_nl <- cumsum(has_nl)
  nc     <- length(content)
  starts <- rep(TRUE, nc)
  if (nc > 1) starts[-1] <- cum_nl[content[-1]] > cum_nl[content[-nc]]
  ls_pos <- which(starts)        # positions within `content`
  ls_idx <- content[ls_pos]      # token indices
  nls    <- length(ls_idx)

  # Any line carrying a ///$tab marker is left ENTIRELY alone, including
  # whatever shares the line with it. app-unbuilt/script.qvs opens with the
  # bytes "I///$tab 00-Main": Adam checked the source app (2026-08-17) and
  # the leading "I" is present after unpacking either by design or benignly,
  # so it is data to preserve, not a wart to repair. 63 markers exist and
  # only 62 start their line - without this the odd one out would classify
  # as an ordinary statement and get indented, moving both the "I" and the
  # marker Qlik keys its editor section tabs off.
  sect_tok   <- which(ty == "COMMENT" & startsWith(tokens$text, "///$"))
  is_section <- logical(nls)
  if (length(sect_tok) > 0) {
    owner <- findInterval(sect_tok, ls_idx)
    is_section[owner[owner >= 1L]] <- TRUE
  }

  # --- field vs continuation --------------------------------------------
  # Reuse find_load_segments() rather than re-deriving field boundaries. The
  # FIRST line start in a segment is the field itself; any later one is a
  # line break before that field's expression finished, i.e. a continuation.
  seg  <- find_load_segments(tokens)
  warn <- c(warn, seg$warnings)
  segs <- seg$segments
  seg_lo <- integer(0); seg_hi <- integer(0)
  if (length(segs) > 0) {
    seg_lo <- vapply(segs, function(s) s$content_idx[1], integer(1))
    seg_hi <- vapply(segs, function(s) s$end, integer(1))
  }

  # --- depth-0 statement terminators -------------------------------------
  d      <- cumsum((ty == "LPAREN") - (ty == "RPAREN"))
  semi0  <- ty == "SEMI" & d == 0L
  cum_semi <- cumsum(semi0)
  # Semicolons strictly BEFORE each position, not at-or-before it. A ';' that
  # sits alone on its own source line (real case, [Grant Managing Region].
  # txt:11) is itself a line start - using the inclusive cum_semi there would
  # count its own semicolon as already having happened, making the SEMICOLON's
  # OWN line look like it starts a new statement instead of the line after it.
  pre_semi <- c(0L, cum_semi[-length(cum_semi)])

  kind        <- character(nls)
  starts_stmt <- logical(nls)
  stmt_id     <- integer(nls)
  depth_out   <- integer(nls)
  first_field <- logical(nls)

  # Index of the next/previous non-trivia token at each position - used
  # below to classify a leading-comma field line by what it actually
  # separates rather than by the comma token itself, and to recognise a
  # LOAD list's true first field (whatever token sits right after the LOAD
  # keyword itself, comma-led or not - see the field/continuation block).
  nxt_nt  <- next_non_trivia_idx(ty)
  prev_nt <- prev_non_trivia_idx(ty)

  stack         <- character(0)
  cur           <- 0L
  pending       <- TRUE   # next line start begins a statement
  cur_directive <- FALSE  # is the CURRENT top-level statement a SET/LET?

  for (j in seq_len(nls)) {
    i <- ls_idx[j]

    # a depth-0 ';' since the previous line start ended that statement
    if (j > 1L && pre_semi[i] > pre_semi[ls_idx[j - 1L]]) pending <- TRUE

    if (is_section[j]) {
      kind[j]      <- "section"
      stmt_id[j]   <- cur
      depth_out[j] <- length(stack)
      next          # protected data, whatever precedes the marker included
    }

    if (ty[i] == "COMMENT") {
      kind[j]      <- "comment"
      stmt_id[j]   <- cur
      depth_out[j] <- length(stack)
      next          # a comment neither opens a statement nor closes one
    }

    w1 <- if (ty[i] == "WORD") lower[i] else ""
    nx <- if (ls_pos[j] < nc) content[ls_pos[j] + 1L] else NA_integer_
    w2 <- if (!is.na(nx) && ty[nx] == "WORD") lower[nx] else ""

    # SET/LET statements (DESIGN §4.11, Adam 2026-08-17): 0 indent, blank
    # lines around them left untouched - a "directive" kind distinct from
    # "statement". Only re-evaluated when a new top-level statement is
    # actually starting here, so a directive's own continuation lines (a
    # multi-line SET/LET expression) inherit it rather than falling back to
    # "statement" partway through.
    if (pending) cur_directive <- nzchar(w1) && w1 %in% c("set", "let")

    opener <- NA_character_
    closer <- NA_character_
    if (pending && nzchar(w1)) {
      has_then <- FALSE
      if (w1 == "if") {
        stop_at  <- if (j < nls) ls_idx[j + 1L] - 1L else n
        rng      <- i:stop_at
        has_then <- any(ty[rng] == "WORD" & lower[rng] == "then" & d[rng] == d[i])
      }
      opener <- .qlik_block_opener(w1, has_then)
      closer <- .qlik_block_closer(w1, w2)
    }

    # a closer leaves its block before being placed, so "NEXT" lines up with
    # the "FOR" that opened it rather than with the body between them
    if (!is.na(closer)) {
      if (length(stack) == 0L) {
        warn <- c(warn, sprintf(
          "Line %d: '%s' closes a %s block that was never opened - layout left flat here.",
          tokens$line[i], tokens$text[i], closer))
      } else {
        if (tail(stack, 1L) != closer) {
          warn <- c(warn, sprintf(
            "Line %d: '%s' closes a %s block but the innermost open block is %s.",
            tokens$line[i], tokens$text[i], closer, tail(stack, 1L)))
        }
        stack <- stack[-length(stack)]
      }
    }

    # Only a top-level statement gets a new id. Inside a block every
    # statement inherits the block's id, which is what makes DESIGN 4.8's
    # "no blank lines inside a statement" cover a whole loop body.
    #
    # A closing line is excluded even though the pop above has already put it
    # back at depth 0: NEXT / END IF belong to the block they close, not to
    # whatever follows. Without this they would take a fresh id and DESIGN
    # 4.8 would then insert two blank lines immediately before the NEXT.
    if (pending && length(stack) == 0L && is.na(closer)) cur <- cur + 1L
    starts_stmt[j] <- pending
    stmt_id[j]     <- cur
    depth_out[j]   <- length(stack)

    # A field line's own first token is normally its content - but after
    # enforce_leading_commas (pass 3) has run, every field but the first
    # starts with the separator COMMA itself, not its content. Classifying
    # on the comma's own index put it in the gap BETWEEN two segments
    # (neither's content_idx), which fell through to "statement" (1 tab
    # instead of 2) - a real bug, not a hypothetical. Anchor on what the
    # comma actually separates instead: the next non-trivia token. A
    # depth>0 comma (mid-expression, never moved by pass 3) still lands
    # inside its own segment's content, so it still resolves to
    # "continuation" as before - this only changes the depth-0 case.
    is_comma_lead <- ty[i] == "COMMA"
    anchor <- if (is_comma_lead && !is.na(nxt_nt[i])) nxt_nt[i] else i

    k <- findInterval(anchor, seg_lo)
    kind[j] <- if (k >= 1L && anchor <= seg_hi[k]) {
      if (anchor == seg_lo[k]) "field" else "continuation"
    } else {
      "statement"
    }
    # The true first field of a LOAD list gets the two-space alignment pad
    # (DESIGN §4.4/§4.5) so its content lines up with the ", " of every
    # comma-led field below it - identified by what sits right before the
    # segment's own content, not by whether THIS line happens to lead with
    # a comma: pass 3 always runs before layout in the real pipeline, so in
    # practice only the true first field is ever comma-less, but this stays
    # correct even tested standalone (verify.R's layout self-tests run it
    # directly on trailing-comma syntax).
    if (kind[j] == "field") {
      prv <- prev_nt[seg_lo[k]]
      first_field[j] <- !is.na(prv) && ty[prv] == "WORD" && lower[prv] == "load"
    }

    if (cur_directive) kind[j] <- "directive"

    if (!is.na(opener)) stack <- c(stack, opener)

    # control-flow statements are newline-terminated, not ';'-terminated, so
    # the next line start begins a new statement
    pending <- !is.na(opener) || !is.na(closer)
  }

  for (b in rev(stack)) {
    warn <- c(warn, sprintf(
      "Reached end of file with a %s block still open - layout left flat inside it.", b))
  }

  list(
    lines = data.frame(
      idx = ls_idx, line = tokens$line[ls_idx], kind = kind,
      starts_stmt = starts_stmt, stmt_id = stmt_id, depth = depth_out,
      first_field = first_field,
      stringsAsFactors = FALSE),
    warnings = warn)
}
