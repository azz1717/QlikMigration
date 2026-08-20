# enforce_bracket_references.R
#
# Style-guide pass: ensures every field name / alias reference is enclosed
# in square brackets rather than quotation marks or nothing at all.
# DESIGN §4.2: "All field and alias references in square brackets."
#
#   - Double-quoted tokens ("...") are ALWAYS field references in Qlik
#     script (there is no double-quoted string literal in Qlik), so every
#     one is converted to brackets, wherever it appears.
#   - Single-quoted tokens ('...') are ordinarily string literals and are
#     left untouched, e.g. the 'FNQLD','GNQLD' list inside a MATCH() call,
#     or 'Overdue' on the left of AS in:
#         'Overdue' AS [Bill Status]
#     EXCEPT: a single-quoted token used as the alias itself - i.e.
#     immediately right of a field's AS - acts as a field/alias reference,
#     not a literal (confirmed by testing; not documented by Qlik), e.g.
#         [streetname] AS 'address'
#     becomes
#         [streetname] AS [address]
#   - Bare (unquoted) WORD tokens inside a LOAD field list also get
#     bracketed, e.g. real cases at app-unbuilt/script.qvs:
#         Fleet.Rego as [Fleet Car Rego]   ->  [Fleet.Rego] AS [Fleet Car Rego]
#         Electorate,                      ->  [Electorate] AS [Electorate]
#     EXCEPT a reserved keyword (AS, AND, DISTINCT... - never a valid field
#     name) or a word in CALL position (next non-trivia token is "(" - a
#     function or SUB invocation, not a reference). That second guard is
#     the one enforce_reserved_word_case.R already needed for the same
#     reason: app-unbuilt/script.qvs:837 has a bare field named `Year`,
#     textually identical to the Year() function - DESIGN §1.3/§1.7.
#     SCOPE, DELIBERATELY NARROW: only inside a LOAD field list
#     (find_load_segments()'s content_idx), never the whole token stream.
#     A bare word elsewhere is very often NOT a field - a FOR loop counter
#     or a LET-assigned variable (numRows, chunkSize, i, chunkText... in
#     app-unbuilt/script.qvs's chunking loop) - and bracketing one would
#     silently break the script by turning a variable reference into a
#     field reference. WHERE-clause and other non-field-list bare words are
#     therefore left untouched; deliberately out of scope (DESIGN §4.11).
#   - SELECT ... ; blocks (raw SQL passed to a LIB CONNECT TO source) are
#     left untouched entirely, same as the other passes.
#   - A token whose content contains "]" cannot be safely represented in
#     brackets (Qlik brackets have no escape mechanism) and is left as
#     quoted, reported back as a warning. (Bare WORDs can never contain "]"
#     - not in the tokenizer's word character class - so this only applies
#     to the quoted branch.)
#
# Operates on a token stream (see qlik_tokenizer.R - source that first).
# Vanilla base R only.

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @return a list with:
#'   $tokens   - the token stream with quoted references converted to brackets
#'   $warnings - character vector describing any reference left as quoted
#'               because its content contains "]"
#'   $changes  - data.frame(line, kind, before, after), one row per
#'               conversion actually made - an exact record of what
#'               changed, for sanity-checking instead of eyeballing a
#'               generic text diff. "kind" is "double-quoted field",
#'               "single-quoted alias" or "bare reference".
enforce_bracket_references <- function(tokens) {
  found <- find_load_segments(tokens)
  warn <- found$warnings

  n <- nrow(tokens)

  # Work on plain vectors and write the columns back once at the end. The
  # loop below both reads and rewrites tokens, and doing that through
  # tokens$col[i] pays data.frame dispatch on every one of n iterations.
  t_text <- tokens$text
  t_type <- tokens$type
  t_line <- tokens$line

  # Single-quoted tokens serving as an alias target (right of AS) are the
  # one confirmed case where a single quote is a reference, not a literal.
  # Recorded as a lookup vector so the test below is O(1) rather than a
  # scan of the whole index set per token.
  is_alias_sq <- logical(n)
  for (seg in found$segments) {
    ai <- seg$alias_content_idx
    if (length(ai) == 1 && t_type[ai] == "SQUOTE") is_alias_sq[ai] <- TRUE
  }

  # change log as plain vectors, assembled into a data.frame once at the end
  ch_line <- integer(n);     ch_kind  <- character(n)
  ch_before <- character(n); ch_after <- character(n)
  nch <- 0L

  in_select <- FALSE

  for (i in seq_len(n)) {
    ty <- t_type[i]

    if (ty == "WORD" && tolower(t_text[i]) == "select") { in_select <- TRUE; next }
    if (in_select) { if (ty == "SEMI") in_select <- FALSE; next }

    convert <- (ty == "DQUOTE") || (ty == "SQUOTE" && is_alias_sq[i])
    if (!convert) next

    # `ty` is DQUOTE or SQUOTE by the guard above, which is exactly the
    # mapping undelimit() (qlik_tokenizer.R) applies internally.
    inner <- undelimit(t_text[i], ty)

    if (grepl("]", inner, fixed = TRUE)) {
      warn <- c(warn, sprintf(
        "Line %d: cannot convert %s to brackets (contains ']') - left as quoted.",
        t_line[i], t_text[i]))
      next
    }

    before <- t_text[i]
    after <- paste0("[", inner, "]")

    nch <- nch + 1L
    ch_line[nch]   <- t_line[i]
    ch_kind[nch]   <- if (ty == "DQUOTE") "double-quoted field" else "single-quoted alias"
    ch_before[nch] <- before
    ch_after[nch]  <- after

    t_text[i] <- after
    t_type[i] <- "BRACKET"
  }

  # Bare (unquoted) field/alias references, LOAD field-list content only -
  # see the module header for why the scope stops there. content_idx spans
  # the whole segment (both sides of AS), so this reaches an un-aliased
  # bare field, a bare alias target, or both, without walking the segment
  # twice.
  if (length(found$segments) > 0) {
    in_field <- logical(n)
    for (seg in found$segments) in_field[seg$content_idx] <- TRUE

    nxt      <- next_non_trivia_idx(t_type)
    call_pos <- !is.na(nxt) & t_type[nxt] == "LPAREN"

    bare <- which(in_field & t_type == "WORD" & !call_pos &
                    !(tolower(t_text) %in% QLIK_KEYWORDS))

    for (i in bare) {
      before <- t_text[i]
      after  <- paste0("[", before, "]")

      nch <- nch + 1L
      ch_line[nch]   <- t_line[i]
      ch_kind[nch]   <- "bare reference"
      ch_before[nch] <- before
      ch_after[nch]  <- after

      t_text[i] <- after
      t_type[i] <- "BRACKET"
    }
  }

  tokens$text <- t_text
  tokens$type <- t_type

  changes_df <- data.frame(
    line   = ch_line[seq_len(nch)],
    kind   = ch_kind[seq_len(nch)],
    before = ch_before[seq_len(nch)],
    after  = ch_after[seq_len(nch)],
    stringsAsFactors = FALSE
  )

  list(tokens = tokens, warnings = warn, changes = changes_df)
}
