# enforce_reserved_word_case.R
#
# Style-guide pass: uppercases Qlik reserved words - both statement keywords
# and built-in functions.
#
#   - Only WORD tokens are ever considered. Anything quoted, bracketed or
#     commented is a single opaque token to the tokenizer, so a field named
#     [year(2012)] or 'year(2012)' or "year(2012)" is structurally invisible
#     here and cannot be touched. (Those are all valid Qlik field references -
#     confirmed by testing.) This is the difference between this pass and a
#     find-and-replace in the Qlik editor.
#   - Statement keywords (QLIK_KEYWORDS) are uppercased on the word alone.
#   - Built-in functions (QLIK_FUNCTIONS) are uppercased ONLY in call
#     position, i.e. when the next non-trivia token is "(". Qlik field names
#     ARE case sensitive, and plenty of function names double as plausible
#     field names - app-unbuilt/script.qvs line 837 has a bare
#         Year as [Data x Reg Year]
#     where Year is a field, not a call. Without the call-position guard this
#     pass would silently rename that field.
#   - A word may be in both lists (Left, Right, Replace, Keep, Join, First...
#     are prefixes as well as functions). Either route uppercases it, so the
#     only thing precedence decides is how the change is labelled: call
#     position is checked first, so Replace(...) is reported as a function
#     and a Replace LOAD prefix as a keyword.
#   - SELECT ... ; blocks are skipped entirely, same as the other passes.
#     This is what protects the camelCase parameters of connector syntax
#     (tableName=, geometryType=, loadDistinct=, crs= ...) that appear inside
#     GeoAnalytics SQL SELECT statements.
#   - User-defined SUB names are not in either list, so a CALL to your own
#     routine keeps its casing.
#   - Uppercasing a keyword-spelled token that sits inside a LOAD field list
#     is reported as a warning unless it is one of the words that genuinely
#     belongs there (AS, AND, OR, NOT, XOR, LIKE, DISTINCT, IF/THEN/ELSE/END).
#     Measured across both fixtures, nothing else ever occurred there - so a
#     warning means something unusual is going on and is worth a look.
#
# Operates on a token stream (see qlik_tokenizer.R - source that first, and
# qlik_reserved_words.R for the vocabulary). Vanilla base R only.

# Keywords that legitimately appear inside a LOAD field list. Anything else
# from QLIK_KEYWORDS showing up there is suspicious enough to report.
.field_ok_keywords <- c("as", "and", "or", "not", "xor", "like", "distinct",
                        "if", "then", "else", "end")

#' @param tokens a token stream data.frame (see tokenize_qlik / read_qlik_script).
#' @return a list with:
#'   $tokens   - the token stream with reserved words uppercased
#'   $warnings - character vector describing keyword-spelled tokens uppercased
#'               in an unexpected position inside a LOAD field list
#'   $changes  - data.frame(line, kind, before, after), one row per token
#'               actually recased. "kind" is "keyword" or "function".
enforce_reserved_word_case <- function(tokens) {
  n <- nrow(tokens)
  empty <- data.frame(line = integer(0), kind = character(0),
                      before = character(0), after = character(0),
                      stringsAsFactors = FALSE)
  if (n == 0) return(list(tokens = tokens, warnings = character(0),
                          changes = empty))

  t_text <- tokens$text
  t_type <- tokens$type
  t_line <- tokens$line
  lower  <- tolower(t_text)

  is_word   <- t_type == "WORD"
  in_select <- in_select_region(t_type, lower)
  nxt       <- next_non_trivia_idx(t_type)
  call_pos  <- !is.na(nxt) & t_type[nxt] == "LPAREN"

  # A word in both lists (Left, Right, Replace, Keep, Join, First...) is a
  # prefix AND a function. Both end up uppercased either way, so precedence
  # only decides how the change is LABELLED - and "Replace(" is a function
  # call, not a prefix. Classify by call position first so the change log and
  # the warning below both describe what the token actually is.
  eligible <- is_word & !in_select
  fn_hit <- eligible & call_pos & lower %in% QLIK_FUNCTIONS
  kw_hit <- eligible & lower %in% QLIK_KEYWORDS & !fn_hit

  upper   <- toupper(t_text)
  changed <- (kw_hit | fn_hit) & t_text != upper

  changes <- data.frame(
    line   = t_line[changed],
    kind   = ifelse(kw_hit[changed], "keyword", "function"),
    before = t_text[changed],
    after  = upper[changed],
    stringsAsFactors = FALSE
  )

  # keyword-spelled tokens recased inside a LOAD field list, other than the
  # ones that genuinely live there
  warn <- character(0)
  found <- find_load_segments(tokens)
  warn <- c(warn, found$warnings)
  if (length(found$segments) > 0) {
    in_field <- logical(n)
    for (seg in found$segments) in_field[seg$content_idx] <- TRUE
    # Only bare occurrences are suspicious. A keyword-spelled token in call
    # position is unambiguously a call, not a field reference.
    odd <- which(changed & kw_hit & in_field & !call_pos &
                   !(lower %in% .field_ok_keywords))
    for (i in odd) {
      warn <- c(warn, sprintf(
        "Line %d: '%s' looks like a field reference but is a reserved word - uppercased to '%s'; check this is not a field name.",
        t_line[i], t_text[i], upper[i]))
    }
  }

  tokens$text <- ifelse(changed, upper, t_text)

  list(tokens = tokens, warnings = warn, changes = changes)
}
