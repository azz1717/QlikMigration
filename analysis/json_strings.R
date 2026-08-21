# json_strings.R — pull every string literal out of a JSON file.
#
# Phase 2, step 1 (DESIGN §6.5). NOT part of the styling pipeline; nothing in
# run_pipeline.R sources this.
#
# This is deliberately NOT a JSON parser. Phase 2 never consults the document
# structure — it treats every string in objects/*.json as possible expression
# text and scans it for field references (DESIGN §6.5: enumerating keys is
# what made the first attempt miss half the usage). Needing only the strings
# turns a parser plus state machine into one regex, which is what makes the
# base-R-only constraint cheap to meet.
#
# Matching runs in BYTE mode for the same reason tokenize_qlik() does — see
# the note above .qlik_token_pattern in qlik_tokenizer.R and DESIGN §3.1. The
# whole file is one string, so a single non-ASCII character anywhere makes
# character-offset conversion quadratic over every match.
#
# The one structural fact used: in JSON a string followed by `:` is a KEY,
# and a value never is (a value is followed by `,`, `}` or `]`). That is
# captured in the pattern itself rather than by tracking nesting.

# A JSON string is a quote, then any run of (non-quote non-backslash | any
# escaped character), then a quote. Because a complete string is consumed at
# a time, an escaped quote inside one cannot end it, and the text BETWEEN
# strings can never contain a quote — so a single left-to-right scan is
# enough. The optional trailing `[ \t\r\n]*:` is the key/value tell.
.json_string_pattern <- '"(?:[^"\\\\]|\\\\.)*"[ \\t\\r\\n]*:?'

# One JSON escape -> the character it denotes. \uXXXX is resolved by the
# caller, which needs lookahead for surrogate pairs.
.json_escape_char <- function(ch) {
  switch(ch,
         "n" = "\n", "t" = "\t", "r" = "\r", "b" = "\b", "f" = "\f",
         "\"" = "\"", "\\" = "\\", "/" = "/",
         ch)
}

# Resolve escapes in ONE string. Escapes are located in a single scan, so a
# literal backslash (`\\`) is consumed whole and cannot be misread as the
# start of the escape that follows it — the classic failure of doing this
# with a sequence of gsub() calls.
.json_unescape_one <- function(s) {
  m <- gregexpr("\\\\(u[0-9A-Fa-f]{4}|.)", s, perl = TRUE)[[1]]
  if (m[1L] == -1L) return(s)
  starts <- as.integer(m)
  lens   <- attr(m, "match.length")

  pieces <- character(0)
  pos <- 1L
  i   <- 1L
  while (i <= length(starts)) {
    if (starts[i] > pos)
      pieces <- c(pieces, substr(s, pos, starts[i] - 1L))
    esc <- substr(s, starts[i], starts[i] + lens[i] - 1L)
    ch  <- substr(esc, 2L, 2L)

    if (ch == "u") {
      cp <- strtoi(substr(esc, 3L, 6L), 16L)
      # A character outside the BMP is written as a surrogate PAIR: a high
      # unit D800-DBFF immediately followed by a low unit DC00-DFFF. Decoding
      # each half alone yields an invalid character, so pair them here.
      nxt <- i + 1L
      if (cp >= 0xD800L && cp <= 0xDBFFL && nxt <= length(starts) &&
          starts[nxt] == starts[i] + lens[i] &&
          substr(s, starts[nxt] + 1L, starts[nxt] + 1L) == "u") {
        lo <- strtoi(substr(s, starts[nxt] + 2L, starts[nxt] + 5L), 16L)
        if (lo >= 0xDC00L && lo <= 0xDFFFL) {
          pieces <- c(pieces,
                      intToUtf8(0x10000L + (cp - 0xD800L) * 1024L + (lo - 0xDC00L)))
          pos <- starts[nxt] + lens[nxt]
          i   <- i + 2L
          next
        }
      }
      pieces <- c(pieces, intToUtf8(cp))
    } else {
      pieces <- c(pieces, .json_escape_char(ch))
    }

    pos <- starts[i] + lens[i]
    i   <- i + 1L
  }
  if (pos <= nchar(s)) pieces <- c(pieces, substr(s, pos, nchar(s)))
  paste0(pieces, collapse = "")
}

# Vectorised, with a fast path: most strings contain no backslash at all.
.json_unescape <- function(x) {
  esc <- grepl("\\", x, fixed = TRUE)
  if (any(esc))
    x[esc] <- vapply(x[esc], .json_unescape_one, character(1), USE.NAMES = FALSE)
  x
}

#' Every string literal in a JSON document, in document order.
#'
#' @param text the whole document as ONE character string.
#' @return data.frame(text, is_key) — `text` is unescaped and has its
#'   surrounding quotes removed; `is_key` marks the strings that name a
#'   member rather than carry a value.
json_string_literals <- function(text) {
  m <- gregexpr(.json_string_pattern, text, perl = TRUE, useBytes = TRUE)
  raw <- regmatches(text, m)[[1]]
  Encoding(raw) <- "UTF-8"   # byte mode returns unmarked bytes; see header

  if (length(raw) == 0L)
    return(data.frame(text = character(0), is_key = logical(0),
                      stringsAsFactors = FALSE))

  is_key <- grepl(":$", raw)
  # The pattern consumes whitespace after the closing quote for VALUES too,
  # not only for keys, so the tell and that whitespace come off together.
  # Stripping only `ws*:` left every value in a pretty-printed file carrying
  # its own closing quote (caught 2026-08-17 — compact test JSON had hidden
  # it, since there the match ends at the quote).
  # `[ \t\r\n]` cannot eat into the content: the closing quote stops it.
  lit    <- sub("[ \t\r\n]*:?$", "", raw)
  body   <- substr(lit, 2L, nchar(lit) - 1L)      # drop both quotes

  data.frame(text = .json_unescape(body), is_key = is_key,
             stringsAsFactors = FALSE)
}

#' Read a JSON file and return its string literals.
#'
#' @param path file to read.
#' @return as json_string_literals(). Line numbers are deliberately not
#'   tracked — phase 2 reports usage per FILE, not per line.
read_json_strings <- function(path) {
  text <- paste(readLines(path, encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  json_string_literals(text)
}
