# json_read.R - a minimal JSON reader, shared. The mirror of json_write.R.
#
# Base R has no JSON parser and `jsonlite` is a development-machine oracle
# that must not reach shipped tooling (INTERFACES.md), so every qlik-cli
# `--json` reply and every unbuilt bundle file is read through here.
#
# SUBSTRATE: one PCRE pass produces a TOKEN VECTOR, then a recursive descent
# walks it by index. The alternative - splitting the text into a character
# vector and walking it one character at a time - is what
# diagnostics/qlik_cli_unbuild.R's private .json_parse does, and it grows a
# string with c(out, ch) per character. That is quadratic on the big bundle
# objects (app-unbuilt/objects/ has a 756 KB sheet), which is the whole
# reason this file does not simply promote that one. CLAUDE.md sanctions a
# per-corpus substrate choice for phase-2/3 tooling (DESIGN 6.7); the token
# stream invariant is the STYLING pipeline's, not this file's.
# .json_parse in qlik_cli_unbuild.R stays where it is until PLAN-fleet.md's
# M2 lifts that script's fetch helper into fleet/qlik_api.R; the two are not
# textual twins (different algorithm), and the fold-in has a scheduled home.
#
# MAPPING: object -> named list, array -> unnamed list, string -> character,
# number -> numeric, true/false -> logical, null -> NULL.
# GOTCHA that dictates the two odd-looking assignments below: `l[[k]] <- NULL`
# DELETES the element in R. A JSON null must stay a present key holding NULL,
# so members are written with `l[k] <- list(v)` (single bracket, value wrapped
# in a list) - the only form that stores a NULL instead of removing the slot.
# Read a possibly-absent member with json_get(), never `x$k` alone, since a
# present-but-null key and an absent key both answer NULL to `$`.
#
# ASCII source only (DESIGN 8.5): a non-ASCII byte in R source risks a parse
# error under an unknown console locale on the VM.

# One alternation, longest-first, in the same spirit as the tokenizer's
# .qlik_token_pattern. Strings first so that braces and commas inside a
# string can never be seen as structure; numbers are strict JSON (no
# leading +, no leading zero), which is what makes a malformed number show
# up as an unmatched gap below instead of parsing to something plausible.
.JR_PATTERN <- paste0(
  '"(?:\\\\.|[^"\\\\])*"',                             # string
  "|-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?",  # number
  "|true|false|null",                                  # literals
  "|[][{}:,]")                                         # structure

# The gap check is the validator. gregexpr only reports what it matched, so
# without it any garbage BETWEEN two tokens would be silently skipped and a
# corrupt file would parse "successfully" - the one failure mode a hand-
# written parser must not have. Positions are BYTE offsets (below).
#
# PERF, measured on app-unbuilt/objects/'s 756 KB sheet (74,156 tokens):
# everything here runs in BYTE space because character space is quadratic on
# a UTF-8 string. R has to walk the whole string from the start to turn a
# byte offset into a character index, once per match, so `gregexpr(...,
# perl = TRUE)` alone took 34s on that file and the follow-up
# `substring(txt, starts, ends)` another 34s - 88s for the 168-file corpus.
# With useBytes = TRUE and regmatches() doing the extraction the same file
# tokenizes in 0.01s + 0.09s. Not a micro-optimisation: it is the difference
# between a usable tool and one nobody runs twice.
# GOTCHA: a byte-mode match never splits a multibyte character, because every
# byte of one is >= 0x80 and so is consumed by the string body class - the
# token boundaries still land on character boundaries, which is what makes
# re-marking each token UTF-8 correct rather than hopeful.
.jr_tokens <- function(txt) {
  m <- gregexpr(.JR_PATTERN, txt, perl = TRUE, useBytes = TRUE)[[1]]
  if (m[1] == -1L) {
    starts <- integer(0); lens <- integer(0); tok <- character(0)
  } else {
    starts <- as.integer(m)
    lens <- attr(m, "match.length")
    tok <- regmatches(txt, list(m))[[1]]
    Encoding(tok) <- "UTF-8"
  }
  nb <- nchar(txt, type = "bytes")
  covered <- logical(nb)
  if (length(starts)) covered[sequence(lens, from = starts)] <- TRUE
  loose <- which(!covered)
  if (length(loose)) {
    rb <- charToRaw(txt)[loose]
    junk <- which(!(rb %in% as.raw(c(9L, 10L, 13L, 32L))))
    if (length(junk)) {
      at <- loose[junk[1]]
      stop(sprintf("json: unexpected character at byte %d: %s", at,
                   rawToChar(charToRaw(txt)[at])), call. = FALSE)
    }
  }
  list(text = tok, start = starts)
}

.jr_err <- function(st, msg) {
  at <- if (st$i <= st$n) st$pos[st$i] else nchar(st$src, type = "bytes")
  stop(sprintf("json: %s at byte %d", msg, at), call. = FALSE)
}

.jr_unescape <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  out <- character(n)
  k <- 0L
  i <- 1L
  while (i <= n) {
    if (ch[i] != "\\") { k <- k + 1L; out[k] <- ch[i]; i <- i + 1L; next }
    e <- ch[i + 1L]
    if (is.na(e)) stop("json: dangling backslash in string", call. = FALSE)
    if (e == "u") {
      if (i + 5L > n) stop("json: truncated \\u escape", call. = FALSE)
      cp <- strtoi(paste(ch[(i + 2L):(i + 5L)], collapse = ""), 16L)
      if (is.na(cp)) stop("json: bad \\u escape", call. = FALSE)
      i <- i + 6L
      # A character above the BMP arrives as a surrogate PAIR. Decoding the
      # halves separately yields two lone surrogates, which intToUtf8()
      # renders as NA - visible corruption, but only once the value is used.
      if (cp >= 0xD800L && cp <= 0xDBFFL && i + 5L <= n &&
          ch[i] == "\\" && ch[i + 1L] == "u") {
        lo <- strtoi(paste(ch[(i + 2L):(i + 5L)], collapse = ""), 16L)
        if (!is.na(lo) && lo >= 0xDC00L && lo <= 0xDFFFL) {
          cp <- 0x10000L + (cp - 0xD800L) * 1024L + (lo - 0xDC00L)
          i <- i + 6L
        }
      }
      k <- k + 1L
      # R strings cannot hold a NUL, so an escaped one becomes "" rather
      # than an error: dropping it beats refusing a whole tenant reply.
      out[k] <- if (cp == 0L) "" else intToUtf8(cp)
    } else {
      k <- k + 1L
      out[k] <- switch(e,
                       '"' = '"', "\\" = "\\", "/" = "/", "b" = "\b",
                       "f" = "\f", "n" = "\n", "r" = "\r", "t" = "\t",
                       stop(sprintf("json: bad escape \\%s", e), call. = FALSE))
      i <- i + 2L
    }
  }
  paste(out[seq_len(k)], collapse = "")
}

# Fast path: a string with no backslash at all (the overwhelming majority)
# needs nothing but its quotes removed.
.jr_string <- function(t) {
  s <- substr(t, 2L, nchar(t) - 1L)
  if (!grepl("\\", s, fixed = TRUE)) s else .jr_unescape(s)
}

# Both containers fill a DOUBLING buffer and are named/trimmed once at the
# end, rather than appending with `out[k] <- list(v)` (which copies the list
# and rehashes its names per member). Honest note on where that came from:
# this WAS the suspected cost of the 88s corpus run and it made no
# measurable difference - the real cost was the character-space tokenizer
# above. Kept because it is the right shape, not because it was the fix.
.jr_object <- function(st) {
  st$i <- st$i + 1L
  if (identical(st$tok[st$i], "}")) { st$i <- st$i + 1L; return(structure(list(), names = character(0))) }
  cap <- 8L
  buf <- vector("list", cap)
  key <- character(cap)
  m <- 0L
  repeat {
    if (st$i > st$n) .jr_err(st, "unterminated object")
    k <- st$tok[st$i]
    if (substr(k, 1L, 1L) != '"') .jr_err(st, "expected a key string")
    kk <- .jr_string(k)
    st$i <- st$i + 1L
    if (!identical(st$tok[st$i], ":")) .jr_err(st, "expected ':'")
    st$i <- st$i + 1L
    v <- .jr_value(st)
    m <- m + 1L
    if (m > cap) { cap <- cap * 2L; length(buf) <- cap; length(key) <- cap }
    buf[m] <- list(v)
    key[m] <- kk
    nxt <- st$tok[st$i]
    if (identical(nxt, ",")) { st$i <- st$i + 1L; next }
    if (identical(nxt, "}")) { st$i <- st$i + 1L; break }
    .jr_err(st, "expected ',' or '}'")
  }
  out <- buf[seq_len(m)]
  names(out) <- key[seq_len(m)]
  out
}

.jr_array <- function(st) {
  st$i <- st$i + 1L
  if (identical(st$tok[st$i], "]")) { st$i <- st$i + 1L; return(list()) }
  cap <- 8L
  buf <- vector("list", cap)
  m <- 0L
  repeat {
    if (st$i > st$n) .jr_err(st, "unterminated array")
    v <- .jr_value(st)
    m <- m + 1L
    if (m > cap) { cap <- cap * 2L; length(buf) <- cap }
    buf[m] <- list(v)
    nxt <- st$tok[st$i]
    if (identical(nxt, ",")) { st$i <- st$i + 1L; next }
    if (identical(nxt, "]")) { st$i <- st$i + 1L; break }
    .jr_err(st, "expected ',' or ']'")
  }
  buf[seq_len(m)]
}

.jr_value <- function(st) {
  if (st$i > st$n) .jr_err(st, "unexpected end of input")
  t <- st$tok[st$i]
  c1 <- substr(t, 1L, 1L)
  if (c1 == "{") return(.jr_object(st))
  if (c1 == "[") return(.jr_array(st))
  st$i <- st$i + 1L
  if (c1 == '"') return(.jr_string(t))
  if (t == "true") return(TRUE)
  if (t == "false") return(FALSE)
  if (t == "null") return(NULL)
  if (grepl("^[-0-9]", t)) return(as.numeric(t))
  st$i <- st$i - 1L
  .jr_err(st, paste0("unexpected token '", t, "'"))
}

#' Parse a JSON document held in a character vector (lines are joined).
json_parse <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  tk <- .jr_tokens(txt)
  st <- new.env(parent = emptyenv())
  st$tok <- tk$text
  st$pos <- tk$start
  st$src <- txt
  st$i <- 1L
  st$n <- length(tk$text)
  if (st$n == 0L) stop("json: no value in input", call. = FALSE)
  v <- .jr_value(st)
  if (st$i <= st$n) .jr_err(st, "trailing content after the top-level value")
  v
}

#' Read and parse one JSON file. Bytes are read whole and marked UTF-8, so a
#' tenant reply carrying non-ASCII app names survives whatever the console
#' codepage happens to be; a UTF-8 BOM is dropped.
json_read <- function(path) {
  sz <- file.info(path)$size
  if (is.na(sz)) stop(sprintf("json: no such file: %s", path), call. = FALSE)
  con <- file(path, "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = sz)
  if (length(bytes) >= 3L &&
      identical(as.integer(bytes[1:3]), c(0xEFL, 0xBBL, 0xBFL)))
    bytes <- bytes[-(1:3)]
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  json_parse(txt)
}

#' Walk a parsed document by key (character) or position (numeric), returning
#' NULL the moment a step is missing. `x$a$b` cannot do this job: it errors on
#' a non-list and cannot tell a present null from an absent key.
json_get <- function(x, ...) {
  for (k in list(...)) {
    if (!is.list(x) || !length(x)) return(NULL)
    if (is.character(k)) {
      if (is.null(names(x)) || !(k %in% names(x))) return(NULL)
      x <- x[[k]]
    } else {
      k <- as.integer(k)
      if (is.na(k) || k < 1L || k > length(x)) return(NULL)
      x <- x[[k]]
    }
  }
  x
}

#' Every named list inside a parsed reply that carries `key` - the shape
#' every qlik-cli listing arrives in, whether the items sit at the top level,
#' under "data", or nested one level deeper. Depth-first, source order.
json_items <- function(x, key = "id") {
  out <- list()
  walk <- function(v) {
    if (!is.list(v) || !length(v)) return(invisible(NULL))
    # A matched item is NOT descended into. Qlik items nest an id inside
    # `resourceAttributes` too, so descending would return the same app
    # twice, once under the wrong id.
    if (!is.null(names(v)) && key %in% names(v)) {
      out[[length(out) + 1L]] <<- v
      return(invisible(NULL))
    }
    for (e in v) walk(e)
    invisible(NULL)
  }
  walk(x)
  out
}
