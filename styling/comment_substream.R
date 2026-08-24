# comment_substream.R
#
# Comment-substream extraction/serialization, from the settled design in
# docs/PLAN-comment-substream.md (its own section 3 = design, section 6 =
# gotchas, section 7 = the fixed-point risk this file exists to kill,
# section 9 = the 2026-08-24 resolved decisions). This is step 2 of that
# plan's suggested sequencing: the isolated extract/serialize layer, built
# and proved before any style pass is touched.
#
# NOT a pass, NOT in the pipeline - run_pipeline.R does not source this yet
# (the recursive driver that will is a later task). No styling pass is
# called, modified or threaded here. Base R only.
#
# Source shared/qlik_tokenizer.R and shared/qlik_reserved_words.R first:
#   source("shared/qlik_tokenizer.R")
#   source("shared/qlik_reserved_words.R")
#   source("styling/comment_substream.R")
#
# ---------------------------------------------------------------------
# THE MODEL
#
# A "run" is a maximal contiguous sequence of source lines each of whose
# only content is a `//` comment (a trailing comment sharing a line with
# live code is never eligible; block comments `/* */` and `///$tab`
# section markers are excluded). Two comment-owning lines are "contiguous"
# exactly when their source line numbers are consecutive integers - by
# construction nothing else can occupy a line between them, so this is a
# complete test with no separate "blank line breaks a run" special case.
#
# Extraction re-tokenizes a run's comment bodies (the text after each `//`,
# joined with "\n") into its own child token stream. A run is "script" if
# find_load_segments() recognises a LOAD block in it, or at least one of
# its lines is field-shaped and none is a commented STATEMENT outside a
# LOAD block (control flow / DROP / STORE - out of scope per the plan's
# resolved decision 2). Prose lines inside an otherwise-script run are
# carried through unstyled (decision 3) - classification only decides
# whether the WHOLE run is extracted, never splits it. A run that parses
# to nothing recognisable, or is not paren-balanced end to end, is refused
# whole and left byte-identical (decision 4).
#
# THE ONE MECHANISM section 7 asks for: a run's own LEADING trivia - any
# whitespace, and a depth-0 comma (with its own trailing whitespace) tying
# it to whatever precedes it, e.g. inside a live LOAD field list - is
# stripped from the child stream on extract and tracked as a plain string,
# never as a token. serialize_comment_run() re-supplies it verbatim.
# Because it is metadata and never a token in `run$tokens`, a second
# extract of a styled run cannot re-read it as body text and add another -
# the growth failure this file exists to make structurally impossible, not
# merely checked for. Whitespace is included, not just the comma: leaving
# a plain leading space inside `tokens` on the FIRST extraction (when a
# run has no comma yet) would let a LATER extraction - after some run
# finally gets a leading comma prepended - swallow that old space into the
# comma's own capture instead of leaving it in `tokens`, reshaping the
# body between cycles with nothing "growing" arithmetically to catch.

# ---- run detection --------------------------------------------------

#' Every contiguous run of line-owning `//` comments in a token stream.
#'
#' Reuses find_block_structure()'s own line classification rather than
#' re-deriving "does this comment own its line": a trailing comment sharing
#' a line with live code is never a line start, so it is never a `kind ==
#' "comment"` row there in the first place. `///$tab` markers are already
#' excluded there too (`kind == "section"`); block comments share the
#' COMMENT token type, so they are excluded here by text prefix.
#'
#' @param tokens a token stream data.frame.
#' @return list(runs, warnings). `runs` is a list of integer vectors, each
#'   the parent token indices (COMMENT rows, source order) of one run.
find_comment_runs <- function(tokens) {
  bs <- find_block_structure(tokens)
  ln <- bs$lines
  if (nrow(ln) == 0) return(list(runs = list(), warnings = bs$warnings))

  is_line_comment <- ln$kind == "comment" & startsWith(tokens$text[ln$idx], "//")
  idx <- ln$idx[is_line_comment]
  if (length(idx) == 0) return(list(runs = list(), warnings = bs$warnings))

  # Contiguous means nothing but whitespace sits between two comment-owning
  # lines - an interior BLANK line no longer breaks a run (Adam, 2026-08-24:
  # not a new scope call - resolved decision 3 already implies an interior
  # gap is carried through unstyled inside ONE child stream, the same as
  # interior prose). Anything else in the gap - live code, a block comment,
  # a ///$tab marker - still breaks it, since those are real tokens, not
  # whitespace. Tested by TOKEN INDEX, not by line-number arithmetic, since
  # the gap's actual content (not its line count) is what matters here.
  brk <- logical(length(idx)); brk[1] <- TRUE
  if (length(idx) > 1L) {
    for (k in 2:length(idx)) {
      gap <- if (idx[k] > idx[k - 1L] + 1L) (idx[k - 1L] + 1L):(idx[k] - 1L) else integer(0)
      brk[k] <- length(gap) > 0L && !all(tokens$type[gap] == "WS")
    }
  }
  grp  <- cumsum(brk)
  runs <- unname(split(idx, grp))
  list(runs = runs, warnings = bs$warnings)
}

# ---- classification ---------------------------------------------------

# Statement vocabulary that marks a line as part of a STATEMENT rather than
# a bare field - reuses QLIK_KEYWORDS' own words (shared/qlik_reserved_words.R)
# rather than a second hand-picked list, restricted to the ones that can
# open or close a statement/clause. LOAD itself is handled separately via
# find_load_segments(), not this list, since a recognised LOAD block is
# in-scope (decision 2) while every other statement here is deferred.
.CS_STMT_WORDS <- c(
  "load", "select", "from", "resident", "inline", "autogenerate", "where",
  "drop", "store", "if", "then", "else", "elseif", "end", "endif",
  "for", "next", "each", "do", "loop", "while", "until", "sub", "endsub",
  "switch", "case", "endswitch", "default", "call", "set", "let", "exit",
  "connect", "disconnect", "directory", "comment", "trace", "sleep", "rem",
  "qualify", "unqualify", "tag", "untag", "declare", "derive", "binary",
  "section")

#' Is one line's content (as non-WS token type/text vectors) field-shaped?
#'
#' Strips one optional leading/trailing depth-0 COMMA (the ordinary
#' separators BETWEEN fields inside one run - not the run's own boundary
#' separator, which extract_comment_runs() has already removed before this
#' is ever called), then requires what remains to open with something that
#' can start a reference or expression and to carry no statement keyword.
#'
#' GOTCHA this guards against: "opens with a WORD" alone accepts any
#' English sentence, since prose is bare words too. What tells them apart
#' is that a run of two or more BARE (non-keyword) WORD tokens with only
#' whitespace between them has no meaning as Qlik syntax - an unbracketed
#' multi-word name cannot be written that way - while it is exactly how a
#' sentence reads. A single bare word (DESIGN 1.7's un-bracketed field
#' reference) and keyword-adjacent pairs ("field as alias") are unaffected.
.cs_is_field_shaped <- function(ctypes, ctext) {
  n <- length(ctypes)
  if (n == 0) return(FALSE)
  if (ctypes[1] == "COMMA") { ctypes <- ctypes[-1]; ctext <- ctext[-1]; n <- n - 1L }
  if (n == 0) return(FALSE)
  if (ctypes[n] == "COMMA") { ctypes <- ctypes[-n]; ctext <- ctext[-n]; n <- n - 1L }
  if (n == 0) return(FALSE)
  if (any(ctypes == "WORD" & tolower(ctext) %in% .CS_STMT_WORDS)) return(FALSE)

  starters <- c("BRACKET", "DQUOTE", "SQUOTE", "WORD", "NUMBER", "LPAREN")
  first_ok <- ctypes[1] %in% starters || (ctypes[1] == "OPERATOR" && ctext[1] == "-")
  if (!first_ok) return(FALSE)

  if (n >= 2L) {
    is_kw <- ctypes == "WORD" & tolower(ctext) %in% QLIK_KEYWORDS
    bare_pair <- ctypes[-n] == "WORD" & ctypes[-1] == "WORD" & !is_kw[-n] & !is_kw[-1]
    if (any(bare_pair)) return(FALSE)
  }
  TRUE
}

#' Classify one line's content: "blank" / "stmt" / "field" / "prose".
.cs_line_kind <- function(ctypes, ctext) {
  n <- length(ctypes)
  if (n == 0) return("blank")
  if (any(ctypes == "WORD" & tolower(ctext) %in% .CS_STMT_WORDS)) return("stmt")
  # a bare table label ("Name:" / "[Name]:") is a statement fragment
  if (n == 2L && ctypes[2] == "OTHER" && ctext[2] == ":") return("stmt")
  if (.cs_is_field_shaped(ctypes, ctext)) "field" else "prose"
}

#' Classify a run's already-leading-separator-stripped child stream.
#'
#' @return list(kind, reason, line_kind, is_prose_only). `kind` is
#'   "load_block" / "field_run" / NA (refused or prose-only - see `reason`
#'   and `is_prose_only`). `line_kind` is per-line "blank"/"stmt"/"field"/
#'   "prose" (child line order) - informational, used for the
#'   prose-carried coverage count and unaffected by the run's own verdict.
.cs_classify_run <- function(child) {
  n <- nrow(child)
  if (n == 0) return(list(kind = NA_character_, reason = "empty body",
                           line_kind = character(0), is_prose_only = FALSE))

  bs  <- find_block_structure(child)
  lns <- bs$lines
  line_kind <- character(0)
  if (nrow(lns) > 0) {
    ends <- c(if (nrow(lns) > 1) lns$idx[-1] - 1L else integer(0), n)
    line_kind <- character(nrow(lns))
    for (k in seq_len(nrow(lns))) {
      rng  <- lns$idx[k]:ends[k]
      ctok <- rng[child$type[rng] != "WS"]
      line_kind[k] <- .cs_line_kind(child$type[ctok], child$text[ctok])
    }
  }

  # multi-line expressions balance only across the whole run (plan section
  # 6.1's last bullet) - this is the one refusal test applied to the RUN as
  # a whole rather than line by line.
  depth <- sum(child$type == "LPAREN") - sum(child$type == "RPAREN")
  if (depth != 0L) {
    return(list(kind = NA_character_, reason = "unbalanced parentheses across the run",
                line_kind = line_kind, is_prose_only = FALSE))
  }

  seg <- find_load_segments(child)
  if (length(seg$warnings) > 0) {
    return(list(kind = NA_character_,
                reason = paste("incomplete LOAD statement:", seg$warnings[1]),
                line_kind = line_kind, is_prose_only = FALSE))
  }
  if (length(seg$segments) > 0) {
    return(list(kind = "load_block", reason = NA_character_,
                line_kind = line_kind, is_prose_only = FALSE))
  }

  if (any(line_kind == "stmt")) {
    return(list(kind = NA_character_,
                reason = "commented statement outside a LOAD block (deferred - plan resolved decision 2)",
                line_kind = line_kind, is_prose_only = FALSE))
  }
  if (any(line_kind == "field")) {
    return(list(kind = "field_run", reason = NA_character_,
                line_kind = line_kind, is_prose_only = FALSE))
  }
  list(kind = NA_character_, reason = "prose only", line_kind = line_kind,
       is_prose_only = TRUE)
}

# ---- extraction ---------------------------------------------------------

#' Extract every comment run of a token stream as its own child stream.
#'
#' @param tokens the parent token stream.
#' @return list(runs, warnings). `runs` is a list, one element per run
#'   found by find_comment_runs(), each a list:
#'     comment_idx - parent token indices of this run's COMMENT rows,
#'                   source order (the run's identity; valid only while the
#'                   parent stream is not edited elsewhere)
#'     line_start, line_end - parent source line numbers
#'     status      - "extracted" / "refused" / "prose_only"
#'     kind        - "load_block" / "field_run" / NA
#'     reason      - refusal or skip reason, or NA
#'     leading_sep - exact text of the run's stripped leading trivia -
#'                   whitespace, and a depth-0 comma with its own trailing
#'                   whitespace if present - or NA if there was none (plan
#'                   section 7's normalising extraction - metadata, never
#'                   a token in `tokens`; re-supplied verbatim by
#'                   serialize_comment_run() unless the caller changes it)
#'     tokens      - the child token stream, leading trivia excluded; NULL
#'                   for a refused or prose_only run
#'     line_kind   - per-line "blank"/"stmt"/"field"/"prose", child order
#' Strip a child stream's own leading trivia - whitespace, and a depth-0
#' boundary comma with its own trailing whitespace if present - as ONE
#' captured prefix, so `tokens` always begins at the first substantive
#' token. Section 7's normalising extraction, factored out so both
#' extract_comment_runs() and a future styling pass that BAKES a new
#' leading separator into `tokens` (e.g. a driver's own comma synthesis via
#' enforce_leading_commas()'s context$first_field=FALSE) can restore the
#' same canonical shape - `tokens` never carries leading trivia, in cycle 1
#' or any later one. See extract_comment_runs()'s own header comment for
#' the full failure this prevents (the growth-per-re-run corruption).
#'
#' @param child a child token stream (text/type/line).
#' @return list(tokens, leading_sep) - `leading_sep` is NA_character_ if
#'   there was none.
.cs_strip_leading_sep <- function(child) {
  n0 <- nrow(child)
  i <- 1L
  if (n0 > 0 && child$type[i] == "WS") i <- i + 1L
  if (i <= n0 && child$type[i] == "COMMA") {
    i <- i + 1L
    if (i <= n0 && child$type[i] == "WS") i <- i + 1L
  }
  cut <- i - 1L
  leading_sep <- NA_character_
  if (cut >= 1L) {
    leading_sep <- paste(child$text[seq_len(cut)], collapse = "")
    child <- child[-seq_len(cut), , drop = FALSE]
    rownames(child) <- NULL
  }
  list(tokens = child, leading_sep = leading_sep)
}

extract_comment_runs <- function(tokens) {
  fr <- find_comment_runs(tokens)
  runs <- vector("list", length(fr$runs))

  for (r in seq_along(fr$runs)) {
    comment_idx <- fr$runs[[r]]
    bodies   <- substring(tokens$text[comment_idx], 3L)
    combined <- paste(bodies, collapse = "\n")
    child    <- tokenize_qlik(combined)

    # Strip the run's own leading trivia - whitespace, and a boundary
    # comma with its own trailing whitespace if present - as ONE captured
    # prefix, so the child's tokens always begin at the first substantive
    # token. This is section 7's normalising extraction generalised past
    # "comma only": a plain leading space (the ordinary "// <body>"
    # authoring shape) is just as capable of drifting the fixed point if
    # left inside `tokens` - a later set_leading_separator() call would
    # prepend a NEW separator in FRONT of that old space, and the next
    # extract would then swallow the old space into ITS capture instead of
    # leaving it as child content, silently reshaping the body between
    # cycles even though no character was duplicated. Capturing it up
    # front removes the asymmetry: `tokens` never carries leading trivia,
    # in cycle 1 or any later one.
    stripped <- .cs_strip_leading_sep(child)
    child <- stripped$tokens
    leading_sep <- stripped$leading_sep

    cls <- .cs_classify_run(child)
    status <- if (isTRUE(cls$is_prose_only)) "prose_only"
              else if (is.na(cls$kind)) "refused" else "extracted"

    runs[[r]] <- list(
      comment_idx = comment_idx,
      line_start  = tokens$line[comment_idx[1]],
      line_end    = tokens$line[comment_idx[length(comment_idx)]],
      status      = status,
      kind        = cls$kind,
      reason      = cls$reason,
      leading_sep = leading_sep,
      tokens      = if (status == "extracted") child else NULL,
      line_kind   = cls$line_kind)
  }
  list(runs = runs, warnings = fr$warnings)
}

# ---- serialization --------------------------------------------------

#' The WS text to use between newly spliced comment lines - sampled from
#' the run's own existing internal line break where one exists (a run of
#' 2+ lines), falling back to the newline+indentation the run's OWN first
#' line already sits on (never the blank-line gap BEFORE the run, which
#' belongs to whatever precedes it).
.cs_line_sep_template <- function(tokens, old_idx) {
  if (length(old_idx) >= 2L) {
    lo <- old_idx[1] + 1L; hi <- old_idx[2] - 1L
    if (hi >= lo && all(tokens$type[lo:hi] == "WS"))
      return(paste(tokens$text[lo:hi], collapse = ""))
  }
  j <- old_idx[1] - 1L
  if (j >= 1L && tokens$type[j] == "WS" && grepl("\n", tokens$text[j], fixed = TRUE)) {
    m <- regmatches(tokens$text[j], regexpr("\n[ \t]*$", tokens$text[j]))
    if (length(m) == 1 && nzchar(m)) return(m)
  }
  "\n"
}

#' Set (or clear) a run's leading separator.
#'
#' The sanctioned way for a future styling pass to add or remove the comma
#' tying a run to whatever precedes it, WITHOUT touching `run$tokens` -
#' see plan section 7: the leading separator is metadata, never a token in
#' the child stream, which is what keeps a second extract from reading a
#' styled-in comma as body text and adding another.
#'
#' @param run one element of extract_comment_runs()'s $runs.
#' @param sep character - exact separator text (e.g. ", "), or NA to clear.
#' @return the run, with `$leading_sep` updated.
set_leading_separator <- function(run, sep) {
  run$leading_sep <- if (is.na(sep)) NA_character_ else sep
  run
}

#' Serialize one (possibly modified) run back into a parent token stream's
#' COMMENT tokens.
#'
#' @param tokens the parent token stream, in the state it was in when the
#'   run was extracted (`run$comment_idx` are ORIGINAL positions - never
#'   call this after the parent has been edited elsewhere).
#' @param run one element of extract_comment_runs()'s $runs with
#'   `status == "extracted"`; `$tokens` and/or `$leading_sep` may have been
#'   modified by the caller.
#' @return the parent token stream with this run's comment lines rewritten.
#'   Existing COMMENT rows get new text in place; splice_tokens()/
#'   void_token() add or remove rows if the modified child now spans a
#'   different number of lines than the original run did.
serialize_comment_run <- function(tokens, run) {
  stopifnot(run$status == "extracted")
  lead <- if (!is.na(run$leading_sep)) run$leading_sep else ""
  body_text <- paste0(lead, paste(run$tokens$text, collapse = ""))

  new_lines <- strsplit(body_text, "\n", fixed = TRUE)[[1]]
  if (length(new_lines) == 0) new_lines <- ""
  if (grepl("\n$", body_text)) new_lines <- c(new_lines, "")
  new_text <- paste0("//", new_lines)

  old_idx <- run$comment_idx
  n_old <- length(old_idx); n_new <- length(new_text)
  m <- min(n_old, n_new)
  tokens$text[old_idx[seq_len(m)]] <- new_text[seq_len(m)]

  if (n_new > n_old) {
    # New rows need real, INCREMENTING line numbers, matching the
    # tokenizer's own convention (a token's line is where it STARTS,
    # before consuming any newline it carries) - find_comment_runs()'s
    # contiguity test compares this field directly, so leaving every
    # inserted row at the same line as the run's old last line (the
    # obvious-looking shortcut) makes the new line look like a BREAK, not
    # a continuation, and it silently starts a second run instead of
    # extending this one.
    sep_text <- .cs_line_sep_template(tokens, old_idx)
    extra <- new_text[(n_old + 1L):n_new]
    k <- length(extra)
    base_line <- tokens$line[old_idx[n_old]]
    ins <- data.frame(
      text = as.vector(rbind(rep(sep_text, k), extra)),
      type = as.vector(rbind(rep("WS", k), rep("COMMENT", k))),
      line = as.vector(rbind(base_line + (seq_len(k) - 1L), base_line + seq_len(k))),
      stringsAsFactors = FALSE)
    tokens <- splice_tokens(tokens, setNames(list(ins), as.character(old_idx[n_old])))
  } else if (n_new < n_old) {
    surplus <- old_idx[(n_new + 1L):n_old]
    ws_before <- vapply(surplus, function(i) {
      j <- i - 1L
      if (j >= 1L && tokens$type[j] == "WS") j else NA_integer_
    }, integer(1))
    tokens <- void_token(tokens, c(surplus, ws_before[!is.na(ws_before)]))
  }

  tokens
}
