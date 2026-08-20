# verify.R
#
# Standing verification suite for the style pipeline. STAGE 3 ONLY - its
# main() always reads both large fixtures, so it runs only at stage 3 of the
# testing methodology (CLAUDE.md), on Adam's go-ahead, and refuses to start
# without the flag that acknowledges that:
#
#     Rscript verify.R --stage3
#
# Exits non-zero if anything fails, so it can gate a commit.
# (For documentation consistency at any stage, use verify_docs.R - it reads
# no fixtures.)
#
# WHY THIS EXISTS
#
# Three signals were being relied on, and each has a blind spot:
#
#   $changes      is SELF-REPORTED and describes intent, not placement. A pass
#                 logs "inserted AS [X] at line 42" and that entry stays true
#                 even if splice_tokens() put it after the wrong token. A
#                 dropped field produces no entry at all.
#   golden diff   answers "did the output change since last time?" - useful
#                 for regressions, and useless the moment output changes on
#                 purpose, which is exactly what adding a pass does.
#   round-trip    proves the TOKENIZER is lossless. It says nothing about
#                 whether a pass corrupted anything.
#
# The semantic equivalence check below is invariant across every cosmetic
# pass, present and future: it keeps holding while the output legitimately
# changes. That matters most for the remaining passes (spacing, layout,
# alignment), which are all whitespace manipulators - the class that can
# corrupt content while reporting truthfully. A space inserted inside
# [Grant Activity], making it [Grant  Activity], is a renamed field, logged
# as "added a space", and invisible to every other check.
#
# LIMITS. This verifies the COSMETIC phase only. Retargeting changes field
# names by design, so equivalence is violated on purpose there. And it proves
# nothing changed outside the permitted set - not that Qlik agrees the two
# scripts behave identically.

# sys.frame(1)$ofile only resolves when this script is source()d, NOT under
# plain `Rscript verify.R` invocation (verified experimentally: it errors
# "not that many frames on the stack" there) - so it was never actually
# self-locating when run the normal way; commandArgs()'s --file= entry is
# the method that works under Rscript. C:/Rtools kept as a last-resort
# fallback for the source()d case.
.file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
setwd(if (length(.file_arg)) {
  tryCatch(dirname(normalizePath(sub("^--file=", "", .file_arg[1]))),
           error = function(e) "C:/Rtools")
} else {
  tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "C:/Rtools")
})

source("qlik_tokenizer.R")
source("qlik_reserved_words.R")
source("ensure_explicit_aliases.R")
source("enforce_bracket_references.R")
source("enforce_leading_commas.R")
source("enforce_intraline_spacing.R")
source("enforce_reserved_word_case.R")
source("enforce_vertical_layout.R")
source("enforce_alias_alignment.R")
source("enforce_commented_field_style.R")

# ---- reporting ---------------------------------------------------------

.fails <- 0L
.checks <- 0L

ok <- function(label, passed, detail = NULL) {
  .checks <<- .checks + 1L
  if (!passed) .fails <<- .fails + 1L
  cat(sprintf("  [%s] %s\n", if (passed) "PASS" else "FAIL", label))
  if (!passed && !is.null(detail)) cat(paste0("         ", detail, "\n"), sep = "")
  flush(stdout())
  invisible(passed)
}

section <- function(...) { cat("\n== ", ..., " ==\n", sep = ""); flush(stdout()) }

# ---- semantic equivalence ----------------------------------------------
# Reduce a token stream to the sequence of things that carry MEANING, with
# every difference the cosmetic passes are ALLOWED to introduce normalised
# away. Two streams that reduce to the same vector are equivalent as far as
# the style guide is concerned.
#
# Normalisations, each matching exactly one permitted transformation:
#
#   whitespace / comments dropped        layout, spacing, comment removal
#   [X] and "X" folded to REF:X          enforce_bracket_references
#   'X' right of AS folded to REF:X      enforce_bracket_references (alias
#                                        position only - a single-quoted
#                                        token anywhere else is a LITERAL and
#                                        stays distinct, so a literal turning
#                                        into a field reference is caught)
#   bare X inside a LOAD field list      enforce_bracket_references, applied
#   folded to REF:X, UNLESS it is a      under the identical rule the pass
#   keyword or in call position          uses (find_load_segments() content,
#                                        not a keyword, next token not "(")
#                                        - so a bare word OUTSIDE a field
#                                        list (a FOR counter, a LET variable)
#                                        is never folded, and one wrongly
#                                        bracketed there would still be caught
#   reserved words upper-cased           enforce_reserved_word_case, applied
#                                        under the SAME call-position rule the
#                                        pass uses, so a bare field named
#                                        Year is NOT folded and a wrongly
#                                        recased one is caught
#   X AS X collapsed to X                ensure_explicit_aliases
#
# Comma relocation needs no rule: moving a comma from trailing to leading
# does not change token ORDER, only the whitespace around it.
#
# Each of those normalisations is an assumption about Qlik, and the check is
# only as correct as they are. They are recorded in DESIGN.md §1 with their
# verification status - notably §1.5, that LOAD X and LOAD X AS X load
# identically, which is what licenses collapsing X AS X here.

# DELIBERATELY NOT qlik_tokenizer.R's undelimit(), though it computes the
# same thing. Adam 2026-08-20, when every other copy in the repo was folded
# into that one: this file is the check, and a check that shares its
# implementation with the code under test cannot fail independently of it.
# undelimit() is what the PASSES use to decide what a reference is called;
# .unquote is this suite's own separate statement of the same DESIGN §1
# assumption, so a bug in one is still visible to the other.
#
# So: this is not an oversight and not a leftover twin. Do not merge it.
.unquote <- function(s, q) {
  inner <- substr(s, 2, nchar(s) - 1)
  gsub(paste0(q, q), q, inner, fixed = TRUE)
}

#' Reduce a token stream to its meaning-carrying canonical form.
#'
#' Source line numbers are carried alongside, because a failure reported as
#' "differs at position 4213" on a 31,439-token stream is not something anyone
#' can act on. Every canonical entry knows the script line it came from.
#'
#' @return list(canon = character, line = integer), the two parallel.
canonical_stream <- function(tokens) {
  # Which ORIGINAL token indices sit inside a LOAD field list - computed
  # before the trivia filter below, because find_load_segments() indexes
  # into the full, unfiltered stream. Mirrors enforce_bracket_references.R's
  # scope exactly: a bare word is only foldable to a reference here if the
  # pass itself would actually bracket it. A bare word elsewhere (a FOR
  # counter, a LET variable) is NOT folded, so if a future bug ever did
  # bracket one, this check would still catch it as a real difference.
  in_field_orig <- logical(nrow(tokens))
  for (seg in find_load_segments(tokens)$segments) in_field_orig[seg$content_idx] <- TRUE

  keep <- !(tokens$type %in% c("WS", "COMMENT", "VOID"))
  ty <- tokens$type[keep]
  tx <- tokens$text[keep]
  ln <- tokens$line[keep]
  in_field <- in_field_orig[keep]
  n <- length(ty)
  if (n == 0) return(list(canon = character(0), line = integer(0)))

  lower <- tolower(tx)
  prev_is_as <- c(FALSE, ty[-n] == "WORD" & lower[-n] == "as")
  next_is_lparen <- c(ty[-1] == "LPAREN", FALSE)

  out <- character(n)
  for (i in seq_len(n)) {
    out[i] <- switch(
      ty[i],
      BRACKET = paste0("REF:", substr(tx[i], 2, nchar(tx[i]) - 1)),
      DQUOTE  = paste0("REF:", .unquote(tx[i], "\"")),
      SQUOTE  = if (prev_is_as[i]) paste0("REF:", .unquote(tx[i], "'"))
                else paste0("STR:", .unquote(tx[i], "'")),
      WORD    = {
        is_kw   <- lower[i] %in% QLIK_KEYWORDS
        call    <- next_is_lparen[i] && lower[i] %in% QLIK_FUNCTIONS
        # a bare word in a field list, not a keyword, not a call - the exact
        # set enforce_bracket_references.R now brackets (call position alone
        # excludes it below too, whether or not the word is in QLIK_FUNCTIONS,
        # matching the pass's own guard - a user-defined SUB call included)
        bare_ref <- in_field[i] && !is_kw && !next_is_lparen[i]
        if (bare_ref) paste0("REF:", tx[i])
        else paste0("W:", if (is_kw || call) toupper(tx[i]) else tx[i])
      },
      paste0("P:", tx[i])
    )
  }

  # collapse a self-alias (X AS X) to X, on BOTH sides. An alias that is not
  # a self-alias does not collapse, so a pass that rewrote X AS Y into X AS X
  # still shows up as a difference. The surviving entry keeps the line of the
  # field itself.
  if (n >= 3) {
    drop <- logical(n)
    i <- 1L
    while (i <= n - 2L) {
      if (out[i + 1L] == "W:AS" && out[i] == out[i + 2L] &&
          !startsWith(out[i], "STR:")) {
        drop[i + 1L] <- TRUE; drop[i + 2L] <- TRUE
        i <- i + 3L
      } else i <- i + 1L
    }
    out <- out[!drop]; ln <- ln[!drop]
  }
  list(canon = out, line = ln)
}

# A single canonical entry can be enormous - a bracketed INLINE block holds
# its whole data table in one token. Truncate for display, or one dropped
# field prints twenty lines of inline data four times over.
.short <- function(x, n = 44L) {
  x <- gsub("[\r\n\t]+", " ", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1L, n - 3L), "..."), x)
}

# render a window of canonical entries, tagged with the lines they span
.window <- function(canon, line, i, span = 4L) {
  if (length(canon) == 0L) return("(empty)")
  lo <- max(1L, i - span); hi <- min(length(canon), i + span)
  sprintf("lines %d-%d: %s", line[lo], line[hi],
          paste(.short(canon[lo:hi]), collapse = " "))
}

.tail <- function(v, from) if (from > length(v)) character(0) else v[from:length(v)]

#' Assert two token streams are semantically equivalent.
#' @return TRUE, or a character vector describing the first differences,
#'   each located by SOURCE LINE rather than by canonical index.
check_equivalent <- function(before, after, max_report = 3) {
  A <- canonical_stream(before); B <- canonical_stream(after)
  a <- A$canon; b <- B$canon
  if (identical(a, b)) return(TRUE)

  msg <- sprintf("canonical length: before=%d after=%d", length(a), length(b))
  m <- min(length(a), length(b))
  d <- which(a[seq_len(m)] != b[seq_len(m)])

  if (length(d) == 0) {
    # identical prefix, so one side simply has more at the end
    long_is_before <- length(a) > length(b)
    extra  <- if (long_is_before) a[(m + 1):length(a)] else b[(m + 1):length(b)]
    exline <- if (long_is_before) A$line[(m + 1):length(a)] else B$line[(m + 1):length(b)]
    msg <- c(msg,
             sprintf("streams agree for %d entries, then %s has %d extra",
                     m, if (long_is_before) "before" else "after", length(extra)),
             sprintf("  first extra at source line %d: %s",
                     exline[1], paste(head(extra, 6), collapse = " ")))
    if (m > 0) msg <- c(msg, sprintf("  last agreeing entry was at source line %d",
                                     if (long_is_before) A$line[m] else B$line[m]))
    return(msg)
  }

  i <- d[1]

  # A single insertion or deletion makes every later entry mismatch, which
  # would otherwise be reported as thousands of differences. Detect it: if
  # skipping one entry on the longer side realigns the rest, that is what
  # happened, and saying so is far more useful than listing the cascade.
  if (length(a) == length(b) + 1L && identical(.tail(a, i + 1L), .tail(b, i))) {
    return(c(msg,
             sprintf("one entry REMOVED, at source line %d: %s", A$line[i], .short(a[i])),
             sprintf("  ctx before %s", .window(a, A$line, i)),
             sprintf("  ctx after  %s", .window(b, B$line, i))))
  }
  if (length(b) == length(a) + 1L && identical(.tail(b, i + 1L), .tail(a, i))) {
    return(c(msg,
             sprintf("one entry ADDED, at source line %d: %s", B$line[i], .short(b[i])),
             sprintf("  ctx before %s", .window(a, A$line, i)),
             sprintf("  ctx after  %s", .window(b, B$line, i))))
  }

  n_shown <- min(length(d), max_report)
  msg <- c(msg, sprintf("%d differing entr%s; first %d shown%s",
                        length(d), if (length(d) == 1L) "y" else "ies", n_shown,
                        if (length(a) != length(b))
                          " (lengths differ, so later entries may just be shifted)" else ""))
  for (i in head(d, max_report)) {
    msg <- c(msg,
             sprintf("  source line %d (before) / %d (after)", A$line[i], B$line[i]),
             sprintf("    before: %s", .short(a[i])),
             sprintf("    after : %s", .short(b[i])),
             sprintf("    ctx before %s", .window(a, A$line, i)),
             sprintf("    ctx after  %s", .window(b, B$line, i)))
  }
  msg
}

# ---- the suite ---------------------------------------------------------

#' Canonical form of every `//` comment body in a stream.
#'
#' canonical_stream() drops COMMENT tokens outright, so until this existed
#' NOTHING in verify.R asserted anything whatsoever about comment content -
#' the 97 checks passed on a run that mangled every comment in the file.
#' That was harmless while comments were exempt from formatting (DESIGN
#' §4.10, pre-2026-08-20) and is not harmless now that they are styled.
#'
#' Each body is canonicalised by the SAME rules live fields get: it is
#' wrapped in a synthetic `LOAD ... ;` so find_load_segments() sees a field
#' list, which is what licenses canonical_stream() to fold a bare word and
#' its bracketed form together (and a self-alias onto its field). Comparing
#' anything less normalised would report every legitimate styling change as
#' a corruption.
#'
#' Separator commas are dropped from the result. Pass 3 deliberately moves a
#' comma from the end of one line to the start of the next, which migrates
#' it ACROSS the comment/live boundary in both directions; position is
#' exactly what this check must not be sensitive to.
#'
#' @param tokens a token stream data.frame.
#' @return character vector, all comment bodies' canonical entries in order.
commented_field_bodies <- function(tokens) {
  cm <- which(tokens$type == "COMMENT" &
                startsWith(tokens$text, "//") &
                !startsWith(tokens$text, "///$"))
  if (length(cm) == 0) return(character(0))

  out <- character(0)
  for (i in cm) {
    body <- sub("^//+", "", tokens$text[i])
    if (!nzchar(trimws(body))) next
    cs <- canonical_stream(tokenize_qlik(paste0("LOAD ", body, " ;")))$canon
    # strip the synthetic wrapper and every separator comma
    if (length(cs) > 0 && cs[1] == "W:LOAD") cs <- cs[-1]
    if (length(cs) > 0 && cs[length(cs)] == "P:;") cs <- cs[-length(cs)]
    out <- c(out, cs[cs != "P:,"])
  }
  out
}

PASSES <- list(
  "ensure_explicit_aliases"     = ensure_explicit_aliases,
  "enforce_bracket_references"  = enforce_bracket_references,
  "enforce_leading_commas"      = enforce_leading_commas,
  "enforce_intraline_spacing"   = enforce_intraline_spacing,
  "enforce_reserved_word_case"  = enforce_reserved_word_case,
  "enforce_vertical_layout"     = enforce_vertical_layout,
  "enforce_alias_alignment"     = enforce_alias_alignment,
  "enforce_commented_field_style" = enforce_commented_field_style
)

verify_file <- function(path) {
  section(path)

  raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tokens <- read_qlik_script(path)

  # 1. tokenizer is lossless
  ok("tokenizer round-trip is exact",
     identical(raw, paste(detokenize(tokens), collapse = "\n")))

  # 2. token stream is well formed
  ok("no NA in token stream",
     !any(is.na(tokens$text)) && !any(is.na(tokens$type)) && !any(is.na(tokens$line)))
  ok("line numbers are non-decreasing", !is.unsorted(tokens$line))

  # 3. each pass in isolation
  cur <- tokens
  for (nm in names(PASSES)) {
    res <- PASSES[[nm]](cur)

    eq <- check_equivalent(cur, res$tokens)
    ok(sprintf("%s: semantically equivalent", nm), isTRUE(eq),
       if (!isTRUE(eq)) eq else NULL)

    ok(sprintf("%s: output round-trips", nm),
       identical(paste(res$tokens$text, collapse = ""),
                 paste(detokenize(res$tokens), collapse = "\n")))

    again <- PASSES[[nm]](res$tokens)
    ok(sprintf("%s: idempotent", nm), nrow(again$changes) == 0L,
       if (nrow(again$changes) > 0)
         sprintf("second run still reported %d change(s)", nrow(again$changes)) else NULL)

    cur <- res$tokens
  }

  # 4. the pipeline as a whole
  eq <- check_equivalent(tokens, cur)
  ok("full pipeline: semantically equivalent to input", isTRUE(eq),
     if (!isTRUE(eq)) eq else NULL)

  # 5. pass-specific guarantees
  r_case <- enforce_reserved_word_case(tokens)
  nonword <- tokens$type != "WORD"
  ok("casing: non-WORD tokens untouched",
     identical(tokens$text[nonword], r_case$tokens$text[nonword]))
  ok("casing: every change is case-only",
     all(tolower(r_case$changes$before) == tolower(r_case$changes$after)))

  r_br <- enforce_bracket_references(tokens)
  ok("brackets: every change yields a bracketed token",
     all(grepl("^\\[.*\\]$", r_br$changes$after)))

  r_lay <- enforce_vertical_layout(tokens)
  # SEMI (and VOID) excluded too, not just WS (2026-08-17): a lone ';'
  # separated from its real target by a comment is now physically relocated
  # (DESIGN §4.9 extension) - the one content token this pass is allowed to
  # move. The relocation leaves the original SEMI's row behind as VOID,
  # which must be excluded from "after" too or the filtered sequences drift
  # out of alignment by one. The SEMI-count equality clause still catches a
  # dropped/duplicated one.
  keep_before <- tokens$type != "WS" & tokens$type != "SEMI"
  keep_after  <- r_lay$tokens$type != "WS" & r_lay$tokens$type != "SEMI" &
    r_lay$tokens$type != "VOID"
  ok("layout: only WS token text changes (plus relocating a SEMI past a comment), nothing else",
     identical(tokens$text[keep_before], r_lay$tokens$text[keep_after]) &&
       identical(tokens$type[keep_before], r_lay$tokens$type[keep_after]) &&
       sum(tokens$type == "SEMI") == sum(r_lay$tokens$type == "SEMI"))

  r_align <- enforce_alias_alignment(r_lay$tokens)
  ok("alignment: every change is tabs plus the preserved space before AS",
     nrow(r_align$changes) == 0L || all(grepl("^\t+ $", r_align$changes$after)))

  # 6. commented-out fields (DESIGN §4.10, Adam 2026-08-20)
  # Pass 8 rewrites the TEXT OF COMMENT TOKENS AND NOTHING ELSE. That single
  # invariant replaces the whole apparatus the earlier unwrap/rewrap design
  # needed, so check it directly and exactly: every non-COMMENT row must be
  # byte-identical, and the row count must not move.
  r_cmt <- enforce_commented_field_style(tokens)
  keep_c <- tokens$type != "COMMENT"
  ok("commented fields: no live token is touched at all",
     nrow(r_cmt$tokens) == nrow(tokens) &&
       identical(tokens$text[keep_c], r_cmt$tokens$text[keep_c]) &&
       identical(tokens$type, r_cmt$tokens$type) &&
       identical(tokens$line, r_cmt$tokens$line))

  ok("commented fields: every change is a comment, still a comment",
     nrow(r_cmt$changes) == 0L ||
       all(startsWith(r_cmt$changes$before, "//") &
             startsWith(r_cmt$changes$after, "//")))

  # the styled text must read back as ONE comment token - if a rewrite ever
  # emitted a newline it would split into live script on the next line
  ok("commented fields: each restyled line re-reads as a single comment",
     nrow(r_cmt$changes) == 0L ||
       all(vapply(r_cmt$changes$after, function(x) {
         tk <- tokenize_qlik(x); nrow(tk) == 1L && tk$type[1] == "COMMENT"
       }, logical(1))))

  # and the field inside the comment must still mean the same thing
  before_c <- commented_field_bodies(tokens)
  after_c  <- commented_field_bodies(r_cmt$tokens)
  ok("commented fields: every comment body preserved through styling",
     identical(before_c, after_c),
     if (!identical(before_c, after_c))
       sprintf("%d canonical comment entries before, %d after",
               length(before_c), length(after_c)) else NULL)

  invisible(NULL)
}

# ---- deliberate-corruption tests ---------------------------------------
# A checker that never fails proves nothing. Confirm it actually detects the
# failure modes it exists for.

verify_detects_corruption <- function() {
  section("self-test: the equivalence check detects real corruption")

  src <- paste("[T]:", "LOAD", "    [Field One] AS [A],",
               "    \"Field Two\" AS [B],", "    Year as [C]",
               "FROM x;", sep = "\n")
  base <- tokenize_qlik(src)

  # a space smuggled inside a bracketed name - the exact bug a whitespace
  # pass could introduce while truthfully logging "added a space"
  bad <- base
  i <- which(bad$type == "BRACKET" & bad$text == "[Field One]")[1]
  bad$text[i] <- "[Field  One]"
  ok("detects a space inserted inside a field name", !isTRUE(check_equivalent(base, bad)))

  # a dropped field
  bad2 <- base
  j <- which(bad2$type == "BRACKET" & bad2$text == "[A]")[1]
  bad2$text[j] <- ""; bad2$type[j] <- "VOID"
  ok("detects a dropped alias", !isTRUE(check_equivalent(base, bad2)))

  # a bare field named after a function, wrongly upper-cased
  bad3 <- base
  k <- which(bad3$type == "WORD" & bad3$text == "Year")[1]
  bad3$text[k] <- "YEAR"
  ok("detects a bare field reference being recased", !isTRUE(check_equivalent(base, bad3)))

  # a string literal turned into a field reference
  src2 <- "[T]:\nLOAD 'Overdue' AS [Bill Status] FROM x;"
  b1 <- tokenize_qlik(src2)
  b2 <- b1
  m <- which(b2$type == "SQUOTE")[1]
  b2$text[m] <- "[Overdue]"; b2$type[m] <- "BRACKET"
  ok("detects a literal becoming a field reference", !isTRUE(check_equivalent(b1, b2)))

  # and must NOT flag the transformations the passes legitimately make
  r <- ensure_explicit_aliases(tokenize_qlik("[T]:\nLOAD [A], \"B\", C\nFROM x;"))
  ok("does not flag legitimate self-aliasing",
     isTRUE(check_equivalent(tokenize_qlik("[T]:\nLOAD [A], \"B\", C\nFROM x;"), r$tokens)))

  r2 <- enforce_bracket_references(tokenize_qlik("[T]:\nLOAD \"A\" AS 'B' FROM x;"))
  ok("does not flag quote-to-bracket conversion",
     isTRUE(check_equivalent(tokenize_qlik("[T]:\nLOAD \"A\" AS 'B' FROM x;"), r2$tokens)))

  # bare-word bracketing (added 2026-08-17, real cases at app-unbuilt/
  # script.qvs: "Electorate," and "Fleet.Rego as [Fleet Car Rego]")
  src3 <- "[T]:\nLOAD Fleet.Rego, Year, YEAR(x) AS [Y] FROM tbl;"
  r3 <- enforce_bracket_references(ensure_explicit_aliases(tokenize_qlik(src3))$tokens)
  ok("does not flag a bare field or a bare field sharing a function's name",
     isTRUE(check_equivalent(tokenize_qlik(src3), r3$tokens)))
  ok("a bare field is actually bracketed, not just unflagged",
     any(r3$tokens$type == "BRACKET" & r3$tokens$text == "[Fleet.Rego]") &&
       any(r3$tokens$type == "BRACKET" & r3$tokens$text == "[Year]"))
  ok("the real function call is left bare, not bracketed into [YEAR](x)",
     any(r3$tokens$type == "WORD" & r3$tokens$text == "YEAR"))

  # --- commented-out fields (DESIGN §4.10, Adam 2026-08-20) --------------
  # Pass 8 styles commented-out fields by rewriting comment TEXT ONLY. The
  # tests that matter are the eligibility boundaries: what it refuses to
  # touch is as load-bearing as what it styles, because a wrong call here is
  # silent - the script still runs, it just says something else.
  src_c1 <- paste("[T]:", "LOAD", "    [A] AS [X],",
                "//     \"B.b%\",", "    [C] AS [Y]", "FROM x;", sep = "
")
  t_c1 <- tokenize_qlik(src_c1)
  r_c1 <- enforce_commented_field_style(t_c1)
  ok("pass 8: a commented field inside a LOAD list is styled",
     nrow(r_c1$changes) == 1L,
     sprintf("%d change(s)", nrow(r_c1$changes)))
  ok("pass 8: the styled result is bracketed, aliased and comma-led",
     nrow(r_c1$changes) == 1L &&
       grepl("[B.b%]", r_c1$changes$after[1], fixed = TRUE) &&
       grepl(" AS ", r_c1$changes$after[1], fixed = TRUE) &&
       grepl("^//	*, ", r_c1$changes$after[1]))
  ok("pass 8: live script is untouched, token for token",
     identical(t_c1$text[t_c1$type != "COMMENT"],
               r_c1$tokens$text[r_c1$tokens$type != "COMMENT"]) &&
       identical(t_c1$type, r_c1$tokens$type))
  ok("pass 8: idempotent",
     nrow(enforce_commented_field_style(r_c1$tokens)$changes) == 0L)

  # prose between fields must NOT be rewritten as code
  src_c2 <- paste("[T]:", "LOAD", "    [A] AS [X],",
                "//  check this against the source system first",
                "    [C] AS [Y]", "FROM x;", sep = "
")
  ok("pass 8: a prose note between fields is left alone",
     nrow(enforce_commented_field_style(tokenize_qlik(src_c2))$changes) == 0L)

  # a trailing comment shares its line with live code; this pass rewrites
  # whole lines, so it must decline
  src_c3 <- paste("[T]:", "LOAD", "    [A] AS [X], //  [B.b%] AS [Z],",
                "    [C] AS [Y]", "FROM x;", sep = "
")
  ok("pass 8: a trailing comment on a live line is refused",
     nrow(enforce_commented_field_style(tokenize_qlik(src_c3))$changes) == 0L)

  # half of a multi-line commented expression: styling it alone is
  # meaningless and its paren depth never returns to zero
  src_c4 <- paste("[T]:", "LOAD", "    [A] AS [X],",
                "//    IF(LEN([B.b%]) > 1, [B.b%],",
                "//    'x')) AS [Z],", "    [C] AS [Y]", "FROM x;", sep = "
")
  ok("pass 8: an unbalanced expression fragment is refused",
     nrow(enforce_commented_field_style(tokenize_qlik(src_c4))$changes) == 0L)

  # a commented FIRST field gets the two-space pad, not a leading comma -
  # otherwise uncommenting it yields "LOAD , [X]", which Qlik rejects
  src_c5 <- paste("[T]:", "LOAD", "//     \"B.b%\",", "    [C] AS [Y]",
                 "FROM x;", sep = "
")
  r_c5 <- enforce_commented_field_style(tokenize_qlik(src_c5))
  ok("pass 8: a commented FIRST field is padded, not comma-led",
     nrow(r_c5$changes) == 1L && !grepl("^//	*,", r_c5$changes$after[1]),
     if (nrow(r_c5$changes)) r_c5$changes$after[1] else "no change")

  # the safety boundary enforce_bracket_references exists to respect: a bare
  # word OUTSIDE a LOAD field list - a FOR loop counter or a LET-assigned
  # variable, real names from app-unbuilt/script.qvs's chunking loop - must
  # never be bracketed. Bracketing chunkText would turn a variable reference
  # into a field reference and silently change what the script does.
  src4 <- "LET chunkText = '';
FOR i = 1 to 3
LET chunkText = chunkText & i;
NEXT"
  r4 <- enforce_bracket_references(tokenize_qlik(src4))
  ok("a LET/FOR variable is never bracketed",
     !any(r4$tokens$type == "BRACKET") &&
       identical(detokenize(r4$tokens), detokenize(tokenize_qlik(src4))))

  invisible(NULL)
}

# ---- block-structure scanner -------------------------------------------
# find_block_structure() has no pass consuming it yet (the vertical layout
# pass is DESIGN 6.2), so nothing exercises it indirectly the way the other
# shared scanners are exercised through the passes that call them. These are
# synthetic streams by design - README's "synthetic proves the logic, the
# real fixture proves it survives" - and the fixtures cover it once the
# layout pass lands.

verify_block_structure <- function() {
  section("self-test: the block-structure scanner")

  kinds <- function(src) find_block_structure(tokenize_qlik(src))$lines$kind

  # a field whose expression runs onto a second line is a continuation, not a
  # new field - the DESIGN 4.5 rule, and the one thing the layout pass most
  # needs right
  src <- paste("[T]:", "LOAD", "    [A] AS [A],", "    IF(x > 1, 'y',",
               "    IF(z, 'a', 'b')) AS [C]", "FROM x;", sep = "\n")
  ok("field vs continuation vs statement",
     identical(kinds(src),
               c("statement", "statement", "field", "field", "continuation", "statement")))

  # DESIGN 4.8: a whole block is ONE statement, so no blank lines land inside
  # a loop body - and the closing NEXT belongs to the block, not to what
  # follows it
  b <- find_block_structure(tokenize_qlik(paste(
    "FOR i = 1 to 3", "LET a = 1;", "LET b = 2;", "NEXT", "LET c = 3;", sep = "\n")))
  ok("a block is one statement, closer included",
     identical(b$lines$stmt_id, c(1L, 1L, 1L, 1L, 2L)))
  ok("block body is deeper than its opener and closer",
     identical(b$lines$depth, c(0L, 1L, 1L, 0L, 0L)))

  # the statement-start guard: "EXIT FOR" must not open a phantom block
  e <- find_block_structure(tokenize_qlik(paste(
    "FOR i = 1 to 3", "EXIT FOR WHEN i > 2;", "NEXT", sep = "\n")))
  ok("EXIT FOR does not open a block",
     max(e$lines$depth) == 1L && length(e$warnings) == 0L)

  # DESIGN 1.6: the function IF( is not the statement IF ... THEN
  f <- find_block_structure(tokenize_qlik("[T]:\nLOAD IF(a, 1, 0) AS [X] FROM y;"))
  ok("function IF( opens no block",
     all(f$lines$depth == 0L) && length(f$warnings) == 0L)
  s <- find_block_structure(tokenize_qlik(paste(
    "IF a = 1 THEN", "LET b = 2;", "END IF", sep = "\n")))
  ok("statement IF ... THEN does open a block",
     identical(s$lines$depth, c(0L, 1L, 0L)) && length(s$warnings) == 0L)

  # unbalanced blocks are reported, not silently mis-indented
  ok("an unclosed block warns",
     length(find_block_structure(tokenize_qlik("FOR i = 1 to 3\nLET a = 1;"))$warnings) == 1L)
  ok("a closer with nothing open warns",
     length(find_block_structure(tokenize_qlik("NEXT\nLET a = 1;"))$warnings) == 1L)

  # a ';' alone on its own line (real case, [Grant Managing Region].txt:11)
  # must NOT look like it starts a new statement - it closes the one before
  # it. Bug found and fixed 2026-08-17 while building the layout pass: the
  # semicolon's own line was incorrectly getting a fresh stmt_id.
  semi_own_line <- find_block_structure(tokenize_qlik(
    "[T]:\nLOAD [A]\n;\n[U]:\nLOAD [B];"))$lines
  ok("a ';' alone on its own line stays part of the statement it closes",
     identical(semi_own_line$stmt_id, c(1L, 1L, 1L, 2L, 2L)))

  # app-unbuilt/script.qvs line 1 is "I///$tab 00-Main" - confirmed correct
  # data (Adam checked the source, 2026-08-17), so the WHOLE line is
  # protected as one section line, silently, not flagged as a defect
  ok("a line sharing a ///$tab marker is protected as one section line",
     identical(kinds("X///$tab Main\nTRACE a = 1;"), c("section", "statement")) &&
       length(find_block_structure(tokenize_qlik("X///$tab Main\nTRACE a = 1;"))$warnings) == 0L)
  ok("a ///$tab marker on its own line is also a section",
     identical(kinds("///$tab Main\nTRACE a = 1;"), c("section", "statement")))

  # DESIGN 4.5/4.8 (Adam 2026-08-17): SET/LET is a distinct "directive" kind,
  # not an ordinary "statement" - including a continuation line of a
  # multi-line SET/LET expression.
  ok("SET/LET is the directive kind, including its own continuation lines",
     identical(kinds("SET a = 1;\nLET b =\n  2;"),
               c("directive", "directive", "directive")))
  ok("a bare word that merely starts with set/let is not mistaken for one",
     identical(kinds("Settings a = 1;"), "statement"))

  invisible(NULL)
}

# ---- vertical layout pass -----------------------------------------------

verify_vertical_layout <- function() {
  section("self-test: enforce_vertical_layout")

  lay <- function(src) paste(detokenize(enforce_vertical_layout(tokenize_qlik(src))$tokens),
                              collapse = "\n")

  # DESIGN 4.5: statement 1 tab, field 2 (+ a 2-space pad on the true first
  # field), continuation 3, comment column 0 - and FLAT: a LOAD inside a FOR
  # gets the SAME indent as a top-level one.
  src <- paste("[T]:", "LOAD", "  [A] AS [A],", "  IF(x > 1, 'y',", "  IF(z,'a','b')) AS [C]",
               "FROM x;", "//a comment", "FOR i = 1 to 3", "LOAD [B] FROM y;", "NEXT", sep = "\n")
  out <- lay(src)
  # "IF(x > 1, 'y'," is a NEW field (it follows field 1's comma) - 2 tabs, no
  # pad (only the true first field, [A], gets the pad). Only
  # "IF(z,'a','b')) AS [C]" is a continuation of THAT field - 3 tabs.
  ok("statement/field/continuation/comment indent, first-field pad, and flat block nesting",
     identical(out, paste(
       "\t[T]:", "\tLOAD", "\t\t  [A] AS [A],", "\t\tIF(x > 1, 'y',",
       "\t\t\tIF(z,'a','b')) AS [C]", "\tFROM x;", "\n\n//a comment", "\tFOR i = 1 to 3",
       "\tLOAD [B] FROM y;", "\tNEXT", sep = "\n")))

  # DESIGN 4.8: exactly 2 blank lines between top-level statements, 0 inside
  # a block - the closing NEXT included, at the SAME id as what it closes.
  # TRACE (not SET/LET) so this exercises the general rule, not the
  # directive exemption tested separately below.
  b <- lay("FOR i=1 to 3\nTRACE a=1;\nTRACE b=2;\nNEXT\nTRACE c=3;")
  ok("2 blank lines between top-level statements, 0 inside a block",
     identical(b, paste("\tFOR i=1 to 3", "\tTRACE a=1;", "\tTRACE b=2;", "\tNEXT",
                        "\n\n\tTRACE c=3;", sep = "\n")))

  # a ';' alone on its own line now joins onto whatever precedes it - tight,
  # no space - instead of staying on its own indented line (DESIGN §4.9
  # extension, Adam 2026-08-17, overriding the original design tested here).
  # It must still not spawn a phantom 2-blank-line gap before the statement
  # that follows it.
  s <- lay("[T]:\nLOAD [A]\nFROM x\n;\n[U]:\nLOAD [B];")
  ok("a lone ';' joins onto the line that precedes it, no blank line before it",
     identical(s, paste("\t[T]:", "\tLOAD [A]", "\tFROM x;",
                        "\n\n\t[U]:", "\tLOAD [B];", sep = "\n")))

  # a leading comment is glued to the statement it describes: the 2-blank-
  # line gap goes ABOVE the comment, not between the comment and the code
  cm <- lay("TRACE a=1;\n//about b\nTRACE b=2;")
  ok("a leading comment stays glued to its statement (convention, confirmed by Adam 2026-08-17)",
     identical(cm, paste("\tTRACE a=1;", "\n\n//about b", "\tTRACE b=2;", sep = "\n")))

  # ///$tab section markers: the WHOLE line carrying one is left completely
  # alone, even when something shares the line with it. The line RIGHT
  # AFTER a section now gets its indentation fixed like any other line
  # (Adam 2026-08-17 - a table label directly after a section was wrongly
  # left at column 0); only the blank-line COUNT of that gap - here, zero -
  # is left exactly as authored rather than normalised to two.
  sec <- lay("TRACE a=1;\nX///$tab Main\nTRACE b=2;")
  ok("a ///$tab marker's own line is untouched; the line after gets indent fixed, blanks preserved",
     identical(sec, "\tTRACE a=1;\nX///$tab Main\n\tTRACE b=2;"))

  # DESIGN 4.5/4.8 (Adam 2026-08-17): SET/LET gets 0 indent, and the blank-line
  # count on EITHER side of one is left exactly as authored - never forced
  # to the standard two, never collapsed to zero.
  dv <- lay("TRACE a=1;\n\n\nSET b=2;\nLET c=3;\n\nTRACE d=4;")
  ok("SET/LET: 0 indent, blank-line count untouched on both sides",
     identical(dv, "\tTRACE a=1;\n\n\nSET b=2;\nLET c=3;\n\n\tTRACE d=4;"))

  # file start: no leading WS token to rewrite at all (real case - both
  # fixtures start this way) - must indent line 1 without corrupting
  # everything after it (the bug found and fixed while building this pass)
  fs <- lay("[T]:\nLOAD [A] FROM x;")
  ok("file with no leading whitespace gets line 1 indented without corruption",
     identical(fs, "\t[T]:\n\tLOAD [A] FROM x;"))

  # idempotency and the only-WS-changes invariant, on a synthetic stream
  # with everything: blocks, comments, a lone ';', a section, no leading WS
  big <- paste("[T]:", "LOAD [A] FROM x\n;", "//about U", "FOR i=1 to 3",
               "LET a=1;", "NEXT", "X///$tab Main", "[U]:", "LOAD [B];", sep = "\n")
  r1 <- enforce_vertical_layout(tokenize_qlik(big))
  r2 <- enforce_vertical_layout(r1$tokens)
  ok("idempotent on a stream exercising every rule at once", nrow(r2$changes) == 0L)

  # DESIGN 4.9 (Adam 2026-08-17): the format spec's opening paren joins the
  # FROM path on one line, one space between, even though the source has it
  # wrapped onto its own line (Qlik's own default export shape).
  fr <- lay("[T]:\nLOAD [A]\nFROM x\n(qvd);")
  ok("FROM clause: format spec joins the path on one line",
     identical(fr, "\t[T]:\n\tLOAD [A]\n\tFROM x (qvd);"))

  invisible(NULL)
}

# ---- alias alignment pass -----------------------------------------------

verify_alias_alignment <- function() {
  section("self-test: enforce_alias_alignment")

  align <- function(src) paste(detokenize(enforce_alias_alignment(tokenize_qlik(src))$tokens),
                                collapse = "\n")

  # DESIGN 4.6: every AS in a block aligns to one column - the smallest tab
  # stop (width 4) strictly past the widest field, TABS only (Adam
  # 2026-08-17, non-negotiable) inserted BEFORE the existing single space
  # before AS, not instead of it (Adam 2026-08-17) - AS lands one column
  # past the tab stop, not on it. Field 1 ends col 13 (2 tabs + 2-space pad
  # + "[A]"), field 2 ends col 14 (2 tabs + ", " + "[BB]") - both round up
  # to tab stop 16, one tab each.
  a1 <- align("[T]:\n\tLOAD\n\t\t  [A] AS [A]\n\t\t, [BB] AS [BB]\n\tFROM x;")
  ok("two fields of different width land their AS on the same tab stop",
     identical(a1, "[T]:\n\tLOAD\n\t\t  [A]\t AS [A]\n\t\t, [BB]\t AS [BB]\n\tFROM x;"))

  # a field whose expression wraps onto another line before AS is now
  # measured from the line AS itself sits on and CAN widen the block's
  # shared column (Adam 2026-08-17, overriding the original design tested
  # here). [A]'s own column is 13 (2 tabs + 2-space pad + "[A]"); the
  # wrapped field's continuation line is 3 tabs (12) + "'y', 'n')" (9) = 21.
  # max(13, 21) = 21 -> next multiple of 4 strictly greater = 24. [A] needs
  # 3 tabs to reach 24 from 13; the wrapped field needs 1 tab from 21.
  a2 <- align(paste0(
    "[T]:\n\tLOAD\n\t\t  [A] AS [A]\n\t\t, IF(x > 1,\n\t\t\t'y', 'n') AS [LongOne]\n\tFROM x;"))
  ok("a wrapped field is measured from AS's own line and can widen the column",
     identical(a2, paste0(
       "[T]:\n\tLOAD\n\t\t  [A]\t\t\t AS [A]\n\t\t, IF(x > 1,\n\t\t\t'y', 'n')\t AS [LongOne]\n\tFROM x;")))

  # every rewritten gap is tabs followed by the one preserved space, never a
  # bare space and never tabs that swallow the space
  r <- enforce_alias_alignment(tokenize_qlik(
    "[T]:\n\tLOAD\n\t\t  [A] AS [A]\n\t\t, [BB] AS [BB]\n\tFROM x;"))
  ok("every change is tabs plus the preserved single space before AS",
     nrow(r$changes) > 0L && all(grepl("^\t+ $", r$changes$after)))

  # a second run of an already-aligned block is a no-op
  again <- enforce_alias_alignment(r$tokens)
  ok("idempotent once aligned", nrow(again$changes) == 0L)

  # two separate LOAD blocks get independent columns - one enormous field in
  # block 1 must not widen block 2's short field (DESIGN 4.6)
  two_blocks <- align(paste0(
    "[T1]:\n\tLOAD\n\t\t  [ReallyLongFieldName] AS [ReallyLongFieldName]\n\tFROM x;\n\n\n",
    "[T2]:\n\tLOAD\n\t\t  [A] AS [A]\n\tFROM y;"))
  ok("two LOAD blocks align independently",
     grepl("\\[T2\\]:\\n\\tLOAD\\n\\t\\t  \\[A\\]\\t AS \\[A\\]", two_blocks))

  invisible(NULL)
}

# ---- runner ------------------------------------------------------------

main <- function() {
  cat("Style pipeline verification\n")

  verify_detects_corruption()
  verify_block_structure()
  verify_vertical_layout()
  verify_alias_alignment()

  fixtures <- c("[Grant Managing Region].txt", "app-unbuilt/script.qvs")
  for (f in fixtures) if (file.exists(f)) verify_file(f) else
    cat("\n(skipping missing fixture: ", f, ")\n", sep = "")

  cat(sprintf("\n%d checks, %d failed\n", .checks, .fails))
  if (.fails > 0L) quit(status = 1L) else cat("ALL PASS\n")
}

# Stage gate (Adam, 2026-08-17): main() reads both stage-3 fixtures, so the
# rule "verify.R is stage 3 only" is enforced HERE, at the point of action,
# not by prose. The flag makes the invocation itself state the claim.
if (!interactive()) {
  if (!"--stage3" %in% commandArgs(trailingOnly = TRUE)) {
    cat("verify.R runs at TESTING STAGE 3 ONLY (it reads both large fixtures).\n",
        "If Adam has signed off stages 1 and 2, rerun with:  Rscript verify.R --stage3\n",
        "For doc consistency at any stage, use verify_docs.R instead.\n", sep = "")
    quit(status = 1L)
  }
  main()
}
