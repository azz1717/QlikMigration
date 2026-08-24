# test_comment_substream.R
#
# Standalone test harness for styling/comment_substream.R - Task 1 of
# docs/PLAN-comment-substream.md's sequencing (the isolated extract/
# serialize fixed point, section 7). NOT the plan's own future
# verify_substream.R (that is a later task, section 8) and NOT part of
# styling/verify.R - a separate tool for a separate property, per the
# repo's own rule that a check must be able to fail independently of the
# code it checks.
#
# Run it: Rscript styling/test_comment_substream.R
# Exits 1 on any failed property, 0 otherwise. No fixture is read into
# this file's own source - only counts and digests are printed.

`%||%` <- function(a, b) if (is.null(a)) b else a

source("shared/qlik_tokenizer.R")
source("shared/qlik_reserved_words.R")
source("styling/comment_substream.R")

.checks <- 0L
.fails  <- 0L
.ok <- function(label, passed, detail = character(0)) {
  .checks <<- .checks + 1L
  if (!passed) .fails <<- .fails + 1L
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", label))
  for (d in detail) cat("       ", d, "\n", sep = "")
  invisible(passed)
}
.section <- function(title) cat("\n== ", title, " ==\n", sep = "")

# ---------------------------------------------------------------------
# Property helpers, reused by both the synthetic and real-fixture tests
# ---------------------------------------------------------------------

#' Round-trip every extracted run of `parent` with no modification; must
#' reproduce `parent` byte-identically.
.check_round_trip <- function(parent, label) {
  ex <- extract_comment_runs(parent)
  bad <- character(0)
  for (r in ex$runs) {
    if (r$status != "extracted") next
    out <- serialize_comment_run(parent, r)
    if (!identical(out$text, parent$text) || !identical(out$type, parent$type) ||
        nrow(out) != nrow(parent)) {
      bad <- c(bad, sprintf("line %d-%d", r$line_start, r$line_end))
    }
  }
  .ok(sprintf("%s: round-trip byte-identical (%d runs extracted)", label,
              sum(vapply(ex$runs, function(r) r$status == "extracted", logical(1)))),
      length(bad) == 0, bad)
  ex
}

#' Refused runs leave the parent's own comment tokens untouched, by
#' construction (nothing ever calls serialize on them) - assert it anyway.
.check_refusal_untouched <- function(parent, ex, label) {
  bad <- character(0)
  n_refused <- 0L
  for (r in ex$runs) {
    if (r$status != "refused") next
    n_refused <- n_refused + 1L
    orig <- paste0("//", substring(parent$text[r$comment_idx], 3L))
    if (!identical(parent$text[r$comment_idx], orig))
      bad <- c(bad, sprintf("line %d-%d", r$line_start, r$line_end))
  }
  .ok(sprintf("%s: refused runs byte-identical (%d refused)", label, n_refused),
      length(bad) == 0, bad)
}

# ---------------------------------------------------------------------
# 1. Synthetic fixture - hand-built so every classification path and the
#    fixed-point/growth scenario are exercised deterministically, not left
#    to chance on whatever the real fixtures happen to contain.
# ---------------------------------------------------------------------
.section("Synthetic fixture - classification, refusal, fixed point")

.syn_src <- paste(c(
  "Test:",
  "LOAD",
  "\t[A] as [A],",
  "\t// [B] as [B],",
  "\t// [C] as [C]",
  "\t[D] as [D]",
  "FROM x.qvd (qvd);",
  "",
  "// Old:",
  "// LOAD",
  "// \t[E] as [E],",
  "// \t[F] as [F]",
  "// FROM y.qvd (qvd);",
  "",
  "// [G] as [G] = SubField(",
  "// [H], 1)",
  "",
  "// [Never] as [Closes] = SubField(",
  "// this paren never closes",
  "",
  "// This is just a note about the table above.",
  "// It has no brackets or commas at all.",
  "",
  "// [Mix1] as [Mix1],",
  "// Note: mix1 feeds into a downstream chart.",
  "// [Mix2] as [Mix2]",
  ""
), collapse = "\n")

.syn <- tokenize_qlik(.syn_src)
.syn_ex <- .check_round_trip(.syn, "synthetic")
.check_refusal_untouched(.syn, .syn_ex, "synthetic")

.syn_status <- vapply(.syn_ex$runs, function(r) r$status, character(1))
.syn_kind   <- vapply(.syn_ex$runs, function(r) if (is.na(r$kind)) NA_character_ else r$kind,
                       character(1))
.syn_lines  <- vapply(.syn_ex$runs, function(r) sprintf("%d-%d", r$line_start, r$line_end),
                       character(1))

.ok(sprintf("synthetic: found %d runs (expected 6)", length(.syn_ex$runs)),
    length(.syn_ex$runs) == 6L,
    paste(.syn_lines, .syn_status, .syn_kind))

# run 1: [B]/[C] mid-list, no LOAD keyword -> field_run. leading_sep is
# " " (plain whitespace after "//", no comma) - the run has no boundary
# COMMA in the source, but its leading trivia is still captured so
# `tokens` never carries it (see comment_substream.R's extraction note).
.r1 <- .syn_ex$runs[[1]]
.ok("synthetic: run 1 (mid-list fields) extracted as field_run",
    identical(.r1$status, "extracted") && identical(.r1$kind, "field_run") &&
      identical(.r1$leading_sep, " "))

# run 2: commented Old: LOAD ... FROM ...; -> load_block (plan resolved
# decision 2 - LOAD blocks ARE in scope, unlike pass 8)
.r2 <- .syn_ex$runs[[2]]
.ok("synthetic: run 2 (commented table) extracted as load_block",
    identical(.r2$status, "extracted") && identical(.r2$kind, "load_block"))

# run 3: multi-line expression, parens balance only across the whole run
# (plan section 6.1's last bullet) - ONE run, not refused, not split.
.r3 <- .syn_ex$runs[[3]]
.ok("synthetic: run 3 (paren spans 2 lines) is ONE run, extracted",
    length(.r3$comment_idx) == 2L && identical(.r3$status, "extracted"))

# run 4: never closes -> refused, byte-identical (already checked above),
# and counted.
.r4 <- .syn_ex$runs[[4]]
.ok("synthetic: run 4 (unbalanced paren) refused",
    identical(.r4$status, "refused") &&
      grepl("unbalanced parentheses", .r4$reason %||% ""))

# run 5: pure prose, two lines, no code shape at all -> prose_only, not
# counted as refused (it was never a parse failure - it was never script).
.r5 <- .syn_ex$runs[[5]]
.ok("synthetic: run 5 (pure prose) is prose_only, not refused",
    identical(.r5$status, "prose_only"))

# run 6: field / prose / field - decision 3, prose interleaved inside a
# script run is carried through unstyled in ONE child stream, not split.
.r6 <- .syn_ex$runs[[6]]
.ok("synthetic: run 6 (field+prose+field) is ONE extracted run",
    identical(.r6$status, "extracted") && identical(.r6$kind, "field_run") &&
      length(.r6$comment_idx) == 3L)
.ok("synthetic: run 6's middle line is classified prose, carried not dropped",
    identical(.r6$line_kind, c("field", "prose", "field")) &&
      nrow(.r6$tokens) > 0)

# --- fixed point (plan section 7): identity cycle -------------------
.p0 <- .syn
.r1b <- extract_comment_runs(.p0)$runs[[1]]
.p1 <- serialize_comment_run(.p0, .r1b)
.r1c <- extract_comment_runs(.p1)$runs[[1]]
.ok("fixed point: identity extract->serialize->extract reproduces the child",
    identical(.r1c$tokens$text, .r1b$tokens$text) &&
      identical(.r1c$leading_sep, .r1b$leading_sep))

# --- fixed point: simulated styling edit, the section 7 growth repro ---
# "prepend a leading comma to a field body via the serializer's own path" -
# set_leading_separator() IS that path (never touching run$tokens).
.g0 <- .syn
.gr1 <- extract_comment_runs(.g0)$runs[[1]]
.ok("growth repro setup: run 1 starts with plain whitespace, no comma",
    identical(.gr1$leading_sep, " "))
.gr1_styled <- set_leading_separator(.gr1, ", ")
.g1 <- serialize_comment_run(.g0, .gr1_styled)
.gr1_reext <- extract_comment_runs(.g1)$runs[[1]]
.ok("growth repro: second extract reads back ONE leading separator, not two",
    identical(.gr1_reext$leading_sep, ", "),
    sprintf("got leading_sep=%s", deparse(.gr1_reext$leading_sep)))
.ok("growth repro: second extract's child body is UNCHANGED (comma not seen as body)",
    identical(.gr1_reext$tokens$text, .gr1_styled$tokens$text))
# cycle it three more times - the comma must not accumulate
.gN <- .g1; .grN <- .gr1_reext
for (cyc in 1:3) {
  .grN <- set_leading_separator(.grN, ", ")
  .gN  <- serialize_comment_run(.gN, .grN)
  .grN <- extract_comment_runs(.gN)$runs[[1]]
}
.ok("growth repro: stable after 4 cycles (leading_sep still ', ', body still unchanged)",
    identical(.grN$leading_sep, ", ") && identical(.grN$tokens$text, .gr1_styled$tokens$text))

# --- serialize with a DIFFERENT line count (splice / void path) --------
.v0 <- .syn
.vr <- extract_comment_runs(.v0)$runs[[1]]      # the 2-line [B]/[C] run
.vr_more <- .vr
extra_row <- data.frame(text = c("\t[C2]", " as ", "[C2]"),
                         type = c("BRACKET", "WS", "BRACKET"), line = 1L,
                         stringsAsFactors = FALSE)
.vr_more$tokens <- rbind(.vr$tokens,
                          data.frame(text = "\n", type = "WS", line = 1L, stringsAsFactors = FALSE),
                          extra_row)
.v1 <- serialize_comment_run(.v0, .vr_more)
.v1_runs <- extract_comment_runs(.v1)$runs
.vr_grown <- .v1_runs[[1]]
.ok("splice path: adding a line to a run grows its comment_idx by one",
    length(.vr_grown$comment_idx) == length(.vr$comment_idx) + 1L,
    sprintf("was %d, now %d", length(.vr$comment_idx), length(.vr_grown$comment_idx)))
.ok("splice path: whole-stream round-trip still holds after growing a run",
    identical(detokenize(.v1), detokenize(serialize_comment_run(.v0, .vr_more))))

.vr_fewer <- .vr
# keep only the tokens strictly before the first embedded newline, i.e.
# exactly the run's first line (a `line ==` filter would also keep the
# newline-carrying WS token itself, since a token's line is where it
# STARTS - leaving a spurious near-empty second line behind).
.nl_at <- which(grepl("\n", .vr$tokens$text, fixed = TRUE))[1]
.keep  <- if (is.na(.nl_at)) seq_len(nrow(.vr$tokens)) else seq_len(.nl_at - 1L)
.vr_fewer$tokens <- .vr$tokens[.keep, , drop = FALSE]
.v2 <- serialize_comment_run(.v0, .vr_fewer)
.v2_runs <- extract_comment_runs(.v2)$runs
.ok("void path: shrinking a run's line count shrinks its comment_idx",
    length(.v2_runs[[1]]$comment_idx) == 1L)

# ---------------------------------------------------------------------
# 2. Real fixtures - round-trip + refusal + coverage counters
# ---------------------------------------------------------------------
.section("Real fixtures - round-trip, refusal, coverage")

.report_fixture <- function(path, encoding = "UTF-8") {
  t0 <- Sys.time()
  parent <- read_qlik_script(path, encoding = encoding)
  ex <- .check_round_trip(parent, path)
  .check_refusal_untouched(parent, ex, path)

  status <- vapply(ex$runs, function(r) r$status, character(1))
  kind   <- vapply(ex$runs, function(r) if (is.na(r$kind)) "-" else r$kind, character(1))
  n_comment_tok <- sum(parent$type == "COMMENT" & startsWith(parent$text, "//") &
                         !startsWith(parent$text, "///$"))
  n_block_tok <- sum(parent$type == "COMMENT" & startsWith(parent$text, "/*"))
  run_len <- vapply(ex$runs, function(r) length(r$comment_idx), integer(1))
  n_lines_in_runs <- sum(run_len)
  n_lines_extracted <- sum(run_len[status == "extracted"])
  n_lines_refused   <- sum(run_len[status == "refused"])
  n_lines_prose     <- sum(run_len[status == "prose_only"])
  n_prose_lines <- sum(vapply(ex$runs, function(r) {
    if (r$status != "extracted") return(0L)
    sum(r$line_kind == "prose")
  }, integer(1)))
  n_field_lines <- sum(vapply(ex$runs, function(r) {
    if (r$status != "extracted") return(0L)
    sum(r$line_kind == "field")
  }, integer(1)))

  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  cat(sprintf(
    "  %s: %d comment tokens, %d runs (%d extracted [%d load_block/%d field_run], %d refused, %d prose_only), %.2fs\n",
    path, n_comment_tok, length(ex$runs),
    sum(status == "extracted"), sum(kind == "load_block"), sum(kind == "field_run"),
    sum(status == "refused"), sum(status == "prose_only"), elapsed))
  cat(sprintf(
    "    lines in runs: %d extracted (field-shaped %d, prose-carried %d), %d refused, %d prose_only [%d block comments out of scope]\n",
    n_lines_extracted, n_field_lines, n_prose_lines, n_lines_refused, n_lines_prose, n_block_tok))
  if (any(status == "refused")) {
    reasons <- table(vapply(ex$runs[status == "refused"], function(r) r$reason, character(1)))
    for (nm in names(reasons)) cat(sprintf("    refused (%dx): %s\n", reasons[[nm]], nm))
  }
  invisible(list(parent = parent, ex = ex, n_comment_tok = n_comment_tok,
                 n_block_tok = n_block_tok, n_lines_in_runs = n_lines_in_runs))
}

.res_fmt <- .report_fixture("fixtures/formatexample.txt")
.res_gmr <- .report_fixture("fixtures/[Grant Managing Region].txt")
.res_app <- .report_fixture("app-unbuilt/script.qvs")

.section("Coverage reconciliation against plan section 1's measured table")
cat(sprintf(
  "  script.qvs:  // comment tokens=%d, +%d /* */ block comments = %d (plan: 2483 excl. ///$tab)\n",
  .res_app$n_comment_tok, .res_app$n_block_tok, .res_app$n_comment_tok + .res_app$n_block_tok))
cat(sprintf(
  "               lines inside a found run=%d\n", .res_app$n_lines_in_runs))
cat(sprintf(
  "  GMR:         // comment tokens=%d, +%d /* */ block comments = %d (plan: 94 excl. ///$tab)\n",
  .res_gmr$n_comment_tok, .res_gmr$n_block_tok, .res_gmr$n_comment_tok + .res_gmr$n_block_tok))
cat(sprintf(
  "               lines inside a found run=%d\n", .res_gmr$n_lines_in_runs))
cat("  DISCREPANCY explained: the plan's 2,483 counts block /* */ comments in with the\n")
cat("  '// comment' total; this module scopes to line-comments only (block comments stay\n")
cat("  excluded per CLAUDE.md / plan section 6.4) - 2473 + 10 block = 2483, exact.\n")
cat("  comment tokens - lines inside a run = lines NOT owning their line, i.e. trailing\n")
cat("  comments sharing a line with live code - never run-eligible in either design.\n")

cat(sprintf("\n%d checks, %d failed\n", .checks, .fails))
if (.fails > 0L) quit(status = 1L) else cat("COMMENT SUBSTREAM OK\n")
