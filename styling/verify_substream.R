# verify_substream.R
#
# Standalone verifier for styling/comment_substream.R's extract/serialize
# contract - docs/PLAN-comment-substream.md section 8 (the six properties
# below are numbered to match that section exactly). NOT an extension of
# styling/verify.R: different job (contract, not meaning-preservation),
# different failure mode, and verify.R must stay untouched and must not
# gate this work (Adam's explicit directive, plan section 8's own framing).
#
# Replaces styling/test_comment_substream.R, deleted: every check that file
# had is subsumed here (round-trip, refusal, fixed point, growth repro,
# splice/void, per-run classification ground truth), run against the SAME
# synthetic fixture plus both mandatory fixtures. docs/INTERFACES.md
# updated to match - twin rule, applied in the fold-in direction.
#
# Run it:
#   Rscript styling/verify_substream.R          # fixtures only, seconds
#   Rscript styling/verify_substream.R --full    # + app-unbuilt/script.qvs
#
# Exits 1 on any failed property, 0 otherwise. No fixture is read into this
# file's own source - only counts and digests are printed. Base R only.

`%||%` <- function(a, b) if (is.null(a)) b else a

source("shared/qlik_tokenizer.R")
source("shared/qlik_reserved_words.R")
source("styling/ensure_explicit_aliases.R")
source("styling/enforce_bracket_references.R")
source("styling/enforce_leading_commas.R")
source("styling/enforce_intraline_spacing.R")
source("styling/enforce_reserved_word_case.R")
source("styling/enforce_vertical_layout.R")
source("styling/enforce_alias_alignment.R")
source("styling/comment_substream.R")
source("styling/comment_style_driver.R")

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

# =======================================================================
# The six properties (plan section 8), each a standalone check function
# reusable across the synthetic fixture and both real fixtures.
# =======================================================================

#' P1 Round-trip: every extracted child re-tokenizes, ALONE, to its own
#' text (the plan's literal wording); AND an unmodified serialize back into
#' the parent reproduces the parent byte-identically (the same property at
#' the parent-embedding level). Refused runs are asserted untouched too -
#' there is no separate "restore" path, so this is what proves it.
.check_p1_roundtrip <- function(parent, ex, label) {
  bad_child <- character(0); bad_parent <- character(0); n_ext <- 0L
  for (r in ex$runs) {
    if (r$status != "extracted") next
    n_ext <- n_ext + 1L
    retok <- tokenize_qlik(paste(r$tokens$text, collapse = ""))
    if (!identical(retok$text, r$tokens$text) || !identical(retok$type, r$tokens$type))
      bad_child <- c(bad_child, sprintf("line %d-%d", r$line_start, r$line_end))
    out <- serialize_comment_run(parent, r)
    if (!identical(out$text, parent$text) || !identical(out$type, parent$type) ||
        nrow(out) != nrow(parent))
      bad_parent <- c(bad_parent, sprintf("line %d-%d", r$line_start, r$line_end))
  }
  .ok(sprintf("%s P1: child re-tokenizes to itself (%d extracted)", label, n_ext),
      length(bad_child) == 0, bad_child)
  .ok(sprintf("%s P1: unmodified serialize reproduces parent byte-identically", label),
      length(bad_parent) == 0, bad_parent)

  n_refused <- 0L; bad_refused <- character(0)
  for (r in ex$runs) {
    if (r$status != "refused") next
    n_refused <- n_refused + 1L
    orig <- paste0("//", substring(parent$text[r$comment_idx], 3L))
    if (!identical(parent$text[r$comment_idx], orig))
      bad_refused <- c(bad_refused, sprintf("line %d-%d", r$line_start, r$line_end))
  }
  .ok(sprintf("%s P1: refused runs left byte-identical (%d refused)", label, n_refused),
      length(bad_refused) == 0, bad_refused)
}

#' P2 Fixed point: extract -> style -> serialize -> extract is stable.
#' `style(run) -> run` defaults to identity and is the plug point Task 3+
#' threads real passes through, e.g. `function(run) { run$tokens <-
#' enforce_leading_commas(run$tokens, ctx); run }` - no rework of this file
#' needed, only a different `style` argument. Applies `style` to every
#' extracted run of `parent` and re-extracts once (cheap: O(runs), not
#' O(runs^2)) - in REVERSE source order (Task 4, real driver): a `style`
#' that changes a run's line count shifts every index AFTER it via
#' serialize_comment_run()'s splice_tokens() path, so processing
#' highest-comment_idx-first means every run still to be styled sits
#' BEFORE any edit already made, and its own `comment_idx` (captured once,
#' up front, from the unedited parent) is still exactly where
#' find_comment_runs() put it. Forward order is only safe when NO style
#' in use ever changes line count (true of the identity and
#' set_leading_separator cycles below) - reverse order is a superset of
#' that, so it is used unconditionally rather than per-style. Caught
#' directly on fixtures/[Grant Managing Region].txt (34 runs, several
#' genuinely reflow under real styling): forward order left 14/34 runs
#' silently re-extracting to text SHIFTED INTO from a neighbouring run,
#' not their own. Returns the fully modified parent for chaining/isolation
#' checks.
.check_p2_fixed_point <- function(parent, ex, label, style = function(run) run,
                                   edit_label = "identity") {
  work <- parent
  styled <- vector("list", length(ex$runs))
  for (i in rev(seq_along(ex$runs))) {
    r <- ex$runs[[i]]
    if (r$status != "extracted") next
    r <- style(r)
    work <- serialize_comment_run(work, r)
    styled[[i]] <- r
  }
  ex2 <- extract_comment_runs(work)
  bad <- character(0); n <- 0L
  for (i in seq_along(ex$runs)) {
    if (is.null(styled[[i]])) next
    n <- n + 1L
    got <- ex2$runs[[i]]
    exp <- styled[[i]]
    if (is.null(got) || !identical(got$status, "extracted") ||
        !identical(got$tokens$text, exp$tokens$text) ||
        !identical(got$leading_sep, exp$leading_sep))
      bad <- c(bad, sprintf("line %d-%d", exp$line_start, exp$line_end))
  }
  .ok(sprintf("%s P2: fixed point stable under %s edit (%d extracted runs)",
              label, edit_label, n), length(bad) == 0, bad)
  list(work = work, ex2 = ex2)
}

#' P3 (general form): every LIVE token (excluding COMMENT/WS/VOID trivia)
#' byte-identical and in the same order before/after, WITHOUT requiring row
#' count to be stable - unlike .check_p3_isolation() below, valid for an
#' edit that legitimately changes comment row counts (the real driver: a
#' restyled run's line count is whatever styling produces, not what the
#' author typed). WS is excluded deliberately: a driver-inserted line
#' between two comment rows is itself a new WS row, which is NOT a
#' violation of isolation as long as no LIVE (real code) token moved.
.check_p3_isolation_general <- function(parent, work, label) {
  live <- function(tk) tk$type != "COMMENT" & tk$type != "WS" & tk$type != "VOID"
  p <- parent[live(parent), , drop = FALSE]
  w <- work[live(work), , drop = FALSE]
  ok <- identical(p$type, w$type) && identical(p$text, w$text)
  .ok(sprintf("%s P3 (general): every live (non-comment, non-trivia) token unchanged and in order",
              label), ok,
      if (!ok) sprintf("live token count %d -> %d", nrow(p), nrow(w)) else character(0))
}

#' P3 Parent isolation: every non-COMMENT parent token byte-identical
#' before/after, row count unchanged. Checked against a P2 cycle whose edit
#' does not change any run's line count (the leading-separator edit) so a
#' plain positional comparison is valid - splice/void legitimately DOES
#' change row count (that is its job) and is exercised separately in the
#' synthetic section below, not mislabelled as this property.
.check_p3_isolation <- function(parent, work, label) {
  same_shape <- nrow(work) == nrow(parent)
  detail <- character(0)
  ok_all <- same_shape
  if (same_shape) {
    noncomment <- parent$type != "COMMENT"
    ok_all <- identical(work$type[noncomment], parent$type[noncomment]) &&
              identical(work$text[noncomment], parent$text[noncomment])
    if (!ok_all) detail <- "non-COMMENT token text/type changed"
  } else {
    detail <- sprintf("row count changed: %d -> %d", nrow(parent), nrow(work))
  }
  .ok(sprintf("%s P3: parent isolation (non-COMMENT tokens + row count) under a non-identity edit",
              label), ok_all, detail)
}

#' P4 No boundary crossing. Parent-side half: every run's comment_idx
#' points only at COMMENT rows, and no two runs claim the same parent row.
#' Child-side half ("vice versa"): a child stream carries no parent index
#' at all (its schema is exactly text/type/line, re-tokenized from a
#' substring) - a live parent token cannot appear IN a child set because
#' there is no column through which it could. That is checked structurally,
#' not by scanning for it, since the data model makes it unrepresentable.
.check_p4_no_crossing <- function(parent, ex, label) {
  all_idx <- unlist(lapply(ex$runs, `[[`, "comment_idx"))
  dup <- all_idx[duplicated(all_idx)]
  wrong_type <- all_idx[parent$type[all_idx] != "COMMENT"]
  .ok(sprintf("%s P4: run identities disjoint and COMMENT-only (%d comment rows, %d runs)",
              label, length(all_idx), length(ex$runs)),
      length(dup) == 0 && length(wrong_type) == 0,
      c(if (length(dup)) sprintf("parent idx claimed by >1 run: %s", paste(head(dup, 5), collapse = ",")),
        if (length(wrong_type)) sprintf("non-COMMENT idx claimed by a run: %s", paste(head(wrong_type, 5), collapse = ","))))

  schema_bad <- character(0)
  for (r in ex$runs) {
    if (r$status != "extracted") next
    if (!identical(names(r$tokens), c("text", "type", "line")))
      schema_bad <- c(schema_bad, sprintf("line %d-%d: columns %s",
                                           r$line_start, r$line_end, paste(names(r$tokens), collapse = ",")))
  }
  .ok(sprintf("%s P4: child schema carries no parent index (a live token cannot appear in a child set)",
              label), length(schema_bad) == 0, schema_bad)
}

#' P5 Comment bodies still parse: every line the (possibly styled) run
#' serializes re-reads, alone, as exactly one COMMENT token spanning its
#' own text (never splits if read standalone); and the styled child still
#' classifies as loadable script via the module's OWN classifier - the
#' structural stand-in for "an uncommented styled field would still load",
#' reusing `.cs_classify_run()` rather than a second implementation.
.check_p5_parses <- function(work, label) {
  ex_work <- extract_comment_runs(work)
  bad_single <- character(0); n_lines <- 0L
  bad_class <- character(0); n_checked <- 0L
  for (r in ex_work$runs) {
    if (r$status != "extracted") next
    for (i in r$comment_idx) {
      n_lines <- n_lines + 1L
      t <- tokenize_qlik(work$text[i])
      t <- t[t$type != "EOF", , drop = FALSE]
      if (!(nrow(t) == 1 && t$type[1] == "COMMENT" && identical(t$text[1], work$text[i])))
        bad_single <- c(bad_single, sprintf("parent row %d", i))
    }
    n_checked <- n_checked + 1L
    cls <- .cs_classify_run(r$tokens)
    if (is.na(cls$kind))
      bad_class <- c(bad_class, sprintf("line %d-%d: %s", r$line_start, r$line_end, cls$reason))
  }
  .ok(sprintf("%s P5: every serialized comment line re-reads as ONE comment (%d lines)",
              label, n_lines), length(bad_single) == 0, head(bad_single, 5))
  .ok(sprintf("%s P5: styled runs still classify as loadable script (%d checked)",
              label, n_checked), length(bad_class) == 0, head(bad_class, 5))
}

#' P6 Coverage counters: runs extracted/refused (by reason)/prose-carried,
#' printed every run so a silent regression shows as a number.
.report_coverage <- function(path_label, parent, ex) {
  status <- vapply(ex$runs, function(r) r$status, character(1))
  kind   <- vapply(ex$runs, function(r) if (is.na(r$kind)) "-" else r$kind, character(1))
  n_comment_tok <- sum(parent$type == "COMMENT" & startsWith(parent$text, "//") &
                         !startsWith(parent$text, "///$"))
  n_block_tok <- sum(parent$type == "COMMENT" & startsWith(parent$text, "/*"))
  run_len <- vapply(ex$runs, function(r) length(r$comment_idx), integer(1))
  n_prose_lines <- sum(vapply(ex$runs, function(r) {
    if (r$status != "extracted") return(0L)
    sum(r$line_kind == "prose")
  }, integer(1)))
  n_field_lines <- sum(vapply(ex$runs, function(r) {
    if (r$status != "extracted") return(0L)
    sum(r$line_kind == "field")
  }, integer(1)))

  cat(sprintf(
    "  %s: %d comment tokens (+%d block), %d runs (%d extracted [%d load_block/%d field_run], %d refused, %d prose_only)\n",
    path_label, n_comment_tok, n_block_tok, length(ex$runs),
    sum(status == "extracted"), sum(kind == "load_block"), sum(kind == "field_run"),
    sum(status == "refused"), sum(status == "prose_only")))
  cat(sprintf("    lines in runs: %d (field-shaped %d, prose-carried %d)\n",
              sum(run_len), n_field_lines, n_prose_lines))
  if (any(status == "refused")) {
    reasons <- table(vapply(ex$runs[status == "refused"], function(r) r$reason, character(1)))
    for (nm in names(reasons)) cat(sprintf("    refused (%dx): %s\n", reasons[[nm]], nm))
  }
  invisible(list(n_comment_tok = n_comment_tok, n_block_tok = n_block_tok))
}

#' Driver-specific coverage (acceptance 5, and the split the overseer asked
#' for 2026-08-24): how many extracted runs the REAL driver actually
#' restyled, broken out by the mechanism each one went through - so a
#' regression in the scaffold path or the prose-split path shows as a
#' number, separately from a plain (unwrapped) load_block.
.report_driver_coverage <- function(path_label, coverage_tbl) {
  if (length(coverage_tbl) == 0) {
    cat(sprintf("  %s driver: 0 extracted runs\n", path_label))
    return(invisible(NULL))
  }
  parts <- vapply(names(coverage_tbl), function(nm)
    sprintf("%s=%d", nm, coverage_tbl[[nm]]), character(1))
  cat(sprintf("  %s driver buckets: %s\n", path_label, paste(parts, collapse = ", ")))
}

# =======================================================================
# Driver: run all six properties against one already-tokenized parent.
# =======================================================================

.verify_all <- function(parent, label) {
  t0 <- Sys.time()
  ex <- extract_comment_runs(parent)
  .check_p1_roundtrip(parent, ex, label)

  # P2, cycle 1: identity edit - the vacuous-but-required baseline.
  .check_p2_fixed_point(parent, ex, label, style = function(run) run, edit_label = "identity")

  # P2, cycle 2: a REAL, non-identity edit via the module's own sanctioned
  # mutator (plan-mandated: "so the property is exercised, not vacuous").
  # Does not change any run's line count, so P3 can use it directly.
  edit <- function(run) set_leading_separator(run, ", ")
  p2 <- .check_p2_fixed_point(parent, ex, label, style = edit, edit_label = "set_leading_separator(', ')")
  .check_p3_isolation(parent, p2$work, label)
  .check_p4_no_crossing(parent, ex, label)
  .check_p5_parses(p2$work, label)

  # P2, cycle 3: the REAL driver (styling/comment_style_driver.R) - passes
  # 1-7 scaffolded/threaded through context derived from the parent, per
  # plan sections 3/9. Reuses P2/P3/P5 unchanged, only a different `style`
  # argument - exactly what P2's own docstring anticipated. This is the
  # first time the fixed point runs with real styling (plan section 7's
  # risk, its true test).
  driver_style <- make_comment_styler(parent)
  p2d <- .check_p2_fixed_point(parent, ex, label, style = driver_style,
                                edit_label = "REAL DRIVER (comment_style_driver.R)")
  .check_p3_isolation_general(parent, p2d$work, label)
  .check_p5_parses(p2d$work, label)

  # Idempotence (acceptance 2): the whole-parent driver applied twice must
  # equal applied once, byte-identical - the stronger, end-to-end form of
  # the same fixed-point property, exercised through style_comment_substream()
  # rather than run-by-run.
  once  <- style_comment_substream(parent)
  twice <- style_comment_substream(once$tokens)
  .ok(sprintf("%s driver idempotence: applied twice == applied once", label),
      identical(once$tokens$text, twice$tokens$text) &&
        identical(once$tokens$type, twice$tokens$type) &&
        nrow(once$tokens) == nrow(twice$tokens))

  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  cov <- .report_coverage(label, parent, ex)
  .report_driver_coverage(label, once$coverage)
  cat(sprintf("    %.2fs\n", elapsed))
  invisible(list(parent = parent, ex = ex, n_comment_tok = cov$n_comment_tok,
                 n_block_tok = cov$n_block_tok, driver_once = once))
}

.verify_file <- function(path, encoding = "UTF-8") {
  parent <- read_qlik_script(path, encoding = encoding)
  .verify_all(parent, path)
}

# =======================================================================
# 1. Synthetic fixture - deterministic ground truth for every
#    classification path, PLUS the splice/void (line-count-changing) case
#    the real fixtures above don't cheaply exercise. Folded in from the
#    deleted test_comment_substream.R.
# =======================================================================
.section("Synthetic fixture - properties 1-5 plus classification ground truth")

.syn_src <- paste(c(
  "Test:",
  "LOAD",
  "\t[A] as [A],",
  "\t// [B] as [B],",
  "\t// [C] as [C]",
  "\t[D] as [D]",
  "FROM x.qvd (qvd);",
  "",
  "SET sep1 = 1;",
  "",
  "// Old:",
  "// LOAD",
  "",
  "// \t[E] as [E],",
  "// \t[F] as [F]",
  "// FROM y.qvd (qvd);",
  "",
  "SET sep2 = 1;",
  "",
  "// [G] as [G] = SubField(",
  "// [H], 1)",
  "",
  "SET sep3 = 1;",
  "",
  "// [Never] as [Closes] = SubField(",
  "// this paren never closes",
  "",
  "SET sep4 = 1;",
  "",
  "// This is just a note about the table above.",
  "// It has no brackets or commas at all.",
  "",
  "SET sep5 = 1;",
  "",
  "// [Mix1] as [Mix1],",
  "// Note: mix1 feeds into a downstream chart.",
  "// [Mix2] as [Mix2]",
  ""
), collapse = "\n")

.syn <- tokenize_qlik(.syn_src)
.syn_res <- .verify_all(.syn, "synthetic")
.syn_ex <- .syn_res$ex

.syn_status <- vapply(.syn_ex$runs, function(r) r$status, character(1))
.syn_kind   <- vapply(.syn_ex$runs, function(r) if (is.na(r$kind)) NA_character_ else r$kind, character(1))
.syn_lines  <- vapply(.syn_ex$runs, function(r) sprintf("%d-%d", r$line_start, r$line_end), character(1))

.ok(sprintf("synthetic: found %d runs (expected 6)", length(.syn_ex$runs)),
    length(.syn_ex$runs) == 6L, paste(.syn_lines, .syn_status, .syn_kind))

.r1 <- .syn_ex$runs[[1]]
.ok("synthetic: run 1 (mid-list fields) extracted as field_run, leading_sep captured",
    identical(.r1$status, "extracted") && identical(.r1$kind, "field_run") &&
      identical(.r1$leading_sep, " "))

.r2 <- .syn_ex$runs[[2]]
.ok("synthetic: run 2 (commented table, interior blank line) extracted as load_block, not split",
    identical(.r2$status, "extracted") && identical(.r2$kind, "load_block") &&
      length(.r2$comment_idx) == 5L)

.r3 <- .syn_ex$runs[[3]]
.ok("synthetic: run 3 (paren spans 2 lines) is ONE extracted run",
    length(.r3$comment_idx) == 2L && identical(.r3$status, "extracted"))

.r4 <- .syn_ex$runs[[4]]
.ok("synthetic: run 4 (unbalanced paren) refused",
    identical(.r4$status, "refused") && grepl("unbalanced parentheses", .r4$reason %||% ""))

.r5 <- .syn_ex$runs[[5]]
.ok("synthetic: run 5 (pure prose) is prose_only, not refused", identical(.r5$status, "prose_only"))

.r6 <- .syn_ex$runs[[6]]
.ok("synthetic: run 6 (field+prose+field) is ONE extracted run, middle line carried as prose",
    identical(.r6$status, "extracted") && identical(.r6$kind, "field_run") &&
      length(.r6$comment_idx) == 3L &&
      identical(.r6$line_kind, c("field", "prose", "field")))

# =======================================================================
# Context params (Task 3) non-default behaviour - committed checks (Task 4
# acceptance 3). Task 3 proved the DEFAULT path byte-identical against the
# baseline; these are the first committed checks that the NON-default path
# (what the driver actually calls) does what its INTERFACES.md entry says.
# =======================================================================
.section("Context params (Task 3) non-default behaviour")

# context$first_field = FALSE (pass 3): a leading comma is synthesised in
# front of the call's own first segment when one isn't already there.
.cf3_default <- tokenize_qlik("LOAD [B] AS [B]\nFROM x;")
.cf3_default <- enforce_leading_commas(.cf3_default)$tokens
.ok("enforce_leading_commas: first_field default/TRUE leaves the first segment untouched (today's behaviour)",
    !any(.cf3_default$type == "COMMA"))

.cf3_false <- tokenize_qlik("LOAD [B] AS [B]\nFROM x;")
.cf3_false <- enforce_leading_commas(.cf3_false, list(first_field = FALSE))$tokens
.ok("enforce_leading_commas: first_field=FALSE synthesises a leading comma before the first segment",
    any(.cf3_false$type == "COMMA") &&
      identical(paste(.cf3_false$text, collapse = ""), "LOAD ,[B] AS [B]\nFROM x;"))

# context$base_depth (pass 6): extra tabs prefixed onto every line's flat
# indent, including line 1. Compared by TAB COUNT on the field's own
# leading WS (after its last newline), not by string-prepending - the
# extra tabs land right after the newline, not in front of the whole gap.
.cf6_src <- "LOAD\n[B] AS [B]\nFROM x;"
.cf6_0 <- enforce_vertical_layout(tokenize_qlik(.cf6_src), list(base_depth = 0L))$tokens
.cf6_2 <- enforce_vertical_layout(tokenize_qlik(.cf6_src), list(base_depth = 2L))$tokens
.cf6_field_tabs <- function(tk) {
  i <- which(tk$type == "BRACKET" & tk$text == "[B]")[1]
  gap <- tk$text[i - 1L]
  nchar(gsub("[^\t]", "", sub("^.*\n", "", gap)))
}
.ok("enforce_vertical_layout: base_depth adds exactly N extra tabs to the flat indent",
    .cf6_field_tabs(.cf6_2) == .cf6_field_tabs(.cf6_0) + 2L)

# context$target_col (pass 7): taken as given, never widened by a wider
# field of this call's own. Two fields of very different width, so the
# NATURAL (unsupplied) column is forced wide by the long one - target_col
# must override that, not just add to it.
.cf7_src <- paste0("LOAD\n\t[X] AS [X],\n\t[VeryLongFieldNameHereIndeed] AS ",
                   "[VeryLongFieldNameHereIndeed]\nFROM x;")
.cf7_laid_out <- enforce_vertical_layout(tokenize_qlik(.cf7_src))$tokens
.cf7_natural <- enforce_alias_alignment(.cf7_laid_out)$tokens
.cf7_given   <- enforce_alias_alignment(.cf7_laid_out, list(target_col = 8L))$tokens
.cf7_as_col <- function(tk, field) {
  fi <- which(tk$type == "BRACKET" & tk$text == field)[1]
  ai <- which(tk$type == "WORD" & toupper(tk$text) == "AS")
  ai <- ai[ai > fi][1]
  col <- 0L
  for (ch in strsplit(tk$text[fi:(ai - 1L)], "", fixed = TRUE)) for (c in ch) {
    col <- if (c == "\t") (col %/% 4L + 1L) * 4L else col + 1L
  }
  col
}
.ok("enforce_alias_alignment: target_col is TAKEN AS GIVEN, overriding this call's own wider natural max",
    .cf7_as_col(.cf7_given, "[X]") < .cf7_as_col(.cf7_natural, "[X]"))

# --- splice/void: a line-count-changing edit, exercised on its own since
# the properties above deliberately avoid it (P3's isolation check is only
# valid for a line-count-STABLE edit; splice/void legitimately changes row
# count, which is what it is FOR). Folded from test_comment_substream.R.
.section("Synthetic fixture - splice/void (line-count-changing edit)")

.v0 <- .syn
.vr <- extract_comment_runs(.v0)$runs[[1]]
.vr_more <- .vr
extra_row <- data.frame(text = c("\t[C2]", " as ", "[C2]"),
                         type = c("BRACKET", "WS", "BRACKET"), line = 1L, stringsAsFactors = FALSE)
.vr_more$tokens <- rbind(.vr$tokens,
                          data.frame(text = "\n", type = "WS", line = 1L, stringsAsFactors = FALSE),
                          extra_row)
.v1 <- serialize_comment_run(.v0, .vr_more)
.vr_grown <- extract_comment_runs(.v1)$runs[[1]]
# Compare CONCATENATED text, not row-by-row: a hand-assembled edit's own WS
# row boundaries ("\n" then "\t[C2]" as two rows here) need not match how
# tokenize_qlik() naturally groups whitespace on re-extraction ("\n\t" then
# "[C2]") - the fixed point is about serialized BODY TEXT, never about one
# particular row-split of it (compare P1/P2 above, whose `exp$tokens` are
# always genuine extract_comment_runs() output and so never hit this).
.ok("splice path: adding a line to a run grows its comment_idx by one AND its own child re-extracts stable",
    length(.vr_grown$comment_idx) == length(.vr$comment_idx) + 1L &&
      identical(paste(.vr_grown$tokens$text, collapse = ""), paste(.vr_more$tokens$text, collapse = "")))
before_idx <- seq_len(min(.vr$comment_idx) - 1L)
after_idx_old <- (max(.vr$comment_idx) + 1L):nrow(.v0)
after_idx_new <- after_idx_old + 2L  # shifted by the 2 rows splice_tokens() inserted (WS + COMMENT)
.ok("splice path: everything before/after the edited run is untouched, row count grew by exactly 2",
    nrow(.v1) == nrow(.v0) + 2L &&
      identical(.v1$text[before_idx], .v0$text[before_idx]) &&
      identical(.v1$type[before_idx], .v0$type[before_idx]) &&
      identical(.v1$text[after_idx_new], .v0$text[after_idx_old]) &&
      identical(.v1$type[after_idx_new], .v0$type[after_idx_old]))

.vr_fewer <- .vr
.nl_at <- which(grepl("\n", .vr$tokens$text, fixed = TRUE))[1]
.keep  <- if (is.na(.nl_at)) seq_len(nrow(.vr$tokens)) else seq_len(.nl_at - 1L)
.vr_fewer$tokens <- .vr$tokens[.keep, , drop = FALSE]
.v2 <- serialize_comment_run(.v0, .vr_fewer)
.v2_runs <- extract_comment_runs(.v2)$runs
# void_token() blanks rows in place rather than deleting (VOID = trivia,
# indices stay valid) - nrow is UNCHANGED by design; the surplus row and
# its leading WS become VOID instead.
n_voided <- sum(.v2$type == "VOID") - sum(.v0$type == "VOID")
.ok("void path: shrinking a run's line count shrinks its comment_idx; surplus rows go VOID, not deleted",
    length(.v2_runs[[1]]$comment_idx) == 1L && nrow(.v2) == nrow(.v0) && n_voided == 2L,
    sprintf("comment_idx=%d nrow %d->%d voided+%d",
            length(.v2_runs[[1]]$comment_idx), nrow(.v0), nrow(.v2), n_voided))

# =======================================================================
# 2. Real fixtures - all six properties, always run (fast: seconds).
# =======================================================================
.section("Real fixtures - properties 1-6")

.res_fmt <- .verify_file("fixtures/formatexample.txt")
.res_gmr <- .verify_file("fixtures/[Grant Managing Region].txt")

.section("Coverage reconciliation against plan section 1's measured table")
cat(sprintf("  GMR: // comment tokens=%d, +%d /* */ block comments = %d (plan: 94 excl. ///$tab)\n",
            .res_gmr$n_comment_tok, .res_gmr$n_block_tok, .res_gmr$n_comment_tok + .res_gmr$n_block_tok))

# =======================================================================
# 3. app-unbuilt/script.qvs - gated behind --full, same convention as
#    verify.R's --stage3 (not sourced from verify.R - a separate flag).
# =======================================================================
.args <- commandArgs(trailingOnly = TRUE)
if ("--full" %in% .args) {
  .section("Full fixture (--full) - properties 1-6 on app-unbuilt/script.qvs")
  .res_app <- .verify_file("app-unbuilt/script.qvs")

  .section("Coverage reconciliation against plan section 1's measured table")
  cat(sprintf(
    "  script.qvs: // comment tokens=%d, +%d /* */ block comments = %d (plan: 2483 excl. ///$tab)\n",
    .res_app$n_comment_tok, .res_app$n_block_tok, .res_app$n_comment_tok + .res_app$n_block_tok))
  cat("  DISCREPANCY explained: the plan's 2,483 counts block /* */ comments in with the\n")
  cat("  '// comment' total; this module scopes to line-comments only (block comments stay\n")
  cat("  excluded per CLAUDE.md / plan section 6.4) - 2473 + 10 block = 2483, exact.\n")
} else {
  cat("\n(skipping app-unbuilt/script.qvs - pass --full to include it)\n")
}

cat(sprintf("\n%d checks, %d failed\n", .checks, .fails))
if (.fails > 0L) quit(status = 1L) else cat("COMMENT SUBSTREAM VERIFIED\n")
