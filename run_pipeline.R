# run_pipeline.R — reformat a Qlik load script to the house style guide.
#
# HOW TO RUN IT (from a terminal, not from inside R). Rscript is not on the
# PATH on this machine, so the full path is needed:
#
#   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" run_pipeline.R in.qvs out.qvs
#
# Add --help for the usage text, or --changes to also get a CSV per pass
# listing every edit it made. With no arguments at all it falls back to the
# built-in test fixture and says so before it starts.
#
# WHAT IT DOES: reads the script once into a TOKEN STREAM — a data.frame
# where every keyword, name, comment and run of spaces is one row — then
# hands that stream to seven passes in turn and writes the result back out.
# No pass ever edits the file as text; they edit tokens. That is what keeps
# cosmetic reformatting from changing what the script means.
#
# The passes, in order:
#   1. ensure_explicit_aliases    - every field gets an explicit AS alias
#   2. enforce_bracket_references - quoted AND bare field/alias refs -> brackets
#   3. enforce_leading_commas     - field-separator commas trailing -> leading
#   4. enforce_intraline_spacing  - comma/operator/paren whitespace -> DESIGN §4.7
#   5. enforce_reserved_word_case - Qlik keywords and functions -> UPPER
#   6. enforce_vertical_layout    - indentation/blank lines -> DESIGN §4.5/§4.8
#   7. enforce_alias_alignment    - AS column alignment -> DESIGN §4.6
#
# WHY THAT ORDER (it is not arbitrary — three of the four constraints are
# real bugs waiting if you reorder):
#   - Spacing (4) after leading-commas (3): it supplies the space a relocated
#     comma needs (DESIGN §6.1).
#   - Casing (5) after passes 1-4: those splice in NEW tokens (the "AS" of an
#     added alias, a relocated comma), so finishing with casing guarantees the
#     whole output is consistently cased whatever they emitted. Casing and
#     spacing don't otherwise interact — one rewrites WORD text, the other
#     only WS tokens — so their order relative to each other is free.
#   - Layout (6) after all five: it is the only one that reads whole-script
#     structure rather than local field segments, so it must see final tokens.
#   - Alignment (7) LAST: the AS column is measured from each field's FINAL
#     indentation (DESIGN §4.6). Any earlier and it aligns to indentation that
#     layout is about to change.
#
# Kept up to date as passes are added or reordered — this is always the
# current full pipeline, not a snapshot from when one pass was built.

# --- where the project lives ---------------------------------------------
# Every source() below, and both default file names, are relative to this
# folder, so the script sets it explicitly instead of depending on where it
# was launched from. The check exists so a moved repo produces one plain
# sentence rather than seven confusing "cannot open file" errors.
PROJECT_DIR <- "C:/Rtools"
if (!dir.exists(PROJECT_DIR)) {
  stop("project folder not found: ", PROJECT_DIR,
       "\n  Edit PROJECT_DIR at the top of run_pipeline.R if the repo moved.",
       call. = FALSE)
}
setwd(PROJECT_DIR)

# source() loads another file's functions into this session. The tokenizer
# and the word lists come first because every pass below depends on them.
#
# DO NOT reformat the seven pass lines that follow — no indenting, no folding
# them into a loop. verify_docs.R reads this file as text and matches them at
# the start of a line to confirm the pass list still agrees with verify.R and
# README. Rewriting them breaks that check silently.
source("qlik_tokenizer.R")
source("qlik_reserved_words.R")
source("ensure_explicit_aliases.R")
source("enforce_bracket_references.R")
source("enforce_leading_commas.R")
source("enforce_intraline_spacing.R")
source("enforce_reserved_word_case.R")
source("enforce_vertical_layout.R")
source("enforce_alias_alignment.R")

# Used for error messages and for --changes file names, so the wording of a
# pass name lives in exactly one place.
PASS_LABELS <- c("explicit aliases", "bracket references", "leading commas",
                 "intra-line spacing", "reserved-word casing",
                 "vertical layout", "alias alignment")

USAGE <- paste(
  "Reformat a Qlik load script to the house style guide.",
  "",
  "  Rscript run_pipeline.R [options] [input-file] [output-file]",
  "",
  "Arguments — both optional, and positional (order matters):",
  "  input-file    script to read.   Default: [Grant Managing Region].txt",
  "  output-file   file to write.    Default: script_out.txt",
  "",
  "Options:",
  "  --help, -h    show this text and exit",
  "  --changes     also write one CSV per pass, listing every edit it made,",
  "                into a 'changes' folder beside the output file",
  "",
  "Quote any path containing spaces.",
  sep = "\n")

# --- read the command line -----------------------------------------------
# commandArgs(trailingOnly = TRUE) is R's way of getting the arguments typed
# after the script name. Anything starting with "-" is treated as an option,
# everything else as a file name.
args <- commandArgs(trailingOnly = TRUE)

if (any(args %in% c("--help", "-h"))) {
  cat(USAGE, "\n", sep = "")
  quit(save = "no", status = 0)
}

flags      <- args[startsWith(args, "-")]
positional <- args[!startsWith(args, "-")]

unknown <- setdiff(flags, "--changes")
if (length(unknown) > 0) {
  stop("unrecognised option: ", paste(unknown, collapse = ", "),
       "\n  Run with --help to see the valid ones.", call. = FALSE)
}
want_changes <- "--changes" %in% flags

# A third file name almost always means an unquoted path with a space in it,
# which the shell split into pieces — so say that rather than just counting.
if (length(positional) > 2) {
  stop("expected at most two file names, got ", length(positional), ":\n  ",
       paste(positional, collapse = "\n  "),
       "\n  A path containing spaces must be quoted.", call. = FALSE)
}

input_path  <- if (length(positional) >= 1) positional[1] else "[Grant Managing Region].txt"
output_path <- if (length(positional) >= 2) positional[2] else "script_out.txt"

if (length(positional) == 0) {
  cat("No file names given — falling back to the built-in test fixture.\n")
  cat("  input:  ", input_path,  "\n", sep = "")
  cat("  output: ", output_path, "\n", sep = "")
  cat("Run with --help to see how to pass your own.\n\n")
}

# --- check the paths before doing any work -------------------------------
if (!file.exists(input_path)) {
  stop("input file not found: ", input_path,
       "\n  Looked in: ", getwd(), call. = FALSE)
}

# mustWork = FALSE because the output file legitimately does not exist yet;
# normalizePath still resolves the two to a comparable form.
if (normalizePath(input_path,  winslash = "/", mustWork = FALSE) ==
    normalizePath(output_path, winslash = "/", mustWork = FALSE)) {
  stop("input and output are the same file: ", input_path,
       "\n  Give a different output name so the original is kept.",
       call. = FALSE)
}

output_dir <- dirname(output_path)
if (!dir.exists(output_dir)) {
  stop("output folder does not exist: ", output_dir, call. = FALSE)
}

# --- run the pipeline ----------------------------------------------------
# Each pass takes the token stream and returns a list of three things:
#   $tokens   - the rewritten stream, fed straight into the next pass
#   $warnings - notes about things it deliberately did NOT change
#   $changes  - a data.frame logging every edit it did make
# The columns of $changes differ per pass; INTERFACES.md lists them.
#
# run_pass() adds nothing to that except a readable error: without it, a
# failure anywhere prints a bare R stack trace that names no pass.
run_pass <- function(number, pass_fn, tokens) {
  tryCatch(pass_fn(tokens), error = function(e)
    stop("pass ", number, " (", PASS_LABELS[number], ") failed: ",
         conditionMessage(e), call. = FALSE))
}

tokens <- tryCatch(read_qlik_script(input_path), error = function(e)
  stop("could not read ", input_path, ": ", conditionMessage(e),
       call. = FALSE))

r1 <- run_pass(1, ensure_explicit_aliases,    tokens)
r2 <- run_pass(2, enforce_bracket_references, r1$tokens)
r3 <- run_pass(3, enforce_leading_commas,     r2$tokens)
r4 <- run_pass(4, enforce_intraline_spacing,  r3$tokens)
r5 <- run_pass(5, enforce_reserved_word_case, r4$tokens)
r6 <- run_pass(6, enforce_vertical_layout,    r5$tokens)
r7 <- run_pass(7, enforce_alias_alignment,    r6$tokens)

results <- list(r1, r2, r3, r4, r5, r6, r7)

# --- report and write ----------------------------------------------------
# Warnings are the pipeline's "I saw this and left it alone" list. They are
# not errors: the output is still complete and still valid.
warnings_all <- unlist(lapply(results, `[[`, "warnings"))
if (length(warnings_all) > 0) {
  cat("=== WARNINGS (", length(warnings_all), ") ===\n", sep = "")
  cat(paste(warnings_all, collapse = "\n"), "\n", sep = "")
} else {
  cat("No warnings.\n")
}

writeLines(detokenize(r7$tokens), output_path)
cat("Wrote ", output_path, "\n", sep = "")

# --- optional: what each pass actually did -------------------------------
# The $changes tables are the answer to "why does my script look like that?".
# They only exist inside this session, so --changes writes them to disk;
# otherwise they vanish the moment the script ends.
if (want_changes) {
  changes_dir <- file.path(output_dir, "changes")
  dir.create(changes_dir, showWarnings = FALSE)

  for (i in seq_along(results)) {
    changes <- results[[i]]$changes
    if (is.null(changes)) next
    csv_path <- file.path(changes_dir,
                          sprintf("%d-%s.csv", i, gsub(" ", "-", PASS_LABELS[i])))
    write.csv(changes, csv_path, row.names = FALSE)
    cat(sprintf("  %d. %-21s %5d changes -> %s\n",
                i, PASS_LABELS[i], nrow(changes), csv_path))
  }
}

# To poke at the results by hand instead, open R in this folder and run
#   source("run_pipeline.R")
# then inspect r1$changes ... r7$changes directly (head(r4$changes), and so
# on). Sourcing keeps the objects alive; running via Rscript does not.
