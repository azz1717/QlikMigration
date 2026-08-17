# run_pipeline.R
#
# Runs the full loadscript style-guide pipeline, in order:
#   1. ensure_explicit_aliases    - every field gets an explicit AS alias
#   2. enforce_bracket_references - quoted AND bare field/alias refs -> brackets
#   3. enforce_leading_commas     - field-separator commas trailing -> leading
#   4. enforce_intraline_spacing  - comma/operator/paren whitespace -> DESIGN §4.7
#   5. enforce_reserved_word_case - Qlik keywords and functions -> UPPER
#   6. enforce_vertical_layout    - indentation/blank lines -> DESIGN §4.5/§4.8
#   7. enforce_alias_alignment    - AS column alignment -> DESIGN §4.6
#
# Spacing runs after leading-commas (DESIGN §6.1: it supplies the space a
# relocated comma needs) and before casing. Casing runs last among the first
# five on purpose: earlier passes splice in new tokens (the "AS" of an added
# alias, a relocated comma), so finishing with the casing pass guarantees the
# whole output is consistently cased no matter what they emitted. Casing and
# spacing don't otherwise interact - one only rewrites WORD text, the other
# only touches WS tokens - so their relative order is free.
#
# Layout runs after those five: it is the only one of them that reads
# whole-script STRUCTURE (find_block_structure()) rather than local field
# segments, so it should see the tokens in their final shape.
#
# Alignment runs LAST, after layout, on purpose (not just by convention):
# the AS column it computes is measured from each field's FINAL indentation
# (DESIGN §4.6) - running it any earlier would align to indentation layout
# was about to change.
#
# Kept up to date as passes are added/reordered - this is always the
# current full pipeline, not a snapshot from when a particular pass was built.

setwd("C:/Rtools")

source("qlik_tokenizer.R")
source("qlik_reserved_words.R")
source("ensure_explicit_aliases.R")
source("enforce_bracket_references.R")
source("enforce_leading_commas.R")
source("enforce_intraline_spacing.R")
source("enforce_reserved_word_case.R")
source("enforce_vertical_layout.R")
source("enforce_alias_alignment.R")

input_path  <- "[Grant Managing Region].txt"
output_path <- "script_out.txt"

tokens <- read_qlik_script(input_path)

r1 <- ensure_explicit_aliases(tokens)
r2 <- enforce_bracket_references(r1$tokens)
r3 <- enforce_leading_commas(r2$tokens)
r4 <- enforce_intraline_spacing(r3$tokens)
r5 <- enforce_reserved_word_case(r4$tokens)
r6 <- enforce_vertical_layout(r5$tokens)
r7 <- enforce_alias_alignment(r6$tokens)

warnings_all <- c(r1$warnings, r2$warnings, r3$warnings, r4$warnings, r5$warnings, r6$warnings, r7$warnings)
if (length(warnings_all) > 0) {
  cat("=== WARNINGS (", length(warnings_all), ") ===\n", sep = "")
  cat(paste(warnings_all, collapse = "\n"), "\n")
} else {
  cat("No warnings.\n")
}

writeLines(detokenize(r7$tokens), output_path)
cat("Wrote", output_path, "\n")

# Per-pass change logs, if you want to inspect what each stage actually did:
#   r1$changes   (aliases added)
#   r2$changes   (quotes/bare words -> brackets)
#   r3$changes   (commas moved)
#   r4$changes   (whitespace inserted/removed/collapsed; $kind says which)
#   r5$changes   (words recased; $kind is "keyword" or "function")
#   r6$changes   (indentation/blank-line rewrites; $kind is the line's kind)
#   r7$changes   (AS-column tab padding rewrites)
