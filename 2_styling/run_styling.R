# run_styling.R - reformat a Qlik load script to the house style guide.
# migrate.R sources this file (from the project folder) and calls
# style_script() once per app.
#
# WHAT IT DOES: reads the script once into a TOKEN STREAM - a data.frame
# where every keyword, name, comment and run of spaces is one row - then
# hands that stream to eight passes in turn and writes the result back out.
# No pass ever edits the file as text; they edit tokens. That is what keeps
# cosmetic reformatting from changing what the script means.
#
# The passes, in order:
#   1. ensure_explicit_aliases    - every field gets an explicit AS alias
#   2. enforce_bracket_references - quoted AND bare field/alias refs -> brackets
#   3. enforce_leading_commas     - field-separator commas trailing -> leading
#   4. enforce_intraline_spacing  - comma/operator/paren whitespace
#   5. enforce_reserved_word_case - Qlik keywords and functions -> UPPER
#   6. enforce_vertical_layout    - indentation and blank lines
#   7. enforce_alias_alignment    - AS column alignment
#   8. style_comment_substream    - the same rules, applied to commented-out
#      fields and blocks, by rewriting comment TEXT only
#
# WHY THAT ORDER:
#   - Spacing (4) after leading commas (3): it supplies the space a moved
#     comma needs.
#   - Casing (5) after 1-4: those add NEW tokens (the "AS" of an added alias,
#     a moved comma), so casing last makes the whole output consistent.
#   - Layout (6) after all five: it reads whole-script structure, so it must
#     see the final tokens.
#   - Alignment (7) last: the AS column is measured from each field's FINAL
#     indentation.

source("shared/qlik_tokenizer.R")
source("shared/qlik_reserved_words.R")
source("2_styling/01_explicit_aliases.R")
source("2_styling/02_bracket_references.R")
source("2_styling/03_leading_commas.R")
source("2_styling/04_spacing.R")
source("2_styling/05_keyword_case.R")
source("2_styling/06_vertical_layout.R")
source("2_styling/07_alias_alignment.R")
source("shared/comment_substream.R")
source("2_styling/08_commented_code.R")

# The pass names, for error messages.
PASS_LABELS <- c("explicit aliases", "bracket references", "leading commas",
                 "intra-line spacing", "reserved-word casing",
                 "vertical layout", "alias alignment", "comment style driver")

# Run one pass; if it fails, say which pass it was.
run_pass <- function(number, pass_fn, tokens) {
  tryCatch(pass_fn(tokens), error = function(e)
    stop("pass ", number, " (", PASS_LABELS[number], ") failed: ",
         conditionMessage(e), call. = FALSE))
}

# Style the script in input_path and write the result to output_path.
style_script <- function(input_path, output_path) {
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
  r8 <- run_pass(8, style_comment_substream,    r7$tokens)

  writeLines(detokenize(r8$tokens), output_path)
  invisible(output_path)
}
