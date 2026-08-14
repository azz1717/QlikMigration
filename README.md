# Qlik load-script style tooling

Base-R tooling that applies a consistent style to Qlik Sense load scripts, to
automate the manual reformatting involved in migrating apps from on-prem to
Qlik Cloud.

The style guide is **cosmetic**. Unconverted scripts run fine in Qlik Cloud;
this exists to make them consistent and maintainable. Retargeting connection
names and paths for the new environment is a separate concern and is not done
here.

See [DESIGN.md](DESIGN.md) for architecture, decisions, the roadmap, and
verified Qlik behaviours.

## Requirements

Vanilla base R. No packages, no installs — the `.R` files are carried to an
airgapped work machine to run, so they must have zero dependencies.
Developed against R 4.5.2.

## Running it

Edit `input_path` / `output_path` at the top of `run_pipeline.R`, then:

```bash
Rscript run_pipeline.R
```

Or interactively, which is usually more useful because you keep every stage:

```r
setwd("C:/Rtools")
source("qlik_tokenizer.R")
source("qlik_reserved_words.R")
source("ensure_explicit_aliases.R")
source("enforce_bracket_references.R")
source("enforce_leading_commas.R")
source("enforce_reserved_word_case.R")

tokens <- read_qlik_script("myscript.qvs")

r1 <- ensure_explicit_aliases(tokens)
r2 <- enforce_bracket_references(r1$tokens)
r3 <- enforce_leading_commas(r2$tokens)
r4 <- enforce_reserved_word_case(r3$tokens)

r1$changes    # exactly what each stage did
r4$warnings   # anything it declined to touch, or wants you to look at

writeLines(detokenize(r4$tokens), "myscript_out.qvs")
```

## The passes

| # | Pass | Does |
|---|------|------|
| 1 | `ensure_explicit_aliases` | every field gets an explicit `AS` alias |
| 2 | `enforce_bracket_references` | quoted field/alias references become `[bracketed]` |
| 3 | `enforce_leading_commas` | trailing field separators move to leading position |
| 4 | `enforce_reserved_word_case` | Qlik keywords and built-in functions become UPPER |

Casing runs last so that tokens spliced in by earlier passes are cased too.
Otherwise the passes are order-independent.

Current cost, whole pipeline including tokenizing:

| script | lines | tokens | total |
|--------|------:|-------:|------:|
| `[Grant Managing Region].txt` | 762 | 5,015 | 0.34s |
| `app-unbuilt/script.qvs` | 13,870 | 31,439 | 1.61s |

## The contract every pass honours

These are not style preferences — breaking one causes silent corruption.

1. **The token stream is the medium.** Tokenize once, hand `$tokens` from pass
   to pass, `detokenize()` only to write a file. Never re-parse text between
   passes.
2. **Concatenating `$text` reproduces the input exactly.** Every character of
   the source belongs to some token. A pass changes only what it intends to.
3. **Never delete a token.** Blank it with `void_token()` instead. Deleting a
   row shifts every index computed from the original stream.
4. **Never grow a token's text to inject syntax.** Use `splice_tokens()`.
   Later passes recognise syntax by token *type*, so anything smuggled into
   another token's text is structurally invisible to them.
5. **Skip `SELECT ... ;` blocks.** That is where foreign syntax lives — raw
   SQL and connector DSLs whose identifiers are case-sensitive and are not
   Qlik's to reformat.
6. **Return `list(tokens, warnings, changes)`.** Never fail silently: if a
   pass declines to transform something, it says so in `$warnings`.

## Adding a pass

Copy the shape of an existing pass. The template:

```r
my_pass <- function(tokens) {
  found <- find_load_segments(tokens)     # if you work per-field
  warn <- found$warnings

  n <- nrow(tokens)

  # 1. hoist columns to plain vectors - tokens$col[i] pays data.frame
  #    dispatch on every access, and passes index in tight loops
  t_text <- tokens$text; t_type <- tokens$type; t_line <- tokens$line

  # 2. preallocate the change log as atomic vectors; building one
  #    data.frame per iteration and rbind()-ing them costs more than the
  #    pass's real work
  ch_line <- integer(n); ch_before <- character(n); nch <- 0L

  # ... decide, collecting insertions / indices to void ...

  # 3. apply structural edits in ONE call each
  tokens <- void_token(tokens, void_idx[seq_len(nvoid)])
  tokens <- splice_tokens(tokens, insertions)

  # 4. build the data.frame once
  changes <- data.frame(line = ch_line[seq_len(nch)],
                        before = ch_before[seq_len(nch)],
                        stringsAsFactors = FALSE)

  list(tokens = tokens, warnings = warn, changes = changes)
}
```

Then add it to `run_pipeline.R`, which is kept current rather than being a
snapshot.

## Verifying a change

Text diffs are not sufficient. A byte-identical output file once hid a real
regression (`splice_tokens` silently dropping the `source_path` attribute).
Check both:

1. **End to end** — run the pipeline over `app-unbuilt/script.qvs` and diff
   against output from before your change.
2. **Object for object** — keep the old implementation alongside the new one
   and assert `identical()` on `$tokens`, `$warnings` *and* `$changes`.

Also assert **idempotence**: running a pass twice must produce zero changes
the second time.

## Fixtures

- **`[Grant Managing Region].txt`** — the real working test case. 762 lines,
  pure ASCII. Use for behaviour and style correctness.
- **`app-unbuilt/`** — a `qlik-cli` unpack of an *unrelated* app, kept as a
  scale and encoding stress fixture: 13,870-line script, 63 `///$tab`
  sections, `FOR`/`NEXT` loops, several `LIB CONNECT TO` blocks, and 204
  lines containing non-ASCII characters. Use for performance, encoding and
  exotic-syntax stress. The Grant script cannot exercise the multibyte code
  path at all.

Generated output (`script_out*.txt`, `scratch_full_out.qvs`) is gitignored —
check out an older commit and re-run to reproduce any previous result.
