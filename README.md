# Qlik load-script style tooling

Base-R tooling that applies a consistent style to Qlik Sense load scripts, to
automate the manual reformatting involved in migrating apps from on-prem to
Qlik Cloud.

The style guide is **cosmetic**. Unconverted scripts run fine in Qlik Cloud;
this exists to make them consistent and maintainable.

This is phase 1 of three. Phase 2 (a nice-to-have) drops tables and loads
nothing in the app actually uses, and phase 3 retargets qvd paths and field
names for the new environment. Phase 1 must be behaviour-preserving; the
later two change data on purpose. Two consequences worth knowing before you
touch anything:

- **The style pipeline runs last in every phase**, not only this one — like
  `gofmt` after a refactor. Retargeting changes field name lengths, which
  disturbs alias alignment.
- **`verify.R` can only check phase 1.** Semantic equivalence is violated by
  design once field names change.

See DESIGN §5.

See [DESIGN.md](DESIGN.md) for architecture, decisions, the roadmap, verified
Qlik behaviours, and the migration debt report (§7).

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
source("enforce_intraline_spacing.R")
source("enforce_reserved_word_case.R")
source("enforce_vertical_layout.R")

tokens <- read_qlik_script("myscript.qvs")

r1 <- ensure_explicit_aliases(tokens)
r2 <- enforce_bracket_references(r1$tokens)
r3 <- enforce_leading_commas(r2$tokens)
r4 <- enforce_intraline_spacing(r3$tokens)
r5 <- enforce_reserved_word_case(r4$tokens)
r6 <- enforce_vertical_layout(r5$tokens)

r1$changes    # exactly what each stage did
r6$warnings   # anything it declined to touch, or wants you to look at

writeLines(detokenize(r6$tokens), "myscript_out.qvs")
```

## The passes

| # | Pass | Does |
|---|------|------|
| 1 | `ensure_explicit_aliases` | every field gets an explicit `AS` alias |
| 2 | `enforce_bracket_references` | quoted AND bare field/alias references become `[bracketed]` |
| 3 | `enforce_leading_commas` | trailing field separators move to leading position |
| 4 | `enforce_intraline_spacing` | comma, operator and parenthesis spacing within a line |
| 5 | `enforce_reserved_word_case` | Qlik keywords and built-in functions become UPPER |
| 6 | `enforce_vertical_layout` | indentation and blank lines between statements |

Spacing runs after the comma pass, which supplies the separator it spaces
(DESIGN §6.1). Casing runs last among the first five so that tokens spliced
in by earlier passes are cased too. Layout runs last of all, since it is the
only pass reading whole-script structure rather than local field segments.
Alias alignment (not yet built) must always run after layout, because it is
computed from the final indentation. See DESIGN §4.6.

Current cost, whole pipeline including tokenizing:

| script | lines | tokens | total |
|--------|------:|-------:|------:|
| `[Grant Managing Region].txt` | 762 | 4,969 | 0.57s |
| `app-unbuilt/script.qvs` | 13,870 | 31,341 | 2.36s |

Re-measured 2026-08-17 with all six passes in place (was 0.42s / 1.97s over
five). Token counts are the *input* stream.

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

Run the suite. It exits non-zero on failure, so it can gate a commit:

```bash
Rscript verify.R
```

It covers, over both fixtures: tokenizer round-trip, token-stream sanity,
and for every pass — **semantic equivalence**, output round-trip, and
idempotence. It also self-tests, by corrupting a stream deliberately and
confirming the equivalence check notices.

The semantic equivalence check is the important one. It reduces both streams
to the tokens that carry meaning, normalising away exactly the differences
the passes are allowed to make, and asserts the rest is identical. Unlike a
golden-file diff it keeps working when output changes on purpose — which is
what happens every time a pass is added — and unlike `$changes` it does not
take a pass's word for what it did.

It earned that on its first run, catching a real idempotence bug in
`enforce_leading_commas` whose docstring explicitly claimed the property.

Two things it cannot do, so still worth doing by hand for a risky change:

- **Object for object** — keep the old implementation beside the new one and
  assert `identical()` on `$tokens`, `$warnings` *and* `$changes`. This is
  what caught `splice_tokens` silently dropping the `source_path` attribute,
  where both the output text and every change log were identical.
- Confirm Qlik agrees the two scripts behave the same. Nothing here can
  establish that.

### Test input must carry the property being tested

This has bitten twice, from opposite directions, and both times the tests
passed while something was badly wrong:

- A scaling benchmark built from **synthetic ASCII-only** input was used to
  test whether non-ASCII characters were causing quadratic tokenizing. R took
  its fast path, the benchmark returned a confident "hypothesis falsified",
  and two rounds went into chasing the wrong cause. The hypothesis was
  correct; the input could not exercise it.
- The equivalence check's self-tests use three-line hand-built streams. They
  passed while its failure reporting was unusable on the real script — a
  single dropped field printed twenty lines of inline data four times over,
  because a real `BRACKET` token can hold an entire INLINE table and a toy
  one never does.

So: run new checks against `app-unbuilt/script.qvs`, not only against
hand-written snippets. The synthetic fixture proves the logic; only the real
one proves it survives 13,870 lines of syntax nobody enumerated.

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
