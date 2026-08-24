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

See [docs/DESIGN.md](docs/DESIGN.md) for architecture, decisions, the roadmap,
verified Qlik behaviours, and the migration debt report (§7).

## Repo layout

Reorganised 2026-08-21 so a VM operator sees only what they need — the repo
root is entry points, everything else is engine room:

| Where | What |
|---|---|
| *(repo root)* | Entry points only: `launch_console_ui.bat`, `qlik_probe.bat`, plus this `README.md` |
| `ui/` | `console_ui.R` — the interactive front end the launcher starts |
| `diagnostics/` | `qlik_cli_probe.R` — tenant reachability probe (DESIGN §8) |
| `input-apps/` | Put the app you want processed here (its own subfolder) |
| `outputs/` | Styled scripts and reports land here, named after the app |
| `shared/` | Tokenizer, reserved-word lists, and the csv/JSON helpers — used by everything below |
| `styling/` | The eight style passes `run_pipeline.R` wires together |
| `analysis/` | Phase 2 usage/review/pruning scripts, and the migration debt report |
| `retargeting/` | Phase 3 — not started yet (DESIGN §6.6) |

The working docs live in `docs/`, which is **ignored here and has its own git
repo with no remote** (2026-08-24). They need history — you cannot safely
compact design prose you cannot diff back — but they do not need publishing,
and git offers no way to track a file locally while withholding it from a
push. Two needs, two repos. Commit doc changes from inside `docs/`; `git
status` at the root will never show them.

Still not tracked, and deliberately: the example apps, `verify.R` (a dev gate
that must never run on a VM), the `fixtures/` it reads, and Adam's personal
`docs/dlmf-log.md`. The fixtures were untracked on 2026-08-24: they exist for
`verify.R`, `verify.R` never runs on a VM, so the repo was carrying its
largest files for nobody. git here exists purely to deliver scripts to VMs that
never commit or push, so what a VM operator downloads stays lean.

## Requirements

Vanilla base R. No packages, no installs — the `.R` files are carried to an
airgapped work machine to run, so they must have zero dependencies.
Developed against R 4.5.2.

## Running it

Both file names are required — no default input (2026-08-21: a missed
argument used to silently style a built-in fixture instead of your script):

```bash
Rscript run_pipeline.R myscript.qvs myscript_out.txt
```

Or interactively, which is usually more useful because you keep every stage
(paths below are relative to the repo root — see "Repo layout"):

```r
setwd("C:/Rtools")   # or wherever this repo was cloned
source("shared/qlik_tokenizer.R")
source("shared/qlik_reserved_words.R")
source("styling/ensure_explicit_aliases.R")
source("styling/enforce_bracket_references.R")
source("styling/enforce_leading_commas.R")
source("styling/enforce_intraline_spacing.R")
source("styling/enforce_reserved_word_case.R")
source("styling/enforce_vertical_layout.R")
source("styling/enforce_alias_alignment.R")

tokens <- read_qlik_script("myscript.qvs")

r1 <- ensure_explicit_aliases(tokens)
r2 <- enforce_bracket_references(r1$tokens)
r3 <- enforce_leading_commas(r2$tokens)
r4 <- enforce_intraline_spacing(r3$tokens)
r5 <- enforce_reserved_word_case(r4$tokens)
r6 <- enforce_vertical_layout(r5$tokens)
r7 <- enforce_alias_alignment(r6$tokens)

r1$changes    # exactly what each stage did
r7$warnings   # anything it declined to touch, or wants you to look at

writeLines(detokenize(r7$tokens), "myscript_out.qvs")
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
| 7 | `enforce_alias_alignment` | aligns every field's `AS` to one column within its own LOAD block |
| 8 | `enforce_commented_field_style` | applies all of the above to commented-out fields, by rewriting comment text only |

Spacing runs after the comma pass, which supplies the separator it spaces
(DESIGN §6.1). Casing runs last among the first five so that tokens spliced
in by earlier passes are cased too. Layout runs after those five, since it
is the only one of them reading whole-script structure rather than local
field segments. Alias alignment runs LAST of all, because its column is
computed from each field's final indentation (DESIGN §4.6).

Pass 8 applies the same rules to commented-out LOAD fields, so uncommenting
one yields a line that reloads without a hand-fix — comma placement and a
missing alias used to break the script every time. It rewrites the TEXT OF
COMMENT TOKENS AND NOTHING ELSE: commented script never becomes live tokens,
which is what keeps it from interfering with the seven passes before it. An
earlier design did unwrap comments into real tokens, and DESIGN §4.10 records
the four distinct corruptions that followed.

Current cost, whole pipeline including tokenizing:

| script | lines | tokens | total |
|--------|------:|-------:|------:|
| `[Grant Managing Region].txt` | 762 | 4,969 | 0.69s |
| `app-unbuilt/script.qvs` | 13,870 | 31,341 | 2.93s |

Re-measured 2026-08-17 with all seven passes in place (was 0.57s / 2.36s
over six). Token counts are the *input* stream.

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

Dev-only, not part of the tracked repo (see "Repo layout") — this gates a
commit on the development machine, a VM never needs it. Run the suite; it
exits non-zero on failure:

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
