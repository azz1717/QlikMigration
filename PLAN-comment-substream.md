# PLAN — comment styling by recursive sub-stream

**Status: not started. Nothing here is signed off.** This is a scoping
document, not a specification and not a change. No pipeline file is touched
by it.

Deliberately kept OUT of the five root docs `verify_docs.R` lints
(`README.md`, `DESIGN.md`, `STATE.md`, `INTERFACES.md`, `CLAUDE.md`). A
ground-up architecture plan should not have its wording shaped by a doc
linter's regex, and this one needs to record decisions still to be made.
If the plan is accepted, the settled parts move into DESIGN §4.10/§6 and the
outstanding decisions move into STATE.md, at which point the linter applies
normally.

---

## 1. What this fixes, measured

Pass 8 (`enforce_commented_field_style`) styles commented-out LOAD fields.
It reaches less than half of the commented-out script in the real app,
and the shortfall is structural rather than conservative.

Measured on the two fixtures (a `//` comment is one token per line, so
comment tokens and comment lines are interchangeable here):

| | `app-unbuilt/script.qvs` | `[Grant Managing Region].txt` |
|---|---:|---:|
| comment tokens, excluding `///$tab` | 2,483 | 94 |
| inside a live LOAD field list — pass 8's whole reach | 1,784 | 72 |
| **outside any live LOAD field list — unreachable** | **699** | **22** |
| styled today | 506 | 68 |
| field-shaped but unreachable | 307 | 3 |
| unreachable and not field-shaped (labels, LOAD, FROM, prose) | 392 | 19 |

Of the unreachable lines, these are unambiguously script:

| | `script.qvs` | `GMR` |
|---|---:|---:|
| commented table label `Name:` | 43 | 2 |
| commented `LOAD` / `SELECT` | 33 | 2 |
| commented `FROM` / `RESIDENT` / `JOIN` / `DROP` / … | 36 | 7 |

Samples of the 307 field-shaped unreachable lines are exactly the content
§4.10 was reopened to fix — mixed quoting, no brackets, trailing commas:

```
"AGIL_Connectivity.AGIL Code" as [AGIL Code],
"AGIL_Connectivity.NBN Fixed Line" as [NBN Fixed Line],
"AGIL_Connectivity.Optus 5G" as [Optus 5G],
```

For scale, `script_debt.R` reports 2,869 of `script.qvs`'s 13,869 lines
(20.7%) as commented-out code.

**Nobody decided to exclude commented-out tables.** It falls out of pass 8's
eligibility rule — the comment must sit inside a live LOAD field list — which
in turn falls out of its one-field synthetic stream. It was written into
STATE.md afterwards as scope. This plan treats it as an architectural limit,
not a decision.

By contrast, the other half of that STATE.md item is genuinely marginal:
paren-unbalanced comment bodies (one line of a multi-line commented
expression) number **2** in `script.qvs` and **4** in GMR. Worth fixing as a
by-product, never worth a redesign on its own.

---

## 2. Why pass 8 cannot simply be widened

To style a commented-out table you must style a whole **statement**: the
label, the `LOAD`, a field list with comma placement, indentation,
alignment, the `FROM` clause and the terminator. That needs passes 3, 6 and
7 — the exact three pass 8 excludes, because its synthetic one-field `LOAD`
carries none of the context they read from the surrounding stream.

So there is no incremental widening. Any version of pass 8 that handles a
commented-out table has already become the design below.

---

## 3. The design

Treat a contiguous run of `//` lines whose bodies are script as a **child
token stream** — its own `data.frame(text, type, line)`, built by
re-tokenizing the comment bodies. Styling becomes recursive:

```
style(stream, context) -> stream
```

For each child region: extract → `style(child, context)` → serialize the
result back into the COMMENT tokens of the parent.

The property that matters: **a pass cannot move a token across the
comment/live boundary, because the two sides are different objects.** It is
not guarded, not checked, not remembered — it is unrepresentable. That is
the whole reason to prefer this over anything that puts commented tokens
into the live stream.

`context` carries what the child cannot derive from itself:

- the alignment column its `AS` tokens should reach;
- the indent depth the block sits at;
- whether the first extracted field is the LOAD list's first field (which
  takes §4.5's two-space pad rather than a leading comma);
- the enclosing tab, for change-log reporting.

---

## 4. Alternatives, and why not

**Unwrap into the live stream, run passes 1–7, re-comment.** This was the
first implementation of pass 8 and it was abandoned. It is attractive — the
commented field becomes a genuine segment of the same LOAD block, so shared
alias alignment is free. It produced four distinct corruptions, every one
caught by `verify.R` at stage 3 and none by the suite as it stood before:

1. pass 3 relocated field separators **across** the comment/live boundary,
   emitting `LOAD , [X]` and merging two live fields into one;
2. pass 6's orphan-`;` rescue re-anchored a statement terminator onto a line
   that was about to become a comment again — a LOAD lost its `;` and ran on
   into the following `SELECT`;
3. a re-run read the styled leading comma as part of the field body and
   added another, growing one comma per run;
4. multi-line commented expressions were styled one line at a time.

Each was individually fixable. The reason to abandon it anyway: every pass
reasonably assumes every token it can see is live, so the class of failure
was open-ended — it extends to passes not yet written. Enumerating it is not
a strategy.

**Tag every token with an origin (`live` / `comment:N`) and forbid
cross-origin moves centrally.** Genuinely attractive, because DESIGN §2.4
already funnels every structural edit through exactly two helpers
(`splice_tokens()`, `void_token()`), so the rule would live in two places
rather than eight. Rejected because it keeps commented and live tokens in
one stream, which means every **scanner** — `find_load_segments()`,
`find_block_structure()` — must then reason about mixed regions. The child
stream keeps the scanners naive, and that is worth more than the tagging is
cheap.

---

## 5. What each pass needs

| pass | change needed |
|---|---|
| 1 `ensure_explicit_aliases` | none — already local |
| 2 `enforce_bracket_references` | none — already local |
| 3 `enforce_leading_commas` | accept "is this the first field of its list" from context instead of deriving it |
| 4 `enforce_intraline_spacing` | none — already local |
| 5 `enforce_reserved_word_case` | none — already local |
| 6 `enforce_vertical_layout` | accept a base indent depth from context; must not treat the child as top-level |
| 7 `enforce_alias_alignment` | accept a target column from context rather than computing the block max |
| 8 `enforce_commented_field_style` | **deleted** — its comma/indent/column re-implementations are what the context threading replaces |

Passes 1, 2, 4 and 5 needing nothing is not luck: they are exactly the
passes that are already provably free of ordering constraints against each
other, which is the same property as being purely local.

---

## 6. GOTCHAs and lessons this work must respect

Grouped by where they came from. Each is a real failure already paid for
once; the note says why it bites *this* work specifically.

### 6.1 From the failed unwrap attempt

- **Separators migrate across boundaries.** Pass 3 moving a comma is correct
  inside one list and catastrophic across a comment edge. The child stream
  makes this structurally impossible — but only if extraction never splits
  one logical field list across the boundary.
- **Terminator re-anchoring.** Pass 6 relocates a lone `;` onto the
  preceding real token. In a child stream it must never see the parent's
  tokens as candidates, and vice versa.
- **Re-run comma growth.** The styled output must re-extract to the same
  child stream. This is the one corruption the architecture does **not**
  kill for free — see §7.
- **Multi-line expressions.** A commented expression spanning lines is
  several `//` tokens whose parens balance only across the whole run. The
  extraction unit must therefore be a contiguous **run** of comment lines,
  never a single line. This is what fixes the 2/4 unbalanced bodies for free.

### 6.2 From the passes' own history

- **Pass 3 idempotence is defined by the line break, not line numbers.**
  Only depth-0 separators move, and only when a newline separates the comma
  from the following field. Comparing source line numbers instead breaks
  single-line field lists — a phantom change plus a leaked VOID every run.
- **Pass 6 must read the WHOLE leading whitespace run.** Earlier passes void
  in place rather than deleting, so a line's leading gap can arrive as
  `WS VOID WS`. Reading only the WS nearest the line start stranded the
  earlier fragment and miscounted blank lines — this was the entire §7.5
  non-idempotence bug (11 drifting lines, fixed 2026-08-20).
- **Pass 6's line-1 insertion must be deferred.** Line 1 usually has no
  preceding WS token; inserting one mid-loop shifts every later original
  index and silently corrupts the rest of the file.
- **Pass 7 must exclude the WS token it is about to rewrite from its own
  width sum.** Including it made a second run measure the first run's tab
  padding as content and re-pad forever — 220/1583 phantom changes.
- **Pass 7 must tolerate no whitespace at all before `AS`.** Real unstyled
  input contains `"field"as[alias]`; the column maths assumed a WS token was
  always there and crashed.
- **`find_block_structure` classifies a comma-led field by what the comma
  separates, not by the comma's own position.** After pass 3 runs, a field
  line's first token is the comma, which falls between segments — every
  comma-led field but the first misclassified as `statement`.
- **`find_block_structure` never matches on raw text.** `for` appears 1,242
  times in `script.qvs` and about 28 are the keyword. Typed tokens hide the
  rest. Any new extraction predicate must be token-typed for the same reason.
- **A `;` alone on its own line ends the PREVIOUS statement**, not a new one
  at itself.
- **Statement `IF … THEN` vs function `IF(`** is told apart by a depth-0
  `THEN` on the line, never by a following paren.

### 6.3 From pass 8, worth carrying forward

- **The WS token carrying a line break also carries the NEXT line's
  indentation.** Treating the token after the break as the line start drops
  the indent and measures every column two tabs short. `.scf_line_text_before()`
  exists solely for this and its logic must survive in some form.
- **`//` occupies columns 0–1**, which the first tab stop absorbs — this is
  why commented and live fields can share a column at all.
- **A commented FIRST field takes the two-space pad, not a leading comma**,
  or uncommenting it yields `LOAD , [X]`.
- **The shared column is the MODAL live column, not the max.** Pass 7
  aligns a block to one column and excludes outliers, so the mode is the
  block's real column; the max chases an outlier.

### 6.4 Architectural invariants that still bind

- Never grow a token's `$text` to inject syntax — later passes recognise
  syntax by token *type*. Use `splice_tokens()`. (Pass 8 is the sanctioned
  exception: it rewrites text that stays inside a comment, where nothing
  reads it as syntax. A child stream re-tokenizes instead, so the exception
  may no longer be needed — check before relying on it.)
- Never delete a row — `void_token()` blanks in place so indices stay valid.
- Collect edits during a pass, apply in ONE call.
- Quoted, bracketed and comment tokens are opaque.
- Every pass skips `SELECT … ;`.
- Pass order constraints (DESIGN §2.5): 3 before 4, 3 before 6, all of 1–6
  before 7, 7 before 8. A recursive driver must preserve these *within* each
  stream.
- Block comments `/* */` can span lines and wrap live code — stay excluded.
- `///$tab` section lines are never touched.

### 6.5 Method lessons from this repo, 2026-08-20

- **Capture an output baseline BEFORE the work and diff after.** This is how
  the `undelimit()` recycling bug was caught: a scalar `type` against a
  vector of tokens silently returned ONE value instead of N, surfacing only
  as a hardcoded-GUID count going 2 → 0 in a **submittable report**. No
  error, no warning. `verify.R` would not have caught it.
- **`ifelse()` returns a value shaped like its TEST.** A scalar where a
  vector was meant truncates silently.
- **Small fixtures under-test structural claims.** `formatexample.txt`
  (2,399 chars) reported that pass ordering had exactly one constraint, with
  zero counterexamples across all 720 permutations. GMR needed two. A
  further refinement held for 300 random full orders on GMR and failed on
  the first order tried against `script.qvs`. **Validate this work against
  `script.qvs`, not only the two smaller fixtures.**
- **Duplication that must stay gets documented at both ends** (see
  `verify.R`'s `.unquote` vs `undelimit()`).
- **A failure buys a mechanism**, not a paragraph.

---

## 7. The one risk the architecture does not remove

Extraction must be a fixed point:

```
extract -> style -> serialize -> extract   ==   extract
```

If styling adds a leading comma and the next extraction reads that comma as
part of the field body, the run adds another — corruption #3 from the failed
attempt, reproduced in a new architecture. This needs its own explicit
mechanism, most likely a normalising extraction that strips a leading
separator before styling and re-supplies it on serialize, so the field body
is always in the same canonical shape whichever run sees it.

**This is the single highest-risk item in the plan and should be built and
tested first, before any pass is touched.**

---

## 8. Verification — a new script, not an extension of `verify.R`

`verify.R` is already too slow to grow (Adam, 2026-08-20). It also has a
different job: it checks that styling preserves meaning. This work needs a
check that the sub-stream contract holds, which is a different property with
a different failure mode.

Proposal: `verify_substream.R`, standalone, gated on exit status, runnable
without `--stage3` on the small fixtures.

What it must assert:

- **Round-trip.** Every child stream serializes back to text that
  re-tokenizes to itself.
- **Fixed point.** `extract → style → serialize → extract` is stable — the
  §7 risk, asserted directly.
- **Parent isolation.** Every non-COMMENT token in the parent is identical
  before and after; the parent row count is unchanged. This is pass 8's
  current invariant and it must survive the redesign intact — it is the
  whole safety story in one line.
- **No boundary crossing.** No token present in a child stream appears in
  the parent's live token set, and vice versa.
- **Comment bodies still parse.** Each restyled line re-reads as a single
  comment; uncommenting a styled field yields a line that would load.
- **Coverage counters.** How many comment runs were extracted, styled,
  refused, and why — so a regression that silently stops styling things
  shows up as a number, not as silence. (`verify.R`'s check count doing
  exactly this is what proved the `undelimit` consolidation lost nothing.)

Runtime budget: it should run on `formatexample.txt` and GMR in seconds. The
`script.qvs` run can be gated behind a flag the same way `--stage3` is.

---

## 9. Decisions needed before any code

These are Adam's, and the plan cannot be costed without them.

1. **Does a commented-out field participate in setting the LIVE alignment
   column?** Symmetry says yes (a commented field is a field). Caution says
   no (dead content should not push live fields around). Pass 8 today takes
   the live column as given and never widens it.
2. **Scope of "commented-out table".** Just a commented `LOAD` with its
   label, field list, `FROM` and terminator? Or also commented control flow
   (`IF`/`FOR` blocks), `DROP`, `STORE`, and bare `SQL SELECT`?
3. **Interleaved prose.** A commented-out table block often contains a human
   note among the commented fields. Does the run break at the prose line
   (two child streams), or is the note carried through unstyled inside one?
4. **What happens to a commented-out block that will not parse?** Refuse the
   whole run and leave it byte-identical, or style the part that parses?
   Refusing whole is safer and easier to assert.
5. **Does pass 8 get deleted or kept as the fields-in-live-lists path?**
   Deleting is cleaner; keeping is a smaller change with two code paths.

---

## 10. Adjacent findings, not part of this work

- **A sixth twin.** `.scf_col()` (pass 8) and `.eaa_tab_col()` (pass 7) are
  the same tab-stop column arithmetic — `.scf_col(s)` is exactly
  `.eaa_tab_col(s, 0L)` — and `.SCF_TAB` duplicates `.eaa_tab_width`. Pass
  8's own comment records this as a deliberate call made before the
  no-duplicate-twins rule. If pass 8 survives this work, promote them; if it
  is deleted, the twin goes with it.
- **`verify_docs.R` has grown iteratively** alongside the pipeline and could
  be reworked more elegantly (Adam, 2026-08-20). Its hardcoded five-file
  `doc_files` list is why this document is unlinted — which is correct here,
  but it means the linter's coverage is a list to maintain rather than a
  rule. Worth revisiting as its own task.
- **Pass 8 is the most expensive pass**: 2.88s of the 7.02s pipeline on
  `script.qvs` (41%) for 504 of 20,035 changes (2.5%), because it builds a
  fresh synthetic stream per field. A whole-run child stream should reduce
  this substantially — a secondary benefit, not a justification.

---

## 11. Suggested sequencing

0. Capture an output baseline for both fixtures and all phase-2 tooling.
1. Resolve the §9 decisions.
2. Build and test the extraction/serialization fixed point in isolation
   (§7) — no pass touched.
3. Build `verify_substream.R` against that, before the passes change.
4. Thread `context` through passes 3, 6 and 7, live behaviour unchanged;
   confirm byte-identical output on both fixtures.
5. Add the recursive driver.
6. Delete pass 8 (or reduce it), fold the twin.
7. Staged testing per CLAUDE.md: formatexample → GMR → `script.qvs` +
   `verify.R --stage3`, each on Adam's sign-off.
