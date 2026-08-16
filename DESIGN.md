# Design notes

Architecture, the decisions behind it and the evidence for them, verified Qlik
behaviours, and what is still to build. See [README.md](README.md) for how to
run and extend the tooling.

---

## 1. Verified Qlik behaviours

The most valuable material here. None of it is in Qlik's documentation; each
was established by running the snippet in Qlik and observing the result. If
any of it is ever in doubt, re-run the snippet rather than trusting this file.

### 1.1 A bracketed or quoted field name may contain anything, including what looks like a function call

```qlik
LOAD
    [year(2012)] AS [A],     // valid - bracketed field reference
    'year(2012)' AS [B],     // valid - single-quoted field reference
    "year(2012)" AS [C]      // valid - double-quoted field reference
FROM x;
```

Verified in Qlik, 2026-08-14. Unusual in practice, but legal — which is
exactly why find-and-replace in the Qlik script editor is unsafe for
reformatting, and why the tooling works on a token stream instead. All three
forms tokenize as single opaque tokens (`BRACKET`, `SQUOTE`, `DQUOTE`), so a
pass that only inspects `WORD` tokens cannot touch them.

### 1.2 A single-quoted token immediately right of `AS` is a field reference, not a literal

```qlik
LOAD [streetname] AS 'address' FROM x;   // 'address' names the field
```

Verified by testing; not documented by Qlik. Everywhere else a single-quoted
token is a string literal:

```qlik
LOAD 'Overdue' AS [Bill Status] FROM x;  // 'Overdue' is a literal value
```

This asymmetry is why `enforce_bracket_references` converts single quotes to
brackets **only** in alias position, and why `ensure_explicit_aliases` refuses
to self-alias a bare single-quoted token.

### 1.3 Keywords are case-insensitive; field and variable names are not

From Qlik Cloud help, *Script statements and keywords*: "All script keywords
can be typed with any combination of lower case and upper case characters.
Field and variable names used in the statements are however case sensitive."

This is what makes the casing pass's call-position guard a **correctness**
requirement rather than a nicety. `app-unbuilt/script.qvs` line 837 contains:

```qlik
Year as [Data x Reg Year]
```

`Year` there is a field reference, not a call. Uppercasing it would rename the
field.

### 1.4 Connector statements embed a foreign, case-sensitive DSL

```qlik
SQL SELECT [OfficeFrom_OfficeTo_Distance]
FROM Closest(costUnit='Kilometers', distance='1000', dataset1='OfficeFrom')
DATASOURCE OfficeFrom INLINE tableName='Offices', tableFields='Office,Location',
    geometryType='POINT', loadDistinct='YES', crs='Auto' {$(...)}
;
```

The camelCase parameter names belong to the GeoAnalytics connector, not to
Qlik, and must not be reformatted. They sit inside a `SQL SELECT ... ;`
statement, so the existing "skip SELECT" convention already covers them — 100%
of the connector parameters in the stress fixture fall inside a skipped
region.

### 1.5 An un-aliased field and a self-aliased field load identically

```qlik
[T1]: LOAD * INLINE [ A, B
1, 2 ];
[T2]: NOCONCATENATE LOAD A, B         RESIDENT T1;
[T3]: NOCONCATENATE LOAD A AS A, B AS B RESIDENT T1;
```

T2 and T3 produce identical field names and values. Verified in Qlik,
2026-08-14, including with `SET HideSuffix = '%'` active, which matters
because so many keys in these scripts end in `Id%`.

This is the assumption `ensure_explicit_aliases` rests on, and it is the pass
that changes the most — +47,217 characters on the stress fixture, more than
every other pass combined. It is also encoded in `verify.R`, which collapses
`X AS X` to `X` on both sides of the equivalence check; if this were false,
that check would pass while the script's behaviour had changed.

### 1.6 Statement `IF ... THEN` and function `IF(...)` share a keyword

In the stress fixture: 132 `if` tokens but only 15 `then`. So roughly 117 are
the *function* and 15 are control flow. Any pass that treats `IF` structurally
(indentation, block nesting) must distinguish them; the casing pass does not
need to, because both become `IF`.

---

## 2. Architecture

### 2.1 Token stream, not text

A script is tokenized once into `data.frame(text, type, line)` and every pass
consumes and returns that stream. Concatenating `$text` reproduces the source
exactly.

The alternative — regex over text, or re-parsing between passes — fails on
this input for a concrete reason: Qlik scripts are full of strings, comments
and bracketed names containing characters that look like syntax. §1.1 is the
sharpest example. Tokenizing once makes those regions structurally opaque,
which is a stronger guarantee than any amount of careful regex.

Token types: `COMMENT`, `DQUOTE`, `SQUOTE`, `BRACKET`, `WS`, `WORD`, `NUMBER`,
`OPERATOR`, `COMMA`, `SEMI`, `LPAREN`, `RPAREN`, `VOID`, `OTHER`.

### 2.2 Shared scanners

`find_load_segments()` walks LOAD field lists and returns per-field segments
with their content token indices, whether they already have an `AS`, and the
alias target. Passes that work per-field share it rather than re-deriving it.

It is also where the `SELECT ... ;` skip lives, so every pass inherits the
same definition of "not ours to touch".

### 2.3 `SELECT ... ;` is the universal escape hatch

An emergent principle worth stating once: a Qlik script is not homogeneous.
`SELECT` statements carry foreign syntax — raw SQL against a `LIB CONNECT TO`
source, and connector DSLs (§1.4) — whose identifiers may be case-sensitive
and whose formatting conventions are not Qlik's.

Every pass skips these regions. In the stress fixture they are 22% of all
tokens. Any new pass should skip them too unless it has a specific reason not
to.

### 2.4 Structural edits go through two helpers

- `void_token()` blanks a token in place rather than deleting the row.
  Deleting shifts every index computed from the original stream — segment
  bounds, other insertions in the same pass — and does so silently.
- `splice_tokens()` inserts new tokens after given indices. Passes must not
  append syntax onto an existing token's text: later passes recognise syntax
  by token *type*, so `" AS X"` glued onto an identifier is invisible to them
  and will be missed or re-added.

Both take vectors. Collect indices during a pass and apply them in one call.

---

## 3. Decisions and evidence

### 3.1 Tokenizing runs in byte mode

**Decision:** match with `useBytes = TRUE` and a UTF-8-aware catch-all, then
re-mark extracted tokens as UTF-8.

**Why:** on a string R knows contains non-ASCII, PCRE reports byte offsets
while R reports character offsets, so `gregexpr()` converts every match
position and `regmatches()` walks the string from the start for every token —
both O(n) per match, quadratic overall.

**Evidence:** on a 216 KB region of the stress fixture, `gregexpr` +
`regmatches` cost 6.30s in character mode versus 0.0085s in byte mode — 746×.
Stripping non-ASCII instead gave 0.0079s, i.e. identical, confirming the cause
is the offset conversion and not the regex.

**The trap:** this is a **cliff, not a gradient**. The whole script is one
string, so a *single* smart quote anywhere drags every token onto the slow
path. Full-file tokenize went 37.7s → 0.35s. The 204 non-ASCII lines came from
prose pasted out of Word into inline data.

Naive `useBytes` is not enough: `.` then matches one *byte*, shattering each
multibyte character into invalid-UTF-8 fragments that break `tolower()` in the
passes. Hence the explicit UTF-8 character alternative.

Nothing is normalised — the token stream is identical to character mode and
the round-trip stays exact.

### 3.2 Data frames are built once, never row by row

**Decision:** hoist columns to plain vectors, accumulate into preallocated
atomic vectors, construct the `data.frame` once at the end.

**Why:** three variants of the same anti-pattern dominated runtime —
`splice_tokens` extracting n single-row data.frames and `rbind`-ing them,
`$<-` on a data.frame inside a loop, and a `data.frame()` per iteration for
the change log.

**Evidence:** pipeline 45s → 1.5s. The change log alone was pure bookkeeping
costing more than the pass's real work — 10% of pass 3 was `sub()` called from
`trimws()` inside it.

### 3.3 Casing uses a call-position guard

**Decision:** statement keywords uppercase on the word alone; built-in
functions uppercase only when the next non-trivia token is `(`.

**Why:** §1.3 — field names are case-sensitive, and many function names are
plausible field names.

**Evidence:** the guard protects the real `Year as [...]` at line 837.
Measured across both fixtures, every keyword-spelled token in field-content
position was a genuine keyword (`as`, `like`, `and`, `or`, `distinct`), never
a field name — so the unconditional keyword rule is safe here. The pass still
warns if a keyword-spelled token appears bare in a field list, so the rare
dangerous case surfaces instead of passing silently.

User-defined `SUB` names are in neither list and keep their casing —
`CALL InvalidInlineData(...)` appears 15 times in call position and is
untouched. A position-only rule would have mangled it.

### 3.4 Line breaks, indentation and blank lines are ONE pass

**Decision:** a single "vertical layout" pass owns all newline-bearing
whitespace, rather than three passes.

**Why:** they are not separable in this token model. The tokenizer matches
whitespace *runs*, so `"\n\n    "` is **one token** carrying a line break, a
blank line and an indent simultaneously. Three passes would each rewrite the
same tokens, order-dependently — separation of files, not of concerns. There
is also a logical dependency: indentation is only correct until something
moves a line break.

**Evidence** (stress fixture, 11,711 whitespace tokens):

| | count |
|---|---:|
| carry a newline **and** indentation together | 4,642 |
| contain blank lines (2+ newlines) | 358 |
| no newline at all (intra-line) | 3,528 |

The 3,528 intra-line tokens *are* a disjoint set, so **intra-line spacing
stays a separate pass**. The merge applies only to the vertical three.

### 3.5 Indentation is limited to statement and field level

**Decision:** normalise indentation of statements and LOAD field lines; leave
continuation lines of multi-line expressions as hand-formatted.

**Why:** the fixture contains hand-aligned nested `IF` chains up to ~23 levels
deep. Re-indenting those rewrites nearly every line, discards alignment that
may carry meaning, and makes the diff unreviewable. The block scanner should
be built so expression-level indenting can be added later without rework.

### 3.6 Indentation uses tabs, at width 4

**Decision:** emit tab characters, and accept that alias alignment (§4.6) is
only correct when rendered at tab width 4.

**Why:** hand-editing a space-padded file means re-padding by hand every time
a field name changes length. Tabs keep that ergonomic.

**The cost, measured:** tab-run alignment cannot hold at two tab widths at
once. A tab does not advance a fixed distance — it advances to the *next
multiple* of the tab width. Short lines therefore need many tabs and long
lines few, so when the width changes they diverge:

| field | tabs needed to reach col 40 @ width 4 | lands @ width 4 | lands @ width 8 |
|---|---:|---:|---:|
| `, [A]` | 9 | 40 | 72 |
| `, IF(x > 1, 'yes', 'no')` | 4 | 40 | 56 |

The drift is `(difference in tab count) × 4`. Choosing a target column that is
a multiple of 8 does **not** help — tested at 40, 48, 56, 64, 80, 96 and 136,
every one aligns at width 4 and breaks at width 8. Only a line needing zero
tabs is width-independent, and if every line needed the same tab count there
would be no alignment happening.

So this is a genuine trade, not an oversight: tabs for editing ergonomics,
paid for with single-width alignment.

**Escape hatch if that proves annoying:** tabs for the structural indent,
spaces only for the padding before `AS`. Indent width stays adjustable and the
alias column becomes stable everywhere. Also worth knowing that most editors
can insert spaces on a Tab keypress, which would give identical typing effort
with portable output.

**Verified:** the Qlik Cloud data load editor renders at tab width 4 (checked
by Adam, 2026-08-14), which is where these scripts are actually read. One
residual risk: if that is a per-user preference rather than a tenant default,
a colleague could still see 8.

Expect the breakage to show up outside Qlik. It was first noticed by editing
in Notepad++ (4) and viewing in notepad.exe, which uses 8 — the file was
fine, the viewer was not. A tab-aligned script only looks right in tools set
to 4, so diff tools, terminals and web views will show it ragged.

### 3.7 Verification compares token streams, not a parsed model

**Decision:** `verify.R` compares a flat canonical token stream. It does not
build a structural model of the script — LOAD blocks, their fields, aliases
and sources — and compare that.

**Why it would be redundant:** `canonical_stream()` is a total ordering of
every meaning-carrying token, so any insertion, deletion, substitution or
reordering changes it. A structural model is **lossier**: it abstracts a LOAD
block into (source, fields, aliases) and discards whatever it judges
incidental — and everything it discards is something it can no longer detect
a change in. The flat stream discards only the five differences declared
permissible in §1, each of them verified.

**Why it would be worse:** a parser is a thing that can be *wrong*. If it
mis-parses a preceding LOAD, a `RESIDENT` with an inline `WHERE`, or one of
the `SQL SELECT ... DATASOURCE` blocks, it yields a confident but incorrect
model — and both sides of the comparison agree, because the same bug ran
twice. That is worse than no check: it manufactures false confidence.
`canonical_stream()` has no model to be wrong about. It is derived
mechanically from a tokenizer already proven lossless by round-trip, which is
why it can be trusted on 13,870 lines of syntax nobody has enumerated.

**But phases 2 and 3 will need one anyway** (§5). `verify.R` cannot verify
retargeting — that phase changes field names on purpose, so equivalence is
violated by design, and the only automated check disappears exactly when
changes start affecting data. A structural model supports the assurance that
phase actually needs: *every difference corresponds to an entry in the rename
map, and nothing else changed.* Phase 3 needs it too — deciding a table is
unused means knowing which tables exist, what they expose and how they join,
cross-referenced against `objects/*.json`.

Build it when phase 2 starts, against phase 2's real requirements rather than
a guess at them. Note that it would be the first component whose correctness
cannot be established by round-tripping, so it needs its own validation
strategy — probably "every token in the source is accounted for by exactly
one part of the model", which is round-tripping in disguise.

---

## 4. Target format specification

Derived from `formatexample.txt` — a hand-written before/after illustration —
plus the decisions taken from it on 2026-08-14. The illustration is the
*intent*; this section is the rule. Where the two disagree, this wins: the
example was hand-edited and contained known slips (a missed quote conversion,
a double space, trailing whitespace).

Each rule notes the pass that implements it, or that none does yet.

### 4.1 Casing — implemented (`enforce_reserved_word_case`)

- Statement keywords, prefixes and clause words: UPPER.
- Built-in functions: UPPER, **in call position only**.
- Field names, variable names and user-defined `SUB` names: untouched.

### 4.2 References — implemented (`enforce_bracket_references`)

All field and alias references in square brackets.

### 4.3 Aliasing — implemented (`ensure_explicit_aliases`)

Every field carries an explicit `AS` alias.

### 4.4 Field separators — implemented (`enforce_leading_commas`)

Field-separator commas lead rather than trail.

The pass relocates the comma and nothing else, on purpose. The rest of the
`, [Field]` shape is owned elsewhere, and deliberately so — duplicating a rule
in two passes lets the two copies drift:

| part | owned by |
|---|---|
| moving the comma to the front | this pass |
| the space around it | §4.7 intra-line spacing — "no space before, one space after every comma" is universal, so it catches relocated separators and function arguments with one mechanism |
| the two-space pad on the first field | §4.5/§4.6 — that is column alignment, not a comma rule; there is no comma there to space |

**Trailing commas before a terminator** are reported, not fixed. The pass
warns when a comma has no following field to attach to — real instance at
`[Grant Managing Region].txt:257`, where a preceding LOAD ends
`... AS 'Performance Review Completed in Last 7 Days',` immediately before its
`;`. That is production script that reloads, so the tooling flags it for a
human rather than silently rewriting it.

This is also a quiet argument for the leading-comma style itself: with
trailing commas, deleting the last field in a list orphans a separator on the
line above. With leading commas, deleting any line — including a
commented-out one (§4.10) — can never orphan anything, because each line
carries its own separator. The comment-removal pass gets that for free.

### 4.5 Indentation — not implemented

Tabs, **tab width 4**. Not spaces (see §3.6).

| element | indent |
|---|---|
| table label, `LOAD`, `FROM`, statement prefixes | 1 tab |
| field lines | 2 tabs |
| true developer comments | **column 0** |

Comments sit at column 0 deliberately, so they stand out against the indented
code. That only reads as a signal because commented-out code is removed
(§4.10) — otherwise dead field references would compete for the same
attention.

### 4.6 Alias alignment — not implemented

Within a LOAD block, every `AS` is aligned to one column. The column is sized
from the widest field line **in that block**, so one enormous expression
widens only its own block rather than the whole file.

### 4.7 Intra-line spacing — implemented (`enforce_intraline_spacing`)

- **No space before, exactly one space after every comma.** Widened from
  "one space after" alone — agreed with Adam 2026-08-17. The extra half
  is not needed for the reason originally assumed (see §6.1, "Other traps"),
  but is justified directly: 16 real hand-typed instances in
  `[Grant Managing Region].txt`, e.g. line 25's `'GWA','KIM' ,'SA'`.
- One space either side of every binary operator: arithmetic (`+ - * /`),
  comparison (`= <> > < >= <=`), concatenation (`&`), logical
  (`AND OR NOT`). The logical words need no special handling — they are
  `WORD` tokens, and two `WORD`s can't be lexically adjacent without
  whitespace between them in the first place.
- **No space immediately inside parentheses** — `IF(x, 1, 0)`, never
  `IF( x, 1, 0 )`.
- Two cases the token type cannot settle, resolved by context — see §6.1 for
  the classifiers and the live counts they were checked against: **unary
  minus** (`AddMonths(Today(), -12)` — the `-` binds to the literal and
  takes no left space) and the **`LOAD *` wildcard**, which is typed
  `OPERATOR` but is not one.
- Any other run of 2+ spaces in content collapses to one.

That collapse rule needs one exception, narrower than originally expected.
The concern was that a naive "collapse everything" rule would eat the
two-space alignment pad in front of a LOAD block's first field — which did
happen once, in ten lines of throwaway code, while repairing
`formatexample.txt` by hand. It turned out **not to need special-casing**:
that pad sits directly after the block's indentation, and the tokenizer
merges a run of whitespace characters into one token, so the pad and its
preceding newline are the *same* token — already out of this pass's scope
per §3.4. Nothing to decompose.

The one place that genuinely needs an exception is the gap after a LOAD
field-separator comma (the depth-0 comma `enforce_leading_commas` relocates)
— that is where a future alignment pass's column padding will live. An
*existing* whitespace token there is never shrunk by the collapse rule, even
though a *missing* one still gets a single space inserted by the comma rule
above.

Confirmed against real scripts: `[Grant Managing Region].txt` lines 17, 78
and 420 (all `as  [`) collapse to `as [`; a second run reports zero changes
on both fixtures.

### 4.8 Vertical spacing — not implemented

- No blank lines inside a statement.
- Exactly **two** blank lines between statements.

### 4.9 FROM clause — not implemented

Path, format spec and terminating semicolon on one line:
`FROM [lib://...](qvd);`

### 4.10 Comments — not implemented

- Commented-out code is removed, each removal logged in `$changes` with the
  **full original text** (not a preview) so it is auditable and revertible.
- A comment counts as dead code only when **both** hold: it sits inside a
  LOAD field list, **and** its body parses as a field reference or expression
  (optionally with an `AS` alias, optionally with a trailing comma). Both
  conditions are required so that prose notes written between fields survive.
- Surviving comments move to column 0 (§4.5).

### 4.11 Not yet specified

The worked example covers a single `LOAD ... FROM` statement. These have no
rule yet and must not be guessed at:

- `RESIDENT` loads.
- Prefix lines — `LEFT JOIN`, `CONCATENATE`, `LEFT KEEP ([Table])`: own line?
  what indent?
- Preceding LOAD (the `LOAD *, ... ; LOAD ...` pattern, which appears in the
  real script).
- Control-flow blocks — `FOR`/`NEXT`, `SUB`/`END SUB`, `IF`/`THEN`/`ENDIF`:
  indent by nesting depth, or flat?
- `SET` / `LET` statements.
- `///$tab` section markers.
- Blank-line rules around `SELECT` blocks, whose interiors are otherwise
  skipped entirely (§2.3).

---

## 5. The migration in three phases

The tooling here is phase 1 of three. Which phase a change belongs to decides
what may be verified about it, so the split matters more than it looks.

| phase | what it does | changes data? |
|---|---|---|
| **1. Style** | the passes in this repo | **no** — output must run like-for-like |
| **2. Pruning** | drop tables and loads that nothing in the app actually uses | yes |
| **3. Retargeting** | qvd paths and field names that have changed in the new environment | **yes**, deliberately |

Pruning comes before retargeting — decided 2026-08-14, reversing the original
order. Retargeting a table is work; retargeting a table that is then deleted
is wasted work, and considerably worse where the source resource no longer
exists and would have to be rebuilt purely to satisfy a reference nothing
needs. Delete first, retarget only what survives.

A supporting reason: pruning is decided by cross-referencing the script
against `objects/*.json`, and those two agree with each other *today*. To the
extent retargeting changes app-facing field names, doing it first pushes the
script and the app definition out of step and makes the usage analysis harder
than it need be.

The non-ASCII question (§6.4) is separate again, and remains deferred.

What the three phases deliberately do *not* fix — the constructs carried
across as-is to hit the deadline — is recorded for handover in §7.

### Phase 1 runs first, and not only for tidiness

The obvious reason is that it produces a checkpoint: a cleaned-up script that
should behave identically on the existing on-prem instance.

The stronger reason is that it makes phase 2 **safer**. Before phase 1 a field
reference can appear four ways — `"Field"`, `'Field'`, `[Field]`, bare
`Field` — with or without an alias. After it, exactly one: `[Field] AS
[Alias]`. A retargeting tool that has to match four variants has four chances
to miss one, in the phase that actually alters data.

### The style pipeline runs LAST in every phase

Not just phase 1. Retargeting changes field name lengths, which disturbs the
alias alignment (§4.6), so style has to be reapplied afterwards — and the same
goes for pruning dropping whole loads.

Reapplying **only** alignment would be the tempting shortcut, and is wrong: it
assumes the later phases emit output that is perfectly style-conformant except
for widths. That assumption would have to stay true forever, in a second tool,
with nothing checking it. Every pass is idempotent and the whole pipeline is
1.6s on the 13,870-line fixture, so re-running all of it costs nothing and
removes the assumption.

Think of it as `gofmt` after a refactor — not a step to be sequenced
carefully, a normaliser applied unconditionally afterwards.

### Phase 1 is the only phase `verify.R` can check

Semantic equivalence is violated by design in phases 2 and 3. So the
automated safety net disappears exactly when changes start affecting data,
which is why those phases need a structural model of their own (§3.7) and why
it is worth keeping each phase a separate tool with its own change log and
separately reviewable output.

---

## 6. Roadmap

Two passes remain from the original list. Grouping follows §3.4; the target
they implement is §4.

### 6.1 Intra-line spacing — implemented (`enforce_intraline_spacing`), 2026-08-17

**The rules are settled — see §4.7.**

Scope: whitespace tokens containing **no newline** (3,528 of 11,711 in the
stress fixture). Newline-bearing whitespace belongs to the layout pass (§3.4)
and is not touched here — this is also what protects the not-yet-built
alignment pass's territory, for free (§4.7).

#### The tokenizer groundwork is already done

Until 2026-08-14 the tokenizer split operators and numbers into single
characters — `>=` was two tokens, `30` was two — so "one space either side of
each operator" would have produced `> =` and `3 0`. That is fixed: `NUMBER`
and `OPERATOR` are now token types and multi-character operators match as
single tokens (option 1 of the two that were on the table; the alternative was
grouping `OTHER` runs privately inside this pass).

The change was verified behaviour-neutral — pipeline output byte-identical on
both fixtures, round-trip intact, full suite green — because nothing outside
the tokenizer ever branched on `OTHER`.

So the pass can rely on:

| source | tokens |
|---|---|
| `a >= 1` | `WORD(a) OPERATOR(>=) NUMBER(1)` |
| `Today()-30` | `... RPAREN OPERATOR(-) NUMBER(30)` |
| `[A] & ', '` | `BRACKET([A]) OPERATOR(&) SQUOTE(', ')` |
| `$(vFoo)` | `OTHER($) LPAREN( WORD(vFoo) RPAREN)` |

Note `$` stays `OTHER` deliberately: a rule that spaces `OPERATOR` tokens
therefore cannot break variable expansion.

**Two things the token type cannot settle, resolved by context:**

- **Unary minus.** `AddMonths(Today(), -12)` — the `-` takes a space on its
  left only when it is binary. Classified by the *previous non-trivia
  token*: binary if that token could end a value — `NUMBER`, `BRACKET`,
  `DQUOTE`, `SQUOTE`, `RPAREN`, or a `WORD` that is not a Qlik keyword
  (reusing `QLIK_KEYWORDS`, since `AND`/`OR`/`THEN`/... cannot end a value
  either) — unary otherwise (previous is `LPAREN`, `COMMA`, `SEMI`, another
  `OPERATOR`, a keyword-`WORD`, or there is no previous token at all).
  `Today()-30`'s `-` has `RPAREN` before it → binary; the comma before
  `AddMonths`'s `-12` → unary. Confirmed against `app-unbuilt/script.qvs`:
  of 24 `-` operators, 17 binary and 7 unary — matches the count above
  exactly.
- **The `LOAD *` wildcard**, typed `OPERATOR` but not one. Classified by
  *position*, not content: a `*` is the wildcard iff its previous
  non-trivia token is the `WORD` `load`, or is `distinct` whose own
  previous non-trivia token is `load` — the only place Qlik syntax allows
  a bare `*` there, so it can never collide with a genuine multiplication
  like `n*chunkSize`. A wildcard gets no operator-spacing rule at all, in
  either direction. Confirmed: of 42 `*` tokens, 31 wildcard and 11
  genuine (`n*chunkSize`, line 1740) — matches exactly.

#### Sanity check of the real script, 2026-08-14

Run against `app-unbuilt/script.qvs` after the tokenizer change:

| check | result |
|---|---|
| adjacent `OPERATOR` pairs | **none** — no multi-char operator is still being split |
| `NUMBER` tokens | 185; no leading-dot decimals, none glued to an adjacent word |
| `$(...)` expansions | 27, **all** simple `$(name)` — no `$(=expression)` |
| bare `.` tokens | 24, **all inside SELECT** — SQL schema qualification, skipped |
| `{` `}` | 14, **all inside SELECT** — GeoAnalytics connector syntax, skipped |
| `/` as division | 15, genuine (`x/(y/100)`, line 920) |
| `&` concatenation | 88 |

Still typed `OTHER`, and correctly so: `:` (136, the table-label terminator —
`[Table]:` must never become `[Table] :`), `$` (27), `.` (24), `{` `}` (14).
None are operators and none should be spaced.

**Count tokens, not text matches.** An earlier draft of this section claimed
494 unary-minus cases, from grepping the raw source. The true figure is 7:
the rest are inside INLINE data blocks and comments, which are single opaque
tokens. Raw-text counts over a Qlik script are inflated by exactly the regions
the tokenizer exists to protect.

#### Other traps, already paid for

- **The two-space pad needs no line decomposition.** Expected to require
  splitting each field line into indent / separator / content (this
  section's original draft, written before the pass existed). It didn't:
  the pad is part of the same whitespace token as its preceding newline, so
  §3.4's scope boundary already excludes it. The one exception that
  survived contact with real code is narrower than a whole "line-leading
  region" — just the gap after a field-separator comma. See §4.7.
- **Skip `SELECT ... ;`** like every other pass (§2.3) — via
  `in_select_region()`, promoted from a private helper in
  `enforce_reserved_word_case` to a shared function in `qlik_tokenizer.R`
  (§2.2), since this pass needed the identical logic and a second private
  copy would drift.
- **Never touch inside a quoted, bracketed or comment token.** Free, as
  expected — the pass only ever rewrites `WS` tokens, never the opaque ones.
- **Voided tokens count as no gap.** A token blanked by an earlier pass
  (`void_token()`) renders as nothing, so "is there already a space here"
  must treat a `VOID` neighbour the same as no token at all, not as
  something occupying the gap.
- **Two rules can want a splice at the same anchor.** A comma directly
  followed by a binary operator with no gap (`,+b`) makes the comma's
  "space after" and the operator's "space before" rules target the same
  physical position. Guarded so it produces one splice and one log entry,
  not two.
- **`find_load_segments()` used to return `seg$start`/`seg$end` as doubles** —
  fixed at source 2026-08-17; the local workaround in this pass is gone. The
  cause was unsuffixed integer literals in its index arithmetic, and the
  reach was wider than first diagnosed: the outer loop's own `i <- i + 1`
  made the counter a double on the first non-`LOAD` token, so *every*
  derived index inherited it. Harmless to arithmetic callers
  (`enforce_leading_commas` does `seg$end + 1`), but fatal to a strict
  `vapply(fn, integer(1))`. **Lesson: a type bug in shared code surfaces at
  an arbitrary caller, so fix it at the source rather than at the call
  site** — the workaround would have been re-invented by the next pass.
  Verified behaviour-neutral: pipeline output byte-identical on both
  fixtures, suite green.

#### Known cases to test against

- Collapse: `[Grant Managing Region].txt` lines 17, 78 and 420, all `as  [`
  — confirmed collapsing to `as [`.
- Preserve: the first field's two-space pad — confirmed automatic (see
  §4.7), by the scope argument plus a synthetic multi-line `LOAD` test, not
  by testing `formatexample.txt` directly (it is not one of `verify.R`'s
  automated fixtures).
- Idempotence: a second run reports zero changes on both fixtures — the
  property `enforce_leading_commas` claimed in its docstring and, at the
  time, did not have.

#### Ordering

Implemented after `enforce_leading_commas` (it supplies the comma that
§4.4/§4.7 rely on) and before `enforce_reserved_word_case`. Casing and
spacing don't otherwise interact — one only rewrites `WORD` text, the other
only touches `WS` tokens — so their relative order was free; layout and
alignment (§6.2 onward) still come after this pass, per the original plan.

#### Finishing

`Rscript verify.R`: 50 checks, 0 failed, across both fixtures — semantic
equivalence, output round-trip and idempotence all hold for the new pass,
alongside every pass before it.

### 6.2 Vertical layout — not started, needs new infrastructure

Line breaks, indentation and blank lines between blocks, as one pass.

This is the first genuinely **global** concern. Every existing pass is local —
look at one field segment and decide. Indentation needs whole-script nesting
depth, which means a new **block-structure scanner**, a sibling to
`find_load_segments()`, tracking:

- control flow: `FOR`/`NEXT` (28/21 in the fixture), `SUB`/`END SUB`,
  `IF`/`THEN`/`ENDIF` (15 statement-level), `DO`/`LOOP`, `SWITCH`/`CASE`
- statement boundaries and `///$tab` sections (63 in the fixture)
- the distinction in §1.6 between statement `IF` and function `IF(`

Note this pass will produce large diffs by nature, which weakens the
reviewability the `$changes` logs otherwise give. Worth deciding how to keep
it auditable before building it.

### 6.3 Commented-out code removal — not started

Remove comments that are commented-out code, logging every removal in
`$changes` so each is auditable and revertible.

The heuristic distinguishing dead code from explanatory prose is the hard
part, and deletion is not recoverable from the output alone — the change log
must carry the full original text, not a preview.

### 6.4 Deferred: non-ASCII data quality

Word-paste artefacts (`’ ‘ “ ” – — •`, non-breaking spaces) are widespread —
204 lines in the stress fixture. Tempting to normalise, but **not all
non-ASCII is an artefact**, and 182 of those 204 lines are live code:

- line 20: `μ` in Qlik's own default `SET NumericalAbbreviation` — the micro
  prefix; changing it alters number formatting
- lines ~1712-1722: an inline table mapping characters to Unicode escapes,
  where the non-ASCII character *is* the data. Normalising `´` to `'`
  collides with the existing `'` row and silently breaks the mapping
- plausibly, legitimate diacritics in Indigenous place and community names

The R-side performance motivation is gone (§3.1 fixed it byte-preservingly).
The open question is whether **Qlik itself** pays a reload-time penalty for
non-ASCII in a script — untested. If it does not, this reduces to a tidiness
pass with real risk, and probably is not worth building. Test that first.

If built: reporting pass first, opt-in rewriting second with an explicit
skip-list. Never couple a data change to a performance fix — it destroys the
ability to tell intended reformatting from normaliser damage in a diff.

---

## 7. The migration debt report — not started, list not yet fleshed out

The three phases deliver a **working** app, not a production-ready one. That
is a deliberate trade, not an oversight: the migration is on a deadline, so
constructs that will run on Cloud but violate the new build standards are
carried across as-is — warts and all — rather than being redesigned in
flight. The bar is that the migrated app works as well as the on-prem
version, and slightly better where pruning removes redundant tables.

The report is the handover document that makes that trade explicit. It
answers one question: **what did we knowingly carry across that should be
addressed before this app goes into production?**

**Style is out of scope.** Everything in §4 is cosmetic and self-evident
from the output. This report is about migration debt, not formatting.

### 7.1 What gets flagged

Not settled. The list below is a seed to be fleshed out before the pass is
built, not a specification:

- inline loads
- direct calls to the database from the load script
- loads from attached files
- hardcoded values in the load script

Before any entry can be implemented it needs two things: a detection rule
expressed against the token stream, and a one-line statement of why it is
debt — the report is read by people deciding what to fix first, so a flag
that cannot justify itself is noise.

### 7.2 The pipeline's output is the priority, not the report

The report itself is a rendering job and can wait; base R can produce HTML,
RTF or even a valid `.docx` with no packages at all. What cannot wait is
that the pipeline **emits the records the report will need**, because those
records are cheap to capture while a file is being processed and expensive
to reconstruct afterwards from output that has already been transformed.

Two constraints follow. Detection is a token-stream query, not a regex over
text (§2.1) — the same reasoning that applies to every other pass applies
here, and bracketed field names (§1.1) will defeat naive text matching.
And every finding must carry enough locality — file, statement, line — to
put a reader on the exact construct. A flag nobody can navigate to is not
actionable.

The pass is **read-only**: it never modifies the script. That keeps it
clear of the §3.4 structural helpers and means it cannot affect
verification.

### 7.3 Run the detection at both ends

Because the pass is read-only and idempotent, running it on both the
incoming script and the final output costs almost nothing — and the
difference is worth having. Phase 2 prunes tables, so a wart inside a load
that gets deleted is not debt in the delivered app. The before/after delta
separates what the migration *fixed* from what it *carried across*, and only
the second list belongs in the handover.

The figure that goes in the report is therefore the one computed on the
final delivered script, not on the input.

The delta is also the only place the migration's **genuine improvements** get
recorded. Almost all of it will be phase 2 removing redundant tables — real
quality gained rather than debt deferred — and nothing else in the tooling
measures it. A handover that lists only what is still wrong understates the
work that was done.

**The baseline must be captured once, not recomputed.** Once the pipeline can
be re-run over a partly hand-fixed script (§7.4), "the input" stops being a
fixed thing. If the before figure is recomputed on each run it converges
silently on the after figure and the improvement record erases itself.
Capture the original on-prem script's findings once, store them alongside the
run, and compare every later run against that stored baseline.

### 7.4 The pipeline must be re-runnable over a hand-fixed script

Some warts will not be resolvable automatically. The expected workflow is
therefore: run the pipeline, hand the report to whoever owns the app, let
them fix what the tooling cannot, and run it again — each pass producing
fewer warts than the last. The style passes already support this: they are
idempotent by design (§3.4) and the whole pipeline costs 1.6s.

What it constrains is the **detector**. If a large inline load is manually
replaced with a curated data source, that new source will not appear in any
list this tooling maintains, and must not need to.

**Detection must therefore be a deny-list, never an allow-list.** Recognise
the debt patterns — inline load, direct database call, attached file,
hardcoded value — and say nothing about anything else. An unrecognised target
is then correct by default: it is simply not one of the things being looked
for, and needs no entry anywhere to pass clean. An allow-list inverts this and
turns every successful manual fix into a false positive, which is exactly the
behaviour that would stop people re-running the tool.

It follows that the detector must not consume phase 3's retargeting lists,
however tempting the reuse looks. Those lists are necessarily incomplete about
the future. The detector must not be.
