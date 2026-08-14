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

### 1.5 Statement `IF ... THEN` and function `IF(...)` share a keyword

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

Token types: `COMMENT`, `DQUOTE`, `SQUOTE`, `BRACKET`, `WS`, `WORD`, `COMMA`,
`SEMI`, `LPAREN`, `RPAREN`, `VOID`, `OTHER`.

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

**Unverified:** what tab width the Qlik Cloud script editor renders at. If it
is not 4, the alignment is decorative in the place these scripts are actually
read.

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
| the space after it | §4.7 intra-line spacing — "one space after every comma" is universal, so it catches relocated separators and function arguments with one mechanism |
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

### 4.7 Intra-line spacing — not implemented

- One space after every comma.
- One space either side of every binary operator: arithmetic (`+ - * /`),
  comparison (`= <> > < >= <=`), concatenation (`&`), logical
  (`AND OR NOT`).
- No double spaces.

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

## 5. Roadmap

Three passes remain from the original list. Grouping follows §3.4; the target
they implement is §4.

### 5.1 Intra-line spacing — not started

Operates only on whitespace tokens containing no newline (3,528 in the stress
fixture), which makes it cleanly independent of layout. Rules still to decide:
space after commas, around operators, inside parentheses.

### 5.2 Vertical layout — not started, needs new infrastructure

Line breaks, indentation and blank lines between blocks, as one pass.

This is the first genuinely **global** concern. Every existing pass is local —
look at one field segment and decide. Indentation needs whole-script nesting
depth, which means a new **block-structure scanner**, a sibling to
`find_load_segments()`, tracking:

- control flow: `FOR`/`NEXT` (28/21 in the fixture), `SUB`/`END SUB`,
  `IF`/`THEN`/`ENDIF` (15 statement-level), `DO`/`LOOP`, `SWITCH`/`CASE`
- statement boundaries and `///$tab` sections (63 in the fixture)
- the distinction in §1.5 between statement `IF` and function `IF(`

Note this pass will produce large diffs by nature, which weakens the
reviewability the `$changes` logs otherwise give. Worth deciding how to keep
it auditable before building it.

### 5.3 Commented-out code removal — not started

Remove comments that are commented-out code, logging every removal in
`$changes` so each is auditable and revertible.

The heuristic distinguishing dead code from explanatory prose is the hard
part, and deletion is not recoverable from the output alone — the change log
must carry the full original text, not a preview.

### 5.4 Deferred: non-ASCII data quality

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
