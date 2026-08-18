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

### 1.7 A bare (unquoted) word is a valid field reference, identically to a quoted or bracketed one

```qlik
LOAD
    Electorate,          // valid - bare field reference, same as [Electorate]
    Fleet.Rego as [Fleet Car Rego]   // valid - bare, dotted, table-qualified
FROM x;
```

Real cases, both previously left un-bracketed by `enforce_bracket_references`
(fixed 2026-08-17): `app-unbuilt/script.qvs` has an entire un-named table
loading `Electorate`, `State`, `Longitude`, `Latitude` bare, and `[Fleet
Cars]:` loads `Fleet.Rego`, `Fleet.Make`, `Fleet.Model` etc. bare. §4.2's
target — "all field and alias references in square brackets" — did not
distinguish bare from quoted, but the pass only ever handled the latter.

This shares its disambiguation problem with §1.3: a bare word that is
**also** a built-in function's name (`Year`) is a field reference when NOT in
call position, and a call when it is. `enforce_bracket_references` now uses
the identical call-position guard `enforce_reserved_word_case` already needed
for the same collision.

**Scope is deliberately narrower than "every bare word":** only inside a
`LOAD` field list. A bare word elsewhere is very often NOT a field — it is a
`FOR` loop counter or a `LET`-assigned variable — and Qlik does not
distinguish those from field references syntactically the way it does for
quoting. `app-unbuilt/script.qvs`'s chunking loop assigns `numRows`,
`chunkSize`, `i`, `chunkText`, `rowNr`: bracketing any of those would turn a
variable reference into a field reference and silently change what the
script loads. WHERE-clause and other non-field-list bare words are left
untouched for the same reason — deliberately out of scope (§4.11).

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

All field and alias references in square brackets — quoted ones and bare
(unquoted) ones alike, the latter scoped to LOAD field-list content only.
See §1.7 for why the scope stops there.

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

### 4.5 Indentation — implemented (`enforce_vertical_layout`)

Tabs, **tab width 4**. Not spaces (see §3.6).

| element | indent |
|---|---|
| table label, `LOAD`, `FROM`, `RESIDENT`, statement prefixes | 1 tab |
| field lines | 2 tabs |
| true developer comments | **column 0** |
| `SET` / `LET` statements | **column 0** |

`RESIDENT` is a source clause and is treated exactly like `FROM` — its own
line at 1 tab, one rule covering both (ratified Adam 2026-08-17 against the
pipeline's existing output; there was never a separate `RESIDENT` rule, only
an unwritten one). Statement prefixes — `LEFT JOIN`, `CONCATENATE`,
`LEFT KEEP ([Table])` — likewise keep their own line at 1 tab, per the row
above.

Comments sit at column 0 deliberately, so they stand out against the indented
code. That only reads as a signal because commented-out code is removed
(§4.10) — otherwise dead field references would compete for the same
attention.

**A LOAD list's true first field gets a two-space pad** after its 2-tab
indent (Adam 2026-08-17, resolving the "owned by §4.5/§4.6" note in §4.4's
table): with leading commas (§4.4), every field but the first starts `, `,
two characters that the first field has nothing to match. The pad keeps the
actual field content aligned in one column regardless of which field starts
the list. Identified structurally — the field sitting right after the `LOAD`
keyword itself — not by "has no leading comma", so it stays correct even
before pass 3 (leading commas) has run.

**Block nesting is flat, not cumulative (decision, Adam 2026-08-17).** A
statement or field line's indent depends only on which row of the table
above applies to it — never on how many `FOR`/`SUB`/`IF`/`DO`/`SWITCH` blocks
enclose it. Chosen over scaling indent per nesting level: simpler to
implement, and avoids compounding the tab-width drift documented in §3.6
(more tabs per line means more drift between width-4 and width-8 viewers).
Neither fixture is evidence either way for this call — `app-unbuilt/` is
unstyled input being tested, not a style reference, and the Grant file has no
control-flow blocks to show a convention — so this is a green-field decision,
not one derived from either.

**Continuation lines get one extra tab.** A line break that falls before a
field's expression is finished — i.e. no depth-0 field-separator comma
follows it, so the next line is not a new field — indents at the field's base
(2 tabs) plus one, flat regardless of how many further line breaks the same
expression contains. This is the one exception to leaving continuation lines
hand-formatted (§3.5): it sets only the line's leading indent, nothing after
it. Confirmed against a real case, `[Grant Managing Region].txt` lines 19-20:

    IF(LEN("PMC Region.PMC Region Code") > 1, 'GMU Region',
    	IF(MATCH("PMC Region.PMC Region Abbr",'FNQLD','GNQLD'),'North Queensland',"PMC Region.PMC Region")) AS [NIAA Region],

### 4.6 Alias alignment — implemented (`enforce_alias_alignment`)

Within a LOAD block, every `AS` is aligned to one column. The column is sized
from the widest field line **in that block**, so one enormous expression
widens only its own block rather than the whole file. Must run LAST in the
pipeline — the column depends on each field's final indentation (§4.5).

**Padding is tabs, always (Adam 2026-08-17, non-negotiable).** The scripts
are hand-edited afterward, and every other indent/pad in this pipeline is
already tabs — this was not treated as a per-feature choice to reconsider.
The tabs are inserted **before** the single space intraline spacing already
guarantees in front of `AS`, not instead of it — that space stays, so `AS`
lands one column past every tab stop, never on it. The known consequence of
tabs-for-alignment (§3.6: tab-run alignment only holds exactly at one tab
width) is an accepted tradeoff, not a defect.

**A wrapped field is measured from the line its `AS` actually sits on, not
excluded (revised 2026-08-17, overriding the original design below — Adam
found a real case, formatexample.txt's `[Programme Id%]` field, where the
wrap's own terminal line ends well under the block's column and should
align like anything else).** Only that final line's own indent + content up
to `AS` counts — the earlier lines of the wrap are irrelevant to the
measurement. Per Adam, this is "full symmetry": a wrapped field's width CAN
widen the block's shared column exactly like a single-line field's can, not
just get squeezed into a column already fixed by others.

**One kind of field is still excluded from a block's column entirely** —
left with its AS exactly as authored, no forced padding:
- A field whose own column (by the measurement above — the wrap's terminal
  line, if it wrapped) reaches **122 characters** or more (Adam 2026-08-17
  — the exact length of a real outlier in formatexample.txt,
  `IF("Grant Activity.Activity Id" = '4-ENJBSQ2', ...)`, not a round
  number). One abnormally long field forcing every short field in the same
  block to pad out to match it looked worse than no alignment at all. The
  column is then set by the widest field *under* that threshold — "move
  onto the next widest column."

A field with no whitespace at all around `AS` (real case, unstyled input:
`"field"as[alias]`) is also excluded — see §4.7's known gap below; that is
a missing rule in an earlier pass, not something this pass should paper
over by inventing spacing on its own.

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

**Fixed 2026-08-17 (found same day):** nothing forced a space around `AS`
itself. The "two `WORD`s can't be adjacent" argument above only protects
`AS` from OTHER words — it says nothing about `AS` sitting directly against
a non-`WORD` token (`BRACKET`, `DQUOTE`, `SQUOTE`, `RPAREN`...), which
tokenizes just fine with zero whitespace. Real case, unstyled input:
`"field"as[alias]` used to survive every earlier pass unchanged apart from
bracketing (§4.2) and casing (§4.1), ending as `[field]AS[alias]` — no
spaces at all. Discovered because it crashed `enforce_alias_alignment`
(§4.6), which assumed a space there always exists; that pass was hardened
not to crash on it, and the actual fix now lives here too: a WORD token
spelled `as` (case-insensitive, this pass runs before casing) outside
`SELECT` gets the same zero-gap insertion treatment as commas and
operators, on both sides. Verified against `formatexample.txt`'s
hand-added test line (`"Grant Activity.Sub Programme"as[Sub Program]`),
stage 1 of the testing methodology now in CLAUDE.md.

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

### 4.8 Vertical spacing — implemented (`enforce_vertical_layout`)

- No blank lines inside a statement.
- Exactly **two** blank lines between statements.
- **Comment attachment (convention call 2026-08-17, confirmed by Adam
  2026-08-17 — reviewed in output, working, no further work):** a
  comment (or contiguous run of comments) immediately before the next real
  statement, with no `///$tab` section in between, is treated as belonging
  to THAT statement — the two-blank-line gap goes above the comment, not
  between the comment and the code it describes. Otherwise the scanner's own
  bookkeeping would put it in the wrong place: a comment inherits the
  *previous* statement's `stmt_id` (it doesn't consume the pending-new-
  statement flag), so a literal "blank lines where `stmt_id` changes" rule
  would separate the comment from what it describes instead of from what
  precedes it. Real case this keeps together, `app-unbuilt/script.qvs`:
  ```
  //These are the records of the actual cars...
  [Fleet Cars]:
  ```
- `///$tab` section markers (Adam, 2026-08-17 — see §6.2): the whole line
  carrying one is left completely untouched. The line immediately AFTER one
  now gets its indentation fixed normally (Adam 2026-08-17, revising the
  original "leave the gap on either side alone" call — a table label left
  at column 0 right after a section marker was wrong); only the blank-line
  *count* of that gap is left exactly as authored, never normalised to the
  standard two.
- `SET`/`LET` directives (Adam 2026-08-17 — indent per §4.5): 0 indent, and the
  blank-line count on EITHER side of one is left exactly as authored —
  never forced to the standard two between statements, never collapsed to
  zero inside a run of consecutive directives. Applies to a directive's own
  continuation lines too (a multi-line SET/LET expression), not just its
  first line.
- `SELECT ... ;` blocks (ratified Adam 2026-08-17): no special rule. A
  `SELECT` is spaced exactly like any other statement — 1 tab, the standard
  two blank lines either side — while its *interior* stays untouched per
  §2.3. The skip is about not rewriting foreign SQL, not about exempting the
  block from the file's vertical rhythm.

### 4.9 FROM clause — implemented (`enforce_vertical_layout`)

Path, format spec and terminating semicolon on one line, one space before
the format spec: `FROM [lib://...] (qvd);` — even though Qlik's own default
export wraps the format spec onto its own line, which this pass now joins
back (Adam 2026-08-17). Detected structurally (the immediately preceding
line starts with the WORD `from`), not by line kind.

**A lone `;` on its own line also joins onto whatever precedes it, tight, no
space (extension, Adam 2026-08-17)** — not only the `(qvd)` case, but the
general one: a `WHERE` clause (or any other statement tail) followed by a
`;` on its own line joins too, even across one or more `//` comment lines
sitting in between. A comment must never end up with the terminator
appended after it (that would silently comment the terminator out forever),
so when a comment is in the way the `;` is physically relocated to sit right
after the last real token instead of just having its surrounding whitespace
collapsed — the one place in this pass that moves a content token rather
than only rewriting whitespace text. Confirmed against
`[Grant Managing Region].txt`'s two real shapes: the simple `(qvd)\n;` case
and the `WHERE ... \n// comment\n;` case (around line 240).

**A preceding LOAD's terminator is the same case, not a special one (ratified
Adam 2026-08-17).** In the `LOAD *, ... ; LOAD ...` chain the first LOAD has
no `FROM`, so its lone `;` is all that marks the boundary between the two
statements — but the general rule above already covers it, and it joins onto
the last field line (`, [A] + 1 AS [C];`). Deliberately no carve-out: the
boundary stays visible through §4.8's blank lines, not through a stranded
terminator.

### 4.10 Comments — not implemented

- Commented-out code is removed, each removal logged in `$changes` with the
  **full original text** (not a preview) so it is auditable and revertible.
- A comment counts as dead code only when **both** hold: it sits inside a
  LOAD field list, **and** its body parses as a field reference or expression
  (optionally with an `AS` alias, optionally with a trailing comma). Both
  conditions are required so that prose notes written between fields survive.
- Surviving comments move to column 0 (§4.5).

### 4.11 Out of scope

**Bracketing bare words outside a LOAD field list** — `WHERE` clauses and
other non-field-list positions — is deliberately out of scope (§1, and
`enforce_bracket_references`'s own header): a bare word there is very often
a variable, not a field — a `FOR` counter or a `LET`-assigned name — and
bracketing one silently changes what the script loads. Reopening this needs
a rule that can tell the two apart before anything is touched.

This section holds settled scope exclusions only. Open questions live in
STATE.md's "Open items" — nowhere else — and `verify_docs.R` enforces that.
(This section was previously an embedded to-do list; it rotted and re-opened
settled decisions. See fc72e32.)

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
3.8s end to end on the 13,870-line fixture (measured 2026-08-17: wall clock
for the whole `Rscript run_pipeline.R` invocation, R startup and sourcing
included), so re-running all of it costs nothing and removes the assumption.

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

### 6.2 Vertical layout — implemented (`enforce_vertical_layout`)

Line breaks, indentation and blank lines between blocks, as one pass. The
block-structure scanner is `find_block_structure()`; the pass consuming it is
`enforce_vertical_layout()` (both 2026-08-17, self-tested in verify.R).

**A real bug turned up building the pass, in the already-committed scanner:**
a `;` sitting alone on its own source line — common style, real case
`[Grant Managing Region].txt:11` — was miscounted as already having ended
its statement by the time its OWN line was processed, because the pending-
new-statement check compared semicolon counts *inclusive* of the current
line's own first token. That made the semicolon's own line look like the
START of a new statement rather than the END of the one it closes, which
surfaced as visibly wrong output: the `;` landing on its own indented line
with a phantom two-blank-line gap floating above it. Fixed by counting
semicolons strictly *before* each line's first token instead of at-or-before
it. Regression-tested (`verify.R`, "a ';' alone on its own line stays part
of the statement it closes").

This is the first genuinely **global** concern. Every existing pass is local —
look at one field segment and decide. Statement/field boundaries, blank-line
rules and continuation-line detection need whole-script structure, which
means a new **block-structure scanner**, a sibling to `find_load_segments()`,
tracking:

- control flow: `FOR`/`NEXT` (28/21 in the fixture), `SUB`/`END SUB`,
  `IF`/`THEN`/`ENDIF` (15 statement-level), `DO`/`LOOP`, `SWITCH`/`CASE`
- statement boundaries and `///$tab` sections (63 in the fixture)
- the distinction in §1.6 between statement `IF` and function `IF(`
- continuation lines: a line break before a field's expression is complete
  (no depth-0 comma follows) — needed for the indent rule in §4.5

Indentation itself does **not** scale with this nesting (decision, §4.5) —
the scanner's depth tracking is for finding these boundaries, not for
setting indent width.

That makes the five block types far cheaper than they look. Because indent is
flat, no rule anywhere needs to know *which* kind of block encloses a line;
block tracking exists purely so §4.8's blank lines don't get injected between
the statements of a loop body. So all five collapse to **one depth counter
driven by a word list** — no per-type logic, and nothing gained by scoping
the pass to only the two types a fixture happens to contain. The single
genuine subtlety is statement `IF` vs function `IF(` above, which is
unavoidable either way.

**Decision (Adam, 2026-08-17): accept the large diff.** This pass rewrites
nearly every whitespace token by nature, which breaks the per-change
reviewability the other five passes give via `$changes`. Rather than build a
coarser change log or a preview/diff mode, `verify.R`'s equivalence check is
the audit mechanism here — it already detects and describes a single
insertion/deletion instead of reporting the whole cascade.

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

### 6.5 Phase 2 usage extraction — not started

Phase 2 (§5) drops loads nothing in the app uses. That verdict needs a usage
graph — field to dimension, measure or chart. Discovery over `app-unbuilt/`
on 2026-08-17 established that the graph is recoverable, and how.

**Two app exports, two roles** (Adam, 2026-08-17). `app-unbuilt/` was picked
as a hard case for styling: one enormous script (13,870 lines), few sheets,
and many unused tables. `app2-unbuilt/` is an app already migrated BY HAND
and known to work — a 25 KB script but many more sheets and charts, with
redundant tables carried across as-is. It is far more representative of the
apps still to migrate, and the better phase-2 fixture: 22 JSON files against
9. The two apps load **separate data models and share nothing**, so pruning a
table from one cannot affect the other; usage analysis is strictly per app.
Phase 2 tooling runs against both.

**app2's existing formatting is an input, never a reference.** The styling
pipeline can and will be run over `app2-unbuilt/script.qvs` — it is a
perfectly good styling input, and a useful one. What it is not is *evidence*:
its layout was applied by hand and loosely, so it must never be used to
argue for a change or an addition to the passes. §4 is signed off and
closed. Seeing something in a fixture that our rules would format
differently is not a finding and not a task — the same rule that already
applies to the other fixtures, which are inputs and not specs.

**That restriction is about STYLE, and only style.** Phase 2 is the opposite
case: the app metadata is precisely what its design should be derived from,
and working out what that metadata can tell us is the task itself. Infer
structure, meaning and relationships from `app*-unbuilt/` freely here. The
styling-era caution — that a fixture's appearance never justifies a rule —
does not transfer to phase 2 and must not be applied to it.

**What the app metadata holds (app-unbuilt).** Two sheets carry every visual;
the story, `appprops`, `loadmodel` and `pinneditems` carry none.
`measures.json` is empty, so there are no master measures and every measure
is inline in a chart. `dimensions.json` holds 3 master dimensions (7 field
defs), and only one `qLibraryId` is referenced anywhere. Master-item
indirection is therefore negligible **in this app only** — app 2 does ship
master measures, so the extractor cannot assume the indirection away.
`variables.json` holds 94 variables.

**Qlik's internal stashes can be read or skipped freely.** Of 166 `qMeasures`
containers, 142 are empty; the 54 real ones all carry a non-empty
`qDef$qDef`, 50 of them distinct. 52 sit in live chart definitions and 2
inside `qUndoExclude`/`qLayoutExclude` — and both of those also appear live,
so the stashes contribute zero unique usage. Reading them costs nothing and
skipping them loses nothing, so the extractor need not tell them apart.

**Do not enumerate keys — scan every string.** Expressions live in at least
`qMeasures`, `qDimensions`, `qFieldDefs`, `qExpression`,
`qAttributeExpressions`, `qCalcCondition`, `qListObjectDef`,
`qLabelExpression` and `qSortCriterias`. Enumerating them is exactly how the
first attempt went wrong: `qFieldDefs` alone yields 48 unique field names,
while scanning all 408 strings in the objects yields **90** unique bracketed
references. Half the usage was missed, in the direction that deletes a field
a chart is using. Collecting every string leaf can only over-report, and
over-reporting under-prunes — the safe failure.

**Consequence: no JSON parser is needed.** Because the structure is never
consulted, base R needs only a string-literal extractor over the raw text
(quoted runs, honouring `\"`) rather than a parser and state machine. That
keeps the base-R-only deployment constraint cheap to satisfy.

**Ambiguity resolves conservatively** (Adam, 2026-08-17). A bare word in an
expression may be a field or one of the 94 variables — the same discriminator
§4.11 lacks. Phase 2 does not solve it: a bare word counts as USED and is
flagged for review. Pruning targets whole loads only, so a load is droppable
only when nothing it produces is referenced anywhere.

**Double quotes mean something different here** (Adam, 2026-08-18). §1.2's
rule — Qlik has no double-quoted string literal, so a `"..."` is always a
field reference — was established on script text and does not hold in app
JSON. Every `DQUOTE` in `app-unbuilt/objects` is a selection value
(`"2022-23"`, `"Yes"`), not a field. They are still collected, because a
wrong exclusion is the failure that deletes a live field, but they are
recorded as their own kind so the report never asserts that `Yes` is a
field. A rule verified in one of this project's two languages is not
thereby verified in the other.

**A LOAD is not a table** (verified against both apps, 2026-08-18). Step 3's
first model — one LOAD produces one table — is wrong five ways, and each had
to be handled before the produced-field list meant anything:

- a **preceding chain**: stacked LOADs, one table, named by the TOP one. The
  bottom statement supplies the data, the top one exposes the fields.
- **JOIN / CONCATENATE**: feeds a table someone else created, so it can never
  be judged droppable on its own.
- **MAPPING**: consumed by `ApplyMap` and dropped; never in the data model.
- **`LOAD ...; SELECT ...;`**: the ODBC form, where the statement below feeds
  the load. All 24 of app-unbuilt's sourceless loads are this, none are
  preceding chains — the two look identical until the next statement is read.
- **a wildcard qvd path** (`FROM .../*.qvd`): Qlik loads every matching file
  and names each resulting table after its own qvd, taking all of that qvd's
  fields (Adam, 2026-08-18). One statement, N tables, none of them named in
  the script.

**A preceding `LOAD *` draws from the statement below, not from the source**
(Adam, 2026-08-18). Given `LOAD *` over `LOAD [year], [quarter] FROM big.qvd`,
the `*` yields exactly those two fields — not everything in the qvd. The
chain's field universe is set by its bottom LOAD's own list. This is why a
wildcard is far less often unknowable than it first appears: of app-unbuilt's
19 apparent unknowns, 14 were `INLINE` (the header row states the fields), 2
were `RESIDENT` (resolvable once every table is known), and only the 3
wildcard paths are genuinely outside the script.

**Variables are expression text too.** A variable expanded with `$()` can
itself name fields, so `variables.json` is scanned as expressions, not merely
as a list of names.

**The deliverable is a document, not console output** (Adam, 2026-08-18).
Phase 2's output is a field/table usage report submittable as part of the
migration project. It stays in phase 2 rather than becoming a phase of its
own: phase 3 already means retargeting (§5), the report *is* step 4 below
rather than a new deliverable, and the review gate and the submission must be
the same artefact — reviewing a console dump and building the document later
from a separate path signs off something other than what is submitted.

**Build order.** Each step produces separately reviewable output, and nothing
in phase 2 deletes anything:

1. JSON string-literal extractor, base R. `jsonlite` is installed on the
   development machine and serves as a test oracle — the base-R reader must
   return the same strings on the real files.
2. App usage: every string through `qlik_tokenizer.R`, giving bracketed and
   bare references, the bare ones flagged.
3. Script side: `find_load_segments()` for what each LOAD produces.
4. Cross-reference, in two layers:
   - **4a, the usage table.** A structured intermediate — one row per load
     and produced field, carrying app, script line range, table, field, how
     the reference was spelled (`bracketed` / `quoted` / `bare`, the last two
     qualified as below) and evidence. Its per-table and per-field categories
     are the five defined under "What the cross-reference distinguishes".
     Greppable and diffable on its own, which is what is wanted when a
     category looks wrong.
   - **4b, the renderer.** Turns 4a into the document. Base R, in a format
     already verified here (HTML / RTF / CSV, or the base-R `.docx` writer).
     Built once and reused for §7's debt report, which has the same shape:
     findings, evidence, and why each matters.

   Adam reviews the report before any pruning tool is written.

**What the cross-reference distinguishes** (Adam, 2026-08-18). Five outcomes,
and they are categories rather than a ranking. Phase 2 **detects and reports**;
it prescribes nothing. Whether a removal or a `DROP` is automated here or left
to a developer is undecided, so no category carries a recommended action — the
value is in separating the cases at all, since a reviewer cannot see any of
them from the script by eye.

Per table:

| category | condition |
|---|---|
| referenced | at least one of its fields appears in the app's own definition |
| build-only, retained | no app reference; another statement reads it; no `DROP` disposes of it, so it occupies memory for the rest of the reload |
| build-only, dropped | no app reference; another statement reads it; a `DROP` disposes of it — scaffolding behaving as intended |
| unreferenced | loaded, then named nowhere at all: no join, no later load, no chart |
| undetermined | a wildcard qvd path; its contents are outside the script |

Per field, within a referenced table: **unreferenced field** — the field is
loaded and appears in no expression anywhere.

The three inputs are `app_used` (any field of the table appears in
`app_usage.R`'s references, in any kind), `script_used` (a `script_refs.R`
mention whose enclosing statement is not a `DROP`), and `dropped`.

**Five encoding rules the categories depend on.** Each was a choice, and each
moves the boundary of `unreferenced`:

- **Case is folded at the join, and only there.** Steps 2 and 3 preserve it.
  Folding over-reports usage, which under-prunes — the recoverable direction.
- **A string that is not an expression is a field name in its entirety.**
  `qFieldDefs` holds UNBRACKETED names, so treating every string as expression
  text and splitting it on whitespace destroys them: `Latest Funding Financial
  Year` became four bare words, the field was never emitted, and its table was
  reported unreferenced. That is the one direction that deletes something
  live. Such strings are now emitted whole, as kind `whole-string`, alongside
  the per-word bare refs. The test is structural — no bracket, quote, paren,
  comma or semicolon anywhere in the string. It is deliberately loose: a chart
  title that happens to equal a field name will match, which over-reports
  usage and under-prunes.
- **A `quoted` match counts as app usage**, even though app JSON's double
  quotes are literals (`"Yes"`). A field genuinely named `Yes` would be hidden
  from the unreferenced list rather than wrongly added to it.
- **A `bare` match counts as app usage**, per the ambiguity rule above. This
  is the dominant effect on the result: app-unbuilt yields 1050 unique bare
  references against 100 bracketed.
- **The unit is a TABLE, not a load.** A table comprises its whole preceding
  chain plus every `joins-into` load feeding it, so a finding is a set of line
  ranges rather than one.
- **Two things are excluded from verdicts**: `multi-table` loads entirely, and
  field-level findings for any table whose `complete_fields` is FALSE.

Position is not consulted. A table the app never references is idle from its
last script read onward wherever that read sits, so the distinction that
matters is whether a `DROP` exists at all, not where it is.

**References carry provenance, not just names.** A set of names is enough to
decide pruning but not enough to publish: the document must answer *why do
you say this is unused?*, so every reference records the JSON file and the
string it was found in, from step 2 onward. Retrofitting provenance after the
fact means rewriting step 2.

Step 3 also settles the caveat on those 90 bracketed names: a few may be
variables or master-dimension labels rather than fields, and only the
script's own field list distinguishes them.

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

Before any entry can be implemented it needs two things: a detection rule
expressed against the token stream, and a one-line statement of why it is
debt — the report is read by people deciding what to fix first, so a flag
that cannot justify itself is noise.

Two entries now have both, from Adam 2026-08-18, and phase 2's step 3 already
detects them — `script_loads.R` emits the signal, §7 decides what to say
about it. Note these are a **separate concern from pruning**: an inline load
or a database call is debt whether or not anything references it.

| flag | detection | why it is debt |
|---|---|---|
| direct database call | `source_kind == "select"` — the `LOAD ...; SELECT ...;` form | **Not permitted** by the team's Cloud build standards, and will not work there by design. Blocking, not cosmetic. 24 in app-unbuilt, 0 in app2. |
| oversized inline load | `source_kind == "inline"` with a large `inline_rows` | Inline loads are legitimate for mapping and crosstabs, but not for importing raw data. A large block is a CSV that was pasted into the script. app-unbuilt's largest is **1113 rows** (`SiteVisitSurvey`); app2's is 33 (`RegionMap`, a genuine mapping table). |

Row count is what separates the two inline cases, which is why step 3 carries
`inline_rows` rather than a bare inline flag; the report states the count and
the threshold is a styling-free judgement recorded in STATE.md.

Remaining seed entries, each still owing both halves above:

- loads from attached files
- hardcoded values in the load script

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
