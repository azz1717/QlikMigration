# INTERFACES — what each script exposes

The contract map for `C:\Rtools`. Read this instead of reading source. Keep an
entry current in the same commit that changes its function.

## Conventions

- Every pass is `f(tokens) -> list(tokens, warnings, changes)`. `$changes`
  columns differ per pass and are listed in each entry.
- Token stream is `data.frame(text, type, line)`; concatenating `$text`
  reproduces the source exactly. Types: `COMMENT DQUOTE SQUOTE BRACKET WS
  WORD NUMBER OPERATOR COMMA SEMI LPAREN RPAREN VOID OTHER`.
- Trivia = `WS`, `COMMENT`, `VOID`.
- Quoted, bracketed and comment tokens are **opaque** — never edit inside
  one. A space inserted into `[Grant Activity]` renames a field.
- Never grow a token's `$text` to inject syntax; later passes recognise
  syntax by token *type*. Use `splice_tokens()`.
- Never delete a row — `void_token()` blanks in place so indices stay valid.
- Collect edits during a pass, apply in ONE `void_token()` / `splice_tokens()`
  call each. Per-row data.frame writes dominate the cost otherwise.
- Every pass skips `SELECT ... ;` (foreign SQL / connector DSL).
- New passes follow README's "Adding a pass" template.
- **Before writing any helper, check the shared scanners in
  `qlik_tokenizer.R` below — promote a private one rather than duplicating.**
  This already paid off once: `in_select_region()` was private to the casing
  pass until the spacing pass needed it.

## qlik_tokenizer.R — tokenizer, serializer, shared scanners (source first)

- `read_qlik_script(path, encoding="UTF-8") -> tokens` — read + tokenize; sets attr "source_path".
- `tokenize_qlik(text) -> data.frame(text, type, line)` — single string in. GOTCHA: byte-mode
  PCRE; alternation order in .qlik_token_pattern is significant (longest first, UTF-8 catch-all last).
- `detokenize(tokens) -> character lines` — exact inverse; restores a trailing blank line.
- `splice_tokens(tokens, insertions)` — insert AFTER given ORIGINAL indices; insertions = named
  list keyed by index-as-string, values data.frame(text,type,line). Never grow $text in place.
- `void_token(tokens, idx)` — blank rows instead of deleting, so indices stay valid. VOID = trivia.
- `in_select_region(type, lower) -> logical` — TRUE inside SELECT...; regions (every pass skips these).
- `prev_non_trivia_idx(type) -> integer/NA per position` — previous non-WS/COMMENT/VOID index.
- `next_non_trivia_idx(type) -> integer/NA per position` — next non-WS/COMMENT/VOID index. Symmetric
  with the above. The call-position test (`t_type[next_non_trivia_idx(t_type)] == "LPAREN"`) is
  shared by casing and bracket references — promoted here 2026-08-17 when a second pass needed it.
- `find_load_segments(tokens) -> list(segments, warnings)` — per-field segments of every LOAD list;
  segment: start, end, content_idx, has_as, as_idx, alias_content_idx, line, load_tok_idx — all
  integer except has_as (logical). Skips SELECT.
  GOTCHA: its index arithmetic uses `1L` literals deliberately; an unsuffixed `1` silently makes
  start/end doubles and breaks callers doing `vapply(..., integer(1))`. Fixed 2026-08-17.
  `as_idx`/`load_tok_idx` added 2026-08-17 for the alias alignment pass — `load_tok_idx` is the
  same value for every field of one LOAD list, so segments sharing it are one alignment block.
- `find_block_structure(tokens) -> list(lines, warnings)` — classifies every LINE, for the vertical
  layout pass. `lines` = data.frame(idx, line, kind, starts_stmt, stmt_id, depth, first_field), one
  row per line start; kind is `statement`/`field`/`continuation`/`comment`/`directive`/`section`.
  Reuses find_load_segments() for field boundaries: a segment's FIRST line start is the field, any
  later one is a continuation. Indentation is FLAT (DESIGN §4.5) so `kind` alone determines it —
  `depth` and `stmt_id` exist only for §4.8's blank-line rule, where a whole block is ONE statement,
  its closer included. `first_field` is TRUE only for the field sitting right after the LOAD keyword
  itself (DESIGN §4.4/§4.5's two-space alignment pad).
  GOTCHA: block keywords count only as a statement's FIRST word. That single guard is what stops
  `EXIT FOR WHEN ...;` opening a phantom block, with no per-keyword special-casing.
  GOTCHA: statement `IF` is told from function `IF(` by a depth-0 `THEN` on the line, NOT by a
  following `(` — `IF (x = 1) THEN` is legal. DESIGN §1.6.
  GOTCHA: never match on raw text. `for` appears 1242 times in app-unbuilt/script.qvs and ~28 are
  the keyword; the rest are inside comments and literals, which typed tokens hide.
  GOTCHA (bug, fixed 2026-08-17): a `;` alone on its own source line (real case, `[Grant Managing
  Region].txt:11`) must count as ending the PREVIOUS statement, not starting a new one at itself.
  The pending-new-statement check counts depth-0 semicolons strictly BEFORE each line's first
  token, not at-or-before — using an inclusive count made the semicolon's own line look like it
  came after its own statement had already ended.
  GOTCHA (bug, fixed 2026-08-17): after pass 3 relocates a field separator to lead its line, that
  line's own first token is the COMMA, not the field's content — and content_idx never includes the
  leading comma, so the comma's own index fell in the gap BETWEEN two segments and every comma-led
  field but the first misclassified as `statement` (1 tab) instead of `field` (2 tabs). Fixed by
  classifying on what the comma actually separates (`next_non_trivia_idx()`) rather than its own
  position — depth>0 commas (mid-expression, never moved by pass 3) are unaffected.
  DESIGN §4.5/§4.8 (Adam 2026-08-17): a SET/LET statement, and any of its own continuation lines, is
  kind `directive` — 0 indent, blank lines on EITHER side left exactly as authored, never forced to
  the standard two or collapsed to zero. Detected once per top-level statement (`pending`), not
  per-token, so it survives a multi-line SET/LET expression.
- Private: `.qlik_token_type` (token text -> type name), `.qlik_block_opener`, `.qlik_block_closer`
  (first word(s) of a statement -> block kind, or NA).

## qlik_reserved_words.R — vocabulary, data only, no logic

- `QLIK_KEYWORDS` — statement/control/prefix/clause words. Safe to uppercase unconditionally.
- `QLIK_FUNCTIONS` — built-ins. Uppercase ONLY in call position (next non-trivia token is `(`),
  because field names are case-sensitive and many share a function's name.
- A word may be in both lists (Left, Right, Replace, Keep, Join, First...). Keywords win.
- Also serves as "can this token end a value?" — the spacing pass uses non-membership of
  QLIK_KEYWORDS to tell a value-ending WORD from `AND`/`OR`/`THEN`.

## ensure_explicit_aliases.R — pass 1

- `ensure_explicit_aliases(tokens)` — gives every un-aliased simple reference an alias equal to
  its own text (`X` -> `X AS X`). `$changes`: line, kind, before, after.
- Only a lone DQUOTE/BRACKET/WORD gets one. Wildcard `*` skipped; a bare SQUOTE is a literal,
  not a reference, so it is NOT self-aliased; un-aliased expressions are warned, not guessed at.
- GOTCHA: `X AS X` is behaviour-neutral in Qlik (DESIGN §1.5) — that is what licenses this.
- Private: `.alias_kind` (token type -> human label for the change log).

## enforce_bracket_references.R — pass 2

- `enforce_bracket_references(tokens)` — quoted AND bare references become `[bracketed]`.
  `$changes`: line, kind ("double-quoted field" / "single-quoted alias" / "bare reference"),
  before, after.
- DQUOTE is ALWAYS a field reference in Qlik (no double-quoted string literal exists) -> always
  converted. SQUOTE is a literal EXCEPT immediately right of a field's `AS`, where it is an
  alias reference (confirmed by testing, undocumented — DESIGN §1.2).
- GOTCHA: content containing `]` cannot be bracketed (no escape in Qlik) — left quoted + warned;
  cannot happen for the bare-word branch, since `]` is not in the tokenizer's WORD character class.
- Bare WORDs are bracketed too (added 2026-08-17, DESIGN §4.2/§1.7), but ONLY inside a LOAD field
  list (`find_load_segments()` content_idx) and only when NOT a QLIK_KEYWORDS member and NOT in
  call position (`next_non_trivia_idx` next token is `(`) — the same guard `enforce_reserved_word_
  case.R` uses, and for the identical reason: a field can share a function's name (`Year`).
- GOTCHA: scope stops at the field list on purpose. Bracketing a bare word ANYWHERE (a `FOR` loop
  counter, a `LET`-assigned variable) would silently turn a variable reference into a field
  reference. Real names this would have broken: `numRows`, `chunkSize`, `i`, `chunkText`, `rowNr`
  in `app-unbuilt/script.qvs`'s chunking loop.
- `verify.R`'s `canonical_stream` mirrors this exact scope+rule to fold a bare field and its
  bracketed form to the same canonical entry — see its comment block before changing either side.
- Private: `.unescape_bracketable` (strip quotes, collapse doubled quote chars).

## enforce_leading_commas.R — pass 3

- `enforce_leading_commas(tokens)` — field-separator commas move from trailing to leading.
  `$changes`: from_line, to_line, field_preview.
- Only depth-0 separators move, and only when a LINE BREAK separates the comma from the field
  after it — that definition is what makes the pass idempotent.
- GOTCHA: comparing source line numbers instead of testing for the newline breaks on
  single-line field lists (phantom change + leaked VOID every run). Don't reintroduce it.
- Trailing comma with no following field (real case at `[Grant Managing Region].txt:257`) is
  warned, not rewritten — it is production script that reloads.

## enforce_intraline_spacing.R — pass 4

- `enforce_intraline_spacing(tokens)` — comma/operator/paren spacing within a line.
  `$changes`: line, kind ("insert" / "remove" / "collapse"), detail.
- Scope is whitespace tokens with NO newline. Newline-bearing WS belongs to the layout pass
  (DESIGN §3.4) — which is also what keeps the alias-alignment pad safe for free.
- Rules: no space before / exactly one after a comma; one space either side of a binary
  operator; none immediately inside parens; other 2+ space runs collapse to one.
- GOTCHA: the gap after a LOAD field-separator comma is the alignment pass's future column
  padding — an existing gap there is never shrunk, though a missing one is still filled.
- Context classifiers (DESIGN §6.1 has the counts): unary minus = previous non-trivia token
  cannot end a value; `LOAD *` wildcard = previous non-trivia token is `load`, or `distinct`
  preceded by `load`. A wildcard gets no spacing rule in either direction.
- Private: `.na_false` (NA -> FALSE in a logical mask).
- **Fixed 2026-08-17:** a rule now forces a space around the `AS` keyword the same way commas and
  operators get one — insertion, not just collapse. Two WORD tokens can't be lexically adjacent
  without whitespace already between them, which is why every OTHER keyword-adjacency case was
  implicitly covered — but `AS` sitting directly against a non-WORD token (`BRACKET`, `DQUOTE`,
  `SQUOTE`, `RPAREN`...) had no such guarantee. Real case: `"field"as[alias]` used to survive
  passes 2-6 unchanged apart from bracketing and casing, ending as `[field]AS[alias]` — no spaces
  at all. `as_idx <- which(t_type == "WORD" & lower == "as" & !in_select)`, spliced the same way
  the comma/operator insertion loops already were. `enforce_alias_alignment.R` (pass 7)'s
  no-WS-before-AS exclusion (below) is now unreachable via this specific case but stays as a
  defensive guard for any other future producer of that shape.

## enforce_reserved_word_case.R — pass 5

- `enforce_reserved_word_case(tokens)` — keywords and built-ins to UPPER.
  `$changes`: line, kind ("keyword" / "function"), before, after.
- Keywords uppercase on the word alone; functions ONLY in call position. Only WORD tokens are
  ever considered, so anything quoted/bracketed/commented is structurally invisible.
- GOTCHA: without the call-position guard this renames the real field at
  `app-unbuilt/script.qvs:837` (`Year as [Data x Reg Year]`). User-defined SUB names keep casing.
- Warns when a keyword-spelled token is uppercased bare inside a field list, unless it is one
  that legitimately lives there (AS, AND, OR, NOT, XOR, LIKE, DISTINCT, IF/THEN/ELSE/END).
- Call position uses the shared `next_non_trivia_idx()` (qlik_tokenizer.R) — no private helper
  here any more; it was promoted 2026-08-17 when `enforce_bracket_references.R` needed the same test.

## enforce_vertical_layout.R — pass 6

- `enforce_vertical_layout(tokens)` — indentation and blank lines. DESIGN §3.4/§4.5/§4.8/§4.9/§6.2.
  Consumes `find_block_structure()`. `$changes`: line, kind (the line's own kind — statement /
  field / continuation / comment / directive, never section), before, after — one row per rewritten
  gap.
- Indent is FLAT (DESIGN §4.5): statement 1 tab, field 2, continuation 3, comment column 0,
  directive (SET/LET) 0 — independent of FOR/IF/SUB/DO/SWITCH nesting depth. A LOAD list's true
  first field (`find_block_structure()`'s `first_field`) additionally gets a two-space pad after
  its indent, so it lines up with the ", " of every comma-led field below it.
- Blank lines (DESIGN §4.8): 2 between top-level statements, 0 inside one — driven by
  `find_block_structure()`'s `stmt_id`, which already treats a whole block as ONE statement.
- A leading comment (run) is reattached to the FOLLOWING statement for this purpose — see DESIGN
  §4.8, and note this convention is not yet separately confirmed with Adam.
- `///$tab` section lines are never rewritten (DESIGN §4.8). The line immediately AFTER one now
  gets its indentation fixed normally (Adam 2026-08-17 — a table label left at column 0 was wrong);
  only the blank-line COUNT of that gap is left untouched, reproduced verbatim from the original
  whitespace token rather than recomputed.
- A `directive` (SET/LET) line gets the same blank-line protection on BOTH its incoming and
  outgoing gap — DESIGN §4.5/§4.8, Adam 2026-08-17: 0 indent, but never force/collapse the spacing
  around one.
- FROM clause (DESIGN §4.9, Adam 2026-08-17): the format spec's opening paren — `(qvd)` and
  similar — joins the FROM path on one line, one space between, even when the source has it
  wrapped onto its own line (Qlik's own default export shape). Detected structurally (immediately
  preceding line starts with the WORD `from`), not by kind.
- Trailing `;` (DESIGN §4.9 extension, Adam 2026-08-17): a lone `;` line joins onto whatever
  precedes it — tight, no space — same rationale as the FROM/(qvd) case. Handled in two parts:
  `.qvl_join_orphan_semicolons()` runs FIRST, before `find_block_structure()` is even called, and
  physically relocates the `;` to sit right after the last real (non-trivia) token when one or more
  `//` comment lines separate it from that target — otherwise a plain whitespace collapse would land
  the `;` at the END of the comment's line, silently commenting the terminator out forever. This is
  the one place in this pass that moves a content token rather than only rewriting whitespace text;
  the relocated token gets a fresh SEMI row via `splice_tokens()`, the original is `void_token()`-ed,
  never deleted. After that pre-pass, every remaining lone `;` is "one WS token after its target" —
  the main per-line loop's own `SEMI`-line branch just collapses that WS to `""`, same technique as
  the FROM/(qvd) case.
- GOTCHA: other than the `;`-relocation above, this pass only rewrites the leading-whitespace GAP
  for each line — it never inserts a new line break where the source didn't already have one, and
  never touches anything after a line's first token.
- GOTCHA: line 1 usually has no preceding WS token at all (true of both fixtures). That insertion
  is deferred until AFTER the main loop finishes — doing it mid-loop shifts every later original
  token index by one and silently corrupts the rest of the file (caught before commit, 2026-08-17).
- `canonical_stream` in verify.R needs no new rule for the indent/blank-line rewriting (WS-only,
  already stripped before comparison), but the `;`-relocation is real content movement — verify.R's
  own pass-isolation check (`layout: only WS token text changes...`) now also excludes `SEMI` and
  `VOID` token rows from its before/after comparison, with a separate SEMI-count equality clause to
  still catch a dropped or duplicated one.

## enforce_alias_alignment.R — pass 7

- `enforce_alias_alignment(tokens)` — aligns every field's `AS` to one column within its own LOAD
  block. DESIGN §4.6. Must run LAST, after vertical layout — the column depends on each field's
  FINAL indentation. Consumes `find_load_segments()` (its `load_tok_idx`/`as_idx` fields, added for
  this pass) and `find_block_structure()` (to recognise a genuine one-field-per-line `field` kind
  line, matched by LINE NUMBER not token index — a comma-led field's line-start token is the
  leading comma, not the field's own content token). `$changes`: line, before, after.
- Padding is TABS, always (Adam 2026-08-17, non-negotiable — scripts are hand-edited afterward).
  Inserted BEFORE the existing single space in front of `AS` (guaranteed by intraline spacing), not
  instead of it — `AS` lands one column past every tab stop, never on it.
- Scope is per LOAD block, not per file (DESIGN §4.6): one enormous field only widens its own
  block.
- A field whose content wraps onto another line before `AS` is no longer excluded outright (Adam
  2026-08-17, overriding the original design): its width is measured from the line `AS` itself
  sits on — `all_line_idx` (a lookup over EVERY line, not just `field`-kind ones, built from
  `find_block_structure()`) resolves that line's start token, identically for a normal single-line
  field (where it's the same line) and a wrapped one (where it's the terminal continuation line).
  Only that terminal line's own indent + content counts, and — per Adam, "full symmetry" — it CAN
  widen the block's shared column same as any other field, not just get squeezed into one already
  set by others. It still gets the outlier treatment below if that measurement alone is too wide.
- A field whose own column reaches `.eaa_max_field_width` (122, Adam 2026-08-17 — the exact length
  of a real outlier field in formatexample.txt) or more gets the same exclusion treatment as a
  wrapped field: "move onto the next widest column" means the block max is taken over the
  remaining fields only.
- GOTCHA (bug, fixed 2026-08-17): the content-width sum originally included the WS token
  immediately before `AS` — the exact token this pass rewrites — so a second run measured the
  first run's tab padding as content width and kept re-padding forever (220/1583 phantom changes
  on the real fixtures). That WS token is now explicitly excluded from the width sum.
- GOTCHA (bug, fixed 2026-08-17): a field with NO whitespace at all before `AS` (real case,
  unstyled data in `app-unbuilt/script.qvs`: `"field"as[alias]`) crashed the column math, which
  assumed intraline spacing always leaves a WS token there — true almost everywhere but not
  universal on ugly input. Now excluded from alignment (same as any other malformed/outlier
  field) instead of crashing. Pass 4's gap that made this reachable is now fixed too (same day —
  see its entry above), so this exclusion path is defensive rather than live, but stays in place.
- Private: `.eaa_tab_width`, `.eaa_max_field_width`, `.eaa_tab_col` (tab-aware column expansion),
  `.eaa_preceding_ws_idx` (local duplicate of enforce_vertical_layout.R's helper — both a few
  lines, not worth a shared-scanner entry).

## json_strings.R — phase 2 step 1, NOT a pass and NOT in the pipeline

- Nothing in `run_pipeline.R` sources this; it is phase 2 tooling (DESIGN §6.5).
- `json_string_literals(text) -> data.frame(text, is_key)` — every string literal in one JSON
  document, in document order, unescaped and with surrounding quotes removed. `is_key` marks a
  string that names a member rather than carrying a value.
- `read_json_strings(path) -> ` same, from a file. Line numbers are deliberately not tracked —
  phase 2 reports usage per file, not per line.
- **Deliberately NOT a JSON parser.** Phase 2 never consults document structure (DESIGN §6.5), so
  only the strings are needed — which is what makes the base-R-only constraint cheap. One regex,
  no state machine.
- Byte mode (`useBytes = TRUE`, then re-mark UTF-8), for the same reason `tokenize_qlik()` uses it
  — DESIGN §3.1. The whole file is one string, so one non-ASCII character anywhere would otherwise
  make offset conversion quadratic over every match.
- Key vs value uses the one structural fact that needs no nesting state: in JSON a string followed
  by `:` is a key, and a value never is (a value is followed by `,`, `}` or `]`). The optional
  trailing `[ \t\r\n]*:` is part of the match pattern itself.
- GOTCHA (bug, fixed 2026-08-17): that pattern consumes trailing whitespace for VALUES too, not
  only keys, so the strip must be `ws*:?` — stripping only `ws*:` left every value in a
  pretty-printed file carrying its own closing quote (`appprops"` for `appprops`). All 14 compact
  hand-built cases passed while all 9 real files were wrong, because compact JSON ends the match at
  the quote. Pretty-printed cases are now in the test set. README's "test input must carry the
  property being tested", third instance.
- `\uXXXX` is decoded, including surrogate PAIRS (a character outside the BMP is two escapes;
  decoding each half alone yields an invalid character). Escapes are resolved in ONE left-to-right
  scan, so a literal `\\` is consumed whole and cannot be misread as starting the next escape —
  the classic failure of chaining `gsub()` calls.
- Private: `.json_string_pattern`, `.json_escape_char()`, `.json_unescape_one()`, `.json_unescape()`.
- Verified against `jsonlite` as an oracle on all 9 real `app-unbuilt` JSON files — exact set
  equality for keys and for values, separately. 738 KB parses in 0.04s.
- That cross-check is now a script: `oracle_json_strings.R`, below.

## app_usage.R — phase 2 step 2, NOT a pass and NOT in the pipeline

- Nothing in `run_pipeline.R` sources this; it is phase 2 tooling (DESIGN §6.5). Base R only.
- `expression_references(text) -> data.frame(ref, kind)` — one string's field references, one row
  per occurrence, in order. `kind` is `"bracketed"`, `"quoted"`, `"bare"` or `"whole-string"`.
- `"whole-string"`: a string containing no bracket, quote, paren, comma or semicolon is emitted
  ENTIRE as one reference, as well as word by word. `qFieldDefs` holds unbracketed names, and
  splitting `Latest Funding Financial Year` into four bare words meant its field was never emitted
  and `LatestFinYear` was reported unreferenced — a live table marked deletable. Do not remove
  this without re-running the raw-JSON false-negative check (see usage_report.R).
- `file_references(path, label) -> data.frame(source_file, ref, kind, n)` — one JSON file,
  aggregated per reference. `read_json_strings()` supplies the strings.
- `app_references(dir) -> data.frame(app, source_file, ref, kind, n)` — one app export,
  `source_file` relative to `dir`. Apps are NEVER merged (DESIGN §6.5: separate data models).
- `Rscript app_usage.R [dir ...] [--csv <path>]` — per-app summary; `--csv` writes the full table.
  Current: app-unbuilt 9 files / 9697 refs / 1393 unique (100 bracketed, 8 quoted, 1050 bare,
  662 whole-string); app2-unbuilt 22 files / 6387 / 978 (56 / 0 / 750 / 559).
- Values only — JSON KEY strings are skipped (Adam 2026-08-18). A member name is structure, never
  expression text. This is NOT the mistake DESIGN §6.5 warns about: that one is privileging certain
  keys' VALUES, and every value is still scanned here whatever names it.
- Bare words reuse the two guards from `enforce_bracket_references.R` — not a `QLIK_KEYWORDS`
  member, not in call position (`next_non_trivia_idx` is `LPAREN`) — for the same reason: a field
  can share a function's name.
- Case is NEVER folded. Whether Qlik matches field names case-insensitively is step 4's problem,
  and folding here would destroy the evidence it needs.
- GOTCHA: `expression_references()` must tolerate a length-0 input — `file_references()` seeds its
  `rbind` with one to keep the columns when a file yields nothing.
- DQUOTE is its own kind, `"quoted"` (Adam 2026-08-18). DESIGN §1.2 — no double-quoted string
  literal — holds for SCRIPT text but NOT for app JSON: all 8 in `app-unbuilt/objects` are
  selection values (`"2022-23"`, `"Yes"`). Still collected, since a wrong exclusion deletes a live
  field; kept separate, so the report never claims `Yes` is a field. Do not re-fold into
  `"bracketed"`; step 4 decides what to do with them.
- `asset_usage(dir, script_path = NULL) -> data.frame(kind, id, name, used)` — the DECLARED
  assets (variables, master dimensions, master measures) and whether anything references them.
  `script_path` defaults to the export's own copy. A separate question from field usage above,
  answered differently per kind.
- A master item is matched by its **qId**, never its title: the object that uses one carries
  `qLibraryId`, which holds the id. A variable is matched by NAME, in the app objects AND in the
  script, since either may reference it.
- The token right of `SET`/`LET` is a variable's own definition and does not count as a use —
  without that guard every variable is used by virtue of existing. Everything else counts,
  comments included: over-inclusive on the phase 2 principle that a false 'used' leaves dead
  weight while a false 'unused' deletes something live.
- Current: app-unbuilt 32/94 variables unused, 2/3 master dimensions, 0 measures declared.
  app2-unbuilt 8/15 variables, 4/13 dimensions, 4/5 measures. Spot-checked against the raw
  files: `vTrip`, `vPopWeight`, `vNTAdmin` each appear once in the script (their own SET) and
  nowhere in objects/.
- Private: `.au_undelimit()`, `.au_declared()`, `.au_object_blob()`, `.au_word_hit()`,
  `.au_var_in_script()`.

## script_loads.R — phase 2 step 3, NOT a pass and NOT in the pipeline

- Nothing in `run_pipeline.R` sources this; it is phase 2 tooling (DESIGN §6.5). Base R only.
- `script_loads(tokens) -> list(loads, fields, warnings)`; `read_script_loads(path)` from a file.
  `Rscript script_loads.R <script> [--csv <stem>]` prints a summary and optionally writes
  `<stem>-loads.csv` / `<stem>-fields.csv`.
- `loads`: load_id, table, producer_kind, prefix, source_kind, source, line_start, line_end,
  chain_of, n_declared, inline_rows, complete_fields, tab.
  `fields`: load_id, table, field, line, aliased, via (`declared` / `wildcard`).
  `inlines`: load_id, table, tab, n_rows, header, sample, line_start, line_end — `sample` is the
  first TEN data rows only (DESIGN §7.1: a developer judges an inline load from its header and a
  glimpse; embedding 1113 rows in a skimmable document is counterproductive).
- `tab` comes from the `///$tab Name` markers and is on every load. It is how a developer
  navigates a Qlik script — a line number alone does not locate anything in the Qlik editor.
  55 tabs in app-unbuilt, 6 in app2, every load mapped.
  `load_id` is positional 1..n, the join key, stable only within one parse of one file.
- **A LOAD is not a table** — `producer_kind` is the whole point of this script:
  `table` / `preceding` (stacked LOADs, one table, named by the TOP one) / `joins-into`
  (JOIN or CONCATENATE, feeds a table someone else made) / `mapping` (ApplyMap fodder, never in
  the data model) / `multi-table` (a wildcard qvd PATH, see below).
- `source_kind`: from / resident / inline / autogenerate / select / sql-select / none.
  `select` is the ODBC `LOAD ...; SELECT ...;` form — 24 in app-unbuilt, 0 in app2.
  `sql-select` is a BARE `SQL SELECT` under a label, with no LOAD at all — 4 in app-unbuilt.
  Kept as its own kind, not folded into `select` (Adam 2026-08-18): a database call is the
  most expensive thing to carry to Cloud, so the report must be able to count this form
  separately rather than have it disappear into a total.
- Wildcard resolution: a `LOAD *` takes the fields of whatever feeds it. Preceding -> the
  statement BELOW (NOT the underlying qvd — DESIGN §6.5); RESIDENT -> that table, in a second
  pass once every table is known; INLINE -> the block's header row. `complete_fields` goes FALSE
  only when none of those apply.
- Expects STYLED input; raw parses but every un-aliased field is warned about, since there the
  produced name was inferred rather than read.
- GOTCHA: never bound a lookahead in TOKEN counts. The `LOAD;SELECT` probe was a 40-token window
  and styling inserts whitespace tokens freely — the same statement resolved on raw input and
  silently did not on styled. `.sl_next_solid()` is unbounded for exactly this reason.
- GOTCHA: the head scan (label + prefix) must NOT stop at the previous `;`. Control flow does not
  end in one, so `END IF` / `NEXT chunkText` above a LOAD displaced the label and the table went
  unnamed (6 in app-unbuilt). It also takes a parenthesised group WHOLE — stopping at the comma
  in `CrossTable(Category, Result, 3)` left the head as a lone `)` and lost both prefix and label
  (CrossTab, PMCrossTab). Prefix ARGUMENTS are arbitrary names and are exempt from the
  known-prefix warning.
- GOTCHA: `QLIK_KEYWORDS` is stored **lowercase**. Every pass compares `tolower(text) %in%
  QLIK_KEYWORDS`; a `toupper()` comparison matches nothing at all and the filter becomes a silent
  no-op. That is exactly what happened here — `LOAD DISTINCT *` went undetected as a wildcard and
  the apparent cause (`DISTINCT` missing from the vocabulary) was wrong; it is present, and the
  casing pass does uppercase a lowercase `distinct`. `.SL_QUALIFIERS` survives only for the
  un-aliased-name strip, where removing the whole keyword vocabulary would mangle an expression.
- Bare `SQL SELECT` is modelled by `.sl_bare_selects()`, appended after the LOAD-derived rows
  so the chain logic never sees them. `find_load_segments()` is built on LOAD field lists and
  cannot find one. The label is the `[Name]:` above the SELECT and the fields are its column
  list, up to the depth-0 FROM. app-unbuilt: 4 loads, 3 distinct tables (`ClosestAssociations`
  twice), 21 fields; 167 -> 171 loads and 118 -> 121 tables. app2 unchanged at 42/26.
- Current (STYLED input): app2 42 loads -> 26 tables, 0 warnings. app-unbuilt 171 -> 121 tables,
  7 warnings, all informational (3 wildcard paths, 4 un-aliased `DISTINCT [Field]`). On RAW input
  every un-aliased field warns as well, which is thousands — not a regression, see the note above.
- Private: `.sl_bare_selects()`, `.sl_bare_select_idx()`, `.sl_bare_label()`,
  `.sl_bare_fields()`, `.sl_prev_solid_idx()`, `.sl_undelimit()`, `.sl_next_solid()`, `.sl_solid()`, `.sl_head()`, `.sl_label()`,
  `.sl_stmt_end()`, `.sl_source()`, `.sl_is_wildcard()`, `.sl_inline()`, `.sl_autoname()`,
  `.sl_paren_target()`, `.sl_wild_rows()`, `.sl_empty_loads()`, `.sl_empty_fields()`,
  `.SL_PREFIXES`, `.SL_QUALIFIERS`.

## render_report.R — phase 2 step 4b, the submittable document. NOT in the pipeline

- `render_report(app_dir, script_path, out) -> path`. `Rscript render_report.R <app-dir>
  [--script <p>] [--out <p>]`. Self-contained HTML: no external stylesheet, script, font or
  image, and collapsibles are native `<details>` so no JS is needed.
- HTML not .docx (Adam 2026-08-18): the report must be reviewable on the machine it is written
  on, and .docx is not openable there. The 4a/4b split means any other format is a second
  renderer over the same table, not a rewrite.
- **The information design IS the deliverable** (DESIGN §7.2). The first draft gave every
  detector a section and every section its whole table — rejected as information overload, and
  the rewrite is governed by three rules that must survive any future edit:
  1. **The unit is the TAB, not the finding.** 40 unused tables listed flat hides that 15 of
     them sit in 10 tabs with nothing used in them at all — one cheap decision, not 15
     investigations. `dead` tabs get their own callout above everything else.
  2. **Count work, not findings.** 24 database calls is not 24 jobs: 12 are in one tab. The
     verdict names the concentration.
  3. **Nothing is visible until asked for**, bar the verdict, the three figures and the dead-tab
     list. Tabs are grouped blocking / judgement / cleanup-only, and cleanup-only — the long
     tail — sits behind a single expander.
- Major and minor breaches are separate figures. A database call blocks migration; a hardcoded
  record id keeps the app in `dev` and must go before production sign-off. Folding them together
  told app2 it had "2 blocking items", which was wrong and alarming.
- Visible rows: app-unbuilt 23 expanders + a 10-name list; app2 2 + none. Was 42 rows and
  200-cell tables inline.
- Private: `.h()` (HTML escape), `.tbl()`, `.plural()`, `.badges()`, `.css`.

## script_debt.R — the report's debt signals. NOT in the pipeline

- `guid_literals(tokens, tabs)`, `commented_out_code(tokens, tabs)`, `duplicate_labels(loads)`,
  `sql_targets(tokens, tabs)`. `Rscript script_debt.R <script>` prints all four plus the
  direct-database-call count.
- The admission test for anything here (DESIGN §7.1): **if retargeting fixes it, it is not
  report material.** DEV connection strings are therefore NOT flagged — that is phase 3's job.
- `guid_literals` is the WHOLE of hardcoded-value detection, deliberately. A general rule would
  flag `IF([Status] <> 'Completed')`, which is fine and far commoner than the real thing. A GUID
  in a load expression is always a patched record id. Precision measured: 2 hits in app2 (both
  genuine), 0 false positives across app-unbuilt's 13,869 lines.
- `commented_out_code` counts LINES, not blocks — legibility is the cost. 513 of 13,869 in
  app-unbuilt, 1 of 632 in app2. The code-vs-prose test is a keyword match and approximate by
  design; a few misjudged blocks do not change what a reader takes from the total.
- `duplicate_labels` is an OBSERVATION, not a severity: whether a repeated label is intentional
  concatenation or an accident is not determinable from the script, and this repo has not
  verified Qlik's behaviour here. 9 in app-unbuilt, clustered as tab pairs (`Fleet`/`OLDFleet`).
- `sql_targets` -> data.frame(line, tab, kind, server, schema, object, target). `kind` splits
  `object` (a qualified `"SERVER".schema."Object"`) from `connector` (a connector function
  call such as `Closest(...)`): an object needs a Cloud data connection, a connector call
  needs the connector to exist on Cloud at all, and they are not the same job.
- Token-based for the reason the whole file is: a raw grep of app-unbuilt finds 25 qualified
  FROM targets, one of which is inside a comment. A commented-out SELECT is one COMMENT
  token, so its FROM never becomes a token and cannot be miscounted.
- Current app-unbuilt: 24 object references, 24 distinct, across 2 servers (21 in
  `AZDB-ZEA-PRD-NIAADL01.abs`), plus 4 connector calls (`Closest` 3, `TravelAreas` 1).
  app2: all zero.
- The 28 targets against 24 `select` loads is NOT a discrepancy, and the split is the point:
  the 24 OBJECT references map one-to-one onto the 24 `LOAD ...; SELECT ...;` loads (verified:
  24 distinct owning loads, no duplicates). The 4 CONNECTOR calls are a different statement
  form entirely — a bare `SQL SELECT ... FROM Closest(...)` under an explicit table label, with
  no LOAD above it. `script_loads()` models LOAD statements, so it never sees them.
- Private: `.SD_GUID`, `.SD_CODEISH`, `.sd_split_qualified()` (splits a qualified name from
  the RIGHT, so a bare `Trip` is an object, not a server).

## usage_report.R — phase 2 step 4a, the cross-reference. NOT in the pipeline

- Nothing in `run_pipeline.R` sources this; it is phase 2 tooling (DESIGN §6.5). Base R only.
  Joins `app_usage.R` + `script_loads.R` + `script_refs.R`. Deletes nothing, prescribes nothing.
- `usage_report(app_dir, script_path) -> list(tables, fields, undetermined, stores, warnings)`.
  `script_path` defaults to the export's own copy; pass the STYLED script where you have it.
- `Rscript usage_report.R <app-dir> [--script <path>] [--csv <stem>]`.
- Categories are DESIGN §6.5's five. Current results:
  app2-unbuilt 26 tables — 15 referenced, 9 build-only dropped, 2 unreferenced, 68 unreferenced
  fields. app-unbuilt 118 tables — 48 referenced, 16 build-only dropped, 14 build-only retained,
  40 unreferenced, 1440 unreferenced fields, 3 undetermined.
- GOTCHA: `dropped` is tested BEFORE `app_used` and beats it. A dropped table is not in the final
  data model, so an app match can only be its field NAMES coinciding with the successor table's —
  exactly what a `_Temp` looks like. Testing app_used first put 7 of app2's 9 dropped tables in
  `referenced`. For a table that is NOT dropped the same name-matching is correct rather than
  coincidental: Qlik's model is associative, so two live tables sharing a field name share the
  field.
- Field-level findings are withheld where `complete_fields` is FALSE — an unresolved `*` means an
  absent name proves nothing.
- **Validation worth repeating after any change to matching**: for every field of every
  `unreferenced` table, plain-substring search the raw JSON bytes, bypassing both the tokenizer
  and `json_strings.R`. app2 returns 0 hits. app-unbuilt returns 8, all confirmed substring
  collisions (`ILOC Code` inside the real field `ILOC Code 2021`; `Make` inside `MakeDate(`).
  This check is what caught the `whole-string` false negative.
- Private: `.ur_fold()`, `.ur_show()`.

## script_refs.R — phase 2 step 4a, NOT a pass and NOT in the pipeline

- Nothing in `run_pipeline.R` sources this; it is phase 2 tooling (DESIGN §6.5). Base R only.
- `script_table_refs(tokens, table_names, own) -> data.frame(table, line, lead, kind)` with
  `kind` = `use` / `drop` / `store` / `self`. `lead` is the enclosing statement's first word,
  which is what separates a use (`RESIDENT Temp`) from a disposal (`DROP TABLE Temp`).
- `script_disposals(tokens, table_names) -> list(drops, stores)`. STORE is scanned separately
  because its TARGET matters: qvd generation inside a user-facing app violates the Cloud build
  standards whether or not the table is otherwise used (DESIGN §7.1).
- Exists because an app-only cross-reference marks every intermediate table unreferenced. A
  `_Temp` read by a later RESIDENT load, or a mapping table consumed by ApplyMap, is used.
- Deliberately over-inclusive: any token whose undelimited text equals a table name counts,
  wherever it sits. A false `use` keeps a table that could have gone; a false `unused` deletes a
  live one, and only the first is recoverable.
- GOTCHA: match TYPED TOKENS, never raw text. A raw-text scan reports 18 STOREs in app-unbuilt
  and 20 in app2; the real count in both is **ZERO** — every hit is the substring "Store" in the
  connection path `lib://Curated Data Store:DataFiles/...`, which is one BRACKET token. Same trap
  the tokenizer records for `for` (1242 raw, ~28 real). Raw `applymap` counts mislead the same
  way: 11 in app-unbuilt, 0 real.
- Current: app2 26 tables, 13 read elsewhere, 9 dropped, 0 stored. app-unbuilt 118 tables, 40
  read elsewhere, 16 dropped, 0 stored.
- Private: `.sr_undelimit()`, `.sr_statements()`.

## oracle_json_strings.R — development-machine cross-check, not shipped tooling

- `Rscript oracle_json_strings.R [dir-or-file ...]` — compares `read_json_strings()` against
  `jsonlite::fromJSON()` on every `.json` under `app*-unbuilt/` by default. Exits 1 on any
  mismatch. Current status: 32 files checked, 0 failed.
- Nothing sources it — not `run_pipeline.R`, not `verify.R`, not phase 2 tooling — so its
  `jsonlite` dependency cannot reach an environment that lacks it. Keep it that way; phase 2's
  own scripts must stay base-R with no reference to this file.
- Compares strings in DOCUMENT ORDER, not as sets, and checks the key/value flag on each. Order
  is the stricter test and the one that catches a dropped or duplicated match.
- `fromJSON(simplifyVector = FALSE)` is required: with simplification on, a string array collapses
  to a character vector and an object to a data.frame, both of which lose position.
- Mismatch output truncates the offending string to 60 characters — a difference inside a
  2,600-line chart definition should print a hint, not the chart.
- Private: `.oracle_strings()`, `.oracle_read()`, `.brief()`, `check_file()`, `main()`.

## run_pipeline.R — the current full pipeline, not a snapshot

- Script, not a function. `setwd(PROJECT_DIR)` (= `C:/Rtools`, existence-checked), sources
  everything, runs all seven passes in order, prints warnings, writes output. Add new passes here.
- Takes optional positional args: `Rscript run_pipeline.R [options] <input_path> <output_path>`.
  Defaults (no args) are `"[Grant Managing Region].txt"` -> `script_out.txt`, unchanged from before
  this became parameterized (2026-08-17, for the staged testing methodology in CLAUDE.md — one
  script now serves all three test-stage files instead of ad hoc copies). Falling back to those
  defaults now prints which files it chose (Adam 2026-08-17) — silence made a novice think they
  had reformatted their own file.
- Flags: `--help`/`-h` prints usage and exits 0; `--changes` writes one CSV per pass
  (`<output_dir>/changes/<n>-<label>.csv`) so the `$changes` tables survive an Rscript run. An
  unrecognised flag, a third positional (an unquoted path with spaces), a missing input, a missing
  output folder, or input == output each stop with one sentence before any work is done.
- Runs stay QUIET per pass (Adam 2026-08-17 — no progress lines); output is warnings, the written
  path, and the `--changes` table if asked.
- Script-local only, deliberately undocumented by name: `PASS_LABELS` (the seven pass names, so the
  error message and the `--changes` file names share one spelling) and a thin wrapper that names
  the failing pass instead of letting a bare stack trace through. `verify_docs.R` excludes
  `run_pipeline.R` from the INTERFACES cross-check in BOTH directions, so naming a helper here in
  call form would be reported as stale — this file documents no function surface for the script.
- **GOTCHA: the seven `source("ensure_|enforce_...")` lines are machine-read.**
  `verify_docs.R`'s `check_pass_lists` matches them anchored at column 0 in the literal
  `source("name.R")` form. Indenting them, or folding them into a vector plus a loop, silently
  breaks the pass-list agreement check. A comment in the file says so at the point of temptation.

## verify.R — standing verification suite; gates on exit status

- Script; run it, don't source it. `main()` runs self-tests then both fixtures.
- STAGE 3 ONLY (2026-08-17): refuses to start without `--stage3` on the command
  line — the flag makes the invocation acknowledge the testing-stage rule at
  the point of action. Interactive sourcing is unaffected.
- `canonical_stream(tokens) -> list(canon, line)` — reduces a stream to meaning-carrying
  entries, normalising away exactly what the passes are ALLOWED to change.
- `check_equivalent(before, after, max_report=3) -> TRUE | character` — the important check.
  Detects a single insertion/deletion and says so instead of reporting the whole cascade.
- `verify_file(path)`, `verify_detects_corruption()`, `ok(label, passed, detail)`, `section(...)`.
- Each normalisation in `canonical_stream` is an assumption about Qlik recorded in DESIGN §1 —
  adding a pass that makes a NEW kind of legitimate change means adding one there too.
- Private: `.unquote`, `.short`, `.window`, `.tail` (display/comparison helpers).

## verify_docs.R — documentation consistency; gates on exit status, no fixtures

- Script; run it, don't source it. Safe at ANY testing stage — reads only
  *.R and the five root .md files, never a fixture. Pre-commit invariant
  (CLAUDE.md): must exit 0 before any commit.
- Checks: every DESIGN §-citation resolves to a real heading; pass list
  agrees across run_pipeline.R / verify.R PASSES / README table; public
  functions all appear in INTERFACES.md and every project-named function
  INTERFACES mentions exists; STATE.md <= 25 lines; open-question markers
  appear nowhere outside STATE.md. The marker wordings live in
  `check_no_tbd`'s pattern and are deliberately NOT restated here — naming
  them in prose trips the check that forbids them. Widened 2026-08-17 after
  the first version matched one literal phrase and missed a stale verify.R
  label that used different words.
