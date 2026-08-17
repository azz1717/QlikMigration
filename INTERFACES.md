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
  segment: start, end, content_idx, has_as, alias_content_idx, line — all integer. Skips SELECT.
  GOTCHA: its index arithmetic uses `1L` literals deliberately; an unsuffixed `1` silently makes
  start/end doubles and breaks callers doing `vapply(..., integer(1))`. Fixed 2026-08-17.
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
  DESIGN §4.11 (Adam 2026-08-17): a SET/LET statement, and any of its own continuation lines, is
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

- `enforce_vertical_layout(tokens)` — indentation and blank lines. DESIGN §3.4/§4.5/§4.8/§4.11/§6.2.
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
  outgoing gap — DESIGN §4.11, Adam 2026-08-17: 0 indent, but never force/collapse the spacing
  around one.
- GOTCHA: only rewrites the leading-whitespace GAP for each line — it never inserts a new line
  break where the source didn't already have one, and never touches anything after a line's first
  token.
- GOTCHA: line 1 usually has no preceding WS token at all (true of both fixtures). That insertion
  is deferred until AFTER the main loop finishes — doing it mid-loop shifts every later original
  token index by one and silently corrupts the rest of the file (caught before commit, 2026-08-17).
- Needs no new `canonical_stream` rule in verify.R: only WS token TEXT is ever rewritten, and
  WS/COMMENT/VOID are already stripped before the equivalence check compares anything.

## run_pipeline.R — the current full pipeline, not a snapshot

- Script, not a function. `setwd("C:/Rtools")`, sources everything, runs all five passes in
  order, prints warnings, writes `script_out.txt`. Add new passes here.

## verify.R — standing verification suite; gates on exit status

- Script; run it, don't source it. `main()` runs self-tests then both fixtures.
- `canonical_stream(tokens) -> list(canon, line)` — reduces a stream to meaning-carrying
  entries, normalising away exactly what the passes are ALLOWED to change.
- `check_equivalent(before, after, max_report=3) -> TRUE | character` — the important check.
  Detects a single insertion/deletion and says so instead of reporting the whole cascade.
- `verify_file(path)`, `verify_detects_corruption()`, `ok(label, passed, detail)`, `section(...)`.
- Each normalisation in `canonical_stream` is an assumption about Qlik recorded in DESIGN §1 —
  adding a pass that makes a NEW kind of legitimate change means adding one there too.
- Private: `.unquote`, `.short`, `.window`, `.tail` (display/comparison helpers).
