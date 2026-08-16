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
- `find_load_segments(tokens) -> list(segments, warnings)` — per-field segments of every LOAD list;
  segment: start, end, content_idx, has_as, alias_content_idx, line. Skips SELECT itself.
  GOTCHA: seg$end is a double, not integer — vapply with double(1), or fix at source first.
- Private: `.qlik_token_type` (token text -> type name).

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

- `enforce_bracket_references(tokens)` — quoted references become `[bracketed]`.
  `$changes`: line, kind ("double-quoted field" / "single-quoted alias"), before, after.
- DQUOTE is ALWAYS a field reference in Qlik (no double-quoted string literal exists) -> always
  converted. SQUOTE is a literal EXCEPT immediately right of a field's `AS`, where it is an
  alias reference (confirmed by testing, undocumented — DESIGN §1.2).
- GOTCHA: content containing `]` cannot be bracketed (no escape in Qlik) — left quoted + warned.
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
- Private: `.next_content_type` (type of the next non-trivia token, for the call-position test).

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
