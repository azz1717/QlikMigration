# HANDOVER-PROTOCOL.md — one-time bootstrap record, ALREADY EXECUTED

**This is not a recurring session-end step.** It ran once, 2026-08-17
(commit 909b1e9), to build INTERFACES.md/STATE.md/CLAUDE.md from a repo that
had none. Ongoing currency is CLAUDE.md's "Maintenance duty" — updates land
in the same commit as whatever changed. There is no separate handover step at
session end (Adam's explicit choice, 2026-08-17: per-commit only, not
end-of-session — a session that ends without committing leaves no updated
record, and that's accepted as the tradeoff).

Kept as a record of *why* the three files exist, and as a rebuild procedure
**only if they are ever lost or badly corrupted**. Do not re-run this
routinely, and do not treat it as orientation for ordinary work — for that,
read STATE.md and INTERFACES.md, per CLAUDE.md.

---

Below is the original bootstrap procedure, unchanged except where noted.
Written after a cold start consumed ~100k tokens of context (~20% of Adam's
usage limit) on orientation alone — reading DESIGN.md, verify.R and every
pass file end-to-end before the first edit.

**If you are re-running this to rebuild from scratch: this file is your
entire orientation.** Do not read README.md, DESIGN.md, memory files, or any
fixture first. Everything you need is either written here verbatim or
gathered by the steps below.

## What a handover is

A handover is the repo carrying its own orientation, so no session pays to
re-derive it. Three artifacts, each with a maintenance duty attached:

| artifact | answers | kept current by |
|---|---|---|
| INTERFACES.md | what does each script expose, how do I call it? | any commit changing a public function |
| STATE.md | where does work stand, what's next, what's open? | any commit starting/finishing a task |
| CLAUDE.md | how must a session behave here? (auto-loads every session) | rarely changes |

A handover is written **continuously** — each commit carries its doc updates,
exactly as this repo already does for DESIGN.md. It is never reconstructed at
session end by re-reading everything: by then the budget is spent.

Target after this protocol runs: a cold session orients on STATE.md +
INTERFACES.md + `git log --oneline -15` (a few thousand tokens), then loads
only the DESIGN.md sections STATE.md names. Fixtures never enter context.

## Rules for you, the executor

- Read each of the 9 `.R` files ONCE, top to bottom. That is this task's main
  cost, and it is the point: spend it once so no session spends it again.
- File header comments in this repo are dense, current and authoritative.
  Extract from them. Do not re-derive behaviour. Do not run R at all.
- NEVER read: `[Grant Managing Region].txt`, anything in `app-unbuilt/`,
  `formatexample.txt`, `script_out.txt`, `dlmf-log.md`.
- Do not modify any `.R` file, DESIGN.md, or README.md. Docs layer only.
- A judgement call these instructions don't settle → ask Adam as a
  multiple-choice question immediately. Do not deliberate silently.

## Step 1 — write INTERFACES.md

One entry per `.R` file, all 9: `qlik_tokenizer.R`, `qlik_reserved_words.R`,
`ensure_explicit_aliases.R`, `enforce_bracket_references.R`,
`enforce_leading_commas.R`, `enforce_intraline_spacing.R`,
`enforce_reserved_word_case.R`, `run_pipeline.R`, `verify.R`.

Entry format — every public (non-dot-prefixed) top-level function gets a
bullet: signature, return shape, one line of what it does, `GOTCHA:` where
one exists. Pass entries must name their `$changes` columns (they differ per
pass). Dot-prefixed private helpers get ONE combined line per file
(`Private: .name (half-line purpose), ...`) — this is what lets a future
session spot promotable logic without opening the file; it has already paid
off once (`in_select_region` was found in the casing pass and promoted).

This entry is written for you and is the quality bar — include it verbatim:

```markdown
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
  GOTCHA: its index arithmetic uses `1L` literals deliberately; an unsuffixed `1` silently makes
  start/end doubles and breaks callers doing `vapply(..., integer(1))`. Fixed 2026-08-17.
Token types: COMMENT DQUOTE SQUOTE BRACKET WS WORD NUMBER OPERATOR COMMA SEMI LPAREN RPAREN VOID OTHER.
```

Open INTERFACES.md with a Conventions block (~12 lines): every pass is
`f(tokens) -> list(tokens, warnings, changes)`; trivia = WS/COMMENT/VOID;
quoted/bracketed/comment tokens are opaque — never edit inside them; collect
edits during a pass, apply in ONE void_token/splice_tokens call; every pass
skips `SELECT ... ;`; new passes copy README's "Adding a pass" template; and
**before writing any helper, check the shared scanners in the tokenizer entry
first — promote a private helper rather than duplicating it.**

Caps: ≤ 130 lines total; tokenizer entry ≤ 18; each pass ≤ 8; scripts ≤ 8.

## Step 2 — write STATE.md, exactly this content

```markdown
# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** cosmetic pipeline has five passes, all green:
aliases -> brackets -> leading commas -> intra-line spacing -> casing.
Spacing pass landed at ebb6af6 (docs at e3b1bef). verify.R: 50 checks, 0 failed.

**Next task:** vertical layout — read ONLY DESIGN §3.4–§3.6, §4.5, §4.8, §6.2.
Needs a block-structure scanner (FOR/NEXT, SUB/END SUB, IF/THEN/ENDIF,
DO/LOOP, SWITCH/CASE, ///$tab sections) and a decision, before building,
on how to keep a huge-diff pass auditable (§6.2).

**Open items:**
- Migration debt report (DESIGN §7): any new pass walking the token stream
  should consider piggybacking detection records — raise with Adam.
- formatexample.txt is not a verify.R fixture; the alignment pad is covered
  by argument and a synthetic test, never against that file directly.
- dlmf-log.md sits untracked at repo root; Adam decides if/when to commit.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
```

If STATE.md still exists, don't use the block above — just fix STATE.md
directly, it's a normal file. The block is only a starting point for the
scenario this whole file exists for: STATE.md is gone and there's nothing
live to defer to. Rebuild its content from `git log` and the repo's current
state at rebuild time, not from the stale snapshot above.

## Step 3 — write CLAUDE.md, exactly this content

```markdown
# C:\Rtools — session rules

## Cold start (this, and only this)
1. Read STATE.md, INTERFACES.md, `git log --oneline -15`.
2. Read ONLY the DESIGN.md sections STATE.md names for the current task.
3. Give Adam a short plan with the open decision points BEFORE implementing.

## Reading budget
- NEVER read fixtures into context: [Grant Managing Region].txt, app-unbuilt/,
  formatexample.txt, script_out.txt. Grep/sed specific lines when needed.
- Don't read a pass's source unless editing that pass — INTERFACES.md is the
  contract. Don't read DESIGN.md or README.md end-to-end; go by section.

## Working with Adam
- Style/scope calls are Adam's. Ask as multiple-choice the moment one
  appears; never deliberate silently past two unknowns.
- Short progress note at each milestone (design settled / code written /
  verify green) — not one wall of output at the end.

## Invariants (do not re-derive)
- Verify before and after any change; it gates on exit status:
  & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" verify.R   (not on PATH)
- Base R only. Token stream, not text. Every pass skips SELECT...;.
- Check INTERFACES.md shared scanners before writing any helper.
- Multi-line commit messages: write to a scratch file, `git commit -F <file>`.

## Maintenance duty (same commit as the change)
- Public function signature/behaviour changed -> update INTERFACES.md entry.
- Task started or finished -> update STATE.md (keep it ≤ 25 lines).
- DESIGN.md stays current — long-standing repo convention; see git history.
```

## Step 4 — verify

1. `grep -n "^[A-Za-z_.][A-Za-z0-9_.]* *<- *function" *.R` — every match
   without a leading dot has a bullet in INTERFACES.md; every dot-prefixed
   match appears on its file's Private line. `grep "^QLIK_"
   qlik_reserved_words.R` for the two data objects.
2. `wc -l INTERFACES.md STATE.md CLAUDE.md` — ≤ 130 / ≤ 25 / ≤ 50. Over
   budget → cut adjectives, not functions.
3. Confirm you modified nothing else: `git status --short` shows only the
   three new files plus this protocol (and dlmf-log.md, untracked — leave it).

## Step 5 — commit

One commit: INTERFACES.md, STATE.md, CLAUDE.md, HANDOVER-PROTOCOL.md.
Adam pre-authorised this commit by adopting this protocol. Message:

    Add the handover layer: interface map, state file, session rules

    Cold sessions were orienting by reading the repo (~100k tokens before
    the first edit). INTERFACES.md carries each script's contract so no
    session re-reads source to discover it; STATE.md carries position and
    next task; CLAUDE.md auto-loads the reading budget and collaboration
    rules that keep it that way. HANDOVER-PROTOCOL.md records the duties
    that keep all three current. Written per Adam's 2026-08-17 direction.

Do not commit dlmf-log.md.
