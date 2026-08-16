# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** cosmetic pipeline has five passes, all green:
aliases -> brackets -> leading commas -> intra-line spacing -> casing.
Spacing pass landed at ebb6af6 (DESIGN at e3b1bef, README at a2f1a57).
verify.R: 50 checks, 0 failed. Cost 0.42s / 1.97s on the two fixtures.

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
