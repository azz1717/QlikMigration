# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** SIX cosmetic passes, all green: aliases ->
brackets -> leading commas -> intra-line spacing -> casing -> vertical
layout. Pass 2 also brackets bare (unquoted) field references now (was a
real gap). verify.R: 80 checks, 0 failed.

**Next task:** open — the five phase-1 style rules originally scoped
(DESIGN §4) are now all implemented. Candidates: §4.6 alias alignment (must
run after layout, per README), §4.9 FROM clause, §4.10 comment removal, or
start phase 2/3 (DESIGN §5). Ask Adam which.

**Open items:**
- Layout's comment-attachment convention (a leading comment glues to the
  FOLLOWING statement, not the one before it — DESIGN §4.8) was a judgment
  call made under time pressure, not separately confirmed with Adam. Review
  script_qvs_out.txt / the layout diff before treating it as settled.
- The line immediately after a `///$tab` section marker is left unindented
  (section-adjacent gaps are never touched) — known, deliberate, but worth
  Adam's eyes on real output.
- formatexample.txt is not a verify.R fixture; the alignment pad is covered
  by argument and a synthetic test, never against that file directly.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
