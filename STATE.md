# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** SIX cosmetic passes, all green: aliases ->
brackets -> leading commas -> intra-line spacing -> casing -> vertical
layout. Layout had 4 real bugs, found by Adam against formatexample.txt and
fixed same day: comma-led fields misclassified `statement` not `field`; no
first-field alignment pad; table label unindented after a `///$tab` marker;
SET/LET given normal statement indent/spacing instead of being exempted.
verify.R: 83 checks, 0 failed. formatexample.txt is now Adam-confirmed
correct for the single-LOAD-block case (by eye, not yet a verify.R fixture).

**Next task:** open — all five phase-1 style rules (DESIGN §4) now
implemented. Candidates: §4.6 alias alignment (after layout, per README),
§4.9 FROM clause, §4.10 comment removal, or phase 2/3 (DESIGN §5). Ask Adam.

**Open items:**
- Layout's comment-attachment convention (a leading comment glues to the
  FOLLOWING statement, not the one before it — DESIGN §4.8) was a judgment
  call made under time pressure, not separately confirmed with Adam.
- Pre-existing bug spotted in passing, NOT fixed here: pass 3 can attach a
  trailing comma with no following field onto a `FROM` line instead of
  warning. Own session. RESIDENT loads, prefix lines, control-flow indent
  depth still open (DESIGN §4.11).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
