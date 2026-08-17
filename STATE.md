# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** SEVEN cosmetic passes, all green: aliases ->
brackets -> leading commas -> intra-line spacing -> casing -> vertical
layout -> alias alignment. Layout had 4 bugs found+fixed same day. Alignment
(§4.6): tabs before AS's existing space, per-block column, 122-char outlier
cap excluded; a wrapped field is now measured from AS's own line and CAN
widen the column (no longer blanket-excluded). FROM clause (§4.9): path +
format spec + terminating `;` all join onto one line, including across a
comment (the `;` physically relocates in that case). Pass 4's AS-spacing gap
is fixed. Three-stage testing methodology (CLAUDE.md) fully exercised for
the first time this session, all three stages signed off by Adam. verify.R:
97 checks, 0 failed.

**Next task:** none queued.

**Open items:**
- Layout's comment-attachment convention (DESIGN §4.8) still not separately
  confirmed with Adam.
- Pre-existing, NOT fixed: pass 3 can attach a trailing comma with no
  following field onto a `FROM` line instead of warning. RESIDENT loads,
  prefix lines, control-flow indent depth still open (§4.11).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
