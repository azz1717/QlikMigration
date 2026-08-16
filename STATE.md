# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** five cosmetic passes, all green: aliases ->
brackets -> leading commas -> intra-line spacing -> casing. Vertical layout
(pass 6) is UNDERWAY — its block-structure scanner `find_block_structure()`
is built and self-tested; the pass itself is not written yet.
verify.R: 60 checks, 0 failed.

**Next task:** write the vertical layout pass on top of that scanner.
Read ONLY DESIGN §3.4–§3.6, §4.5, §4.8, §6.2. Every style decision is settled
and recorded there — do not re-open them:
- indent is FLAT: statement 1 tab, field 2, continuation 3, comment column 0
- a whole FOR/IF/SUB/DO/SWITCH block is ONE statement: no blank lines inside,
  two between top-level statements
- `///$tab` section markers: leave their whitespace entirely alone
- the large diff is accepted; verify.R's equivalence check is the audit

**Open items:**
- Migration debt report (DESIGN §7): `app-unbuilt/script.qvs` starts with the
  bytes `I///$tab 00-Main` — a stray `I` ahead of the section marker. The
  scanner warns on it; it is a debt-report candidate. Raise with Adam.
- formatexample.txt is not a verify.R fixture; the alignment pad is covered
  by argument and a synthetic test, never against that file directly.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
