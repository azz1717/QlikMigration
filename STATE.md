# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17):** SEVEN cosmetic passes, all green: aliases ->
brackets -> leading commas -> intra-line spacing -> casing -> vertical
layout -> alias alignment. Layout had 4 bugs found+fixed same day. Alignment
(new, §4.6): tabs before AS's existing space, per-block column, 122-char
outlier cap excluded. FROM clause (§4.9) also done: one line, one space.
verify.R: 97 checks, 0 failed.

**Next task — TOP PRIORITY:** `enforce_intraline_spacing` (pass 4) never
forces a space around `AS` next to a non-WORD token (`"field"as[alias]` ->
`[field]AS[alias]`, zero spaces, real unstyled-input case). Found building
alignment, which was hardened not to crash on it rather than fix it — the
actual fix belongs in pass 4. See INTERFACES.md's entry for that pass.

**Open items:**
- Layout's comment-attachment convention (DESIGN §4.8) still not separately
  confirmed with Adam.
- Pre-existing, NOT fixed: pass 3 can attach a trailing comma with no
  following field onto a `FROM` line instead of warning. RESIDENT loads,
  prefix lines, control-flow indent depth still open (§4.11).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
