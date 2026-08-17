# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17): styling functionally COMPLETE; framework hardened.**
Seven cosmetic passes, all green — aliases -> brackets -> leading commas ->
intra-line spacing -> casing -> vertical layout -> alias alignment. Rules
live in DESIGN §4.1-§4.9. Three-stage testing fully exercised, signed off.
verify.R: 97 checks, 0 failed (now requires --stage3 to run at all).

**Framework (this commit):** doc rot, stage-gate violations and rumination
were structural — prose enforcement loses to trained defaults, and dupli-
cated facts drift. Enforcement moved into mechanisms: `verify_docs.R`
(7 checks: citations resolve, pass lists agree, INTERFACES ↔ code, STATE
cap, open-question markers confined here) gates every commit; verify.R
refuses fixtures without --stage3; CLAUDE.md rewritten to 40 imperative
lines, histories deleted; HANDOVER-PROTOCOL.md removed from tree (git has
it); DESIGN §4.11 is now settled scope exclusions only.

**Next task:** none queued. Next real work is phase 2 (pruning), DESIGN §5.

**Open items:**
- Bracketing bare words outside LOAD field lists (WHERE clauses etc.) —
  out of scope for styling (§4.11); reopening needs a variable-vs-field
  discriminator first. Phase-2+ question.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
