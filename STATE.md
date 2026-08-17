# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17): styling functionally COMPLETE; framework hardened.**
Seven cosmetic passes, all green — aliases -> brackets -> leading commas ->
intra-line spacing -> casing -> vertical layout -> alias alignment. Rules
live in DESIGN §4.1-§4.9. Three-stage testing fully exercised, signed off.
verify.R: 97 checks, 0 failed (requires --stage3). verify_docs.R: 7 checks,
gates every commit.

**Branch: phase2-pruning** (off main). verify.R is styling-only — it does NOT
apply to phase 2 work; verify_docs.R still gates every commit (Adam).

**Next task:** phase 2 usage extraction, DESIGN §6.5. Step 1 DONE —
json_strings.R, oracle-tested vs jsonlite. Steps 2-4 next.

**Open items:**
- What can the work machine actually run? §6.5's extractor is base-R-only
  precisely to dodge this, but the answer decides whether jsonlite can be
  the test oracle there as it is here. Adam to check; access, not capability.
- Bare word = field or variable? Unresolved, and now blocking two things:
  §4.11 bracketing outside LOAD field lists, and precise field-level usage
  in phase 2. Phase 2 works around it (ambiguous counts as used, whole
  loads only); §4.11 still needs the real discriminator.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
