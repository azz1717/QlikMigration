# STATE — read this first; update before any commit that starts/finishes a task

**Position: pruning merged to `main`; on `phase2-remapping`.** §4.1-§4.10 CLOSED,
stage 3 GREEN 119/0. Retargeting design written up: DESIGN §6.6 — stage order,
`views.csv` as oracle, two lookup tables, resolver/rewriter split. No code yet.

**Next task:** build the resolver (DESIGN §6.6). Read §6.6 + §6.5 + §7.1.

**Open items:**
- §4.10 scope: commented-out TABLES and multi-line commented expressions
  stay unstyled. Adam's call whether to extend.
- qlik-cli reachability CLOSED (DESIGN §8, verified on VM 2026-08-21). Open:
  can qlik-cli fetch/put an app's load script? Untested, and phase 3 needs it.
- Parked, Adam's: before/after pruning report; explicit+unique table names;
  comment-styling sub-stream, PLAN-comment-substream.md (§4.10 scope, above).
- What can the work machine run? Decides whether jsonlite can be the oracle
  there as here. Adam to check; access, not capability.
- Qlik behaviour for a repeated explicit table label? Unverified; 9 in app1.
- 3 wildcard qvd loads make totals a floor, and are `unmatched` by construction
  (§6.6); a qvd inventory with FIELDS resolves them. Adam has names+paths.
- 23 used field refs match nothing known, look like theirs — load-bearing.
- Bare word = field or variable? Blocks §4.11 bracketing outside LOAD field
  lists and field-level usage. Phase 2 works around it (ambiguous = used).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
