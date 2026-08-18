# STATE — read this first; update before any commit that starts/finishes a task

**Position: styling COMPLETE and closed** — 7 passes, DESIGN §4.1-§4.9,
verify.R 97 checks 0 failed (--stage3). Branch phase2-pruning off main;
verify.R is styling-only, verify_docs.R gates every commit.

**Next task:** phase 2, DESIGN §6.5 + §7.1/§7.2. Steps 1-4a DONE
(json_strings +oracle, app_usage, script_loads, script_refs, usage_report,
script_debt). 4b UNRESOLVED — first open item.

**Open items:**
- 4b report design: TWO drafts rejected (data dump; then tab-grouped,
  "arguably worse"). Start fresh WITH Adam; do not iterate render_report.R.
  Layers 1-4a are sound and stay. DESIGN §7.2 records what was tried.
- What can the work machine run? Decides whether jsonlite can be the oracle
  there as here. Adam to check; access, not capability.
- Qlik behaviour for a repeated explicit table label? Unverified; 9 in app1.
- Parked, Adam's: before/after pruning report; explicit+unique table names
  as a forward design standard.
- A qvd inventory (name + path + FIELDS) would resolve the 3 wildcard-path
  loads. Adam has names+paths before; fields unknown, hundreds of qvds.
- Bare word = field or variable? Blocks §4.11 bracketing outside LOAD field
  lists and field-level usage. Phase 2 works around it (ambiguous = used).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
