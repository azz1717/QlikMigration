# STATE — read this first; update before any commit that starts/finishes a task

**Position: styling COMPLETE and closed** — 7 passes, DESIGN §4.1-§4.9,
verify.R 97 checks 0 failed (--stage3). Branch phase2-pruning off main;
verify.R is styling-only, verify_docs.R gates every commit.

**Next task:** phase 2, DESIGN §6.5 + §7.1/§7.2. Steps 1-4a DONE
(json_strings +oracle, app_usage, script_loads, script_refs, usage_report,
script_debt, render_report). Phase 2 tooling COMPLETE; next is use.

**Open items:**
- 4b DONE: render_report.R emits the signed-off report_mock.html from live
  data. WHAT and HOW MUCH only; severity/effort are Adam's, never computed.
- What can the work machine run? Decides whether jsonlite can be the oracle
  there as here. Adam to check; access, not capability.
- Qlik behaviour for a repeated explicit table label? Unverified; 9 in app1.
- 3 wildcard qvd loads make table/field totals a floor; 23 used field refs match
  nothing known and look like theirs, so they are load-bearing, not dead.
- Parked, Adam's: before/after pruning report; explicit+unique table names.
- A qvd inventory (name + path + FIELDS) resolves the 3 wildcard-path loads.
  Adam has names+paths; fields unknown, hundreds of qvds.
- Bare word = field or variable? Blocks §4.11 bracketing outside LOAD field
  lists and field-level usage. Phase 2 works around it (ambiguous = used).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
