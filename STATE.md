# STATE — read this first; update before any commit that starts/finishes a task

**Position: styling COMPLETE and closed** — 7 passes, DESIGN §4.1-§4.9,
verify.R 97 checks 0 failed (--stage3). Branch phase2-pruning off main;
verify.R is styling-only, verify_docs.R gates every commit.

**Next task:** phase 2 usage extraction, DESIGN §6.5. Steps 1-3 DONE:
json_strings.R (+oracle, 32 files 0 failed), app_usage.R (both apps),
script_loads.R, script_refs.R, usage_report.R. 4a RUNS: app2 26 tables
(15 referenced / 9 dropped / 2 unreferenced); app1 118 (48/16/14/40).
Step 4b next: renderer -> submittable document.

**Open items:**
- What can the work machine actually run? Decides whether jsonlite can be
  the oracle there as here. Adam to check; access, not capability.
- Inline-load row count above which it is debt, not a mapping table? app1's
  largest is 1113 rows, app2's 33. Adam, when the report is built.
- A qvd inventory (name + path + FIELDS) would resolve the 3 wildcard-path
  loads, which expand to one table per qvd. Adam has extracted names+paths
  before; field names unknown, hundreds of qvds, by hand impractical.
- Bare word = field or variable? Blocks §4.11 bracketing outside LOAD field
  lists, and precise field-level usage in phase 2. Phase 2 works around it
  (ambiguous counts as used, whole loads only); §4.11 needs the real answer.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
