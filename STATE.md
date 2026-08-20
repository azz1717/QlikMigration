# STATE — read this first; update before any commit that starts/finishes a task

**Position: §4.1-§4.10 CLOSED.** `undelimit()` is the repo's one delimiter strip
— 4 twins folded in; verify.R's `.unquote` stays separate ON PURPOSE. Stage 3
GREEN: 119/0. Pass-ordering constraints consolidated + evidenced, DESIGN §2.5.

**Next task:** phase 2, DESIGN §6.5 + §7.1/§7.2. Tooling COMPLETE — next is USE.

**Open items:**
- §4.10 scope: commented-out TABLES and multi-line commented expressions
  stay unstyled. Adam's call whether to extend.
- run_pipeline.R hard-codes PROJECT_DIR="C:/Rtools", so a worktree run
  silently tests main's passes. Adam's call whether to derive it from $0.
- Parked, Adam's: before/after pruning report; explicit+unique table names.
- What can the work machine run? Decides whether jsonlite can be the oracle
  there as here. Adam to check; access, not capability.
- Qlik behaviour for a repeated explicit table label? Unverified; 9 in app1.
- 3 wildcard qvd loads make table/field totals a floor; 23 used field refs
  match nothing known, look like theirs — load-bearing, not dead.
- A qvd inventory (name+path+FIELDS) resolves those 3 wildcard-path loads.
  Adam has names+paths; fields unknown, hundreds of qvds.
- Bare word = field or variable? Blocks §4.11 bracketing outside LOAD field
  lists and field-level usage. Phase 2 works around it (ambiguous = used).

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
