# STATE — read this first; update before any commit that starts/finishes a task

**Position: §4.10 reopened + done** (Adam + group, 2026-08-20): commented-out
fields styled via NEW PASS 8, comment text only. 3 stages run, --stage3 119/0.
UNCOMMITTED. §4.1-§4.9 closed.

**Next task:** phase 2, DESIGN §6.5 + §7.1/§7.2. Tooling COMPLETE — next is USE.

**Open items:**
- §4.10 scope: commented-out TABLES and multi-line commented expressions
  stay unstyled. Adam's call whether to extend.
- Re-run over own output churns 11 lines (CRLF); PRE-EXISTING at 4c73dd7,
  violates §7.5, touches closed §4.7/§4.8.
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
