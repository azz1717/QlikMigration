# STATE — read this first; update before any commit that starts/finishes a task

**Position (2026-08-17): styling is functionally COMPLETE.** Seven cosmetic
passes, all green: aliases -> brackets -> leading commas -> intra-line
spacing -> casing -> vertical layout -> alias alignment. Rules live in
DESIGN §4.1-§4.9; don't re-derive them here. Three-stage testing fully
exercised, signed off by Adam. verify.R: 97 checks, 0 failed.

**§4.11 was documentation rot, not missing work (audited 2026-08-17).** All
six "unspecified" items were resolved: three already had written rules
(§4.5 prefix lines, §4.5 flat control-flow indent, §4.8 ///$tab), three had
working behaviour Adam ratified this session (RESIDENT = FROM per §4.5;
preceding LOAD's `;` uses §4.9's general lone-`;` rule; SELECT spaced as any
statement per §4.8). Comment attachment (§4.8) confirmed working. The stale
list caused a settled decision to be re-opened; CLAUDE.md now carries a duty
to prune §4.11 the moment a call is made.

**Next task:** none queued. Next real work is phase 2 (pruning), DESIGN §5.

**Open items:**
- Bracketing bare words outside LOAD field lists (WHERE clauses etc.) —
  §4.11, the one genuine gap. Out of scope deliberately: a bare word there
  is often a variable, and bracketing one changes what the script loads.

(Cap: 25 lines. Longer means it is rotting — prune to position/next/open.)
