# C:\Rtools — session rules
# Principle: a failure buys a mechanism (check/guard/flag), never another
# paragraph — here or anywhere. Histories live in git.
## Cold start (this, and only this)
1. Read STATE.md, INTERFACES.md, `git log --oneline -15`. Nothing else.
2. Read ONLY the DESIGN.md sections STATE.md names for the current task.
3. Short plan + open decision points to Adam BEFORE implementing.

## Reading budget
- NEVER read fixtures into context: [Grant Managing Region].txt, app-unbuilt/,
  script_out.txt, script_qvs_out.txt. Grep specific lines when needed.
  Exception: formatexample.txt and its pipeline output — read freely.
- DESIGN.md / README.md by named section only, never end-to-end.
- Don't read a pass's source unless editing it — INTERFACES.md is the contract.

## Working with Adam
- Style/scope calls are Adam's. Trigger: 2 unresolved unknowns, OR 2
  exploratory tool calls without convergence -> STOP, ask as multiple choice.
- Short progress note per milestone. No essays unless asked.
- An explicit sequencing instruction from Adam overrides any general rule.

## Invariants
- Base R only. Token stream, not text. Every pass skips SELECT...;.
- Check INTERFACES.md shared scanners before writing any helper.
- Rscript is not on PATH: & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
- Before ANY commit: verify_docs.R must exit 0 (doc consistency; no fixtures).
- verify.R refuses to run without --stage3; the flag = Adam's stage-3 go-ahead.
- Multi-line commit messages: scratch file, then `git commit -F <file>`.

## Testing — three stages, in order, each gated on Adam's sign-off
1. formatexample.txt: run pipeline, eyeball output myself, hand to Adam.
2. [Grant Managing Region].txt: run pipeline only, NEVER read the output;
   Adam reviews, naming regions to grep if off.
3. script.qvs: run pipeline + verify.R --stage3 (exit 0 required), then Adam.

## Maintenance (same commit as the change — there is no session-end step)
- Signature changed -> INTERFACES.md. Task start/finish -> STATE.md (<=25).
  Pass added/reordered -> run_pipeline.R, verify.R PASSES, README, DESIGN.
- Question answered -> rule into its owning DESIGN section. Open questions
  live in STATE.md ONLY. verify_docs.R enforces this — trust its exit status.
