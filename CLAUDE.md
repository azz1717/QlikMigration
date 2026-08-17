# C:\Rtools — session rules

## Cold start (this, and only this)
1. Read STATE.md, INTERFACES.md, `git log --oneline -15`.
2. Read ONLY the DESIGN.md sections STATE.md names for the current task.
3. Give Adam a short plan with the open decision points BEFORE implementing.

Do NOT read HANDOVER-PROTOCOL.md. It looks like orientation (it's a
"protocol" file sitting at the repo root) and isn't — it's a one-time
bootstrap record for rebuilding these three files if they're ever lost.
Opening it "just to be safe" is exactly the over-reading habit this file
exists to prevent.

## Reading budget
- NEVER read fixtures into context: [Grant Managing Region].txt, app-unbuilt/,
  script_out.txt, script_qvs_out.txt. Grep/sed specific lines when needed.
- EXCEPTION: formatexample.txt and its pipeline output (formatexample_after.txt
  or equivalent) MAY be read directly — it's the stage-1 fixture in the Testing
  methodology below and small enough to eyeball (Adam, 2026-08-17).
- NEVER read HANDOVER-PROTOCOL.md — see Cold start above.
- Don't read a pass's source unless editing that pass — INTERFACES.md is the
  contract. Don't read DESIGN.md or README.md end-to-end; go by section.

## Working with Adam
- Style/scope calls are Adam's. Ask as multiple-choice the moment one
  appears; never deliberate silently past two unknowns.
- Short progress note at each milestone (design settled / code written /
  verify green) — not one wall of output at the end.

## Invariants (do not re-derive)
- Verify before and after any change; it gates on exit status:
  & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" verify.R   (not on PATH)
- Base R only. Token stream, not text. Every pass skips SELECT...;.
- Check INTERFACES.md shared scanners before writing any helper.
- Multi-line commit messages: write to a scratch file, `git commit -F <file>`.

## Testing methodology (Adam, 2026-08-17) — gated, three stages, cheapest first
A change is NOT "pass testing" until all three stages below have Adam's
sign-off, in order. Do not skip ahead or batch stages together.
1. **formatexample.txt** — run the pipeline, eyeball the output myself
   (permitted, see Reading budget). If it looks right, hand it to Adam for
   his own sign-off. No verify.R requirement at this stage.
2. **[Grant Managing Region].txt** — run the pipeline only. I do NOT read the
   output (too large, stays in the never-read list). Hand the file to Adam;
   he reviews it himself. If something's off he'll name a region (e.g. a
   table name) for me to grep, not a general re-read. No verify.R requirement
   at this stage.
3. **script.qvs** — run the pipeline AND verify.R; verify.R passing (exit 0)
   is mandatory here. Once green, hand the output to Adam — he either signs
   off or names a specific region to check.
Point of the staging: cheap/small first so a regression surfaces before
spending tokens on the big files, and I never bulk-read the two large
fixtures — Adam reviews those, I only grep where he directs.

**verify.R's own fixtures list is `[Grant Managing Region].txt` +
`script.qvs` (see its `main()`).** Running verify.R therefore ALWAYS
touches both, regardless of which stage is active. Do NOT run verify.R as a
reflexive "verify before/after any change" habit while stage 1 or stage 2
is still open — that runs the pipeline against a later stage's fixture
before its turn, which is the exact thing staging exists to prevent
(violated 2026-08-17, Adam had to say so directly). verify.R is invoked
ONLY at stage 3, on Adam's go-ahead. A specific, explicit sequencing
instruction from Adam overrides a general standing invariant when the two
conflict — don't default to the general rule.

## Maintenance duty (same commit as the change, NOT a session-end step)
- Public function signature/behaviour changed -> update INTERFACES.md entry.
- Task started or finished -> update STATE.md (keep it <= 25 lines).
- A pass added/reordered -> update run_pipeline.R, verify.R's PASSES, README's
  pass table AND its worked example, and the relevant DESIGN section. README
  was missed once already (fixed in a2f1a57) — check it explicitly.
- This is the ONLY mechanism that keeps STATE.md/INTERFACES.md current.
  There is no separate end-of-session handover step (Adam's explicit choice,
  2026-08-17) — a session that ends without committing leaves no updated
  record, and that's accepted as the tradeoff.
