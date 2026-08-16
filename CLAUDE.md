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
  formatexample.txt, script_out.txt. Grep/sed specific lines when needed.
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
