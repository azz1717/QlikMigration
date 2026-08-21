# qlik_cli_probe.R — can R invoke qlik-cli, and can it reach a named space?
# SPIKE, not shipped tooling. Not a pass, not sourced by run_pipeline.R, no
# INTERFACES.md entry yet by design (Adam 2026-08-21: "spike now, promote if
# it works"). Promote only once it has run green on a real VM.
#
# WHY IT IS SHAPED LIKE THIS. It is written to be run somewhere I cannot see,
# on a machine I cannot reach, where one round-trip costs a push/pull cycle.
# So every rung reports independently, nothing stops at the first failure that
# later rungs can still survive, and a failed subcommand automatically captures
# its own --help. The point is that ONE run produces enough evidence to fix the
# next thing, instead of three runs producing three guesses.
#
# THE ACTUAL QUESTION is narrower than "does qlik work" — Adam has already
# confirmed qlik-cli runs and connects to the tenant by hand. What is unproven
# is whether it still works when R is the one invoking it: PATH, quoting of a
# path containing spaces, and whether the saved qlik context is visible to a
# child process of R. So every rung prints the equivalent hand-runnable command
# (HINT lines). If a rung fails from R but that same line works when pasted
# into a shell, the fault is in the R invocation, which is the whole subject of
# the test. That distinction is the deliverable.

# --- where the project lives ---------------------------------------------
# Same commandArgs() self-location as run_pipeline.R and verify.R: --file= is
# the only method that resolves under plain `Rscript file.R` (sys.frame()$ofile
# does not — verified 2026-08-21). Deliberately the same idiom, not a variant.
.file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
PROJECT_DIR <- if (length(.file_arg)) {
	tryCatch(dirname(normalizePath(sub("^--file=", "", .file_arg[1]))),
	         error = function(e) NA)
} else NA
if (is.na(PROJECT_DIR) || !dir.exists(PROJECT_DIR)) {
	stop("could not determine the project folder. Run this as ",
	     "'Rscript qlik_cli_probe.R'.", call. = FALSE)
}
setwd(PROJECT_DIR)

CONFIG_FILE <- "qlik_cli_path.txt"
OUT_DIR     <- "qlik_probe_out"
SPACE_NAME  <- "On Prem Apps"
TIMEOUT_S   <- 60

# --- rung bookkeeping -----------------------------------------------------
# Results accumulate into one frame so the run ends with a summary that fits
# on a screen. Adam relays that summary back; the raw files back it up.
.results <- list()
.record <- function(rung, label, ok, detail) {
	.results[[length(.results) + 1L]] <<-
		list(rung = rung, label = label, ok = ok, detail = detail)
	cat(sprintf("  [%s] %d. %s%s\n",
	            if (ok) "PASS" else "FAIL", rung, label,
	            if (nzchar(detail)) paste0(" — ", detail) else ""))
}

# --- running qlik ---------------------------------------------------------
# Invocation follows console_ui.R:72 exactly — command passed UNQUOTED,
# arguments individually shQuote()d. That pairing is already proven on a path
# containing a space ("C:\Program Files\R\..."), which is the same hazard as
# "D:\installed software\qlik-cli\". Do not "fix" this by wrapping the command
# in shQuote() too; on Windows that double-quotes it and it stops resolving.
#
# timeout is the guard against the console-input trap: if qlik ever decides to
# prompt (expired context, confirmation), an un-timed system2() hangs the whole
# session with no way out. R kills it at TIMEOUT_S and reports status 124.
.qlik_hint <- function(qlik, args) {
	paste(c(shQuote(qlik), args), collapse = " ")
}

.run_qlik <- function(qlik, args, slug) {
	hint <- .qlik_hint(qlik, args)
	cat("      HINT (run by hand if this rung fails): ", hint, "\n", sep = "")
	out <- tryCatch(
		suppressWarnings(system2(qlik, shQuote(args),
		                         stdout = TRUE, stderr = TRUE,
		                         timeout = TIMEOUT_S)),
		error = function(e) structure(paste("R could not start the process:",
		                                    conditionMessage(e)),
		                              status = -1L))
	status <- attr(out, "status")
	if (is.null(status)) status <- 0L
	if (!is.null(slug)) {
		writeLines(c(paste("$", hint), paste("# exit status:", status), "", out),
		           file.path(OUT_DIR, paste0(slug, ".txt")))
	}
	list(out = out, status = status, hint = hint)
}

# A failed subcommand captures its own --help, so the log Adam sends back
# already contains the correct usage. Costs one extra call on the failure
# path and can save an entire round-trip when a guessed flag was wrong.
.capture_help <- function(qlik, subcommand, slug) {
	cat("      (capturing '", paste(subcommand, collapse = " "),
	    " --help' so the correct usage comes back with this log)\n", sep = "")
	.run_qlik(qlik, c(subcommand, "--help"), slug)
}

cat("\nqlik-cli reachability probe\n")
cat("Project: ", PROJECT_DIR, "\n", sep = "")
cat(strrep("-", 68), "\n", sep = "")

# Cleared, not merely created. A rung that does not run this time leaves its
# file from LAST time sitting there, and that file is evidence I will read as
# if it were current — a stale 06-apps-in-space.txt after a failed run says
# the apps listed fine when it never ran at all. Caught 2026-08-21 doing
# exactly that during stub testing. Every run starts empty so the folder can
# only ever describe one run.
if (dir.exists(OUT_DIR)) unlink(file.path(OUT_DIR, "*.txt"))
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

# --- rung 1: locate qlik.exe ---------------------------------------------
# One-line config file (Adam's call 2026-08-21). It is gitignored, so each VM
# carries its own path and a pulled snapshot never overwrites it. That also
# means a fresh VM has no config at all, which is why the failure message
# spells out the exact file and content to create rather than just complaining.
QLIK <- NA
if (!file.exists(CONFIG_FILE)) {
	.record(1, "locate qlik.exe", FALSE,
	        paste0("no ", CONFIG_FILE))
	cat("\n", strrep("-", 68), "\n", sep = "")
	cat("Create ", CONFIG_FILE, " in ", PROJECT_DIR, " containing one line:\n\n",
	    "    D:\\installed software\\qlik-cli\\qlik.exe\n\n",
	    "(see ", CONFIG_FILE, ".example). Then run this again.\n", sep = "")
	quit(status = 1)
}

.cfg <- readLines(CONFIG_FILE, warn = FALSE)
.cfg <- trimws(.cfg)
.cfg <- .cfg[nzchar(.cfg) & !startsWith(.cfg, "#")]
if (!length(.cfg)) {
	.record(1, "locate qlik.exe", FALSE,
	        paste0(CONFIG_FILE, " is empty"))
	quit(status = 1)
}
# Quotes are stripped because pasting from Explorer's "Copy as path" wraps the
# path in them, and that is the likeliest way this file gets filled in.
QLIK <- gsub('^"|"$', "", .cfg[1])

if (!file.exists(QLIK)) {
	.record(1, "locate qlik.exe", FALSE, paste0("not found: ", QLIK))
	cat("\n", strrep("-", 68), "\n", sep = "")
	cat("That path came from ", CONFIG_FILE, ". Check it and run again.\n", sep = "")
	quit(status = 1)
}
.record(1, "locate qlik.exe", TRUE, QLIK)

# --- rung 2: can R invoke it at all? -------------------------------------
# The core question. Everything below is meaningless if this fails, so this
# is the one rung that stops the run.
r2 <- .run_qlik(QLIK, "version", "02-version")
if (r2$status != 0) {
	.record(2, "invoke qlik from R", FALSE,
	        paste0("exit ", r2$status,
	               if (r2$status == 124) " (TIMED OUT — it may be prompting)" else ""))
	cat("\n", strrep("-", 68), "\n", sep = "")
	cat("STOPPING: if R cannot run 'qlik version' nothing below can work.\n",
	    "Run the HINT line above by hand. If it works there but not here,\n",
	    "the fault is in R's invocation and that is exactly what to report.\n", sep = "")
	cat("Output was:\n"); cat(paste0("    ", r2$out, collapse = "\n"), "\n")
	quit(status = 1)
}
.record(2, "invoke qlik from R", TRUE, paste(r2$out, collapse = " "))

# --- rung 3: is the saved context visible to a child of R? ---------------
# Adam authenticates with a saved context (qlik context use), which lives in
# his user profile. A child process of R runs as the same user, so it SHOULD
# inherit it — but "should" is the assumption this whole day exists to test.
r3 <- .run_qlik(QLIK, c("context", "ls"), "03-context-ls")
if (r3$status != 0) {
	.record(3, "saved context visible", FALSE, paste0("exit ", r3$status))
	.capture_help(QLIK, "context", "03-context-help")
} else {
	.record(3, "saved context visible", TRUE,
	        paste0(length(r3$out), " line(s) returned"))
}

# --- rung 4: list spaces -------------------------------------------------
# Captured TWICE, default and explicit --json. I have never seen this
# command's output, so I am not guessing which one it is: whichever parses
# more cleanly becomes what the promoted version uses.
r4 <- .run_qlik(QLIK, c("space", "ls"), "04-space-ls-default")
if (r4$status != 0) {
	.record(4, "list spaces", FALSE, paste0("exit ", r4$status))
	.capture_help(QLIK, "space", "04-space-help")
} else {
	.record(4, "list spaces", TRUE, paste0(length(r4$out), " line(s)"))
}
r4j <- .run_qlik(QLIK, c("space", "ls", "--json"), "05-space-ls-json")
if (r4j$status != 0) {
	.record(5, "list spaces (--json)", FALSE,
	        paste0("exit ", r4j$status, " — may simply not be a valid flag"))
} else {
	.record(5, "list spaces (--json)", TRUE, paste0(length(r4j$out), " line(s)"))
}

# --- rung 6: is the target space there? ----------------------------------
# A substring test, deliberately. Confirming the space is REACHABLE needs no
# parser, and json_strings.R cannot help here — it extracts string literals
# and discards structure by design (INTERFACES.md), so it cannot associate a
# name with its id. Writing a real JSON parser against output I have never
# seen is the mistake this rung is shaped to avoid.
.haystack <- c(r4$out, r4j$out)
.found <- any(grepl(SPACE_NAME, .haystack, fixed = TRUE))
.record(6, paste0("space '", SPACE_NAME, "' present"), .found,
        if (.found) "matched in space listing" else "NOT found in either listing")

# --- rung 7: best-effort id, then the apps in it -------------------------
# HEURISTIC, and labelled as one: take the "id" nearest-preceding the line
# holding the space name. That is right for a flat array of objects and wrong
# for anything nested. It is here to get today unblocked, not to be kept —
# the raw dumps are what a correct implementation gets written against.
SPACE_ID <- NA
if (.found) {
	flat <- paste(.haystack, collapse = "\n")
	at <- regexpr(SPACE_NAME, flat, fixed = TRUE)
	if (at > 0) {
		before <- substr(flat, 1, at - 1)
		ids <- gregexpr('"id"[[:space:]]*:[[:space:]]*"([^"]+)"', before)[[1]]
		if (ids[1] > 0) {
			last <- ids[length(ids)]
			frag <- substr(before, last, last + attr(ids, "match.length")[length(ids)] - 1)
			SPACE_ID <- sub('.*"([^"]+)"$', "\\1", frag)
		}
	}
}
if (is.na(SPACE_ID)) {
	.record(7, "extract space id", FALSE,
	        "heuristic found none — read qlik_probe_out/ and tell me the real shape")
} else {
	.record(7, "extract space id", TRUE, paste0(SPACE_ID, " (heuristic)"))

	r8 <- .run_qlik(QLIK, c("app", "ls", "--spaceId", SPACE_ID), "06-apps-in-space")
	if (r8$status != 0) {
		.record(8, "list apps in space", FALSE, paste0("exit ", r8$status))
		.capture_help(QLIK, "app", "06-app-help")
	} else {
		.record(8, "list apps in space", TRUE, paste0(length(r8$out), " line(s)"))
	}
}

# --- summary --------------------------------------------------------------
cat("\n", strrep("-", 68), "\n", sep = "")
.pass <- sum(vapply(.results, function(r) isTRUE(r$ok), logical(1)))
cat(sprintf("%d of %d rungs passed.\n", .pass, length(.results)))
cat("Raw output: ", normalizePath(OUT_DIR), "\n", sep = "")
cat("\nSend back: the lines above, plus the files in ", OUT_DIR, ".\n", sep = "")
cat("Those files are what a correct parser gets written against — until then\n",
    "rung 7 is a guess and is marked as one.\n", sep = "")
