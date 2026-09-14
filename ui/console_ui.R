# console_ui.R - the Qlik app migration walk-through, four steps, one line
# of travel. Step 1 picks the space, step 2 unbuilds every app in it, step 3
# asks what to do with them (format + retarget, or report only), step 4 asks
# where the rebuilt apps go and takes the one confirmation this tool has.
#
# It HOLDS NO LOGIC: every step is one `fleet.R` verb run in its own process,
# so the same migration is reproducible from a command line without this
# walk-through; RUNBOOK.md has the same four steps written as commands. The
# other verbs (status, stamp, reconcile, doctor, rollup, single-app styling
# and reports) are command-line only - see RUNBOOK.md.
#
# 72 columns, ASCII only (DESIGN.md section 8.5: the return channel from the
# VM is a photograph of the screen).

RSCRIPT <- file.path(R.home("bin"), "Rscript")

# One persistent stdin connection for the whole run. GOTCHA (verified
# 2026-08-19): readLines("stdin", n=1) called repeatedly reopens a fresh
# anonymous connection each time, which silently discards whatever the
# previous call's read-ahead buffer had left over on stdin - every call
# after the first returns character(0), no error. Opening the connection
# once up front and reusing it avoids that entirely.
.cui_stdin <- file("stdin")
open(.cui_stdin, "r")

.cui_read_line <- function(prompt) {
	cat(prompt)
	v <- readLines(.cui_stdin, n = 1)
	if (!length(v)) "" else v
}

# Every step is one fleet.R run. Nothing is echoed while things go well - the
# operator is reading fleet.R's own sentences, not a transcript of commands.
# A bad exit prints the command so the failure can be reproduced by hand.
# Status 2 is "some apps blocked", which is information, not a failure.
.cui_fleet <- function(...) {
	args <- c(shQuote(file.path("fleet", "fleet.R")), ...)
	status <- system2(RSCRIPT, args)
	if (!status %in% c(0, 2))
		cat("That step failed (fleet.R exit ", status, "). Command: ",
		    "Rscript fleet/fleet.R ", paste(args[-1], collapse = " "), "\n",
		    sep = "")
	invisible(status)
}

# The space list fleet.R cached on its last `spaces` run (index, id, name,
# type). The screen shows names; the UI passes fleet.R the id, never the
# index, so a stale cache can never point a step at the wrong space.
.cui_spaces <- function() {
	p <- file.path("fleet", "spaces.csv")
	if (!file.exists(p)) return(NULL)
	s <- tryCatch(utils::read.csv(p, colClasses = "character",
	                              stringsAsFactors = FALSE),
	              error = function(e) NULL)
	if (is.null(s) || !nrow(s) || !all(c("id", "name") %in% names(s))) return(NULL)
	if (is.null(s$type)) s$type <- ""
	s
}

.cui_show_spaces <- function(s) {
	for (i in seq_len(nrow(s)))
		cat(sprintf("[%d] %s  (%s)\n", i, s$name[i], s$type[i]))
}

# A number from the list, or blank/Q to stop. Returns the row, or NULL.
.cui_ask_space <- function(s, prompt) {
	repeat {
		v <- toupper(trimws(.cui_read_line(prompt)))
		if (!nzchar(v) || v == "Q") return(NULL)
		k <- suppressWarnings(as.integer(v))
		if (!is.na(k) && k >= 1 && k <= nrow(s)) return(s[k, ])
		cat("Type one of the numbers above, or Q to quit.\n")
	}
}

# A number from a fixed list of choices on this screen, or blank/Q to stop.
.cui_ask_choice <- function(choices, prompt) {
	repeat {
		v <- toupper(trimws(.cui_read_line(prompt)))
		if (!nzchar(v) || v == "Q") return(NULL)
		if (v %in% choices) return(v)
		cat("Type ", paste(choices, collapse = " or "), ", or Q to quit.\n",
		    sep = "")
	}
}

# How many ledger rows this space has. Presentation only - it is what the
# confirmation sentence counts; the selection itself is fleet.R's job.
.cui_count <- function(space_id) {
	p <- file.path("fleet", "manifest.csv")
	if (!file.exists(p)) return(0L)
	m <- tryCatch(utils::read.csv(p, colClasses = "character",
	                              stringsAsFactors = FALSE),
	              error = function(e) NULL)
	if (is.null(m) || !nrow(m) || is.null(m$space_id)) return(0L)
	sum(m$space_id == space_id)
}

.cui_finished <- function() {
	cat("\nFinished.\n")
	cat("  Each app's files:        fleet/apps/<app name>/\n")
	cat("  Loads that need a hand:  fleet/apps/<app name>/dev_notes.txt\n")
	cat("                           (all apps together: fleet/master_loads.csv)\n")
	cat("  What is at what stage:   fleet/master.csv\n")
}

# One pass of the walk-through. TRUE when it ran to the end (so the operator
# is asked whether to do another space), FALSE when they stopped it.
.cui_walk <- function() {
	cat("\nStep 1 of 4 - Which space are the apps in?\n")
	if (.cui_fleet("spaces") != 0) return(FALSE)
	sp <- .cui_spaces()
	if (is.null(sp)) {
		cat("Could not list spaces - is qlik-cli signed in?",
		    " (qlik context use <name>)\n", sep = "")
		return(FALSE)
	}
	src <- .cui_ask_space(sp, "Space number (or Q to quit): ")
	if (is.null(src)) return(FALSE)

	cat("\nStep 2 of 4 - Unbuilding every app in \"", src$name, "\"\n", sep = "")
	if (.cui_fleet("add", "--space", shQuote(src$id), "--all") != 0) return(FALSE)
	.cui_fleet("fetch", "--space", shQuote(src$id), "--live")

	cat("\nStep 3 of 4 - What do you want done to them?\n")
	cat("[1] Format + retarget the scripts, then rebuild the apps in the cloud\n")
	cat("[2] Report only - review + tech-debt report per app; nothing in the",
	    " cloud changes\n", sep = "")
	what <- .cui_ask_choice(c("1", "2"), "Choice (or Q to quit): ")
	if (is.null(what)) return(FALSE)
	if (what == "2") {
		.cui_fleet("report", "--space", shQuote(src$id))
		cat("\nDone. Each app's report is fleet/apps/<app name>/report.html\n")
		return(TRUE)
	}
	.cui_fleet("process", "--space", shQuote(src$id))

	cat("\nStep 4 of 4 - Where do the rebuilt apps go?\n")
	cat("[1] Overwrite the originals in \"", src$name, "\"\n", sep = "")
	cat("[2] Upload as copies, named \"<app> [mig]\", into another space\n")
	where <- .cui_ask_choice(c("1", "2"), "Choice (or Q to quit): ")
	if (is.null(where)) return(FALSE)
	dest <- src
	if (where == "2") {
		cat("Which space should the copies go to?\n")
		.cui_show_spaces(sp)
		dest <- .cui_ask_space(sp, "Space number (or Q to quit): ")
		if (is.null(dest)) return(FALSE)
	}

	n <- .cui_count(src$id)
	if (where == "2")
		cat("\nAbout to upload ", n, " app(s) as copies into \"", dest$name,
		    "\".\n", sep = "")
	else
		cat("\nAbout to upload ", n, " app(s), overwriting the originals in \"",
		    dest$name, "\".\n", sep = "")
	if (!identical(trimws(.cui_read_line(
		"Type YES to go ahead, anything else to stop: ")), "YES")) {
		cat("Stopped. Nothing in the cloud was changed.\n")
		return(FALSE)
	}
	mode <- if (where == "2") "copy" else "overwrite"
	to <- if (where == "2") c("--to-space", shQuote(dest$id)) else NULL
	sel <- c("--space", shQuote(src$id))
	.cui_fleet("upload", "--mode", mode, to, sel, "--live")
	.cui_fleet("verify", sel, "--live")
	.cui_finished()
	TRUE
}

main <- function() {
	cat("Qlik app migration\n")
	repeat {
		if (!.cui_walk()) break
		again <- toupper(trimws(
			.cui_read_line("\nDo another space? Enter = yes, Q = quit: ")))
		if (again == "Q") break
	}
	cat("Bye.\n")
}

if (identical(environment(), globalenv())) main()
