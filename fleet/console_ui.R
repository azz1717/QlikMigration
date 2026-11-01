# console_ui.R - the Qlik app migration walk-through. Three questions, one
# confirmation, then it runs and ends on one results screen:
#   1. which space the apps are in
#   2. which apps (all, or by number)
#   3. where they go: back over the originals, or as copies into another space
# then download -> format + retarget -> upload -> check, one line per stage.
#
# It HOLDS NO LOGIC: every step is one `fleet.R` verb run in its own process,
# so the same migration is reproducible from a command line (RUNBOOK.md).
# Reports are NOT part of this flow (Adam, 2026-11-02). RUN_REPORTS.bat runs
# this file with --reports: the same space and app questions, then download
# and `fleet.R report` on that pick, ending on the report screen.
#
# QUIET BY DESIGN (Adam, 2026-11-02: "i don't need all the verbose console
# readouts"): each verb's whole output goes to fleet/last_run.log, and the
# screen gets one line per stage plus `fleet.R results`, which names every
# app that did not reach the cloud with the reason in qlik's own words.
#
# 72 columns, ASCII only (DESIGN.md section 8.5: the return channel from the
# VM is a photograph of the screen).

RSCRIPT <- file.path(R.home("bin"), "Rscript")
RUN_LOG <- file.path("fleet", "last_run.log")
SCREEN_W <- 72L

# One persistent stdin connection for the whole run. GOTCHA (verified
# 2026-08-19): readLines("stdin", n=1) called repeatedly reopens a fresh
# anonymous connection each time, which silently discards whatever the
# previous call's read-ahead buffer had left over on stdin - every call
# after the first returns character(0), no error. Opening the connection
# once up front and reusing it avoids that entirely.
.cui_stdin <- file("stdin")
open(.cui_stdin, "r")

.cui_read_line <- function(prompt) {
	cat(prompt, "\n", sep = "")
	v <- readLines(.cui_stdin, n = 1)
	if (!length(v)) "" else v
}

.cui_clip <- function(s) ifelse(nchar(s) > SCREEN_W,
                                paste0(substr(s, 1L, SCREEN_W - 3L), "..."), s)
.cui_rule <- function() cat(strrep("-", SCREEN_W), "\n", sep = "")

# One fleet.R run, its output captured into RUN_LOG instead of the screen.
# stdout and stderr are captured together and then written, never both
# redirected to one file (on Windows the second handle truncates the first -
# fleet.R's .fl_child() has the same rule). Returns list(status, out).
.cui_fleet <- function(...) {
	args <- c(shQuote(file.path("fleet", "fleet.R")), ...)
	out <- tryCatch(suppressWarnings(system2(RSCRIPT, args, stdout = TRUE,
	                                         stderr = TRUE)),
	                error = function(e) structure(conditionMessage(e), status = -1L))
	st <- attr(out, "status")
	if (is.null(st)) st <- 0L
	out <- as.character(out)
	cat("\n$ Rscript fleet/fleet.R ", paste(args[-1], collapse = " "), "  [",
	    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] exit ", st, "\n", sep = "",
	    file = RUN_LOG, append = TRUE)
	if (length(out)) cat(out, sep = "\n", file = RUN_LOG, append = TRUE)
	list(status = as.integer(st), out = out)
}

# The last thing a failed verb said, for the one line the screen gives it.
.cui_why <- function(r) {
	o <- trimws(r$out[nzchar(trimws(r$out))])
	if (!length(o)) return("(no output)")
	paste(utils::tail(o, 2L), collapse = " | ")
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
		cat(.cui_clip(sprintf("[%d] %s  (%s)", i, s$name[i], s$type[i])), "\n", sep = "")
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

# How many apps the last `add` picked - what the confirmation sentence counts.
# Presentation only; the selection itself is fleet.R's job (--selected).
.cui_count <- function() {
	p <- file.path("fleet", "selection.csv")
	if (!file.exists(p)) return(0L)
	s <- tryCatch(utils::read.csv(p, colClasses = "character",
	                              stringsAsFactors = FALSE),
	              error = function(e) NULL)
	if (is.null(s)) 0L else nrow(s)
}

# "1,3-5" or "all", expanded into the index list `add --apps` takes. The ONE
# piece of parsing this menu owns, and it is presentation: the operator typed
# a range at a prompt, and a range is not something a CLI flag should have to
# understand. Returns "all", a comma list, or "" when the input makes no sense.
.cui_expand_sel <- function(s) {
	s <- tolower(trimws(s))
	if (!nzchar(s)) return("")
	if (s == "all") return("all")
	out <- integer(0)
	for (part in trimws(strsplit(s, ",", fixed = TRUE)[[1]])) {
		if (!nzchar(part)) next
		if (grepl("^[0-9]+-[0-9]+$", part)) {
			ab <- as.integer(strsplit(part, "-", fixed = TRUE)[[1]])
			if (ab[1] > ab[2]) return("")
			out <- c(out, ab[1]:ab[2])
		} else if (grepl("^[0-9]+$", part)) {
			out <- c(out, as.integer(part))
		} else return("")
	}
	out <- sort(unique(out))
	if (!length(out) || any(out < 1L)) return("")
	paste(out, collapse = ",")
}

# Questions 1 and 2, shared by the migration and the reports: which space,
# which apps, and `add` records the pick (fleet/selection.csv, which every
# later `--selected` reads). Returns list(src, sp), or NULL when stopped.
.cui_pick <- function() {
	cat("\n1. Which space are the apps in?\n")
	r <- .cui_fleet("spaces")
	sp <- if (r$status == 0L) .cui_spaces() else NULL
	if (is.null(sp)) {
		cat(.cui_clip(paste0("Could not list spaces: ", .cui_why(r))), "\n",
		    "Is qlik-cli signed in? (qlik context use <name>)\n", sep = "")
		return(NULL)
	}
	.cui_show_spaces(sp)
	src <- .cui_ask_space(sp, "Space number (Q to quit):")
	if (is.null(src)) return(NULL)

	cat("\n2. Which apps in \"", src$name, "\"?\n", sep = "")
	r <- .cui_fleet("apps", "--space", shQuote(src$id))
	if (r$status != 0L) {
		cat(.cui_clip(paste0("Could not list the apps: ", .cui_why(r))), "\n", sep = "")
		return(NULL)
	}
	rows <- grep("^\\[[0-9]+\\] ", r$out, value = TRUE)
	if (!length(rows)) { cat("That space has no apps.\n"); return(NULL) }
	cat(.cui_clip(rows), sep = "\n")
	repeat {
		v <- .cui_read_line("Apps: all, or numbers like 1,3-5 (Q to quit):")
		if (identical(toupper(trimws(v)), "Q") || !nzchar(trimws(v))) return(NULL)
		sel <- .cui_expand_sel(v)
		if (nzchar(sel)) break
		cat("Type all, or numbers from the list (1,3-5), or Q to quit.\n")
	}
	add <- if (identical(sel, "all")) "--all" else c("--apps", shQuote(sel))
	r <- .cui_fleet("add", "--space", shQuote(src$id), add)
	if (r$status != 0L) {
		cat(.cui_clip(paste0("Could not record the pick: ", .cui_why(r))), "\n", sep = "")
		return(NULL)
	}
	list(src = src, sp = sp)
}

# The migration's three questions and the confirmation. Returns
# list(src, dest, copy) when the operator typed YES, NULL when they stopped.
.cui_ask <- function() {
	pk <- .cui_pick()
	if (is.null(pk)) return(NULL)
	src <- pk$src; sp <- pk$sp

	cat("\n3. Where should the finished apps go?\n")
	cat(.cui_clip(paste0("[1] Back into \"", src$name, "\" - overwrite the originals")),
	    "\n", sep = "")
	cat("[2] Into another space - as copies named \"<app> [mig]\"\n")
	where <- .cui_ask_choice(c("1", "2"), "Choice (Q to quit):")
	if (is.null(where)) return(NULL)
	dest <- src
	if (where == "2") {
		cat("Which space should the copies go to?\n")
		.cui_show_spaces(sp)
		dest <- .cui_ask_space(sp, "Space number (Q to quit):")
		if (is.null(dest)) return(NULL)
	}

	n <- .cui_count()
	cat("\n")
	.cui_rule()
	cat(.cui_clip(paste0(n, " app(s) from \"", src$name, "\"")), "\n", sep = "")
	cat(.cui_clip(if (where == "2") paste0("  -> copies in \"", dest$name, "\"")
	              else "  -> overwrite the originals in place"), "\n", sep = "")
	cat("Download, format + retarget, upload, then check each upload.\n")
	.cui_rule()
	if (!identical(trimws(.cui_read_line(
		"Type YES to start, anything else to stop:")), "YES")) {
		cat("Stopped. Nothing in the cloud was changed.\n")
		return(NULL)
	}
	list(src = src, dest = dest, copy = where == "2")
}

# Download -> format + retarget -> upload -> check, then the results screen.
# A stage that exits 2 ("some apps did not make it") carries on - the others
# are still worth doing, and `results` names the ones that stopped. Any other
# non-zero exit is the step itself failing, and the run stops there.
.cui_run <- function(plan) {
	pick <- "--selected"
	mode <- if (plan$copy) c("--mode", "copy", "--to-space", shQuote(plan$dest$id))
	        else c("--mode", "overwrite")
	stages <- list(
		list("Downloading the apps", c("fetch", pick, "--live")),
		list("Formatting + retargeting", c("process", pick)),
		list("Uploading", c("upload", mode, pick, "--live")),
		list("Checking each upload", c("verify", pick, "--live")))
	cat("\nRunning. Every detail goes to ", RUN_LOG, "\n", sep = "")
	for (k in seq_along(stages)) {
		cat(sprintf("  %d/%d %s ...\n", k, length(stages), stages[[k]][[1]]))
		r <- do.call(.cui_fleet, as.list(stages[[k]][[2]]))
		if (!r$status %in% c(0L, 2L)) {
			cat(.cui_clip(paste0("  STOPPED: that step failed (exit ", r$status, "): ",
			                     .cui_why(r))), "\n", sep = "")
			break
		}
	}
	cat("\n")
	r <- .cui_fleet("results", pick)
	cat(r$out, sep = "\n")
	cat("Full log of this run: ", RUN_LOG, "\n", sep = "")
	cat("Each app's qlik replies: fleet/apps/<app name>/upload.log\n")
	invisible(r$status)
}

# RUN_REPORTS.bat (`console_ui.R --reports`): the same two questions, then
# download the picked apps (a tenant READ - nothing in the cloud changes) and
# write each one's report. Ends on the screen `report` prints.
.cui_reports <- function() {
	pk <- .cui_pick()
	if (is.null(pk)) return(invisible(NULL))
	stages <- list(
		list("Downloading the apps", c("fetch", "--selected", "--live")),
		list("Writing the reports", c("report", "--selected")))
	cat("\nRunning. Every detail goes to ", RUN_LOG, "\n", sep = "")
	r <- NULL
	for (k in seq_along(stages)) {
		cat(sprintf("  %d/%d %s ...\n", k, length(stages), stages[[k]][[1]]))
		r <- do.call(.cui_fleet, as.list(stages[[k]][[2]]))
		if (!r$status %in% c(0L, 2L)) {
			cat(.cui_clip(paste0("  STOPPED: that step failed (exit ", r$status, "): ",
			                     .cui_why(r))), "\n", sep = "")
			return(invisible(NULL))
		}
	}
	cat("\n")
	at <- grep("^=+$", r$out)
	if (length(at)) cat(r$out[at[1]:length(r$out)], sep = "\n")
	cat("Full log of this run: ", RUN_LOG, "\n", sep = "")
	invisible(NULL)
}

main <- function() {
	reports <- "--reports" %in% commandArgs(trailingOnly = TRUE)
	cat(if (reports) "Qlik app reports\n" else "Qlik app migration\n")
	.cui_rule()
	dir.create("fleet", showWarnings = FALSE)
	cat("=== console run ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "",
	    file = RUN_LOG)
	if (reports) .cui_reports() else {
		plan <- .cui_ask()
		if (!is.null(plan)) .cui_run(plan)
	}
	cat("Bye.\n")
}

if (identical(environment(), globalenv())) main()
