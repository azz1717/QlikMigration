# console_ui.R — crude console launcher over run_pipeline.R and render_report.R.
# Script, not sourced tooling: shells out to each pipeline as its own Rscript
# process (INTERFACES.md: run_pipeline.R is "a script, not a function").
# Not part of run_pipeline.R, not a pass, not phase 2 tooling — a menu on top.

RSCRIPT <- file.path(R.home("bin"), "Rscript")

# One persistent stdin connection for the whole run. GOTCHA (verified
# 2026-08-19): readLines("stdin", n=1) called repeatedly reopens a fresh
# anonymous connection each time, which silently discards whatever the
# previous call's read-ahead buffer had left over on stdin — every call
# after the first returns character(0), no error. Opening the connection
# once up front and reusing it avoids that entirely.
.cui_stdin <- file("stdin")
open(.cui_stdin, "r")

.cui_read_line <- function(prompt) {
	cat(prompt)
	readLines(.cui_stdin, n = 1)
}

# Any subfolder of the working directory holding app-properties.json.
# Display name = qTitle from that file, falling back to folder name — same
# rule render_report.R's .rr_title() uses, so menu names match report titles.
.cui_find_apps <- function(wd) {
	hits <- list.files(wd, pattern = "^app-properties\\.json$", recursive = TRUE, full.names = TRUE)
	dirs <- unique(dirname(hits))
	titles <- vapply(dirs, function(d) {
		title <- tryCatch({
			txt <- readLines(file.path(d, "app-properties.json"), warn = FALSE, encoding = "UTF-8")
			m <- regmatches(txt, regexpr('"qTitle"\\s*:\\s*"[^"]*"', txt))
			m <- m[nzchar(m)]
			if (length(m)) sub('.*"qTitle"\\s*:\\s*"([^"]*)".*', "\\1", m[1]) else NA_character_
		}, error = function(e) NA_character_)
		if (is.na(title) || !nzchar(title)) basename(d) else title
	}, character(1))
	data.frame(dir = dirs, title = titles, stringsAsFactors = FALSE)
}

.cui_pick_app <- function(wd) {
	repeat {
		apps <- .cui_find_apps(wd)
		cat("\nWorking directory:", wd, "\n\n")
		if (nrow(apps) == 0) {
			cat("No apps found (no app-properties.json in any subfolder).\n")
		} else {
			for (i in seq_len(nrow(apps))) cat(sprintf("[%d] %s\n", i, apps$title[i]))
		}
		cat("[W] change working directory\n[Q] quit\n")
		choice <- .cui_read_line("> ")
		u <- toupper(trimws(choice))
		if (u == "Q") return(NULL)
		if (u == "W") {
			new_wd <- .cui_read_line("New working directory: ")
			if (dir.exists(new_wd)) wd <- new_wd else cat("No such directory.\n")
			next
		}
		idx <- suppressWarnings(as.integer(u))
		if (!is.na(idx) && idx >= 1 && idx <= nrow(apps)) return(apps[idx, ])
		cat("Not a valid choice.\n")
	}
}

# Both outputs land in outputs/ (2026-08-21 reorg), not beside their app and
# not at bare repo root - so results are always in one obvious place instead
# of scattered across however many app folders were processed. Prefixed with
# the app's own folder name so two apps never collide on script_out.txt.
.cui_output_dir <- function() {
	if (!dir.exists("outputs")) dir.create("outputs")
	"outputs"
}

.cui_run_formatting <- function(app) {
	input <- file.path(app$dir, "script.qvs")
	if (!file.exists(input)) {
		cat("No script.qvs in", app$dir, "\n")
		return(invisible())
	}
	output <- file.path(.cui_output_dir(), paste0(basename(app$dir), "-script_out.txt"))
	cat("Formatting", input, "->", output, "\n")
	status <- system2(RSCRIPT, c(shQuote("styling/run_pipeline.R"), shQuote(input), shQuote(output)))
	if (status != 0) cat("run_pipeline.R exited with status", status, "\n") else cat("Done.\n")
}

.cui_run_report <- function(app) {
	output <- file.path(.cui_output_dir(), paste0(basename(app$dir), "-report.html"))
	cat("Rendering report for", app$title, "->", output, "\n")
	status <- system2(RSCRIPT, c(shQuote("analysis/render_report.R"), shQuote(app$dir), "--out", shQuote(output)))
	if (status != 0) cat("render_report.R exited with status", status, "\n") else cat("Done.\n")
}

# The fleet verbs (PLAN-fleet.md section 4). The UI HOLDS NO LOGIC: every item
# below is one `fleet.R <verb>` run in its own process, so the same action is
# reproducible from a command line without this menu, and testable offline.
# fleet.R self-locates the project folder, so it needs no working directory.
.cui_fleet <- function(...) {
	args <- c(shQuote(file.path("fleet", "fleet.R")), ...)
	cat("\n$ Rscript", paste(args, collapse = " "), "\n\n")
	status <- system2(RSCRIPT, args)
	# 2 is "some apps blocked", which is information, not a failure.
	if (!status %in% c(0, 2)) cat("fleet.R exited with status", status, "\n")
	invisible(status)
}

# PLAN-fleet.md section 4's selection syntax, "1,3-5,all", expanded into the
# index list `fleet.R add --apps` takes. This is the ONE piece of parsing the
# menu owns, and it is presentation: the operator typed a range at a prompt,
# and ranges are not something a CLI flag should have to understand. Returns
# "all", a comma list of indexes, or "" when the input makes no sense.
.cui_expand_sel <- function(s, n) {
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
	if (!length(out) || any(out < 1L) || (n > 0L && any(out > n))) return("")
	paste(out, collapse = ",")
}

# [3] Cloud browse: spaces -> pick one -> apps -> select -> add. Four fleet.R
# runs and no logic of its own; every step is reproducible from a command
# line, which is the rule for this whole menu.
.cui_cloud_browse <- function() {
	if (.cui_fleet("spaces") != 0) return(invisible())
	sp <- trimws(.cui_read_line("\nSpace (number from the list above, or a space id): "))
	if (!nzchar(sp)) return(invisible())
	if (.cui_fleet("apps", "--space", shQuote(sp)) != 0) return(invisible())
	sel <- .cui_read_line("\nApps to add (e.g. 1,3-5 or all, blank to cancel): ")
	if (!nzchar(trimws(sel))) return(invisible())
	# n = 0: the count lives in the listing fleet.R just printed, and the UI
	# stores no listing. An out-of-range index is refused by `add` itself.
	ex <- .cui_expand_sel(sel, 0L)
	if (!nzchar(ex)) { cat("Not a valid selection.\n"); return(invisible()) }
	if (identical(ex, "all")) .cui_fleet("add", "--space", shQuote(sp), "--all")
	else .cui_fleet("add", "--space", shQuote(sp), "--apps", shQuote(ex))
	invisible()
}

# A tenant-touching run is offered as a dry run FIRST and needs the word LIVE
# typed to go further - the same shape PLAN-fleet.md section 4 specifies for
# [8]. `fleet.R` defaults to DRY_RUN anyway; this is the second lock.
.cui_live_confirm <- function(what) {
	cat("\nThat was the dry run. Type LIVE to ", what, " for real, ",
	    "anything else to stop.\n", sep = "")
	identical(trimws(.cui_read_line("> ")), "LIVE")
}

.cui_fetch <- function() {
	cat("\n[1] apps at stage 'listed' (the usual)\n[2] every app in the ledger\n")
	pick <- trimws(.cui_read_line("> "))
	sel <- if (pick == "1") c("--stage", "listed") else if (pick == "2") "--all" else NULL
	if (is.null(sel)) { cat("Not a valid choice.\n"); return(invisible()) }
	.cui_fleet("fetch", sel)
	if (.cui_live_confirm("fetch")) .cui_fleet("fetch", sel, "--live")
	invisible()
}

.cui_tags <- function() {
	cat("\n[1] stamp - dry run (print the plan)\n[2] stamp - LIVE\n",
	    "[3] reconcile - report tag vs ledger drift\n",
	    "[4] reconcile --adopt - move the ledger to the tag where artefacts exist\n",
	    sep = "")
	pick <- trimws(.cui_read_line("> "))
	if (pick == "1") .cui_fleet("stamp", "--all")
	else if (pick == "2") {
		.cui_fleet("stamp", "--all")
		if (.cui_live_confirm("stamp every app")) .cui_fleet("stamp", "--all", "--live")
	}
	else if (pick == "3") .cui_fleet("reconcile", "--all")
	else if (pick == "4") .cui_fleet("reconcile", "--all", "--adopt")
	else cat("Not a valid choice.\n")
	invisible()
}

# The target space for an upload. fleet/spaces.csv is whatever the last
# `fleet.R spaces` run cached (index, id, name, type); with no cache the
# operator types an id. Blank means "the app's own space", which `upload`
# then reads from the ledger - and refuses for a copy if there is none.
.cui_pick_space <- function() {
	p <- file.path("fleet", "spaces.csv")
	s <- if (file.exists(p))
		tryCatch(utils::read.csv(p, colClasses = "character",
		                         stringsAsFactors = FALSE),
		         error = function(e) NULL) else NULL
	if (!is.null(s) && nrow(s) && all(c("id", "name") %in% names(s))) {
		cat("\nSpaces (from fleet/spaces.csv):\n")
		for (i in seq_len(nrow(s)))
			cat(sprintf("[%d] %-26s %s\n", i, substr(s$id[i], 1, 26),
			            substr(s$name[i], 1, 36)))
	} else {
		s <- NULL
		cat("\nNo fleet/spaces.csv yet - run [3] first, or type a space id.\n")
	}
	v <- trimws(.cui_read_line("Target space (number, id, blank = app's own): "))
	if (!nzchar(v)) return("")
	idx <- suppressWarnings(as.integer(v))
	if (!is.na(idx) && !is.null(s) && idx >= 1 && idx <= nrow(s)) return(s$id[idx])
	v
}

# [8] Upload (PLAN-fleet.md section 4): mode, target space, selection, the
# DRY-RUN command lines, then the literal word LIVE. Two locks, on purpose:
# fleet.R is dry by default and only --live turns that off, and this menu
# will not pass --live until the word is typed exactly.
.cui_upload <- function() {
	cat("\nUpload mode:\n")
	cat("[1] copy into a target space, name + \" [mig]\"  (default)\n")
	cat("[2] overwrite the source app in place\n")
	pick <- trimws(.cui_read_line("> "))
	mode <- if (pick %in% c("", "1")) "copy" else if (pick == "2") "overwrite" else ""
	if (!nzchar(mode)) { cat("Not a valid choice.\n"); return(invisible()) }
	sp <- if (mode == "copy") .cui_pick_space() else ""
	cat("\nWhich apps?\n[1] stage 'retargeted' (the usual)\n")
	cat("[2] every app in the ledger\n[3] type app ids\n")
	pick <- trimws(.cui_read_line("> "))
	sel <- if (pick == "1") c("--stage", "retargeted")
	       else if (pick == "2") "--all"
	       else if (pick == "3") {
	       	ids <- trimws(.cui_read_line("App ids, comma separated: "))
	       	if (nzchar(ids)) c("--apps", shQuote(ids)) else NULL
	       } else NULL
	if (is.null(sel)) { cat("Not a valid choice.\n"); return(invisible()) }
	args <- c("upload", "--mode", mode,
	          if (nzchar(sp)) c("--to-space", shQuote(sp)), sel)
	.cui_fleet(args)
	if (!.cui_live_confirm(paste0("upload (--mode ", mode, ")"))) return(invisible())
	.cui_fleet(c(args, "--live"))
	v <- trimws(.cui_read_line("\nVerify the uploaded apps now? [y/N] "))
	if (toupper(v) == "Y") .cui_fleet(c("verify", "--stage", "built", "--live"))
	invisible()
}

# Items that belong to a later milestone are LISTED and say so, rather than
# being hidden: the menu is the only map of this tool most operators will see,
# and a gap in the numbering is harder to read than a named "not yet".
.CUI_LATER <- c("M" = "Map upkeep                       (run map_*.R by hand)")

main <- function() {
	cat("Rtools console launcher\n")
	wd <- getwd()
	repeat {
		cat("\nWhat would you like to do?\n")
		cat("[1] Run formatting (one app)\n")
		cat("[2] Run report (one app)\n")
		cat("[3] Cloud: browse spaces / add apps\n")
		cat("[4] Fetch (unbuild from tenant)\n")
		cat("[5] Process the fleet (style + retarget)\n")
		cat("[6] Report the fleet (usage + flags)\n")
		cat("[7] Status board\n")
		cat("[8] Upload (copy or overwrite) + verify\n")
		cat("[9] Stamp / reconcile tags\n")
		cat("[M] ", .CUI_LATER[["M"]], "\n", sep = "")
		cat("[Q] Quit\n")
		choice <- .cui_read_line("> ")
		u <- toupper(trimws(choice))
		if (u == "Q") break
		if (u %in% names(.CUI_LATER)) {
			cat("Not yet: ", .CUI_LATER[[u]], "\n", sep = "")
			next
		}
		if (u == "3") { .cui_cloud_browse(); next }
		if (u == "4") { .cui_fetch(); next }
		if (u == "5") { .cui_fleet("process", "--all"); next }
		if (u == "6") { .cui_fleet("report", "--all"); next }
		if (u == "7") { .cui_fleet("status"); next }
		if (u == "8") { .cui_upload(); next }
		if (u == "9") { .cui_tags(); next }
		if (!u %in% c("1", "2")) {
			cat("Not a valid choice.\n")
			next
		}
		app <- .cui_pick_app(wd)
		if (is.null(app)) next
		if (u == "1") .cui_run_formatting(app) else .cui_run_report(app)
	}
	cat("Bye.\n")
}

if (identical(environment(), globalenv())) main()
