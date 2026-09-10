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

# Items that belong to a later milestone are LISTED and say so, rather than
# being hidden: the menu is the only map of this tool most operators will see,
# and a gap in the numbering is harder to read than a named "not yet".
.CUI_LATER <- c("3" = "Cloud: browse spaces / add apps (M2)",
                "4" = "Fetch (unbuild from tenant)      (M2)",
                "8" = "Upload (copy or overwrite)       (M3)",
                "9" = "Stamp / reconcile tags           (M4)",
                "M" = "Map upkeep                       (run map_*.R by hand)")

main <- function() {
	cat("Rtools console launcher\n")
	wd <- getwd()
	repeat {
		cat("\nWhat would you like to do?\n")
		cat("[1] Run formatting (one app)\n")
		cat("[2] Run report (one app)\n")
		cat("[3] ", .CUI_LATER[["3"]], "\n", sep = "")
		cat("[4] ", .CUI_LATER[["4"]], "\n", sep = "")
		cat("[5] Process the fleet (style + retarget)\n")
		cat("[6] Report the fleet (usage + flags)\n")
		cat("[7] Status board\n")
		cat("[8] ", .CUI_LATER[["8"]], "\n", sep = "")
		cat("[9] ", .CUI_LATER[["9"]], "\n", sep = "")
		cat("[M] ", .CUI_LATER[["M"]], "\n", sep = "")
		cat("[Q] Quit\n")
		choice <- .cui_read_line("> ")
		u <- toupper(trimws(choice))
		if (u == "Q") break
		if (u %in% names(.CUI_LATER)) {
			cat("Not yet: ", .CUI_LATER[[u]], "\n", sep = "")
			next
		}
		if (u == "5") { .cui_fleet("process", "--all"); next }
		if (u == "6") { .cui_fleet("report", "--all"); next }
		if (u == "7") { .cui_fleet("status"); next }
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
