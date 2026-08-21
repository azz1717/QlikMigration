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

main <- function() {
	cat("Rtools console launcher\n")
	wd <- getwd()
	repeat {
		cat("\nWhat would you like to do?\n[1] Run formatting\n[2] Run report\n[Q] Quit\n")
		choice <- .cui_read_line("> ")
		u <- toupper(trimws(choice))
		if (u == "Q") break
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
