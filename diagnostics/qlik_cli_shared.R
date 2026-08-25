# qlik_cli_shared.R - helpers shared by qlik_cli_probe.R and
# qlik_cli_unbuild.R. source()d after setwd(PROJECT_DIR); not a pass,
# nothing else may source it. Extracted 2026-08-25 when the unbuild script
# grew twins of the probe's helpers and verify_docs caught it.
#
# The functions read SCREEN_W, TIMEOUT_S, OUT_DIR and VERBOSE from the
# sourcing script's globals at CALL time, so each script keeps its own
# constants. The photo-output rationale behind them is the probe's header
# and DESIGN S8.5; the comments here keep only the per-function traps.

.trunc <- function(s, n) {
	s <- gsub("[\r\n\t]+", " ", paste(s, collapse = " "))
	# ASCII "..." not a Unicode ellipsis: this is read off a photo of a Windows
	# console, whose codepage may not be UTF-8, and a mojibake character in the
	# middle of truncated output is exactly the noise that costs a re-shoot.
	if (nchar(s) > n) paste0(substr(s, 1, n - 3), "...") else s
}
.rule <- function() cat(strrep("-", SCREEN_W), "\n", sep = "")
.say  <- function(...) cat(.trunc(paste0(...), SCREEN_W), "\n", sep = "")

# What a parser needs from output I have never seen: the key names. Distinct,
# in first-seen order, one line - it replaces "send me the JSON file" with
# something photographable.
.keys_of <- function(lines) {
	m <- gregexpr('"([A-Za-z_][A-Za-z0-9_]*)"[[:space:]]*:', paste(lines, collapse = " "))
	hits <- regmatches(paste(lines, collapse = " "), m)[[1]]
	if (!length(hits)) return(NA_character_)
	paste(unique(sub('^"([^"]+)".*$', "\\1", hits)), collapse = ",")
}

# Invocation follows console_ui.R:72 exactly - command passed UNQUOTED,
# arguments individually shQuote()d. That pairing is proven against a path
# containing a space. Do not "fix" this by shQuote()ing the command too; on
# Windows that double-quotes it and it stops resolving.
#
# timeout guards the console-input trap: if qlik ever prompts (expired
# context, a confirmation), an un-timed system2() hangs the session with no
# way out. R kills it at the limit, which surfaces as status 124. The
# default is the sourcing script's TIMEOUT_S; slow calls (unbuild) pass a
# larger one.
.run_qlik <- function(qlik, args, slug, timeout = TIMEOUT_S) {
	out <- tryCatch(
		suppressWarnings(system2(qlik, shQuote(args),
		                         stdout = TRUE, stderr = TRUE,
		                         timeout = timeout)),
		error = function(e) structure(paste("could not start process:",
		                                    conditionMessage(e)),
		                              status = -1L))
	status <- attr(out, "status")
	if (is.null(status)) status <- 0L
	if (!is.null(slug)) {
		writeLines(c(paste("$ Q", paste(args, collapse = " ")),
		             paste("# exit:", status), "", out),
		           file.path(OUT_DIR, paste0(slug, ".txt")))
	}
	list(out = out, status = status, args = paste(args, collapse = " "))
}

# On failure the command and a couple of lines of its output are the whole
# diagnosis, so they print - but capped, or several failures overflow the
# screen and cost the photo.
.explain <- function(r) {
	.say("     $ Q ", r$args)
	for (ln in utils::head(r$out, if (VERBOSE) 20L else 2L)) .say("     > ", ln)
}

# An id must LOOK like one before it is believed. .run_qlik merges stderr
# into stdout, so a command that succeeds while printing a warning hands
# back that warning text as its first line - without this check that text
# once became a space id (probe, 2026-08-21). Wrong AND confident is the
# one outcome worth spending code to prevent.
.looks_like_id <- function(s) grepl("^[A-Za-z0-9_-]{8,}$", s)
