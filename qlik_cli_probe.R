# qlik_cli_probe.R - can R invoke qlik-cli, and can it reach a named space?
# Not a pass, not sourced by run_pipeline.R, nothing depends on it. Contract in
# INTERFACES.md; the findings it established are DESIGN §8. Ran green on the VM
# 2026-08-21 (9/9) and was promoted from spike to documented tooling then.
#
# It stays useful after answering its original question: it is the thing to run
# FIRST on any new VM, because it distinguishes "the environment is wrong" from
# "our code is wrong" before anything else is debugged.
#
# THE RETURN PATH DICTATES THE DESIGN. Adam 2026-08-21: getting files OFF the
# VM takes ~30 minutes per file, no zip, and is worse for several. The one
# cheap channel back is a PHOTOGRAPH OF THE SCREEN. So the entire output is
# built to survive being read off a photo:
#   - never wider than SCREEN_W, because a wrapped line doubles the height and
#     a wide one forces a font too small to read;
#   - one line per rung, whole run on one screen;
#   - the qlik path printed ONCE as "Q" instead of on every hint line (that
#     alone was pushing lines to 190 characters);
#   - NOTHING is ever asked for as a file.
# The first version of this script failed all four and ended by asking him to
# send the output folder - the one thing he cannot do.
#
# HOW A PARSER GETS WRITTEN FROM A PHOTO. I cannot see this output's real
# shape, and json_strings.R cannot help (it extracts string literals and
# discards structure by design - INTERFACES.md - so it cannot map a name to
# an id). What a parser actually needs is the KEY NAMES, not the data. So the
# probe prints a one-line key digest per listing. One photo of that is enough
# to write the real thing; the raw dumps stay on disk purely as a fallback.
#
# THE NARROW QUESTION. Adam has already confirmed qlik-cli runs and reaches
# the tenant by hand, so this is not "does qlik work". It is whether it still
# works with R as the caller: quoting of a path containing spaces, and whether
# the saved qlik context is visible to a child process of R. A failed rung
# prints its command as "Q <args>" - if that works pasted into a shell but
# fails here, the fault is R's invocation, and THAT is the finding.

# --- where the project lives ---------------------------------------------
# Same commandArgs() self-location as run_pipeline.R and verify.R: --file= is
# the only method that resolves under plain `Rscript file.R` (sys.frame()$ofile
# does not - verified 2026-08-21). Deliberately the same idiom, not a variant.
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
SCREEN_W    <- 72      # hard ceiling; nothing printed may exceed this
VERBOSE     <- "--verbose" %in% commandArgs(trailingOnly = TRUE)

# --- narrow output helpers ------------------------------------------------
.trunc <- function(s, n) {
	s <- gsub("[\r\n\t]+", " ", paste(s, collapse = " "))
	# ASCII "..." not a Unicode ellipsis: this is read off a photo of a Windows
	# console, whose codepage may not be UTF-8, and a mojibake character in the
	# middle of truncated output is exactly the noise that costs a re-shoot.
	if (nchar(s) > n) paste0(substr(s, 1, n - 3), "...") else s
}
.rule <- function() cat(strrep("-", SCREEN_W), "\n", sep = "")
.say  <- function(...) cat(.trunc(paste0(...), SCREEN_W), "\n", sep = "")

.results <- list()
.record <- function(rung, label, ok, detail) {
	.results[[length(.results) + 1L]] <<- list(ok = ok)
	# Fixed columns so the eye can run down the PASS/FAIL stripe in a photo
	# without reading the labels at all.
	cat(sprintf(" %d %-14s %-4s %s\n", rung, .trunc(label, 14),
	            if (ok) "PASS" else "FAIL",
	            .trunc(detail, SCREEN_W - 23)))
}

# What a parser needs from output I have never seen: the key names. Distinct,
# in first-seen order, one line. This is the whole point of the redesign - it
# replaces "send me the JSON file" with something photographable.
.keys_of <- function(lines) {
	m <- gregexpr('"([A-Za-z_][A-Za-z0-9_]*)"[[:space:]]*:', paste(lines, collapse = " "))
	hits <- regmatches(paste(lines, collapse = " "), m)[[1]]
	if (!length(hits)) return(NA_character_)
	paste(unique(sub('^"([^"]+)".*$', "\\1", hits)), collapse = ",")
}

# Whether we are even looking at JSON decides which of the two space listings
# the promoted version should use, so it is worth one word on screen.
.shape_of <- function(lines) {
	txt <- paste(lines, collapse = "")
	if (grepl("[{\\[]", txt) && grepl('"[^"]+"[[:space:]]*:', txt)) "json" else "text"
}

# --- running qlik ---------------------------------------------------------
# Invocation follows console_ui.R:72 exactly - command passed UNQUOTED,
# arguments individually shQuote()d. That pairing is already proven against a
# path containing a space ("C:\Program Files\R\..."), the same hazard as
# "D:\software-installed\qlik-cli\". Do not "fix" this by shQuote()ing the
# command too; on Windows that double-quotes it and it stops resolving.
#
# timeout guards the console-input trap: if qlik ever prompts (expired
# context, a confirmation), an un-timed system2() hangs the session with no
# way out. R kills it at TIMEOUT_S, which surfaces as status 124.
.run_qlik <- function(qlik, args, slug) {
	out <- tryCatch(
		suppressWarnings(system2(qlik, shQuote(args),
		                         stdout = TRUE, stderr = TRUE,
		                         timeout = TIMEOUT_S)),
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

# A failed subcommand captures its --help TO DISK only. It is usually long,
# so it must never reach the screen uninvited; if a guessed flag was wrong I
# ask for one targeted photo of that file rather than burning this one.
.capture_help <- function(qlik, subcommand, slug) {
	invisible(.run_qlik(qlik, c(subcommand, "--help"), slug))
}

# --- rung 1: locate qlik.exe ---------------------------------------------
# One-line config file (Adam's call). Gitignored, so each VM carries its own
# path and a pulled snapshot never overwrites it - which also means a fresh
# VM has none, hence a failure message that states exactly what to create.
if (dir.exists(OUT_DIR)) unlink(file.path(OUT_DIR, "*.txt"))
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

cat("\n"); .rule()
.say(" qlik probe                        ", format(Sys.time(), "%Y-%m-%d %H:%M"))

# CONFIG FILE ONLY. Adam has no permission to modify PATH on these VMs, so a
# PATH lookup is dead code that only invents ways to be wrong - an earlier
# version added one and built a "no setup needed" story on top of it that was
# never true here. The file is created once per VM by pasting one line into
# Notepad; it is gitignored, so pulling a fresh snapshot never clobbers it.
if (!file.exists(CONFIG_FILE)) {
	.rule()
	.say(" NO ", CONFIG_FILE, " in ", PROJECT_DIR)
	.say(" Create it (Notepad) containing one line, the path to qlik.exe:")
	.say("     D:\\software-installed\\qlik-cli\\qlik.exe")
	.rule(); quit(status = 1)
}
.cfg <- trimws(readLines(CONFIG_FILE, warn = FALSE))
.cfg <- .cfg[nzchar(.cfg) & !startsWith(.cfg, "#")]
if (!length(.cfg)) {
	.rule(); .say(" ", CONFIG_FILE, " is empty."); .rule(); quit(status = 1)
}
# Quotes stripped because Explorer's "Copy as path" wraps the path in them,
# which is the likeliest way this file gets filled in.
QLIK <- gsub('^"|"$', "", .cfg[1])

.say(" Q = ", QLIK)
.rule()

if (!file.exists(QLIK)) {
	.record(1, "locate qlik", FALSE, "no such file")
	.rule(); .say(" Fix the path in ", CONFIG_FILE, " and re-run."); .rule()
	quit(status = 1)
}
.record(1, "locate qlik", TRUE, "")

# --- rung 2: can R invoke it at all? -------------------------------------
# The core question; everything below is meaningless if it fails, so this is
# the only rung that stops the run.
r2 <- .run_qlik(QLIK, "version", "02-version")
if (r2$status != 0) {
	.record(2, "Q version", FALSE,
	        paste0("exit ", r2$status, if (r2$status == 124) " TIMEOUT" else ""))
	.explain(r2)
	.rule()
	.say(" STOP: R cannot run qlik at all.")
	.say(" Run the $ line above by hand. If it works there but not")
	.say(" here, R's invocation is the fault - that is the finding.")
	.rule(); quit(status = 1)
}
.record(2, "Q version", TRUE, .trunc(r2$out, 40))

# --- rung 3: is the saved context visible to a child of R? ---------------
# Adam authenticates with a saved context, which lives in his user profile. A
# child of R runs as the same user so it SHOULD inherit it - "should" being
# the assumption this whole exercise exists to test.
r3 <- .run_qlik(QLIK, c("context", "ls"), "03-context")
if (r3$status != 0) {
	.record(3, "Q context ls", FALSE, paste0("exit ", r3$status))
	.explain(r3); .capture_help(QLIK, "context", "03-context-help")
} else {
	.record(3, "Q context ls", TRUE, paste0(length(r3$out), " lines"))
}

# --- rungs 4-5: list spaces ----------------------------------------------
# Captured both default and --json because I have never seen which one this
# build emits; whichever is structured becomes what the promoted version uses.
# --limit is NOT optional here. qlik-cli paginates: without it the API's own
# default page size applies (20 on some endpoints), so a tenant with more
# spaces than that silently returns a partial list - and a substring test
# against a partial list reports "NOT in listing" for a space that is present
# and reachable. That is exactly what rung 6 did on Adam's first real run
# (2026-08-21). The listing was short, not the tenant.
SPACE_LS <- c("space", "ls", "--limit", "1000")
r4 <- .run_qlik(QLIK, SPACE_LS, "04-space-ls")
if (r4$status != 0) {
	.record(4, "Q space ls", FALSE, paste0("exit ", r4$status))
	.explain(r4); .capture_help(QLIK, "space", "04-space-help")
} else {
	.record(4, "Q space ls", TRUE,
	        paste0(length(r4$out), " lines, ", .shape_of(r4$out)))
}
r4j <- .run_qlik(QLIK, c(SPACE_LS, "--json"), "05-space-json")
if (r4j$status != 0) {
	.record(5, "  + --json", FALSE, paste0("exit ", r4j$status, ", flag may not exist"))
} else {
	.record(5, "  + --json", TRUE,
	        paste0(length(r4j$out), " lines, ", .shape_of(r4j$out)))
}

# --- rung 6: is the target space there? ----------------------------------
# A substring test, deliberately: proving the space is REACHABLE needs no
# parser, and a parser written against unseen output is the mistake this
# whole design avoids.
.haystack <- c(r4$out, r4j$out)
.found <- any(grepl(SPACE_NAME, .haystack, fixed = TRUE))
.record(6, SPACE_NAME, .found,
        if (.found) "found" else paste0("not in ", length(.haystack), " lines"))

# When it is NOT found, the names that WERE returned are the whole diagnosis,
# and they separate the only two explanations without another round-trip:
# a SHORT list means pagination truncated it (see --limit above), while a full
# list containing something like "On-Prem Apps" means the name in SPACE_NAME
# is simply wrong. Printing the count alongside is what makes those two
# distinguishable. Capped at two lines so the screen still fits a photo.
if (!.found) {
	.nm <- regmatches(paste(.haystack, collapse = " "),
	                  gregexpr('"name"[[:space:]]*:[[:space:]]*"([^"]+)"',
	                           paste(.haystack, collapse = " ")))[[1]]
	.nm <- unique(sub('.*"([^"]+)"$', "\\1", .nm))
	if (length(.nm)) {
		.say("   ", length(.nm), " space name(s) returned:")
		.say("   ", .trunc(paste(.nm, collapse = ", "), SCREEN_W - 6))
	} else {
		.say("   no \"name\" keys found - listing may not be JSON")
	}
}

# --- rungs 7-9: the space id, verified, then the apps in it --------------
# `space filter --names <name> --quiet` is what qlik.dev documents, and it is
# NOT AVAILABLE on this tenant: qlik-cli 3.2.0 answers
#   Error: unknown flag: --names
# (measured on the VM 2026-08-21). The published reference describes some
# other version. The call is gone rather than kept as a disabled branch -
# it cost a subprocess and a confusing screen line to tell us nothing.
#
# So the id comes from the positional heuristic below, which is SAFE ONLY
# BECAUSE rung 8 verifies it against the tenant. Heuristic proposes, `space
# get` disposes: if the guess were ever wrong, rung 8 fails loudly instead of
# quietly listing the apps of some other space.
SPACE_ID <- NA
.id_how  <- ""

# An id must LOOK like one before it is believed. .run_qlik merges stderr into
# stdout (stderr = TRUE), so a command that succeeds while printing a warning
# hands back that warning text as its first line - and without this check that
# text became the space id and was passed straight to `app ls --spaceId`.
# Caught 2026-08-21 when a stub returned status 0 with an error on stderr and
# rung 7 cheerfully reported "not a command" as the id. Wrong AND confident is
# the one outcome worth spending code to prevent.
.looks_like_id <- function(s) grepl("^[A-Za-z0-9_-]{8,}$", s)

if (.found) {
	flat <- paste(.haystack, collapse = "\n")
	at <- regexpr(SPACE_NAME, flat, fixed = TRUE)
	if (at > 0) {
		before <- substr(flat, 1, at - 1)
		ids <- gregexpr('"id"[[:space:]]*:[[:space:]]*"([^"]+)"', before)[[1]]
		if (ids[1] > 0) {
			last <- ids[length(ids)]
			frag <- substr(before, last, last + attr(ids, "match.length")[length(ids)] - 1)
			.hit <- sub('.*"([^"]+)"$', "\\1", frag)
			# Same believability test as the filter path above - the heuristic
			# has more ways to go wrong, not fewer.
			if (.looks_like_id(.hit)) { SPACE_ID <- .hit; .id_how <- " (unverified)" }
		}
	}
}
apps <- NULL
if (is.na(SPACE_ID)) {
	.record(7, "space id", FALSE, "no id found near that name")
} else {
	.record(7, "space id", TRUE, paste0(SPACE_ID, .id_how))

	# --- rung 8: is that id actually the right space? --------------------
	# The heuristic takes the id nearest-PRECEDING the name, which is correct
	# only if id comes before name inside each object. On this tenant it does
	# (keys: id,type,ownerId,tenantId,name,...), so the guess is probably
	# right - but "probably" is not a foundation to build a migration on.
	# `space get <id>` (documented, qlik.dev) closes the loop: ask the tenant
	# what that id IS, and check the answer says "On Prem Apps".
	rv <- .run_qlik(QLIK, c("space", "get", SPACE_ID), "07-space-get")
	.vok <- rv$status == 0 && any(grepl(SPACE_NAME, rv$out, fixed = TRUE))
	.record(8, "verify id", .vok,
	        if (.vok) "space get returns that name"
	        else paste0("exit ", rv$status, ", name NOT in reply"))
	if (!.vok) .explain(rv)

	# --- rung 9: the apps in it ------------------------------------------
	# --limit for the same pagination reason as the space listing. The plain
	# listing is printed verbatim because it came back as 3 lines of TEXT, and
	# "3 lines, text" told us nothing about whether those were apps, a warning,
	# or an empty result. Small enough to just show.
	r9 <- .run_qlik(QLIK, c("app", "ls", "--spaceId", SPACE_ID,
	                        "--limit", "1000"), "08-apps")
	if (r9$status != 0) {
		.record(9, "Q app ls", FALSE, paste0("exit ", r9$status))
		.explain(r9); .capture_help(QLIK, "app", "08-app-help")
	} else {
		.record(9, "Q app ls", TRUE,
		        paste0(length(r9$out), " lines, ", .shape_of(r9$out)))
		for (ln in utils::head(r9$out, 5L)) .say("     | ", ln)
		if (length(r9$out) > 5L) .say("     | ... ", length(r9$out) - 5L, " more")
		# Separate --json call purely for the key digest; not printed.
		apps <- .run_qlik(QLIK, c("app", "ls", "--spaceId", SPACE_ID,
		                          "--limit", "1000", "--json"), "08-apps-json")$out
	}
}

# --- summary --------------------------------------------------------------
.rule()
.pass <- sum(vapply(.results, function(r) isTRUE(r$ok), logical(1)))
.say(sprintf(" %d/%d PASS", .pass, length(.results)))
# The two lines that make a photo sufficient. Without these I would be asking
# for a JSON file, which costs half an hour and often is not possible at all.
.say(" space keys: ", .trunc(.keys_of(.haystack), SCREEN_W - 14))
if (!is.null(apps)) .say(" app keys:   ", .trunc(.keys_of(apps), SCREEN_W - 14))
.say(" >> PHOTOGRAPH THIS SCREEN - it is all I need.")
.rule()
