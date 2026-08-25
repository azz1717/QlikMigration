# qlik_cli_unbuild.R - unbuild every app in one space, resumably.
# Not a pass, not sourced by run_pipeline.R. Contract in INTERFACES.md.
# Sibling of qlik_cli_probe.R and built on its VERIFIED findings (DESIGN
# S8): same config file, same invocation pairing, --limit on every ls,
# positional id heuristic gated by `space get`. Run the probe FIRST on any
# new VM; this script assumes the probe's nine rungs are green.
#
# WHY IT EXISTS (Adam 2026-08-25): many on-prem QVDs come from dedicated
# QVD-generator apps, now gathered into one space. Their load scripts name
# the ORIGINAL field/table sources, so unbuilding all of them gives the
# field-mapping work real before/after ground truth. Adam has run
#   qlik app unbuild --app "<id>" --dir ./<dir>
# by hand on the VM successfully; this only adds the iteration.
#
# Output lands under unbuilt/<sanitized app name>/, one dir per app, plus
# unbuilt/_manifest.txt (dir, id, name, status - tab-separated). A dir that
# already exists is SKIPPED, so a rerun after a timeout only does the
# missing apps; delete a dir to force its refresh. A FAILED unbuild's dir
# is deleted so the rerun retries it instead of skipping a partial.
#
# Flags: --dry-run  list what would be done, run nothing
#        --space=X  target a space other than the default below
#        --verbose  full failure output instead of 2 lines
#
# The photo rules apply (DESIGN S8.5): 72 cols, ASCII, digest over raw
# data. The per-app lines scroll during the run; the SUMMARY block at the
# end is the part to photograph.

# --- where the project lives ---------------------------------------------
# Same commandArgs() self-location as the probe; see its header for why the
# extra dirname() (script lives in diagnostics/, config lives at root).
.file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
PROJECT_DIR <- if (length(.file_arg)) {
	tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .file_arg[1])))),
	         error = function(e) NA)
} else NA
if (is.na(PROJECT_DIR) || !dir.exists(PROJECT_DIR)) {
	stop("could not determine the project folder. Run this as ",
	     "'Rscript qlik_cli_unbuild.R'.", call. = FALSE)
}
setwd(PROJECT_DIR)

CONFIG_FILE <- "qlik_cli_path.txt"
OUT_DIR     <- "qlik_unbuild_out"   # command logs, one file per call
DEST_DIR    <- "unbuilt"            # one subdir per app
SPACE_NAME  <- "On Prem Apps"
TIMEOUT_S   <- 60                   # listings
UNBUILD_TIMEOUT_S <- 600            # one whole app definition; be generous
SCREEN_W    <- 72
.argv    <- commandArgs(trailingOnly = TRUE)
VERBOSE  <- "--verbose" %in% .argv
DRY_RUN  <- "--dry-run" %in% .argv
.sp <- grep("^--space=", .argv, value = TRUE)
if (length(.sp)) SPACE_NAME <- sub("^--space=", "", .sp[1])

# --- shared helpers (qlik_cli_shared.R, extracted from the probe) ---------
# .trunc/.rule/.say, .keys_of (the key-name digest that lets one photo fix
# the parser), .run_qlik, .explain, .looks_like_id.
source("diagnostics/qlik_cli_shared.R")

.die <- function(...) { .say(" STOP: ", ...); .rule(); quit(status = 1) }

# --- locate qlik (probe rungs 1-2, condensed: any failure here stops) -----
if (dir.exists(OUT_DIR)) unlink(file.path(OUT_DIR, "*.txt"))
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

cat("\n"); .rule()
.say(" qlik unbuild", if (DRY_RUN) " (DRY RUN)" else "",
     "                  ", format(Sys.time(), "%Y-%m-%d %H:%M"))
if (!file.exists(CONFIG_FILE)) {
	.rule()
	.say(" NO ", CONFIG_FILE, " in ", PROJECT_DIR)
	.say(" Create it (Notepad) containing one line, the path to qlik.exe.")
	.rule(); quit(status = 1)
}
.cfg <- trimws(readLines(CONFIG_FILE, warn = FALSE))
.cfg <- .cfg[nzchar(.cfg) & !startsWith(.cfg, "#")]
if (!length(.cfg)) { .rule(); .say(" ", CONFIG_FILE, " is empty."); .rule(); quit(status = 1) }
QLIK <- gsub('^"|"$', "", .cfg[1])
.say(" Q = ", QLIK)
.rule()
if (!file.exists(QLIK)) .die("no such file: ", QLIK, " - fix ", CONFIG_FILE)
rv <- .run_qlik(QLIK, "version", "01-version")
if (rv$status != 0) { .explain(rv); .die("R cannot run qlik (run the $ line by hand)") }

# --- space id: heuristic proposes, `space get` disposes (DESIGN S8.4) -----
# Unlike the probe, an unverified id is FATAL here: this script writes real
# output, and unbuilding some other space's apps quietly is the one outcome
# worth stopping for.
SPACE_LS <- c("space", "ls", "--limit", "1000")
r1 <- .run_qlik(QLIK, SPACE_LS, "02-space-ls")
r2 <- .run_qlik(QLIK, c(SPACE_LS, "--json"), "03-space-json")
.haystack <- c(if (r1$status == 0) r1$out, if (r2$status == 0) r2$out)
if (!length(.haystack)) { .explain(r1); .die("space ls failed") }
flat <- paste(.haystack, collapse = "\n")
at <- regexpr(SPACE_NAME, flat, fixed = TRUE)
if (at < 0) .die("space '", SPACE_NAME, "' not in listing (", length(.haystack), " lines)")
SPACE_ID <- NA
before <- substr(flat, 1, at - 1)
ids <- gregexpr('"id"[[:space:]]*:[[:space:]]*"([^"]+)"', before)[[1]]
if (ids[1] > 0) {
	last <- ids[length(ids)]
	frag <- substr(before, last, last + attr(ids, "match.length")[length(ids)] - 1)
	.hit <- sub('.*"([^"]+)"$', "\\1", frag)
	if (.looks_like_id(.hit)) SPACE_ID <- .hit
}
if (is.na(SPACE_ID)) .die("no id found near the space name")
rg <- .run_qlik(QLIK, c("space", "get", SPACE_ID), "04-space-get")
if (rg$status != 0 || !any(grepl(SPACE_NAME, rg$out, fixed = TRUE))) {
	.explain(rg); .die("space get did not confirm '", SPACE_NAME, "'")
}
.say(" space: ", SPACE_NAME, "  id: ", SPACE_ID)

# --- the apps: id/name pairs from the --json listing ----------------------
# The app listing's JSON key order was never recorded (the probe printed it
# on a photo only), so the extraction is defensive: exact-key matches for
# "id" and "name" ("ownerId" cannot false-match "id" - the opening quote is
# part of the pattern), then a STRICT ALTERNATION check (id, name, id,
# name, ...) which is only satisfied when each object carries exactly one
# of each in that order. Anything else - nested owner objects with their
# own "name", name before id - fails the check and stops the run with the
# key digest, rather than pairing the wrong name to an id.
ra <- .run_qlik(QLIK, c("app", "ls", "--spaceId", SPACE_ID,
                        "--limit", "1000", "--json"), "05-apps-json")
if (ra$status != 0) { .explain(ra); .die("app ls failed") }
ajson <- paste(ra$out, collapse = "\n")
.pairs_of <- function(txt) {
	gid <- gregexpr('"id"[[:space:]]*:[[:space:]]*"([^"]+)"', txt)[[1]]
	gnm <- gregexpr('"name"[[:space:]]*:[[:space:]]*"([^"]+)"', txt)[[1]]
	if (gid[1] < 0 || gnm[1] < 0 || length(gid) != length(gnm)) return(NULL)
	if (any(gid >= gnm)) return(NULL)                 # every id before its name
	if (length(gid) > 1 && any(gnm[-length(gnm)] >= gid[-1])) return(NULL)
	pull <- function(pos, len) {
		sub('.*"([^"]+)"$', "\\1", substr(rep(txt, length(pos)), pos, pos + len - 1))
	}
	list(id   = pull(gid, attr(gid, "match.length")),
	     name = pull(gnm, attr(gnm, "match.length")))
}
P <- .pairs_of(ajson)
if (is.null(P) || !length(P$id) || !all(.looks_like_id(P$id))) {
	.say(" app keys: ", .trunc(.keys_of(ra$out), SCREEN_W - 12))
	.die("could not pair app ids to names - photograph the line above")
}
.say(" apps in space: ", length(P$id))
.rule()

# --- destination dirs: sanitized name, deterministic ----------------------
# Windows-safe: keep [A-Za-z0-9 ._-], replace the rest, trim trailing dots/
# spaces. Duplicate NAMES get a -<first 8 of id> suffix - deterministic, so
# the skip-if-exists rerun logic keys on the same dir every run regardless
# of listing order.
.san <- gsub("[^A-Za-z0-9 ._-]", "_", P$name)
.san <- sub("[. ]+$", "", trimws(.san))
.san[!nzchar(.san)] <- P$id[!nzchar(.san)]
dup <- .san %in% .san[duplicated(.san)]
.san[dup] <- paste0(.san[dup], "-", substr(P$id[dup], 1, 8))
DEST <- file.path(DEST_DIR, .san)

if (!dir.exists(DEST_DIR)) dir.create(DEST_DIR)

# --- the loop -------------------------------------------------------------
status <- character(length(P$id))
for (i in seq_along(P$id)) {
	if (dir.exists(DEST[i])) {
		status[i] <- "SKIP"
	} else if (DRY_RUN) {
		status[i] <- "TODO"
	} else {
		ru <- .run_qlik(QLIK, c("app", "unbuild", "--app", P$id[i],
		                        "--dir", DEST[i]),
		                sprintf("app-%03d", i), timeout = UNBUILD_TIMEOUT_S)
		ok <- ru$status == 0 && dir.exists(DEST[i]) &&
		      length(list.files(DEST[i], recursive = TRUE)) > 0
		if (!ok && dir.exists(DEST[i])) {
			unlink(DEST[i], recursive = TRUE)   # partial dir; rerun must retry
		}
		status[i] <- if (ok) "OK" else
			if (ru$status == 124) "FAIL-TIMEOUT" else "FAIL"
		if (!ok) .explain(ru)
	}
	cat(sprintf(" %3d/%d %-12s %s\n", i, length(P$id),
	            status[i], .trunc(P$name[i], SCREEN_W - 22)))
}

# Manifest rewritten whole each run: dir, id, name, this run's status.
writeLines(c("# dir\tapp id\tname\tstatus (this run)",
             paste(.san, P$id, P$name, status, sep = "\t")),
           file.path(DEST_DIR, "_manifest.txt"))

# --- summary: the photographable part -------------------------------------
.rule()
.say(sprintf(" %d apps: %d ok, %d skipped, %d failed%s",
             length(status), sum(status == "OK"), sum(status == "SKIP"),
             sum(startsWith(status, "FAIL")),
             if (DRY_RUN) sprintf(", %d to do (dry run)", sum(status == "TODO")) else ""))
for (i in which(startsWith(status, "FAIL"))) {
	.say("   FAIL ", .trunc(P$name[i], 30), "  ", P$id[i])
}
.say(" manifest: ", file.path(DEST_DIR, "_manifest.txt"))
.say(" rerun skips existing dirs; delete a dir to redo that app")
.say(" >> PHOTOGRAPH FROM THE LINE ABOVE THE COUNTS.")
.rule()
if (any(startsWith(status, "FAIL"))) quit(status = 1)
