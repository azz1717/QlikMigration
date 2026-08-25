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

# --- the apps: text listing is PRIMARY, JSON parse is the fallback --------
# Adam's proven manual unbuild used ids straight off the plain (non-JSON)
# `app ls` table, so that table is the primary source: first whitespace
# token on a line, if it looks like an id, is the app id and the rest of
# the line (trimmed) is the name. Lines that do not start with something
# id-shaped (header, blanks, warnings) are dropped. If that yields zero
# rows, fall back to parsing the --json listing (items-API shape: "name"
# first, real app id under one of resourceId / resourceAttributes.id /
# id - which key holds it is unverified per tenant, so all three are kept
# as candidates). Either way, the FIRST app actually unbuilt this run
# calibrates: if the primary id fails, the JSON candidates for that same
# app (matched by id or by exact name) are tried in priority order, and
# whichever key works is adopted for every remaining app.
.parse_id_name_lines <- function(lines) {
	id <- character(0); nm <- character(0)
	for (ln in lines) {
		ln <- sub("^[ \t]+", "", ln)
		if (!nzchar(ln)) next
		sp <- regexpr("[ \t]", ln)
		if (sp < 0) next
		tok  <- substr(ln, 1, sp - 1)
		rest <- trimws(substr(ln, sp + 1, nchar(ln)))
		if (.looks_like_id(tok) && nzchar(rest)) { id <- c(id, tok); nm <- c(nm, rest) }
	}
	list(id = id, name = nm)
}
rt <- .run_qlik(QLIK, c("app", "ls", "--spaceId", SPACE_ID, "--limit", "1000"),
                 "05-apps-text")
TXT <- if (rt$status == 0) .parse_id_name_lines(rt$out) else list(id = character(0), name = character(0))

# Minimal recursive-descent JSON parser, base R only (fallback source, and
# the calibration safety net's candidate source). Objects -> named list,
# arrays -> unnamed list, JSON null -> NA (not NULL: out[[key]] <- NULL
# would silently drop the key from an R list). Text split to a character
# vector ONCE and walked by index - no substr-in-a-loop - so a ~1 MB,
# 1000-item listing parses in seconds.
.json_parse <- function(txt) {
	chars <- strsplit(txt, "")[[1]]
	n <- length(chars)
	i <- 1L
	perr <- function(msg) .die("JSON parse error at char ", i, ": ", msg)
	skip_ws <- function() while (i <= n && chars[i] %in% c(" ", "\t", "\n", "\r")) i <<- i + 1L
	expect_lit <- function(lit) {
		l <- nchar(lit)
		if (i + l - 1 > n || paste(chars[i:(i + l - 1)], collapse = "") != lit)
			perr(paste0("expected '", lit, "'"))
		i <<- i + l
	}
	parse_string <- function() {
		i <<- i + 1L
		out <- character(0)
		repeat {
			if (i > n) perr("unterminated string")
			ch <- chars[i]
			if (ch == '"') { i <<- i + 1L; break }
			if (ch == "\\") {
				i <<- i + 1L
				esc <- chars[i]
				out <- c(out, switch(esc,
					'"' = '"', "\\" = "\\", "/" = "/", "b" = "\b",
					"f" = "\f", "n" = "\n", "r" = "\r", "t" = "\t",
					"u" = { hex <- paste(chars[(i + 1):(i + 4)], collapse = "")
					        i <<- i + 4L
					        intToUtf8(strtoi(hex, 16L)) },
					perr(paste0("bad escape \\", esc))))
				i <<- i + 1L
			} else { out <- c(out, ch); i <<- i + 1L }
		}
		paste(out, collapse = "")
	}
	parse_number <- function() {
		start <- i
		while (i <= n && grepl("[-+0-9.eE]", chars[i])) i <<- i + 1L
		if (i == start) perr("bad number")
		as.numeric(paste(chars[start:(i - 1)], collapse = ""))
	}
	parse_value <- function() {
		skip_ws()
		if (i > n) perr("unexpected end")
		ch <- chars[i]
		if (ch == "{") parse_object()
		else if (ch == "[") parse_array()
		else if (ch == '"') parse_string()
		else if (ch == "t") { expect_lit("true"); TRUE }
		else if (ch == "f") { expect_lit("false"); FALSE }
		else if (ch == "n") { expect_lit("null"); NA }
		else parse_number()
	}
	parse_array <- function() {
		i <<- i + 1L; skip_ws()
		out <- list()
		if (i <= n && chars[i] == "]") { i <<- i + 1L; return(out) }
		repeat {
			out[[length(out) + 1L]] <- parse_value()
			skip_ws()
			if (i > n) perr("unterminated array")
			if (chars[i] == ",") { i <<- i + 1L; skip_ws(); next }
			if (chars[i] == "]") { i <<- i + 1L; break }
			perr("expected , or ]")
		}
		out
	}
	parse_object <- function() {
		i <<- i + 1L; skip_ws()
		out <- list()
		if (i <= n && chars[i] == "}") { i <<- i + 1L; return(out) }
		repeat {
			skip_ws()
			if (i > n || chars[i] != '"') perr("expected key string")
			key <- parse_string()
			skip_ws()
			if (i > n || chars[i] != ":") perr("expected :")
			i <<- i + 1L
			out[[key]] <- parse_value()
			skip_ws()
			if (i > n) perr("unterminated object")
			if (chars[i] == ",") { i <<- i + 1L; skip_ws(); next }
			if (chars[i] == "}") { i <<- i + 1L; break }
			perr("expected , or }")
		}
		out
	}
	skip_ws()
	parse_value()
}

.is_item_list <- function(x) {
	is.list(x) && length(x) > 0 && is.null(names(x)) &&
		all(vapply(x, function(e) is.list(e) && !is.null(names(e)), logical(1)))
}
CAND_KEYS <- c("resourceId", "resourceAttributes$id", "id")
.get_field <- function(item, key) {
	if (key == "resourceAttributes$id") {
		ra <- item[["resourceAttributes"]]
		if (is.list(ra)) ra[["id"]] else NULL
	} else item[[key]]
}
.cand_vals <- function(it) {
	vals <- setNames(rep(NA_character_, length(CAND_KEYS)), CAND_KEYS)
	for (k in CAND_KEYS) {
		v <- .get_field(it, k)
		if (is.character(v) && length(v) == 1 && .looks_like_id(v)) vals[k] <- v
	}
	vals
}

JSON_ITEMS <- NULL   # lazy: fetched only if the text listing is unusable, or
                      # as the calibration app's fallback candidate source.
.fetch_json_items <- function() {
	if (!is.null(JSON_ITEMS)) return(JSON_ITEMS)
	ra <- .run_qlik(QLIK, c("app", "ls", "--spaceId", SPACE_ID,
	                        "--limit", "1000", "--json"), "05b-apps-json")
	if (ra$status != 0) { .explain(ra); .die("app ls --json failed") }
	ajson <- paste(ra$out, collapse = "\n")
	parsed <- .json_parse(ajson)
	items <- if (.is_item_list(parsed)) parsed else {
		hit <- NULL
		if (is.list(parsed)) for (v in parsed) if (.is_item_list(v)) { hit <- v; break }
		if (is.null(hit)) .die("could not find an item array in the app listing")
		hit
	}
	JSON_ITEMS <<- items
	items
}
# Candidates for the item matching a text row: by id-in-any-candidate first
# (unambiguous by construction). Only if that finds nothing do we try name -
# and only when the name is carried by EXACTLY ONE item; zero or several
# matches returns all-NA (a plain FAIL with the app's own text id) rather
# than guess. No ordinal/listing-order matching: text and JSON listing
# order parity with each other is unverified.
.json_cands_for <- function(name_txt, id_txt) {
	items <- .fetch_json_items()
	empty <- setNames(rep(NA_character_, length(CAND_KEYS)), CAND_KEYS)
	if (!is.na(id_txt)) {
		for (it in items) {
			vals <- .cand_vals(it)
			if (any(vals == id_txt, na.rm = TRUE)) return(vals)
		}
	}
	hits <- Filter(function(it) {
		nm <- it[["name"]]
		is.character(nm) && length(nm) == 1 && identical(nm, name_txt)
	}, items)
	if (length(hits) == 1) return(.cand_vals(hits[[1]]))
	empty
}

if (length(TXT$id) >= 1) {
	P_NAME <- TXT$name
	P_TEXTID <- TXT$id
} else {
	items <- .fetch_json_items()
	n_items <- length(items)
	item_name <- character(n_items)
	item_cand <- vector("list", n_items)
	for (idx in seq_len(n_items)) {
		it <- items[[idx]]
		nm <- it[["name"]]
		item_name[idx] <- if (is.character(nm) && length(nm) == 1 && nzchar(nm)) nm else NA_character_
		item_cand[[idx]] <- .cand_vals(it)
	}
	mapped <- !is.na(item_name) & vapply(item_cand, function(v) any(!is.na(v)), logical(1))
	n_unmapped <- sum(!mapped)
	if (!any(mapped)) {
		allkeys <- sort(unique(unlist(lapply(items, names))))
		.say(" all ", n_items, " items unmapped; top-level keys:")
		line <- ""
		for (k in allkeys) {
			add <- if (nzchar(line)) paste0(line, ",", k) else k
			if (nchar(add) + 1 > SCREEN_W) { .say(" ", line); line <- k } else line <- add
		}
		if (nzchar(line)) .say(" ", line)
		.die("no id candidates found in app listing")
	}
	P_NAME <- item_name[mapped]
	JSON_CAND <- item_cand[mapped]     # used directly; no text id to try first
	if (n_unmapped > 0) .say(" ", n_unmapped, " item(s) unmapped (no name or no id) - skipped")
	P_TEXTID <- rep(NA_character_, length(P_NAME))   # no text source in this branch
}
.say(" apps in space: ", length(P_NAME))
.rule()

# --- destination dirs: sanitized name, deterministic ----------------------
# Windows-safe: keep [A-Za-z0-9 ._-], replace the rest, trim trailing dots/
# spaces. Duplicate NAMES get a -<first 8 of id> suffix - deterministic, so
# the skip-if-exists rerun logic keys on the same dir every run regardless
# of listing order. The id used only for that suffix is whatever's first
# available (text id, else first JSON candidate); calibration below is
# what actually decides which id gets PASSED to `unbuild`.
.rep_id <- if (length(TXT$id) >= 1) P_TEXTID else
	vapply(JSON_CAND, function(v) { nz <- v[!is.na(v)]; if (length(nz)) nz[[1]] else NA_character_ }, character(1))
.san <- gsub("[^A-Za-z0-9 ._-]", "_", P_NAME)
.san <- sub("[. ]+$", "", trimws(.san))
.san[!nzchar(.san)] <- .rep_id[!nzchar(.san)]
dup <- .san %in% .san[duplicated(.san)]
.san[dup] <- paste0(.san[dup], "-", substr(.rep_id[dup], 1, 8))
DEST <- file.path(DEST_DIR, .san)

if (!dir.exists(DEST_DIR)) dir.create(DEST_DIR)

# --- the loop: self-calibrating id key --------------------------------------
# CAL_KEY is "text" (the primary listing's id worked) or one of CAND_KEYS
# (a JSON fallback candidate worked). Once set, no more per-app retries -
# a later failure with the adopted key is a plain FAIL.
CAL_KEY <- NA_character_
status <- character(length(P_NAME))
.attempt <- function(idv, dest, slug) {
	ru <- .run_qlik(QLIK, c("app", "unbuild", "--app", idv, "--dir", dest),
	                slug, timeout = UNBUILD_TIMEOUT_S)
	ok <- ru$status == 0 && dir.exists(dest) && length(list.files(dest, recursive = TRUE)) > 0
	if (!ok && dir.exists(dest)) unlink(dest, recursive = TRUE)
	list(ok = ok, r = ru)
}
for (i in seq_along(P_NAME)) {
	if (dir.exists(DEST[i])) {
		status[i] <- "SKIP"
	} else if (DRY_RUN) {
		status[i] <- "TODO"
	} else if (is.na(CAL_KEY)) {
		ok <- FALSE; last <- NULL
		if (!is.na(P_TEXTID[i])) {
			res <- .attempt(P_TEXTID[i], DEST[i], sprintf("app-%03d", i))
			last <- res$r
			if (res$ok) { ok <- TRUE; CAL_KEY <- "text" }
		}
		if (!ok) {
			jc <- if (length(TXT$id) >= 1) .json_cands_for(P_NAME[i], P_TEXTID[i]) else JSON_CAND[[i]]
			for (k in CAND_KEYS) {
				idv <- jc[[k]]
				if (is.na(idv)) next
				res <- .attempt(idv, DEST[i], sprintf("app-%03d", i))
				last <- res$r
				if (res$ok) { ok <- TRUE; CAL_KEY <- k; break }
			}
		}
		if (!ok) {
			if (!is.null(last)) .explain(last)
			.die("no id candidate worked on the calibration app")
		}
		status[i] <- "OK"
	} else if (CAL_KEY == "text") {
		res <- .attempt(P_TEXTID[i], DEST[i], sprintf("app-%03d", i))
		status[i] <- if (res$ok) "OK" else if (res$r$status == 124) "FAIL-TIMEOUT" else "FAIL"
		if (!res$ok) .explain(res$r)
	} else {
		jc <- if (length(TXT$id) >= 1) .json_cands_for(P_NAME[i], P_TEXTID[i]) else JSON_CAND[[i]]
		idv <- jc[[CAL_KEY]]
		if (is.na(idv)) {
			status[i] <- "FAIL"
		} else {
			res <- .attempt(idv, DEST[i], sprintf("app-%03d", i))
			status[i] <- if (res$ok) "OK" else if (res$r$status == 124) "FAIL-TIMEOUT" else "FAIL"
			if (!res$ok) .explain(res$r)
		}
	}
	cat(sprintf(" %3d/%d %-12s %s\n", i, length(P_NAME),
	            status[i], .trunc(P_NAME[i], SCREEN_W - 22)))
}

# Manifest rewritten whole each run: dir, id, name, this run's status. The
# id column is whatever id was actually tried last (adopted key when known
# and present, else the text id, else the first JSON candidate).
manifest_id <- vapply(seq_along(P_NAME), function(i) {
	if (!is.na(CAL_KEY) && CAL_KEY == "text" && !is.na(P_TEXTID[i])) return(P_TEXTID[i])
	jc <- if (length(TXT$id) >= 1) {
		if (is.na(CAL_KEY) || CAL_KEY == "text") NULL else .json_cands_for(P_NAME[i], P_TEXTID[i])
	} else JSON_CAND[[i]]
	if (!is.null(jc)) {
		if (!is.na(CAL_KEY) && CAL_KEY != "text" && !is.na(jc[[CAL_KEY]])) return(jc[[CAL_KEY]])
		nz <- jc[!is.na(jc)]
		if (length(nz)) return(nz[[1]])
	}
	if (!is.na(P_TEXTID[i])) return(P_TEXTID[i])
	NA_character_
}, character(1))
writeLines(c("# dir\tapp id\tname\tstatus (this run)",
             paste(.san, manifest_id, P_NAME, status, sep = "\t")),
           file.path(DEST_DIR, "_manifest.txt"))

# --- summary: the photographable part -------------------------------------
.rule()
.say(sprintf(" %d apps: %d ok, %d skipped, %d failed%s",
             length(status), sum(status == "OK"), sum(status == "SKIP"),
             sum(startsWith(status, "FAIL")),
             if (DRY_RUN) sprintf(", %d to do (dry run)", sum(status == "TODO")) else ""))
.say(" app id key: ", if (is.na(CAL_KEY)) "uncalibrated" else CAL_KEY)
for (i in which(startsWith(status, "FAIL"))) {
	.say("   FAIL ", .trunc(P_NAME[i], 30), "  ", manifest_id[i])
}
.say(" manifest: ", file.path(DEST_DIR, "_manifest.txt"))
.say(" rerun skips existing dirs; delete a dir to redo that app")
.say(" >> PHOTOGRAPH FROM THE LINE ABOVE THE COUNTS.")
.rule()
if (any(startsWith(status, "FAIL"))) quit(status = 1)
