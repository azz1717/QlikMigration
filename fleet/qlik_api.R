# qlik_api.R - the ONE place fleet/ talks to qlik-cli. PLAN-fleet.md section 1.
# Sourced (after shared/json_read.R), never run. Contract in INTERFACES.md.
#
# Built on qlik_cli_probe.R's VERIFIED findings (DESIGN 8): the command is
# passed to system2() UNQUOTED and every argument is shQuote()d individually
# (proven against an install path containing a space); every call carries a
# timeout, because an un-timed system2() hangs forever if qlik ever prompts;
# every listing passes --limit (DESIGN 8.3).
#
# THE TWO RULES THIS FILE EXISTS TO ENFORCE
#  1. No tenant WRITE happens except through qc_write(), and qc_write() does
#     nothing at all while DRY_RUN is TRUE - which it is unless a caller
#     turns it off. The first run of any write verb is therefore a listing of
#     the exact command lines, reviewable before anything is touched.
#  2. Every call, dry or live, leaves a line in fleet/audit.log with a
#     timestamp. Console output is not a record (PLAN-fleet.md, Adam).
#
# Globals read at CALL time, the same convention qlik_cli_shared.R uses, so a
# caller (or a test) can set them after this file is sourced:
#   DRY_RUN    TRUE (default) = write verbs print and log, run nothing.
#   AUDIT_LOG  path of the audit log; default "fleet/audit.log".
#   QC_TIMEOUT_S / QC_UNBUILD_TIMEOUT_S  seconds.
#   QLIK_CLI_PATH (env var) overrides qlik_cli_path.txt - see qlik_exe().

QC_TIMEOUT_S <- 60L
QC_UNBUILD_TIMEOUT_S <- 600L
if (!exists("DRY_RUN", envir = globalenv(), inherits = FALSE)) DRY_RUN <- TRUE

.qc_opt <- function(name, default) {
	v <- get0(name, envir = globalenv(), ifnotfound = default)
	if (is.null(v) || (length(v) == 1L && is.na(v))) default else v
}
.qc_dry <- function() isTRUE(.qc_opt("DRY_RUN", TRUE))

#' Locate qlik.exe. Env var QLIK_CLI_PATH wins over the config file.
#'
#' The env var is what lets a test (and only a test) point the whole stack at
#' diagnostics/mock_qlik.bat: qlik_cli_path.txt is per-machine, gitignored and
#' hand-written, so a test that overwrote it would destroy an operator's real
#' setting and silently leave a mock path behind on a VM. Reading the file
#' follows qlik_cli_unbuild.R exactly - blank lines and # comments dropped,
#' first real line wins, surrounding double quotes stripped so a straight
#' "Copy as path" paste works.
qlik_exe <- function(config = "qlik_cli_path.txt") {
	env <- Sys.getenv("QLIK_CLI_PATH", "")
	if (nzchar(env)) {
		p <- gsub('^"|"$', "", trimws(env))
		if (!file.exists(p))
			stop(sprintf("QLIK_CLI_PATH points at nothing: %s", p), call. = FALSE)
		return(p)
	}
	if (!file.exists(config))
		stop(sprintf("no %s (one line: the path to qlik.exe), and no QLIK_CLI_PATH",
		             config), call. = FALSE)
	cfg <- trimws(readLines(config, warn = FALSE))
	cfg <- cfg[nzchar(cfg) & !startsWith(cfg, "#")]
	if (!length(cfg)) stop(sprintf("%s is empty", config), call. = FALSE)
	p <- gsub('^"|"$', "", cfg[1])
	if (!file.exists(p))
		stop(sprintf("no such file: %s - fix %s", p, config), call. = FALSE)
	p
}

.qc_audit <- function(kind, args, status = NA_integer_, note = "") {
	path <- .qc_opt("AUDIT_LOG", file.path("fleet", "audit.log"))
	dir <- dirname(path)
	if (nzchar(dir) && !dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
	line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), kind,
	              if (is.na(status)) "-" else paste0("exit=", status),
	              paste(args, collapse = " "),
	              if (nzchar(note)) paste0("| ", note) else "", sep = "\t")
	cat(line, "\n", sep = "", file = path, append = TRUE)
	invisible(line)
}

#' The invocation primitive: run qlik with these args, capture everything.
#' Returns list(out, status, args). stderr is merged into stdout, so a
#' warning printed by a SUCCESSFUL call arrives as output text - which is why
#' nothing downstream may treat out[1] as data without checking its shape
#' (DESIGN 8.4, and the id that turned out to be a warning line).
qc_run <- function(args, timeout = .qc_opt("QC_TIMEOUT_S", 60L), qlik = NULL) {
	if (is.null(qlik)) qlik <- qlik_exe()
	out <- tryCatch(
		suppressWarnings(system2(qlik, shQuote(args), stdout = TRUE, stderr = TRUE,
		                         timeout = timeout)),
		error = function(e) structure(paste("could not start process:",
		                                    conditionMessage(e)), status = -1L))
	status <- attr(out, "status")
	if (is.null(status)) status <- 0L
	list(out = as.character(out), status = as.integer(status), args = args)
}

#' A failed call is a VALUE, not a stop(): a batch verb must be able to record
#' one app's failure and carry on. qc_failed() is the test; the object keeps
#' the exit status and the output for last_error.
qc_error <- function(r) structure(list(status = r$status, out = r$out, args = r$args),
                                  class = "qc_error")
qc_failed <- function(x) inherits(x, "qc_error")

# --- shape guards ---------------------------------------------------------
# Every reply shape this project reads was INFERRED from qlik.dev, not measured
# on the tenant (DESIGN 8.7). A reply that carries its rows under `items`
# instead of `data`, or an app copy that answers `id` where `attributes.id` was
# expected, used to read as "no rows" / "" and travel on as a silent wrong
# answer - json_items() will even find an `id` nested under ANY key. So the
# known keys are read in ONE place, and a missing one is a LOUD failure that
# names the call, the key and the keys that did arrive. `doctor` is the verb
# that runs these checks on purpose.

.qc_keys <- function(x) {
	if (is.list(x) && !is.null(names(x))) return(names(x))
	if (is.list(x)) return(sprintf("(unnamed list of %d)", length(x)))
	if (is.null(x)) return("(nothing)")
	sprintf("(%s)", class(x)[1])
}

#' A wrong-SHAPE failure, in the same qc_error currency as an exit code, with
#' status -3L so .fl_fail_msg() can print the message alone.
qc_shape_error <- function(what, expected, reply, args = character(0)) {
	qc_error(list(status = -3L, args = args,
	              out = sprintf("%s: expected `%s`, got keys: %s", what, expected,
	                            paste(.qc_keys(reply), collapse = ", "))))
}

#' Read a KNOWN key out of a reply, or fail loudly.
#'
#' `keys` is either one path - a character vector walked by json_get(), so
#' c("attributes", "id") means attributes.id - or a LIST of such paths, tried
#' in order, which is how `qName` or `name` is expressed. `what` names the call
#' for the message. A present key holding an EMPTY array is a value, not a
#' miss: an empty `data` is a listing with no rows, which is a normal answer.
qc_expect <- function(reply, keys, what) {
	paths <- if (is.list(keys)) keys else list(keys)
	for (p in paths) {
		v <- do.call(json_get, c(list(reply), as.list(p)))
		if (!is.null(v)) return(v)
	}
	qc_shape_error(what,
	               paste(vapply(paths, function(p) paste(as.character(p), collapse = "."),
	                            character(1)), collapse = "` or `"),
	               reply)
}

#' Run a READ-ONLY qlik command and parse its reply.
#'
#' json = TRUE appends --json when the caller has not already. The parsed
#' value is returned on success; a qc_error object (qc_failed() TRUE) on a
#' non-zero exit or an unparseable reply. Every call is audited.
qc <- function(args, json = TRUE, timeout = .qc_opt("QC_TIMEOUT_S", 60L)) {
	if (json && !("--json" %in% args)) args <- c(args, "--json")
	r <- qc_run(args, timeout = timeout)
	.qc_audit("read", args, r$status)
	if (r$status != 0L) return(qc_error(r))
	if (!json) return(r$out)
	txt <- paste(r$out, collapse = "\n")
	parsed <- tryCatch(json_parse(txt), error = function(e) e)
	if (inherits(parsed, "error")) {
		# The TEXT, not just the parser's complaint: "unexpected token" tells an
		# operator nothing, while the first 200 characters of what actually came
		# back (a login prompt, an HTML error page, a plain-text warning) names
		# the problem on sight. .fl_fail_msg() prints the first two lines, so the
		# text rides along with every recorded failure.
		r$out <- c(paste("unparseable reply:", conditionMessage(parsed)),
		           paste0("text: ", substr(txt, 1L, 200L)), r$out)
		r$status <- -2L
		.qc_audit("read", args, r$status, "unparseable reply")
		return(qc_error(r))
	}
	parsed
}

#' The `next` page token of a listing reply, or NA when that was the last page.
#'
#' Shape (PLAN-fleet.md section 0, INFERRED from qlik.dev, mirrored by
#' diagnostics/mock_qlik.R): links.next.href is a URL carrying `next=<token>`.
#' A bare string token under links.next is accepted too. Anything else counts
#' as "no more pages", so an unexpected shape ends the loop instead of
#' spinning forever - the failure mode worth designing against here.
qc_next_token <- function(reply, what = "listing") {
	nx <- json_get(reply, "links", "next")
	if (is.null(nx)) return(NA_character_)
	href <- if (is.character(nx)) nx else qc_expect(nx, "href", paste(what, "paging"))
	if (qc_failed(href)) return(href)
	if (!is.character(href) || length(href) != 1L) return(NA_character_)
	m <- regmatches(href, regexpr("[?&]next=([^&]+)", href))
	if (!length(m)) return(NA_character_)
	sub("^[?&]next=", "", m)
}

#' Items of a listing reply, whatever it is wrapped in: a bare array, a
#' `data` array, or a nested one. Keyed on `key` being present (json_items()).
qc_items <- function(reply, key = "id", what = "listing") {
	if (is.null(reply)) return(list())
	if (qc_failed(reply)) return(reply)
	# A bare ARRAY is a listing in its own right (`collection ls` and `item
	# collections` answer that way). An OBJECT must carry `data`: without this
	# a reply wrapped in `items` fell through to json_items(), which happily
	# found the rows under any key at all and reported success.
	if (is.list(reply) && !is.null(names(reply))) {
		d <- qc_expect(reply, "data", what)
		if (qc_failed(d)) return(d)
		return(json_items(d, key))
	}
	json_items(reply, key)
}

#' Follow a paged listing to the end and return every item.
#'
#' --limit is always sent (DESIGN 8.3: without it the API's own default page
#' silently truncates a listing, and a substring search over the truncation
#' once reported a present space as missing). max_pages is a stop, not a
#' policy: a server that keeps handing back the same token would otherwise
#' loop forever.
qc_pages <- function(args, limit = 100L, key = "id",
                     timeout = .qc_opt("QC_TIMEOUT_S", 60L), max_pages = 100L,
                     what = NULL) {
	if (!("--limit" %in% args)) args <- c(args, "--limit", as.character(limit))
	# The call, for the shape messages: the first two words that are not flags
	# and not a flag's value ("app ls", "data-connection ls").
	if (is.null(what))
		what <- paste(utils::head(args[!startsWith(args, "--")], 2L), collapse = " ")
	out <- list()
	token <- NA_character_
	seen <- character(0)
	for (page in seq_len(max_pages)) {
		a <- if (is.na(token)) args else c(args, "--next", token)
		reply <- qc(a, json = TRUE, timeout = timeout)
		if (qc_failed(reply)) return(reply)
		items <- qc_items(reply, key, what)
		if (qc_failed(items)) return(items)
		if (length(items)) out[(length(out) + 1L):(length(out) + length(items))] <- items
		token <- qc_next_token(reply, what)
		if (qc_failed(token)) return(token)
		if (is.na(token) || token %in% seen) break
		seen <- c(seen, token)
	}
	out
}

#' The ONLY way a tenant write is issued.
#'
#' DRY_RUN (default TRUE) prints the exact command line, writes a DRYRUN line
#' to the audit log and returns NULL without running anything. Live, it runs
#' the command, audits the exit status, and returns the parsed reply (or a
#' qc_error). `label` is free text for the audit line - which app, which verb.
qc_write <- function(args, json = TRUE, label = "",
                     timeout = .qc_opt("QC_TIMEOUT_S", 60L)) {
	if (json && !("--json" %in% args)) args <- c(args, "--json")
	if (.qc_dry()) {
		cat("DRY RUN $ qlik ", paste(shQuote(args), collapse = " "), "\n", sep = "")
		.qc_audit("DRYRUN", args, NA_integer_, label)
		return(invisible(NULL))
	}
	r <- qc_run(args, timeout = timeout)
	.qc_audit("WRITE", args, r$status, label)
	if (r$status != 0L) return(qc_error(r))
	if (!json) return(r$out)
	parsed <- tryCatch(json_parse(paste(r$out, collapse = "\n")),
	                   error = function(e) NULL)
	if (is.null(parsed)) return(r$out)
	parsed
}

#' `app unbuild` into `dir`, with the long timeout one whole app definition
#' needs. A FAILED unbuild's directory is removed, so a rerun retries that app
#' instead of skipping a half-written bundle - qlik_cli_unbuild.R learned that
#' the hard way and the rule travels with the operation, not with the script.
#'
#' Audited as `fetch`, not `WRITE`: it changes nothing on the tenant. It still
#' honours DRY_RUN, because it writes a directory HERE and a batch fetch over
#' the whole estate is worth being able to rehearse.
qc_unbuild <- function(app_id, dir, timeout = .qc_opt("QC_UNBUILD_TIMEOUT_S", 600L)) {
	args <- c("app", "unbuild", "--app", app_id, "--dir", dir, "--no-data")
	if (.qc_dry()) {
		cat("DRY RUN $ qlik ", paste(shQuote(args), collapse = " "), "\n", sep = "")
		.qc_audit("DRYRUN", args, NA_integer_, paste("unbuild", app_id))
		return(invisible(NULL))
	}
	r <- qc_run(args, timeout = timeout)
	.qc_audit("fetch", args, r$status, paste("unbuild", app_id))
	ok <- r$status == 0L && dir.exists(dir) &&
		length(list.files(dir, recursive = TRUE)) > 0L
	if (!ok) {
		if (dir.exists(dir)) unlink(dir, recursive = TRUE)
		return(qc_error(r))
	}
	invisible(dir)
}
