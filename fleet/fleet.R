# fleet.R - the batch CLI. `Rscript fleet/fleet.R <verb> [options]`.
# PLAN-fleet.md sections 1-3. Contract in INTERFACES.md.
#
# One subcommand per action, all of them driven by ONE ledger
# (fleet/manifest.csv, keyed by app id - the tenant has 578 duplicate app
# NAMES, so nothing here may key on a name). The console UI added in M1 is a
# menu that shells out to these same verbs, so every action stays
# reproducible without the UI and testable offline against
# diagnostics/mock_qlik.bat (fleet/test_fleet.R).
#
# M0 implemented spaces / apps / add / status. M1 adds import-unbuilt /
# process / report / rollup and turns `status` into a reader of master.csv.
# M2 adds fetch / reconcile-ids, M4 stamp / reconcile. The remaining verbs are
# named here and exit 1 with the milestone that brings them, so the CLI never
# pretends to have done something it has not.
#
# Exit codes (PLAN-fleet.md section 3): 0 all ok, 2 some rows blocked,
# 1 usage or configuration error.
#
# Output is 72 columns and ASCII (DESIGN 8.5): on the VM the return channel
# is a photograph of the console, and files - not the console - are the
# record.

.fl_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.FL_DIRECT <- length(.fl_file) > 0L &&
	grepl("fleet[/\\\\]fleet[.]R$", .fl_file[1])
if (.FL_DIRECT) {
	.fl_dir <- tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .fl_file[1])))),
	                    error = function(e) NA_character_)
	if (is.na(.fl_dir) || !dir.exists(.fl_dir))
		stop("could not locate the project folder; run 'Rscript fleet/fleet.R'",
		     call. = FALSE)
	setwd(.fl_dir)
}
source("shared/json_read.R")
source("shared/csv_read.R")
source("fleet/qlik_api.R")

FLEET_DIR <- "fleet"
MANIFEST_DEFAULT <- file.path(FLEET_DIR, "manifest.csv")
SPACES_CSV <- file.path(FLEET_DIR, "spaces.csv")
SCREEN_W <- 72L

# Ordered. `blocked` is deliberately NOT in this vector: it is a state a row
# can be in at ANY stage (with last_error set), not a stage of its own, and
# giving it a rank would let a comparison quietly decide it comes after
# `verified`.
FLEET_STAGES <- c("listed", "unbuilt", "styled", "retargeted", "built", "verified")
MANIFEST_COLS <- c("space_id", "space_name", "app_id", "app_name", "item_id",
                   "stage", "stage_at", "target_app_id", "target_name",
                   "last_error", "notes")

.fl_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.fl_say <- function(...) {
	s <- gsub("[\r\n\t]+", " ", paste0(...))
	if (nchar(s) > SCREEN_W) s <- paste0(substr(s, 1L, SCREEN_W - 3L), "...")
	cat(s, "\n", sep = "")
}
.fl_rule <- function() cat(strrep("-", SCREEN_W), "\n", sep = "")
.fl_warn <- function(...) cat("fleet: ", paste0(...), "\n", sep = "", file = stderr())

#' Position of `stage` in the pipeline, NA for `blocked` or anything unknown.
stage_rank <- function(stage) {
	r <- match(as.character(stage), FLEET_STAGES)
	as.integer(r)
}

#' Is moving a row from `from` to `to` allowed?
#'
#' Forward ONE step at a time, or stay put, or go BACKWARD any distance
#' (map upkeep drops apps back to `styled`; a re-fetch drops them to
#' `unbuilt`). Skipping forward is what this refuses: a row cannot reach
#' `built` without having been `retargeted`, because the artefacts each stage
#' promises are what the next stage reads. `blocked` may be entered from and
#' left to any stage - it is an error state, not a position.
stage_advance_ok <- function(from, to) {
	if (identical(to, "blocked") || identical(from, "blocked")) return(TRUE)
	rf <- stage_rank(from)
	rt <- stage_rank(to)
	if (is.na(rt)) return(FALSE)
	if (is.na(rf)) return(TRUE)          # a new row starts wherever it is put
	rt <= rf + 1L
}

#' An empty ledger with the columns of PLAN-fleet.md section 2.
manifest_new <- function() {
	m <- as.data.frame(matrix(character(0), nrow = 0L, ncol = length(MANIFEST_COLS)),
	                   stringsAsFactors = FALSE)
	names(m) <- MANIFEST_COLS
	m
}

#' Read the ledger. A missing file is an EMPTY ledger, not an error: the first
#' `add` on a new machine has nothing to read. Missing columns are filled in
#' rather than refused, so a hand-edited csv that lost a column still loads;
#' everything stays character (shared/csv_read.R's reason: ids and codes must
#' not be coerced).
manifest_read <- function(path = MANIFEST_DEFAULT) {
	if (!file.exists(path)) return(manifest_new())
	m <- read_csv_any(path)
	for (cn in MANIFEST_COLS) if (is.null(m[[cn]])) m[[cn]] <- rep("", nrow(m))
	m <- m[, MANIFEST_COLS, drop = FALSE]
	for (cn in MANIFEST_COLS) {
		v <- as.character(m[[cn]])
		v[is.na(v)] <- ""
		m[[cn]] <- v
	}
	m
}

#' Write the ledger, sorted by space then app name so a diff between two runs
#' is readable. Directory created if needed.
manifest_write <- function(m, path = MANIFEST_DEFAULT) {
	d <- dirname(path)
	if (nzchar(d) && !dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
	if (nrow(m) > 1L) m <- m[order(m$space_name, m$app_name, m$app_id), , drop = FALSE]
	utils::write.csv(m, path, row.names = FALSE, na = "")
	invisible(path)
}

#' Upsert rows by app_id. `rows` is a data.frame with any subset of the
#' manifest columns plus app_id.
#'
#' An existing row keeps every field the new one leaves empty - `add` re-run
#' after `fetch` must not blank a target_app_id or reset a stage. A stage
#' change is refused (row untouched, a warning printed) when
#' stage_advance_ok() says it would skip forward: the ledger is the only
#' record of what has actually been done to an app, so a silent jump is worse
#' than a refusal.
manifest_upsert <- function(m, rows) {
	if (is.null(rows) || !nrow(rows)) return(m)
	for (i in seq_len(nrow(rows))) {
		new <- rows[i, , drop = FALSE]
		id <- as.character(new$app_id)
		at <- which(m$app_id == id)
		if (!length(at)) {
			blank <- manifest_new()[1, , drop = FALSE]
			blank[1, ] <- ""
			for (cn in intersect(names(new), MANIFEST_COLS)) {
				v <- as.character(new[[cn]])
				if (length(v) && !is.na(v) && nzchar(v)) blank[[cn]] <- v
			}
			if (!nzchar(blank$stage)) blank$stage <- "listed"
			if (!nzchar(blank$stage_at)) blank$stage_at <- .fl_now()
			m <- rbind(m, blank)
			next
		}
		k <- at[1]
		for (cn in intersect(names(new), MANIFEST_COLS)) {
			if (cn %in% c("stage", "stage_at")) next
			v <- as.character(new[[cn]])
			if (length(v) && !is.na(v) && nzchar(v)) m[[cn]][k] <- v
		}
		ns <- as.character(new$stage)
		if (length(ns) && !is.na(ns) && nzchar(ns) && !identical(ns, m$stage[k])) {
			if (stage_advance_ok(m$stage[k], ns)) {
				m$stage[k] <- ns
				m$stage_at[k] <- .fl_now()
			} else {
				.fl_warn("refused stage ", m$stage[k], " -> ", ns, " for ", id,
				         " (would skip forward)")
			}
		}
	}
	m
}

# --- argument parsing -----------------------------------------------------
.FL_VALUE_FLAGS <- c("--manifest", "--apps", "--stage", "--space", "--name",
                     "--type", "--limit", "--to-space", "--mode", "--dir")
.FL_BOOL_FLAGS <- c("--dry-run", "--live", "--all", "--digest", "--no-rollup",
                    "--force", "--no-style", "--allow-unresolved", "--adopt",
                     "--help")

#' Split argv into a verb and an options list. Returns
#' list(verb, opts, error): `error` non-empty means usage, and the caller
#' exits 1 without running anything. An unknown flag is an ERROR, never
#' ignored - a mistyped --appss selecting everything instead of three apps is
#' exactly the accident a batch tool must not have.
fleet_parse_args <- function(argv) {
	res <- list(verb = "", opts = list(), error = "", pos = NULL)
	if (!length(argv)) return(list(verb = "", opts = list(), error = "no verb given",
	                               pos = NULL))
	res$verb <- argv[1]
	i <- 2L
	while (i <= length(argv)) {
		a <- argv[i]
		# ONE bare positional (PLAN-fleet.md section 3: `import-unbuilt <dir>`),
		# kept out of `opts` so it cannot be mistaken for a flag. Anything
		# starting with `--` is still matched against the known flags and is
		# still an ERROR when unknown - a mistyped --appss must never widen a
		# selection, which is the whole reason this parser is strict.
		if (!startsWith(a, "--") && !nzchar(res$error)) {
			if (!is.null(res$pos)) {
				res$error <- paste("unexpected second argument:", a)
				return(res)
			}
			res$pos <- a
			i <- i + 1L
			next
		}
		if (a %in% .FL_BOOL_FLAGS) {
			res$opts[[sub("^--", "", a)]] <- TRUE
			i <- i + 1L
		} else if (a %in% .FL_VALUE_FLAGS) {
			if (i + 1L > length(argv)) {
				res$error <- paste(a, "needs a value")
				return(res)
			}
			res$opts[[sub("^--", "", a)]] <- argv[i + 1L]
			i <- i + 2L
		} else {
			res$error <- paste("unknown option:", a)
			return(res)
		}
	}
	if (isTRUE(res$opts[["dry-run"]]) && isTRUE(res$opts[["live"]]))
		res$error <- "--dry-run and --live contradict each other"
	res
}

.fl_opt <- function(opts, name, default = NULL) {
	v <- opts[[name]]
	if (is.null(v)) default else v
}

# --- shared verb plumbing -------------------------------------------------
.fl_fail_msg <- function(r) {
	head_out <- utils::head(r$out, 2L)
	paste0("exit ", r$status, if (length(head_out)) paste0(": ", paste(head_out, collapse = " ")))
}

.fl_str <- function(x, default = "") {
	if (is.null(x) || !length(x)) return(default)
	v <- x[[1]]
	if (is.null(v) || is.na(v)) default else as.character(v)
}

# Sorted by name, always. `apps` prints an index and `add --apps 1,3` selects
# by that index, so the two verbs must agree on order without either of them
# storing a listing - and tenant listing order is not something this project
# has ever measured.
.fl_app_rows <- function(space_id, name_filter = NULL) {
	items <- qc_pages(c("app", "ls", "--spaceId", space_id))
	if (qc_failed(items)) return(items)
	if (!length(items)) return(data.frame(id = character(0), name = character(0),
	                                      modified = character(0),
	                                      stringsAsFactors = FALSE))
	df <- data.frame(
		id = vapply(items, function(it) .fl_str(it[["id"]]), character(1)),
		name = vapply(items, function(it) .fl_str(it[["name"]]), character(1)),
		modified = vapply(items, function(it)
			.fl_str(if (!is.null(it[["modifiedDate"]])) it[["modifiedDate"]] else it[["modified"]]),
			character(1)),
		stringsAsFactors = FALSE)
	if (!is.null(name_filter) && nzchar(name_filter))
		df <- df[grepl(name_filter, df$name, fixed = TRUE), , drop = FALSE]
	df[order(df$name, df$id), , drop = FALSE]
}

.fl_space_rows <- function(name_filter = NULL, type_filter = NULL) {
	items <- qc_pages(c("space", "ls"))
	if (qc_failed(items)) return(items)
	df <- data.frame(
		id = vapply(items, function(it) .fl_str(it[["id"]]), character(1)),
		name = vapply(items, function(it) .fl_str(it[["name"]]), character(1)),
		type = vapply(items, function(it) .fl_str(it[["type"]]), character(1)),
		stringsAsFactors = FALSE)
	# Filtered HERE, not by a --name flag on the CLI: DESIGN 8.2 - the
	# published reference documents flags the installed 3.2.0 build rejects,
	# and a listing this project already has in hand needs no second call.
	if (!is.null(name_filter) && nzchar(name_filter))
		df <- df[grepl(name_filter, df$name, fixed = TRUE), , drop = FALSE]
	if (!is.null(type_filter) && nzchar(type_filter))
		df <- df[df$type %in% strsplit(type_filter, ",", fixed = TRUE)[[1]], , drop = FALSE]
	df[order(df$name, df$id), , drop = FALSE]
}

# An index from a previous `spaces` run, or a literal space id. The cache is
# what makes "--space 2" mean the same thing twice in a row; a space id is
# always accepted, so the cache is a convenience and never a dependency.
.fl_resolve_space <- function(sp) {
	if (is.null(sp) || !nzchar(sp)) return(NULL)
	if (grepl("^[0-9]+$", sp)) {
		if (!file.exists(SPACES_CSV)) return(NULL)
		cache <- read_csv_any(SPACES_CSV)
		k <- as.integer(sp)
		if (k < 1L || k > nrow(cache)) return(NULL)
		return(cache$id[k])
	}
	sp
}

.fl_write_csv <- function(df, path) {
	d <- dirname(path)
	if (nzchar(d) && !dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
	utils::write.csv(df, path, row.names = FALSE, na = "")
	invisible(path)
}

# --- verbs ----------------------------------------------------------------
.fl_verb_spaces <- function(opts) {
	df <- .fl_space_rows(.fl_opt(opts, "name"), .fl_opt(opts, "type"))
	if (qc_failed(df)) { .fl_warn("space ls failed: ", .fl_fail_msg(df)); return(1L) }
	.fl_write_csv(cbind(index = seq_len(nrow(df)), df), SPACES_CSV)
	.fl_rule()
	.fl_say(sprintf("%-3s %-26s %-9s %s", "#", "id", "type", "name"))
	for (i in seq_len(nrow(df)))
		.fl_say(sprintf("%-3d %-26s %-9s %s", i, df$id[i], df$type[i], df$name[i]))
	.fl_rule()
	.fl_say(nrow(df), " spaces; cached in ", SPACES_CSV)
	0L
}

.fl_verb_apps <- function(opts) {
	sid <- .fl_resolve_space(.fl_opt(opts, "space"))
	if (is.null(sid)) { .fl_warn("apps needs --space <id|index from 'spaces'>"); return(1L) }
	df <- .fl_app_rows(sid, .fl_opt(opts, "name"))
	if (qc_failed(df)) { .fl_warn("app ls failed: ", .fl_fail_msg(df)); return(1L) }
	.fl_rule()
	.fl_say(sprintf("%-3s %-38s %s", "#", "id", "name"))
	for (i in seq_len(nrow(df)))
		.fl_say(sprintf("%-3d %-38s %s", i, df$id[i], df$name[i]))
	.fl_rule()
	.fl_say(nrow(df), " apps in space ", sid, " (manifest untouched)")
	0L
}

# item_id is fetched in one call for up to 100 apps (`item ls --resourceIds`),
# not one call per app: it is needed only by the tag verbs (M4), and a blank
# is recorded rather than a failure if the item lookup itself fails.
.fl_item_ids <- function(app_ids) {
	out <- setNames(rep("", length(app_ids)), app_ids)
	if (!length(app_ids)) return(out)
	chunks <- split(app_ids, ceiling(seq_along(app_ids) / 100L))
	for (ch in chunks) {
		items <- qc_pages(c("item", "ls", "--resourceIds", paste(ch, collapse = ","),
		                    "--resourceType", "app"))
		if (qc_failed(items)) {
			.fl_warn("item ls failed: ", .fl_fail_msg(items), " (item_id left blank)")
			next
		}
		for (it in items) {
			rid <- .fl_str(it[["resourceId"]])
			if (nzchar(rid) && rid %in% names(out)) out[[rid]] <- .fl_str(it[["id"]])
		}
	}
	out
}

.fl_verb_add <- function(opts) {
	sid <- .fl_resolve_space(.fl_opt(opts, "space"))
	if (is.null(sid)) { .fl_warn("add needs --space <id|index from 'spaces'>"); return(1L) }
	sel_idx <- .fl_opt(opts, "apps")
	if (is.null(sel_idx) && !isTRUE(opts[["all"]]) && is.null(.fl_opt(opts, "name"))) {
		.fl_warn("add needs one of --apps <i,j>, --name <substr>, --all")
		return(1L)
	}
	spaces <- .fl_space_rows()
	sname <- if (!qc_failed(spaces) && sid %in% spaces$id) spaces$name[match(sid, spaces$id)] else ""
	df <- .fl_app_rows(sid, .fl_opt(opts, "name"))
	if (qc_failed(df)) { .fl_warn("app ls failed: ", .fl_fail_msg(df)); return(1L) }
	if (!is.null(sel_idx)) {
		idx <- suppressWarnings(as.integer(strsplit(sel_idx, ",", fixed = TRUE)[[1]]))
		if (any(is.na(idx)) || any(idx < 1L) || any(idx > nrow(df))) {
			.fl_warn("--apps must be indexes from 'fleet.R apps --space ", sid, "'")
			return(1L)
		}
		df <- df[idx, , drop = FALSE]
	}
	if (!nrow(df)) { .fl_say("nothing selected"); return(0L) }
	items <- .fl_item_ids(df$id)
	rows <- data.frame(space_id = sid, space_name = sname, app_id = df$id,
	                   app_name = df$name, item_id = unname(items[df$id]),
	                   stage = "listed", stringsAsFactors = FALSE)
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_upsert(manifest_read(path), rows)
	manifest_write(m, path)
	.fl_say(nrow(rows), " app(s) upserted into ", path, "; ", nrow(m), " row(s) total")
	0L
}

.fl_verb_status <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	.fl_rule()
	.fl_say("manifest: ", path, "  rows: ", nrow(m))
	counts <- table(factor(m$stage, levels = c(FLEET_STAGES, "blocked")))
	.fl_say(paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = "  "))
	if (!isTRUE(opts[["digest"]])) {
		for (i in seq_len(nrow(m)))
			.fl_say(sprintf("%-11s %-8s %s", substr(m$app_id[i], 1L, 11L),
			                m$stage[i], m$app_name[i]))
	} else {
		blocked <- m[m$stage == "blocked", , drop = FALSE]
		for (i in seq_len(min(nrow(blocked), 10L)))
			.fl_say(sprintf("BLOCKED %-11s %s", substr(blocked$app_id[i], 1L, 11L),
			                blocked$last_error[i]))
	}
	.fl_rule()
	# The board: master.csv, sorted by readiness, as PLAN-fleet.md section 3
	# specifies. It is a READER here - rollup is what builds it - so `status`
	# never disagrees with the file Adam is actually given.
	ms <- .fl_read(MASTER_CSV)
	if (is.null(ms) || !nrow(ms)) {
		.fl_say("no ", MASTER_CSV, " yet - run 'fleet.R rollup'")
	} else {
		ms <- ms[order(-.fl_num(ms$readiness), ms$app_name), , drop = FALSE]
		n <- if (isTRUE(opts[["digest"]])) min(nrow(ms), 12L) else nrow(ms)
		.fl_say(sprintf("%-3s %-32s %-10s %4s %s", "rdy", "app", "stage", "%rtg",
		                "blockers"))
		for (i in seq_len(n))
			.fl_say(sprintf("%-3s %-32s %-10s %4s %s",
			                ms$readiness[i], substr(ms$app_name[i], 1L, 32L),
			                substr(ms$stage[i], 1L, 10L), ms$pct_retargeted[i],
			                ms$blockers[i]))
		.fl_rule()
		.fl_say(MASTER_CSV, ": ", nrow(ms), " row(s); map_check findings ",
		        if (is.null(ms$map_check_findings)) "-" else ms$map_check_findings[1])
	}
	if (any(m$stage == "blocked")) 2L else 0L
}

# =========================================================================
# M1: local bundles in, style + retarget out, and the master ledger.
# PLAN-fleet.md sections 3 and 5. No tenant is touched by any of it.
# =========================================================================

RSCRIPT <- file.path(R.home("bin"), "Rscript")
APPS_DIR <- file.path(FLEET_DIR, "apps")
FLAGS_CSV <- file.path(FLEET_DIR, "flags.csv")
MASTER_CSV <- file.path(FLEET_DIR, "master.csv")
MASTER_LOADS_CSV <- file.path(FLEET_DIR, "master_loads.csv")
MASTER_UNUSED_CSV <- file.path(FLEET_DIR, "master_unused.csv")
MAP_CHECK_STATUS <- file.path(FLEET_DIR, "map_check_status.csv")
FLEET_CHILD_TIMEOUT_S <- 300L

# retarget_loads.R's own status vocabulary, named once here so master.csv and
# the blocking rule (D7) cannot drift apart. `already-mapped` is what a rerun
# on an already-retargeted script reports, and it is a SUCCESS.
FLEET_LOAD_OK <- c("retargeted", "retargeted-pending-import", "already-mapped")
FLEET_LOAD_UNRESOLVED <- c("not-in-map", "multi-source")

# D14 (ANSWERED 2026-09-10 by the overseer, PLAN-fleet.md section 8): the
# pct_retargeted DENOMINATOR is ELIGIBLE loads only. These four statuses were
# never retargeting work in the first place - an out-of-scope or commented
# load, a geospatial one, a directory statement - so counting them made 14
# generator apps read 0% with nothing left to do. DONE is the pair that means
# a load now points at the cloud; `retargeted-pending-import` is deliberately
# NOT here, because pending is not done.
FLEET_LOAD_INELIGIBLE <- c("out-of-scope", "commented", "geospatial",
                           "directory-statement")
FLEET_LOAD_DONE <- c("retargeted", "already-mapped")

# The local bundles on this machine (PLAN-fleet.md M1 acceptance): the 18
# generator exports, the two example apps and the retarget trial app.
.FL_LOCAL_SOURCES <- c("retargeting/unbuilt", "app-unbuilt", "app2-unbuilt",
                       "retargeting/05 - Grant Acquittals Report")

#' Where one app's artefacts live.
#'
#' The app_id is the key everywhere; the DIRECTORY NAME is a sanitised form of
#' it, because a D12 local key ("local:01-ess-qvd-builder-cdp") contains a
#' colon and Windows has no such filename. Every caller goes through here, so
#' the two spellings can never be derived independently.
.fl_app_dir <- function(app_id, apps_dir = APPS_DIR)
	file.path(apps_dir, gsub("[^A-Za-z0-9._-]", "_", app_id))

.fl_slug <- function(x) {
	s <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(x)))
	if (nzchar(s)) s else "unnamed"
}

#' The app id of a local bundle. D12 + PLAN-fleet.md section 3 (CORRECTED).
#'
#' app-properties.json has no id key. Where the export carries a thumbnail the
#' real tenant id is inside its url (`/api/v1/apps/<guid>/media/...`) - 10 of
#' the 21 bundles here do - and a real id is always preferred: it is what M2's
#' reconcile-ids would otherwise have to recover. Otherwise D12's synthetic
#' key, `local:` + the lowercased qTitle, and the manifest row says so.
fleet_local_app_id <- function(props, dir) {
	url <- .fl_str(json_get(props, "qThumbnail", "qUrl"))
	m <- regmatches(url, regexpr("apps/[0-9a-fA-F]{8}-[0-9a-fA-F-]{27}", url))
	if (length(m) && nzchar(m[1])) return(sub("^apps/", "", m[1]))
	paste0("local:", .fl_slug(.fl_str(props[["qTitle"]], basename(dir))))
}

#' Every unbuilt bundle under `root` - or `root` itself if it is one.
fleet_bundle_dirs <- function(root) {
	if (!dir.exists(root)) return(character(0))
	if (file.exists(file.path(root, "app-properties.json"))) return(root)
	hits <- list.files(root, pattern = "^app-properties[.]json$",
	                   recursive = TRUE, full.names = TRUE)
	sort(unique(dirname(hits)))
}

#' Run one child pipeline as its own Rscript process, exactly as
#' ui/console_ui.R does, and append everything it said to `log`.
#'
#' stdout and stderr are CAPTURED and then written, rather than redirected to
#' the same file by system2(): one file named as both `stdout` and `stderr`
#' is two independent handles, and on Windows the second truncates the first.
#' Always timed out - an un-timed child that prompts hangs the whole batch.
.fl_child <- function(args, log, timeout = FLEET_CHILD_TIMEOUT_S) {
	cat("\n$ Rscript ", paste(args, collapse = " "), "  [", .fl_now(), "]\n",
	    sep = "", file = log, append = TRUE)
	out <- tryCatch(
		suppressWarnings(system2(RSCRIPT, shQuote(args), stdout = TRUE,
		                         stderr = TRUE, timeout = timeout)),
		error = function(e) structure(paste("could not start Rscript:",
		                                    conditionMessage(e)), status = -1L))
	st <- attr(out, "status")
	if (is.null(st)) st <- 0L
	out <- as.character(out)
	if (length(out)) cat(out, sep = "\n", file = log, append = TRUE)
	list(status = as.integer(st), out = out)
}

# A count that survives whatever shape a scanner hands back. Real case, caught
# on the first fleet-wide `report` run: an empty scanner result is NULL, and
# nrow(NULL) is NULL, not 0 - which made data.frame() stop with "arguments
# imply differing number of rows" and took the whole 21-app batch down with it.
.fl_count <- function(x) {
	n <- if (is.data.frame(x)) nrow(x) else length(x)
	if (!length(n) || is.na(n)) 0L else as.integer(n)
}

.fl_tail_msg <- function(out, n = 2L) {
	out <- out[nzchar(trimws(out))]
	s <- paste(utils::tail(out, n), collapse = " | ")
	if (nchar(s) > 160L) substr(s, 1L, 160L) else s
}

#' Mark a row blocked with its reason. The ledger is the only record of what
#' has been done to an app, so a failure is WRITTEN, never only printed.
.fl_block <- function(m, id, msg) {
	.fl_warn(id, ": ", msg)
	manifest_upsert(m, data.frame(app_id = id, stage = "blocked",
	                              last_error = msg, stringsAsFactors = FALSE))
}

# manifest_upsert() deliberately never blanks a field it was not given, which
# is right for every field except this one: a row that succeeds must lose the
# error that blocked it last time, or the ledger reports a stale failure
# forever. Cleared directly, at the one place a stage actually advances.
.fl_clear_error <- function(m, id) {
	k <- which(m$app_id == id)
	if (length(k)) m$last_error[k] <- ""
	m
}

#' Rows selected by --apps <id,id> / --stage <s> / --all. NULL = no selection
#' given, which every batch verb treats as a usage error rather than "all".
.fl_select <- function(m, opts) {
	ids <- .fl_opt(opts, "apps")
	stg <- .fl_opt(opts, "stage")
	if (!is.null(ids))
		return(m[m$app_id %in% trimws(strsplit(ids, ",", fixed = TRUE)[[1]]), , drop = FALSE])
	if (!is.null(stg))
		return(m[m$stage %in% trimws(strsplit(stg, ",", fixed = TRUE)[[1]]), , drop = FALSE])
	if (isTRUE(opts[["all"]])) return(m)
	NULL
}

# --- import-unbuilt -------------------------------------------------------
.fl_verb_import_unbuilt <- function(opts, pos = NULL) {
	roots <- if (!is.null(pos)) pos else .fl_opt(opts, "dir")
	if (is.null(roots)) roots <- .FL_LOCAL_SOURCES
	dirs <- unlist(lapply(roots, fleet_bundle_dirs), use.names = FALSE)
	if (!length(dirs)) {
		.fl_warn("no app-properties.json under: ", paste(roots, collapse = " "))
		return(1L)
	}
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	rows <- NULL
	seen <- character(0)
	n_synth <- 0L
	for (d in dirs) {
		props <- tryCatch(json_read(file.path(d, "app-properties.json")),
		                  error = function(e) NULL)
		if (is.null(props)) { .fl_warn("unreadable app-properties.json: ", d); next }
		title <- .fl_str(props[["qTitle"]], basename(d))
		id <- fleet_local_app_id(props, d)
		# Two bundles that share a qTitle would share a synthetic key and one
		# would silently overwrite the other's artefacts. Suffixed and warned
		# instead - nothing here may key on a name (578 duplicates on tenant).
		if (id %in% seen) {
			k <- 2L
			while (paste0(id, "-", k) %in% seen) k <- k + 1L
			.fl_warn("duplicate id ", id, " for ", d, " -> ", paste0(id, "-", k))
			id <- paste0(id, "-", k)
		}
		seen <- c(seen, id)
		if (startsWith(id, "local:")) n_synth <- n_synth + 1L
		dest <- .fl_app_dir(id)
		if (!dir.exists(dest)) dir.create(dest, recursive = TRUE, showWarnings = FALSE)
		file.copy(list.files(d, full.names = TRUE), dest,
		          recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
		writeLines(title, file.path(dest, "name.txt"), useBytes = TRUE)
		# An app already past `unbuilt` keeps its stage: re-importing the same
		# bundle refreshes the source files, it does not undo a process run.
		cur <- m$stage[match(id, m$app_id)]
		stg <- if (is.na(cur) || !nzchar(cur) || identical(cur, "listed")) "unbuilt" else ""
		rows <- rbind(rows, data.frame(
			app_id = id, app_name = title, stage = stg,
			notes = if (startsWith(id, "local:")) "id pending tenant match" else "",
			stringsAsFactors = FALSE))
	}
	if (is.null(rows)) { .fl_warn("nothing imported"); return(1L) }
	m <- manifest_upsert(m, rows)
	manifest_write(m, path)
	.fl_say(nrow(rows), " bundle(s) imported into ", APPS_DIR, "; ",
	        n_synth, " with a local: id")
	.fl_say("manifest: ", path, " (", nrow(m), " row(s))")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	0L
}

# --- process --------------------------------------------------------------

#' Run retargeting/map_check.R and report what it found. D13: `process` runs it
#' FIRST and RECORDS the outcome, but does not block on it - today's map exits
#' 1 on 85 duplicate CRM keys and that is Adam's data to settle, not a reason
#' for the whole fleet to stop. Blocking stays for unresolved LOADS (D7).
fleet_map_check <- function(log = tempfile("mapcheck", fileext = ".log")) {
	r <- .fl_child(c("retargeting/map_check.R"), log)
	f <- file.path("retargeting", "map_check_findings.csv")
	n <- if (file.exists(f)) tryCatch(nrow(read_csv_any(f)),
	                                  error = function(e) NA_integer_) else 0L
	list(exit = r$status, findings = as.integer(n))
}

.fl_verb_process <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("process needs --all, --apps <id,id> or --stage <s>")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	mc <- fleet_map_check()
	.fl_write_csv(data.frame(checked_at = .fl_now(), exit = mc$exit,
	                         findings = mc$findings, stringsAsFactors = FALSE),
	              MAP_CHECK_STATUS)
	.fl_say("map_check: exit ", mc$exit, ", ", mc$findings,
	        " finding(s) - recorded, not blocking (D13)")
	blocked <- 0L; done <- 0L; skipped <- 0L
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		dir <- .fl_app_dir(id)
		if (!dir.exists(dir)) { skipped <- skipped + 1L; next }
		log <- file.path(dir, "log.txt")
		cat("\n=== process ", id, " [", .fl_now(), "]\nmap_check exit=", mc$exit,
		    " findings=", mc$findings, "\n", sep = "", file = log, append = TRUE)
		src <- file.path(dir, "script.qvs")
		styled <- file.path(dir, "script_styled.qvs")
		if (!file.exists(src)) {
			m <- .fl_block(m, id, "no script.qvs in the bundle")
			blocked <- blocked + 1L; next
		}
		if (!isTRUE(opts[["no-style"]])) {
			r <- .fl_child(c("styling/run_pipeline.R", src, styled, "--changes"), log)
			if (r$status != 0L) {
				m <- .fl_block(m, id, paste0("style exit ", r$status, ": ",
				                             .fl_tail_msg(r$out)))
				blocked <- blocked + 1L; next
			}
		}
		if (!file.exists(styled)) {
			m <- .fl_block(m, id, "no script_styled.qvs (--no-style with nothing styled)")
			blocked <- blocked + 1L; next
		}
		m <- manifest_upsert(m, data.frame(app_id = id, stage = "styled",
		                                   stringsAsFactors = FALSE))
		r2 <- .fl_child(c("retargeting/retarget_loads.R", styled,
		                  file.path(dir, "script_retargeted.qvs"),
		                  "--report", file.path(dir, "retarget_report.csv"),
		                  "--fields-report", file.path(dir, "retarget_fields_report.csv"),
		                  "--notes", file.path(dir, "dev_notes.txt")), log)
		if (r2$status != 0L) {
			m <- .fl_block(m, id, paste0("retarget exit ", r2$status, ": ",
			                             .fl_tail_msg(r2$out)))
			blocked <- blocked + 1L; next
		}
		unres <- .fl_unresolved_loads(dir)
		if (unres > 0L && !isTRUE(opts[["allow-unresolved"]])) {
			m <- .fl_block(m, id, paste0(unres,
			     " unresolved load(s); rerun with --allow-unresolved to proceed"))
			blocked <- blocked + 1L; next
		}
		m <- .fl_clear_error(m, id)
		m <- manifest_upsert(m, data.frame(app_id = id, stage = "retargeted",
		                                   stringsAsFactors = FALSE))
		done <- done + 1L
	}
	manifest_write(m, path)
	.fl_say(done, " retargeted, ", blocked, " blocked, ", skipped, " skipped (no bundle)")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	if (blocked > 0L) 2L else 0L
}

.fl_unresolved_loads <- function(dir) {
	p <- file.path(dir, "retarget_report.csv")
	if (!file.exists(p)) return(0L)
	r <- tryCatch(read_csv_any(p), error = function(e) NULL)
	if (is.null(r) || is.null(r$status)) return(0L)
	sum(r$status %in% FLEET_LOAD_UNRESOLVED)
}

# --- report ---------------------------------------------------------------

# The phase 2 scanners are sourced ON DEMAND, not at startup: `spaces` and
# `status` have no use for a tokenizer and every fleet verb would otherwise
# pay for one. Sourced from INSIDE a function, so each file's own
# `if (sys.nframe() == 0L) main(...)` tail cannot fire.
.FL_SCANNERS <- FALSE
.fl_need_scanners <- function() {
	if (.FL_SCANNERS) return(invisible(TRUE))
	source("shared/qlik_tokenizer.R")
	source("analysis/json_strings.R")
	source("analysis/script_debt.R")
	source("analysis/script_refs.R")
	.FL_SCANNERS <<- TRUE
	invisible(TRUE)
}

#' The feature-flag patterns (PLAN-fleet.md section 5). Adam extends this file
#' by hand and rollup grows a column per row - no code change per flag.
fleet_flags_read <- function(path = FLAGS_CSV)
	.fl_read_cols(path, c("flag", "where", "pattern", "note"))

# One reader for every small fixed-column fleet csv (flags.csv, tags.csv): a
# missing file is an EMPTY table, a missing column is filled rather than
# refused. Factored out when tags.csv needed the identical five lines - the
# twin rule (CLAUDE.md), applied in the fold-in direction.
.fl_read_cols <- function(path, cols) {
	if (!file.exists(path)) {
		d <- as.data.frame(matrix(character(0), nrow = 0L, ncol = length(cols)),
		                   stringsAsFactors = FALSE)
		names(d) <- cols
		return(d)
	}
	d <- read_csv_any(path)
	for (cn in cols) if (is.null(d[[cn]])) d[[cn]] <- rep("", nrow(d))
	d[, cols, drop = FALSE]
}

.fl_json_strings <- function(path) {
	x <- tryCatch(read_json_strings(path), error = function(e) NULL)
	if (is.null(x)) return(character(0))
	if (is.data.frame(x)) return(unlist(lapply(x, as.character), use.names = FALSE))
	as.character(x)
}

#' Flag hits for one app.
#'
#' Detection reads the SHARED scanners, never a raw-text grep (PLAN section 5):
#'   script      live (non-COMMENT) token text, regrouped per source line
#'   comments    COMMENT token text
#'   objects     string VALUES from objects/*.json + dimensions/measures/
#'               variables.json, via analysis/json_strings.R
#'   connections connections.yml read as lines - it is YAML, and this repo has
#'               no YAML scanner to reuse; said plainly rather than pretended
#'   report      COMPUTED (see .fl_unknown_src), never pattern-matched
#' The `comments` scope is this milestone's one addition to section 5's three
#' (a comma list, same shape): the nprint heuristic exists to read comments and
#' titles, while a connector flag must NOT be tripped by a commented-out line.
fleet_app_flags <- function(app_dir, script_path, flags) {
	.fl_need_scanners()
	tok <- if (file.exists(script_path))
		tryCatch(read_qlik_script(script_path), error = function(e) NULL) else NULL
	live <- character(0); cmts <- character(0)
	if (!is.null(tok) && nrow(tok)) {
		isc <- tok$type == "COMMENT"
		if (any(!isc)) live <- unname(vapply(split(tok$text[!isc], tok$line[!isc]),
		                                     function(x) paste(x, collapse = ""), character(1)))
		if (any(isc)) cmts <- unname(vapply(split(tok$text[isc], tok$line[isc]),
		                                    function(x) paste(x, collapse = " "), character(1)))
	}
	objf <- c(list.files(file.path(app_dir, "objects"), pattern = "[.]json$",
	                     full.names = TRUE),
	          file.path(app_dir, c("dimensions.json", "measures.json", "variables.json")))
	objf <- objf[file.exists(objf)]
	objs <- unlist(lapply(objf, .fl_json_strings), use.names = FALSE)
	cyml <- file.path(app_dir, "connections.yml")
	conns <- if (file.exists(cyml)) readLines(cyml, warn = FALSE) else character(0)
	pools <- list(script = live, comments = cmts, objects = objs, connections = conns)

	out <- data.frame(flag = character(0), where = character(0),
	                  count = integer(0), first_hit = character(0),
	                  stringsAsFactors = FALSE)
	for (i in seq_len(nrow(flags))) {
		fl <- flags$flag[i]; wh <- flags$where[i]; pat <- flags$pattern[i]
		if (identical(trimws(wh), "report")) {
			out <- rbind(out, data.frame(flag = fl, where = wh,
			                             count = .fl_unknown_src(app_dir),
			                             first_hit = "computed", stringsAsFactors = FALSE))
			next
		}
		pool <- unlist(pools[trimws(strsplit(wh, ",", fixed = TRUE)[[1]])], use.names = FALSE)
		hit <- if (!length(pool) || !nzchar(pat)) logical(0) else
			grepl(pat, pool, ignore.case = TRUE, perl = TRUE)
		first <- if (any(hit)) substr(trimws(pool[which(hit)[1]]), 1L, 80L) else ""
		out <- rbind(out, data.frame(flag = fl, where = wh, count = sum(hit),
		                             first_hit = first, stringsAsFactors = FALSE))
	}
	list(flags = out, tokens = tok)
}

# unknown_src is the one flag section 5 marks COMPUTED: loads with no lineage
# row or more than one source, plus tables whose field list is incomplete (an
# unresolved wildcard load hides both what is used and what exists).
.fl_unknown_src <- function(app_dir) {
	n <- 0L
	rp <- file.path(app_dir, "retarget_report.csv")
	if (file.exists(rp)) {
		r <- tryCatch(read_csv_any(rp), error = function(e) NULL)
		if (!is.null(r) && !is.null(r$status)) n <- n + sum(r$status %in% FLEET_LOAD_UNRESOLVED)
	}
	tp <- file.path(app_dir, "usage-tables.csv")
	if (file.exists(tp)) {
		t <- tryCatch(read_csv_any(tp), error = function(e) NULL)
		if (!is.null(t) && !is.null(t$complete_fields))
			n <- n + sum(toupper(t$complete_fields) == "FALSE")
	}
	as.integer(n)
}

.fl_verb_report <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("report needs --all, --apps <id,id> or --stage <s>")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	flags <- fleet_flags_read()
	blocked <- 0L; done <- 0L; skipped <- 0L
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		dir <- .fl_app_dir(id)
		if (!dir.exists(dir)) { skipped <- skipped + 1L; next }
		log <- file.path(dir, "log.txt")
		src <- file.path(dir, "script.qvs")
		styled <- file.path(dir, "script_styled.qvs")
		# The STYLED script where there is one: script_loads.R reads a produced
		# name off an explicit alias, which passes 1-2 are what guarantee.
		script <- if (file.exists(styled)) styled else src
		if (!file.exists(script)) { skipped <- skipped + 1L; next }
		cat("\n=== report ", id, " [", .fl_now(), "]\n", sep = "",
		    file = log, append = TRUE)
		r <- .fl_child(c("analysis/render_report.R", dir, "--script", script,
		                 "--out", file.path(dir, "report.html")), log)
		if (r$status != 0L) {
			m <- .fl_block(m, id, paste0("render_report exit ", r$status, ": ",
			                             .fl_tail_msg(r$out)))
			blocked <- blocked + 1L; next
		}
		r2 <- .fl_child(c("analysis/usage_report.R", dir, "--script", script,
		                  "--csv", file.path(dir, "usage")), log)
		if (r2$status != 0L) {
			m <- .fl_block(m, id, paste0("usage_report exit ", r2$status, ": ",
			                             .fl_tail_msg(r2$out)))
			blocked <- blocked + 1L; next
		}
		# In-process work, so a stop() here would take the whole batch down the
		# way a child process's failure cannot. One app's failure is one app's
		# last_error - that is the rule the rest of this verb already follows.
		fa <- tryCatch({
			f <- fleet_app_flags(dir, script, flags)
			.fl_write_csv(f$flags, file.path(dir, "flags.csv"))
			.fl_write_csv(fleet_app_summary(dir, f$tokens), file.path(dir, "summary.csv"))
			""
		}, error = function(e) conditionMessage(e))
		if (nzchar(fa)) {
			m <- .fl_block(m, id, paste0("flags/summary: ", substr(fa, 1L, 120L)))
			blocked <- blocked + 1L; next
		}
		done <- done + 1L
	}
	manifest_write(m, path)
	.fl_say(done, " reported, ", blocked, " blocked, ", skipped, " skipped (no bundle)")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	if (blocked > 0L) 2L else 0L
}

#' The two per-app counts master.csv needs that no other csv carries.
#'
#' Written by `report` rather than derived by `rollup`, because section 5's
#' rollup READS csvs: re-tokenizing a 13,869-line script inside a rollup over
#' the whole estate is work already done a moment earlier.
fleet_app_summary <- function(app_dir, tok) {
	dead <- 0L; stores <- 0L
	if (!is.null(tok) && nrow(tok)) {
		dead <- .fl_count(commented_out_code(tok))
		tp <- file.path(app_dir, "usage-tables.csv")
		if (file.exists(tp)) {
			t <- tryCatch(read_csv_any(tp), error = function(e) NULL)
			if (!is.null(t) && !is.null(t$table) && nrow(t))
				stores <- .fl_count(script_disposals(tok, unique(t$table))$stores)
		}
	}
	data.frame(dead_code_lines = as.integer(dead),
	           inapp_store_count = as.integer(stores),
	           script_lines = if (is.null(tok)) 0L else as.integer(max(c(0L, tok$line))),
	           stringsAsFactors = FALSE)
}

# --- rollup ---------------------------------------------------------------

#' Readiness, 0-100, in ONE function (D10).
#'
#' pct_retargeted minus 10 per DISTINCT blocker KIND, floored at 0. A kind is
#' the text left of the colon, so "not-in-map:3" costs the same 10 as
#' "not-in-map:1" - the number of unresolved loads is already its own column,
#' and counting it twice would make one bad app dominate the ordering.
#' pct_retargeted over ELIGIBLE loads only (D14), in ONE function.
#'
#' `status` is the retarget report's own status column, restricted to its
#' `load` rows by the caller. Eligible = not one of FLEET_LOAD_INELIGIBLE;
#' done = FLEET_LOAD_DONE. An app with loads but NO eligible one has nothing
#' left to do and scores 100 - that is D14's whole point.
#'
#' `measured` is what tells "nothing eligible" apart from "never processed":
#' an app with no retarget_report.csv at all has not been measured and scores
#' 0, not 100. Without that distinction every unfetched app in the ledger
#' would top the readiness board.
fleet_pct_retargeted <- function(status, measured = TRUE) {
	s <- as.character(status)
	s <- s[!is.na(s) & nzchar(s)]
	elig <- s[!(s %in% FLEET_LOAD_INELIGIBLE)]
	pct <- if (!isTRUE(measured)) 0L
	       else if (!length(elig)) 100L
	       else as.integer(round(100 * sum(elig %in% FLEET_LOAD_DONE) / length(elig)))
	list(total = length(s), eligible = length(elig),
	     done = sum(elig %in% FLEET_LOAD_DONE), pct = pct)
}

fleet_readiness <- function(pct_retargeted, blocker_kinds = character(0)) {
	pct <- suppressWarnings(as.integer(round(as.numeric(pct_retargeted))))
	if (is.na(pct)) pct <- 0L
	pct <- max(0L, min(100L, pct))
	k <- unique(sub(":.*$", "", blocker_kinds[nzchar(blocker_kinds)]))
	max(0L, pct - 10L * length(k))
}

.fl_num <- function(x, default = 0L) {
	v <- suppressWarnings(as.numeric(x))
	v[is.na(v)] <- default
	v
}

.fl_read <- function(path) {
	if (!file.exists(path)) return(NULL)
	tryCatch(read_csv_any(path), error = function(e) NULL)
}

.fl_verb_rollup <- function(opts = list()) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	flags <- fleet_flags_read()
	mcf <- .fl_read(MAP_CHECK_STATUS)
	mc_findings <- if (is.null(mcf) || !nrow(mcf)) "" else as.character(mcf$findings[1])

	rows <- NULL; loads <- NULL; unused <- NULL
	for (i in seq_len(nrow(m))) {
		id <- m$app_id[i]; nm <- m$app_name[i]
		dir <- .fl_app_dir(id)
		rr <- .fl_read(file.path(dir, "retarget_report.csv"))
		ut <- .fl_read(file.path(dir, "usage-tables.csv"))
		uf <- .fl_read(file.path(dir, "usage-fields.csv"))
		uv <- .fl_read(file.path(dir, "usage-vars.csv"))
		sm <- .fl_read(file.path(dir, "summary.csv"))
		af <- .fl_read(file.path(dir, "flags.csv"))

		ld <- if (is.null(rr) || is.null(rr$status)) character(0) else rr$status
		kind <- if (is.null(rr) || is.null(rr$kind)) character(0) else rr$kind
		n_total <- sum(kind == "load")
		n_ok <- sum(ld %in% FLEET_LOAD_OK)
		n_nim <- sum(ld == "not-in-map")
		n_multi <- sum(ld == "multi-source")
		n_cmt <- sum(ld == "commented")
		n_wild <- sum(ld == "wildcard")
		# D14: the denominator is ELIGIBLE loads. loads_total stays the raw
		# count of `load` rows; loads_eligible is the number pct is over.
		ldl <- if (length(kind) == length(ld)) ld[kind == "load"] else ld
		pc <- fleet_pct_retargeted(ldl, measured = !is.null(rr))
		pct <- pc$pct

		t_total <- if (is.null(ut)) 0L else nrow(ut)
		t_unused <- if (is.null(ut) || is.null(ut$category)) 0L else sum(ut$category == "unreferenced")
		f_pct <- if (is.null(uf) || is.null(uf$field_finding) || !nrow(uf)) 0L else
			round(100 * sum(toupper(uf$field_finding) == "TRUE") / nrow(uf))
		vkind <- if (is.null(uv) || is.null(uv$kind)) character(0) else uv$kind
		vused <- if (is.null(uv) || is.null(uv$used)) character(0) else toupper(uv$used)
		v_total <- sum(vkind == "variable")
		v_unused <- sum(vkind == "variable" & vused == "FALSE")
		d_unused <- sum(vkind == "dimension" & vused == "FALSE")
		me_unused <- sum(vkind == "measure" & vused == "FALSE")

		blockers <- character(0)
		if (n_nim > 0L) blockers <- c(blockers, paste0("not-in-map:", n_nim))
		if (n_multi > 0L) blockers <- c(blockers, paste0("multi-source:", n_multi))
		if (n_wild > 0L) blockers <- c(blockers, paste0("wildcard:", n_wild))
		if (identical(m$stage[i], "blocked")) blockers <- c(blockers, "blocked")

		row <- data.frame(
			app_id = id, app_name = nm, space_name = m$space_name[i],
			stage = m$stage[i], stage_at = m$stage_at[i],
			target_app_id = m$target_app_id[i],
			loads_total = n_total, loads_eligible = pc$eligible,
			loads_retargeted = n_ok,
			loads_not_in_map = n_nim, loads_multi_source = n_multi,
			loads_commented = n_cmt, loads_wildcard = n_wild,
			pct_retargeted = pct,
			tables_total = t_total, tables_unused = t_unused,
			fields_unused_pct = f_pct, vars_total = v_total, vars_unused = v_unused,
			dims_unused = d_unused, measures_unused = me_unused,
			dead_code_lines = if (is.null(sm)) 0L else as.integer(.fl_num(sm$dead_code_lines[1])),
			inapp_store_count = if (is.null(sm)) 0L else as.integer(.fl_num(sm$inapp_store_count[1])),
			stringsAsFactors = FALSE)

		# One column per row of flags.csv, named flag_<name>. A HEURISTIC flag
		# (its note says so) is reported but is NOT a blocker and costs no
		# readiness: NPrinting lives outside the app and this is a hint, never
		# proof - section 5 says so in as many words.
		for (j in seq_len(nrow(flags))) {
			fn <- paste0("flag_", flags$flag[j])
			cnt <- 0L
			if (!is.null(af) && !is.null(af$flag)) {
				k <- which(af$flag == flags$flag[j])
				if (length(k)) cnt <- as.integer(.fl_num(af$count[k[1]]))
			}
			row[[fn]] <- cnt
			if (cnt > 0L && !startsWith(toupper(flags$note[j]), "HEURISTIC"))
				blockers <- c(blockers, flags$flag[j])
		}
		row$blockers <- paste(blockers, collapse = ";")
		row$readiness <- fleet_readiness(pct, blockers)
		row$map_check_findings <- mc_findings
		row$last_error <- m$last_error[i]
		row$notes <- m$notes[i]
		rows <- rbind(rows, row)

		if (!is.null(rr) && nrow(rr))
			loads <- rbind(loads, data.frame(
				app_id = id, app_name = nm,
				line = rr$line, onprem_qvd = rr$old_path, status = rr$status,
				cloud_target = rr$new_path, note = rr$detail,
				stringsAsFactors = FALSE))

		if (!is.null(ut) && !is.null(ut$category)) {
			k <- which(ut$category == "unreferenced")
			if (length(k)) unused <- rbind(unused, data.frame(
				app_id = id, app_name = nm, kind = "table", name = ut$table[k],
				detail = paste0(ut$n_fields[k], " fields, lines ",
				                ut$line_start[k], "-", ut$line_end[k]),
				stringsAsFactors = FALSE))
		}
		if (!is.null(uf) && !is.null(uf$field_finding)) {
			k <- which(toupper(uf$field_finding) == "TRUE")
			if (length(k)) unused <- rbind(unused, data.frame(
				app_id = id, app_name = nm, kind = "field", name = uf$field[k],
				detail = paste0("table ", uf$table[k]), stringsAsFactors = FALSE))
		}
		if (!is.null(uv) && !is.null(uv$used)) {
			k <- which(toupper(uv$used) == "FALSE")
			if (length(k)) unused <- rbind(unused, data.frame(
				app_id = id, app_name = nm, kind = uv$kind[k], name = uv$name[k],
				detail = uv$id[k], stringsAsFactors = FALSE))
		}
	}
	if (is.null(rows)) rows <- data.frame(app_id = character(0), app_name = character(0),
	                                      stringsAsFactors = FALSE)
	if (nrow(rows) > 1L)
		rows <- rows[order(-rows$readiness, rows$app_name), , drop = FALSE]
	.fl_write_csv(rows, MASTER_CSV)
	.fl_write_csv(if (is.null(loads)) data.frame(
		app_id = character(0), app_name = character(0), line = character(0),
		onprem_qvd = character(0), status = character(0),
		cloud_target = character(0), note = character(0),
		stringsAsFactors = FALSE) else loads, MASTER_LOADS_CSV)
	.fl_write_csv(if (is.null(unused)) data.frame(
		app_id = character(0), app_name = character(0), kind = character(0),
		name = character(0), detail = character(0),
		stringsAsFactors = FALSE) else unused, MASTER_UNUSED_CSV)
	.fl_say("rollup: ", nrow(rows), " app(s), ",
	        if (is.null(loads)) 0L else nrow(loads), " load row(s), ",
	        if (is.null(unused)) 0L else nrow(unused), " unused row(s)")
	0L
}

# =========================================================================
# M2: fetch (unbuild from the tenant) and reconcile-ids (D12).
# PLAN-fleet.md section 3. Every tenant call goes through fleet/qlik_api.R,
# so the whole of this section is provable against diagnostics/mock_qlik.bat.
# =========================================================================

#' `fetch` - unbuild every selected app from the tenant into fleet/apps/<id>/.
#'
#' Normally driven by `--stage listed`, which is the stage `add` leaves a row
#' at; any selection is accepted, because a re-fetch of an app already further
#' along is legitimate (the stage enum allows a backward move, DESIGN 8.6).
#'
#' DRY_RUN is honoured as a FETCH AUDIT, not as a write: qc_unbuild() prints
#' the exact command line and writes a DRYRUN audit line, nothing is
#' downloaded, and - the part that matters - the ledger is NOT touched, so a
#' rehearsal cannot advance a single row.
#'
#' Unbuilt into a STAGING directory and copied in on success. qc_unbuild()
#' deletes the directory it was given when a fetch fails, so unbuilding
#' straight into fleet/apps/<id>/ would take an existing bundle's styled
#' script, reports and log down with a failed re-fetch. The destination is
#' still section 3's fleet/apps/<id>/; only the failure path differs.
.fl_verb_fetch <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("fetch needs --all, --apps <id,id> or --stage <s>",
		         " (usually --stage listed)")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	dry <- .qc_dry()
	done <- 0L; blocked <- 0L; skipped <- 0L; planned <- 0L
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		# A `local:` key is D12's placeholder: there is no tenant app behind
		# it, so this is a skip with a reason, never a blocked row.
		if (!nzchar(id) || startsWith(id, "local:")) {
			.fl_warn(id, ": no tenant app id - run 'fleet.R reconcile-ids' first")
			skipped <- skipped + 1L
			next
		}
		stage_dir <- file.path(tempdir(), paste0("fetch-", .fl_slug(id)))
		unlink(stage_dir, recursive = TRUE)
		r <- qc_unbuild(id, stage_dir)
		if (dry) { planned <- planned + 1L; next }
		if (qc_failed(r)) {
			m <- .fl_block(m, id, paste0("fetch: ", .fl_fail_msg(r)))
			blocked <- blocked + 1L
			next
		}
		dest <- .fl_app_dir(id)
		if (!dir.exists(dest)) dir.create(dest, recursive = TRUE, showWarnings = FALSE)
		file.copy(list.files(stage_dir, full.names = TRUE), dest,
		          recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
		unlink(stage_dir, recursive = TRUE)
		props <- tryCatch(json_read(file.path(dest, "app-properties.json")),
		                  error = function(e) NULL)
		title <- .fl_str(if (is.null(props)) NULL else props[["qTitle"]],
		                 sel$app_name[i])
		writeLines(title, file.path(dest, "name.txt"), useBytes = TRUE)
		m <- .fl_clear_error(m, id)
		m <- manifest_upsert(m, data.frame(app_id = id, app_name = title,
		                                   stage = "unbuilt",
		                                   stringsAsFactors = FALSE))
		done <- done + 1L
	}
	if (dry) {
		.fl_say(planned, " app(s) would be fetched, ", skipped,
		        " skipped (no tenant id); ledger untouched. Re-run with --live.")
		return(0L)
	}
	manifest_write(m, path)
	.fl_say(done, " fetched, ", blocked, " blocked, ", skipped,
	        " skipped (no tenant id)")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	if (blocked > 0L) 2L else 0L
}

#' One manifest row's app_name against a tenant listing (D12).
#'
#' EXACT match on the name, and nothing else: the tenant has 578 duplicate app
#' names, so a fuzzy or case-folded match would silently hand an app another
#' app's id. A non-unique match is a NOTE on the row, never a guess.
fleet_reconcile_match <- function(app_name, listing) {
	miss <- list(status = "none", n = 0L, id = "", space_id = "", space_name = "")
	if (is.null(listing) || !nrow(listing) || !nzchar(app_name)) return(miss)
	k <- which(listing$name == app_name)
	if (!length(k)) return(miss)
	if (length(k) > 1L)
		return(list(status = "ambiguous", n = length(k), id = "",
		            space_id = "", space_name = ""))
	list(status = "unique", n = 1L, id = listing$id[k],
	     space_id = listing$space_id[k], space_name = listing$space_name[k])
}

# `app ls` over each space in turn, stacked into one listing with the space
# each row came from. A space that fails to list is warned about and skipped:
# a partial listing can only produce "no match", which is already a safe
# outcome here, whereas aborting would strand every other space's matches.
.fl_tenant_listing <- function(space_ids, spaces = NULL) {
	out <- NULL
	for (sid in space_ids) {
		df <- .fl_app_rows(sid)
		if (qc_failed(df)) {
			.fl_warn("app ls failed for space ", sid, ": ", .fl_fail_msg(df))
			next
		}
		if (!nrow(df)) next
		snm <- if (!is.null(spaces) && !qc_failed(spaces) && sid %in% spaces$id)
			spaces$name[match(sid, spaces$id)] else ""
		out <- rbind(out, data.frame(id = df$id, name = df$name, space_id = sid,
		                             space_name = snm, stringsAsFactors = FALSE))
	}
	out
}

#' `reconcile-ids` - give D12's `local:` rows their real tenant ids.
#'
#' Reads only (`app ls`, `item ls`); the ledger and the app directory are
#' local, so there is no tenant write here and DRY_RUN does not gate it.
.fl_verb_reconcile_ids <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	local <- which(startsWith(m$app_id, "local:"))
	if (!length(local)) {
		.fl_say("no local: ids in ", path, " - nothing to reconcile")
		return(0L)
	}
	sp <- .fl_resolve_space(.fl_opt(opts, "space"))
	sids <- if (!is.null(sp)) sp else unique(m$space_id[nzchar(m$space_id)])
	if (!length(sids)) {
		.fl_warn("no space to search: the manifest knows no space_id yet, ",
		         "so pass --space <id|index from 'spaces'>")
		return(1L)
	}
	spaces <- .fl_space_rows()
	listing <- .fl_tenant_listing(sids, spaces)
	if (is.null(listing) || !nrow(listing)) {
		.fl_warn("the tenant listing came back empty - nothing to match against")
		return(1L)
	}
	res <- lapply(local, function(k) fleet_reconcile_match(m$app_name[k], listing))
	uniq <- vapply(res, function(r) identical(r$status, "unique"), logical(1))
	ids <- vapply(res, function(r) r$id, character(1))
	items <- if (any(uniq)) .fl_item_ids(ids[uniq]) else character(0)
	n_ok <- 0L; n_amb <- 0L; n_none <- 0L
	for (j in seq_along(local)) {
		k <- local[j]
		r <- res[[j]]
		if (!identical(r$status, "unique")) {
			note <- if (identical(r$status, "ambiguous"))
				paste0("reconcile-ids: ", r$n, " tenant apps share this name")
			else "reconcile-ids: no tenant app with this exact name"
			m$notes[k] <- note
			.fl_warn(m$app_id[k], ": ", note)
			if (identical(r$status, "ambiguous")) n_amb <- n_amb + 1L
			else n_none <- n_none + 1L
			next
		}
		if (any(m$app_id == r$id)) {
			m$notes[k] <- "reconcile-ids: that tenant id is already in the ledger"
			.fl_warn(m$app_id[k], ": matched id ", r$id, " already present")
			n_amb <- n_amb + 1L
			next
		}
		old_dir <- .fl_app_dir(m$app_id[k])
		new_dir <- .fl_app_dir(r$id)
		if (dir.exists(old_dir) && !dir.exists(new_dir)) file.rename(old_dir, new_dir)
		m$app_id[k] <- r$id
		m$item_id[k] <- if (r$id %in% names(items)) items[[r$id]] else ""
		m$space_id[k] <- r$space_id
		m$space_name[k] <- r$space_name
		m$notes[k] <- ""
		n_ok <- n_ok + 1L
	}
	manifest_write(m, path)
	.fl_say(n_ok, " id(s) resolved, ", n_amb, " ambiguous, ", n_none, " unmatched")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	0L
}

# =========================================================================
# M4: tag stamping. PLAN-fleet.md section 7 (D4 = a).
# A Qlik Cloud tag IS a public collection, so `mig:<stage>` is a collection
# and stamping an app is adding its ITEM (item id != app id) to one.
# =========================================================================

TAGS_CSV <- file.path(FLEET_DIR, "tags.csv")
TAG_DRIFT_CSV <- file.path(FLEET_DIR, "tag_drift.csv")
FLEET_TAG_PREFIX <- "mig:"

#' The tag that names a stage. Section 7's scheme, one place.
fleet_tag_for <- function(stage) paste0(FLEET_TAG_PREFIX, stage)

#' The tag -> collection id cache (fleet/tags.csv). A missing file is empty.
fleet_tags_read <- function(path = TAGS_CSV)
	.fl_read_cols(path, c("tag", "collection_id", "cached_at"))

# The id of the public collection with this exact name, or "" - a READ, so it
# works under DRY_RUN and is what makes the 409 recovery below possible.
.fl_collection_id <- function(name) {
	items <- qc_pages(c("collection", "ls"))
	if (qc_failed(items)) return("")
	for (it in items)
		if (identical(.fl_str(it[["name"]]), name)) return(.fl_str(it[["id"]]))
	""
}

.fl_tag_cache <- function(tags, tag, id) {
	k <- which(tags$tag == tag)
	if (length(k)) {
		tags$collection_id[k[1]] <- id
		tags$cached_at[k[1]] <- .fl_now()
		return(tags)
	}
	rbind(tags, data.frame(tag = tag, collection_id = id, cached_at = .fl_now(),
	                       stringsAsFactors = FALSE))
}

#' The collection id behind a tag, creating the collection on first use.
#'
#' Returns list(id, tags, created); `tags` is the cache to carry on with, so a
#' caller stamping 200 apps reads `collection ls` once per NEW tag, not once
#' per app. The cache is a convenience and never a dependency - a miss simply
#' re-reads the listing.
#'
#' A create that comes back 409 is NOT a failure: the name already exists
#' (someone made it by hand, or two operators raced), so the listing is re-read
#' and the existing id used. Section 7 asks for exactly that. Under DRY_RUN
#' qc_write() runs nothing and there is no id to have - the id comes back ""
#' and the caller prints the create rather than inventing one.
fleet_tag_id <- function(tag, tags = fleet_tags_read(), create = TRUE) {
	k <- which(tags$tag == tag)
	if (length(k) && nzchar(tags$collection_id[k[1]]))
		return(list(id = tags$collection_id[k[1]], tags = tags, created = FALSE))
	found <- .fl_collection_id(tag)
	created <- FALSE
	if (!nzchar(found) && isTRUE(create)) {
		r <- qc_write(c("collection", "create", "--name", tag, "--type", "public"),
		              label = paste("collection create", tag))
		if (is.null(r)) return(list(id = "", tags = tags, created = FALSE))
		if (qc_failed(r)) {
			found <- .fl_collection_id(tag)
			if (!nzchar(found)) {
				.fl_warn("collection create ", tag, " failed: ", .fl_fail_msg(r))
				return(list(id = "", tags = tags, created = FALSE))
			}
		} else {
			found <- .fl_str(if (!is.null(r[["id"]])) r[["id"]]
			                 else json_get(r, "attributes", "id"))
			created <- TRUE
		}
	}
	if (!nzchar(found)) return(list(id = "", tags = tags, created = FALSE))
	list(id = found, tags = .fl_tag_cache(tags, tag, found), created = created)
}

#' The collections one item is in, as data.frame(id, name), or a qc_error.
fleet_item_tags <- function(item_id) {
	r <- qc(c("item", "collections", item_id))
	if (qc_failed(r)) return(r)
	items <- qc_items(r)
	if (!length(items)) return(data.frame(id = character(0), name = character(0),
	                                      stringsAsFactors = FALSE))
	data.frame(id = vapply(items, function(it) .fl_str(it[["id"]]), character(1)),
	           name = vapply(items, function(it) .fl_str(it[["name"]]), character(1)),
	           stringsAsFactors = FALSE)
}

#' What to add and what to remove so one item carries exactly one mig: tag.
#'
#' Exclusivity is scoped to the mig: prefix and nothing else. An item's other
#' collections are somebody's curation of the estate and are none of this
#' tool's business - removing them would be a silent, unrecoverable edit to
#' another team's work.
fleet_stamp_plan <- function(current_names, want) {
	cur <- current_names[startsWith(current_names, FLEET_TAG_PREFIX)]
	list(add = if (want %in% cur) character(0) else want,
	     remove = setdiff(cur, want))
}

#' Which stage an item's tags name, or "" when there is no mig: tag or more
#' than one. Two mig: tags is exactly the drift `reconcile` exists to report,
#' so it must not resolve to whichever came first.
fleet_tag_stage <- function(tag_names) {
	t <- tag_names[startsWith(tag_names, FLEET_TAG_PREFIX)]
	if (length(t) != 1L) return("")
	sub(paste0("^", FLEET_TAG_PREFIX), "", t[1])
}

#' Do the local artefacts a stage PROMISES actually exist? The --adopt guard
#' (section 7): a tag is a label somebody can set by hand, so believing one
#' without the files behind it would let the ledger claim work nobody did.
fleet_stage_artefacts <- function(app_dir, stage, target_app_id = "") {
	f <- switch(stage,
	            listed = "",
	            unbuilt = "script.qvs",
	            styled = "script_styled.qvs",
	            retargeted = "script_retargeted.qvs",
	            built = "script_retargeted.qvs",
	            verified = "script_retargeted.qvs",
	            NA_character_)
	if (is.na(f)) return(FALSE)
	if (stage %in% c("built", "verified") && !nzchar(as.character(target_app_id)))
		return(FALSE)
	!nzchar(f) || file.exists(file.path(app_dir, f))
}

.fl_verb_stamp <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("stamp needs --all, --apps <id,id> or --stage <s>")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	tags <- fleet_tags_read()
	dry <- .qc_dry()
	n_ok <- 0L; n_add <- 0L; n_rm <- 0L; n_skip <- 0L
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		item <- .fl_str(sel$item_id[i])
		if (!nzchar(item)) {
			.fl_warn(id, ": no item_id - run 'add' or 'reconcile-ids' first")
			n_skip <- n_skip + 1L
			next
		}
		want <- fleet_tag_for(sel$stage[i])
		cur <- fleet_item_tags(item)
		if (qc_failed(cur)) {
			.fl_warn(id, ": item collections failed: ", .fl_fail_msg(cur))
			n_skip <- n_skip + 1L
			next
		}
		plan <- fleet_stamp_plan(cur$name, want)
		for (nm in plan$remove) {
			qc_write(c("collection", "item", "rm", item, "--collectionId",
			           cur$id[match(nm, cur$name)]),
			         json = FALSE, label = paste("untag", id, nm))
			n_rm <- n_rm + 1L
		}
		if (length(plan$add)) {
			g <- fleet_tag_id(want, tags)
			tags <- g$tags
			if (!nzchar(g$id)) {
				# Live, no id means the create genuinely failed. Dry, it only
				# means the collection does not exist yet - print the step.
				if (!dry) {
					.fl_warn(id, ": no collection id for ", want)
					n_skip <- n_skip + 1L
					next
				}
				cat("DRY RUN $ qlik collection item create --collectionId <new ",
				    want, "> --id ", item, "\n", sep = "")
			} else {
				qc_write(c("collection", "item", "create", "--collectionId", g$id,
				           "--id", item),
				         json = FALSE, label = paste("tag", id, want))
			}
			n_add <- n_add + 1L
		}
		n_ok <- n_ok + 1L
	}
	if (!dry) .fl_write_csv(tags, TAGS_CSV)
	.fl_say(if (dry) "PLAN: " else "", n_ok, " item(s), ", n_add, " tag(s) to add, ",
	        n_rm, " to remove, ", n_skip, " skipped")
	0L
}

#' `reconcile` - report where the tenant's tag and the ledger's stage differ.
#'
#' The report is a FILE (fleet/tag_drift.csv); the console lines are the photo
#' fallback. `--adopt` moves the manifest FORWARD to the tag's stage, and only
#' when the local artefacts that stage promises are actually here and the move
#' is not a forward skip. Everything it declines is written down with the
#' reason, so a refusal is visible rather than silent.
.fl_verb_reconcile <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("reconcile needs --all, --apps <id,id> or --stage <s>")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	adopt <- isTRUE(opts[["adopt"]])
	rows <- NULL
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		item <- .fl_str(sel$item_id[i])
		tag_stage <- ""
		action <- ""
		if (!nzchar(item)) {
			action <- "no item_id"
		} else {
			cur <- fleet_item_tags(item)
			if (qc_failed(cur)) action <- paste0("item collections failed: ",
			                                     .fl_fail_msg(cur))
			else tag_stage <- fleet_tag_stage(cur$name)
		}
		if (!nzchar(action)) {
			if (!nzchar(tag_stage)) action <- "no single mig: tag"
			else if (identical(tag_stage, sel$stage[i])) action <- "agree"
			else if (!adopt) action <- "drift"
			else if (!is.na(stage_rank(sel$stage[i])) && !is.na(stage_rank(tag_stage)) &&
			         stage_rank(tag_stage) < stage_rank(sel$stage[i]))
				action <- "adopt refused: the tag is behind the ledger"
			else if (!fleet_stage_artefacts(.fl_app_dir(id), tag_stage,
			                                sel$target_app_id[i]))
				action <- "adopt refused: no local artefacts for that stage"
			else if (!stage_advance_ok(sel$stage[i], tag_stage))
				action <- "adopt refused: would skip forward"
			else {
				m <- manifest_upsert(m, data.frame(app_id = id, stage = tag_stage,
				                                   stringsAsFactors = FALSE))
				action <- "adopted"
			}
		}
		rows <- rbind(rows, data.frame(app_id = id, app_name = sel$app_name[i],
		                               item_id = item,
		                               manifest_stage = sel$stage[i],
		                               tag_stage = tag_stage, action = action,
		                               stringsAsFactors = FALSE))
	}
	.fl_write_csv(rows, TAG_DRIFT_CSV)
	if (adopt) manifest_write(m, path)
	.fl_rule()
	for (i in which(rows$action != "agree"))
		.fl_say(sprintf("%-11s %-10s tag=%-10s %s", substr(rows$app_id[i], 1L, 11L),
		                rows$manifest_stage[i],
		                if (nzchar(rows$tag_stage[i])) rows$tag_stage[i] else "-",
		                rows$action[i]))
	.fl_rule()
	.fl_say(nrow(rows), " item(s) checked, ",
	        sum(rows$action %in% c("drift", "adopted")), " differing; ", TAG_DRIFT_CSV)
	0L
}

# =========================================================================
# M3: upload (copy or overwrite) and verify. PLAN-fleet.md section 3, and
# decisions D2 (copy into a target space, name suffix " [mig]"), D3a (never
# reload in v1 - every build carries --no-reload), D5a (the SCRIPT only, no
# --connections/--objects/--dimensions/--measures/--variables) and D7a (an
# unresolved load stops the upload unless --force).
#
# Every tenant write goes through qc_write(), so a dry run prints the exact
# command lines, writes a DRYRUN audit line and touches neither the tenant
# nor the ledger - the same rule `fetch` follows, and the reason the first
# real run of this verb can be reviewed on paper.
# =========================================================================

UPLOAD_SUFFIX <- " [mig]"                      # D2
UPLOAD_MODES <- c("copy", "overwrite")

#' The copied app's name (D2). Idempotent: a name that already carries the
#' suffix does not collect a second one on a re-upload.
fleet_upload_target_name <- function(app_name, suffix = UPLOAD_SUFFIX) {
	nm <- .fl_str(app_name)
	if (endsWith(nm, suffix)) nm else paste0(nm, suffix)
}

#' Everything that must be true before one app is uploaded (section 3).
#'
#' Returns list(ok, reason, space, script). A refusal is a REASON, never a
#' stop(): the batch prints it, leaves the row exactly where it was and
#' carries on to the next app. The unresolved-load rule is D7a - not-in-map
#' and multi-source loads mean the script still points somewhere on prem, so
#' uploading it would publish a broken app; --force is the deliberate override.
fleet_upload_preflight <- function(row, app_dir, mode = "copy",
                                   to_space = NULL, force = FALSE) {
	no <- function(why) list(ok = FALSE, reason = why, space = "", script = "")
	id <- .fl_str(row$app_id)
	if (!nzchar(id) || startsWith(id, "local:"))
		return(no("no tenant app id - run 'fleet.R reconcile-ids' first"))
	if (!identical(.fl_str(row$stage), "retargeted"))
		return(no(paste0("stage is ", .fl_str(row$stage, "(none)"),
		                 ", not retargeted")))
	script <- file.path(app_dir, "script_retargeted.qvs")
	if (!file.exists(script) || file.size(script) <= 0)
		return(no("no script_retargeted.qvs, or it is empty"))
	unres <- .fl_unresolved_loads(app_dir)
	if (unres > 0L && !isTRUE(force))
		return(no(paste0(unres, " unresolved load(s) (D7) - rerun with --force",
		                 " to upload anyway")))
	space <- .fl_str(to_space, .fl_str(row$space_id))
	if (identical(mode, "copy") && !nzchar(space))
		return(no("no target space - pass --to-space <id>"))
	list(ok = TRUE, reason = "", space = space, script = script)
}

#' The `lib://<connection>` names a retargeted script needs, deduplicated.
#'
#' Text, not tokens, and deliberately so (CLAUDE.md: the token-stream rule is
#' the STYLING pipeline's; a phase 3 tool picks its substrate). The script is
#' never printed - only the connection names are.
fleet_script_connections <- function(path) {
	if (!file.exists(path)) return(character(0))
	txt <- readLines(path, warn = FALSE)
	m <- unlist(regmatches(txt, gregexpr("lib://[^'\"|)]+", txt, ignore.case = TRUE)),
	            use.names = FALSE)
	if (!length(m)) return(character(0))
	nm <- trimws(sub("[/:].*$", "", sub("^lib://", "", m, ignore.case = TRUE)))
	sort(unique(nm[nzchar(nm)]))
}

#' The data connections a space holds, or NULL when the CLI cannot say.
#'
#' `data-connection ls --spaceId` is INFERRED (DESIGN 8.7): it is in the
#' published reference and has not been run against the installed build. NULL
#' is therefore a real answer here - "could not ask" - and the caller warns
#' rather than blocking on it.
fleet_space_connections <- function(space_id) {
	if (!nzchar(space_id)) return(NULL)
	r <- qc_pages(c("data-connection", "ls", "--spaceId", space_id))
	if (qc_failed(r)) return(NULL)
	nm <- vapply(r, function(x) .fl_str(json_get(x, "qName"),
	                                    .fl_str(json_get(x, "name"))), character(1))
	nm[nzchar(nm)]
}

# A SOFT check (section 3 marks it inferred): a missing connection is worth
# saying out loud before a build, and is never a reason to refuse one - the
# lookup itself is unverified, and a false refusal would be worse than a
# false alarm.
.fl_conn_warn <- function(script, space_id) {
	want <- fleet_script_connections(script)
	if (!length(want) || !nzchar(space_id)) return(invisible(character(0)))
	have <- fleet_space_connections(space_id)
	if (is.null(have)) {
		.fl_say("  connection check skipped (data-connection ls unavailable)")
		return(invisible(character(0)))
	}
	miss <- want[!(tolower(want) %in% tolower(have))]
	if (length(miss))
		.fl_warn("data connection(s) not in the target space: ",
		         paste(miss, collapse = ", "), " - warning only, not a block")
	invisible(miss)
}

#' `upload` - copy the app (D2) or overwrite it, then build the retargeted
#' script onto the target. Never a reload (D3a), never anything but the
#' script (D5a).
.fl_verb_upload <- function(opts) {
	mode <- .fl_opt(opts, "mode", "copy")
	if (!mode %in% UPLOAD_MODES) {
		.fl_warn("--mode must be copy or overwrite (got: ", mode, ")")
		return(1L)
	}
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("upload needs --all, --apps <id,id> or --stage <s>",
		         " (usually --stage retargeted)")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	dry <- .qc_dry()
	force <- isTRUE(opts[["force"]])
	to_space <- .fl_opt(opts, "to-space")
	done <- 0L; blocked <- 0L; refused <- 0L; planned <- 0L
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		dir <- .fl_app_dir(id)
		pf <- fleet_upload_preflight(sel[i, , drop = FALSE], dir, mode, to_space, force)
		if (!pf$ok) {
			.fl_warn(id, ": ", pf$reason)
			refused <- refused + 1L
			next
		}
		.fl_conn_warn(pf$script, pf$space)
		target <- id
		nm <- fleet_upload_target_name(sel$app_name[i])
		if (identical(mode, "copy")) {
			r <- qc_write(c("app", "copy", id, "--attributes-spaceId", pf$space,
			                "--attributes-name", nm), label = paste("copy", id))
			if (dry) {
				# The build's --app is the id the copy would return, which does
				# not exist yet. Printed as a placeholder rather than invented.
				cat("DRY RUN $ qlik ",
				    paste(shQuote(c("app", "build", "--app", "<id of the new copy>",
				                    "--script", pf$script, "--no-reload", "--silent")),
				          collapse = " "), "\n", sep = "")
				planned <- planned + 1L
				next
			}
			if (qc_failed(r)) {
				m <- .fl_block(m, id, paste0("copy: ", .fl_fail_msg(r)))
				blocked <- blocked + 1L
				next
			}
			target <- .fl_str(json_get(r, "attributes", "id"),
			                  .fl_str(json_get(r, "id")))
			if (!nzchar(target)) {
				m <- .fl_block(m, id, "copy: no app id in the reply")
				blocked <- blocked + 1L
				next
			}
			# WRITTEN BEFORE THE BUILD, and flushed to disk. A build that fails
			# after a successful copy has still created an app on the tenant;
			# a target id nobody recorded is an orphan nobody can find again.
			m <- manifest_upsert(m, data.frame(app_id = id, target_app_id = target,
			                                   target_name = nm,
			                                   stringsAsFactors = FALSE))
			manifest_write(m, path)
		}
		rb <- qc_write(c("app", "build", "--app", target, "--script", pf$script,
		                 "--no-reload", "--silent"), json = FALSE,
		               label = paste("build", target))
		if (dry) { planned <- planned + 1L; next }
		if (qc_failed(rb)) {
			m <- .fl_block(m, id, paste0("build: ", .fl_fail_msg(rb)))
			blocked <- blocked + 1L
			next
		}
		m <- .fl_clear_error(m, id)
		m <- manifest_upsert(m, data.frame(app_id = id, stage = "built",
		                                   target_app_id = target,
		                                   target_name = if (identical(mode, "copy")) nm
		                                                 else .fl_str(sel$app_name[i]),
		                                   stringsAsFactors = FALSE))
		done <- done + 1L
	}
	if (dry) {
		.fl_say(planned, " app(s) would be uploaded (--mode ", mode, "), ", refused,
		        " refused by preflight; ledger untouched. Re-run with --live.")
		return(if (refused > 0L) 2L else 0L)
	}
	manifest_write(m, path)
	.fl_say(done, " built, ", blocked, " blocked, ", refused, " refused by preflight")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	if (blocked > 0L || refused > 0L) 2L else 0L
}

#' One script file as compared text: CRLF normalised, trailing blank lines
#' dropped. A round trip through the tenant may legitimately change either,
#' and neither is a difference in what the script SAYS.
fleet_read_script <- function(path) {
	if (!file.exists(path)) return(character(0))
	x <- sub("\r$", "", readLines(path, warn = FALSE))
	while (length(x) && !nzchar(x[length(x)])) x <- x[-length(x)]
	x
}

#' What `verify` says about a target app's script versus the local one.
#'
#' Returns list(same, summary). The summary is COUNTS AND A LINE NUMBER only -
#' never a line of script. This runs on a console whose return channel is a
#' photograph (DESIGN 8.5), and app scripts are the customer's data.
fleet_script_diff <- function(downloaded, local) {
	a <- fleet_read_script(downloaded)
	b <- fleet_read_script(local)
	if (identical(a, b))
		return(list(same = TRUE,
		            summary = paste0("identical, ", length(a), " lines")))
	n <- min(length(a), length(b))
	k <- if (n > 0L) which(a[seq_len(n)] != b[seq_len(n)]) else integer(0)
	first <- if (length(k)) k[1] else n + 1L
	list(same = FALSE,
	     summary = paste0("differs: tenant ", length(a), " lines, local ",
	                      length(b), " lines, first difference at line ", first))
}

#' `verify` - unbuild the uploaded app again and compare its script with the
#' one we sent. The proof that the upload did what it said.
.fl_verb_verify <- function(opts) {
	path <- .fl_opt(opts, "manifest", MANIFEST_DEFAULT)
	m <- manifest_read(path)
	sel <- .fl_select(m, opts)
	if (is.null(sel)) {
		.fl_warn("verify needs --all, --apps <id,id> or --stage <s>",
		         " (usually --stage built)")
		return(1L)
	}
	if (!nrow(sel)) { .fl_say("nothing selected"); return(0L) }
	dry <- .qc_dry()
	good <- 0L; bad <- 0L; skipped <- 0L; planned <- 0L
	for (i in seq_len(nrow(sel))) {
		id <- sel$app_id[i]
		target <- .fl_str(sel$target_app_id[i])
		local <- file.path(.fl_app_dir(id), "script_retargeted.qvs")
		if (!nzchar(target)) {
			.fl_warn(id, ": no target_app_id - run 'fleet.R upload' first")
			skipped <- skipped + 1L
			next
		}
		if (!file.exists(local)) {
			.fl_warn(id, ": no script_retargeted.qvs to compare against")
			skipped <- skipped + 1L
			next
		}
		tmp <- file.path(tempdir(), paste0("verify-", .fl_slug(target)))
		unlink(tmp, recursive = TRUE)
		r <- qc_unbuild(target, tmp)
		if (dry) { planned <- planned + 1L; next }
		if (qc_failed(r)) {
			m <- .fl_block(m, id, paste0("verify: unbuild ", .fl_fail_msg(r)))
			bad <- bad + 1L
			next
		}
		d <- fleet_script_diff(file.path(tmp, "script.qvs"), local)
		unlink(tmp, recursive = TRUE)
		.fl_say(substr(id, 1L, 20L), ": ", d$summary)
		if (!d$same) {
			m <- .fl_block(m, id, paste0("verify: ", d$summary))
			bad <- bad + 1L
			next
		}
		m <- .fl_clear_error(m, id)
		m <- manifest_upsert(m, data.frame(app_id = id, stage = "verified",
		                                   stringsAsFactors = FALSE))
		good <- good + 1L
	}
	if (dry) {
		.fl_say(planned, " app(s) would be verified, ", skipped,
		        " skipped; ledger untouched. Re-run with --live.")
		return(0L)
	}
	manifest_write(m, path)
	.fl_say(good, " verified, ", bad, " blocked, ", skipped, " skipped")
	if (!isTRUE(opts[["no-rollup"]])) .fl_verb_rollup(opts)
	if (bad > 0L) 2L else 0L
}

.FL_TODO <- c("map" = "M5")

.fl_usage <- function() {
	.fl_say("usage: Rscript fleet/fleet.R <verb> [options]")
	.fl_say("verbs now:   spaces apps add import-unbuilt fetch reconcile-ids")
	.fl_say("             process report rollup status stamp reconcile")
	.fl_say("             upload verify")
	.fl_say("verbs later: ", paste(names(.FL_TODO), collapse = " "))
	.fl_say("options: --manifest f --space id|# --apps i,j|id,id --name s")
	.fl_say("         --type t --dir d --all --stage s --digest --no-rollup")
	.fl_say("         --no-style --allow-unresolved --adopt --dry-run --live")
	.fl_say("         --mode copy|overwrite --to-space id --force")
}

#' Run one fleet command. Returns the process exit code; `fleet.R` run as a
#' script quits with it, and fleet/test_fleet.R calls this directly - the
#' tested path and the shipped path are the same path.
fleet_main <- function(argv) {
	p <- fleet_parse_args(argv)
	if (nzchar(p$error)) { .fl_warn(p$error); .fl_usage(); return(1L) }
	if (isTRUE(p$opts[["help"]]) || p$verb %in% c("help", "--help")) { .fl_usage(); return(0L) }
	if (isTRUE(p$opts[["live"]])) DRY_RUN <<- FALSE
	if (isTRUE(p$opts[["dry-run"]])) DRY_RUN <<- TRUE
	switch(p$verb,
	       spaces = .fl_verb_spaces(p$opts),
	       apps = .fl_verb_apps(p$opts),
	       add = .fl_verb_add(p$opts),
	       status = .fl_verb_status(p$opts),
	       "import-unbuilt" = .fl_verb_import_unbuilt(p$opts, p$pos),
	       fetch = .fl_verb_fetch(p$opts),
	       "reconcile-ids" = .fl_verb_reconcile_ids(p$opts),
	       stamp = .fl_verb_stamp(p$opts),
	       reconcile = .fl_verb_reconcile(p$opts),
	       upload = .fl_verb_upload(p$opts),
	       verify = .fl_verb_verify(p$opts),
	       process = .fl_verb_process(p$opts),
	       report = .fl_verb_report(p$opts),
	       rollup = .fl_verb_rollup(p$opts),
	       {
	       	if (p$verb %in% names(.FL_TODO)) {
	       		.fl_warn(p$verb, ": not implemented (", .FL_TODO[[p$verb]], ")")
	       	} else {
	       		.fl_warn("unknown verb: ", p$verb)
	       		.fl_usage()
	       	}
	       	1L
	       })
}

if (.FL_DIRECT) quit(status = fleet_main(commandArgs(trailingOnly = TRUE)))
