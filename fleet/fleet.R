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
# The remaining verbs are named here and exit 1 with the milestone that brings
# them, so the CLI never pretends to have done something it has not.
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
                    "--force", "--no-style", "--allow-unresolved", "--help")

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
fleet_flags_read <- function(path = FLAGS_CSV) {
	cols <- c("flag", "where", "pattern", "note")
	if (!file.exists(path)) {
		f <- as.data.frame(matrix(character(0), nrow = 0L, ncol = length(cols)),
		                   stringsAsFactors = FALSE)
		names(f) <- cols
		return(f)
	}
	f <- read_csv_any(path)
	for (cn in cols) if (is.null(f[[cn]])) f[[cn]] <- rep("", nrow(f))
	f[, cols, drop = FALSE]
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
		pct <- if (n_total > 0L) round(100 * n_ok / n_total) else 0L

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
			loads_total = n_total, loads_retargeted = n_ok,
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

.FL_TODO <- c("fetch" = "M2", "upload" = "M3", "verify" = "M3",
              "stamp" = "M4", "reconcile" = "M4", "map" = "M5")

.fl_usage <- function() {
	.fl_say("usage: Rscript fleet/fleet.R <verb> [options]")
	.fl_say("verbs now:   spaces apps add import-unbuilt process report")
	.fl_say("             rollup status")
	.fl_say("verbs later: ", paste(names(.FL_TODO), collapse = " "))
	.fl_say("options: --manifest f --space id|# --apps i,j|id,id --name s")
	.fl_say("         --type t --dir d --all --stage s --digest --no-rollup")
	.fl_say("         --no-style --allow-unresolved --dry-run --live")
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
