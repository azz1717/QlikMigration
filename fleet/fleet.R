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
# M0 implements spaces / apps / add / status. The remaining verbs are named
# here and exit 1 with the milestone that brings them, so the CLI never
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
                    "--force", "--no-style", "--allow-unresolved", "--help")

#' Split argv into a verb and an options list. Returns
#' list(verb, opts, error): `error` non-empty means usage, and the caller
#' exits 1 without running anything. An unknown flag is an ERROR, never
#' ignored - a mistyped --appss selecting everything instead of three apps is
#' exactly the accident a batch tool must not have.
fleet_parse_args <- function(argv) {
	res <- list(verb = "", opts = list(), error = "")
	if (!length(argv)) return(list(verb = "", opts = list(), error = "no verb given"))
	res$verb <- argv[1]
	i <- 2L
	while (i <= length(argv)) {
		a <- argv[i]
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
	# master.csv and its readiness ordering arrive with rollup in M1; until
	# then this verb reports the ledger itself rather than inventing a
	# readiness number out of data nothing has produced yet.
	.fl_say("master.csv: rollup lands in M1 (PLAN-fleet.md section 5)")
	if (any(m$stage == "blocked")) 2L else 0L
}

.FL_TODO <- c("import-unbuilt" = "M1", "process" = "M1", "report" = "M1",
              "rollup" = "M1", "fetch" = "M2", "upload" = "M3", "verify" = "M3",
              "stamp" = "M4", "reconcile" = "M4", "map" = "M5")

.fl_usage <- function() {
	.fl_say("usage: Rscript fleet/fleet.R <verb> [options]")
	.fl_say("verbs now:   spaces  apps  add  status")
	.fl_say("verbs later: ", paste(names(.FL_TODO), collapse = " "))
	.fl_say("options: --manifest f --space id|# --apps i,j --name s --type t")
	.fl_say("         --all --stage s --digest --dry-run --live --no-rollup")
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
