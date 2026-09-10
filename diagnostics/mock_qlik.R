# mock_qlik.R - a fake qlik.exe, for offline tests. Reached through
# mock_qlik.bat, whose path goes into QLIK_CLI_PATH (or qlik_cli_path.txt)
# so that fleet/qlik_api.R talks to it exactly as it would to the real CLI.
# PLAN-fleet.md M0. Never run this on a VM; it answers from canned data and
# knows nothing about any tenant.
#
# WHAT IT IS FOR: every fleet verb has to be provable on a machine with no
# tenant, and a write verb has to be provable WITHOUT writing anything
# anywhere. So this exe (a) answers the subcommands fleet/ issues with
# plausible JSON, (b) appends every argv line it is given to the file named
# by env var MOCK_QLIK_LOG, which is what lets a test assert that a dry run
# issued NO call at all, and (c) creates a plausible bundle directory on
# `app unbuild` so the fetch path has something real to find.
#
# SHAPE HONESTY: the reply shapes here are the ones PLAN-fleet.md section 0
# derived from qlik.dev, not ones measured against the tenant - `space ls`
# paging and `app copy`'s reply are marked INFERRED there. This file is
# therefore a statement of what fleet/ EXPECTS, and the pairing is only
# proven once `qlik <cmd> --help` and one real read-only call are run on the
# VM (PLAN-fleet.md section 10, DESIGN 8.2: the published reference did not
# match the installed CLI once already). Fix both ends together when it is.
#
# ASCII only, base R only.

.argv <- commandArgs(trailingOnly = TRUE)

.log <- Sys.getenv("MOCK_QLIK_LOG", "")
if (nzchar(.log)) {
	cat(paste(.argv, collapse = " "), "\n", sep = "", file = .log, append = TRUE)
}

.mk_die <- function(msg, status = 1L) {
	cat("Error: ", msg, "\n", sep = "", file = stderr())
	quit(status = status)
}
# Value of --name, or NA. Long form only; the real CLI takes --name=value too,
# and fleet/qlik_api.R never emits that form, so neither is accepted here -
# a mock that is more permissive than the thing it stands in for hides bugs.
.mk_val <- function(name, default = NA_character_) {
	at <- which(.argv == name)
	if (!length(at)) return(default)
	if (at[1] + 1L > length(.argv)) .mk_die(paste("flag needs an argument:", name))
	.argv[at[1] + 1L]
}
.mk_has <- function(name) name %in% .argv
# Positional words: everything that is not a flag and not a flag's argument.
.mk_words <- function() {
	skip <- logical(length(.argv))
	for (k in seq_along(.argv)) {
		if (startsWith(.argv[k], "-")) {
			skip[k] <- TRUE
			if (.argv[k] %in% c("--app", "--dir", "--script", "--spaceId", "--name",
			                    "--type", "--limit", "--next", "--collectionId", "--id",
			                    "--resourceId", "--resourceIds", "--resourceType",
			                    "--attributes-name", "--attributes-spaceId",
			                    "--attributes-description") &&
			    k + 1L <= length(.argv)) skip[k + 1L] <- TRUE
		}
	}
	.argv[!skip]
}

# --- canned tenant --------------------------------------------------------
# Ids are id-shaped for .looks_like_id() (8+ of [A-Za-z0-9_-]); names carry a
# space and a dash on purpose, since quoting is the hazard this mock exists
# to exercise.
.SPACE_ID   <- c("sp00000000001", "sp00000000002", "sp00000000003", "sp00000000004")
.SPACE_NAME <- c("On Prem Apps", "Curated Data Store", "Migration Staging", "Adam - Personal")
.SPACE_TYPE <- c("shared", "managed", "shared", "personal")

.APP_ID    <- c("a0000000-0000-0000-0000-000000000001",
                "a0000000-0000-0000-0000-000000000002",
                "a0000000-0000-0000-0000-000000000003",
                "b0000000-0000-0000-0000-000000000009")
.APP_NAME  <- c("01 ESS QVD Builder - CDP", "Grants QVD Generator",
                "05 - Grant Acquittals Report", "Curated - Sales Extract")
.APP_SPACE <- c(1L, 1L, 1L, 2L)
.APP_MOD   <- c("2026-08-25T00:13:48Z", "2026-08-25T01:02:03Z",
                "2026-08-26T09:00:00Z", "2026-07-01T12:00:00Z")

.q <- function(s) paste0('"', gsub('"', '\\\\"', s), '"')
.obj <- function(...) paste0("{", paste(c(...), collapse = ","), "}")
.fld <- function(k, v) paste0(.q(k), ":", .q(v))

# Paging contract: `links.next.href` carrying a `next=` query parameter,
# which is the shape fleet/qlik_api.R's qc_pages() follows. The page size is
# fixed at 2 and --limit is DELIBERATELY ignored: a mock that honoured a
# caller's --limit 100 would return everything in one page and the
# multi-page path - the whole reason DESIGN 8.3 exists - would never run in
# a test.
# MOCK_QLIK_SHAPE=items makes every paged listing carry its rows under `items`
# instead of `data` - a tenant whose reply shape is not the inferred one. It
# exists so the shape guards (qc_expect/qc_items) and `fleet.R doctor` can be
# proved to FAIL when they should; unset, nothing about this mock changes.
.SHAPE <- Sys.getenv("MOCK_QLIK_SHAPE", "")
.rows_key <- function() if (identical(.SHAPE, "items")) "items" else "data"

.page <- function(items, page, per = 2L) {
	n <- length(items)
	from <- (page - 1L) * per + 1L
	to <- min(n, page * per)
	body <- if (from > n) character(0) else items[from:to]
	more <- to < n
	nxt <- if (more)
		paste0(',', .q("links"), ':', .obj(paste0(.q("next"), ":",
			.obj(.fld("href", paste0("https://mock.invalid/api/v1/items?limit=", per,
			                         "&next=page", page + 1L))))))
	else paste0(",", .q("links"), ":", .obj())
	paste0("{", .q(.rows_key()), ":[", paste(body, collapse = ","), "]", nxt, "}")
}
.page_no <- function() {
	tok <- .mk_val("--next")
	if (is.na(tok)) return(1L)
	n <- suppressWarnings(as.integer(sub("^page", "", tok)))
	if (is.na(n)) .mk_die(paste("bad next token:", tok))
	n
}

.item_id <- function(app_id) paste0("itm-", substr(app_id, 1L, 8L), "-0001")

# --- the fake app store, and one canned failure (M3) ----------------------
# `app build` has to be provable end to end: upload writes a script onto an
# app, `verify` unbuilds that app again and compares. A mock that only
# printed "updated" could prove neither half. So when env var
# MOCK_QLIK_APPS names a directory, `app build` KEEPS the script it was
# given there and `app unbuild` hands that same file back as script.qvs.
# UNSET, both behave exactly as they did in M0, which is what keeps
# fleet/test_fleet.R untouched by this addition.
#
# MOCK_QLIK_FAIL is the other half: a verb's failure path (a build that
# fails AFTER a copy has already created an app) cannot be tested against a
# mock that always succeeds. Its value is matched against the start of the
# command words, so "app build" fails every build and "app copy" every copy.
.APPS_STORE <- Sys.getenv("MOCK_QLIK_APPS", "")
.FAIL <- Sys.getenv("MOCK_QLIK_FAIL", "")
.mk_store_path <- function(id)
	file.path(.APPS_STORE, paste0(gsub("[^A-Za-z0-9._-]", "_", id), ".qvs"))

# Data connections are SPACE-level in cloud (PLAN-fleet.md section 0), which
# is why `upload`'s connection check asks per space. INFERRED flag, marked as
# such in DESIGN 8.7: the mock states what fleet/ expects, not what the
# installed CLI was measured to answer.
.CONN_ID    <- c("dc0000000001", "dc0000000002", "dc0000000003")
.CONN_NAME  <- c("DataFiles", "AzureDataLake", "CuratedDataStore")
.CONN_SPACE <- c(1L, 3L, 2L)

# --- collections, and the one piece of STATE this mock keeps ---------------
# M4's verbs are about a round trip: stamp adds an item to a collection and
# expects `item collections` to say so afterwards, and reconcile has to see a
# tag that disagrees with the ledger. A purely canned answer can prove neither.
#
# So: when env var MOCK_QLIK_STATE names a file, `collection create` and
# `collection item create|rm` write to it and `collection ls` / `item
# collections` read from it. When it is UNSET the mock behaves exactly as it
# did in M0 - fixed answers, no file - which is what keeps fleet/test_fleet.R
# untouched by this addition. The format is JSON the mock itself is the only
# writer of, so the small readers below are safe on it and stay base R.
.STATE <- Sys.getenv("MOCK_QLIK_STATE", "")
.COL_ID <- c("col000000001", "col000000002")
.COL_NAME <- c("mig:unbuilt", "mig:styled")

.st_read <- function() {
	s <- list(collections = character(0), items = list())
	if (!nzchar(.STATE) || !file.exists(.STATE)) return(s)
	txt <- paste(readLines(.STATE, warn = FALSE), collapse = "")
	cb <- regmatches(txt, regexpr('"collections":[{][^}]*[}]', txt))
	if (length(cb)) {
		prs <- regmatches(cb, gregexpr('"[^"]+":"[^"]*"', cb))[[1]]
		if (length(prs))
			s$collections <- setNames(sub('^.*:"([^"]*)"$', "\\1", prs),
			                          sub('^"([^"]+)":.*$', "\\1", prs))
	}
	for (e in regmatches(txt, gregexpr('"[^"]+":\\[[^]]*\\]', txt))[[1]]) {
		k <- sub('^"([^"]+)":.*$', "\\1", e)
		# GOTCHA (caught by fleet/test_fleet_m2.R, not by inspection): the
		# match positions and the string they are extracted from must be the
		# SAME string. Computing gregexpr() on the stripped remainder and then
		# calling regmatches() on the unstripped `e` reads at the wrong offsets
		# and silently yields the item id instead of its collection ids.
		rest <- sub('^"[^"]+":', "", e)
		s$items[[k]] <- gsub('"', "", regmatches(rest, gregexpr('"[^"]*"', rest))[[1]])
	}
	s
}

.st_write <- function(s) {
	if (!nzchar(.STATE)) return(invisible(NULL))
	cols <- if (length(s$collections))
		paste(sprintf('"%s":"%s"', names(s$collections), s$collections), collapse = ",")
		else ""
	its <- if (length(s$items))
		paste(vapply(names(s$items), function(k)
			sprintf('"%s":[%s]', k,
			        paste(sprintf('"%s"', s$items[[k]]), collapse = ",")),
			character(1), USE.NAMES = FALSE), collapse = ",")
		else ""
	cat('{"collections":{', cols, '},"items":{', its, '}}\n', sep = "", file = .STATE)
	invisible(NULL)
}

# Every collection the tenant has: the two canned ones plus anything created
# during this test run.
.all_collections <- function(s) {
	c(setNames(.COL_NAME, .COL_ID), s$collections)
}

# --- subcommands ----------------------------------------------------------
.cmd_space <- function(w) {
	if (length(w) < 2L || w[2] != "ls") .mk_die(paste("unknown space command:", paste(w, collapse = " ")))
	keep <- seq_along(.SPACE_ID)
	ty <- .mk_val("--type")
	if (!is.na(ty)) keep <- keep[.SPACE_TYPE[keep] %in% strsplit(ty, ",", fixed = TRUE)[[1]]]
	nm <- .mk_val("--name")
	if (!is.na(nm)) keep <- keep[grepl(nm, .SPACE_NAME[keep], fixed = TRUE)]
	if (!.mk_has("--json")) {
		for (k in keep) cat(.SPACE_ID[k], " ", .SPACE_NAME[k], "\n", sep = "")
		return(invisible(NULL))
	}
	items <- vapply(keep, function(k)
		.obj(.fld("id", .SPACE_ID[k]), .fld("name", .SPACE_NAME[k]),
		     .fld("type", .SPACE_TYPE[k])), character(1))
	cat(.page(items, .page_no()), "\n", sep = "")
}

.cmd_app <- function(w) {
	sub <- if (length(w) >= 2L) w[2] else ""
	if (sub == "ls") {
		keep <- seq_along(.APP_ID)
		sp <- .mk_val("--spaceId")
		if (!is.na(sp)) {
			hit <- match(sp, .SPACE_ID)
			if (is.na(hit)) .mk_die(paste("no such space:", sp))
			keep <- keep[.APP_SPACE[keep] == hit]
		}
		nm <- .mk_val("--name")
		if (!is.na(nm)) keep <- keep[grepl(nm, .APP_NAME[keep], fixed = TRUE)]
		if (!.mk_has("--json")) {
			for (k in keep) cat(.APP_ID[k], " ", .APP_NAME[k], "\n", sep = "")
			return(invisible(NULL))
		}
		items <- vapply(keep, function(k)
			.obj(.fld("id", .APP_ID[k]), .fld("name", .APP_NAME[k]),
			     .fld("modifiedDate", .APP_MOD[k]),
			     .fld("spaceId", .SPACE_ID[.APP_SPACE[k]])), character(1))
		cat(.page(items, .page_no()), "\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "unbuild") {
		id <- .mk_val("--app")
		dir <- .mk_val("--dir")
		if (is.na(id) || is.na(dir)) .mk_die("app unbuild needs --app and --dir")
		k <- match(id, .APP_ID)
		stored <- if (nzchar(.APPS_STORE)) .mk_store_path(id) else ""
		has_stored <- nzchar(stored) && file.exists(stored)
		# An app the store knows is a real app here even when it is not one of
		# the canned four: `upload --mode copy` creates one and `verify`
		# unbuilds it a moment later.
		if (is.na(k) && !has_stored) .mk_die(paste("no such app:", id))
		nm <- if (is.na(k)) paste("Copied App", id) else .APP_NAME[k]
		sp <- if (is.na(k)) .SPACE_ID[3] else .SPACE_ID[.APP_SPACE[k]]
		dir.create(file.path(dir, "objects"), recursive = TRUE, showWarnings = FALSE)
		writeLines(c("{", paste0('  "qTitle": ', .q(nm), ","),
		             paste0('  "qThumbnail": { "qUrl": "/api/v1/apps/', id, '/media/files/t.png" },'),
		             paste0('  "spaceId": ', .q(sp), ","),
		             '  "published": false', "}"),
		           file.path(dir, "app-properties.json"))
		if (has_stored) file.copy(stored, file.path(dir, "script.qvs"), overwrite = TRUE)
		else writeLines(c("///$tab Main", "SET vMock = 1;", "",
		             "MockTable:", "LOAD 1 AS [Mock Field]", "AUTOGENERATE 1;"),
		           file.path(dir, "script.qvs"))
		writeLines("[]", file.path(dir, "variables.json"))
		writeLines("[]", file.path(dir, "dimensions.json"))
		writeLines("[]", file.path(dir, "measures.json"))
		writeLines("connections:", file.path(dir, "connections.yml"))
		writeLines(paste0('{ "qInfo": { "qId": "obj-0001", "qType": "sheet" }, "title": ',
		                  .q(paste("Sheet -", nm)), " }"),
		           file.path(dir, "objects", "sheet-mock.json"))
		cat("unbuilt ", id, " to ", dir, "\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "build") {
		id <- .mk_val("--app")
		script <- .mk_val("--script")
		if (is.na(id)) .mk_die("app build needs --app")
		if (is.na(script) || !file.exists(script)) .mk_die(paste("no such script file:", script))
		if (is.na(match(id, .APP_ID)) && !startsWith(id, "copy-")) .mk_die(paste("no such app:", id))
		# The script is KEPT (when there is a store) so `app unbuild` can hand
		# the very same bytes back - which is the whole of what `verify` checks.
		if (nzchar(.APPS_STORE)) {
			dir.create(.APPS_STORE, recursive = TRUE, showWarnings = FALSE)
			file.copy(script, .mk_store_path(id), overwrite = TRUE)
		}
		cat("app ", id, " updated from ", basename(script), "\n", sep = "")
		return(invisible(NULL))
	}
	if (sub %in% c("copy", "create")) {
		nm <- .mk_val("--attributes-name", "Untitled")
		sp <- .mk_val("--attributes-spaceId", "")
		src <- if (sub == "copy") {
			if (length(w) < 3L) .mk_die("app copy needs an app id") else w[3]
		} else "new"
		newid <- paste0("copy-", substr(gsub("[^A-Za-z0-9]", "", src), 1L, 8L), "-0001")
		cat(.obj(paste0(.q("attributes"), ":",
		                .obj(.fld("id", newid), .fld("name", nm), .fld("spaceId", sp)))),
		    "\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "update") {
		if (length(w) < 3L) .mk_die("app update needs an app id")
		cat("app ", w[3], " updated\n", sep = "")
		return(invisible(NULL))
	}
	.mk_die(paste("unknown app command:", sub))
}

.cmd_item <- function(w) {
	sub <- if (length(w) >= 2L) w[2] else ""
	if (sub == "ls") {
		ids <- .mk_val("--resourceIds")
		if (is.na(ids)) ids <- .mk_val("--resourceId")
		keep <- if (is.na(ids)) seq_along(.APP_ID)
		        else which(.APP_ID %in% strsplit(ids, ",", fixed = TRUE)[[1]])
		items <- vapply(keep, function(k)
			.obj(.fld("id", .item_id(.APP_ID[k])), .fld("resourceId", .APP_ID[k]),
			     .fld("resourceType", "app"), .fld("name", .APP_NAME[k])), character(1))
		if (!.mk_has("--json")) {
			for (k in keep) cat(.item_id(.APP_ID[k]), " ", .APP_NAME[k], "\n", sep = "")
			return(invisible(NULL))
		}
		cat(.page(items, .page_no(), per = 100L), "\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "collections") {
		if (length(w) < 3L) .mk_die("item collections needs an item id")
		if (!nzchar(.STATE)) {
			cat("[", .obj(.fld("id", "col000000001"), .fld("name", "mig:unbuilt"),
			              .fld("type", "public")), "]\n", sep = "")
			return(invisible(NULL))
		}
		s <- .st_read()
		all <- .all_collections(s)
		mine <- s$items[[w[3]]]
		if (is.null(mine)) mine <- character(0)
		mine <- mine[mine %in% names(all)]
		cat("[", paste(vapply(mine, function(cid)
			.obj(.fld("id", cid), .fld("name", unname(all[cid])),
			     .fld("type", "public")), character(1), USE.NAMES = FALSE),
			collapse = ","), "]\n", sep = "")
		return(invisible(NULL))
	}
	.mk_die(paste("unknown item command:", sub))
}

.cmd_collection <- function(w) {
	sub <- if (length(w) >= 2L) w[2] else ""
	s <- .st_read()
	if (sub == "ls") {
		all <- .all_collections(s)
		cat("[", paste(vapply(names(all), function(cid)
			.obj(.fld("id", cid), .fld("name", unname(all[cid])),
			     .fld("type", "public")), character(1), USE.NAMES = FALSE),
			collapse = ","), "]\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "create") {
		nm <- .mk_val("--name")
		if (is.na(nm)) .mk_die("collection create needs --name")
		# A tag name is UNIQUE on the tenant, so a second create of the same
		# name is a 409. fleet/fleet.R's fleet_tag_id() treats that as "someone
		# else made it" and re-reads the listing, which is the recovery
		# PLAN-fleet.md section 7 asks for - and it cannot be tested at all
		# unless the mock actually refuses.
		if (nm %in% .all_collections(s))
			.mk_die(paste0("409 Conflict: a collection named ", nm, " already exists"),
			        status = 1L)
		newid <- if (!nzchar(.STATE)) "col000000009"
		         else sprintf("col%09d", 9L + length(s$collections) + 1L)
		s$collections[[newid]] <- nm
		.st_write(s)
		cat(.obj(.fld("id", newid), .fld("name", nm),
		         .fld("type", .mk_val("--type", "public"))), "\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "item") {
		act <- if (length(w) >= 3L) w[3] else ""
		col <- .mk_val("--collectionId")
		if (is.na(col)) .mk_die("collection item needs --collectionId")
		if (act == "create") {
			id <- .mk_val("--id")
			if (is.na(id)) .mk_die("collection item create needs --id")
			s$items[[id]] <- unique(c(s$items[[id]], col))
			.st_write(s)
			cat("added ", id, " to ", col, "\n", sep = "")
			return(invisible(NULL))
		}
		if (act == "rm") {
			if (length(w) < 4L) .mk_die("collection item rm needs an item id")
			s$items[[w[4]]] <- setdiff(s$items[[w[4]]], col)
			.st_write(s)
			cat("removed ", w[4], " from ", col, "\n", sep = "")
			return(invisible(NULL))
		}
		.mk_die(paste("unknown collection item action:", act))
	}
	.mk_die(paste("unknown collection command:", sub))
}

.cmd_dataconnection <- function(w) {
	if (length(w) < 2L || w[2] != "ls")
		.mk_die(paste("unknown data-connection command:", paste(w, collapse = " ")))
	keep <- seq_along(.CONN_ID)
	sp <- .mk_val("--spaceId")
	if (!is.na(sp)) {
		hit <- match(sp, .SPACE_ID)
		if (is.na(hit)) .mk_die(paste("no such space:", sp))
		keep <- keep[.CONN_SPACE[keep] == hit]
	}
	if (!.mk_has("--json")) {
		for (k in keep) cat(.CONN_ID[k], " ", .CONN_NAME[k], "\n", sep = "")
		return(invisible(NULL))
	}
	items <- vapply(keep, function(k)
		.obj(.fld("id", .CONN_ID[k]), .fld("qName", .CONN_NAME[k]),
		     .fld("name", .CONN_NAME[k]),
		     .fld("spaceId", .SPACE_ID[.CONN_SPACE[k]])), character(1))
	cat(.page(items, .page_no()), "\n", sep = "")
}

.mk_main <- function() {
	if (.mk_has("--version") || .mk_has("version")) {
		cat("qlik version 3.2.0-mock\n")
		return(invisible(0L))
	}
	w <- .mk_words()
	if (!length(w)) .mk_die("no command given")
	if (nzchar(.FAIL) && startsWith(paste(w, collapse = " "), .FAIL))
		.mk_die(paste0("canned failure for '", .FAIL, "' (MOCK_QLIK_FAIL)"))
	switch(w[1],
	       space = .cmd_space(w),
	       app = .cmd_app(w),
	       item = .cmd_item(w),
	       collection = .cmd_collection(w),
	       "data-connection" = .cmd_dataconnection(w),
	       .mk_die(paste("unknown command:", w[1])))
	invisible(0L)
}

.mk_main()
quit(status = 0L)
