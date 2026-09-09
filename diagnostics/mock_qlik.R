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
	paste0("{", .q("data"), ":[", paste(body, collapse = ","), "]", nxt, "}")
}
.page_no <- function() {
	tok <- .mk_val("--next")
	if (is.na(tok)) return(1L)
	n <- suppressWarnings(as.integer(sub("^page", "", tok)))
	if (is.na(n)) .mk_die(paste("bad next token:", tok))
	n
}

.item_id <- function(app_id) paste0("itm-", substr(app_id, 1L, 8L), "-0001")

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
		if (is.na(k)) .mk_die(paste("no such app:", id))
		dir.create(file.path(dir, "objects"), recursive = TRUE, showWarnings = FALSE)
		writeLines(c("{", paste0('  "qTitle": ', .q(.APP_NAME[k]), ","),
		             paste0('  "qThumbnail": { "qUrl": "/api/v1/apps/', id, '/media/files/t.png" },'),
		             paste0('  "spaceId": ', .q(.SPACE_ID[.APP_SPACE[k]]), ","),
		             '  "published": false', "}"),
		           file.path(dir, "app-properties.json"))
		writeLines(c("///$tab Main", "SET vMock = 1;", "",
		             "MockTable:", "LOAD 1 AS [Mock Field]", "AUTOGENERATE 1;"),
		           file.path(dir, "script.qvs"))
		writeLines("[]", file.path(dir, "variables.json"))
		writeLines("[]", file.path(dir, "dimensions.json"))
		writeLines("[]", file.path(dir, "measures.json"))
		writeLines("connections:", file.path(dir, "connections.yml"))
		writeLines(paste0('{ "qInfo": { "qId": "obj-0001", "qType": "sheet" }, "title": ',
		                  .q(paste("Sheet -", .APP_NAME[k])), " }"),
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
		cat("[", .obj(.fld("id", "col000000001"), .fld("name", "mig:unbuilt"),
		              .fld("type", "public")), "]\n", sep = "")
		return(invisible(NULL))
	}
	.mk_die(paste("unknown item command:", sub))
}

.cmd_collection <- function(w) {
	sub <- if (length(w) >= 2L) w[2] else ""
	if (sub == "ls") {
		cat("[", paste(c(.obj(.fld("id", "col000000001"), .fld("name", "mig:unbuilt"),
		                      .fld("type", "public")),
		                 .obj(.fld("id", "col000000002"), .fld("name", "mig:styled"),
		                      .fld("type", "public"))), collapse = ","), "]\n", sep = "")
		return(invisible(NULL))
	}
	if (sub == "create") {
		nm <- .mk_val("--name")
		if (is.na(nm)) .mk_die("collection create needs --name")
		cat(.obj(.fld("id", "col000000009"), .fld("name", nm),
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
			cat("added ", id, " to ", col, "\n", sep = "")
			return(invisible(NULL))
		}
		if (act == "rm") {
			if (length(w) < 4L) .mk_die("collection item rm needs an item id")
			cat("removed ", w[4], " from ", col, "\n", sep = "")
			return(invisible(NULL))
		}
		.mk_die(paste("unknown collection item action:", act))
	}
	.mk_die(paste("unknown collection command:", sub))
}

.mk_main <- function() {
	if (.mk_has("--version") || .mk_has("version")) {
		cat("qlik version 3.2.0-mock\n")
		return(invisible(0L))
	}
	w <- .mk_words()
	if (!length(w)) .mk_die("no command given")
	switch(w[1],
	       space = .cmd_space(w),
	       app = .cmd_app(w),
	       item = .cmd_item(w),
	       collection = .cmd_collection(w),
	       .mk_die(paste("unknown command:", w[1])))
	invisible(0L)
}

.mk_main()
quit(status = 0L)
