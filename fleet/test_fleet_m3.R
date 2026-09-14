# test_fleet_m3.R - the offline gate for PLAN-fleet.md M3: `upload` (copy or
# overwrite) and `verify`, plus the console UI's [8].
#
#     Rscript fleet/test_fleet_m3.R
#
# Exits 0 when every check passes, 1 otherwise. Hand-run, like
# fleet/test_fleet.R, test_fleet_m1.R and test_fleet_m2.R: styling/verify.R
# stays styling-only and docs/verify_docs.R reads no fixtures.
#
# NO TENANT IS TOUCHED, and it is enforced rather than promised: QLIK_CLI_PATH
# points at diagnostics/mock_qlik.bat for the whole run, so even a bug that
# issued an unintended write could only reach the mock. MOCK_QLIK_LOG proves
# a dry run issued no write call at all; MOCK_QLIK_APPS is the mock's fake app
# store, which is what lets `verify` really unbuild what `upload` really
# built; MOCK_QLIK_FAIL is how the build-fails-after-a-successful-copy path -
# the one that can orphan an app on the tenant - gets tested at all.
#
# Every fleet path is redirected into tempdir() by reassigning the globals
# fleet.R reads at CALL time, so a run cannot touch the real ledger, the real
# fleet/apps/ or the real master CSVs. No file under C:/Rtools is written.

.t3_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.t3_root <- if (length(.t3_file))
	tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .t3_file[1])))),
	         error = function(e) NA_character_) else NA_character_
if (is.na(.t3_root) || !dir.exists(.t3_root))
	stop("run this as 'Rscript fleet/test_fleet_m3.R'", call. = FALSE)
setwd(.t3_root)

source("fleet/fleet.R")

.T3_CHECKS <- 0L
.T3_FAILS <- 0L
.t3_ok <- function(label, passed, detail = NULL) {
	.T3_CHECKS <<- .T3_CHECKS + 1L
	if (isTRUE(passed)) { cat("  ok   ", label, "\n", sep = ""); return(invisible(TRUE)) }
	.T3_FAILS <<- .T3_FAILS + 1L
	cat("  FAIL ", label, "\n", sep = "")
	for (d in detail) cat("       ", d, "\n", sep = "")
	invisible(FALSE)
}
.t3_section <- function(s) cat("\n== ", s, "\n", sep = "")

MOCK <- normalizePath("diagnostics/mock_qlik.bat", winslash = "\\", mustWork = TRUE)
Sys.setenv(QLIK_CLI_PATH = MOCK)

TMP <- file.path(tempdir(), paste0("fleetm3-", as.integer(Sys.time())))
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
APPS_DIR <- file.path(TMP, "apps")
MANIFEST_DEFAULT <- file.path(TMP, "manifest.csv")
MASTER_CSV <- file.path(TMP, "master.csv")
MASTER_LOADS_CSV <- file.path(TMP, "master_loads.csv")
MASTER_UNUSED_CSV <- file.path(TMP, "master_unused.csv")
MAP_CHECK_STATUS <- file.path(TMP, "map_check_status.csv")
SPACES_CSV <- file.path(TMP, "spaces.csv")
TAGS_CSV <- file.path(TMP, "tags.csv")
TAG_DRIFT_CSV <- file.path(TMP, "tag_drift.csv")
AUDIT_LOG <- file.path(TMP, "audit.log")
CALLS <- file.path(TMP, "mock_calls.log")
STORE <- file.path(TMP, "mock_apps")
Sys.setenv(MOCK_QLIK_LOG = CALLS, MOCK_QLIK_APPS = STORE)

.t3_calls <- function() if (file.exists(CALLS)) readLines(CALLS, warn = FALSE) else character(0)
# The call log is cleared inline with unlink(CALLS) rather than through a
# helper: fleet/test_fleet_m2.R already owns that one-liner, and a second
# copy of it here is exactly the twin docs/verify_docs.R refuses.
.t3_row <- function(id) {
	m <- manifest_read(MANIFEST_DEFAULT)
	k <- match(id, m$app_id)
	if (is.na(k)) m[0, , drop = FALSE] else m[k, , drop = FALSE]
}
.t3_stage <- function(id) {
	r <- .t3_row(id)
	if (!nrow(r)) NA_character_ else r$stage
}

# The mock's canned tenant (diagnostics/mock_qlik.R).
SPACE3 <- "sp00000000003"                        # "Migration Staging"
APP1 <- "a0000000-0000-0000-0000-000000000001"   # "01 ESS QVD Builder - CDP"
APP2 <- "a0000000-0000-0000-0000-000000000002"   # "Grants QVD Generator"
APP3 <- "a0000000-0000-0000-0000-000000000003"   # "05 - Grant Acquittals Report"
COPY1 <- "copy-a0000000-0001"                    # what `app copy` of APP1 returns

SCRIPT <- c("///$tab Main", "SET vEnv = 'PROD';", "", "Sales:",
            "LOAD [Id] AS [Id]", ", [Amount] AS [Amount]",
            "FROM [lib://AzureDataLake/curated/sales.qvd] (qvd);")

# One app in the ledger with the artefacts a stage promises. Rows are created
# directly at the stage under test - manifest_upsert() accepts any stage on a
# NEW row and only refuses a forward SKIP on an existing one.
.t3_app <- function(id, name, stage = "retargeted", script = SCRIPT,
                    statuses = NULL, space = SPACE3) {
	manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
		data.frame(app_id = id, app_name = name, stage = stage,
		           space_id = space, space_name = "Migration Staging",
		           stringsAsFactors = FALSE)), MANIFEST_DEFAULT)
	d <- .fl_app_dir(id)
	dir.create(d, recursive = TRUE, showWarnings = FALSE)
	if (!is.null(script))
		writeLines(script, file.path(d, "script_retargeted.qvs"))
	if (!is.null(statuses))
		utils::write.csv(data.frame(line = seq_along(statuses), status = statuses,
		                            stringsAsFactors = FALSE),
		                 file.path(d, "retarget_report.csv"), row.names = FALSE)
	d
}

# --- 1. preflight, one refusal at a time ----------------------------------
# PLAN-fleet.md section 3: stage, unresolved loads (D7), a non-empty
# retargeted script, a known target space. Each is checked on its own, so a
# refusal can never be mistaken for a different refusal.
.t3_section("upload preflight (section 3) - every refusal, one at a time")
d_ok <- .t3_app(APP1, "01 ESS QVD Builder - CDP", "retargeted",
                statuses = c("retargeted", "already-mapped"))
pf <- fleet_upload_preflight(.t3_row(APP1), d_ok, "copy", SPACE3)
.t3_ok("a retargeted app with a clean report passes", isTRUE(pf$ok), pf$reason)
.t3_ok("and the target space comes back with it", identical(pf$space, SPACE3))

.t3_app(APP2, "Grants QVD Generator", "unbuilt")
pf <- fleet_upload_preflight(.t3_row(APP2), .fl_app_dir(APP2), "copy", SPACE3)
.t3_ok("a stage other than retargeted is refused",
       !pf$ok && grepl("not formatted and retargeted yet", pf$reason), pf$reason)

LOCAL <- "local:not-on-tenant"
.t3_app(LOCAL, "Not On Tenant", "retargeted")
pf <- fleet_upload_preflight(.t3_row(LOCAL), .fl_app_dir(LOCAL), "copy", SPACE3)
.t3_ok("a local: key has no tenant app to upload to",
       !pf$ok && grepl("not matched to an app on the tenant", pf$reason), pf$reason)

EMPTY <- "e0000000-0000-0000-0000-00000000000e"
de <- .t3_app(EMPTY, "Empty Script", "retargeted", script = NULL)
pf <- fleet_upload_preflight(.t3_row(EMPTY), de, "copy", SPACE3)
.t3_ok("a missing script_retargeted.qvs is refused",
       !pf$ok && grepl("script_retargeted", pf$reason), pf$reason)
file.create(file.path(de, "script_retargeted.qvs"))
pf <- fleet_upload_preflight(.t3_row(EMPTY), de, "copy", SPACE3)
.t3_ok("an EMPTY script_retargeted.qvs is refused too",
       !pf$ok && grepl("empty", pf$reason), pf$reason)

UNRES <- "f0000000-0000-0000-0000-00000000000f"
du <- .t3_app(UNRES, "Unresolved Loads", "retargeted",
              statuses = c("retargeted", "not-in-map", "multi-source"))
pf <- fleet_upload_preflight(.t3_row(UNRES), du, "copy", SPACE3)
# D7 WITHDRAWN 2026-09-14 (Adam): an unresolved load is output, not failure.
# The loads are listed in dev_notes.txt; the app uploads like any other.
.t3_ok("not-in-map / multi-source loads do NOT stop the upload (D7 gone)",
       isTRUE(pf$ok), pf$reason)
.t3_ok("preflight has no force override left to take",
       !"force" %in% names(formals(fleet_upload_preflight)))

NOSPACE <- "c0000000-0000-0000-0000-00000000000c"
dn <- .t3_app(NOSPACE, "No Space", "retargeted", space = "")
pf <- fleet_upload_preflight(.t3_row(NOSPACE), dn, "copy", NULL)
.t3_ok("a copy with no target space anywhere is refused",
       !pf$ok && grepl("--to-space", pf$reason), pf$reason)
pf <- fleet_upload_preflight(.t3_row(NOSPACE), dn, "overwrite", NULL)
.t3_ok("an overwrite needs no space at all (app build has no --spaceId)",
       isTRUE(pf$ok), pf$reason)
pf <- fleet_upload_preflight(.t3_row(NOSPACE), dn, "copy", SPACE3)
.t3_ok("--to-space supplies the space the ledger does not have",
       isTRUE(pf$ok) && identical(pf$space, SPACE3), pf$reason)

# --- 2. the small helpers --------------------------------------------------
.t3_section("names, connections, and the diff summary")
.t3_ok("D2's suffix is appended once",
       identical(fleet_upload_target_name("Sales"), "Sales [mig]"))
.t3_ok("and never twice on a re-upload",
       identical(fleet_upload_target_name("Sales [mig]"), "Sales [mig]"))
cn <- fleet_script_connections(file.path(d_ok, "script_retargeted.qvs"))
.t3_ok("the lib:// connection name is read out of the script",
       identical(cn, "AzureDataLake"), paste(cn, collapse = " "))
.t3_ok("a script with no lib:// at all yields nothing, not an error", {
	p <- file.path(TMP, "plain.qvs")
	writeLines(c("LOAD 1 AS x AUTOGENERATE 1;"), p)
	identical(fleet_script_connections(p), character(0))
})
DRY_RUN <- FALSE
have <- fleet_space_connections(SPACE3)
.t3_ok("data-connection ls answers per space (INFERRED flag, DESIGN 8.7)",
       is.character(have) && "AzureDataLake" %in% have, paste(have, collapse = " "))
.t3_ok("a space the CLI cannot answer for is NULL, not a failure",
       is.null(fleet_space_connections("")))
w <- capture.output(miss <- .fl_conn_warn(file.path(d_ok, "script_retargeted.qvs"),
                                          SPACE3))
.t3_ok("a connection that IS there raises nothing", !length(miss))
p2 <- file.path(TMP, "missing_conn.qvs")
writeLines("FROM [lib://NoSuchConnection/x.qvd] (qvd);", p2)
miss <- .fl_conn_warn(p2, SPACE3)
.t3_ok("a connection that is NOT there is named", identical(miss, "NoSuchConnection"),
       paste(miss, collapse = " "))
DRY_RUN <- TRUE

same <- fleet_script_diff(file.path(d_ok, "script_retargeted.qvs"),
                          file.path(d_ok, "script_retargeted.qvs"))
.t3_ok("a file compares equal to itself", isTRUE(same$same), same$summary)
crlf <- file.path(TMP, "crlf.qvs")
con <- file(crlf, "wb")
writeBin(charToRaw(paste0(paste(SCRIPT, collapse = "\r\n"), "\r\n")), con)
close(con)
d <- fleet_script_diff(crlf, file.path(d_ok, "script_retargeted.qvs"))
.t3_ok("CRLF against LF is the SAME script", isTRUE(d$same), d$summary)
tamp <- file.path(TMP, "tampered.qvs")
writeLines(c(SCRIPT[1:4], "LOAD [Id] AS [Nope]", SCRIPT[6:7]), tamp)
d <- fleet_script_diff(tamp, file.path(d_ok, "script_retargeted.qvs"))
.t3_ok("a changed line is caught, with its line number",
       !d$same && grepl("first difference at line 5", d$summary), d$summary)
.t3_ok("the summary never prints a line of script",
       !any(grepl("LOAD|lib://", c(d$summary, same$summary))), d$summary)

# --- 3. the dry run: exact command lines, nothing written ------------------
.t3_section("dry run - the command lines Adam reviews before anything happens")
DRY_RUN <- TRUE
unlink(CALLS)
out <- capture.output(rc <- fleet_main(c("upload", "--mode", "copy",
                                         "--to-space", SPACE3,
                                         "--apps", APP1, "--no-rollup")))
cmds <- grep("^DRY RUN", out, value = TRUE)
.t3_ok("a dry copy exits 0", rc == 0L, paste("exit", rc))
.t3_ok("it prints exactly two command lines", length(cmds) == 2L, cmds)
.t3_ok("line 1 is the copy, with the space and the ' [mig]' name (D2)",
       grepl("app.*copy", cmds[1]) && grepl(SPACE3, cmds[1], fixed = TRUE) &&
       	grepl("[mig]", cmds[1], fixed = TRUE), cmds[1])
.t3_ok("line 2 is the build, --no-reload (D3) and --silent",
       grepl("app.*build", cmds[2]) && grepl("--no-reload", cmds[2]) &&
       	grepl("--silent", cmds[2]), cmds[2])
.t3_ok("the build names the copy's id as a placeholder, never an invented one",
       grepl("id of the new copy", cmds[2], fixed = TRUE), cmds[2])
.t3_ok("script only - no --connections/--objects/--variables etc (D5)",
       !any(grepl("--connections|--objects|--dimensions|--measures|--variables",
                  cmds)), cmds)
.t3_ok("a dry run issues NO app copy and NO app build against the CLI",
       !any(grepl("app copy|app build", .t3_calls())), .t3_calls())
.t3_ok("a dry run leaves the stage exactly where it was",
       identical(.t3_stage(APP1), "retargeted"))
.t3_ok("and records no target_app_id", !nzchar(.t3_row(APP1)$target_app_id))
.t3_ok("a dry run is still AUDITED (the console is not the record)",
       any(grepl("DRYRUN", readLines(AUDIT_LOG, warn = FALSE), fixed = TRUE)))

out <- capture.output(rc <- fleet_main(c("upload", "--mode", "overwrite",
                                         "--apps", APP1, "--no-rollup")))
cmds <- grep("^DRY RUN", out, value = TRUE)
.t3_ok("a dry overwrite exits 0", rc == 0L)
.t3_ok("an overwrite is ONE command line: the build onto the source app",
       length(cmds) == 1L && grepl("app.*build", cmds[1]) &&
       	grepl(APP1, cmds[1], fixed = TRUE), cmds)
.t3_ok("an overwrite never copies", !any(grepl("app.*copy", cmds)), cmds)

out <- capture.output(rc <- fleet_main(c("upload", "--apps", UNRES,
                                         "--to-space", SPACE3, "--no-rollup")))
cmds <- grep("^DRY RUN", out, value = TRUE)
.t3_ok("an app with unresolved loads is planned like any other: exit 0, two lines",
       rc == 0L && length(cmds) == 2L, paste("exit", rc, "lines", length(cmds)))

# --- 4. live against the mock: copy, build, then verify --------------------
.t3_section("live against the mock - copy, build, verify the round trip")
unlink(CALLS)
rc <- fleet_main(c("upload", "--mode", "copy", "--to-space", SPACE3,
                   "--apps", APP1, "--live", "--no-rollup"))
.t3_ok("a live copy-mode upload exits 0", rc == 0L, paste("exit", rc))
row1 <- .t3_row(APP1)
.t3_ok("the app advanced to stage built", identical(row1$stage, "built"), row1$stage)
.t3_ok("the copy's id is in the ledger", identical(row1$target_app_id, COPY1),
       row1$target_app_id)
.t3_ok("so is its ' [mig]' name",
       identical(row1$target_name, "01 ESS QVD Builder - CDP [mig]"),
       row1$target_name)
calls <- .t3_calls()
.t3_ok("the CLI really was asked to copy, into the target space",
       any(grepl("app copy", calls, fixed = TRUE)) &&
       	any(grepl(SPACE3, calls, fixed = TRUE)), calls)
.t3_ok("and to build the RETARGETED script onto the copy, without reloading",
       any(grepl("app build", calls, fixed = TRUE)) &&
       	any(grepl("script_retargeted.qvs", calls, fixed = TRUE)) &&
       	any(grepl("--no-reload", calls, fixed = TRUE)), calls)

rc <- fleet_main(c("verify", "--apps", APP1, "--live", "--no-rollup"))
.t3_ok("verify exits 0 when the tenant's script matches", rc == 0L, paste("exit", rc))
.t3_ok("and the app advances to stage verified",
       identical(.t3_stage(APP1), "verified"))
.t3_ok("verify really did unbuild the TARGET app",
       any(grepl(paste("app unbuild --app", COPY1), .t3_calls(), fixed = TRUE)),
       utils::tail(.t3_calls(), 3L))

DRY_RUN <- TRUE
unlink(CALLS)
out <- capture.output(rc <- fleet_main(c("verify", "--apps", APP1, "--no-rollup")))
.t3_ok("a dry verify issues no call and moves nothing",
       rc == 0L && !any(grepl("app unbuild", .t3_calls())) &&
       	identical(.t3_stage(APP1), "verified"), .t3_calls())

# A tampered script is what verify exists to catch: build one thing, put
# something else on disk, and the compare must fail with a line number.
# The folder is named for the APP, and the runs above renamed it off the id
# it was created under, so the path is re-derived rather than remembered.
d_ok <- .fl_app_dir(APP1)
writeLines(c(SCRIPT[1:4], "LOAD [Id] AS [Nope]", SCRIPT[6:7]),
           file.path(d_ok, "script_retargeted.qvs"))
rc <- fleet_main(c("verify", "--apps", APP1, "--live", "--no-rollup"))
row1 <- .t3_row(APP1)
.t3_ok("a tampered script fails verification (exit 2)", rc == 2L, paste("exit", rc))
.t3_ok("the row is blocked with a ONE-LINE diff summary",
       identical(row1$stage, "blocked") &&
       	grepl("first difference at line 5", row1$last_error), row1$last_error)
.t3_ok("the recorded error carries no script text",
       !grepl("Nope", row1$last_error), row1$last_error)
writeLines(SCRIPT, file.path(d_ok, "script_retargeted.qvs"))

# --- 5. a build that fails AFTER the copy succeeded ------------------------
# The failure this closes: the copy created a real app on the tenant, the
# build then failed, and nothing wrote the new app's id down - an orphan
# nobody can find again.
.t3_section("a failed build after a successful copy keeps the target id")
d3 <- .t3_app(APP3, "05 - Grant Acquittals Report", "retargeted",
              statuses = "retargeted")
Sys.setenv(MOCK_QLIK_FAIL = "app build")
rc <- fleet_main(c("upload", "--mode", "copy", "--to-space", SPACE3,
                   "--apps", APP3, "--live", "--no-rollup"))
Sys.unsetenv("MOCK_QLIK_FAIL")
row3 <- .t3_row(APP3)
.t3_ok("the run exits 2 (some rows blocked)", rc == 2L, paste("exit", rc))
.t3_ok("the row is blocked, and says the build failed",
       identical(row3$stage, "blocked") && grepl("^build:", row3$last_error),
       row3$last_error)
.t3_ok("the copy's app id was recorded ANYWAY - nothing is orphaned",
       nzchar(row3$target_app_id), row3$target_app_id)
.t3_ok("as was the name it was created with", nzchar(row3$target_name))

# --- 6. an app with unresolved loads, end to end ---------------------------
.t3_section("an app with unresolved loads uploads like any other (D7 gone)")
unlink(CALLS)
# The mock knows nothing of this id, so the overwrite build must fail - which
# is exactly the proof that the call was issued rather than refused up front.
rc <- fleet_main(c("upload", "--mode", "overwrite", "--apps", UNRES,
                   "--live", "--no-rollup"))
.t3_ok("the build is really attempted, not refused for its loads",
       any(grepl("app build", .t3_calls(), fixed = TRUE)), .t3_calls())
.t3_ok("a failing build blocks the row with the CLI's own message",
       identical(.t3_stage(UNRES), "blocked") &&
       	grepl("^build:", .t3_row(UNRES)$last_error), .t3_row(UNRES)$last_error)

# --- 7. CLI hygiene --------------------------------------------------------
.t3_section("CLI hygiene for upload / verify")
.t3_ok("upload without a selection is a usage error",
       fleet_main(c("upload")) == 1L)
.t3_ok("verify without a selection is a usage error",
       fleet_main(c("verify")) == 1L)
.t3_ok("an unknown --mode is a usage error, not a guess",
       fleet_main(c("upload", "--all", "--mode", "sideways")) == 1L)
.t3_ok("--mode and --to-space are known flags", {
	o <- fleet_parse_args(c("upload", "--all", "--mode", "copy", "--to-space",
	                        "s1"))$opts
	identical(o[["mode"]], "copy") && identical(o[["to-space"]], "s1")
})
.t3_ok("--force is gone: the gate it overrode is gone too",
       nzchar(fleet_parse_args(c("upload", "--all", "--force"))$error))
.t3_ok("a mistyped --to-spce is still refused",
       nzchar(fleet_parse_args(c("upload", "--all", "--to-spce", "s"))$error))
.t3_ok("upload and verify no longer report themselves as unimplemented",
       !any(c("upload", "verify") %in% names(.FL_TODO)))
.t3_ok("the console UI is the four-step walk-through, holding no logic", {
	ui <- readLines("ui/console_ui.R", warn = FALSE)
	live <- grep("--live", ui, fixed = TRUE, value = TRUE)
	length(live) > 0L &&
		all(grepl("upload|fetch|verify", live)) &&
		any(grepl('"YES"', ui, fixed = TRUE)) &&
		any(grepl("--space", ui, fixed = TRUE)) &&
		!any(grepl('u == "8"', ui, fixed = TRUE))
})

# --- 8. app folders are named for the app, and index.csv is the record -----
# Adam 2026-09-14: a folder called 7862b8ac-... tells the operator nothing.
# The id stays the key (578 duplicate names on the tenant), so the mapping
# has to live somewhere: index.csv, read and written in ONE place.
.t3_section("fleet/apps/<app name>/ and its index")
IXD <- file.path(TMP, "ixapps")
dir.create(IXD, recursive = TRUE, showWarnings = FALSE)
n1 <- "n0000000-0000-0000-0000-0000000000n1"
n2 <- "n0000000-0000-0000-0000-0000000000n2"
d1 <- .fl_app_dir(n1, IXD, name = "A: B/C")
.t3_ok("a name Windows cannot spell becomes a legal folder",
       !grepl("[:/\\\\]", basename(d1)) && nzchar(basename(d1)), basename(d1))
.t3_ok("the index remembers it: the same id gives the same folder",
       identical(.fl_app_dir(n1, IXD), d1), .fl_app_dir(n1, IXD))
dup1 <- .fl_app_dir("d1", IXD, name = "Same Name")
dup2 <- .fl_app_dir("d2", IXD, name = "Same Name")
.t3_ok("two apps with ONE name get two folders, not one",
       identical(basename(dup1), "Same Name") &&
       	identical(basename(dup2), "Same Name (2)"), basename(dup2))
legacy <- file.path(IXD, gsub("[^A-Za-z0-9._-]", "_", n2))
dir.create(legacy, recursive = TRUE, showWarnings = FALSE)
writeLines("x", file.path(legacy, "script.qvs"))
d2 <- .fl_app_dir(n2, IXD, name = "Renamed App")
.t3_ok("an id-named folder from before is RENAMED, never orphaned",
       !dir.exists(legacy) && file.exists(file.path(d2, "script.qvs")), d2)
rk <- .fl_dir_rekey(n2, "real-id-for-n2", IXD)
.t3_ok("rekey changes the key and keeps the folder",
       identical(.fl_app_dir("real-id-for-n2", IXD), d2) &&
       	file.exists(file.path(d2, "script.qvs")), rk)

unlink(TMP, recursive = TRUE)
cat("\n", sprintf("%d checks, %d failed", .T3_CHECKS, .T3_FAILS), "\n", sep = "")
if (.T3_FAILS > 0L) quit(status = 1L) else cat("FLEET M3 GREEN\n")
