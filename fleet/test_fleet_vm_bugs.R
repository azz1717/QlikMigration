# test_fleet_vm_bugs.R - the three faults Adam brought back from the VM
# (2026-09-23, troubleshoot/), each reproduced against the mock and shown
# fixed. Hand-run like the other fleet gates:
#
#     Rscript fleet/test_fleet_vm_bugs.R
#
# Exits 0 when every check passes, 1 otherwise. NO TENANT IS TOUCHED:
# QLIK_CLI_PATH points at diagnostics/mock_qlik.bat, and every fleet path is
# redirected into tempdir(), exactly as fleet/test_fleet_m3.R does.
#
#  1. A copy that does not land in the asked-for space must FAIL loudly,
#     naming where it went and what qlik said - not end "verified".
#  2. An upload that did not happen must not read as done: a build that exits
#     0 but prints Error, and a verify that used to re-check a stale target.
#  3. A folder name freed by deleting the folder must be reusable: index.csv
#     used to pin " (2)" on a same-named twin forever.
#  4. A failed download must fail the app: an old folder left on disk from
#     an earlier run used to be formatted and uploaded as if it were new.

.tv_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.tv_root <- if (length(.tv_file))
	tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .tv_file[1])))),
	         error = function(e) NA_character_) else NA_character_
if (is.na(.tv_root) || !dir.exists(.tv_root))
	stop("run this as 'Rscript fleet/test_fleet_vm_bugs.R'", call. = FALSE)
setwd(.tv_root)

source("fleet/fleet.R")

.TV_CHECKS <- 0L
.TV_FAILS <- 0L
.tv_ok <- function(label, passed, detail = NULL) {
	.TV_CHECKS <<- .TV_CHECKS + 1L
	if (isTRUE(passed)) { cat("  ok   ", label, "\n", sep = ""); return(invisible(TRUE)) }
	.TV_FAILS <<- .TV_FAILS + 1L
	cat("  FAIL ", label, "\n", sep = "")
	for (d in detail) cat("       ", d, "\n", sep = "")
	invisible(FALSE)
}
.tv_section <- function(s) cat("\n== ", s, "\n", sep = "")

MOCK <- normalizePath("diagnostics/mock_qlik.bat", winslash = "\\", mustWork = TRUE)
Sys.setenv(QLIK_CLI_PATH = MOCK)

TMP <- file.path(tempdir(), paste0("fleetvm-", as.integer(Sys.time())))
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
APPS_DIR <- file.path(TMP, "apps")
MANIFEST_DEFAULT <- file.path(TMP, "manifest.csv")
SELECTION_CSV <- file.path(TMP, "selection.csv")
MASTER_CSV <- file.path(TMP, "master.csv")
MASTER_LOADS_CSV <- file.path(TMP, "master_loads.csv")
MASTER_UNUSED_CSV <- file.path(TMP, "master_unused.csv")
MAP_CHECK_STATUS <- file.path(TMP, "map_check_status.csv")
SPACES_CSV <- file.path(TMP, "spaces.csv")
TAGS_CSV <- file.path(TMP, "tags.csv")
TAG_DRIFT_CSV <- file.path(TMP, "tag_drift.csv")
AUDIT_LOG <- file.path(TMP, "audit.log")
STORE <- file.path(TMP, "mock_apps")
Sys.setenv(MOCK_QLIK_APPS = STORE, MOCK_QLIK_LOG = file.path(TMP, "calls.log"))
.tv_env <- function(...) {
	Sys.unsetenv(c("MOCK_QLIK_COPY", "MOCK_QLIK_MOVE", "MOCK_QLIK_BUILD",
	               "MOCK_QLIK_TWIN", "MOCK_QLIK_FAIL"))
	if (length(list(...))) Sys.setenv(...)
}

SPACE1 <- "sp00000000001"   # "On Prem Apps"
SPACE3 <- "sp00000000003"   # "Migration Staging"
APP1 <- "a0000000-0000-0000-0000-000000000001"
APP3 <- "a0000000-0000-0000-0000-000000000003"   # "05 - Grant Acquittals Report"
TWIN <- "c0000000-0000-0000-0000-00000000000c"   # same name, MOCK_QLIK_TWIN
SCRIPT <- c("///$tab Main", "Sales:", "LOAD [Id] AS [Id]",
            "FROM [lib://AzureDataLake/curated/sales.qvd] (qvd);")

.tv_app <- function(id, name, files = c(script_retargeted.qvs = TRUE)) {
	manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
		data.frame(app_id = id, app_name = name, stage = "retargeted",
		           space_id = SPACE1, space_name = "On Prem Apps",
		           stringsAsFactors = FALSE)), MANIFEST_DEFAULT)
	d <- .fl_app_dir(id, name = name)
	dir.create(d, recursive = TRUE, showWarnings = FALSE)
	for (f in names(files)) writeLines(SCRIPT, file.path(d, f))
	d
}
.tv_res <- function(id) {
	r <- .fl_results_read(MANIFEST_DEFAULT)
	if (is.null(r)) return(NULL)
	k <- match(id, r$app_id)
	if (is.na(k)) NULL else r[k, , drop = FALSE]
}
.tv_quiet <- function(expr) {
	out <- character(0)
	rc <- NULL
	out <- capture.output(rc <- suppressMessages(expr), type = "output")
	list(rc = rc, out = out)
}

invisible(.tv_quiet(fleet_main(c("spaces"))))   # the space-name cache

# --- BUG 1 ---------------------------------------------------------------
.tv_section("bug 1 - a copy that lands outside the asked-for space")
.tv_env(MOCK_QLIK_COPY = "ignore-space", MOCK_QLIK_MOVE = "fail")
d1 <- .tv_app(APP1, "01 ESS QVD Builder - CDP")
x <- .tv_quiet(fleet_main(c("upload", "--mode", "copy", "--to-space", SPACE3,
                            "--apps", APP1, "--live", "--no-rollup")))
r <- .tv_res(APP1)
.tv_ok("upload exits 2, not 0", identical(x$rc, 2L), paste("exit", x$rc))
.tv_ok("the app is FAILED in this run's results", !is.null(r) && r$result == "FAILED",
       if (!is.null(r)) r$reason)
.tv_ok("the reason says where the copy really went, by name",
       grepl("NOT in \"Migration Staging\"", r$reason, fixed = TRUE) &&
       	grepl("\"Adam - Personal\"", r$reason, fixed = TRUE), r$reason)
.tv_ok("and quotes the tenant's refusal of the move",
       grepl("403 Forbidden", r$reason, fixed = TRUE), r$reason)
.tv_ok("nothing was built into the stray copy",
       !file.exists(file.path(STORE, "copy-a0000000-0001.qvs")))
lg <- readLines(file.path(d1, "upload.log"), warn = FALSE)
.tv_ok("upload.log holds the copy's full reply and the placement verdict",
       any(grepl('"attributes"', lg, fixed = TRUE)) &&
       	any(grepl("NOT in", lg, fixed = TRUE)), tail(lg, 5))
x <- .tv_quiet(fleet_main(c("verify", "--apps", APP1, "--live", "--no-rollup")))
.tv_ok("verify does not turn the failed copy into 'verified'",
       .tv_res(APP1)$result == "FAILED" &&
       	manifest_read(MANIFEST_DEFAULT)$stage[match(APP1,
       		manifest_read(MANIFEST_DEFAULT)$app_id)] != "verified")
x <- .tv_quiet(fleet_main(c("results", "--apps", APP1)))
.tv_ok("the results screen names it FAILED with the reason, within 72 columns",
       any(grepl("^FAILED  01 ESS QVD Builder - CDP", x$out)) &&
       	any(grepl("Adam - Personal", x$out)) && all(nchar(x$out) <= 72L) &&
       	identical(x$rc, 2L), x$out)

unlink(STORE, recursive = TRUE)
.tv_env(MOCK_QLIK_COPY = "ignore-space")
x <- .tv_quiet(fleet_main(c("upload", "--mode", "copy", "--to-space", SPACE3,
                            "--apps", APP1, "--live", "--no-rollup")))
r <- .tv_res(APP1)
.tv_ok("when the move is allowed, the copy is moved in and the app is ok",
       identical(x$rc, 0L) && r$result == "ok" &&
       	grepl("was moved in", r$reason, fixed = TRUE), c(x$rc, r$reason))
x <- .tv_quiet(fleet_main(c("verify", "--apps", APP1, "--live", "--no-rollup")))
.tv_ok("and verify then confirms the copy's script", .tv_res(APP1)$verified == "yes")

unlink(c(STORE, file.path(TMP, "calls.log")), recursive = TRUE)
.tv_env()
x <- .tv_quiet(fleet_main(c("upload", "--mode", "copy", "--to-space", SPACE3,
                            "--apps", APP1, "--live", "--no-rollup")))
r <- .tv_res(APP1)
.tv_ok("a tenant that honours the space: ok, no note, no move attempted",
       identical(x$rc, 0L) && r$result == "ok" && !nzchar(r$reason) &&
       	!any(grepl("app space update", readLines(file.path(TMP, "calls.log")),
       	           fixed = TRUE)), r$reason)

# --- BUG 2 ---------------------------------------------------------------
.tv_section("bug 2 - an upload that did not happen must not read as done")
.tv_env(MOCK_QLIK_BUILD = "error-exit0")
x <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP1,
                            "--live", "--no-rollup")))
r <- .tv_res(APP1)
.tv_ok("a build that exits 0 but prints Error is FAILED, with qlik's words",
       identical(x$rc, 2L) && r$result == "FAILED" &&
       	grepl("build exited 0 but said: Error: could not save", r$reason, fixed = TRUE),
       c(x$rc, r$reason))
# The ledger still holds target_app_id from the successful copy above. The
# old verify re-checked THAT and passed; this run's upload failed.
.tv_env()
x <- .tv_quiet(fleet_main(c("verify", "--apps", APP1, "--live", "--no-rollup")))
m <- manifest_read(MANIFEST_DEFAULT)
.tv_ok("verify ignores a stale target from an earlier run",
       .tv_res(APP1)$result == "FAILED" && m$stage[match(APP1, m$app_id)] == "blocked" &&
       	nzchar(m$last_error[match(APP1, m$app_id)]))

d3 <- .tv_app(APP3, "05 - Grant Acquittals Report", files = c(script_styled.qvs = TRUE))
x <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP3,
                            "--live", "--no-rollup")))
x <- .tv_quiet(fleet_main(c("verify", "--apps", APP3, "--live", "--no-rollup")))
r <- .tv_res(APP3)
.tv_ok("a styled-only upload is verified against the styled script it sent",
       r$result == "ok" && r$sent == "styled" && r$verified == "yes", unlist(r))
x <- .tv_quiet(fleet_main(c("results", "--apps", APP3)))
.tv_ok("and the results screen says it is not fully retargeted",
       any(grepl("^ok      05 - Grant Acquittals Report", x$out)) &&
       	any(grepl("sent the styled script", x$out, fixed = TRUE)), x$out)

# --- BUG 3 ---------------------------------------------------------------
.tv_section("bug 3 - ' (2)' must not outlive the folders")
IX <- file.path(TMP, "ix")
dir.create(IX, recursive = TRUE, showWarnings = FALSE)
da <- .fl_app_dir("orig", IX, name = "10 - Twin App"); dir.create(da)
db <- .fl_app_dir("twin", IX, name = "10 - Twin App"); dir.create(db)
.tv_ok("two live apps with one name: X and X (2), as before",
       basename(da) == "10 - Twin App" && basename(db) == "10 - Twin App (2)")
.tv_ok("with the folders present, prune keeps both entries",
       !nrow(.fl_index_prune(IX)) && nrow(.fl_index_read(IX)) == 2L)
unlink(c(da, db), recursive = TRUE)            # Adam clears fleet\apps
dropped <- .fl_index_prune(IX)
.tv_ok("once the folders are deleted, prune drops both entries",
       nrow(dropped) == 2L && nrow(.fl_index_read(IX)) == 0L)
.tv_ok("and the twin, fetched alone, gets the plain name back",
       basename(.fl_app_dir("twin", IX, name = "10 - Twin App")) == "10 - Twin App")

# End to end: the space really holds two apps with one name.
.tv_env(MOCK_QLIK_TWIN = "1")
x <- .tv_quiet(fleet_main(c("apps", "--space", SPACE1)))
tw <- grep("05 - Grant Acquittals Report", x$out, value = TRUE)
.tv_ok("the app list tells twins apart (id tail + date)",
       length(tw) == 2L && all(grepl("(twin: id ..", tw, fixed = TRUE)) &&
       	any(grepl("2026-09-23", tw, fixed = TRUE)), tw)
unlink(file.path(APPS_DIR, c("05 - Grant Acquittals Report", "05 - Grant Acquittals Report (2)")),
       recursive = TRUE)
k <- grep("05 - Grant Acquittals Report", tw)
idx <- sub("^\\[([0-9]+)\\].*$", "\\1", tw)
x <- .tv_quiet(fleet_main(c("add", "--space", SPACE1, "--apps", paste(idx, collapse = ","))))
x <- .tv_quiet(fleet_main(c("fetch", "--selected", "--live", "--no-rollup")))
ix <- .fl_index_read()
.tv_ok("both twins fetched: one plain, one (2)",
       setequal(ix$dir[ix$app_id %in% c(APP3, TWIN)],
                c("05 - Grant Acquittals Report", "05 - Grant Acquittals Report (2)")),
       ix$dir)
twin_dir <- ix$dir[match(TWIN, ix$app_id)]
unlink(file.path(APPS_DIR, ix$dir[ix$app_id %in% c(APP3, TWIN)]), recursive = TRUE)
twin_idx <- idx[grepl("2026-09-23", tw, fixed = TRUE)]
x <- .tv_quiet(fleet_main(c("add", "--space", SPACE1, "--apps", twin_idx)))
x <- .tv_quiet(fleet_main(c("fetch", "--selected", "--live", "--no-rollup")))
ix <- .fl_index_read()
.tv_ok("after the folders are cleared, the twin picked alone is NOT ' (2)'",
       identical(ix$dir[match(TWIN, ix$app_id)], "05 - Grant Acquittals Report"),
       c(paste("was:", twin_dir), paste("now:", ix$dir[match(TWIN, ix$app_id)])))
.tv_ok("and add cleared the previous run's upload results",
       is.null(.fl_results_read(MANIFEST_DEFAULT)))

# --- Adam's rulings, 2026-11-02 -------------------------------------------
.tv_section("ruling - live is the default, --dry-run is the opt-in")
.tv_env()
unlink(c(STORE, file.path(TMP, "calls.log")), recursive = TRUE)
x <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP1,
                            "--no-rollup")))
.tv_ok("upload with no flag really builds (no --live needed)",
       identical(x$rc, 0L) && file.exists(file.path(STORE, paste0(APP1, ".qvs"))) &&
       	identical(DRY_RUN, FALSE))
unlink(file.path(TMP, "calls.log"))
x <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP1,
                            "--no-rollup", "--dry-run")))
calls <- if (file.exists(file.path(TMP, "calls.log")))
	readLines(file.path(TMP, "calls.log")) else character(0)
.tv_ok("--dry-run issues no build and says how to do it for real",
       !any(grepl("app build", calls, fixed = TRUE)) &&
       	any(grepl("without --dry-run", x$out, fixed = TRUE)), c(calls, x$out))
x <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP1,
                            "--no-rollup", "--live")))
.tv_ok("--live is still accepted (no-op) and the next call is live again",
       identical(x$rc, 0L) && identical(DRY_RUN, FALSE))

.tv_section("ruling - no coverage check: write whatever was retargeted")
# A lib:// that the load scanner cannot see as a load (here a LET string,
# the shape that blocked 4 apps) used to quit(1) and lose every other load.
cov_in <- file.path(TMP, "cov_in.qvs")
writeLines(c("///$tab Main", "LET vRoot = 'lib://AzureDataLake/curated/';",
             "Sales:", "LOAD [Id] AS [Id]",
             "FROM [lib://AzureDataLake/curated/sales.qvd] (qvd);"), cov_in)
cov_out <- file.path(TMP, "cov_out.qvs")
cr <- .fl_child(c("3_retargeting/retarget_loads.R", cov_in, cov_out,
                  "--report", file.path(TMP, "cov_report.csv")),
                file.path(TMP, "cov.log"))
.tv_ok("retarget exits 0 when the scanner saw fewer lib:// than the text has",
       identical(cr$status, 0L) && file.exists(cov_out) && file.size(cov_out) > 0,
       c(paste("exit", cr$status), cr$out))
.tv_ok("and says the shortfall as a NOTE, not a failure",
       any(grepl("^NOTE: 2 lib:// occurrence", cr$out)) &&
       	!any(grepl("FAILED", cr$out, fixed = TRUE)), cr$out)

.tv_section("a failed download fails the app - the old folder is never sent")
# An app processed in an earlier run, so its folder holds an OLD script.
# This run's download fails: nothing may be formatted or uploaded from it.
.tv_env()
unlink(c(STORE, file.path(TMP, "calls.log")), recursive = TRUE)
dold <- .tv_app(APP1, "01 ESS QVD Builder - CDP",
                files = c(script.qvs = TRUE, script_retargeted.qvs = TRUE))
before <- unname(tools::md5sum(file.path(dold, "script_retargeted.qvs")))
.tv_env(MOCK_QLIK_FAIL = "app unbuild")
x <- .tv_quiet(fleet_main(c("fetch", "--apps", APP1, "--live", "--no-rollup")))
m <- manifest_read(MANIFEST_DEFAULT)
.tv_ok("fetch exits 2 and the row is blocked with the fetch error",
       identical(x$rc, 2L) && m$stage[match(APP1, m$app_id)] == "blocked" &&
       	startsWith(m$last_error[match(APP1, m$app_id)], "fetch: "),
       c(x$rc, m$last_error[match(APP1, m$app_id)]))
.tv_env()
x <- .tv_quiet(fleet_main(c("process", "--apps", APP1, "--no-rollup")))
m <- manifest_read(MANIFEST_DEFAULT)
.tv_ok("process leaves the old folder alone and the app still blocked",
       identical(x$rc, 2L) && !file.exists(file.path(dold, "script_styled.qvs")) &&
       	identical(unname(tools::md5sum(file.path(dold, "script_retargeted.qvs"))), before) &&
       	startsWith(m$last_error[match(APP1, m$app_id)], "fetch: "), x$rc)
x <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP1,
                            "--live", "--no-rollup")))
calls <- if (file.exists(file.path(TMP, "calls.log")))
	readLines(file.path(TMP, "calls.log")) else character(0)
r <- .tv_res(APP1)
.tv_ok("upload sends nothing for it and it is FAILED, saying why",
       identical(x$rc, 2L) && !any(grepl("app build", calls, fixed = TRUE)) &&
       	!is.null(r) && r$result == "FAILED" &&
       	grepl("download failed", r$reason, fixed = TRUE), c(x$rc, calls, r$reason))
x <- .tv_quiet(fleet_main(c("results", "--apps", APP1)))
.tv_ok("the results screen names it FAILED with the reason, within 72 columns",
       any(grepl("^FAILED  01 ESS QVD Builder - CDP", x$out)) &&
       	any(grepl("download failed", x$out, fixed = TRUE)) &&
       	# the detail path is this test's long tempdir, unbreakable by strwrap
       	all(nchar(x$out[!grepl("fleetvm-", x$out, fixed = TRUE)]) <= 72L),
       x$out)
x <- .tv_quiet(fleet_main(c("fetch", "--apps", APP1, "--live", "--no-rollup")))
x2 <- .tv_quiet(fleet_main(c("upload", "--mode", "overwrite", "--apps", APP1,
                             "--live", "--no-rollup")))
m <- manifest_read(MANIFEST_DEFAULT)
.tv_ok("a download that works again clears it, and the app uploads",
       identical(x$rc, 0L) && identical(x2$rc, 0L) &&
       	!nzchar(m$last_error[match(APP1, m$app_id)]) && .tv_res(APP1)$result == "ok",
       c(x$rc, x2$rc, m$last_error[match(APP1, m$app_id)]))

.tv_env()
unlink(TMP, recursive = TRUE)
cat("\n", sprintf("%d checks, %d failed", .TV_CHECKS, .TV_FAILS), "\n", sep = "")
if (.TV_FAILS > 0L) quit(status = 1L) else cat("FLEET VM BUGS GREEN\n")
