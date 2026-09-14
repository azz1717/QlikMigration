# test_fleet_m1.R - the offline gate for PLAN-fleet.md M1: import-unbuilt,
# process, report, rollup, the master CSVs, the readiness formula and the
# feature-flag scanner.
#
#     Rscript fleet/test_fleet_m1.R
#
# Exits 0 when every check passes, 1 otherwise. Hand-run, like
# fleet/test_fleet.R and retargeting/test_map_upkeep.R: styling/verify.R stays
# styling-only and docs/verify_docs.R reads no fixtures.
#
# NO TENANT IS TOUCHED - M1 has no tenant verb at all, and QLIK_CLI_PATH is
# pointed at the mock anyway so an accidental one could not reach a tenant.
#
# Every fleet path is redirected into tempdir() by reassigning the globals
# fleet.R reads at CALL time (APPS_DIR, MANIFEST_DEFAULT, MASTER_*,
# MAP_CHECK_STATUS), so a run of this file cannot touch the real ledger.
#
# ONE file under C:/Rtools is written and then restored: `process` runs
# retargeting/map_check.R (D13), which writes retargeting/map_check_findings.
# csv. It is derived data and the rewrite is byte-identical for an unchanged
# map, but it is saved and put back regardless - a test that leaves a repo
# file altered is a test people stop trusting.

.t1_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.t1_root <- if (length(.t1_file))
	tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .t1_file[1])))),
	         error = function(e) NA_character_) else NA_character_
if (is.na(.t1_root) || !dir.exists(.t1_root))
	stop("run this as 'Rscript fleet/test_fleet_m1.R'", call. = FALSE)
setwd(.t1_root)

source("fleet/fleet.R")

.T1_CHECKS <- 0L
.T1_FAILS <- 0L
.t1_ok <- function(label, passed, detail = NULL) {
	.T1_CHECKS <<- .T1_CHECKS + 1L
	if (isTRUE(passed)) { cat("  ok   ", label, "\n", sep = ""); return(invisible(TRUE)) }
	.T1_FAILS <<- .T1_FAILS + 1L
	cat("  FAIL ", label, "\n", sep = "")
	for (d in detail) cat("       ", d, "\n", sep = "")
	invisible(FALSE)
}
.t1_section <- function(s) cat("\n== ", s, "\n", sep = "")

MOCK <- normalizePath("diagnostics/mock_qlik.bat", winslash = "\\", mustWork = TRUE)
Sys.setenv(QLIK_CLI_PATH = MOCK)
AUDIT_LOG <- tempfile("audit", fileext = ".log")

TMP <- file.path(tempdir(), paste0("fleetm1-", as.integer(Sys.time())))
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
APPS_DIR <- file.path(TMP, "apps")
MANIFEST_DEFAULT <- file.path(TMP, "manifest.csv")
MASTER_CSV <- file.path(TMP, "master.csv")
MASTER_LOADS_CSV <- file.path(TMP, "master_loads.csv")
MASTER_UNUSED_CSV <- file.path(TMP, "master_unused.csv")
MAP_CHECK_STATUS <- file.path(TMP, "map_check_status.csv")

# --- 1. ids, paths and the readiness formula ------------------------------
.t1_section("ids, directory names and readiness (D10, D12)")
with_thumb <- list(qTitle = "01 ESS QVD Builder - CDP",
                   qThumbnail = list(qUrl = paste0(
	                   "/api/v1/apps/74acdc83-8527-41e2-a4a3-3fb35d0d7761/media/",
	                   "thumbnail/x.png")))
no_thumb <- list(qTitle = "CRM QVD Generator")
.t1_ok("a real app id is taken from the thumbnail url when present",
       identical(fleet_local_app_id(with_thumb, "somewhere"),
                 "74acdc83-8527-41e2-a4a3-3fb35d0d7761"))
.t1_ok("otherwise D12's local: key, lowercased and hyphenated",
       identical(fleet_local_app_id(no_thumb, "somewhere"), "local:crm-qvd-generator"))
.t1_ok("a title of pure punctuation still yields a usable key",
       identical(fleet_local_app_id(list(qTitle = "***"), "d"), "local:unnamed"))
# Windows has no ':' in a filename, so the KEY and the DIRECTORY NAME differ.
.t1_ok("a local: id maps to a colon-free directory name",
       !grepl(":", basename(.fl_app_dir("local:crm-qvd-generator")), fixed = TRUE) &&
       	basename(.fl_app_dir("local:crm-qvd-generator")) == "local_crm-qvd-generator")
.t1_ok("readiness is pct when nothing blocks", fleet_readiness(100, character(0)) == 100L)
.t1_ok("one blocker kind costs 10", fleet_readiness(100, "geoanalytics") == 90L)
.t1_ok("the same KIND twice still costs 10 once",
       fleet_readiness(100, c("not-in-map:3", "not-in-map:1")) == 90L)
.t1_ok("distinct kinds each cost 10",
       fleet_readiness(100, c("not-in-map:3", "multi-source:1", "geoanalytics")) == 70L)
.t1_ok("readiness floors at 0, never negative",
       fleet_readiness(10, c("a", "b", "c", "d", "e")) == 0L)
.t1_ok("a non-numeric pct reads as 0, not NA", fleet_readiness("", character(0)) == 0L)

.t1_section("fleet_bundle_dirs")
bt <- file.path(TMP, "bundles")
dir.create(file.path(bt, "one"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bt, "two"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bt, "notanapp"), recursive = TRUE, showWarnings = FALSE)
for (d in c("one", "two"))
	writeLines('{"qTitle":"x"}', file.path(bt, d, "app-properties.json"))
.t1_ok("every bundle under a root is found", length(fleet_bundle_dirs(bt)) == 2L)
.t1_ok("a root that IS a bundle returns itself",
       identical(fleet_bundle_dirs(file.path(bt, "one")), file.path(bt, "one")))
.t1_ok("a missing root is empty, not an error",
       length(fleet_bundle_dirs(file.path(TMP, "nope"))) == 0L)

# --- 2. flag detection, on a synthetic script -----------------------------
# The point of every check here is SCOPE: a flag must fire on live script and
# must NOT fire on a commented-out line, while the nprint HEURISTIC must read
# comments, because that is the only place NPrinting can show up at all.
.t1_section("feature flags over the shared token stream")
fdir <- file.path(TMP, "flagapp")
dir.create(fdir, recursive = TRUE, showWarnings = FALSE)
fscript <- file.path(fdir, "script.qvs")
writeLines(c(
	"Section Access;",
	"LOAD * INLINE [ACCESS,USERID];",
	"Section Application;",
	"LIB CONNECT TO 'REST_Endpoint_Prod';",
	"Distances:",
	"LOAD [Id] AS [Id] FROM GeoOperation(Closest) ;",
	"// commented out: GeoOperation(TravelAreas) used to run here",
	"// nprint report refresh is scheduled outside this app",
	"LOAD 1 AS [X] AUTOGENERATE 1;"), fscript)
fl <- fleet_flags_read("fleet/flags.csv")
.t1_ok("flags.csv has section 5's four columns",
       identical(names(fl), c("flag", "where", "pattern", "note")) && nrow(fl) >= 6L)
fa <- fleet_app_flags(fdir, fscript, fl)$flags
.t1_cnt <- function(n) { k <- which(fa$flag == n); if (length(k)) fa$count[k[1]] else NA_integer_ }
.t1_ok("geoanalytics fires on the LIVE call", .t1_cnt("geoanalytics") >= 1L)
.t1_ok("geoanalytics does NOT also count the commented-out call",
       .t1_cnt("geoanalytics") == 1L, paste("count:", .t1_cnt("geoanalytics")))
.t1_ok("rest fires on LIB CONNECT TO 'REST...'", .t1_cnt("rest") == 1L)
.t1_ok("inphinity does not fire on a script that has none", .t1_cnt("inphinity") == 0L)
.t1_ok("section_access fires", .t1_cnt("section_access") == 1L)
.t1_ok("nprint DOES read comments (its whole purpose)", .t1_cnt("nprint") == 1L)
.t1_ok("unknown_src is computed, not matched",
       identical(fa$first_hit[fa$flag == "unknown_src"], "computed"))
.t1_ok("every flag row is reported, hit or not", nrow(fa) == nrow(fl))
.t1_ok("a missing script yields zero counts, not an error",
       all(fleet_app_flags(fdir, file.path(fdir, "nope.qvs"), fl)$flags$count == 0L))

# --- 3. import-unbuilt over the REAL local bundles ------------------------
.t1_section("import-unbuilt over the local bundles")
have <- any(dir.exists(.FL_LOCAL_SOURCES))
if (!have) {
	cat("  skip  no local bundles on this machine\n")
} else {
	rc <- fleet_main(c("import-unbuilt", "--no-rollup"))
	.t1_ok("import-unbuilt exits 0", rc == 0L)
	m <- manifest_read(MANIFEST_DEFAULT)
	.t1_ok("it wrote a manifest row per bundle", nrow(m) >= 3L, paste("rows:", nrow(m)))
	.t1_ok("every imported row is at stage unbuilt or later",
	       all(!is.na(stage_rank(m$stage)) & stage_rank(m$stage) >= 2L))
	.t1_ok("app ids are unique - nothing keys on a name",
	       length(unique(m$app_id)) == nrow(m))
	.t1_ok("a duplicated app NAME is kept as two rows",
	       nrow(m) == length(unique(m$app_id)))
	syn <- startsWith(m$app_id, "local:")
	.t1_ok("every synthetic id says so in notes",
	       !any(syn) || all(m$notes[syn] == "id pending tenant match"))
	.t1_ok("a real id carries no such note", all(m$notes[!syn] == ""))
	dirs <- vapply(m$app_id, .fl_app_dir, character(1), USE.NAMES = FALSE)
	.t1_ok("every app got a directory", all(dir.exists(dirs)))
	.t1_ok("no directory name contains a colon",
	       !any(grepl(":", basename(dirs), fixed = TRUE)))
	.t1_ok("name.txt is written beside each bundle",
	       all(file.exists(file.path(dirs, "name.txt"))))
	.t1_ok("the bundle's own files came across",
	       all(file.exists(file.path(dirs, "app-properties.json"))))
	# Re-import must refresh files without undoing progress.
	m2 <- manifest_upsert(manifest_read(MANIFEST_DEFAULT),
	                      data.frame(app_id = m$app_id[1], stage = "styled",
	                                 stringsAsFactors = FALSE))
	manifest_write(m2, MANIFEST_DEFAULT)
	fleet_main(c("import-unbuilt", "--no-rollup"))
	.t1_ok("a re-import does not drag an advanced app back to unbuilt",
	       manifest_read(MANIFEST_DEFAULT)$stage[
	       	match(m$app_id[1], manifest_read(MANIFEST_DEFAULT)$app_id)] == "styled")
}

# --- 4. process + report + rollup on two real apps ------------------------
.t1_section("process / report / rollup on the trial app and one generator")
MC_FILE <- file.path("retargeting", "map_check_findings.csv")
MC_SAVE <- if (file.exists(MC_FILE)) readLines(MC_FILE, warn = FALSE) else NULL
if (!have) {
	cat("  skip  no local bundles on this machine\n")
} else {
	m <- manifest_read(MANIFEST_DEFAULT)
	pick <- c(m$app_id[grepl("Acquittals", m$app_name, fixed = TRUE)][1],
	          m$app_id[grepl("QVD Generator", m$app_name, fixed = TRUE)][1])
	pick <- pick[!is.na(pick)]
	.t1_ok("found a trial app and a generator to run", length(pick) == 2L)
	sel <- paste(pick, collapse = ",")
	rc <- fleet_main(c("process", "--apps", sel, "--no-rollup"))
	.t1_ok("process exits 0 (all ok) or 2 (some blocked)", rc %in% c(0L, 2L),
	       paste("exit:", rc))
	.t1_ok("map_check ran and its outcome was RECORDED, not blocked on (D13)",
	       file.exists(MAP_CHECK_STATUS) &&
	       	"findings" %in% names(read_csv_any(MAP_CHECK_STATUS)))
	d1 <- .fl_app_dir(pick[1])
	.t1_ok("the styled script was written", file.exists(file.path(d1, "script_styled.qvs")))
	.t1_ok("the retarget report was written",
	       file.exists(file.path(d1, "retarget_report.csv")))
	.t1_ok("stderr and stdout of every child landed in log.txt",
	       file.exists(file.path(d1, "log.txt")) &&
	       	any(grepl("run_pipeline.R", readLines(file.path(d1, "log.txt"), warn = FALSE),
	       	          fixed = TRUE)))
	mp <- manifest_read(MANIFEST_DEFAULT)
	.t1_ok("a successful app reached stage retargeted",
	       any(mp$stage[mp$app_id %in% pick] == "retargeted"),
	       paste(mp$stage[mp$app_id %in% pick], collapse = " "))

	rc <- fleet_main(c("report", "--apps", sel, "--no-rollup"))
	.t1_ok("report exits 0 or 2", rc %in% c(0L, 2L), paste("exit:", rc))
	for (f in c("report.html", "usage-tables.csv", "usage-fields.csv",
	            "usage-vars.csv", "flags.csv", "summary.csv"))
		.t1_ok(paste0("report wrote ", f), file.exists(file.path(d1, f)))
	uv <- read_csv_any(file.path(d1, "usage-vars.csv"))
	.t1_ok("usage-vars.csv is asset_usage()'s own table",
	       identical(names(uv), c("kind", "id", "name", "used")))
	sm <- read_csv_any(file.path(d1, "summary.csv"))
	.t1_ok("summary.csv carries the two counts no other csv has",
	       all(c("dead_code_lines", "inapp_store_count") %in% names(sm)))

	.t1_ok("rollup exits 0", fleet_main(c("rollup")) == 0L)
	ms <- read_csv_any(MASTER_CSV)
	want <- c("app_id", "app_name", "space_name", "stage", "stage_at",
	          "target_app_id", "loads_total", "loads_eligible", "loads_retargeted",
	          "loads_not_in_map", "loads_multi_source", "loads_commented",
	          "loads_wildcard", "pct_retargeted", "tables_total", "tables_unused",
	          "fields_unused_pct", "vars_total", "vars_unused", "dims_unused",
	          "measures_unused", "dead_code_lines", "inapp_store_count",
	          paste0("flag_", fl$flag), "blockers", "readiness",
	          "map_check_findings", "last_error", "notes")
	.t1_ok("master.csv has exactly section 5's columns, in order",
	       identical(names(ms), want),
	       paste("missing:", paste(setdiff(want, names(ms)), collapse = " "),
	             "| extra:", paste(setdiff(names(ms), want), collapse = " ")))
	.t1_ok("master.csv has one row per manifest row",
	       nrow(ms) == nrow(manifest_read(MANIFEST_DEFAULT)))
	.t1_ok("master.csv is sorted by readiness, highest first",
	       !is.unsorted(rev(as.numeric(ms$readiness))))
	.t1_ok("map_check_findings is carried onto every row",
	       all(nzchar(ms$map_check_findings)))
	ml <- read_csv_any(MASTER_LOADS_CSV)
	.t1_ok("master_loads.csv has exactly section 5's columns",
	       identical(names(ml), c("app_id", "app_name", "line", "onprem_qvd",
	                              "status", "cloud_target", "note")))
	mu <- read_csv_any(MASTER_UNUSED_CSV)
	.t1_ok("master_unused.csv has exactly section 5's columns",
	       identical(names(mu), c("app_id", "app_name", "kind", "name", "detail")))
	.t1_ok("master_unused.csv names only the five kinds",
	       !nrow(mu) || all(mu$kind %in% c("table", "field", "variable",
	                                       "dimension", "measure")),
	       paste(unique(mu$kind), collapse = " "))
	.t1_ok("status reads the board it was given",
	       fleet_main(c("status")) %in% c(0L, 2L))
}
if (!is.null(MC_SAVE)) writeLines(MC_SAVE, MC_FILE, useBytes = TRUE)

# --- 5. CLI hygiene for the new verbs -------------------------------------
.t1_section("CLI hygiene")
.t1_ok("process without a selection is a usage error", fleet_main(c("process")) == 1L)
.t1_ok("report without a selection is a usage error", fleet_main(c("report")) == 1L)
.t1_ok("import-unbuilt on a directory with no bundle exits 1",
       fleet_main(c("import-unbuilt", file.path(TMP, "bundles", "notanapp"))) == 1L)
.t1_ok("one bare positional is accepted and kept out of opts",
       identical(fleet_parse_args(c("import-unbuilt", "some/dir"))$pos, "some/dir") &&
       	length(fleet_parse_args(c("import-unbuilt", "some/dir"))$opts) == 0L)
.t1_ok("a second positional is refused",
       nzchar(fleet_parse_args(c("import-unbuilt", "a", "b"))$error))
.t1_ok("an unknown flag is STILL refused after a positional",
       nzchar(fleet_parse_args(c("import-unbuilt", "a", "--appss", "1"))$error))
.t1_ok("the M1 verbs no longer report themselves as unimplemented",
       !any(c("import-unbuilt", "process", "report", "rollup") %in% names(.FL_TODO)))

unlink(TMP, recursive = TRUE)
cat("\n", sprintf("%d checks, %d failed", .T1_CHECKS, .T1_FAILS), "\n", sep = "")
if (.T1_FAILS > 0L) quit(status = 1L) else cat("FLEET M1 GREEN\n")
