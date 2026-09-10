# test_fleet_m2.R - the offline gate for PLAN-fleet.md M2 (fetch,
# reconcile-ids), M4 (stamp, reconcile) and decision D14 (the readiness
# denominator).
#
#     Rscript fleet/test_fleet_m2.R
#
# Exits 0 when every check passes, 1 otherwise. Hand-run, like
# fleet/test_fleet.R and fleet/test_fleet_m1.R: styling/verify.R stays
# styling-only and docs/verify_docs.R reads no fixtures.
#
# NO TENANT IS TOUCHED, and it is enforced rather than promised: QLIK_CLI_PATH
# points at diagnostics/mock_qlik.bat for the whole run, so even a bug that
# issued an unintended write could only reach the mock. MOCK_QLIK_LOG is what
# lets a dry run be proven to have issued NO call, rather than only to have
# printed something; MOCK_QLIK_STATE gives the mock the small piece of
# collection state a tag ROUND TRIP needs (stamp, then read the tag back).
#
# Every fleet path is redirected into tempdir() by reassigning the globals
# fleet.R reads at CALL time, so a run cannot touch the real ledger, the real
# fleet/apps/ or the real master CSVs. No file under C:/Rtools is written.

.t2_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.t2_root <- if (length(.t2_file))
	tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .t2_file[1])))),
	         error = function(e) NA_character_) else NA_character_
if (is.na(.t2_root) || !dir.exists(.t2_root))
	stop("run this as 'Rscript fleet/test_fleet_m2.R'", call. = FALSE)
setwd(.t2_root)

source("fleet/fleet.R")

.T2_CHECKS <- 0L
.T2_FAILS <- 0L
.t2_ok <- function(label, passed, detail = NULL) {
	.T2_CHECKS <<- .T2_CHECKS + 1L
	if (isTRUE(passed)) { cat("  ok   ", label, "\n", sep = ""); return(invisible(TRUE)) }
	.T2_FAILS <<- .T2_FAILS + 1L
	cat("  FAIL ", label, "\n", sep = "")
	for (d in detail) cat("       ", d, "\n", sep = "")
	invisible(FALSE)
}
.t2_section <- function(s) cat("\n== ", s, "\n", sep = "")

MOCK <- normalizePath("diagnostics/mock_qlik.bat", winslash = "\\", mustWork = TRUE)
Sys.setenv(QLIK_CLI_PATH = MOCK)

TMP <- file.path(tempdir(), paste0("fleetm2-", as.integer(Sys.time())))
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
STATE <- file.path(TMP, "mock_state.json")
Sys.setenv(MOCK_QLIK_LOG = CALLS, MOCK_QLIK_STATE = STATE)

.t2_calls <- function() if (file.exists(CALLS)) readLines(CALLS, warn = FALSE) else character(0)
.t2_reset_calls <- function() { unlink(CALLS); invisible(NULL) }
.t2_reset_state <- function() { unlink(STATE); invisible(NULL) }
.t2_stage <- function(id) {
	m <- manifest_read(MANIFEST_DEFAULT)
	k <- match(id, m$app_id)
	if (is.na(k)) NA_character_ else m$stage[k]
}
.t2_row <- function(id) {
	m <- manifest_read(MANIFEST_DEFAULT)
	m[match(id, m$app_id), , drop = FALSE]
}

# The mock's canned tenant (diagnostics/mock_qlik.R): three apps in space 1.
SPACE1 <- "sp00000000001"
APP1 <- "a0000000-0000-0000-0000-000000000001"   # "01 ESS QVD Builder - CDP"
APP2 <- "a0000000-0000-0000-0000-000000000002"   # "Grants QVD Generator"
GHOST <- "a0000000-0000-0000-0000-0000000000ff"  # id-shaped, not on the tenant

# --- 1. D14: the readiness denominator ------------------------------------
# The failure this closes: 14 generator apps read pct 0 with nothing left to
# do, because every one of their load rows was out-of-scope / commented /
# geospatial and section 5 divided by ALL of them.
.t2_section("D14 - pct_retargeted over ELIGIBLE loads only")
p <- fleet_pct_retargeted(c("out-of-scope", "commented", "geospatial",
                            "directory-statement"))
.t2_ok("the four ineligible statuses leave zero eligible loads", p$eligible == 0L)
.t2_ok("zero eligible scores 100, not 0 (nothing left to do)", p$pct == 100L)
.t2_ok("loads_total stays the RAW count", p$total == 4L)
p <- fleet_pct_retargeted(c("retargeted", "not-in-map"))
.t2_ok("half of two eligible loads is 50", p$pct == 50L && p$eligible == 2L)
p <- fleet_pct_retargeted(c("retargeted", "already-mapped", "commented",
                            "out-of-scope", "geospatial"))
.t2_ok("already-mapped counts as done", p$pct == 100L && p$done == 2L,
       paste("pct", p$pct, "done", p$done))
p <- fleet_pct_retargeted(c("retargeted", "retargeted-pending-import"))
.t2_ok("pending-import is eligible but NOT done", p$pct == 50L && p$eligible == 2L,
       paste("pct", p$pct))
.t2_ok("an unmeasured app (no retarget report) scores 0, not 100",
       fleet_pct_retargeted(character(0), measured = FALSE)$pct == 0L)
.t2_ok("an empty MEASURED report still scores 100",
       fleet_pct_retargeted(character(0))$pct == 100L)
.t2_ok("readiness still subtracts 10 per blocker kind on top",
       fleet_readiness(fleet_pct_retargeted(c("retargeted", "commented"))$pct,
                       "geoanalytics") == 90L)

# --- 2. fetch against the mock --------------------------------------------
.t2_section("fetch (M2) against diagnostics/mock_qlik.bat")
DRY_RUN <- TRUE
.t2_ok("add pulls the mock's space-1 apps into the ledger",
       fleet_main(c("add", "--space", SPACE1, "--all")) == 0L)
m <- manifest_read(MANIFEST_DEFAULT)
.t2_ok("three apps landed at stage listed",
       nrow(m) == 3L && all(m$stage == "listed"), paste("rows:", nrow(m)))
.t2_ok("add recorded an item_id for each (M4 needs it)", all(nzchar(m$item_id)))

.t2_reset_calls()
rc <- fleet_main(c("fetch", "--stage", "listed", "--no-rollup"))
.t2_ok("a dry fetch exits 0", rc == 0L)
.t2_ok("a dry fetch issues NO call at all",
       !any(grepl("app unbuild", .t2_calls(), fixed = TRUE)),
       .t2_calls())
.t2_ok("a dry fetch leaves every stage where it was",
       all(manifest_read(MANIFEST_DEFAULT)$stage == "listed"))
.t2_ok("a dry fetch is still AUDITED (the console is not the record)",
       any(grepl("DRYRUN", readLines(AUDIT_LOG, warn = FALSE), fixed = TRUE)))

rc <- fleet_main(c("fetch", "--stage", "listed", "--live", "--no-rollup"))
.t2_ok("a live fetch exits 0", rc == 0L)
d1 <- .fl_app_dir(APP1)
.t2_ok("the bundle landed under fleet/apps/<app_id>/",
       dir.exists(d1) && file.exists(file.path(d1, "script.qvs")) &&
       	file.exists(file.path(d1, "app-properties.json")))
.t2_ok("name.txt is written beside it", file.exists(file.path(d1, "name.txt")))
.t2_ok("every fetched row advanced to stage unbuilt",
       all(manifest_read(MANIFEST_DEFAULT)$stage == "unbuilt"),
       paste(manifest_read(MANIFEST_DEFAULT)$stage, collapse = " "))
.t2_ok("the live fetch really did call app unbuild",
       sum(grepl("app unbuild", .t2_calls(), fixed = TRUE)) == 3L)

# A D12 local: key has no tenant app behind it: a SKIP with a reason, never a
# blocked row and never a guessed id.
manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
                               data.frame(app_id = "local:not-on-tenant",
                                          app_name = "Not On Tenant",
                                          stage = "unbuilt",
                                          stringsAsFactors = FALSE)),
               MANIFEST_DEFAULT)
fleet_main(c("fetch", "--apps", "local:not-on-tenant", "--live", "--no-rollup"))
.t2_ok("a local: id is skipped, not blocked",
       identical(.t2_stage("local:not-on-tenant"), "unbuilt"))

# The reason fetch stages into a temp directory: qc_unbuild() deletes the
# directory it was given when a fetch fails, and an app's styled script,
# reports and log live in that directory.
manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
                               data.frame(app_id = GHOST, app_name = "Ghost",
                                          stage = "listed",
                                          stringsAsFactors = FALSE)),
               MANIFEST_DEFAULT)
gd <- .fl_app_dir(GHOST)
dir.create(gd, recursive = TRUE, showWarnings = FALSE)
writeLines("keep me", file.path(gd, "script_styled.qvs"))
rc <- fleet_main(c("fetch", "--apps", GHOST, "--live", "--no-rollup"))
.t2_ok("a failed fetch exits 2 (some rows blocked)", rc == 2L, paste("exit:", rc))
.t2_ok("a failed fetch blocks the row with a reason",
       identical(.t2_stage(GHOST), "blocked") && nzchar(.t2_row(GHOST)$last_error))
.t2_ok("a failed fetch does NOT destroy the app's existing artefacts",
       file.exists(file.path(gd, "script_styled.qvs")))

# --- 3. reconcile-ids (D12) -----------------------------------------------
.t2_section("reconcile-ids - local: keys matched to tenant ids (D12)")
listing <- data.frame(
	id = c("id-one", "id-two", "id-three"),
	name = c("Grants QVD Generator", "Twin Name", "Twin Name"),
	space_id = SPACE1, space_name = "On Prem Apps", stringsAsFactors = FALSE)
r <- fleet_reconcile_match("Grants QVD Generator", listing)
.t2_ok("a unique exact name match resolves",
       identical(r$status, "unique") && identical(r$id, "id-one"))
r <- fleet_reconcile_match("Twin Name", listing)
.t2_ok("two apps with the same name are AMBIGUOUS, never guessed",
       identical(r$status, "ambiguous") && r$n == 2L && !nzchar(r$id))
.t2_ok("no match at all is 'none'",
       identical(fleet_reconcile_match("Nothing Like This", listing)$status, "none"))
.t2_ok("matching is exact, not case-folded (578 duplicate names on tenant)",
       identical(fleet_reconcile_match("grants qvd generator", listing)$status, "none"))
.t2_ok("an empty listing is 'none', not an error",
       identical(fleet_reconcile_match("x", listing[0, , drop = FALSE])$status, "none"))

# End to end, against the mock: "Grants QVD Generator" is one of its apps.
LOCAL <- "local:grants-qvd-generator"
manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
                               data.frame(app_id = LOCAL,
                                          app_name = "Grants QVD Generator",
                                          stage = "unbuilt",
                                          notes = "id pending tenant match",
                                          stringsAsFactors = FALSE)),
               MANIFEST_DEFAULT)
ld <- .fl_app_dir(LOCAL)
dir.create(ld, recursive = TRUE, showWarnings = FALSE)
writeLines("marker", file.path(ld, "script_styled.qvs"))
manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
                               data.frame(app_id = "local:nothing-like-this",
                                          app_name = "Nothing Like This",
                                          stage = "unbuilt",
                                          stringsAsFactors = FALSE)),
               MANIFEST_DEFAULT)
# APP2 is already in the ledger from `add`, so drop it first: the point of
# this run is the local: row acquiring APP2's id, and a collision would (also
# correctly) be refused.
mm <- manifest_read(MANIFEST_DEFAULT)
manifest_write(mm[mm$app_id != APP2, , drop = FALSE], MANIFEST_DEFAULT)
# Its fetched directory goes too: reconcile-ids renames the local: directory
# onto the real id and deliberately does NOT overwrite one already there.
unlink(.fl_app_dir(APP2), recursive = TRUE)
rc <- fleet_main(c("reconcile-ids", "--space", SPACE1, "--no-rollup"))
.t2_ok("reconcile-ids exits 0", rc == 0L)
.t2_ok("the local: row now carries the real tenant id",
       !is.na(.t2_stage(APP2)), paste(manifest_read(MANIFEST_DEFAULT)$app_id,
                                      collapse = " "))
row2 <- .t2_row(APP2)
.t2_ok("its space fields were filled in from the listing",
       identical(row2$space_id, SPACE1) && nzchar(row2$space_name))
.t2_ok("its item_id was fetched in the same pass", nzchar(row2$item_id))
.t2_ok("the 'id pending tenant match' note is cleared", !nzchar(row2$notes))
.t2_ok("the app DIRECTORY was renamed, artefacts intact",
       file.exists(file.path(.fl_app_dir(APP2), "script_styled.qvs")) &&
       	!dir.exists(ld))
.t2_ok("an unmatched local: row keeps its key and says why",
       identical(.t2_stage("local:nothing-like-this"), "unbuilt") &&
       	grepl("no tenant app", .t2_row("local:nothing-like-this")$notes))
.t2_ok("reconcile-ids with nothing to reconcile exits 0",
       fleet_main(c("reconcile-ids", "--manifest", file.path(TMP, "empty.csv"),
                    "--no-rollup")) == 0L)
# A local: row and no space to search it in: a usage error, not a silent
# "matched nothing" - there is a real difference between looking and finding
# none, and never having looked.
NOSPACE <- file.path(TMP, "nospace.csv")
manifest_write(manifest_upsert(manifest_new(),
                               data.frame(app_id = "local:x", app_name = "X",
                                          stage = "unbuilt",
                                          stringsAsFactors = FALSE)), NOSPACE)
.t2_ok("reconcile-ids with a local: row but no space is a usage error",
       fleet_main(c("reconcile-ids", "--manifest", NOSPACE, "--no-rollup")) == 1L)

# --- 4. stamp (M4) --------------------------------------------------------
.t2_section("stamp - exclusive mig:* tags per item (section 7)")
.t2_reset_state()
.t2_reset_calls()
DRY_RUN <- TRUE
.t2_ok("the tag for a stage is mig:<stage>",
       identical(fleet_tag_for("retargeted"), "mig:retargeted"))
pl <- fleet_stamp_plan(c("mig:unbuilt", "Finance curated"), "mig:styled")
.t2_ok("stamping removes the OTHER mig: tag", identical(pl$remove, "mig:unbuilt"))
.t2_ok("stamping adds the wanted one", identical(pl$add, "mig:styled"))
.t2_ok("a non-mig: collection is never touched",
       !("Finance curated" %in% pl$remove))
pl <- fleet_stamp_plan(c("mig:styled"), "mig:styled")
.t2_ok("an already-correct item is a no-op",
       !length(pl$add) && !length(pl$remove))

rc <- fleet_main(c("stamp", "--apps", APP1))
.t2_ok("a dry stamp exits 0", rc == 0L)
.t2_ok("a dry stamp writes NO collection at all",
       !any(grepl("collection create|collection item", .t2_calls())),
       .t2_calls())
.t2_ok("a dry stamp writes no tags.csv either", !file.exists(TAGS_CSV))

rc <- fleet_main(c("stamp", "--apps", APP1, "--live"))
.t2_ok("a live stamp exits 0", rc == 0L)
item1 <- .t2_row(APP1)$item_id
DRY_RUN <- FALSE
cur <- fleet_item_tags(item1)
.t2_ok("the item now carries exactly one mig: tag", nrow(cur) == 1L,
       paste(cur$name, collapse = " "))
.t2_ok("and it is the ledger's own stage",
       identical(fleet_tag_stage(cur$name), .t2_stage(APP1)),
       paste("tag", fleet_tag_stage(cur$name), "stage", .t2_stage(APP1)))
tg <- fleet_tags_read(TAGS_CSV)
.t2_ok("tags.csv cached the collection id",
       nrow(tg) >= 1L && all(nzchar(tg$collection_id)))

# Exclusivity, the point of the scheme: move the stage and re-stamp.
manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
                               data.frame(app_id = APP1, stage = "styled",
                                          stringsAsFactors = FALSE)),
               MANIFEST_DEFAULT)
fleet_main(c("stamp", "--apps", APP1, "--live"))
cur <- fleet_item_tags(item1)
.t2_ok("re-stamping leaves exactly ONE mig: tag, the new one",
       nrow(cur) == 1L && identical(fleet_tag_stage(cur$name), "styled"),
       paste(cur$name, collapse = " "))

DRY_RUN <- FALSE
r <- qc_write(c("collection", "create", "--name", "mig:unbuilt", "--type", "public"))
.t2_ok("the mock refuses a duplicate collection name (409)",
       qc_failed(r) && any(grepl("409", r$out)), paste(r$out, collapse = " "))
g <- fleet_tag_id("mig:unbuilt", fleet_tags_read(file.path(TMP, "nosuch.csv")))
.t2_ok("fleet_tag_id recovers the existing id instead of failing",
       nzchar(g$id), paste("id:", g$id))
.t2_ok("a cache hit issues no listing call at all", {
	.t2_reset_calls()
	g2 <- fleet_tag_id("mig:unbuilt", g$tags)
	identical(g2$id, g$id) && !length(.t2_calls())
})

# --- 5. reconcile + --adopt ------------------------------------------------
.t2_section("reconcile - tag vs ledger drift, and the --adopt guard")
# The item is tagged mig:styled (above); put the ledger back to unbuilt so the
# tag is genuinely AHEAD, which is the only direction --adopt may move.
manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
                               data.frame(app_id = APP1, stage = "unbuilt",
                                          stringsAsFactors = FALSE)),
               MANIFEST_DEFAULT)
unlink(file.path(.fl_app_dir(APP1), "script_styled.qvs"))
rc <- fleet_main(c("reconcile", "--apps", APP1))
.t2_ok("reconcile exits 0 - drift is information, not failure", rc == 0L)
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("the drift report is a FILE with section 7's columns",
       identical(names(dr), c("app_id", "app_name", "item_id", "manifest_stage",
                              "tag_stage", "action")))
.t2_ok("it reports the disagreement", identical(dr$action[1], "drift"),
       paste(dr$action, collapse = " "))
fleet_main(c("reconcile", "--apps", APP1, "--adopt"))
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("--adopt REFUSES a stage whose local artefacts are missing",
       grepl("no local artefacts", dr$action[1]), dr$action[1])
.t2_ok("and the ledger did not move", identical(.t2_stage(APP1), "unbuilt"))
writeLines("styled", file.path(.fl_app_dir(APP1), "script_styled.qvs"))
fleet_main(c("reconcile", "--apps", APP1, "--adopt"))
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("--adopt moves the ledger once the artefacts are there",
       identical(dr$action[1], "adopted") && identical(.t2_stage(APP1), "styled"),
       paste(dr$action[1], .t2_stage(APP1)))
.t2_ok("a second reconcile now agrees", {
	fleet_main(c("reconcile", "--apps", APP1))
	identical(read_csv_any(TAG_DRIFT_CSV)$action[1], "agree")
})
.t2_ok("a row with no item_id is reported, not skipped silently", {
	fleet_main(c("reconcile", "--apps", "local:nothing-like-this"))
	identical(read_csv_any(TAG_DRIFT_CSV)$action[1], "no item_id")
})
.t2_ok("the artefact guard knows what each stage promises",
       fleet_stage_artefacts(.fl_app_dir(APP1), "styled") &&
       	!fleet_stage_artefacts(.fl_app_dir(APP1), "built") &&
       	!fleet_stage_artefacts(.fl_app_dir(APP1), "nonsense"))

# --- 6. CLI hygiene --------------------------------------------------------
.t2_section("CLI hygiene for the M2/M4 verbs")
.t2_ok("fetch without a selection is a usage error", fleet_main(c("fetch")) == 1L)
.t2_ok("stamp without a selection is a usage error", fleet_main(c("stamp")) == 1L)
.t2_ok("reconcile without a selection is a usage error",
       fleet_main(c("reconcile")) == 1L)
.t2_ok("--adopt is a known flag",
       isTRUE(fleet_parse_args(c("reconcile", "--all", "--adopt"))$opts[["adopt"]]))
.t2_ok("a mistyped --adop is still refused",
       nzchar(fleet_parse_args(c("reconcile", "--all", "--adop"))$error))
.t2_ok("the M2/M4 verbs no longer report themselves as unimplemented",
       !any(c("fetch", "stamp", "reconcile") %in% names(.FL_TODO)))
.t2_ok("upload and verify still do (M3 is not built)",
       all(c("upload", "verify") %in% names(.FL_TODO)))

unlink(TMP, recursive = TRUE)
cat("\n", sprintf("%d checks, %d failed", .T2_CHECKS, .T2_FAILS), "\n", sep = "")
if (.T2_FAILS > 0L) quit(status = 1L) else cat("FLEET M2/M4 GREEN\n")
