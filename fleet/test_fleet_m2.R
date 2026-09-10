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

# --- 4. stamp (M4; scheme reworked 2026-09-10 on Adam's answer to D4) ------
# Nobody sees an unbuilt app in the hub, so the tags are: the first scripts
# have run (progress), and what is left outstanding (one per flag).
.t2_section("stamp - progress + outstanding mig:* tags (section 7)")
.t2_reset_state()
.t2_reset_calls()
DRY_RUN <- TRUE
.t2_set_stage <- function(id, s)
	manifest_write(manifest_upsert(manifest_read(MANIFEST_DEFAULT),
	                               data.frame(app_id = id, stage = s,
	                                          stringsAsFactors = FALSE)),
	               MANIFEST_DEFAULT)
.t2_master <- function(inphinity, nprint)
	.fl_write_csv(data.frame(app_id = APP1, app_name = "01 ESS QVD Builder - CDP",
	                         flag_geoanalytics = 0, flag_inphinity = inphinity,
	                         flag_nprint = nprint, flag_unknown_src = 0,
	                         stringsAsFactors = FALSE), MASTER_CSV)

.t2_ok("a stage below retargeted earns NO progress tag",
       !nzchar(fleet_progress_tag("unbuilt")) && !nzchar(fleet_progress_tag("styled")))
.t2_ok("blocked earns none either - the ledger holds last_error",
       !nzchar(fleet_progress_tag("blocked")))
.t2_ok("retargeted means the first scripts have run: mig:processed",
       identical(fleet_progress_tag("retargeted"), "mig:processed"))
.t2_ok("built and verified name themselves",
       identical(fleet_progress_tag("built"), "mig:built") &&
       	identical(fleet_progress_tag("verified"), "mig:verified"))
.t2_ok("a flag's tag is its name with hyphens",
       identical(fleet_flag_tag("unknown_src"), "mig:unknown-src") &&
       	identical(fleet_flag_tag("inphinity"), "mig:inphinity"))

.t2_master(1, 2)
.t2_ok("one outstanding tag per NON-ZERO flag column, none for a zero",
       identical(fleet_outstanding_tags(APP1), c("mig:inphinity", "mig:nprint")),
       paste(fleet_outstanding_tags(APP1), collapse = " "))
.t2_ok("an app with no master.csv row has no outstanding tags",
       !length(fleet_outstanding_tags("local:nothing-like-this")))
.t2_ok("the wanted set is the progress tag plus the outstanding ones",
       identical(fleet_want_tags("retargeted", APP1),
                 c("mig:processed", "mig:inphinity", "mig:nprint")))
.t2_ok("an app below retargeted still says what is outstanding",
       identical(fleet_want_tags("styled", APP1), c("mig:inphinity", "mig:nprint")))

pl <- fleet_stamp_plan(c("mig:unbuilt", "mig:inphinity", "Finance curated"),
                       c("mig:processed", "mig:inphinity"))
.t2_ok("a mig: tag nobody wants goes - retired scheme included",
       identical(pl$remove, "mig:unbuilt"))
.t2_ok("the missing progress tag is added", identical(pl$add, "mig:processed"))
.t2_ok("an outstanding tag already in place is left alone",
       !("mig:inphinity" %in% c(pl$add, pl$remove)))
.t2_ok("a non-mig: collection is never touched",
       !("Finance curated" %in% pl$remove))
pl <- fleet_stamp_plan(c("mig:processed", "mig:inphinity"),
                       c("mig:processed", "mig:inphinity"))
.t2_ok("an already-correct item is a no-op",
       !length(pl$add) && !length(pl$remove))

.t2_set_stage(APP1, "styled")
.t2_set_stage(APP1, "retargeted")
rc <- fleet_main(c("stamp", "--apps", APP1))
.t2_ok("a dry stamp exits 0", rc == 0L)
.t2_ok("a dry stamp writes NO collection at all",
       !any(grepl("collection create|collection item", .t2_calls())),
       .t2_calls())
.t2_ok("a dry stamp writes no tags.csv either", !file.exists(TAGS_CSV))

item1 <- .t2_row(APP1)$item_id
DRY_RUN <- FALSE
# A tag from the retired scheme, put there by hand: stamp must clear it.
qc_write(c("collection", "item", "create", "--collectionId", "col000000001",
           "--id", item1), json = FALSE)
rc <- fleet_main(c("stamp", "--apps", APP1, "--live"))
.t2_ok("a live stamp exits 0", rc == 0L)
DRY_RUN <- FALSE
cur <- fleet_item_tags(item1)
.t2_ok("the item carries the progress tag and one tag per flag",
       setequal(cur$name, c("mig:processed", "mig:inphinity", "mig:nprint")),
       paste(cur$name, collapse = " "))
.t2_ok("an unknown mig: tag was removed by the same stamp",
       !("mig:unbuilt" %in% cur$name), paste(cur$name, collapse = " "))
tg <- fleet_tags_read(TAGS_CSV)
.t2_ok("tags.csv cached the collection ids",
       nrow(tg) >= 3L && all(nzchar(tg$collection_id)), paste(tg$tag, collapse = " "))

# A flag that clears loses its tag on the next stamp - that is the whole point
# of "what is left outstanding".
.t2_master(0, 2)
fleet_main(c("stamp", "--apps", APP1, "--live"))
DRY_RUN <- FALSE
cur <- fleet_item_tags(item1)
.t2_ok("clearing a flag removes ITS tag and only its tag",
       setequal(cur$name, c("mig:processed", "mig:nprint")),
       paste(cur$name, collapse = " "))

# Progress is exclusive: move the stage and the old progress tag goes.
.t2_set_stage(APP1, "built")
fleet_main(c("stamp", "--apps", APP1, "--live"))
DRY_RUN <- FALSE
cur <- fleet_item_tags(item1)
.t2_ok("a later stage replaces the earlier progress tag",
       setequal(cur$name, c("mig:built", "mig:nprint")),
       paste(cur$name, collapse = " "))

# An app that has not been retargeted yet carries no progress tag at all.
.t2_set_stage(APP1, "styled")
fleet_main(c("stamp", "--apps", APP1, "--live"))
DRY_RUN <- FALSE
cur <- fleet_item_tags(item1)
.t2_ok("dropping below retargeted leaves the outstanding tags only",
       setequal(cur$name, "mig:nprint"), paste(cur$name, collapse = " "))

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
.t2_section("reconcile - PROGRESS drift only, and the --adopt guard")
# The ledger is at styled and the item carries no progress tag: that is what
# the scheme ASKS for, outstanding tags notwithstanding.
rc <- fleet_main(c("reconcile", "--apps", APP1))
.t2_ok("reconcile exits 0 - drift is information, not failure", rc == 0L)
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("the drift report is a FILE with section 7's columns",
       identical(names(dr), c("app_id", "app_name", "item_id", "manifest_stage",
                              "tag_stage", "action")))
.t2_ok("outstanding tags are informational: no progress tag wanted, none there",
       identical(dr$action[1], "agree"), dr$action[1])
.t2_set_stage(APP1, "retargeted")
fleet_main(c("reconcile", "--apps", APP1))
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("a ledger ahead of the tenant reads as a MISSING progress tag",
       identical(dr$action[1], "no progress tag"), dr$action[1])

# Now the tag ahead of the ledger, which is the only direction --adopt moves.
fleet_main(c("stamp", "--apps", APP1, "--live"))
DRY_RUN <- FALSE
.t2_set_stage(APP1, "styled")
unlink(file.path(.fl_app_dir(APP1), "script_retargeted.qvs"))
fleet_main(c("reconcile", "--apps", APP1))
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("mig:processed against a styled ledger is drift",
       identical(dr$action[1], "drift") && identical(dr$tag_stage[1], "retargeted"),
       paste(dr$action[1], dr$tag_stage[1]))
fleet_main(c("reconcile", "--apps", APP1, "--adopt"))
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("--adopt REFUSES a stage whose local artefacts are missing",
       grepl("no local artefacts", dr$action[1]), dr$action[1])
.t2_ok("and the ledger did not move", identical(.t2_stage(APP1), "styled"))
writeLines("retargeted", file.path(.fl_app_dir(APP1), "script_retargeted.qvs"))
fleet_main(c("reconcile", "--apps", APP1, "--adopt"))
dr <- read_csv_any(TAG_DRIFT_CSV)
.t2_ok("--adopt moves the ledger once the artefacts are there",
       identical(dr$action[1], "adopted") && identical(.t2_stage(APP1), "retargeted"),
       paste(dr$action[1], .t2_stage(APP1)))
.t2_ok("a second reconcile now agrees", {
	fleet_main(c("reconcile", "--apps", APP1))
	identical(read_csv_any(TAG_DRIFT_CSV)$action[1], "agree")
})
.t2_ok("an outstanding tag is never read back as a stage",
       !nzchar(fleet_tag_stage(c("mig:inphinity", "mig:nprint"))) &&
       	identical(fleet_tag_stage(c("mig:processed", "mig:nprint")), "retargeted"))
.t2_ok("two progress tags resolve to no stage at all",
       !nzchar(fleet_tag_stage(c("mig:processed", "mig:built"))))
.t2_ok("a row with no item_id is reported, not skipped silently", {
	fleet_main(c("reconcile", "--apps", "local:nothing-like-this"))
	identical(read_csv_any(TAG_DRIFT_CSV)$action[1], "no item_id")
})
writeLines("styled", file.path(.fl_app_dir(APP1), "script_styled.qvs"))
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
# M3 built upload/verify (2026-09-10); `map` is still the only verb fleet.R
# names as somebody else's script.
.t2_ok("only the map verb still reports itself as unimplemented",
       identical(names(.FL_TODO), "map"), paste(names(.FL_TODO), collapse = " "))

unlink(TMP, recursive = TRUE)
cat("\n", sprintf("%d checks, %d failed", .T2_CHECKS, .T2_FAILS), "\n", sep = "")
if (.T2_FAILS > 0L) quit(status = 1L) else cat("FLEET M2/M4 GREEN\n")
