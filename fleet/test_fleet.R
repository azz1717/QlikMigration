# test_fleet.R - the offline gate for shared/json_read.R, fleet/qlik_api.R and
# fleet/fleet.R. PLAN-fleet.md M0 acceptance.
#
#     Rscript fleet/test_fleet.R
#
# Exits 0 when every check passes, 1 otherwise. Hand-run, like
# styling/verify_substream.R: styling/verify.R stays styling-only.
#
# NO TENANT IS TOUCHED, and that is enforced rather than promised: the whole
# run points QLIK_CLI_PATH at diagnostics/mock_qlik.bat. The env var exists
# for exactly this reason - qlik_cli_path.txt is a per-machine, gitignored,
# hand-written file, and a test that rewrote it would wreck an operator's
# real setting and could leave a mock path behind on a VM.
#
# Every path this test writes to is a tempfile: the audit log, the mock's
# call log, the manifest and the spaces cache are all redirected by
# reassigning the globals that qlik_api.R and fleet.R read at CALL time.

.tf_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.tf_root <- if (length(.tf_file))
	tryCatch(dirname(dirname(normalizePath(sub("^--file=", "", .tf_file[1])))),
	         error = function(e) NA_character_) else NA_character_
if (is.na(.tf_root) || !dir.exists(.tf_root))
	stop("run this as 'Rscript fleet/test_fleet.R'", call. = FALSE)
setwd(.tf_root)

source("fleet/fleet.R")

.TF_CHECKS <- 0L
.TF_FAILS <- 0L
.tf_ok <- function(label, passed, detail = NULL) {
	.TF_CHECKS <<- .TF_CHECKS + 1L
	if (isTRUE(passed)) { cat("  ok   ", label, "\n", sep = ""); return(invisible(TRUE)) }
	.TF_FAILS <<- .TF_FAILS + 1L
	cat("  FAIL ", label, "\n", sep = "")
	for (d in detail) cat("       ", d, "\n", sep = "")
	invisible(FALSE)
}
.tf_section <- function(s) cat("\n== ", s, "\n", sep = "")
.tf_errmsg <- function(expr) tryCatch({ force(expr); "" },
                                      error = function(e) conditionMessage(e))

# --- 1. the parser, against the real bundles ------------------------------
.tf_section("json_read.R over every bundle *.json")
.tf_corpus <- function() {
	roots <- c("retargeting/unbuilt", "app-unbuilt", "app2-unbuilt",
	           "retargeting/05 - Grant Acquittals Report")
	roots <- roots[dir.exists(roots)]
	unlist(lapply(roots, function(r)
		list.files(r, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)),
		use.names = FALSE)
}
fs <- .tf_corpus()
.tf_ok(paste0("corpus found (", length(fs), " files)"), length(fs) > 0L,
       "no unbuilt bundles on this machine - the parser corpus check is empty")
bad <- character(0)
t0 <- Sys.time()
for (f in fs) {
	msg <- .tf_errmsg(json_read(f))
	if (nzchar(msg)) bad <- c(bad, paste(f, msg))
}
secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
.tf_ok(paste0("every bundle json parses (", secs, "s)"), length(bad) == 0L,
       utils::head(bad, 5L))
# A wall-clock ceiling is a real check here, not decoration: the first
# version of this parser worked and took 88s on this corpus (character-space
# offsets on a UTF-8 string are quadratic), which is the kind of slow that
# gets a tool quietly abandoned.
.tf_ok("corpus parses in under 20s", secs < 20)

.tf_section("json_read.R edge cases")
x <- json_parse('{"s":"a\\"b\\\\c\\/d\\be\\ff\\ng\\rh\\ti","u":"\\u00e9\\u0041",
                 "sur":"\\ud83d\\ude00","n":[0,-1,2.5,-3.75e2,1E+2],
                 "t":true,"f":false,"z":null,"o":{},"a":[],"deep":[[[{"k":[1]}]]]}')
.tf_ok("escapes decode", identical(x$s, "a\"b\\c/d\be\ff\ng\rh\ti"),
       paste("got:", x$s))
.tf_ok("\\uXXXX decodes", identical(utf8ToInt(x$u), c(233L, 65L)))
.tf_ok("surrogate pair decodes to one character",
       nchar(x$sur) == 1L && utf8ToInt(x$sur) == 128512L)
.tf_ok("numbers", isTRUE(all.equal(unlist(x$n), c(0, -1, 2.5, -375, 100))))
.tf_ok("true/false", isTRUE(x$t) && identical(x$f, FALSE))
.tf_ok("null is a PRESENT key holding NULL",
       "z" %in% names(x) && is.null(x$z))
.tf_ok("empty object is a named list, empty array is not",
       !is.null(names(x$o)) && length(x$o) == 0L &&
       	is.null(names(x$a)) && length(x$a) == 0L)
.tf_ok("nesting", identical(x$deep[[1]][[1]][[1]]$k[[1]], 1))
.tf_ok("json_get walks and gives up quietly",
       identical(json_get(x, "deep", 1, 1, 1, "k", 1), 1) &&
       	is.null(json_get(x, "nope", "deeper")))
.tf_ok("top-level array parses", length(json_parse(' [1,"two",null] ')) == 3L)
.tf_ok("top-level scalar parses", identical(json_parse(' "hi" '), "hi"))
for (bad_txt in c('{"a":1,}', '[1 2]', '{"a" 1}', '"unterminated',
                  '{"a":01}', '', '{"a":1}}', "{'a':1}", '[1,2', 'nul')) {
	.tf_ok(paste0("malformed input refused: ", substr(bad_txt, 1L, 14L)),
	       nzchar(.tf_errmsg(json_parse(bad_txt))))
}
.tf_ok("json_items finds items and does not descend into one",
       length(json_items(json_parse('{"data":[{"id":"a","resourceAttributes":{"id":"x"}},{"id":"b"}]}'))) == 2L)

# --- 2. qlik_api.R against the mock ---------------------------------------
.tf_section("qlik_api.R against diagnostics/mock_qlik.bat")
MOCK <- normalizePath("diagnostics/mock_qlik.bat", winslash = "\\", mustWork = TRUE)
CALL_LOG <- tempfile("mockcalls", fileext = ".txt")
AUDIT_LOG <- tempfile("audit", fileext = ".log")
Sys.setenv(QLIK_CLI_PATH = MOCK, MOCK_QLIK_LOG = CALL_LOG)
DRY_RUN <- TRUE
.tf_calls <- function() if (file.exists(CALL_LOG)) readLines(CALL_LOG, warn = FALSE) else character(0)
.tf_audit <- function() if (file.exists(AUDIT_LOG)) readLines(AUDIT_LOG, warn = FALSE) else character(0)

.tf_ok("qlik_exe() prefers QLIK_CLI_PATH", identical(qlik_exe(), MOCK))
ver <- qc(c("--version"), json = FALSE)
.tf_ok("a .bat resolves through system2 (--version answers)",
       !qc_failed(ver) && any(grepl("3.2.0-mock", ver, fixed = TRUE)),
       paste(ver, collapse = " | "))

sp <- qc_pages(c("space", "ls"))
.tf_ok("space ls pages to the end (2 pages, 4 spaces)",
       !qc_failed(sp) && length(sp) == 4L,
       paste("got", if (qc_failed(sp)) "failure" else length(sp)))
.tf_ok("paged items keep their fields",
       identical(.fl_str(sp[[1]][["name"]]), "On Prem Apps"))
.tf_ok("--limit is always sent",
       any(grepl("--limit", .tf_calls(), fixed = TRUE)))
.tf_ok("the second page was requested with --next",
       any(grepl("--next page2", .tf_calls(), fixed = TRUE)))

ap <- qc_pages(c("app", "ls", "--spaceId", "sp00000000001"))
.tf_ok("app ls pages (3 apps in the first space)",
       !qc_failed(ap) && length(ap) == 3L)
bogus <- qc(c("bogus", "verb"))
.tf_ok("a failing call is a value, not a crash",
       qc_failed(bogus) && bogus$status != 0L)
.tf_ok("every read is audited", length(.tf_audit()) >= 4L)

# --- 3. DRY_RUN is the guard, and it is airtight --------------------------
.tf_section("qc_write() under DRY_RUN")
script_f <- tempfile("script", fileext = ".qvs")
writeLines("LOAD 1 AS [X] AUTOGENERATE 1;", script_f)
calls_before <- length(.tf_calls())
audit_before <- length(.tf_audit())
DRY_RUN <- TRUE
res <- qc_write(c("app", "build", "--app", "a0000000-0000-0000-0000-000000000001",
                  "--script", script_f, "--no-reload"), json = FALSE, label = "test build")
.tf_ok("dry run returns nothing", is.null(res))
.tf_ok("dry run issued NO call to qlik", length(.tf_calls()) == calls_before)
.tf_ok("dry run still left an audit line",
       length(.tf_audit()) == audit_before + 1L &&
       	any(grepl("DRYRUN", utils::tail(.tf_audit(), 1L), fixed = TRUE)))

DRY_RUN <- FALSE
res2 <- qc_write(c("app", "build", "--app", "a0000000-0000-0000-0000-000000000001",
                   "--script", script_f, "--no-reload"), json = FALSE, label = "test build")
.tf_ok("live write runs the command",
       length(.tf_calls()) == calls_before + 1L &&
       	any(grepl("updated", res2, fixed = TRUE)))
.tf_ok("live write audits the exit status",
       any(grepl("WRITE", utils::tail(.tf_audit(), 1L), fixed = TRUE)) &&
       	any(grepl("exit=0", utils::tail(.tf_audit(), 1L), fixed = TRUE)))

DRY_RUN <- TRUE
ub_dir <- file.path(tempdir(), "bundle-dry")
.tf_ok("qc_unbuild honours DRY_RUN",
       is.null(qc_unbuild("a0000000-0000-0000-0000-000000000001", ub_dir)) &&
       	!dir.exists(ub_dir))
DRY_RUN <- FALSE
ub_dir2 <- file.path(tempdir(), "bundle-live")
unlink(ub_dir2, recursive = TRUE)
qc_unbuild("a0000000-0000-0000-0000-000000000001", ub_dir2)
.tf_ok("qc_unbuild writes a bundle",
       file.exists(file.path(ub_dir2, "app-properties.json")) &&
       	file.exists(file.path(ub_dir2, "script.qvs")))
.tf_ok("the mock's bundle parses with json_read()",
       identical(.fl_str(json_read(file.path(ub_dir2, "app-properties.json"))[["qTitle"]]),
                 "01 ESS QVD Builder - CDP"))
bad_ub <- qc_unbuild("no-such-app-id", file.path(tempdir(), "bundle-bad"))
.tf_ok("a failed unbuild leaves no half-written directory",
       qc_failed(bad_ub) && !dir.exists(file.path(tempdir(), "bundle-bad")))
DRY_RUN <- TRUE

# --- 4. the manifest rules ------------------------------------------------
.tf_section("manifest read/write/upsert and the stage enum")
MANIFEST <- tempfile("manifest", fileext = ".csv")
.tf_ok("a missing manifest reads as empty, not an error",
       nrow(manifest_read(MANIFEST)) == 0L &&
       	identical(names(manifest_read(MANIFEST)), MANIFEST_COLS))
m <- manifest_upsert(manifest_new(),
                     data.frame(app_id = "app-1", app_name = "One",
                                stringsAsFactors = FALSE))
.tf_ok("a new row defaults to stage listed and gets a timestamp",
       nrow(m) == 1L && m$stage == "listed" && nzchar(m$stage_at))
m <- manifest_upsert(m, data.frame(app_id = "app-1", stage = "unbuilt",
                                   stringsAsFactors = FALSE))
.tf_ok("one step forward is allowed", m$stage == "unbuilt")
m2 <- manifest_upsert(m, data.frame(app_id = "app-1", stage = "built",
                                    stringsAsFactors = FALSE))
.tf_ok("skipping forward is refused", m2$stage == "unbuilt")
m3 <- manifest_upsert(m, data.frame(app_id = "app-1", stage = "listed",
                                    stringsAsFactors = FALSE))
.tf_ok("going backward is allowed (map upkeep, re-fetch)", m3$stage == "listed")
m4 <- manifest_upsert(m, data.frame(app_id = "app-1", stage = "blocked",
                                    last_error = "no map row",
                                    stringsAsFactors = FALSE))
.tf_ok("blocked is reachable from any stage and keeps the error",
       m4$stage == "blocked" && m4$last_error == "no map row")
.tf_ok("blocked has no rank of its own", is.na(stage_rank("blocked")))
.tf_ok("an upsert never blanks a field it was not given",
       manifest_upsert(m4, data.frame(app_id = "app-1", stage = "unbuilt",
                                      stringsAsFactors = FALSE))$app_name == "One")
manifest_write(m4, MANIFEST)
.tf_ok("write then read round-trips",
       identical(manifest_read(MANIFEST)$app_id, m4$app_id) &&
       	identical(manifest_read(MANIFEST)$last_error, m4$last_error))

# --- 5. the verbs, end to end against the mock ----------------------------
.tf_section("verbs: spaces / apps / add / status")
MANIFEST <- tempfile("manifest2", fileext = ".csv")
SPACES_CSV <- tempfile("spaces", fileext = ".csv")
.tf_ok("spaces exits 0", fleet_main(c("spaces")) == 0L)
sc <- read_csv_any(SPACES_CSV)
.tf_ok("spaces caches every space with an index",
       nrow(sc) == 4L && identical(sc$index[1], "1"))
.tf_ok("spaces --type filters locally",
       fleet_main(c("spaces", "--type", "managed")) == 0L &&
       	nrow(read_csv_any(SPACES_CSV)) == 1L)
fleet_main(c("spaces"))
.tf_ok("apps exits 0 for a space index", fleet_main(c("apps", "--space", "4")) == 0L)
.tf_ok("apps without --space is a usage error", fleet_main(c("apps")) == 1L)
.tf_ok("add without a selection is a usage error",
       fleet_main(c("add", "--space", "4", "--manifest", MANIFEST)) == 1L)
.tf_ok("add --apps 1,3 exits 0",
       fleet_main(c("add", "--space", "4", "--apps", "1,3",
                    "--manifest", MANIFEST)) == 0L)
mm <- manifest_read(MANIFEST)
.tf_ok("add wrote 2 rows at stage listed",
       nrow(mm) == 2L && all(mm$stage == "listed"))
.tf_ok("add resolved the space name and the item id",
       all(nzchar(mm$space_name)) && all(startsWith(mm$item_id, "itm-")))
.tf_ok("add keyed on app id, not name",
       length(unique(mm$app_id)) == 2L && all(nzchar(mm$app_id)))
.tf_ok("add --all is idempotent and adds the rest",
       fleet_main(c("add", "--space", "4", "--all", "--manifest", MANIFEST)) == 0L &&
       	nrow(manifest_read(MANIFEST)) == 3L)
.tf_ok("add --name filters",
       fleet_main(c("add", "--space", "4", "--name", "Acquittals",
                    "--manifest", MANIFEST)) == 0L &&
       	nrow(manifest_read(MANIFEST)) == 3L)
.tf_ok("status exits 0 with nothing blocked",
       fleet_main(c("status", "--manifest", MANIFEST)) == 0L)
mb <- manifest_upsert(manifest_read(MANIFEST),
                      data.frame(app_id = manifest_read(MANIFEST)$app_id[1],
                                 stage = "blocked", last_error = "mock block",
                                 stringsAsFactors = FALSE))
manifest_write(mb, MANIFEST)
.tf_ok("status exits 2 when a row is blocked",
       fleet_main(c("status", "--manifest", MANIFEST, "--digest")) == 2L)

.tf_section("CLI hygiene")
.tf_ok("an unknown option is refused", fleet_main(c("status", "--appss", "1")) == 1L)
.tf_ok("a value flag with no value is refused", fleet_main(c("add", "--space")) == 1L)
.tf_ok("--dry-run and --live together are refused",
       fleet_main(c("status", "--dry-run", "--live")) == 1L)
.tf_ok("an unknown verb exits 1", fleet_main(c("frobnicate")) == 1L)
.tf_ok("a later-milestone verb exits 1 and says so",
       fleet_main(c("process")) == 1L && fleet_main(c("upload")) == 1L)
.tf_ok("help exits 0", fleet_main(c("help")) == 0L)
.tf_ok("parse: verb plus flags",
       identical(fleet_parse_args(c("add", "--space", "2", "--all"))$opts,
                 list(space = "2", all = TRUE)))

# --- 6. reply SHAPES: loud, not silent ------------------------------------
# Every shape fleet/ reads was INFERRED from qlik.dev (DESIGN 8.7). These
# checks are the mechanism that turns a wrong guess into a message instead of
# an empty listing - and `doctor` is the verb that runs them on purpose.
.tf_section("shape guards (qc_expect / qc_items) and doctor")
raw <- qc(c("--version"))               # answers plain text, not JSON
.tf_ok("an unparseable reply is a failure that SHOWS the text",
       qc_failed(raw) && any(grepl("unparseable", raw$out, fixed = TRUE)) &&
       	any(grepl("3.2.0-mock", raw$out[2], fixed = TRUE)),
       paste(utils::head(raw$out, 2L), collapse = " | "))
.tf_ok("the text is capped at 200 characters", nchar(raw$out[2]) <= 206L)

.tf_ok("qc_expect walks a nested path and takes the first path that hits",
       identical(qc_expect(json_parse('{"attributes":{"id":"x"}}'),
                           list(c("attributes", "id"), "id"), "app copy"), "x") &&
       	identical(qc_expect(json_parse('{"id":"y"}'),
                            list(c("attributes", "id"), "id"), "app copy"), "y"))
miss <- qc_expect(json_parse('{"qName2":"n","spaceId":"s"}'),
                  list("qName", "name"), "data-connection ls")
.tf_ok("a missing key names the call, the key and the keys received",
       qc_failed(miss) &&
       	grepl("data-connection ls", miss$out, fixed = TRUE) &&
       	grepl("qName` or `name", miss$out, fixed = TRUE) &&
       	grepl("qName2, spaceId", miss$out, fixed = TRUE),
       miss$out)
.tf_ok("a shape failure prints as a sentence, with no exit code in front",
       identical(.fl_fail_msg(miss), miss$out))
wrong <- qc_items(json_parse('{"items":[{"id":"a"}],"links":{}}'), "id", "app ls")
.tf_ok("rows under the wrong key are a failure, not an empty listing",
       qc_failed(wrong) &&
       	grepl("app ls: expected `data`, got keys: items, links", wrong$out, fixed = TRUE),
       if (qc_failed(wrong)) wrong$out else paste("got", length(wrong), "items"))
empty <- qc_items(json_parse('{"data":[],"links":{}}'), "id", "app ls")
.tf_ok("an empty data array is a listing with no rows, NOT an error",
       !qc_failed(empty) && length(empty) == 0L)
.tf_ok("a bare array is still a listing (collection ls, item collections)",
       length(qc_items(json_parse('[{"id":"c1","name":"mig:x"}]'), "id", "collection ls")) == 1L)

.tf_ok("doctor exits 0 against the mock",
       fleet_main(c("doctor", "--space", "sp00000000001")) == 0L)
.tf_ok("doctor exits 0 without a space (space-scoped checks skipped)",
       fleet_main(c("doctor")) == 0L)
Sys.setenv(MOCK_QLIK_SHAPE = "items")
.tf_ok("a listing under `items` fails loudly through qc_pages",
       qc_failed(qc_pages(c("space", "ls"))))
.tf_ok("doctor exits 1 when the tenant answers with the wrong shape",
       fleet_main(c("doctor", "--space", "sp00000000001")) == 1L)
Sys.unsetenv("MOCK_QLIK_SHAPE")
.tf_ok("and passes again once the shape is right",
       fleet_main(c("doctor", "--space", "sp00000000001")) == 0L)

cat("\n", sprintf("%d checks, %d failed", .TF_CHECKS, .TF_FAILS), "\n", sep = "")
cat("call log: ", CALL_LOG, "\naudit log: ", AUDIT_LOG, "\n", sep = "")
if (.TF_FAILS > 0L) quit(status = 1L) else cat("FLEET M0 GREEN\n")
