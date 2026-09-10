#!/usr/bin/env Rscript
# test_map_upkeep.R - offline gate for the M5 map-upkeep CLIs
# (map_add.R / map_check.R / map_refresh.R). Hand-run; exits 0 when green.
#
#     Rscript retargeting/test_map_upkeep.R
#
# Everything runs on SCRATCH COPIES in tempdir(): a six-row synthetic
# DBfixture, a two-schema loaded_schemas, an empty lineage_manual.csv and a
# hand-written map. Nothing under C:/Rtools is written, and no real fixture is
# read (they are 8.8MB and dev-PC-only), so this is safe to run at any stage.
# The one exception is a read-only schema check of the committed
# retargeting/lineage_manual.csv, which must stay the frozen 11-column shape.
#
# map_refresh.R's rebuild is exercised through its --builder seam with a stub
# that writes a canned map: the real builder takes ~26s and reads the big
# fixtures, and what is under test here is the DIFF, not the build.
#
# Base R only. styling/verify.R stays styling-only (same split as
# fleet/test_fleet.R).

.tm_argv <- commandArgs(trailingOnly = FALSE)
.tm_file <- sub("^--file=", "", .tm_argv[grepl("^--file=", .tm_argv)])
.TM_DIR  <- if (length(.tm_file) == 1 && nzchar(.tm_file))
  dirname(normalizePath(.tm_file, winslash = "/")) else getwd()
.TM_ROOT <- if (basename(.TM_DIR) == "retargeting") dirname(.TM_DIR) else getwd()
source(file.path(.TM_ROOT, "retargeting", "map_upkeep.R"))

.TM_RSCRIPT <- file.path(R.home("bin"), "Rscript")
.tm_checks <- 0L
.tm_fails  <- 0L

.tm_ok <- function(label, passed, detail = NULL) {
  .tm_checks <<- .tm_checks + 1L
  if (isTRUE(passed)) { cat("  ok   ", label, "\n", sep = ""); return(invisible(TRUE)) }
  .tm_fails <<- .tm_fails + 1L
  cat("  FAIL ", label, "\n", sep = "")
  for (d in detail) cat("       ", d, "\n", sep = "")
  invisible(FALSE)
}

# Run one upkeep CLI as a child process; returns status + captured output.
.tm_run <- function(script, args) {
  # NOT shQuote()d: system2() does its own quoting of `command` on Windows,
  # and quoting first gives it a doubly-quoted path it cannot find.
  out <- suppressWarnings(system2(.TM_RSCRIPT,
                                  c("--vanilla", shQuote(file.path(.TM_ROOT, "retargeting", script)), args),
                                  stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else as.integer(st), out = paste(out, collapse = "\n"))
}

.tm_write_csv <- function(d, path) write.csv(d, path, row.names = FALSE)

.tm_nrow <- function(path) nrow(read.csv(path, colClasses = "character"))

# --- scratch fixtures ----------------------------------------------------
.tm_make_db <- function(path) {
  d <- data.frame(
    TABLE_SCHEMA = c("TESTS", "TESTS", "TESTS", "TESTS", "OTHER"),
    TABLE_NAME   = c("V One", "V One", "V One", "T Two", "V Three"),
    TABLE_TYPE   = c("VIEW", "VIEW", "VIEW", "BASE TABLE", "VIEW"),
    COLUMN_NAME  = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
    ORDINAL_POSITION = c("1", "2", "3", "1", "1"),
    DATA_TYPE    = "varchar", stringsAsFactors = FALSE)
  .tm_write_csv(d, path)
}

.tm_make_schemas <- function(path) {
  .tm_write_csv(data.frame(Schema = c("TESTS", "LOADEDONLY"), Script = "x",
                           Comment = "y", stringsAsFactors = FALSE), path)
}

.tm_make_manual <- function(path) {
  d <- as.data.frame(setNames(replicate(length(MAP_LINEAGE_COLS), character(0), simplify = FALSE),
                              MAP_LINEAGE_COLS), stringsAsFactors = FALSE)
  .tm_write_csv(d, path)
}

# A map in the real 16-column shape. `dirty = TRUE` seeds one instance of
# every finding map_check.R looks for.
.tm_make_map <- function(path, dirty = FALSE) {
  d <- data.frame(
    source_app = "manual", onprem_qvd = c("TESTS/One.qvd", "TESTS/One.qvd"),
    onprem_qvd_temp = NA_character_, onprem_field = c("Alpha", "Beta"),
    verdict = "in-cloud", cloud_view_schema = "TESTS", cloud_view_name = "V One",
    cloud_qvd = "TESTS/V One.qvd", cloud_field = c("Alpha", "Beta"),
    source_database = "AzureDbProdNIAADL", source_schema = "TESTS", source_object = "V One",
    source_column = c("Alpha", "Beta"), evidence_file = "fixtures/DBfixture1.csv",
    evidence = "VIEW TESTS.V One has it (schema loaded)", truth_source = "manual",
    stringsAsFactors = FALSE)
  if (dirty) {
    dup <- d[1, ]                                            # duplicate-key
    gone <- d[1, ]; gone$onprem_field <- "Gamma"             # view-missing
    gone$cloud_view_name <- "V Nowhere"; gone$cloud_field <- "Gamma"
    unl <- d[1, ]; unl$onprem_field <- "Delta"               # schema-unloaded
    unl$cloud_view_schema <- "OTHER"; unl$cloud_view_name <- "V Three"
    noev <- d[1, ]; noev$onprem_field <- "Epsilon"           # empty-evidence
    noev$evidence <- ""
    d <- rbind(d, dup, gone, unl, noev)
  }
  .tm_write_csv(d, path)
}

# Stub rebuild script honouring only --out; writes `src` verbatim.
.tm_make_builder <- function(path, src) {
  writeLines(c(
    "a <- commandArgs(trailingOnly = TRUE)",
    "out <- a[match('--out', a) + 1L]",
    # forward slashes: a Windows path pasted into R source turns \U into an
    # escape ("'\U' used without hex digits") and the stub will not parse.
    sprintf("stopifnot(file.copy(%s, out, overwrite = TRUE))",
            shQuote(gsub("\\\\", "/", src)))), path)
}

# --- the suite -----------------------------------------------------------
.tm_main <- function() {
  tmp <- gsub("\\\\", "/", file.path(tempdir(), paste0("map_upkeep_", as.integer(Sys.time()))))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  db <- file.path(tmp, "db.csv"); sch <- file.path(tmp, "schemas.csv")
  man <- file.path(tmp, "lineage_manual.csv"); map <- file.path(tmp, "qvd_field_map.csv")
  .tm_make_db(db); .tm_make_schemas(sch); .tm_make_manual(man); .tm_make_map(map)
  common <- c("--db", shQuote(db), "--schemas", shQuote(sch),
              "--manual", shQuote(man), "--map", shQuote(map))
  cat("scratch: ", tmp, "\n\n", sep = "")

  cat("-- frozen file --\n")
  repo_manual <- file.path(.TM_ROOT, "retargeting", "lineage_manual.csv")
  .tm_ok("retargeting/lineage_manual.csv exists", file.exists(repo_manual))
  rm_ok <- tryCatch({ read_lineage_csv(repo_manual); TRUE }, error = function(e) conditionMessage(e))
  .tm_ok("it has the frozen 11-column lineage schema", isTRUE(rm_ok), rm_ok)

  cat("\n-- map_add --\n")
  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/New.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"), "--all-fields"))
  .tm_ok("--all-fields exits 0", r$status == 0L, r$out)
  .tm_ok("--all-fields appended 3 rows", .tm_nrow(man) == 3L, paste("rows:", .tm_nrow(man)))
  .tm_ok("appended rows are printed", grepl("+ TESTS/New.qvd | Alpha -> TESTS.V One.Alpha", r$out, fixed = TRUE), r$out)
  added <- read.csv(man, colClasses = "character")
  .tm_ok("rows carry status manual and the NIAADL connection",
         all(added$status == "manual") && all(added$db_connection == "AzureDbProdNIAADL"))

  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/New.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"), "--all-fields"))
  .tm_ok("re-adding the same keys exits 1", r$status == 1L, r$out)
  .tm_ok("...and says REFUSED", grepl("REFUSED", r$out, fixed = TRUE), r$out)
  .tm_ok("...and wrote nothing", .tm_nrow(man) == 3L, paste("rows:", .tm_nrow(man)))

  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("lib://AppData/PROD/AzureDataLake/TESTS/New.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"), "--fields", "Alpha"))
  .tm_ok("the same key as a lib:// path is refused too", r$status == 1L, r$out)

  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/New.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"), "--all-fields", "--force"))
  .tm_ok("--force appends anyway", r$status == 0L && .tm_nrow(man) == 6L, r$out)
  .tm_make_manual(man)

  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/Two.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"),
                              "--fields", shQuote("My Field=Alpha,Beta")))
  .tm_ok("--fields f=c round-trips", r$status == 0L && .tm_nrow(man) == 2L, r$out)
  got <- read.csv(man, colClasses = "character")
  .tm_ok("...mapping the named column", identical(got$qvd_field, c("My Field", "Beta")) &&
           identical(got$db_column, c("Alpha", "Beta")), paste(got$qvd_field, got$db_column))

  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/Three.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"), "--fields", "X=Nope"))
  .tm_ok("an unknown column is refused", r$status == 1L, r$out)
  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/Three.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V Nowhere"), "--all-fields"))
  .tm_ok("an unknown view is refused", r$status == 1L, r$out)
  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/Three.qvd"),
                              "--schema", "OTHER", "--view", shQuote("V Three"), "--all-fields"))
  .tm_ok("an unloaded schema warns but succeeds", r$status == 0L && grepl("WARN", r$out), r$out)
  r <- .tm_run("map_add.R", c(common, "--onprem-qvd", shQuote("TESTS/Four.qvd"),
                              "--schema", "TESTS", "--view", shQuote("V One"), "--all-fields", "--wat"))
  .tm_ok("an unknown option is an error", r$status == 1L, r$out)

  cat("\n-- map_check --\n")
  find <- file.path(tmp, "findings.csv")
  r <- .tm_run("map_check.R", c("--map", shQuote(map), "--db", shQuote(db),
                                "--schemas", shQuote(sch), "--out", shQuote(find)))
  .tm_ok("a clean map exits 0", r$status == 0L, r$out)
  .tm_ok("...and writes no findings file", !file.exists(find))

  dirty <- file.path(tmp, "dirty_map.csv"); .tm_make_map(dirty, dirty = TRUE)
  r <- .tm_run("map_check.R", c("--map", shQuote(dirty), "--db", shQuote(db),
                                "--schemas", shQuote(sch), "--out", shQuote(find)))
  .tm_ok("a dirty map exits 1", r$status == 1L, r$out)
  .tm_ok("...and writes the findings file", file.exists(find))
  if (file.exists(find)) {
    f <- read.csv(find, colClasses = "character")
    for (nm in c("duplicate-key", "view-missing", "schema-unloaded", "empty-evidence"))
      .tm_ok(paste("...finding present:", nm), sum(f$check == nm) > 0,
             paste(unique(f$check), collapse = ", "))
  }
  r <- .tm_run("map_check.R", c("--map", shQuote(map), "--db", shQuote(db),
                                "--schemas", shQuote(sch), "--out", shQuote(find)))
  .tm_ok("a clean re-run deletes the stale findings file", r$status == 0L && !file.exists(find), r$out)

  cat("\n-- dedupe (the builder's rule, tested on map_upkeep.R's function) --\n")
  dd <- read.csv(map, colClasses = "character")           # 2 rows, distinct keys
  clash <- dd[1, ]; clash$verdict <- "not-found"          # same key, disagreeing
  clash$source_object <- "T Two"; clash$evidence <- "searched it"
  dd2 <- map_dedupe(rbind(dd[1, ], clash, dd[2, ]))
  .tm_ok("dedupe keeps one row per key", nrow(dd2) == 2L, paste("rows:", nrow(dd2)))
  .tm_ok("...and it is the resolvable one",
         identical(dd2$verdict, c("in-cloud", "in-cloud")), paste(dd2$verdict, collapse = ", "))
  .tm_ok("...evidence records the drop",
         grepl("dedupe: dropped 1 row(s) with verdict not-found via source_object T Two",
               dd2$evidence[1], fixed = TRUE), dd2$evidence[1])
  .tm_ok("...a map with no duplicates is returned unchanged",
         identical(map_dedupe(dd), dd))

  cat("\n-- map_refresh --\n")
  same <- file.path(tmp, "canned_same.csv"); file.copy(map, same, overwrite = TRUE)
  stub_same <- file.path(tmp, "stub_same.R"); .tm_make_builder(stub_same, same)
  r <- .tm_run("map_refresh.R", c("--map", shQuote(map), "--builder", shQuote(stub_same),
                                  "--master-loads", shQuote(file.path(tmp, "absent.csv"))))
  .tm_ok("unchanged inputs exit 0", r$status == 0L, r$out)
  .tm_ok("...and print 0 flips", grepl("0 flips", r$out, fixed = TRUE), r$out)
  .tm_ok("...and leave a .prev copy", file.exists(file.path(tmp, "qvd_field_map.prev.csv")))
  refresh_csv <- file.path(tmp, sprintf("map_refresh_%s.csv", format(Sys.Date(), "%Y-%m-%d")))
  .tm_ok("...and write the dated refresh csv", file.exists(refresh_csv))
  if (file.exists(refresh_csv)) .tm_ok("...with 0 change rows", .tm_nrow(refresh_csv) == 0L)

  flipped <- file.path(tmp, "canned_flip.csv")
  fm <- read.csv(map, colClasses = "character"); fm$verdict[1] <- "not-found"
  .tm_write_csv(fm, flipped)
  stub_flip <- file.path(tmp, "stub_flip.R"); .tm_make_builder(stub_flip, flipped)
  ml <- file.path(tmp, "master_loads.csv")
  .tm_write_csv(data.frame(app_id = "app-1", app_name = "Test App", line = "10",
                           onprem_qvd = "tests/one.qvd", status = "retargeted",
                           cloud_target = "x", note = "", stringsAsFactors = FALSE), ml)
  r <- .tm_run("map_refresh.R", c("--map", shQuote(map), "--builder", shQuote(stub_flip),
                                  "--master-loads", shQuote(ml)))
  .tm_ok("a changed verdict exits 0", r$status == 0L, r$out)
  .tm_ok("...and reports 1 flip", grepl("1 flips", r$out, fixed = TRUE), r$out)
  .tm_ok("...names the affected fleet app", grepl("app-1", r$out, fixed = TRUE), r$out)
  .tm_ok("...and does not touch the manifest", grepl("manifest NOT edited", r$out, fixed = TRUE), r$out)
  if (file.exists(refresh_csv)) {
    ch <- read.csv(refresh_csv, colClasses = "character")
    .tm_ok("...one flip row, original path case kept",
           nrow(ch) == 1L && ch$kind[1] == "flip" && ch$onprem_qvd[1] == "TESTS/One.qvd",
           paste(ch$kind, ch$onprem_qvd))
  }

  broken <- file.path(tmp, "stub_broken.R")
  writeLines("stop('boom')", broken)
  before <- readLines(map, warn = FALSE)
  r <- .tm_run("map_refresh.R", c("--map", shQuote(map), "--builder", shQuote(broken)))
  .tm_ok("a failed rebuild exits 1", r$status == 1L, r$out)
  .tm_ok("...and restores the previous map", identical(before, readLines(map, warn = FALSE)))

  cat(sprintf("\n%d checks, %d failed\n", .tm_checks, .tm_fails))
  if (.tm_fails > 0L) 1L else { cat("MAP UPKEEP OK\n"); 0L }
}

if (!interactive()) quit(status = as.integer(.tm_main()))
