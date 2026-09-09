#!/usr/bin/env Rscript
# map_refresh.R - rebuild the qvd field map and report what MOVED
# (PLAN-fleet.md section 6, trigger A: a cloud view or schema now exists, so
# import-view / create-view / extend-view / not-found rows should flip to
# in-cloud).
#
# Usage:
#   Rscript retargeting/map_refresh.R [--db f] [--schemas f]
#       [--map f] [--builder f] [--rscript f] [--no-build]
#
#   --db/--schemas  passed straight through to build_qvd_field_map.R.
#   --builder       the rebuild script (default retargeting/build_qvd_field_map.R).
#   --rscript       Rscript.exe to run the child with (default: this one).
#   --no-build      diff the map against the existing .prev copy without
#                   rebuilding (re-report the last refresh).
#   --master-loads  fleet rollup CSV to name affected apps from (default
#                   fleet/master_loads.csv; M1 writes it).
#
# What it does, in order:
#   1. copy qvd_field_map.csv -> qvd_field_map.prev.csv
#   2. rebuild in place through the builder's --db/--schemas/--out overrides;
#      a failed rebuild restores the .prev copy and exits 1, so the map is
#      never left half-written.
#   3. diff prev vs new on (onprem_qvd, onprem_field) - verdict flips, added
#      keys, removed keys - print the verdict flips and write
#      retargeting/map_refresh_<YYYY-MM-DD>.csv.
#   4. if fleet/master_loads.csv exists (M1's rollup deliverable), print the
#      app ids whose load rows touch a flipped qvd. It does NOT edit the
#      manifest: stage changes belong to fleet.R rollup, and a reporting
#      script that quietly rewrites the ledger is how two sources of truth
#      start.
#
# Base R only. Reads fixtures, so dev PC only (D6).

.mr_argv <- commandArgs(trailingOnly = FALSE)
.mr_file <- sub("^--file=", "", .mr_argv[grepl("^--file=", .mr_argv)])
.MR_DIR  <- if (length(.mr_file) == 1 && nzchar(.mr_file))
  dirname(normalizePath(.mr_file, winslash = "/")) else getwd()
.MR_ROOT <- if (basename(.MR_DIR) == "retargeting") dirname(.MR_DIR) else getwd()
source(file.path(.MR_ROOT, "retargeting", "map_upkeep.R"))

.MR_KNOWN <- c("--db", "--schemas", "--map", "--builder", "--rscript", "--no-build",
               "--master-loads")

# One row per (onprem_qvd, onprem_field) key, carrying the path and field in
# their ORIGINAL case (the key itself is upper-cased, and a report that
# renamed every path would be useless). Duplicate keys are map_check.R's
# finding, not this script's: here their verdicts fold to a sorted signature
# so a diff still runs and reports honestly - "in-cloud|not-found" is
# visibly not one verdict.
.mr_index <- function(d) {
  k <- map_key(d$onprem_qvd, d$onprem_field)
  v <- vapply(split(d$verdict, k), function(x) paste(sort(unique(x)), collapse = "|"), character(1))
  first <- !duplicated(k)
  idx <- data.frame(key = k[first], onprem_qvd = d$onprem_qvd[first],
                    onprem_field = d$onprem_field[first], stringsAsFactors = FALSE)
  idx$verdict <- unname(v[idx$key])
  idx
}

.mr_main <- function(args) {
  map_check_flags(args, .MR_KNOWN)
  root <- .MR_ROOT
  map_path  <- map_opt(args, "--map",     file.path(root, "retargeting", "qvd_field_map.csv"))
  builder   <- map_opt(args, "--builder", file.path(root, "retargeting", "build_qvd_field_map.R"))
  rscript   <- map_opt(args, "--rscript", file.path(R.home("bin"), "Rscript"))
  db_path      <- map_opt(args, "--db")
  schemas_path <- map_opt(args, "--schemas")
  no_build  <- map_flag(args, "--no-build")

  if (!file.exists(map_path)) stop("map not found: ", map_path)
  prev_path <- sub("\\.csv$", ".prev.csv", map_path)

  if (!no_build) {
    if (!file.copy(map_path, prev_path, overwrite = TRUE))
      stop("could not copy ", map_path, " to ", prev_path)
    cat(sprintf("prev: %s\n", prev_path))
    cargs <- c(shQuote(builder), "--out", shQuote(map_path))
    if (!is.na(db_path))      cargs <- c(cargs, "--db", shQuote(db_path))
    if (!is.na(schemas_path)) cargs <- c(cargs, "--schemas", shQuote(schemas_path))
    cat(sprintf("rebuild: %s %s\n", basename(rscript), paste(cargs, collapse = " ")))
    # `command` is NOT shQuote()d: system2() quotes it itself on Windows and
    # a pre-quoted path comes back as "not found".
    log <- system2(rscript, cargs, stdout = TRUE, stderr = TRUE)
    st  <- attr(log, "status")
    if (!is.null(st) && st != 0L) {
      file.copy(prev_path, map_path, overwrite = TRUE)
      cat(paste(utils::tail(log, 20L), collapse = "\n"), "\n", sep = "")
      cat("\nrebuild FAILED; previous map restored.\n")
      return(1L)
    }
    cat(paste(utils::tail(log, 3L), collapse = "\n"), "\n", sep = "")
  } else if (!file.exists(prev_path)) {
    stop("--no-build needs an existing ", prev_path)
  }

  prev <- read.csv(prev_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  new  <- read.csv(map_path,  stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  pidx <- .mr_index(prev); nidx <- .mr_index(new)

  in_new  <- match(pidx$key, nidx$key)
  in_prev <- match(nidx$key, pidx$key)
  flip_p  <- which(!is.na(in_new) & pidx$verdict != nidx$verdict[in_new])
  add_n   <- which(is.na(in_prev))
  rm_p    <- which(is.na(in_new))
  n_flip  <- length(flip_p)

  mk <- function(kind, src, i, from, to) {
    if (length(i) == 0) return(NULL)
    data.frame(kind = kind, onprem_qvd = src$onprem_qvd[i], onprem_field = src$onprem_field[i],
               verdict_prev = from, verdict_new = to, stringsAsFactors = FALSE)
  }
  changes <- do.call(rbind, Filter(Negate(is.null), list(
    mk("flip",    pidx, flip_p, pidx$verdict[flip_p], nidx$verdict[in_new[flip_p]]),
    mk("added",   nidx, add_n,  NA_character_,        nidx$verdict[add_n]),
    mk("removed", pidx, rm_p,   pidx$verdict[rm_p],   NA_character_))))
  if (is.null(changes))
    changes <- data.frame(kind = character(0), onprem_qvd = character(0),
                          onprem_field = character(0), verdict_prev = character(0),
                          verdict_new = character(0), stringsAsFactors = FALSE)

  cat(sprintf("\n%d flips, %d added keys, %d removed keys (prev %d rows -> new %d rows)\n",
              n_flip, length(add_n), length(rm_p), nrow(prev), nrow(new)))
  if (n_flip > 0) {
    fl <- changes[changes$kind == "flip", , drop = FALSE]
    cat("\n---- verdict flips ----\n")
    print(table(paste(fl$verdict_prev, "->", fl$verdict_new)))
    print(utils::head(fl[, c("onprem_qvd", "onprem_field", "verdict_prev", "verdict_new")], 20L))
    if (nrow(fl) > 20L) cat(sprintf("... %d more\n", nrow(fl) - 20L))
  }

  out_path <- file.path(dirname(map_path), sprintf("map_refresh_%s.csv", format(Sys.Date(), "%Y-%m-%d")))
  write.csv(changes, out_path, row.names = FALSE)
  cat(sprintf("\nWrote %d change row(s) to %s\n", nrow(changes), out_path))

  ml_path <- map_opt(args, "--master-loads", file.path(root, "fleet", "master_loads.csv"))
  if (!file.exists(ml_path)) {
    cat("fleet/master_loads.csv absent (M1 writes it) -- no fleet apps to name.\n")
  } else {
    ml <- read.csv(ml_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
    if (!all(c("app_id", "onprem_qvd") %in% names(ml))) {
      cat("fleet/master_loads.csv has no app_id/onprem_qvd columns -- skipped.\n")
    } else if (n_flip == 0) {
      cat("0 flips -- no fleet apps affected.\n")
    } else {
      fq <- unique(toupper(changes$onprem_qvd[changes$kind == "flip"]))
      hit <- ml[toupper(trimws(ml$onprem_qvd)) %in% fq, , drop = FALSE]
      if (nrow(hit) == 0) {
        cat("no fleet app loads a flipped qvd.\n")
      } else {
        nm <- if ("app_name" %in% names(hit)) hit$app_name else rep("", nrow(hit))
        u <- unique(data.frame(app_id = hit$app_id, app_name = nm, stringsAsFactors = FALSE))
        cat(sprintf("\n%d fleet app(s) load a flipped qvd -- re-run `fleet.R process` for:\n", nrow(u)))
        for (i in seq_len(nrow(u))) cat(sprintf("  %s  %s\n", u$app_id[i], u$app_name[i]))
        cat("(manifest NOT edited: stage changes belong to fleet.R.)\n")
      }
    }
  }
  0L
}

if (!interactive()) {
  .mr_status <- tryCatch(.mr_main(commandArgs(trailingOnly = TRUE)),
                         error = function(e) { cat("ERROR: ", conditionMessage(e), "\n", sep = ""); 1L })
  quit(status = as.integer(.mr_status))
}
