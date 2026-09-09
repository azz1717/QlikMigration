#!/usr/bin/env Rscript
# map_upkeep.R - plumbing shared by the qvd field map builder and the three
# map-upkeep CLIs (map_add.R, map_check.R, map_refresh.R). Sourced, never run.
#
# WHY IT EXISTS: PLAN-fleet.md section 6 adds three scripts that all need the
# SAME on-prem path normalisation and the SAME (onprem_qvd, onprem_field) key
# the rewriter keys on. build_qvd_field_map.R already owned both rules as
# private helpers. docs/verify_docs.R's "no duplicate twins" check turns that
# into exit status, so the rules were promoted here rather than copied - the
# same move retarget_shared.R made for the rewriter and the scorecard.
#
# Base R only.

# The 11-column schema every frozen lineage CSV shares (docs/INTERFACES.md,
# build_qvd_field_map.R). lineage_manual.csv is written to this schema too.
MAP_LINEAGE_COLS <- c("generator_app", "qvd_path_raw", "qvd_field", "db_connection",
                      "db_schema", "db_table", "db_column", "src_qvd_path",
                      "src_qvd_field", "status", "line")

# Byte that joins the two halves of a map key. Cannot occur in a path or a
# field name, so no separator collision is possible.
MAP_KSEP <- "\u0001"

#' Normalise an on-prem qvd path: relative-to-AzureDataLake, forward slashes,
#' name case kept verbatim (DESIGN section 6.7 Directive 1).
#'
#' Geospatial stores carry no /AzureDataLake/ segment
#' (lib://AppData\PROD\Geospatial\...), so a documented fallback strips at
#' /AppData/PROD/ instead and calls `on_fallback` (the builder counts them).
#' A lib:// path matching NEITHER marker is a hard STOP, never a guess.
#' An already-relative path is returned with its separators normalised only,
#' which is what lets a caller fold both forms to one key.
qvd_relativize <- function(xs, on_fallback = NULL) {
  vapply(xs, function(x) {
    if (is.na(x) || x == "") return(x)
    y <- gsub("\\\\", "/", x)
    if (!grepl("^lib://", y, ignore.case = TRUE)) return(y)
    m <- regexpr("AzureDataLake/", y, ignore.case = TRUE)
    if (m[1] > 0) return(substring(y, m[1] + attr(m, "match.length")))
    m2 <- regexpr("AppData/PROD/", y, ignore.case = TRUE)
    if (m2[1] > 0) {
      if (!is.null(on_fallback)) on_fallback()
      return(substring(y, m2[1] + attr(m2, "match.length")))
    }
    stop("STOP (Directive 1): path does not strip cleanly at '/AzureDataLake/' or the 'AppData/PROD/' fallback: ", x)
  }, character(1), USE.NAMES = FALSE)
}

#' The map's identity key, with the rewriter's own matching semantics:
#' retarget_loads.R resolves onprem_qvd case-INSENSITIVELY and then
#' onprem_field case-SENSITIVELY (docs/INTERFACES.md), so a duplicate-key
#' check that folded field case would refuse rows the rewriter treats as
#' distinct. Paths are relativized first so a lib:// row in a lineage file and
#' its relative twin in the map collide as they should.
map_key <- function(qvd, field) {
  paste(toupper(qvd_relativize(trimws(qvd))), trimws(ifelse(is.na(field), "", field)),
        sep = MAP_KSEP)
}

#' Read one frozen lineage CSV, schema-checked. Everything stays character
#' (ids, codes and paths must not be coerced); no encoding detection, because
#' these files are repo-owned ASCII and the builder's byte-for-byte output
#' depends on this exact read.
read_lineage_csv <- function(p) {
  if (!file.exists(p)) stop("STOP: lineage file missing: ", p)
  d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  if (!identical(names(d), MAP_LINEAGE_COLS))
    stop("STOP: unexpected schema in ", p, " -- got: ", paste(names(d), collapse = ","))
  d
}

#' Pull `--flag value` pairs out of a commandArgs() vector. Unknown `--flags`
#' are an ERROR, not a silent no-op: a typo in a batch script must not look
#' like a successful default run.
map_opt <- function(args, flag, default = NA_character_) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop("STOP: ", flag, " needs a value")
  args[i + 1L]
}

#' TRUE when a bare `--flag` (no value) is present.
map_flag <- function(args, flag) flag %in% args

#' Stop when `args` carries a `--flag` that is not in `known`.
map_check_flags <- function(args, known) {
  seen <- args[grepl("^--", args)]
  bad <- setdiff(seen, known)
  if (length(bad) > 0)
    stop("STOP: unknown option(s): ", paste(bad, collapse = " "),
         " (known: ", paste(known, collapse = " "), ")")
  invisible(TRUE)
}
