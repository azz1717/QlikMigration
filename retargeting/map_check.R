#!/usr/bin/env Rscript
# map_check.R - standing validator for retargeting/qvd_field_map.csv
# (PLAN-fleet.md section 6). Gates on exit status: 0 clean, 1 findings.
#
# WHY: the rewriter keys on (onprem_qvd, onprem_field) and takes the FIRST
# match, so a duplicate key is a silently wrong rewrite, not an error. No
# validator existed (PLAN section 0). Every check below turns one such silent
# class into exit status, and `fleet.R process` runs this first.
#
# Usage:
#   Rscript retargeting/map_check.R [--map f] [--db f] [--schemas f] [--out f]
#
# Checks:
#   duplicate-key    two rows share (onprem_qvd case-insens, onprem_field
#                    case-sens) - the rewriter's own matching semantics.
#   view-missing     verdict in-cloud / import-view / extend-view but
#                    (cloud_view_schema, cloud_view_name) is not an object in
#                    DBfixture1.csv.
#   schema-unloaded  verdict in-cloud but cloud_view_schema is not in
#                    loaded_schemas.csv - Cloud loads whole schemas, so such a
#                    row cannot actually be in cloud.
#   empty-evidence   evidence (or evidence_file) blank: an unfalsifiable row.
#
# Findings go to retargeting/map_check_findings.csv (columns: check,
# onprem_qvd, onprem_field, verdict, detail) and a per-check count prints. A
# clean run writes no file and deletes a stale one, so file and exit status
# can never disagree.
#
# Base R only. Reads fixtures, so dev PC only (D6).

.mc_argv <- commandArgs(trailingOnly = FALSE)
.mc_file <- sub("^--file=", "", .mc_argv[grepl("^--file=", .mc_argv)])
.MC_DIR  <- if (length(.mc_file) == 1 && nzchar(.mc_file))
  dirname(normalizePath(.mc_file, winslash = "/")) else getwd()
.MC_ROOT <- if (basename(.MC_DIR) == "retargeting") dirname(.MC_DIR) else getwd()
source(file.path(.MC_ROOT, "retargeting", "map_upkeep.R"))

.MC_KNOWN <- c("--map", "--db", "--schemas", "--out")
.MC_CLOUD_VERDICTS <- c("in-cloud", "import-view", "extend-view")

.mc_up <- function(x) toupper(trimws(x))
.mc_blank <- function(x) is.na(x) | !nzchar(trimws(x))

.mc_finding <- function(check, m, idx, detail) {
  if (length(idx) == 0) return(NULL)
  data.frame(check = check, onprem_qvd = m$onprem_qvd[idx], onprem_field = m$onprem_field[idx],
             verdict = m$verdict[idx], detail = detail, stringsAsFactors = FALSE)
}

.mc_main <- function(args) {
  map_check_flags(args, .MC_KNOWN)
  root <- .MC_ROOT
  map_path     <- map_opt(args, "--map",     file.path(root, "retargeting", "qvd_field_map.csv"))
  db_path      <- map_opt(args, "--db",      file.path(root, "fixtures", "DBfixture1.csv"))
  schemas_path <- map_opt(args, "--schemas", file.path(root, "fixtures", "loaded_schemas.csv"))
  out_path     <- map_opt(args, "--out",     file.path(root, "retargeting", "map_check_findings.csv"))

  if (!file.exists(map_path)) stop("map not found: ", map_path)
  m <- read.csv(map_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  need <- c("onprem_qvd", "onprem_field", "verdict", "cloud_view_schema", "cloud_view_name",
            "evidence_file", "evidence")
  miss <- setdiff(need, names(m))
  if (length(miss) > 0) stop("map is missing column(s): ", paste(miss, collapse = ", "))
  cat(sprintf("map: %s (%d rows)\n", map_path, nrow(m)))

  db1 <- read.csv(db_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  obj_set <- unique(paste(.mc_up(db1$TABLE_SCHEMA), .mc_up(db1$TABLE_NAME), sep = MAP_KSEP))
  loaded <- read.csv(schemas_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  loaded_set <- unique(.mc_up(loaded$Schema))

  found <- list()

  k <- map_key(m$onprem_qvd, m$onprem_field)
  dup_keys <- unique(k[duplicated(k)])
  dup_idx <- which(k %in% dup_keys)
  found$dup <- .mc_finding("duplicate-key", m, dup_idx,
                           sprintf("key appears %d times",
                                   as.integer(table(k)[k[dup_idx]])))

  cloudish <- m$verdict %in% .MC_CLOUD_VERDICTS
  named <- cloudish & !.mc_blank(m$cloud_view_schema) & !.mc_blank(m$cloud_view_name)
  vkey <- paste(.mc_up(m$cloud_view_schema), .mc_up(m$cloud_view_name), sep = MAP_KSEP)
  vm_idx <- which(named & !(vkey %in% obj_set))
  found$view <- .mc_finding("view-missing", m, vm_idx,
                            sprintf("%s.%s is not an object in %s",
                                    m$cloud_view_schema[vm_idx], m$cloud_view_name[vm_idx],
                                    basename(db_path)))
  nn_idx <- which(cloudish & !named)
  found$unnamed <- .mc_finding("view-missing", m, nn_idx,
                               sprintf("verdict %s with no cloud view named", m$verdict[nn_idx]))

  su_idx <- which(m$verdict == "in-cloud" & !.mc_blank(m$cloud_view_schema) &
                    !(.mc_up(m$cloud_view_schema) %in% loaded_set))
  found$schema <- .mc_finding("schema-unloaded", m, su_idx,
                              sprintf("schema %s is not in %s",
                                      m$cloud_view_schema[su_idx], basename(schemas_path)))

  ev_idx <- which(.mc_blank(m$evidence) | .mc_blank(m$evidence_file))
  found$ev <- .mc_finding("empty-evidence", m, ev_idx, "evidence or evidence_file is blank")

  findings <- do.call(rbind, Filter(Negate(is.null), found))
  if (is.null(findings)) findings <- data.frame(check = character(0), onprem_qvd = character(0),
                                                onprem_field = character(0), verdict = character(0),
                                                detail = character(0), stringsAsFactors = FALSE)

  cat("\n---- findings by check ----\n")
  for (nm in c("duplicate-key", "view-missing", "schema-unloaded", "empty-evidence"))
    cat(sprintf("  %-16s %d\n", nm, sum(findings$check == nm)))

  if (nrow(findings) == 0) {
    if (file.exists(out_path)) {
      file.remove(out_path)
      cat(sprintf("\nremoved stale %s\n", out_path))
    }
    cat("\nMAP CLEAN\n")
    return(0L)
  }
  write.csv(findings, out_path, row.names = FALSE)
  cat(sprintf("\nWrote %d finding(s) to %s\n", nrow(findings), out_path))
  cat("\n---- first 10 ----\n")
  print(utils::head(findings, 10L))
  1L
}

if (!interactive()) {
  .mc_status <- tryCatch(.mc_main(commandArgs(trailingOnly = TRUE)),
                         error = function(e) { cat("ERROR: ", conditionMessage(e), "\n", sep = ""); 1L })
  quit(status = as.integer(.mc_status))
}
