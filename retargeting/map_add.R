#!/usr/bin/env Rscript
# map_add.R - add hand-asserted lineage rows for a qvd the extractors never
# resolved (PLAN-fleet.md section 6, trigger B).
#
# The ONLY file this script writes is retargeting/lineage_manual.csv, the one
# hand-edited member of the frozen 11-column lineage family. The map itself is
# never touched here: rerun build_qvd_field_map.R (or map_refresh.R) to fold
# the new rows in. That separation is what keeps qvd_field_map.csv a pure
# function of its inputs.
#
# Usage:
#   Rscript retargeting/map_add.R --onprem-qvd <path> --schema S --view V
#       (--fields f1=c1,f2=c2 | --all-fields)
#       [--source-app A] [--force]
#       [--db f] [--schemas f] [--manual f] [--map f]
#
#   --onprem-qvd  on-prem qvd path, either relative ("ESS/CDP/X.QVD") or the
#                 full lib:// store path; normalised the same way the builder
#                 normalises every other path.
#   --fields      comma list of <qvd field>=<db column>; a bare name means the
#                 qvd field and the db column are spelled the same.
#   --all-fields  every column DBfixture1 lists for the view, 1:1.
#   --source-app  generator_app to record (default "manual").
#   --force       downgrade the refusals below to warnings and append anyway.
#
# Refuses (exit 1) when: the view is absent from DBfixture1; a named column is
# absent from it; any (onprem_qvd, onprem_field) key already exists in
# lineage_manual.csv or qvd_field_map.csv. Warns (never fatal) when the schema
# is not in loaded_schemas.csv, or the object exists but is not a VIEW.
#
# Base R only.

.ma_argv <- commandArgs(trailingOnly = FALSE)
.ma_file <- sub("^--file=", "", .ma_argv[grepl("^--file=", .ma_argv)])
.MA_DIR  <- if (length(.ma_file) == 1 && nzchar(.ma_file))
  dirname(normalizePath(.ma_file, winslash = "/")) else getwd()
.MA_ROOT <- if (basename(.MA_DIR) == "retargeting") dirname(.MA_DIR) else getwd()
source(file.path(.MA_ROOT, "retargeting", "map_upkeep.R"))

.MA_KNOWN <- c("--onprem-qvd", "--schema", "--view", "--fields", "--all-fields",
               "--source-app", "--force", "--db", "--schemas", "--manual", "--map")

# Connection every hand-added row is asserted against. The builder routes a
# row three-tier only when its db_connection is NIAADL or blank (anything else
# is verdict non-niaa-source), and a manual row exists precisely to say "this
# qvd's fields live in the NIAADL warehouse".
.MA_CONN <- "AzureDbProdNIAADL"

.ma_up <- function(x) toupper(trimws(x))

# "f1=c1,f2=c2" -> data.frame(field, column). A bare name means field == column.
.ma_parse_fields <- function(spec) {
  parts <- trimws(strsplit(spec, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) stop("--fields is empty")
  eq <- regexpr("=", parts, fixed = TRUE)
  fld <- ifelse(eq > 0, trimws(substr(parts, 1, eq - 1)), parts)
  col <- ifelse(eq > 0, trimws(substring(parts, eq + 1)), parts)
  if (any(!nzchar(fld)) || any(!nzchar(col)))
    stop("--fields entry with an empty side: ", paste(parts[!nzchar(fld) | !nzchar(col)], collapse = ", "))
  data.frame(field = fld, column = col, stringsAsFactors = FALSE)
}

.ma_main <- function(args) {
  map_check_flags(args, .MA_KNOWN)
  root <- .MA_ROOT
  qvd    <- map_opt(args, "--onprem-qvd")
  schema <- map_opt(args, "--schema")
  view   <- map_opt(args, "--view")
  spec   <- map_opt(args, "--fields")
  allf   <- map_flag(args, "--all-fields")
  force  <- map_flag(args, "--force")
  app    <- map_opt(args, "--source-app", "manual")
  db_path      <- map_opt(args, "--db",      file.path(root, "fixtures", "DBfixture1.csv"))
  schemas_path <- map_opt(args, "--schemas", file.path(root, "fixtures", "loaded_schemas.csv"))
  manual_path  <- map_opt(args, "--manual",  file.path(root, "retargeting", "lineage_manual.csv"))
  map_path     <- map_opt(args, "--map",     file.path(root, "retargeting", "qvd_field_map.csv"))

  if (is.na(qvd) || is.na(schema) || is.na(view))
    stop("--onprem-qvd, --schema and --view are all required")
  if (allf == !is.na(spec))
    stop("give exactly one of --fields or --all-fields")

  n_refusal <- 0L
  refuse <- function(msg) {
    if (force) { cat("WARN (forced): ", msg, "\n", sep = "") } else {
      cat("REFUSED: ", msg, "\n", sep = ""); n_refusal <<- n_refusal + 1L
    }
  }

  db1 <- read.csv(db_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  obj <- db1[.ma_up(db1$TABLE_SCHEMA) == .ma_up(schema) & .ma_up(db1$TABLE_NAME) == .ma_up(view), , drop = FALSE]
  if (nrow(obj) == 0) {
    refuse(sprintf("no object %s.%s in %s", schema, view, basename(db_path)))
  } else if (!any(.ma_up(obj$TABLE_TYPE) == "VIEW")) {
    cat("WARN: ", sprintf("%s.%s exists but is a %s, not a VIEW -- the builder will verdict it create-view\n",
                          schema, view, obj$TABLE_TYPE[1]), sep = "")
  }

  loaded <- read.csv(schemas_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  if (!(.ma_up(schema) %in% .ma_up(loaded$Schema)))
    cat("WARN: ", sprintf("schema %s is not in %s -- rows will verdict import-view, not in-cloud\n",
                          schema, basename(schemas_path)), sep = "")

  if (allf) {
    if (nrow(obj) == 0) stop("--all-fields needs the object to exist in DBfixture1")
    fields <- data.frame(field = obj$COLUMN_NAME, column = obj$COLUMN_NAME, stringsAsFactors = FALSE)
  } else {
    fields <- .ma_parse_fields(spec)
    unknown <- fields$column[!(.ma_up(fields$column) %in% .ma_up(obj$COLUMN_NAME))]
    if (length(unknown) > 0)
      refuse(sprintf("column(s) not in %s.%s: %s", schema, view, paste(unknown, collapse = ", ")))
  }
  if (any(duplicated(fields$field)))
    refuse(sprintf("repeated qvd field(s) in this call: %s",
                   paste(unique(fields$field[duplicated(fields$field)]), collapse = ", ")))

  new_keys <- map_key(rep(qvd, nrow(fields)), fields$field)

  existing <- read_lineage_csv(manual_path)
  if (nrow(existing) > 0) {
    hit <- new_keys %in% map_key(existing$qvd_path_raw, existing$qvd_field)
    if (any(hit)) refuse(sprintf("%d key(s) already in %s: %s", sum(hit), basename(manual_path),
                                 paste(utils::head(fields$field[hit], 5), collapse = ", ")))
  }
  if (file.exists(map_path)) {
    m <- read.csv(map_path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
    hit <- new_keys %in% map_key(m$onprem_qvd, m$onprem_field)
    if (any(hit)) refuse(sprintf("%d key(s) already in %s: %s", sum(hit), basename(map_path),
                                 paste(utils::head(fields$field[hit], 5), collapse = ", ")))
  }

  if (n_refusal > 0L) {
    cat(sprintf("\n%d refusal(s); nothing written. Re-run with --force to append anyway.\n", n_refusal))
    return(1L)
  }

  add <- data.frame(
    generator_app = app,
    qvd_path_raw  = qvd,
    qvd_field     = fields$field,
    db_connection = .MA_CONN,
    db_schema     = schema,
    db_table      = view,
    db_column     = fields$column,
    src_qvd_path  = NA_character_,
    src_qvd_field = NA_character_,
    status        = "manual",
    line          = NA_character_,
    stringsAsFactors = FALSE
  )
  write.csv(rbind(existing, add), manual_path, row.names = FALSE)

  cat(sprintf("Appended %d row(s) to %s\n", nrow(add), manual_path))
  shown <- utils::head(seq_len(nrow(add)), 50L)
  for (i in shown)
    cat(sprintf("  + %s | %s -> %s.%s.%s\n", add$qvd_path_raw[i], add$qvd_field[i],
                add$db_schema[i], add$db_table[i], add$db_column[i]))
  if (nrow(add) > length(shown)) cat(sprintf("  ... %d more\n", nrow(add) - length(shown)))
  cat("Rebuild the map (retargeting/map_refresh.R) to fold these in.\n")
  0L
}

if (!interactive()) {
  .ma_status <- tryCatch(.ma_main(commandArgs(trailingOnly = TRUE)),
                         error = function(e) { cat("ERROR: ", conditionMessage(e), "\n", sep = ""); 1L })
  quit(status = as.integer(.ma_status))
}
