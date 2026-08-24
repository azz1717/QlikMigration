# csv_read.R — read a csv whose encoding is not guaranteed. Shared.
#
# The fixtures disagree with each other: most are UTF-8 or plain ASCII and
# `appcatalog.csv` is Windows-1252 (one 0x96 en dash). Re-saving the odd one
# out would hold only until the next extract, so the encoding is DETECTED per
# file instead of assumed or remembered.
#
# Guessing wrong is visible either way, never silent: a 1252 byte read as
# UTF-8 becomes an invalid string that later `grepl`/`sort` calls warn on, and
# UTF-8 read as 1252 shows mojibake.
#
# Promoted out of build_retarget_map.R on 2026-08-24 when csv_viewer.R needed
# the same reader. Sourced, never run.

file_encoding <- function(path) {
  raw <- readLines(path, warn = FALSE, encoding = "bytes")
  if (all(validUTF8(raw))) "UTF-8" else "Windows-1252"
}

#' `read.csv` with the encoding detected and everything left as text.
#'
#' Columns stay character deliberately: this repo's csvs carry ids, paths and
#' codes that R would otherwise coerce to numbers or factors, and a leading
#' zero lost in a load id is a silent wrong answer.
read_csv_any <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  fileEncoding = file_encoding(path))
}
