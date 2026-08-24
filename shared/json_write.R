# json_write.R — a minimal JSON writer, shared.
#
# Base R has none, and `jsonlite` is a development-machine oracle that must
# not reach shipped tooling (INTERFACES.md). Only the shapes anything here
# needs are supported: a string, a character vector, and a named list of
# those.
#
# Promoted out of build_retarget_map.R on 2026-08-24 when csv_viewer.R needed
# the same writer. Sourced, never run.

json_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  # `<` becomes its < escape so the output can sit inside an HTML
  # <script> block: a literal closing script tag in a data value would
  # otherwise end the block early and turn the rest of the payload into
  # markup. Valid JSON either way, and it parses back to the same string.
  x <- gsub("<", "\\u003c", x, fixed = TRUE)
  # Remaining C0 controls would produce invalid JSON; drop them rather than
  # emit a file that will not parse. Qlik scripts do carry stray control
  # characters, so this is not hypothetical.
  gsub("[\001-\037]", "", x)
}
json_str <- function(x) paste0("\"", json_escape(as.character(x)), "\"")
json_arr <- function(x)
  if (!length(x)) "[]" else paste0("[", paste(json_str(x), collapse = ","), "]")
json_obj <- function(lst) {
  parts <- vapply(names(lst), function(k) {
    v <- lst[[k]]
    paste0(json_str(k), ":", if (length(v) == 1L && !is.list(v)) json_str(v) else json_arr(v))
  }, character(1))
  paste0("{", paste(parts, collapse = ","), "}")
}
