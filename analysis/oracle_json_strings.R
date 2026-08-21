# oracle_json_strings.R — cross-check the JSON string reader against jsonlite.
#
# Development-machine only, and not referenced by anything else in the repo:
# no pipeline script, no verify script and no phase 2 tooling sources this, so
# nothing breaks anywhere jsonlite is absent. Run it by hand:
#
#   Rscript oracle_json_strings.R [dir-or-file ...]
#
# With no arguments it walks every .json under app*-unbuilt/.
#
# The oracle is a real parser, so it agrees with json_strings.R only if the
# regex reader is genuinely equivalent on real input: same strings, same order,
# same key/value split, same escape handling.

source("analysis/json_strings.R")

# Every string in a parsed document, in document order, as the reader would
# report it. Keys come out before the value they name, which is the order they
# appear in the text. Non-string leaves (numbers, logicals, null) contribute
# nothing — the reader never sees them.
#
# simplifyVector = FALSE keeps every scalar its own list element; with
# simplification on, a string array would collapse into a character vector and
# an object into a data.frame, both of which lose position.
.oracle_strings <- function(x) {
  out <- list()

  walk <- function(node) {
    if (is.list(node)) {
      nms <- names(node)
      for (i in seq_along(node)) {
        if (!is.null(nms) && !is.na(nms[i]))
          out[[length(out) + 1L]] <<- list(text = nms[i], is_key = TRUE)
        walk(node[[i]])
      }
    } else if (is.character(node) && length(node) == 1L && !is.na(node)) {
      out[[length(out) + 1L]] <<- list(text = node, is_key = FALSE)
    }
  }
  walk(x)

  if (length(out) == 0L)
    return(data.frame(text = character(0), is_key = logical(0),
                      stringsAsFactors = FALSE))

  data.frame(text   = vapply(out, `[[`, character(1), "text"),
             is_key = vapply(out, `[[`, logical(1),   "is_key"),
             stringsAsFactors = FALSE)
}

# An empty JSON object carries no name vector, so `names()` is NULL and the
# key branch is skipped — correct, there is no key to report.
.oracle_read <- function(path) {
  .oracle_strings(jsonlite::fromJSON(path, simplifyVector = FALSE))
}

# Truncated so a mismatch inside a 2,600-line chart definition prints a hint,
# not the chart.
.brief <- function(s, n = 60L) {
  s <- gsub("[\r\n\t]", " ", s)
  if (nchar(s) > n) paste0(substr(s, 1L, n), "...") else s
}

check_file <- function(path) {
  mine <- read_json_strings(path)
  ref  <- .oracle_read(path)

  if (nrow(mine) != nrow(ref))
    return(sprintf("count: %d strings, oracle says %d", nrow(mine), nrow(ref)))

  bad <- which(mine$text != ref$text | mine$is_key != ref$is_key)
  if (length(bad) == 0L) return(NA_character_)

  i <- bad[1L]
  sprintf("%d of %d differ; first at #%d: [%s](key=%s) vs oracle [%s](key=%s)",
          length(bad), nrow(mine), i,
          .brief(mine$text[i]), mine$is_key[i],
          .brief(ref$text[i]),  ref$is_key[i])
}

main <- function(targets) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("jsonlite is not installed; this cross-check cannot run here.")

  if (length(targets) == 0L)
    targets <- list.dirs(".", recursive = FALSE)[
      grepl("app[0-9]*-unbuilt$", list.dirs(".", recursive = FALSE))]

  files <- unlist(lapply(targets, function(t)
    if (dir.exists(t)) list.files(t, pattern = "\\.json$", recursive = TRUE,
                                  full.names = TRUE) else t))

  if (length(files) == 0L) stop("no .json files found in: ",
                                paste(targets, collapse = ", "))

  failed <- 0L
  for (f in files) {
    msg <- tryCatch(check_file(f), error = function(e) paste("error:", conditionMessage(e)))
    if (!is.na(msg)) {
      failed <- failed + 1L
      cat(sprintf("FAIL %s\n     %s\n", f, msg))
    }
  }

  cat(sprintf("%d files checked, %d failed\n", length(files), failed))
  if (failed > 0L) quit(status = 1L)
  cat("READER MATCHES ORACLE\n")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
