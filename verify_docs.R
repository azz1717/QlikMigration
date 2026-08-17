# verify_docs.R
#
# Documentation consistency checker - gates on exit status, like verify.R,
# but for PROSE. Reads no fixtures, so it is safe to run at ANY testing
# stage, any time, and costs zero model tokens: the cross-document sweep
# that the reading budget (CLAUDE.md) forbids doing by eye is done here by
# code instead.
#
# WHY THIS EXISTS
#
# Doc rot was structural, not a discipline failure (audited 2026-08-17,
# commit fc72e32): a fact lived in up to ten places, wrong prose cost
# nothing at write time, and the reading budget guaranteed nobody ever
# performed the sweep that would catch drift. Every check below converts
# one rot class from vigilance into exit status:
#
#   1. every DESIGN section citation resolves to a real heading
#      (ten stale ones fixed in fc72e32 would all have been caught)
#   2. the pass list agrees across run_pipeline.R, verify.R's PASSES,
#      and README's pass table (README was missed once, fixed a2f1a57)
#   3. every top-level function is documented in INTERFACES.md, and every
#      project-named function INTERFACES mentions actually exists
#   4. STATE.md respects its own 25-line cap
#   5. open-question markers live in STATE.md only, never in the spec
#
# Run before any commit:
#     Rscript verify_docs.R
#
# Vanilla base R only.

.checks <- 0L
.fails  <- 0L

ok <- function(label, passed, detail = NULL) {
  .checks <<- .checks + 1L
  if (isTRUE(passed)) return(invisible(TRUE))
  .fails <<- .fails + 1L
  cat("FAIL: ", label, "\n", sep = "")
  for (d in detail) cat("      ", d, "\n", sep = "")
  invisible(FALSE)
}

read_utf8 <- function(f) readLines(f, encoding = "UTF-8", warn = FALSE)

doc_files <- c("README.md", "DESIGN.md", "STATE.md", "INTERFACES.md", "CLAUDE.md")
r_files   <- list.files(".", pattern = "\\.R$")
scan_files <- c(doc_files[file.exists(doc_files)], r_files)

# --- 1. section citations resolve --------------------------------------
# Citations appear as "§4.5", "§4.1-§4.9" (both ends are their own token),
# "DESIGN §3.4/§4.5", "DESIGN 4.5/4.8" (verify.R comment style), "DESIGN 7".
check_citations <- function() {
  d <- read_utf8("DESIGN.md")
  valid <- c(sub("^## (\\d+)\\..*$",      "\\1", grep("^## \\d+\\.",      d, value = TRUE)),
             sub("^### (\\d+\\.\\d+).*$", "\\1", grep("^### \\d+\\.\\d+", d, value = TRUE)))
  pat  <- "(?:DESIGN(?:\\.md)? §?|§)\\d+(?:\\.\\d+)?(?:/§?\\d+(?:\\.\\d+)?)*"
  bad  <- character(0)
  for (f in scan_files) {
    lines <- read_utf8(f)
    m <- regmatches(lines, gregexpr(pat, lines, perl = TRUE))
    for (i in seq_along(m)) {
      if (length(m[[i]]) == 0) next
      toks <- unlist(regmatches(m[[i]], gregexpr("\\d+(?:\\.\\d+)?", m[[i]])))
      miss <- setdiff(toks, valid)
      if (length(miss) > 0)
        bad <- c(bad, sprintf("%s:%d cites §%s (no such DESIGN heading)",
                              f, i, paste(miss, collapse = ", §")))
    }
  }
  ok("every DESIGN section citation resolves", length(bad) == 0, bad)
}

# --- 2. the three pass lists agree --------------------------------------
check_pass_lists <- function() {
  rp <- read_utf8("run_pipeline.R")
  rp <- sub('^source\\("(.*)\\.R"\\).*$', "\\1",
            grep('^source\\("(ensure_|enforce_)', rp, value = TRUE))

  vf <- read_utf8("verify.R")
  a  <- grep("^PASSES <- list\\(", vf)
  b  <- a + grep("^\\)", vf[(a + 1):length(vf)])[1]
  vp <- regmatches(vf[a:b], regexpr('"[a-z_]+"', vf[a:b]))
  vp <- gsub('"', "", vp)

  rd <- read_utf8("README.md")
  rd <- grep("^\\| *\\d+ *\\|", rd, value = TRUE)
  rp_readme <- gsub("`", "", regmatches(rd, regexpr("`[a-z_]+`", rd)))

  ok("pass list: run_pipeline.R == verify.R PASSES", identical(rp, vp),
     c(paste("run_pipeline:", paste(rp, collapse = " ")),
       paste("verify.R:    ", paste(vp, collapse = " "))))
  ok("pass list: run_pipeline.R == README table", identical(rp, rp_readme),
     c(paste("run_pipeline:", paste(rp, collapse = " ")),
       paste("README:      ", paste(rp_readme, collapse = " "))))
}

# --- 3. INTERFACES.md <-> code ------------------------------------------
check_interfaces <- function() {
  iface <- paste(read_utf8("INTERFACES.md"), collapse = "\n")

  # Two scopes. "Must be documented" covers PUBLIC pipeline functions only:
  # dot-prefixed helpers are private implementation, and the gate scripts'
  # internals (verify.R, verify_docs.R) are not contract - their file-level
  # entries suffice. "Must exist" (the stale check below) covers definitions
  # from EVERY file, so a verify.R function cited by INTERFACES still counts.
  grab_defs <- function(fs) {
    defs <- character(0)
    for (f in fs) {
      lines <- read_utf8(f)
      defs <- c(defs, sub("^(\\.?[A-Za-z][A-Za-z0-9._]*) *<- *function.*$", "\\1",
               grep("^\\.?[A-Za-z][A-Za-z0-9._]* *<- *function", lines, value = TRUE)))
    }
    defs
  }
  defs <- grab_defs(setdiff(r_files, c("run_pipeline.R", "verify.R", "verify_docs.R")))
  all_defs <- c(defs, grab_defs(intersect(r_files, c("verify.R", "verify_docs.R"))))
  pub <- defs[!startsWith(defs, ".")]
  undoc <- pub[!vapply(pub, function(d)
    grepl(paste0(d, "("), iface, fixed = TRUE), logical(1))]
  ok("every top-level function appears in INTERFACES.md", length(undoc) == 0,
     paste("undocumented:", paste(undoc, collapse = ", ")))

  # reverse: project-named functions INTERFACES mentions must exist.
  # Scoped to names with a "_" or a leading "." (this project's namespaces)
  # so base R mentioned in prose can't false-positive.
  mentions <- unlist(regmatches(iface, gregexpr("`\\.?[A-Za-z][A-Za-z0-9._]*\\(", iface)))
  mentions <- unique(sub("^`", "", sub("\\($", "", mentions)))
  mentions <- mentions[grepl("_", mentions) | startsWith(mentions, ".")]
  stale <- setdiff(mentions, all_defs)
  ok("every project function INTERFACES.md mentions exists in code",
     length(stale) == 0, paste("stale:", paste(stale, collapse = ", ")))
}

# --- 4. STATE.md line cap ------------------------------------------------
check_state_cap <- function() {
  n <- length(read_utf8("STATE.md"))
  ok(sprintf("STATE.md within its 25-line cap (has %d)", n), n <= 25)
}

# --- 5. open questions live in STATE.md only -----------------------------
check_no_tbd <- function() {
  # Widened 2026-08-17 after the first version matched only "not yet
  # specified" and missed a verify.R test label reading "not yet confirmed
  # with Adam" - a convention Adam had already signed off. An open-question
  # marker is a marker whatever words it uses; match the family, not a phrase.
  pat <- paste("not yet specified", "not (yet |separately )?confirmed",
               "\\bTBD\\b", "\\bFIXME\\b", "to be decided", sep = "|")
  bad <- character(0)
  for (f in setdiff(scan_files, c("STATE.md", "verify_docs.R"))) {
    hits <- grep(pat, read_utf8(f), ignore.case = TRUE)
    if (length(hits) > 0)
      bad <- c(bad, sprintf("%s:%s", f, paste(hits, collapse = ",")))
  }
  ok("no open-question markers outside STATE.md", length(bad) == 0, bad)
}

main <- function() {
  check_citations()
  check_pass_lists()
  check_interfaces()
  check_state_cap()
  check_no_tbd()
  cat(sprintf("%d checks, %d failed\n", .checks, .fails))
  if (.fails > 0L) quit(status = 1L) else cat("DOCS CONSISTENT\n")
}

if (!interactive()) main()
