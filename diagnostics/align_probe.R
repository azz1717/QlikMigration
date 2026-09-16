# align_probe.R - why is the AS column ragged? Answers it in one screen.
#
#     Rscript diagnostics/align_probe.R <a styled .qvs> [first-line]
#
# Reads a styled script and, for the first LOAD field block it finds (or the
# one containing `first-line`), prints per line: the leading whitespace, the
# padding whitespace, and the column the `AS` lands on under TAB-STOP
# semantics at width 4 (advance to the next multiple of 4) and under
# FIXED semantics (every tab is exactly 4 columns).
#
# Read it like this:
#   - STOP column all the same, FIXED column ragged  -> the file is correct
#     and the editor is advancing tabs by a fixed 4, not to a tab stop.
#   - STOP column ragged -> the file itself is wrong; the numbers say how.
#   - LEAD not all identical -> the indentation differs line to line, which
#     no amount of padding can rescue.
#
# Output is <=72 columns and ~20 lines on purpose: DESIGN 8.5, the return
# channel from the VM is a photograph of the screen. Vanilla base R.

W <- 4L

.ap_col <- function(s, upto, fixed) {
	col <- 0L
	if (upto <= 1L) return(col)
	for (ch in strsplit(substr(s, 1L, upto - 1L), "")[[1]])
		col <- if (ch != "\t") col + 1L
		       else if (fixed) col + W else (col %/% W + 1L) * W
	col
}

.ap_ws <- function(s) {
	m <- regmatches(s, regexpr("^[ \t]*", s))
	paste0(sum(strsplit(m, "")[[1]] == "\t"), "t",
	       sum(strsplit(m, "")[[1]] == " "), "s")
}

main <- function(args) {
	if (!length(args)) {
		cat("usage: Rscript diagnostics/align_probe.R <styled.qvs> [line]\n")
		return(1L)
	}
	f <- args[1]
	if (!file.exists(f)) { cat("No such file: ", f, "\n", sep = ""); return(1L) }
	x <- readLines(f, warn = FALSE, encoding = "UTF-8")
	x <- sub("\r$", "", x)

	at <- regexpr("[ \t]+ ?AS ", x, ignore.case = TRUE)
	len <- attr(at, "match.length")
	ok <- which(at > 0)
	if (!length(ok)) { cat("No ' AS ' field lines in that file.\n"); return(1L) }

	start <- if (length(args) > 1L) suppressWarnings(as.integer(args[2])) else NA
	if (!is.na(start)) ok <- ok[ok >= start]
	if (!length(ok)) { cat("No ' AS ' lines at or after that line.\n"); return(1L) }

	# ONE contiguous block: stop at the first gap. Two LOAD blocks legitimately
	# align to different columns, so running them together would report a
	# raggedness that is not there - which this probe did until 2026-09-16.
	brk <- which(diff(ok) != 1L)
	run <- if (!length(brk)) ok else ok[seq_len(brk[1])]
	run <- run[seq_len(min(length(run), 16L))]

	cat("file: ", basename(f), "   lines ", run[1], "-",
	    run[length(run)], "\n", sep = "")
	cat("LEAD = leading tabs/spaces, PAD = padding before AS\n")
	cat(sprintf("%6s %7s %7s %5s %5s\n", "line", "LEAD", "PAD", "STOP", "FIXED"))
	cat(strrep("-", 40), "\n", sep = "")

	stops <- integer(0); fixes <- integer(0)
	for (i in run) {
		a <- regexpr("AS ", substr(x[i], at[i], at[i] + len[i]), fixed = TRUE)
		as_at <- at[i] + a - 1L
		s <- .ap_col(x[i], as_at, FALSE)
		fx <- .ap_col(x[i], as_at, TRUE)
		pad <- substr(x[i], at[i], as_at - 1L)
		cat(sprintf("%6d %7s %7s %5d %5d\n", i, .ap_ws(x[i]),
		            paste0(sum(strsplit(pad, "")[[1]] == "\t"), "t",
		                   sum(strsplit(pad, "")[[1]] == " "), "s"), s, fx))
		stops <- c(stops, s); fixes <- c(fixes, fx)
	}
	cat(strrep("-", 40), "\n", sep = "")
	cat("STOP  distinct columns: ", length(unique(stops)),
	    "   FIXED: ", length(unique(fixes)), "\n", sep = "")
	cat("tab width in this build: ", W, "\n", sep = "")
	0L
}

if (sys.nframe() == 0L) {
	q(status = main(commandArgs(trailingOnly = TRUE)), save = "no")
}
