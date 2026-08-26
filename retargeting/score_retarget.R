#!/usr/bin/env Rscript
# score_retarget.R
#
# Grades a machine-retargeted Qlik script against a hand-migrated "answer
# key" script: per FROM-bearing LOAD, did the machine pick the same cloud
# source and the same field names as the human? Differences are FINDINGS,
# never errors — this tool reports, it does not judge migration quality.
#
# Usage:
#   Rscript retargeting/score_retarget.R <ours.qvs> <theirs.qvs> <retarget_report.csv>
#       [--out <scorecard.csv>] [--fields-out <scorecard_fields.csv>]
#
# Base R only. Reads BOTH scripts with the shared tokenizer
# (shared/qlik_tokenizer.R) — never a private re-implementation.

# Top-level bootstrap (mirrors retarget_loads.R's; kept as statements, not
# functions, so docs/verify_docs.R's duplicate-body check has nothing to
# compare). Shared plumbing lives in retarget_shared.R; the SCORING logic
# below stays independent of the rewriter on purpose.
.sr_argv <- commandArgs(trailingOnly = FALSE)
.sr_file <- sub("^--file=", "", .sr_argv[grepl("^--file=", .sr_argv)])
.SR_DIR  <- if (length(.sr_file) == 1 && nzchar(.sr_file))
  dirname(normalizePath(.sr_file, winslash = "/")) else getwd()
.SR_ROOT <- if (basename(.SR_DIR) == "retargeting") dirname(.SR_DIR) else getwd()
source(file.path(.SR_ROOT, "shared", "qlik_tokenizer.R"))
source(file.path(.SR_ROOT, "retargeting", "retarget_shared.R"))

# Whole-expression text for a span of (non-trivia) content indices: the
# literal source substring from the first to the last index INCLUSIVE, with
# every internal whitespace RUN (including embedded newlines/tabs from the
# source's own line wrapping) collapsed to one space, so a CSV cell/printed
# summary never carries an embedded line break. This changes only whitespace
# — every content token, its casing and punctuation, stays exactly as
# authored — so it does not mask a real difference, only a formatting one
# that the two scripts' independent line-wrapping would otherwise inject.
sr_expr_text <- function(tokens, idx) {
  if (length(idx) == 0) return("")
  lo <- min(idx); hi <- max(idx)
  raw <- paste(tokens$text[lo:hi], collapse = "")
  trimws(gsub("[ \t\r\n]+", " ", raw))
}

# One field segment -> list(source_name, alias_name). A lone BRACKET/DQUOTE/
# WORD reference is undelimited to its plain name; anything else (an
# expression) takes the whole expression text. Un-aliased fields use the
# source name as the alias too.
sr_field_names <- function(tokens, seg) {
  src_idx <- if (isTRUE(seg$has_as)) seg$content_idx[seg$content_idx < seg$as_idx] else seg$content_idx
  src_idx <- src_idx[!(tokens$type[src_idx] %in% c("WS", "COMMENT", "VOID"))]

  src_name <- if (length(src_idx) == 1L && tokens$type[src_idx] %in% c("BRACKET", "DQUOTE", "WORD")) {
    undelimit(tokens$text[src_idx], tokens$type[src_idx])
  } else {
    sr_expr_text(tokens, src_idx)
  }

  if (isTRUE(seg$has_as)) {
    al_idx <- seg$alias_content_idx
    al_idx <- al_idx[!(tokens$type[al_idx] %in% c("WS", "COMMENT", "VOID"))]
    alias_name <- if (length(al_idx) == 1L && tokens$type[al_idx] %in% c("BRACKET", "DQUOTE", "WORD")) {
      undelimit(tokens$text[al_idx], tokens$type[al_idx])
    } else {
      sr_expr_text(tokens, al_idx)
    }
  } else {
    alias_name <- src_name
  }

  list(source_name = src_name, alias_name = alias_name)
}

# ---- path normalization ----------------------------------------------------

# Backslashes -> '/', lower-case, strip the "lib://...connection..." prefix
# through (and including) "DataFiles/" or "AzureDataLake/", then keep only
# the trailing schema/name (last two path segments) — this is what survives
# differing intermediate folder structure between the on-prem tree and the
# cloud "10 Landing Area" tree, and is what "same-source" means regardless
# of connection-name spelling.
sr_normalize_path <- function(raw_undelimited) {
  s <- gsub("\\\\", "/", raw_undelimited, fixed = FALSE)
  s <- tolower(s)
  pos_df <- regexpr("datafiles/", s, fixed = TRUE)
  pos_adl <- regexpr("azuredatalake/", s, fixed = TRUE)
  tail <- s
  if (pos_df > 0 && (pos_adl < 0 || pos_df > pos_adl)) {
    tail <- substring(s, pos_df + attr(pos_df, "match.length"))
  } else if (pos_adl > 0) {
    tail <- substring(s, pos_adl + attr(pos_adl, "match.length"))
  }
  segs <- strsplit(tail, "/", fixed = TRUE)[[1]]
  segs <- segs[nzchar(segs)]
  n <- length(segs)
  if (n >= 2L) paste(segs[(n - 1L):n], collapse = "/") else tail
}

# ---- per-script load extraction --------------------------------------------

# One row per FROM-bearing LOAD, plus a parallel list of its fields.
sr_extract_loads <- function(tokens) {
  prevnt <- prev_non_trivia_idx(tokens$type)
  from_loads <- rt_find_from_loads(tokens)
  loads <- list()
  for (ld in from_loads) {
    pi <- ld$path_idx
    raw <- tokens$text[pi]
    body <- undelimit(raw, tokens$type[pi])
    label <- rt_table_label(tokens, ld$load_tok_idx, prevnt)
    fields <- lapply(ld$segments, sr_field_names, tokens = tokens)
    loads[[length(loads) + 1L]] <- list(
      line = tokens$line[pi],
      table_label = label,
      raw_path = raw,
      path_body = body,
      path_key = sr_normalize_path(body),
      source_names = vapply(fields, function(f) f$source_name, character(1)),
      alias_names = vapply(fields, function(f) f$alias_name, character(1)))
  }
  loads
}

# ---- multiset compare -------------------------------------------------------

# Case-sensitive exact multiset compare. Returns list(n_match, only_a, only_b)
# where only_a/only_b are the leftover elements (as multisets, i.e. repeats
# kept) after removing the common intersection.
sr_multiset_compare <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  ta <- table(a); tb <- table(b)
  all_names <- union(names(ta), names(tb))
  n_match <- 0L
  only_a <- character(0); only_b <- character(0)
  for (nm in all_names) {
    ca <- if (nm %in% names(ta)) ta[[nm]] else 0L
    cb <- if (nm %in% names(tb)) tb[[nm]] else 0L
    common <- min(ca, cb)
    n_match <- n_match + common
    if (ca > common) only_a <- c(only_a, rep(nm, ca - common))
    if (cb > common) only_b <- c(only_b, rep(nm, cb - common))
  }
  list(n_match = n_match, only_a = only_a, only_b = only_b)
}

.sr_join_capped <- function(x, cap = 15L) {
  if (length(x) == 0) return("")
  shown <- utils::head(x, cap)
  s <- paste(shown, collapse = ";")
  if (length(x) > cap) s <- paste0(s, ";... (+", length(x) - cap, " more)")
  s
}

# ---- pairing ----------------------------------------------------------------

# Alias-set overlap ratio: |intersection| / min(|A|,|B|), on DISTINCT names
# (set overlap, not multiset — this is only used to pick among candidates,
# never as the scored comparison).
.sr_alias_overlap_ratio <- function(a, b) {
  a <- unique(as.character(a)); b <- unique(as.character(b))
  if (length(a) == 0 || length(b) == 0) return(0)
  length(intersect(a, b)) / min(length(a), length(b))
}

#' Pair our-loads and their-loads. Returns a data.frame(our_i, their_i) of
#' 1-based indices into `ours`/`theirs`, one row per pair. Unpaired loads on
#' either side simply do not appear.
sr_pair_loads <- function(ours, theirs) {
  n_o <- length(ours); n_t <- length(theirs)
  our_path <- vapply(ours, function(l) l$path_key, character(1))
  their_path <- vapply(theirs, function(l) l$path_key, character(1))

  paired_o <- rep(FALSE, n_o); paired_t <- rep(FALSE, n_t)
  pairs <- list()

  # ---- phase 1: pair by matching normalized path tail ----------------------
  keys <- intersect(unique(our_path), unique(their_path))
  for (k in keys) {
    oi <- which(our_path == k); ti <- which(their_path == k)
    if (length(oi) == 1L && length(ti) == 1L) {
      pairs[[length(pairs) + 1L]] <- c(oi, ti)
      paired_o[oi] <- TRUE; paired_t[ti] <- TRUE
      next
    }
    # multiple candidates share this path tail on at least one side:
    # disambiguate by largest alias-set overlap, greedy, highest first.
    cand <- expand.grid(o = oi, t = ti)
    cand$overlap <- mapply(function(o, t) .sr_alias_overlap_ratio(ours[[o]]$alias_names, theirs[[t]]$alias_names),
                            cand$o, cand$t)
    cand <- cand[order(-cand$overlap), , drop = FALSE]
    used_o <- character(0); used_t <- character(0)
    for (r in seq_len(nrow(cand))) {
      o <- cand$o[r]; t <- cand$t[r]
      if (o %in% used_o || t %in% used_t) next
      pairs[[length(pairs) + 1L]] <- c(o, t)
      paired_o[o] <- TRUE; paired_t[t] <- TRUE
      used_o <- c(used_o, o); used_t <- c(used_t, t)
    }
  }

  # ---- phase 2: remaining OUR loads, pair by alias-set overlap alone -------
  rem_o <- which(!paired_o); rem_t <- which(!paired_t)
  if (length(rem_o) && length(rem_t)) {
    cand <- expand.grid(o = rem_o, t = rem_t)
    cand$overlap <- mapply(function(o, t) .sr_alias_overlap_ratio(ours[[o]]$alias_names, theirs[[t]]$alias_names),
                            cand$o, cand$t)
    cand <- cand[cand$overlap >= 0.5, , drop = FALSE]
    cand <- cand[order(-cand$overlap), , drop = FALSE]
    used_o <- integer(0); used_t <- integer(0)
    for (r in seq_len(nrow(cand))) {
      o <- cand$o[r]; t <- cand$t[r]
      if (o %in% used_o || t %in% used_t) next
      pairs[[length(pairs) + 1L]] <- c(o, t)
      paired_o[o] <- TRUE; paired_t[t] <- TRUE
      used_o <- c(used_o, o); used_t <- c(used_t, t)
    }
  }

  if (length(pairs) == 0) return(data.frame(our_i = integer(0), their_i = integer(0)))
  do.call(rbind, lapply(pairs, function(p) data.frame(our_i = p[1], their_i = p[2])))
}

# ---- CLI --------------------------------------------------------------------

sr_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  getopt <- function(flag, default) {
    i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[i + 1L]
  }
  pos <- character(0)
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (a %in% c("--out", "--fields-out")) { i <- i + 2L; next }
    pos <- c(pos, a); i <- i + 1L
  }
  if (length(pos) < 3) {
    cat("Usage: Rscript retargeting/score_retarget.R <ours.qvs> <theirs.qvs> <retarget_report.csv> [--out <scorecard.csv>] [--fields-out <scorecard_fields.csv>]\n")
    quit(status = 1)
  }
  ours_path <- pos[1]; theirs_path <- pos[2]; report_path <- pos[3]
  out_path <- getopt("--out", file.path(dirname(ours_path), "scorecard.csv"))
  fields_out_path <- getopt("--fields-out", file.path(dirname(ours_path), "scorecard_fields.csv"))

  for (p in c(ours_path, theirs_path, report_path)) {
    if (!file.exists(p)) { cat(sprintf("File not found: %s\n", p)); quit(status = 1) }
  }

  our_tokens <- read_qlik_script(ours_path)
  their_tokens <- read_qlik_script(theirs_path)
  report <- utils::read.csv(report_path, stringsAsFactors = FALSE, colClasses = "character")
  report$line <- as.integer(report$line)
  report_load <- report[report$kind == "load", , drop = FALSE]

  ours <- sr_extract_loads(our_tokens)
  theirs <- sr_extract_loads(their_tokens)

  pairs <- sr_pair_loads(ours, theirs)
  our_partner <- setNames(rep(NA_integer_, length(ours)), NULL)
  their_partner <- setNames(rep(NA_integer_, length(theirs)), NULL)
  if (nrow(pairs)) {
    our_partner[pairs$our_i] <- pairs$their_i
    their_partner[pairs$their_i] <- pairs$our_i
  }

  status_by_line <- setNames(report_load$status, report_load$line)

  rows <- list()
  for (oi in seq_along(ours)) {
    ol <- ours[[oi]]
    ti <- our_partner[oi]
    has_partner <- !is.na(ti)
    tl <- if (has_partner) theirs[[ti]] else NULL

    our_status <- unname(status_by_line[as.character(ol$line)])
    if (is.null(our_status) || length(our_status) == 0) our_status <- NA_character_

    if (has_partner) {
      path_verdict <- if (identical(ol$path_key, tl$path_key)) "same-source" else "different-source"
      src_cmp <- sr_multiset_compare(ol$source_names, tl$source_names)
      al_cmp <- sr_multiset_compare(ol$alias_names, tl$alias_names)
      rows[[length(rows) + 1L]] <- data.frame(
        our_line = ol$line, table_label = ol$table_label, our_status = our_status,
        our_path = ol$raw_path, their_line = tl$line, their_path = tl$raw_path,
        path_verdict = path_verdict,
        n_our_fields = length(ol$source_names), n_their_fields = length(tl$source_names),
        n_source_name_matches = src_cmp$n_match, n_alias_matches = al_cmp$n_match,
        source_names_only_ours = .sr_join_capped(src_cmp$only_a),
        source_names_only_theirs = .sr_join_capped(src_cmp$only_b),
        alias_only_ours = .sr_join_capped(al_cmp$only_a),
        alias_only_theirs = .sr_join_capped(al_cmp$only_b),
        stringsAsFactors = FALSE)
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        our_line = ol$line, table_label = ol$table_label, our_status = our_status,
        our_path = ol$raw_path, their_line = NA_integer_, their_path = NA_character_,
        path_verdict = "no-counterpart",
        n_our_fields = length(ol$source_names), n_their_fields = 0L,
        n_source_name_matches = 0L, n_alias_matches = 0L,
        source_names_only_ours = .sr_join_capped(ol$source_names),
        source_names_only_theirs = "",
        alias_only_ours = .sr_join_capped(ol$alias_names),
        alias_only_theirs = "",
        stringsAsFactors = FALSE)
    }
  }

  for (ti in seq_along(theirs)) {
    if (!is.na(their_partner[ti])) next
    tl <- theirs[[ti]]
    note <- if (nzchar(tl$table_label)) sprintf("THEIR-ONLY (%s)", tl$table_label) else "THEIR-ONLY"
    rows[[length(rows) + 1L]] <- data.frame(
      our_line = NA_integer_, table_label = note, our_status = NA_character_,
      our_path = NA_character_, their_line = tl$line, their_path = tl$raw_path,
      path_verdict = "no-counterpart",
      n_our_fields = 0L, n_their_fields = length(tl$source_names),
      n_source_name_matches = 0L, n_alias_matches = 0L,
      source_names_only_ours = "", source_names_only_theirs = .sr_join_capped(tl$source_names),
      alias_only_ours = "", alias_only_theirs = .sr_join_capped(tl$alias_names),
      stringsAsFactors = FALSE)
  }

  scorecard <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (nrow(scorecard)) {
    ord <- order(is.na(scorecard$our_line), scorecard$our_line, scorecard$their_line)
    scorecard <- scorecard[ord, , drop = FALSE]
    rownames(scorecard) <- NULL
  }

  # ---- scorecard_fields.csv: per source-name difference within PAIRED loads --
  frows <- list()
  if (nrow(pairs)) {
    for (r in seq_len(nrow(pairs))) {
      oi <- pairs$our_i[r]; ti <- pairs$their_i[r]
      ol <- ours[[oi]]; tl <- theirs[[ti]]
      cmp <- sr_multiset_compare(ol$source_names, tl$source_names)
      for (nm in cmp$only_a) frows[[length(frows) + 1L]] <- data.frame(
        our_line = ol$line, their_line = tl$line, name = nm, side = "ours-only", stringsAsFactors = FALSE)
      for (nm in cmp$only_b) frows[[length(frows) + 1L]] <- data.frame(
        our_line = ol$line, their_line = tl$line, name = nm, side = "theirs-only", stringsAsFactors = FALSE)
    }
  }
  scorecard_fields <- if (length(frows)) do.call(rbind, frows) else
    data.frame(our_line = integer(0), their_line = integer(0), name = character(0), side = character(0),
               stringsAsFactors = FALSE)
  if (nrow(scorecard_fields)) {
    scorecard_fields <- scorecard_fields[order(scorecard_fields$our_line, scorecard_fields$their_line), , drop = FALSE]
    rownames(scorecard_fields) <- NULL
  }

  rt_write_csv(scorecard, out_path)
  rt_write_csv(scorecard_fields, fields_out_path)

  # ---- summary ---------------------------------------------------------------
  n_our <- length(ours); n_their <- length(theirs); n_paired <- nrow(pairs)
  cat("==== SCORE RETARGET SUMMARY ====\n")
  cat(sprintf("our FROM-loads:   %d\n", n_our))
  cat(sprintf("their FROM-loads: %d\n", n_their))
  cat(sprintf("paired:           %d\n", n_paired))
  cat("\nper our_status (paired same-source count):\n")
  our_rows_only <- scorecard[!is.na(scorecard$our_line), , drop = FALSE]
  statuses <- sort(unique(ifelse(is.na(our_rows_only$our_status), "NA", our_rows_only$our_status)))
  for (st in statuses) {
    sub <- our_rows_only[ifelse(is.na(our_rows_only$our_status), "NA", our_rows_only$our_status) == st, , drop = FALSE]
    n_tot <- nrow(sub)
    n_paired_st <- sum(!is.na(sub$their_line))
    n_same_src <- sum(sub$path_verdict == "same-source")
    cat(sprintf("  %-28s total=%-3d paired=%-3d same-source=%-3d\n", st, n_tot, n_paired_st, n_same_src))
  }

  rewritten <- our_rows_only[our_rows_only$our_status %in% c("retargeted", "retargeted-pending-import") &
                               !is.na(our_rows_only$our_status), , drop = FALSE]
  paired_rewritten <- rewritten[!is.na(rewritten$their_line), , drop = FALSE]
  tot_our_fields <- sum(paired_rewritten$n_our_fields)
  tot_matches <- sum(paired_rewritten$n_source_name_matches)
  pct <- if (tot_our_fields > 0) 100 * tot_matches / tot_our_fields else NA
  cat(sprintf("\nsource-name agreement across paired REWRITTEN loads: %d/%d fields = %s%%\n",
              tot_matches, tot_our_fields, if (is.na(pct)) "NA" else sprintf("%.1f", pct)))

  cat("\n==== REFUSED LOADS -- what the human did ====\n")
  refused_statuses <- c("not-in-map", "multi-source", "field-mismatch", "wildcard",
                         "unusable-verdict", "directory-statement", "commented")
  refused <- our_rows_only[our_rows_only$our_status %in% refused_statuses & !is.na(our_rows_only$our_status), , drop = FALSE]
  if (nrow(refused) == 0) {
    cat("(none)\n")
  } else {
    for (r in seq_len(nrow(refused))) {
      rr <- refused[r, ]
      counterpart <- if (is.na(rr$their_path)) "none" else rr$their_path
      cat(sprintf("  line %-5d [%-20s] status=%-20s our_path=%s\n    -> human counterpart: %s\n",
                  rr$our_line, ifelse(nzchar(rr$table_label), rr$table_label, "(unlabelled)"),
                  rr$our_status, rr$our_path, counterpart))
    }
  }

  cat(sprintf("\nWrote %s and %s\n", out_path, fields_out_path))
  invisible(NULL)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  sr_main()
}
