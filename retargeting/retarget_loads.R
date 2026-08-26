#!/usr/bin/env Rscript
# retarget_loads.R
#
# Mechanically retarget on-prem QVD loads in a STYLED Qlik script to their
# Qlik Cloud equivalents, driven ONLY by retargeting/qvd_field_map.csv.
# Base R only. See docs/DESIGN.md sec 6.7 for the retargeting rule this
# implements verbatim.
#
# Usage:
#   Rscript retargeting/retarget_loads.R <styled_in.qvs> <out.qvs>
#       [--map retargeting/qvd_field_map.csv]
#       [--report <report.csv>] [--fields-report <fields.csv>]
#       [--store <prefix>]   (full "lib://...:DataFiles/.../" prefix, must
#                              end in '/'; overrides .RL_STORE_PREFIX below)
#   Rscript retargeting/retarget_loads.R --selftest

# ---- locate and source shared helpers -----------------------------------

# Top-level on purpose: score_retarget.R needs this same bootstrap before it
# can source anything shared, and docs/verify_docs.R's duplicate-body check
# compares functions across files — the shared helpers live in
# retarget_shared.R instead.
.rl_argv <- commandArgs(trailingOnly = FALSE)
.rl_file <- sub("^--file=", "", .rl_argv[grepl("^--file=", .rl_argv)])
.RL_DIR  <- if (length(.rl_file) == 1 && nzchar(.rl_file))
  dirname(normalizePath(.rl_file, winslash = "/")) else getwd()
# this file lives at <root>/retargeting/retarget_loads.R
.RL_ROOT <- if (basename(.RL_DIR) == "retargeting") dirname(.RL_DIR) else getwd()
source(file.path(.RL_ROOT, "shared", "qlik_tokenizer.R"))
source(file.path(.RL_ROOT, "shared", "csv_read.R"))
source(file.path(.RL_ROOT, "retargeting", "retarget_shared.R"))

# Qlik Cloud connection ("lib://...") name is environment config (the tenant
# connection is provisioned per-environment, it is not part of the qvd field
# map data), so it lives as a constant + CLI override here, not in
# qvd_field_map.csv.
.RL_STORE_PREFIX <- "lib://Curated Data Store:DataFiles/10 Landing Area/"

# ---- small text helpers --------------------------------------------------

# Count occurrences of a fixed substring `needle`, case-insensitive, in `s`.
# GOTCHA: gregexpr() silently IGNORES ignore.case=TRUE when fixed=TRUE (it
# warns "argument 'ignore.case = TRUE' will be ignored") - so case folding
# has to happen on the strings themselves, not via the flag. Needle has no
# regex metacharacters and folding case never changes byte length, so
# positions found against the lower-cased haystack are valid against the
# original.
.rl_count_ci <- function(s, needle) {
  if (!nzchar(s)) return(0L)
  m <- gregexpr(tolower(needle), tolower(s), fixed = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1L) return(0L)
  length(m)
}

# Positions (1-based, byte offsets within s) of every occurrence of `needle`,
# case-insensitive. character(0)/integer(0) if none.
.rl_positions_ci <- function(s, needle) {
  if (!nzchar(s)) return(integer(0))
  m <- gregexpr(tolower(needle), tolower(s), fixed = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1L) return(integer(0))
  as.integer(m)
}

# ---- path classification -------------------------------------------------

# Normalize a raw (already undelimited) lib:// path body and classify it.
# Returns a list(scope, key) where scope is one of "azure","geospatial",
# "curated","other", and key is the AzureDataLake-relative map key (only
# meaningful when scope == "azure").
.rl_classify_path <- function(body) {
  norm <- gsub("\\\\", "/", body, fixed = FALSE)
  lower <- tolower(norm)
  is_azure <- grepl("^lib://appdataprod/azuredatalake/", lower) ||
              grepl("^lib://appdata/prod/azuredatalake/", lower)
  if (is_azure) {
    pos <- regexpr("azuredatalake/", norm, ignore.case = TRUE)
    key <- substr(norm, pos + attr(pos, "match.length"), nchar(norm))
    return(list(scope = "azure", key = key))
  }
  if (grepl("geospatial", lower, fixed = TRUE)) return(list(scope = "geospatial", key = NA_character_))
  if (grepl("^lib://curated data store", lower)) return(list(scope = "curated", key = NA_character_))
  list(scope = "other", key = NA_character_)
}

.rl_scope_status <- function(scope) {
  switch(scope,
    geospatial = "geospatial",
    curated    = "already-mapped",
    "out-of-scope")
}

# ---- alias multiset (SAFETY GUARD) ---------------------------------------

# Multiset of alias names (token right of AS) across every LOAD field
# segment in the whole token stream.
rl_alias_multiset <- function(tokens) {
  fls <- find_load_segments(tokens)
  out <- character(0)
  for (seg in fls$segments) {
    if (!isTRUE(seg$has_as)) next
    ai <- seg$alias_content_idx
    if (length(ai) == 0) next
    nm <- paste(undelimit(tokens$text[ai], tokens$type[ai]), collapse = "")
    out <- c(out, nm)
  }
  out
}

# Compare two alias multisets. Returns list(ok, diff) where diff is a
# character description of what differs (empty string if ok).
rl_check_alias_guard <- function(before_tokens, after_tokens) {
  b <- sort(rl_alias_multiset(before_tokens))
  a <- sort(rl_alias_multiset(after_tokens))
  if (identical(b, a)) return(list(ok = TRUE, diff = ""))
  tb <- table(b); ta <- table(a)
  all_names <- union(names(tb), names(ta))
  lines <- character(0)
  for (nm in all_names) {
    cb <- if (nm %in% names(tb)) tb[[nm]] else 0L
    ca <- if (nm %in% names(ta)) ta[[nm]] else 0L
    if (cb != ca) lines <- c(lines, sprintf("  alias %-40s before=%d after=%d", nm, cb, ca))
  }
  list(ok = FALSE, diff = paste(c(
    sprintf("ALIAS GUARD FAILED: %d before, %d after", length(b), length(a)),
    lines), collapse = "\n"))
}

# ---- LOAD statement discovery --------------------------------------------
# rt_find_from_loads() / rt_table_label() come from retarget_shared.R.

# Terminating depth-0 SEMI index for the statement whose FROM path token is
# at path_idx (scans forward tracking paren depth across the format spec
# and any WHERE/GROUP BY clause).
rl_statement_semi <- function(tokens, path_idx) {
  n <- nrow(tokens)
  depth <- 0L
  j <- path_idx + 1L
  while (j <= n) {
    ty <- tokens$type[j]
    if (ty == "LPAREN") depth <- depth + 1L
    else if (ty == "RPAREN") depth <- depth - 1L
    else if (depth == 0L && ty == "SEMI") return(j)
    j <- j + 1L
  }
  NA_integer_
}

# ---- DIRECTORY / comment occurrence scans --------------------------------

rl_find_directory_paths <- function(tokens, prevnt) {
  n <- nrow(tokens)
  out <- list()
  nxt <- next_non_trivia_idx(tokens$type)
  for (i in seq_len(n)) {
    if (tokens$type[i] == "WORD" && tolower(tokens$text[i]) == "directory") {
      pi <- nxt[i]
      if (!is.na(pi) && tokens$type[pi] %in% c("BRACKET", "DQUOTE")) {
        out[[length(out) + 1L]] <- list(directory_idx = i, path_idx = pi)
      }
    }
  }
  out
}

# GOTCHA: an UNBRACKETED "lib://..." reference (e.g. a bare
# "DIRECTORY LIB://AppDataProd/...;" with no [] around the path) is not one
# token. The tokenizer's line-comment alternative ("//[^\n]*") is tried
# before the bracket alternative and matches unconditionally wherever "//"
# occurs outside a bracket/quote - so "LIB://rest-of-line" tokenizes as
# WORD "LIB" + OTHER ":" + COMMENT "//rest-of-line" (the comment token's OWN
# text never contains "lib://", so neither the per-COMMENT-token scan nor
# the per-BRACKET/DQUOTE-token scan ever sees this occurrence; only a
# concatenated-text scan does). Detected here as its own 3-token shape so
# the report can still cover it and attribute it correctly (as
# "directory-statement" when it follows the word DIRECTORY, same as the
# bracketed case) instead of the independent lib:// count silently
# outrunning the report.
rl_find_bare_lib_spans <- function(tokens, prevnt) {
  n <- nrow(tokens)
  out <- list()
  if (n < 3) return(out)
  for (i in seq_len(n - 2)) {
    if (tokens$type[i] == "WORD" && tolower(tokens$text[i]) == "lib" &&
        tokens$type[i + 1L] == "OTHER" && tokens$text[i + 1L] == ":" &&
        tokens$type[i + 2L] == "COMMENT" && startsWith(tokens$text[i + 2L], "//")) {
      pv <- prevnt[i]
      is_directory <- !is.na(pv) && tokens$type[pv] == "WORD" &&
        tolower(tokens$text[pv]) == "directory"
      out[[length(out) + 1L]] <- list(
        lib_idx = i, colon_idx = i + 1L, comment_idx = i + 2L,
        is_directory = is_directory)
    }
  }
  out
}

# ---- $(...) variable-expansion spans --------------------------------------

# Qlik $(name) / $([name]) variable expansion is resolved at reload time, not
# a qvd field reference - tokens inside the parens must never be collected as
# read fields and must never be rewritten. Detected as: a token whose type is
# OTHER and text is "$", whose next non-trivia token is LPAREN; the span runs
# to the matching RPAREN (paren depth tracked, since the expansion body can
# itself contain parens). Returns a list of spans, one per expansion, each
# with: dollar_idx, lparen_idx, rparen_idx, span_idx (every token index in
# the span, inclusive) and content (the undelimited expansion body, e.g.
# "vDaysoverdue" for both "$(vDaysoverdue)" and "$([vDaysoverdue])").
rl_find_dollar_spans <- function(tokens) {
  n <- nrow(tokens)
  nxt <- next_non_trivia_idx(tokens$type)
  out <- list()
  for (i in seq_len(n)) {
    if (tokens$type[i] != "OTHER" || tokens$text[i] != "$") next
    pi <- nxt[i]
    if (is.na(pi) || tokens$type[pi] != "LPAREN") next
    depth <- 1L
    j <- pi + 1L
    close_idx <- NA_integer_
    while (j <= n) {
      ty <- tokens$type[j]
      if (ty == "LPAREN") depth <- depth + 1L
      else if (ty == "RPAREN") {
        depth <- depth - 1L
        if (depth == 0L) { close_idx <- j; break }
      }
      j <- j + 1L
    }
    if (is.na(close_idx)) next
    content_idx <- if (close_idx > pi + 1L) (pi + 1L):(close_idx - 1L) else integer(0)
    content <- if (length(content_idx))
      paste(undelimit(tokens$text[content_idx], tokens$type[content_idx]), collapse = "")
    else ""
    out[[length(out) + 1L]] <- list(dollar_idx = i, lparen_idx = pi, rparen_idx = close_idx,
                                     span_idx = i:close_idx, content = content)
  }
  out
}

# Distinct "depends-on-variable: ..." detail fragment for the dollar spans
# whose opening "$" falls within range_idx (a load statement's token range),
# or "" if none. Multiple variable names are comma-joined.
rl_dollar_var_detail <- function(dollar_spans, range_idx) {
  if (length(dollar_spans) == 0 || length(range_idx) == 0) return("")
  nms <- character(0)
  for (sp in dollar_spans) {
    if (sp$dollar_idx %in% range_idx) nms <- c(nms, sp$content)
  }
  nms <- unique(nms)
  if (length(nms) == 0) return("")
  paste0("depends-on-variable: ", paste(nms, collapse = ", "))
}

# Join two report `detail` fragments with "; ", omitting either side if empty.
rl_join_detail <- function(existing, extra) {
  if (!nzchar(existing)) return(extra)
  if (!nzchar(extra)) return(existing)
  paste(existing, extra, sep = "; ")
}

# ---- core retargeting -----------------------------------------------------

#' @param tokens token stream (data.frame text,type,line)
#' @param map_df qvd_field_map.csv contents (character columns)
#' @param store_prefix full "lib://...:DataFiles/.../" cloud prefix (ending
#'   in '/') used to build every rewritten FROM path; defaults to
#'   .RL_STORE_PREFIX, overridable via --store.
#' @return list(tokens = edited tokens, report = data.frame, fields = data.frame)
retarget_tokens <- function(tokens, map_df, store_prefix = .RL_STORE_PREFIX) {
  n <- nrow(tokens)
  prevnt <- prev_non_trivia_idx(tokens$type)
  dollar_spans <- rl_find_dollar_spans(tokens)
  in_dollar_span <- rep(FALSE, n)
  for (sp in dollar_spans) in_dollar_span[sp$span_idx] <- TRUE

  report_rows <- list()
  field_rows <- list()

  add_report <- function(line, kind, table_label, old_path, status, new_path,
                          n_fields_read, n_renamed, detail) {
    report_rows[[length(report_rows) + 1L]] <<- data.frame(
      line = as.integer(line), kind = kind, table_label = table_label,
      old_path = old_path, status = status, new_path = new_path,
      n_fields_read = as.integer(n_fields_read), n_renamed = as.integer(n_renamed),
      detail = detail, stringsAsFactors = FALSE)
  }
  add_field <- function(line, qvd, old_name, new_name, kind) {
    field_rows[[length(field_rows) + 1L]] <<- data.frame(
      line = as.integer(line), qvd = qvd, old_name = old_name,
      new_name = new_name, kind = kind, stringsAsFactors = FALSE)
  }

  map_lower_qvd <- tolower(map_df$onprem_qvd)

  # ---- DIRECTORY statements -------------------------------------------
  dir_entries <- rl_find_directory_paths(tokens, prevnt)
  handled_path_idx <- integer(0)
  for (d in dir_entries) {
    pi <- d$path_idx
    raw <- tokens$text[pi]
    if (.rl_count_ci(raw, "lib://") == 0) next
    handled_path_idx <- c(handled_path_idx, pi)
    add_report(tokens$line[pi], "directory", "", raw, "directory-statement", "",
               0L, 0L, "")
  }

  # ---- bare (unbracketed) lib:// spans, e.g. DIRECTORY LIB://...; --------
  bare_spans <- rl_find_bare_lib_spans(tokens, prevnt)
  for (bs in bare_spans) {
    span_text <- paste0(tokens$text[bs$lib_idx], tokens$text[bs$colon_idx], tokens$text[bs$comment_idx])
    handled_path_idx <- c(handled_path_idx, bs$lib_idx, bs$colon_idx, bs$comment_idx)
    if (bs$is_directory) {
      add_report(tokens$line[bs$lib_idx], "directory", "", trimws(span_text),
                 "directory-statement", "", 0L, 0L, "")
    } else {
      add_report(tokens$line[bs$lib_idx], "load", "", trimws(span_text),
                 "out-of-scope", "", 0L, 0L,
                 "unbracketed lib:// span, not attached to DIRECTORY or FROM")
    }
  }

  # ---- FROM-bearing LOADs -----------------------------------------------
  from_loads <- rt_find_from_loads(tokens)
  for (ld in from_loads) {
    pi <- ld$path_idx
    raw <- tokens$text[pi]
    if (.rl_count_ci(raw, "lib://") == 0) next
    handled_path_idx <- c(handled_path_idx, pi)

    label <- rt_table_label(tokens, ld$load_tok_idx, prevnt)
    body <- undelimit(raw, tokens$type[pi])
    cls <- .rl_classify_path(body)

    # whole-statement token range (LOAD ... through the terminating depth-0
    # SEMI, or through the FROM path if no SEMI is found) - used to attribute
    # any $(...) variable-expansion spans to this load's report row.
    semi_idx <- rl_statement_semi(tokens, pi)
    stmt_range <- if (!is.na(semi_idx)) ld$load_tok_idx:semi_idx else ld$load_tok_idx:pi
    var_detail <- rl_dollar_var_detail(dollar_spans, stmt_range)

    if (cls$scope != "azure") {
      add_report(tokens$line[pi], "load", label, raw, .rl_scope_status(cls$scope),
                 "", 0L, 0L, rl_join_detail("", var_detail))
      next
    }

    key <- cls$key
    rows <- map_df[map_lower_qvd == tolower(key), , drop = FALSE]

    if (nrow(rows) == 0) {
      add_report(tokens$line[pi], "load", label, raw, "not-in-map", "",
                 0L, 0L, rl_join_detail("", var_detail))
      next
    }

    src_pairs <- unique(rows[, c("source_schema", "source_object")])
    if (nrow(src_pairs) > 1) {
      detail <- paste(apply(src_pairs, 1, function(r) paste(r[1], r[2], sep = "/")),
                       collapse = "; ")
      add_report(tokens$line[pi], "load", label, raw, "multi-source", "",
                 0L, 0L, rl_join_detail(detail, var_detail))
      next
    }

    # wildcard check: a field segment whose ENTIRE content is the lone
    # token "*" (LOAD * / LOAD DISTINCT *, ...). GOTCHA: "*" is also the
    # multiplication operator (e.g. "(...) * 30"), which shows up as just
    # another token inside a longer segment - only a segment with NO other
    # content token is the wildcard syntax, so length must be checked, not
    # just token identity.
    has_wildcard <- any(vapply(ld$segments, function(seg) {
      length(seg$content_idx) == 1L && tokens$text[seg$content_idx] == "*"
    }, logical(1)))
    if (has_wildcard) {
      add_report(tokens$line[pi], "load", label, raw, "wildcard", "",
                 0L, 0L, rl_join_detail("LOAD field list contains wildcard *", var_detail))
      next
    }

    field_names_set <- unique(rows$onprem_field)

    post_from_range <- if (!is.na(semi_idx) && semi_idx > pi + 1L) (pi + 1L):(semi_idx - 1L) else integer(0)

    reads <- list()  # each: idx, name, kind
    collect <- function(idx, kind_region) {
      if (in_dollar_span[idx]) return(invisible(NULL))
      pv <- prevnt[idx]
      if (!is.na(pv) && tokens$type[pv] == "WORD" && tolower(tokens$text[pv]) == "as") return(invisible(NULL))
      ty <- tokens$type[idx]
      if (ty %in% c("BRACKET", "DQUOTE")) {
        nm <- undelimit(tokens$text[idx], ty)
        reads[[length(reads) + 1L]] <<- list(idx = idx, name = nm, kind = kind_region)
      } else if (ty == "WORD") {
        if (tokens$text[idx] %in% field_names_set) {
          reads[[length(reads) + 1L]] <<- list(idx = idx, name = tokens$text[idx], kind = "bare-word")
        }
      }
    }
    for (seg in ld$segments) for (idx in seg$content_idx) collect(idx, "segment")
    for (idx in post_from_range) collect(idx, "post-from")

    n_reads <- length(reads)

    # match each read against the map rows for this qvd
    unmatched <- character(0)
    matched <- list()
    for (rd in reads) {
      hit <- rows[rows$onprem_field == rd$name, , drop = FALSE]
      if (nrow(hit) == 0) {
        unmatched <- c(unmatched, rd$name)
      } else {
        matched[[length(matched) + 1L]] <- list(idx = rd$idx, name = rd$name,
                                                  kind = rd$kind,
                                                  verdict = hit$verdict[1],
                                                  source_column = hit$source_column[1])
      }
    }

    if (length(unmatched) > 0) {
      add_report(tokens$line[pi], "load", label, raw, "field-mismatch", "",
                 n_reads, 0L, rl_join_detail(paste(unique(unmatched), collapse = "; "), var_detail))
      next
    }

    bad_verdict <- Filter(function(m) !(m$verdict %in% c("in-cloud", "import-view")), matched)
    if (length(bad_verdict) > 0) {
      detail <- paste(unique(vapply(bad_verdict, function(m) paste0(m$name, ":", m$verdict), character(1))),
                       collapse = "; ")
      add_report(tokens$line[pi], "load", label, raw, "unusable-verdict", "",
                 n_reads, 0L, rl_join_detail(detail, var_detail))
      next
    }

    # ---- REWRITE ---------------------------------------------------------
    schema <- rows$source_schema[1]; object <- rows$source_object[1]
    new_path <- sprintf("[%s%s/%s.qvd]", store_prefix, schema, object)
    tokens$text[pi] <- new_path
    tokens$type[pi] <- "BRACKET"

    for (m in matched) {
      old_txt <- tokens$text[m$idx]
      new_txt <- paste0("[", m$source_column, "]")
      tokens$text[m$idx] <- new_txt
      tokens$type[m$idx] <- "BRACKET"
      add_field(tokens$line[m$idx], key, m$name, m$source_column, m$kind)
    }

    any_import <- any(vapply(matched, function(m) identical(m$verdict, "import-view"), logical(1)))
    status <- if (any_import) "retargeted-pending-import" else "retargeted"
    add_report(tokens$line[pi], "load", label, raw, status, new_path,
               n_reads, length(matched), rl_join_detail("", var_detail))
  }

  # ---- COMMENT occurrences (reported only) ------------------------------
  for (i in seq_len(n)) {
    if (tokens$type[i] != "COMMENT") next
    if (i %in% handled_path_idx) next
    txt <- tokens$text[i]
    positions <- .rl_positions_ci(txt, "lib://")
    if (length(positions) == 0) next
    for (p in positions) {
      before <- substr(txt, 1, p - 1)
      nl <- .rl_count_ci(before, "\n")
      # substr/count above is case-insensitive-safe for "\n" too (no case)
      occ_line <- tokens$line[i] + nl
      add_report(occ_line, "comment", "", trimws(txt), "commented", "", 0L, 0L, "")
    }
  }

  # ---- coverage / fallback for any stray lib:// bracket occurrence ------
  for (i in seq_len(n)) {
    if (!(tokens$type[i] %in% c("BRACKET", "DQUOTE"))) next
    if (.rl_count_ci(tokens$text[i], "lib://") == 0) next
    if (i %in% handled_path_idx) next
    body <- undelimit(tokens$text[i], tokens$type[i])
    cls <- .rl_classify_path(body)
    add_report(tokens$line[i], "load", "", tokens$text[i], .rl_scope_status(cls$scope),
               "", 0L, 0L, "unclassified lib:// occurrence (no FROM/DIRECTORY context)")
  }

  report <- if (length(report_rows)) do.call(rbind, report_rows) else
    data.frame(line = integer(0), kind = character(0), table_label = character(0),
               old_path = character(0), status = character(0), new_path = character(0),
               n_fields_read = integer(0), n_renamed = integer(0), detail = character(0),
               stringsAsFactors = FALSE)
  fields <- if (length(field_rows)) do.call(rbind, field_rows) else
    data.frame(line = integer(0), qvd = character(0), old_name = character(0),
               new_name = character(0), kind = character(0), stringsAsFactors = FALSE)

  report <- report[order(report$line), , drop = FALSE]
  rownames(report) <- NULL
  if (nrow(fields)) { fields <- fields[order(fields$line), , drop = FALSE]; rownames(fields) <- NULL }

  list(tokens = tokens, report = report, fields = fields)
}

# ---- DEV NOTES (developer-facing summary) ---------------------------------
# Adam's directive: ultra-concise, developer-facing signal only - nothing
# Qlik's own data load editor already surfaces at reload (missing
# fields/variables/files). Built entirely from the report data.frame that
# retarget_tokens() already produced, plus the same map_df/store_prefix the
# run used, so it never touches the on-disk report/fields CSVs.

# Recover the raw text of a bracketed/quoted report `old_path`/`new_path`
# value with its outer delimiter stripped - report rows keep only text, not
# token type, so this mirrors undelimit()'s two delimiter kinds by looking
# at the first/last character instead.
.rl_strip_raw_delim <- function(raw) {
  if (nchar(raw) < 2) return(raw)
  first <- substr(raw, 1, 1); last <- substr(raw, nchar(raw), nchar(raw))
  body <- substr(raw, 2, nchar(raw) - 1)
  if (first == '"' && last == '"') return(gsub('""', '"', body, fixed = TRUE))
  if (first == "[" && last == "]") return(body)
  raw
}

# The AzureDataLake-relative key (e.g. "SSR/RGU Staff.qvd") for a report
# row's `old_path` - every status this is called for was only reached from
# the cls$scope == "azure" branch in retarget_tokens(), so this always
# succeeds.
.rl_key_from_old_path <- function(raw) {
  .rl_classify_path(.rl_strip_raw_delim(raw))$key
}

# The generator app(s) the map records as writing `key` (source_app column).
.rl_generator_app <- function(map_df, key) {
  rows <- map_df[tolower(map_df$onprem_qvd) == tolower(key), , drop = FALSE]
  apps <- unique(rows$source_app)
  apps <- apps[!is.na(apps) & nzchar(apps)]
  paste(apps, collapse = "; ")
}

# The cloud path a wildcard LOAD's single map row would have retargeted to,
# had it not been LOAD *.
.rl_wildcard_target <- function(map_df, key, store_prefix) {
  rows <- map_df[tolower(map_df$onprem_qvd) == tolower(key), , drop = FALSE]
  sprintf("[%s%s/%s.qvd]", store_prefix, rows$source_schema[1], rows$source_object[1])
}

# report$detail for field-mismatch/unusable-verdict/wildcard rows may carry
# a trailing "; depends-on-variable: ..." fragment (rl_join_detail) that is
# reload-time noise here (Qlik already reports an unresolved variable) - cut
# it off.
.rl_strip_var_suffix <- function(detail) sub("; depends-on-variable:.*$", "", detail)

# The one-line "lead" for an untouched (not-in-map/multi-source/
# field-mismatch/wildcard/unusable-verdict) report row.
.rl_lead_for_row <- function(row, map_df, store_prefix) {
  key <- .rl_key_from_old_path(row$old_path)
  if (row$status == "not-in-map") {
    return("no lineage found")
  }
  if (row$status == "multi-source") {
    return(sprintf("generated by %s", .rl_generator_app(map_df, key)))
  }
  if (row$status == "field-mismatch") {
    return(sprintf("field(s) %s not in lineage", .rl_strip_var_suffix(row$detail)))
  }
  if (row$status == "wildcard") {
    tgt <- .rl_wildcard_target(map_df, key, store_prefix)
    return(sprintf("LOAD * not field-renamable; lineage target %s", tgt))
  }
  if (row$status == "unusable-verdict") {
    parts <- strsplit(.rl_strip_var_suffix(row$detail), "; ", fixed = TRUE)[[1]]
    formatted <- vapply(parts, function(p) {
      ci <- regexpr(":", p, fixed = TRUE)
      if (ci < 0) return(p)
      paste0(substr(p, 1, ci - 1), " (", substr(p, ci + 1, nchar(p)), ")")
    }, character(1))
    return(sprintf("field(s) %s have no cloud source column",
                    paste(formatted, collapse = "; ")))
  }
  stop("rl_lead_for_row: unexpected status: ", row$status)
}

# The generated-views evidence (fixtures/views.csv) is a dated EXTRACT, not
# live cloud state — the 05 trial proved it can lag reality (all 4 "pending"
# qvds existed and loaded). Update this date whenever the extract is
# refreshed; a live Curated Data Store DataFiles listing would supersede it.
.RL_VIEWS_EXTRACT_DATE <- "2026-08-20"

# schema.object for the target view a "retargeted-pending-import" row's
# new_path points at (parsed back out of the same sprintf shape the rewrite
# built it with).
.rl_view_from_new_path <- function(new_path, store_prefix) {
  inner <- substr(new_path, 2, nchar(new_path) - 1)
  rest <- substr(inner, nchar(store_prefix) + 1, nchar(inner))
  rest <- sub("\\.qvd$", "", rest)
  slash <- regexpr("/", rest, fixed = TRUE)
  paste0(substr(rest, 1, slash - 1), ".", substr(rest, slash + 1, nchar(rest)))
}

# Build the DEV NOTES block (character vector, one element per line).
rl_build_dev_notes <- function(report, map_df, store_prefix) {
  qvd_load_status <- c("retargeted", "retargeted-pending-import", "not-in-map",
                        "multi-source", "field-mismatch", "wildcard", "unusable-verdict")
  rewritten_status <- c("retargeted", "retargeted-pending-import")
  untouched_status <- c("not-in-map", "multi-source", "field-mismatch",
                         "wildcard", "unusable-verdict")

  scoped <- report[report$status %in% qvd_load_status, , drop = FALSE]
  n_rewritten <- sum(scoped$status %in% rewritten_status)

  untouched <- scoped[scoped$status %in% untouched_status, , drop = FALSE]
  headline <- sprintf("Retargeted %d/%d qvd loads.%s", n_rewritten, nrow(scoped),
                      if (nrow(untouched) > 0) " Not retargeted:" else "")
  lines <- c("DEV NOTES", headline)
  for (i in seq_len(nrow(untouched))) {
    row <- untouched[i, ]
    lines <- c(lines, sprintf("  line %d  %s — %s", row$line,
                               .rl_key_from_old_path(row$old_path),
                               .rl_lead_for_row(row, map_df, store_prefix)))
  }

  pending <- scoped[scoped$status == "retargeted-pending-import", , drop = FALSE]
  if (nrow(pending) > 0) {
    views <- character(0)
    for (i in seq_len(nrow(pending))) {
      v <- .rl_view_from_new_path(pending$new_path[i], store_prefix)
      if (!(v %in% views)) views <- c(views, v)
    }
    lines <- c(lines, sprintf(
      "Retargeted, but target qvd was not being generated as at %s — may not load:",
      .RL_VIEWS_EXTRACT_DATE),
      sprintf("  %s", views))
  }

  lines
}

# ---- self-test ------------------------------------------------------------

rl_selftest <- function() {
  txt <- "LOAD [Foo] AS [A]\n\t, [Bar] AS [B]\nFROM [lib://x];\n"
  toks <- tokenize_qlik(txt)
  before <- toks
  after <- toks
  # corrupt one alias: find the token whose text is "[A]" and whose prev
  # non-trivia is the word AS
  prevnt <- prev_non_trivia_idx(after$type)
  alias_idx <- which(after$type == "BRACKET" & after$text == "[A]")
  stopifnot(length(alias_idx) == 1)
  stopifnot(after$type[prevnt[alias_idx]] == "WORD" && tolower(after$text[prevnt[alias_idx]]) == "as")
  after$text[alias_idx] <- "[CORRUPTED]"

  chk <- rl_check_alias_guard(before, after)
  if (chk$ok) {
    cat("SELFTEST FAILED: guard did not detect corrupted alias\n")
    quit(status = 1)
  }
  cat(chk$diff, "\n", sep = "")
  cat("GUARD OK\n")
  quit(status = 0)
}

# ---- CLI --------------------------------------------------------------

rl_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if ("--selftest" %in% args) { rl_selftest(); return(invisible(NULL)) }

  pos <- character(0)
  map_path <- file.path(.RL_ROOT, "retargeting", "qvd_field_map.csv")
  report_path <- NA_character_
  fields_report_path <- NA_character_
  store_prefix <- .RL_STORE_PREFIX
  notes_path <- NA_character_

  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--map") { map_path <- args[i + 1L]; i <- i + 2L; next }
    if (a == "--report") { report_path <- args[i + 1L]; i <- i + 2L; next }
    if (a == "--fields-report") { fields_report_path <- args[i + 1L]; i <- i + 2L; next }
    if (a == "--store") { store_prefix <- args[i + 1L]; i <- i + 2L; next }
    if (a == "--notes") { notes_path <- args[i + 1L]; i <- i + 2L; next }
    pos <- c(pos, a)
    i <- i + 1L
  }

  if (length(pos) < 2) {
    cat("Usage: Rscript retarget_loads.R <styled_in.qvs> <out.qvs> [--map retargeting/qvd_field_map.csv] [--report <report.csv>] [--fields-report <fields.csv>] [--store <prefix>] [--notes <file>]\n")
    cat("       Rscript retarget_loads.R --selftest\n")
    quit(status = 1)
  }

  in_path <- pos[1]; out_path <- pos[2]

  if (!file.exists(in_path)) {
    cat(sprintf("Input file not found: %s\n", in_path)); quit(status = 1)
  }
  if (!file.exists(map_path)) {
    cat(sprintf("Map file not found: %s\n", map_path)); quit(status = 1)
  }

  tokens <- read_qlik_script(in_path)
  map_df <- read_csv_any(map_path)

  before_tokens <- tokens

  # independent total-occurrence count, computed on the UNMODIFIED source
  full_text <- paste(tokens$text, collapse = "")
  n_independent <- .rl_count_ci(full_text, "lib://")

  result <- retarget_tokens(tokens, map_df, store_prefix = store_prefix)

  n_covered <- nrow(result$report)
  if (n_covered != n_independent) {
    cat(sprintf("COVERAGE CHECK FAILED: independent scan found %d lib:// occurrences, report covers %d\n",
                n_independent, n_covered))
    quit(status = 1)
  }

  guard <- rl_check_alias_guard(before_tokens, result$tokens)
  if (!guard$ok) {
    cat(guard$diff, "\n", sep = "")
    cat("Guard failed - refusing to write output.\n")
    quit(status = 1)
  }

  out_lines <- detokenize(result$tokens)
  con <- file(out_path, open = "w", encoding = "UTF-8")
  writeLines(out_lines, con, useBytes = TRUE)
  close(con)

  if (!is.na(report_path)) rt_write_csv(result$report, report_path)
  if (!is.na(fields_report_path)) rt_write_csv(result$fields, fields_report_path)

  cat(sprintf("Wrote %s (%d lib:// occurrences covered, %d rewritten loads)\n",
              out_path, n_independent,
              sum(result$report$status %in% c("retargeted", "retargeted-pending-import"))))

  notes_lines <- rl_build_dev_notes(result$report, map_df, store_prefix)
  cat(paste(notes_lines, collapse = "\n"), "\n", sep = "")
  if (!is.na(notes_path)) {
    con <- file(notes_path, open = "w", encoding = "UTF-8")
    writeLines(notes_lines, con, useBytes = TRUE)
    close(con)
  }
  invisible(NULL)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  rl_main()
}
