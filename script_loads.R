# script_loads.R — what every LOAD in the script produces.
#
# Phase 2, step 3 (DESIGN §6.5). NOT part of the styling pipeline; nothing in
# run_pipeline.R sources this. Base R only.
#
# The app side (app_usage.R) says which names are referenced. This side says
# which names exist and which table holds them, so step 4 can cross-reference
# the two. Nothing here deletes anything.
#
# Expects STYLED input (Adam 2026-08-18). DESIGN §5: phase 1 runs first
# precisely so a field is spelled exactly one way, `[Field] AS [Alias]`,
# instead of four. Raw input still parses — an un-aliased field falls back to
# its own content text — but every fallback is warned about, because there the
# produced name was inferred rather than read.
#
# A LOAD is NOT a table. Three things break that assumption and all three are
# modelled here (DESIGN §6.5):
#   * a PRECEDING chain — stacked LOADs, one table, named by the TOP one;
#   * a JOIN/CONCATENATE load, which feeds a table someone else created;
#   * a MAPPING load, which never enters the data model at all.

source("qlik_tokenizer.R")
source("qlik_reserved_words.R")

# Words that may sit between a table label and its LOAD. Not a correctness
# guard — the head scan takes whatever it finds — but an unrecognised word
# there is worth a warning rather than a silent pass.
.SL_PREFIXES <- c("CONCATENATE", "NOCONCATENATE", "JOIN", "KEEP", "LEFT",
                  "RIGHT", "INNER", "OUTER", "MAPPING", "BUFFER", "ADD",
                  "REPLACE", "ONLY", "FIRST", "SEMANTIC", "HIERARCHY",
                  "CROSSTABLE", "GENERIC", "UNLESS", "WHEN", "INTERVALMATCH")

# One name out of a token RANGE. `undelimit()` (qlik_tokenizer.R) is
# elementwise; the join is this file's own naming rule, not part of it — an
# un-aliased expression field spans several tokens and its inferred name is
# all of them run together. The two callers that pass a range use this; the
# four that pass a single token call `undelimit()` directly.
.sl_name <- function(text, type) paste(undelimit(text, type), collapse = "")

# First non-trivia token at or after `from`, unbounded.
#
# GOTCHA: this used to be a fixed 40-token window and that was wrong. Styling
# inserts whitespace tokens freely, so the same statement that showed `SELECT`
# 3 tokens later in the raw script showed it past 40 in the styled one — and
# `LOAD;SELECT` was misread as a sourceless load on the input this tool is
# actually meant to consume. Never bound a lookahead in TOKEN counts across
# a pipeline that changes token counts.
.sl_next_solid <- function(tokens, from) {
  i <- from
  n <- nrow(tokens)
  while (i <= n) {
    if (!(tokens$type[i] %in% c("WS", "COMMENT", "VOID"))) return(i)
    i <- i + 1L
  }
  NA_integer_
}

.sl_solid <- function(type, lo, hi) {
  if (hi < lo) return(integer(0))
  i <- lo:hi
  i[!(type[i] %in% c("WS", "COMMENT", "VOID"))]
}

# The statement head: the label and any prefix, scanned backwards from LOAD.
#
# Stopping at the previous `;` would be simpler and is wrong: control flow does
# not end in a semicolon, so `END IF` / `NEXT chunkText` sitting above a LOAD
# get swept into the head. That is not cosmetic — it displaces the label from
# the head's first position and the load silently loses its table name
# (app-unbuilt, 6 places). So accept only what may legally appear in a head and
# stop at the first thing that may not.
.sl_head <- function(tokens, load_idx) {
  out <- integer(0)
  i   <- load_idx - 1L
  while (i >= 1L) {
    ty <- tokens$type[i]
    if (ty %in% c("WS", "COMMENT", "VOID")) { i <- i - 1L; next }
    if (ty == "OTHER" && tokens$text[i] == ":") {
      # The label is whatever sits immediately left of the colon, and nothing
      # may precede it — a head starts at its label.
      j <- i - 1L
      while (j >= 1L && tokens$type[j] %in% c("WS", "COMMENT", "VOID")) j <- j - 1L
      if (j >= 1L && tokens$type[j] %in% c("WORD", "BRACKET", "DQUOTE"))
        out <- c(j, i, out)
      break
    }
    if (ty == "RPAREN") {
      # A prefix argument list takes anything, commas included:
      # `CrossTable(Attribute, Data, 3) LOAD ...`. Stopping at the comma left
      # the head as a lone `)`, which cost both the prefix and the LABEL — the
      # load then got auto-named after its source and the real table went
      # missing (app-unbuilt: CrossTab, PMCrossTab). Take the group whole.
      depth <- 0L
      while (i >= 1L) {
        if (tokens$type[i] == "RPAREN") depth <- depth + 1L
        else if (tokens$type[i] == "LPAREN") {
          depth <- depth - 1L
          if (depth == 0L) break
        }
        out <- c(i, out); i <- i - 1L
      }
      if (i < 1L) break
      out <- c(i, out); i <- i - 1L
      next
    }
    ok <- ty %in% c("NUMBER", "BRACKET", "DQUOTE") ||
          (ty == "WORD" && toupper(tokens$text[i]) %in% .SL_PREFIXES)
    if (!ok) break
    out <- c(i, out)
    i <- i - 1L
  }
  out
}

# A label is the head's FIRST token followed by `:` — the tokenizer types a
# colon as OTHER, there being no colon operator in Qlik.
.sl_label <- function(tokens, head) {
  if (length(head) >= 2L && tokens$type[head[2L]] == "OTHER" &&
      tokens$text[head[2L]] == ":")
    return(list(table = undelimit(tokens$text[head[1L]], tokens$type[head[1L]]),
                prefix_idx = head[-(1:2)]))
  list(table = NA_character_, prefix_idx = head)
}

# First `;` at paren depth 0 at or after `from`. Depth matters: a subquery or a
# function argument carries its own punctuation.
.sl_stmt_end <- function(tokens, from) {
  depth <- 0L
  for (i in from:nrow(tokens)) {
    ty <- tokens$type[i]
    if (ty == "LPAREN") depth <- depth + 1L
    else if (ty == "RPAREN") depth <- depth - 1L
    else if (ty == "SEMI" && depth <= 0L) return(i)
  }
  nrow(tokens)
}

# What feeds the load, read from the tail of the statement. `none` is not a
# failure to parse — it is the signature of a preceding load, which draws from
# the statement below it rather than from any source of its own.
.sl_source <- function(tokens, lo, hi) {
  idx <- .sl_solid(tokens$type, lo, hi)
  if (length(idx) == 0L) return(list(kind = "none", src = NA_character_))
  up <- toupper(tokens$text[idx])

  after <- function(k) {
    p <- idx[which(up == k)[1L] + 1L]
    if (is.na(p) || p > hi) NA_character_
    else undelimit(tokens$text[p], tokens$type[p])
  }

  if ("RESIDENT"     %in% up) return(list(kind = "resident",     src = after("RESIDENT")))
  if ("INLINE"       %in% up) return(list(kind = "inline",       src = NA_character_))
  if ("AUTOGENERATE" %in% up) return(list(kind = "autogenerate", src = NA_character_))
  if ("FROM"         %in% up) return(list(kind = "from",         src = after("FROM")))
  list(kind = "none", src = NA_character_)
}

# Qualifiers that may sit between LOAD and its field list. `DISTINCT` is in
# QLIK_KEYWORDS already; this list exists only for the un-aliased-name strip
# below, where taking the WHOLE keyword vocabulary out of an expression would
# mangle it. Narrow on purpose.
.SL_QUALIFIERS <- c("DISTINCT")

# Is this field just `*`? A qualifier is not part of the field list, so it is
# dropped before the test — `LOAD DISTINCT *` is a wildcard.
#
# GOTCHA: QLIK_KEYWORDS is stored LOWERCASE. Every pass in this repo compares
# `tolower(text) %in% QLIK_KEYWORDS`; comparing `toupper()` against it matches
# nothing at all, silently, and the filter becomes a no-op.
.sl_is_wildcard <- function(tokens, lo, hi) {
  idx <- .sl_solid(tokens$type, lo, hi)
  idx <- idx[!(tokens$type[idx] == "WORD" &
               tolower(tokens$text[idx]) %in% QLIK_KEYWORDS)]
  length(idx) == 1L && tokens$type[idx] == "OPERATOR" && tokens$text[idx] == "*"
}

# An INLINE block is ONE token: the tokenizer's bracket pattern is `\[[^\]]*\]`,
# so the whole block including its newlines arrives as a single BRACKET. Its
# first non-blank line is the field list, every later one a data row.
#
# The row count is not incidental. Adam 2026-08-18: inline loads are legitimate
# for mapping and crosstabs but NOT for importing raw data, and app-unbuilt has
# inline blocks that are a pasted CSV. Size is what tells those apart, so the
# count is carried through to the report (§7.1).
.sl_inline <- function(tokens, lo, hi) {
  idx <- .sl_solid(tokens$type, lo, hi)
  if (length(idx) == 0L) return(NULL)
  up <- toupper(tokens$text[idx])
  p  <- which(up == "INLINE")[1L]
  if (is.na(p) || p >= length(idx)) return(NULL)
  b <- idx[p + 1L]
  if (tokens$type[b] != "BRACKET") return(NULL)

  # `INLINE [...] (delimiter is '|')` — comma unless told otherwise.
  delim <- ","
  d <- which(up == "DELIMITER")
  if (length(d)) {
    q <- idx[d[1L] + 2L]
    if (!is.na(q) && q <= hi && tokens$type[q] == "SQUOTE")
      delim <- undelimit(tokens$text[q], "SQUOTE")
  }

  body  <- undelimit(tokens$text[b], "BRACKET")
  lines <- strsplit(body, "\r?\n")[[1L]]
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0L) return(NULL)

  hdr <- trimws(strsplit(lines[1L], delim, fixed = TRUE)[[1L]])
  hdr <- sub('^"(.*)"$', "\\1", sub("^'(.*)'$", "\\1", hdr))
  # Ten rows, never the block. Adam 2026-08-18: a developer judges an inline
  # load's appropriateness from its header and a glimpse of the data —
  # embedding 1113 rows in a document meant to be skimmed is counterproductive.
  list(fields = hdr[nzchar(hdr)], n_rows = length(lines) - 1L,
       sample = head(trimws(lines[-1L]), 10L))
}

# `///$tab Name` markers divide a Qlik script into tabs. They are how a
# developer actually navigates it, so every load carries the tab it lives on —
# a line number alone does not help someone opening the script in Qlik.
.sl_tabs <- function(tokens) {
  cm <- which(tokens$type == "COMMENT" & grepl("^///\\$tab", tokens$text))
  if (length(cm) == 0L)
    return(data.frame(line = integer(0), tab = character(0),
                      stringsAsFactors = FALSE))
  data.frame(line = tokens$line[cm],
             tab  = trimws(sub("^///\\$tab", "", tokens$text[cm])),
             stringsAsFactors = FALSE)
}

.sl_tab_at <- function(tabs, line) {
  if (nrow(tabs) == 0L) return(NA_character_)
  i <- which(tabs$line <= line)
  if (!length(i)) NA_character_ else tabs$tab[max(i)]
}

# Qlik names an unlabelled table after its source: the qvd's base name, or the
# resident table it copies. Only reached for a load that is nobody's chain
# member and carries no join prefix.
.sl_autoname <- function(kind, src) {
  if (is.na(src)) return(NA_character_)
  if (kind == "resident") return(src)
  if (kind == "from") return(sub("\\.[Qq][Vv][Dd]$", "", basename(src)))
  NA_character_
}

#' Every LOAD in a token stream, and the fields it produces.
#'
#' @param tokens as `read_qlik_script()`.
#' @return list(loads, fields, warnings).
#'   loads:  data.frame(load_id, table, producer_kind, prefix, source_kind,
#'                      source, line_start, line_end, chain_of, n_declared,
#'                      complete_fields)
#'   fields: data.frame(load_id, table, field, source_field, line, aliased, via)
#'
#' `load_id` is positional, 1..n in script order — the join key between the two
#' frames, stable only within one parse of one file. `chain_of` names the
#' load_id whose table a preceding-chain member contributes to; it equals
#' `load_id` for a load that stands on its own.
#'
#' `producer_kind`:
#'   table      — creates a table of its own
#'   preceding  — a stacked LOAD; part of the chain named by `chain_of`
#'   joins-into — JOIN/CONCATENATE; adds to a table someone else created
#'   mapping    — a MAPPING load; consumed by ApplyMap, never in the data model
#'
#' `complete_fields` is FALSE where a `*` could not be resolved to a named
#' list. Such a table can only be judged whole, never field by field.
script_loads <- function(tokens) {
  seg  <- find_load_segments(tokens)
  warn <- seg$warnings
  s    <- seg$segments

  if (length(s) == 0L)
    return(list(loads = .sl_empty_loads(), fields = .sl_empty_fields(),
                inlines = .sl_empty_inlines(), warnings = warn))

  load_tok <- vapply(s, function(x) x$load_tok_idx, integer(1))
  order_lt <- unique(load_tok)
  n        <- length(order_lt)
  tabs     <- .sl_tabs(tokens)

  rec <- vector("list", n)
  for (i in seq_len(n)) {
    lt   <- order_lt[i]
    mine <- which(load_tok == lt)
    head <- .sl_head(tokens, lt)
    lab  <- .sl_label(tokens, head)

    pidx <- lab$prefix_idx
    ptxt <- toupper(tokens$text[pidx])
    # A prefix's ARGUMENTS are arbitrary names — `CrossTable(Category, Result,
    # 3)` — so only words outside the parentheses are checked against the
    # known-prefix list.
    depth <- cumsum(tokens$type[pidx] == "LPAREN") -
             cumsum(tokens$type[pidx] == "RPAREN")
    top  <- tokens$type[pidx] == "WORD" & depth == 0L
    odd  <- setdiff(ptxt[top], .SL_PREFIXES)
    if (length(odd))
      warn <- c(warn, sprintf("line %d: unrecognised word before LOAD: %s",
                              tokens$line[lt], paste(odd, collapse = " ")))

    list_end <- max(vapply(s[mine], function(x) x$end, integer(1)))
    stmt_end <- .sl_stmt_end(tokens, list_end)

    srcinfo <- .sl_source(tokens, list_end, stmt_end)
    # `LOAD ...; SELECT ...;` — the ODBC form, where the statement BELOW feeds
    # the load. All 24 sourceless loads in app-unbuilt are this, not preceding
    # loads, so the two must be told apart before contiguity is consulted.
    if (srcinfo$kind == "none") {
      nx <- .sl_next_solid(tokens, stmt_end + 1L)
      if (!is.na(nx) && toupper(tokens$text[nx]) %in% c("SELECT", "SQL"))
        srcinfo <- list(kind = "select", src = NA_character_)
    }

    inl <- if (srcinfo$kind == "inline") .sl_inline(tokens, list_end, stmt_end) else NULL

    rec[[i]] <- c(list(load_tok = lt, seg = mine, label = lab$table,
                       inline_fields = if (is.null(inl)) character(0) else inl$fields,
                       inline_rows = if (is.null(inl)) NA_integer_ else inl$n_rows,
                       inline_sample = if (is.null(inl)) character(0) else inl$sample,
                       prefix = if (length(ptxt)) paste(ptxt, collapse = " ") else NA_character_,
                       prefix_words = ptxt[tokens$type[pidx] == "WORD"],
                       head_start = if (length(head)) head[1L] else lt,
                       stmt_end = stmt_end,
                       line_start = tokens$line[if (length(head)) head[1L] else lt],
                       line_end = tokens$line[stmt_end]),
                  srcinfo)
  }

  # --- chains -------------------------------------------------------------
  # A preceding load has no source of its own and the next statement starts
  # immediately after its `;`, nothing but trivia between. Contiguity is what
  # distinguishes a preceding load from a load that merely failed to parse.
  feeds_into <- rep(NA_integer_, n)
  for (i in seq_len(n - 1L)) {
    if (rec[[i]]$kind != "none") next
    gap <- .sl_solid(tokens$type, rec[[i]]$stmt_end + 1L,
                     rec[[i + 1L]]$head_start - 1L)
    if (length(gap) == 0L) feeds_into[i] <- i + 1L
    else warn <- c(warn, sprintf("line %d: LOAD with no source and no statement below it",
                                 rec[[i]]$line_start))
  }

  # chain_of: walk UP from each member to the topmost load of its chain — the
  # one that carries the label and exposes the table.
  chain_of <- seq_len(n)
  for (i in seq_len(n)) {
    j <- i
    while (j > 1L && !is.na(feeds_into[j - 1L]) && feeds_into[j - 1L] == j) j <- j - 1L
    chain_of[i] <- j
  }

  # --- producer kind and table name ---------------------------------------
  is_join <- vapply(rec, function(r) any(c("JOIN", "CONCATENATE") %in% r$prefix_words), logical(1))
  is_map  <- vapply(rec, function(r) "MAPPING" %in% r$prefix_words, logical(1))

  # A wildcard PATH is not one table. Adam 2026-08-18: Qlik loads every qvd
  # matching the pattern and names each resulting table after its own qvd file,
  # taking all of that qvd's fields. So the statement expands to N tables whose
  # names and fields are both outside the script — resolvable only against a
  # qvd inventory, which this repo does not have (see STATE.md).
  is_multi <- vapply(rec, function(r) r$kind == "from" && !is.na(r$src) &&
                       grepl("*", basename(r$src), fixed = TRUE), logical(1))

  kind  <- rep("table", n)
  kind[chain_of != seq_len(n)] <- "preceding"
  kind[is_join]  <- "joins-into"
  kind[is_map]   <- "mapping"
  kind[is_multi] <- "multi-table"

  tbl  <- rep(NA_character_, n)
  last <- NA_character_
  for (i in seq_len(n)) {
    top <- chain_of[i]
    if (kind[i] == "joins-into") {
      # `JOIN (Target) LOAD` names its target; a bare `JOIN LOAD` attaches to
      # the most recently created table.
      inparen <- rec[[i]]$prefix_words
      tgt <- if (grepl("(", rec[[i]]$prefix %||% "", fixed = TRUE))
               .sl_paren_target(tokens, rec[[i]]) else NA_character_
      tbl[i] <- if (!is.na(tgt)) tgt else last
      if (is.na(tbl[i]))
        warn <- c(warn, sprintf("line %d: JOIN/CONCATENATE with no target and no table before it",
                                rec[[i]]$line_start))
    } else if (kind[i] == "multi-table") {
      tbl[i] <- NA_character_
      warn <- c(warn, sprintf("line %d: `FROM %s` expands to one table per matching qvd; names and fields are outside the script",
                              rec[[i]]$line_start, rec[[i]]$src))
    } else if (kind[i] == "preceding" || top != i) {
      tbl[i] <- NA_character_   # filled below from the chain top
    } else {
      tbl[i] <- rec[[i]]$label
      if (is.na(tbl[i])) tbl[i] <- .sl_autoname(rec[[i]]$kind, rec[[i]]$src)
      if (is.na(tbl[i]))
        warn <- c(warn, sprintf("line %d: LOAD with no label and no name derivable from its source",
                                rec[[i]]$line_start))
      if (kind[i] != "mapping") last <- tbl[i]
    }
  }
  # a chain member inherits the name its top exposes
  for (i in seq_len(n)) if (is.na(tbl[i])) tbl[i] <- tbl[chain_of[i]]

  # --- declared field names ------------------------------------------------
  declared <- vector("list", n)
  wild     <- vector("list", n)
  for (i in seq_len(n)) {
    nm <- character(0); sn <- character(0)
    ln <- integer(0); al <- logical(0); w <- FALSE
    for (k in rec[[i]]$seg) {
      x <- s[[k]]
      if (.sl_is_wildcard(tokens, x$content_idx[1L],
                          x$content_idx[length(x$content_idx)]) && !x$has_as) {
        w <- TRUE
        next
      }
      j <- if (x$has_as) x$alias_content_idx else x$content_idx
      # `LOAD DISTINCT [Trip ID], ...` — the qualifier rides along on the first
      # field's content, so an un-aliased one would be named `DISTINCT[Trip ID]`.
      if (!x$has_as && length(j) > 1L)
        j <- j[!(tokens$type[j] == "WORD" &
                 toupper(tokens$text[j]) %in% .SL_QUALIFIERS)]
      # The SOURCE side — left of any `AS` — is the name the qvd actually
      # holds, and the only one a retarget may rewrite (DESIGN §6.6): the
      # alias is the app's internal name and renaming it breaks every chart.
      # GOTCHA: `content_idx` spans the WHOLE segment, both sides of the `AS`
      # — taking it whole yields `Grant Activity IdASGrant Activity Id%`.
      # Split on `as_idx`, mirroring how `alias_content_idx` is derived.
      # The qualifier strip applies here whether or not an alias follows,
      # since the qualifier always rides on the CONTENT of the first field.
      sj <- if (x$has_as) x$content_idx[x$content_idx < x$as_idx] else x$content_idx
      if (length(sj) > 1L)
        sj <- sj[!(tokens$type[sj] == "WORD" &
                   toupper(tokens$text[sj]) %in% .SL_QUALIFIERS)]
      if (!x$has_as)
        warn <- c(warn, sprintf("line %d: field with no AS; produced name inferred from `%s`",
                                x$line, paste(tokens$text[j], collapse = "")))
      nm <- c(nm, .sl_name(tokens$text[j], tokens$type[j]))
      sn <- c(sn, .sl_name(tokens$text[sj], tokens$type[sj]))
      ln <- c(ln, x$line); al <- c(al, x$has_as)
    }
    declared[[i]] <- data.frame(field = nm, source_field = sn, line = ln,
                                aliased = al, stringsAsFactors = FALSE)
    wild[[i]] <- w
  }

  # --- resolve wildcards ---------------------------------------------------
  # A `*` takes the fields of whatever feeds this load. Adam 2026-08-18: in a
  # preceding chain that is the statement BELOW, not the underlying qvd — the
  # chain's field universe is set by its bottom LOAD's own list, so a `LOAD *`
  # over `LOAD [year], [quarter] FROM big.qvd` yields exactly two fields.
  # A single forward pass suffices: RESIDENT can only name a table already
  # built, and a chain resolves upward from a bottom that is never a wildcard
  # over another chain.
  resolved <- vector("list", n)
  complete <- rep(TRUE, n)
  by_table <- list()
  for (i in rev(seq_len(n))) {
    extra <- character(0)
    if (isTRUE(wild[[i]])) {
      if (!is.na(feeds_into[i])) {
        extra <- resolved[[feeds_into[i]]]$field
        complete[i] <- complete[feeds_into[i]]
      } else if (rec[[i]]$kind == "resident" && !is.na(rec[[i]]$src) &&
                 !is.null(by_table[[rec[[i]]$src]])) {
        extra <- by_table[[rec[[i]]$src]]
      } else if (rec[[i]]$kind == "inline" && length(rec[[i]]$inline_fields)) {
        extra <- rec[[i]]$inline_fields
      } else if (is_map[i]) {
        # A MAPPING table is consumed by ApplyMap and dropped; it never enters
        # the data model, so an unresolvable `*` in one costs nothing.
        extra <- character(0)
      } else {
        # No warning yet: a RESIDENT `*` naming a table built further up is
        # still rescued by the second pass below, once every table is known.
        complete[i] <- FALSE
      }
    }
    d <- declared[[i]]
    resolved[[i]] <- rbind(
      .sl_wild_rows(extra, rec[[i]]$line_start),
      if (nrow(d)) data.frame(d, via = "declared", stringsAsFactors = FALSE))
  }
  # a forward pass to register finished tables for later RESIDENT lookups
  for (i in seq_len(n)) {
    if (!is.na(tbl[i]) && kind[i] != "mapping")
      by_table[[tbl[i]]] <- unique(c(by_table[[tbl[i]]], resolved[[i]]$field))
  }
  # second wildcard pass, now that RESIDENT targets are known
  for (i in seq_len(n)) {
    if (isTRUE(wild[[i]]) && !complete[i] && rec[[i]]$kind == "resident" &&
        !is.na(rec[[i]]$src) && !is.null(by_table[[rec[[i]]$src]])) {
      resolved[[i]] <- rbind(
        .sl_wild_rows(by_table[[rec[[i]]$src]], rec[[i]]$line_start),
        resolved[[i]][resolved[[i]]$via == "declared", ])
      complete[i] <- TRUE
    }
  }
  # multi-table loads already carry a warning that says more than this one would
  for (i in setdiff(which(!complete), which(is_multi)))
    warn <- c(warn, sprintf("line %d: `LOAD *` from %s — field list not knowable from the script; %s can only be judged whole",
                            rec[[i]]$line_start, rec[[i]]$kind,
                            if (is.na(tbl[i])) "this table" else tbl[i]))

  loads <- data.frame(
    load_id = seq_len(n), table = tbl, producer_kind = kind,
    prefix = vapply(rec, function(r) r$prefix %||% NA_character_, character(1)),
    source_kind = vapply(rec, function(r) r$kind, character(1)),
    source = vapply(rec, function(r) r$src %||% NA_character_, character(1)),
    line_start = vapply(rec, function(r) r$line_start, integer(1)),
    line_end = vapply(rec, function(r) r$line_end, integer(1)),
    chain_of = chain_of,
    n_declared = vapply(declared, nrow, integer(1)),
    inline_rows = vapply(rec, function(r) r$inline_rows, integer(1)),
    complete_fields = complete, stringsAsFactors = FALSE)
  loads$tab <- vapply(loads$line_start, function(x) .sl_tab_at(tabs, x), character(1))

  inlines <- do.call(rbind, lapply(which(!is.na(loads$inline_rows)), function(i)
    data.frame(load_id = i, table = tbl[i], tab = loads$tab[i],
               n_rows = rec[[i]]$inline_rows,
               header = paste(rec[[i]]$inline_fields, collapse = ", "),
               sample = paste(rec[[i]]$inline_sample, collapse = "\n"),
               line_start = rec[[i]]$line_start, line_end = rec[[i]]$line_end,
               stringsAsFactors = FALSE)))
  if (is.null(inlines))
    inlines <- data.frame(load_id = integer(0), table = character(0),
                          tab = character(0), n_rows = integer(0),
                          header = character(0), sample = character(0),
                          line_start = integer(0), line_end = integer(0),
                          stringsAsFactors = FALSE)

  fields <- do.call(rbind, lapply(seq_len(n), function(i) {
    r <- resolved[[i]]
    if (is.null(r) || nrow(r) == 0L) return(NULL)
    data.frame(load_id = i, table = tbl[i], r, row.names = NULL,
               stringsAsFactors = FALSE)
  }))
  if (is.null(fields)) fields <- .sl_empty_fields()

  # A table can be produced with no LOAD at all — see .sl_bare_selects().
  claimed <- unlist(lapply(rec, function(r)
    if (identical(r$kind, "select")) .sl_next_solid(tokens, r$stmt_end + 1L) else NULL))
  bare <- .sl_bare_selects(tokens, tabs, claimed, n)
  if (nrow(bare$loads)) {
    loads  <- rbind(loads, bare$loads)
    fields <- rbind(fields, bare$fields)
    for (i in which(is.na(bare$loads$table)))
      warn <- c(warn, sprintf("line %d: bare SQL SELECT with no table label",
                              bare$loads$line_start[i]))
  }

  list(loads = loads, fields = fields, inlines = inlines, warnings = warn)
}

# --- bare SQL SELECT ------------------------------------------------------
#
# A table can be produced with no LOAD at all:
#
#   [TravelAreas]:
#   SQL SELECT [Office], [Dataset_Cost] FROM TravelAreas(...);
#
# find_load_segments() is built on LOAD field lists, so it never sees one of
# these, and app-unbuilt's model was 4 tables short as a result (Adam
# 2026-08-18). They are modelled here and given their own `source_kind`,
# `sql-select`, rather than being folded into `select`: a database call is
# the single most expensive thing to carry to Cloud, so the report has to be
# able to count this form separately, not merely include it in a total.

# Statement-initial SELECT / SQL SELECT that is not already the second half
# of a `LOAD ...; SELECT ...;` pair.
.sl_bare_select_idx <- function(tokens, claimed) {
  up   <- toupper(tokens$text)
  cand <- which(tokens$type == "WORD" & up %in% c("SELECT", "SQL"))
  cand <- setdiff(cand, claimed)
  keep <- vapply(cand, function(i) {
    p <- .sl_prev_solid_idx(tokens, i - 1L)
    if (is.na(p)) return(TRUE)
    tokens$type[p] == "SEMI" || tokens$text[p] == ":"
  }, logical(1))
  cand <- cand[keep]
  # `SQL SELECT` is two tokens; keep only the first of an adjacent pair.
  if (length(cand) > 1L) {
    nxt <- vapply(cand, function(i) .sl_next_solid(tokens, i + 1L), integer(1))
    cand <- cand[!(cand %in% nxt)]
  }
  cand
}

.sl_prev_solid_idx <- function(tokens, from) {
  if (from < 1L) return(NA_integer_)
  i <- from
  while (i >= 1L && tokens$type[i] %in% c("WS", "COMMENT", "VOID")) i <- i - 1L
  if (i < 1L) NA_integer_ else i
}

# The label above a bare SELECT: `[Name]:` — the colon is what makes it one.
.sl_bare_label <- function(tokens, idx) {
  p <- .sl_prev_solid_idx(tokens, idx - 1L)
  if (is.na(p) || tokens$text[p] != ":") return(NA_character_)
  q <- .sl_prev_solid_idx(tokens, p - 1L)
  if (is.na(q) || !tokens$type[q] %in% c("BRACKET", "DQUOTE", "WORD")) return(NA_character_)
  undelimit(tokens$text[q], tokens$type[q])
}

# The column list, between SELECT and its FROM. Depth-0 commas separate the
# columns; a bare `*` means the field list is not knowable from the script,
# exactly as it does for a `LOAD *`.
.sl_bare_fields <- function(tokens, lo, hi) {
  idx <- lo:hi
  up  <- toupper(tokens$text[idx])
  dep <- cumsum(tokens$type[idx] == "LPAREN") - cumsum(tokens$type[idx] == "RPAREN")
  f   <- which(up == "FROM" & dep == 0L & tokens$type[idx] == "WORD")
  if (!length(f)) return(list(fields = character(0), complete = FALSE))
  span <- idx[seq_len(f[1L] - 1L)]
  span <- span[-1L]                       # drop SELECT itself
  if (!length(span)) return(list(fields = character(0), complete = FALSE))
  d <- cumsum(tokens$type[span] == "LPAREN") - cumsum(tokens$type[span] == "RPAREN")
  cut <- which(tokens$type[span] == "COMMA" & d == 0L)
  parts <- split(span, cumsum(seq_along(span) %in% (cut + 1L)))
  out <- vapply(parts, function(p) {
    p <- p[!tokens$type[p] %in% c("WS", "COMMENT", "VOID", "COMMA")]
    if (!length(p)) return(NA_character_)
    k <- p[tokens$type[p] %in% c("BRACKET", "DQUOTE", "WORD")]
    if (!length(k)) return(NA_character_)
    undelimit(tokens$text[k[length(k)]], tokens$type[k[length(k)]])
  }, character(1), USE.NAMES = FALSE)
  out <- out[!is.na(out)]
  if (any(vapply(parts, function(p) any(tokens$text[p] == "*"), logical(1))))
    return(list(fields = out, complete = FALSE))
  list(fields = out, complete = length(out) > 0L)
}

#' Bare SQL SELECT statements as loads, in the shape script_loads() returns.
.sl_bare_selects <- function(tokens, tabs, claimed, next_id) {
  idx <- .sl_bare_select_idx(tokens, claimed)
  if (!length(idx))
    return(list(loads = .sl_empty_loads(), fields = .sl_empty_fields()))

  rows <- lapply(seq_along(idx), function(k) {
    i    <- idx[k]
    end  <- .sl_stmt_end(tokens, i)
    lab  <- .sl_bare_label(tokens, i)
    fl   <- .sl_bare_fields(tokens, i, end)
    id   <- next_id + k
    ls   <- tokens$line[if (is.na(lab)) i else max(1L, i - 4L)]
    list(load = data.frame(
           load_id = id, table = lab, producer_kind = "table",
           prefix = NA_character_, source_kind = "sql-select", source = NA_character_,
           line_start = tokens$line[i], line_end = tokens$line[end],
           chain_of = NA_integer_, n_declared = length(fl$fields),
           inline_rows = NA_integer_, complete_fields = fl$complete,
           tab = .sl_tab_at(tabs, tokens$line[i]), stringsAsFactors = FALSE),
         field = if (length(fl$fields))
           # A bare `SQL SELECT` column list is read straight from the
           # database object, so source and produced name coincide.
           data.frame(load_id = id, table = lab, field = fl$fields,
                      source_field = fl$fields,
                      line = tokens$line[i], aliased = FALSE, via = "declared",
                      stringsAsFactors = FALSE) else NULL)
  })
  list(loads  = do.call(rbind, lapply(rows, `[[`, "load")),
       fields = do.call(rbind, lapply(rows, `[[`, "field")) %||% .sl_empty_fields())
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

# `JOIN (Target) LOAD` — the target sits between the head's parentheses.
.sl_paren_target <- function(tokens, r) {
  idx <- .sl_solid(tokens$type, r$head_start, r$load_tok - 1L)
  lp  <- which(tokens$type[idx] == "LPAREN")[1L]
  rp  <- which(tokens$type[idx] == "RPAREN")[1L]
  if (is.na(lp) || is.na(rp) || rp <= lp + 1L) return(NA_character_)
  inner <- idx[(lp + 1L):(rp - 1L)]
  .sl_name(tokens$text[inner], tokens$type[inner])
}

# Wildcard-derived rows. Kept a function because `extra` is empty far more
# often than not, and data.frame() recycles a scalar `line` against a zero-row
# `field` into an error rather than an empty frame.
.sl_wild_rows <- function(field, line) {
  if (length(field) == 0L)
    return(data.frame(field = character(0), source_field = character(0),
                      line = integer(0), aliased = logical(0),
                      via = character(0), stringsAsFactors = FALSE))
  # A `*` reads whatever feeds it under its own name, so source and produced
  # name coincide — there is no `AS` to separate them.
  data.frame(field = field, source_field = field, line = line,
             aliased = NA, via = "wildcard", stringsAsFactors = FALSE)
}

.sl_empty_loads <- function()
  data.frame(load_id = integer(0), table = character(0),
             producer_kind = character(0), prefix = character(0),
             source_kind = character(0), source = character(0),
             line_start = integer(0), line_end = integer(0),
             chain_of = integer(0), n_declared = integer(0),
             inline_rows = integer(0), complete_fields = logical(0),
             stringsAsFactors = FALSE)

.sl_empty_inlines <- function()
  data.frame(load_id = integer(0), table = character(0), tab = character(0),
             n_rows = integer(0), header = character(0), sample = character(0),
             line_start = integer(0), line_end = integer(0),
             stringsAsFactors = FALSE)

.sl_empty_fields <- function()
  data.frame(load_id = integer(0), table = character(0), field = character(0),
             source_field = character(0),
             line = integer(0), aliased = logical(0), via = character(0),
             stringsAsFactors = FALSE)

#' As script_loads(), from a file path.
read_script_loads <- function(path) script_loads(read_qlik_script(path))

# Run directly for a summary; `--csv <stem>` writes <stem>-loads.csv and
# <stem>-fields.csv.
main <- function(args) {
  stem <- if ("--csv" %in% args) args[match("--csv", args) + 1L] else NA
  args <- setdiff(args, c("--csv", stem))
  if (length(args) == 0L) stop("usage: Rscript script_loads.R <script> [--csv <stem>]")

  r <- read_script_loads(args[1L])
  l <- r$loads

  cat(sprintf("%s: %d loads -> %d tables, %d field rows, %d unique names\n",
              args[1L], nrow(l), length(unique(na.omit(l$table))),
              nrow(r$fields), length(unique(r$fields$field))))
  k <- table(l$producer_kind)
  cat("  producers: ", paste(names(k), k, sep = "=", collapse = ", "), "\n", sep = "")
  k <- table(l$source_kind)
  cat("  sources:   ", paste(names(k), k, sep = "=", collapse = ", "), "\n", sep = "")
  cat("  wildcard-resolved field rows: ", sum(r$fields$via == "wildcard"), "\n", sep = "")
  bad <- l[!l$complete_fields, ]
  cat("  tables with an UNRESOLVED `*` (whole-table verdict only): ", nrow(bad), "\n", sep = "")
  if (nrow(bad)) print(bad[, c("load_id", "table", "source_kind", "source", "line_start")],
                       row.names = FALSE)
  # Migration-debt signals (DESIGN §7.1), reported here because this is where
  # they are detected, not because step 3 judges them.
  cat("  direct database calls (LOAD;SELECT — will not run on Cloud): ",
      sum(l$source_kind == "select"), "\n", sep = "")
  inl <- l[!is.na(l$inline_rows), ]
  if (nrow(inl))
    cat("  inline loads: ", nrow(inl), "; largest ", max(inl$inline_rows),
        " rows (", inl$table[which.max(inl$inline_rows)], ")\n", sep = "")
  cat("  unnamed loads: ", sum(is.na(l$table)), "\n", sep = "")
  cat("  warnings: ", length(r$warnings), "\n", sep = "")
  if (length(r$warnings)) cat(paste0("    ", head(unique(r$warnings), 10L), collapse = "\n"), "\n")

  if (!is.na(stem)) {
    write.csv(l, paste0(stem, "-loads.csv"), row.names = FALSE)
    write.csv(r$fields, paste0(stem, "-fields.csv"), row.names = FALSE)
    cat("wrote ", stem, "-loads.csv and ", stem, "-fields.csv\n", sep = "")
  }
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
