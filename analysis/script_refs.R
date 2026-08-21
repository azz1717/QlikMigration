# script_refs.R — how the SCRIPT itself uses the tables it builds.
#
# Phase 2, step 4a (DESIGN §6.5). NOT part of the styling pipeline; nothing in
# run_pipeline.R sources this. Base R only.
#
# Why this exists: cross-referencing the app against the script alone would
# mark every intermediate table unreferenced and recommend deleting it. A
# `_Temp` table read by a later RESIDENT load, or a mapping table consumed by
# ApplyMap, is used — just not by a chart. app-unbuilt has 44 RESIDENT reads
# and 24 DROPs, so the difference is not marginal.
#
# Matching is on TYPED TOKENS, never raw text. That is not a style preference:
# a raw-text scan of app-unbuilt reports 18 STOREs, and the real number is
# ZERO — every hit is the substring "Store" inside the connection path
# `lib://Curated Data Store:DataFiles/...`, which lives in a BRACKET token.
# Same trap the tokenizer notes for `for` (1242 raw, ~28 real).

source("shared/qlik_tokenizer.R")

# Statement id per token, and the first solid WORD of each statement. The
# leading word is what says whether a mention is a use (`RESIDENT Temp`) or a
# disposal (`DROP TABLE Temp`) — the same name means opposite things.
.sr_statements <- function(tokens) {
  sid <- cumsum(c(0L, head(tokens$type == "SEMI", -1L))) + 1L
  solid <- !(tokens$type %in% c("WS", "COMMENT", "VOID"))
  first <- tapply(which(solid), sid[solid], function(i) i[1L])
  lead <- rep(NA_character_, max(sid))
  fi <- as.integer(unlist(first))
  lead[as.integer(names(first))] <- tolower(tokens$text[fi])
  list(sid = sid, lead = lead)
}

#' Every mention of a known table name in the script.
#'
#' @param tokens as `read_qlik_script()`.
#' @param table_names character vector of tables the script builds.
#' @param own data.frame(table, line_start, line_end) — each table's own
#'   producing statement, whose self-mention is not a use.
#' @return data.frame(table, line, lead, kind) with `kind` one of
#'   `use` / `drop` / `store` / `self`.
#'
#' Deliberately crude and deliberately over-inclusive: any token whose
#' undelimited text equals a table name counts, wherever it appears. A false
#' "used" keeps a table that could have gone; a false "unused" deletes a live
#' one. Only the first is recoverable, so the scan errs that way.
script_table_refs <- function(tokens, table_names, own = NULL) {
  keep <- !is.na(table_names) & nzchar(table_names)
  table_names <- unique(table_names[keep])
  if (length(table_names) == 0L)
    return(data.frame(table = character(0), line = integer(0),
                      lead = character(0), kind = character(0),
                      stringsAsFactors = FALSE))

  lut <- setNames(table_names, tolower(table_names))
  st  <- .sr_statements(tokens)

  cand <- which(tokens$type %in% c("WORD", "BRACKET", "DQUOTE", "SQUOTE"))
  txt  <- tolower(vapply(cand, function(i)
    undelimit(tokens$text[i], tokens$type[i]), character(1)))
  hit  <- cand[txt %in% names(lut)]
  if (length(hit) == 0L)
    return(data.frame(table = character(0), line = integer(0),
                      lead = character(0), kind = character(0),
                      stringsAsFactors = FALSE))

  tbl  <- unname(lut[tolower(vapply(hit, function(i)
    undelimit(tokens$text[i], tokens$type[i]), character(1)))])
  lead <- st$lead[st$sid[hit]]
  line <- tokens$line[hit]

  kind <- ifelse(lead %in% c("drop"), "drop",
          ifelse(lead %in% c("store"), "store", "use"))

  # A table naming itself in its own LOAD is not a use of it.
  if (!is.null(own) && nrow(own)) {
    for (k in seq_len(nrow(own))) {
      self <- tbl == own$table[k] & line >= own$line_start[k] &
              line <= own$line_end[k]
      kind[self] <- "self"
    }
  }

  data.frame(table = tbl, line = line, lead = lead, kind = kind,
             stringsAsFactors = FALSE)
}

#' Tables disposed of with DROP, and tables written out with STORE.
#'
#' @return list(drops, stores) — drops: data.frame(table, line);
#'   stores: data.frame(table, into, line).
#'
#' STORE is its own scanner rather than a `kind` because the target matters:
#' a load script that generates a qvd violates the Cloud build standards
#' regardless of whether the table is otherwise used (DESIGN §7.1).
script_disposals <- function(tokens, table_names) {
  refs <- script_table_refs(tokens, table_names)
  drops <- refs[refs$kind == "drop", c("table", "line")]

  stores <- data.frame(table = character(0), into = character(0),
                       line = integer(0), stringsAsFactors = FALSE)
  st <- .sr_statements(tokens)
  for (s in which(st$lead == "store")) {
    idx <- which(st$sid == s & !(tokens$type %in% c("WS", "COMMENT", "VOID")))
    if (length(idx) < 2L) next
    low  <- tolower(tokens$text[idx])
    into <- which(low == "into")[1L]
    if (is.na(into)) next
    nm <- idx[setdiff(seq_len(into - 1L), which(low[seq_len(into - 1L)] %in%
                                                c("store", "*", "from")))]
    tg <- idx[into + 1L]
    stores <- rbind(stores, data.frame(
      table = if (length(nm)) undelimit(tokens$text[nm[1L]], tokens$type[nm[1L]]) else NA_character_,
      into  = if (!is.na(tg)) undelimit(tokens$text[tg], tokens$type[tg]) else NA_character_,
      line  = tokens$line[idx[1L]], stringsAsFactors = FALSE))
  }

  list(drops = drops, stores = stores)
}

main <- function(args) {
  if (length(args) == 0L) stop("usage: Rscript script_refs.R <script>")
  source("analysis/script_loads.R")
  tok <- read_qlik_script(args[1L])
  l   <- script_loads(tok)$loads
  own <- unique(l[!is.na(l$table), c("table", "line_start", "line_end")])
  nms <- unique(na.omit(l$table))

  refs <- script_table_refs(tok, nms, own)
  dis  <- script_disposals(tok, nms)

  cat(args[1L], ": ", length(nms), " tables\n", sep = "")
  cat("  mentions by kind: ",
      paste(names(table(refs$kind)), table(refs$kind), sep = "=", collapse = ", "),
      "\n", sep = "")
  used <- unique(refs$table[refs$kind == "use"])
  cat("  tables the SCRIPT reads elsewhere: ", length(used), "\n", sep = "")
  cat("  tables DROPped: ", length(unique(dis$drops$table)), "\n", sep = "")
  cat("  STORE statements: ", nrow(dis$stores), "\n", sep = "")
  if (nrow(dis$stores)) print(dis$stores, row.names = FALSE)
  cat("  never read by the script (candidates for step 4's tier 1): ",
      length(setdiff(nms, used)), "\n", sep = "")
}

if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
