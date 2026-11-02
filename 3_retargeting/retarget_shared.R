# retarget_shared.R - helpers retarget_loads.R uses to find the loads in a
# token stream.
#
# Requires shared/qlik_tokenizer.R to be sourced first (find_load_segments,
# next_non_trivia_idx, undelimit).

# Every LOAD keyword whose field list terminates on a depth-0 WORD "from"
# (find_load_segments' own end-keyword scan), with its FROM path token index
# and its own segments. LOADs terminating on resident/inline/autogenerate/
# where/; (no FROM) are not returned.
rt_find_from_loads <- function(tokens) {
  n <- nrow(tokens)
  fls <- find_load_segments(tokens)
  if (length(fls$segments) == 0) return(list())
  load_ids <- unique(vapply(fls$segments, function(s) s$load_tok_idx, integer(1)))
  nxt <- next_non_trivia_idx(tokens$type)
  out <- list()
  for (lid in load_ids) {
    segs_here <- Filter(function(s) s$load_tok_idx == lid, fls$segments)
    max_end <- max(vapply(segs_here, function(s) s$end, integer(1)))
    end_kw_idx <- max_end + 1L
    if (end_kw_idx > n) next
    if (tokens$type[end_kw_idx] != "WORD" || tolower(tokens$text[end_kw_idx]) != "from") next
    path_idx <- nxt[end_kw_idx]
    if (is.na(path_idx)) next
    if (!(tokens$type[path_idx] %in% c("BRACKET", "DQUOTE"))) next
    out[[length(out) + 1L]] <- list(
      load_tok_idx = lid, from_idx = end_kw_idx, path_idx = path_idx,
      segments = segs_here)
  }
  out
}

# The `Name:` label immediately preceding a LOAD keyword, undelimited; ""
# when the LOAD is unlabelled. `prevnt` = prev_non_trivia_idx(tokens$type),
# passed in because callers already have it.
rt_table_label <- function(tokens, load_idx, prevnt) {
  c1 <- prevnt[load_idx]
  if (is.na(c1)) return("")
  if (tokens$type[c1] == "OTHER" && tokens$text[c1] == ":") {
    c2 <- prevnt[c1]
    if (!is.na(c2) && tokens$type[c2] %in% c("WORD", "BRACKET", "DQUOTE")) {
      return(undelimit(tokens$text[c2], tokens$type[c2]))
    }
  }
  ""
}
