# qlik_reserved_words.R
#
# The Qlik script vocabulary, split by how a casing pass is allowed to treat
# each word. Data only - no logic. Sourced by enforce_reserved_word_case.R,
# and available to any later pass that needs to recognise Qlik vocabulary.
#
# WHY TWO LISTS
#
# Qlik's docs are explicit: "All script keywords can be typed with any
# combination of lower case and upper case characters. Field and variable
# names used in the statements are however case sensitive."
#
# So uppercasing a keyword is always safe, but uppercasing something that is
# actually a FIELD name renames the field. Many built-in function names are
# also plausible field names - this codebase has a real example, a bare
# `Year as [Data x Reg Year]` in app-unbuilt/script.qvs line 837, where Year
# is a field reference and not a call.
#
# Hence:
#   QLIK_KEYWORDS  - uppercased on the word alone. Statement, control and
#                    prefix words, plus LOAD clause words and operators.
#                    Measured against both fixtures: every occurrence in
#                    field-content position was a genuine keyword (as, like,
#                    and, or, distinct), never a field name.
#   QLIK_FUNCTIONS - uppercased ONLY when the next non-trivia token is "(",
#                    i.e. in call position. This is what protects bare field
#                    references that happen to share a function's name.
#
# A word may appear in both lists (Left, Right, Replace, Keep, Join, First,
# Sample, Only, Add, Merge, Mapping): those are both prefixes and functions.
# Membership of QLIK_KEYWORDS wins, since it is unconditional.
#
# PROVENANCE
#
# Entries marked [docs] were taken from Qlik Cloud help, retrieved 2026-08-14:
#   Script control statements, Script regular statements, Script prefixes,
#   String / Date and time / Conditional / Interpretation / Formatting /
#   Mapping / NULL / Counter / Table / Inter-record / General numeric /
#   Basic aggregation / Geospatial function pages.
# Entries marked [curated] are well-established functions not captured by
# those particular pages. They are safe to extend - a missing function only
# means it keeps its existing casing, never that anything breaks.
#
# Everything is stored lower case; matching is done case-insensitively and
# the pass emits toupper(). Note the tokenizer's WORD pattern includes "#",
# so Date#, Num#, Time#, Timestamp#, Interval# and Money# are single tokens
# and need their own entries.

QLIK_KEYWORDS <- c(
  # --- control statements [docs] ---
  "call", "do", "loop", "exit", "for", "each", "next", "if", "then",
  "elseif", "else", "end", "sub", "switch", "case", "default", "when",
  "unless", "while", "until", "to", "step",

  # --- regular statements [docs] ---
  "alias", "binary", "comment", "connect", "constrain", "declare", "derive",
  "directory", "disconnect", "drop", "flushlog", "force", "load", "let",
  "loosen", "map", "nullasnull", "nullasvalue", "qualify", "rem", "rename",
  "section", "select", "set", "sleep", "sql", "sqlcolumns", "sqltables",
  "sqltypes", "star", "store", "tag", "trace", "unmap", "unqualify",
  "untag", "variable",

  # --- prefixes [docs] ---
  "add", "concatenate", "crosstable", "first", "generic", "hierarchy",
  "hierarchybelongsto", "inner", "intervalmatch", "join", "keep", "left",
  "mapping", "merge", "noconcatenate", "outer", "replace", "right",
  "sample", "semantic",

  # --- LOAD clause words and operators [curated] ---
  # Not listed as "statements" in the docs, but they are structural script
  # vocabulary and appear constantly in field lists.
  "from", "resident", "inline", "autogenerate", "where", "group", "by",
  "order", "having", "distinct", "as", "into", "with", "lib", "script",
  "field", "fields", "table", "tables", "buffer", "bundle", "info",
  "execute", "on", "and", "or", "not", "xor", "like"
)

QLIK_FUNCTIONS <- c(
  # --- string [docs] ---
  "capitalize", "chr", "countregex", "evaluate", "extractregex",
  "extractregexgroup", "findoneof", "hash128", "hash160", "hash256",
  "index", "indexregex", "indexregexgroup", "isjson", "isregex", "jsonget",
  "jsonset", "keepchar", "left", "len", "levenshteindist", "lower", "ltrim",
  "matchregex", "mid", "ord", "purgechar", "repeat", "replace",
  "replaceregex", "replaceregexgroup", "right", "rtrim", "subfield",
  "subfieldregex", "substringcount", "textbetween", "trim", "upper",

  # --- date and time [docs] ---
  "addmonths", "addyears", "age", "converttolocaltime", "day", "dayend",
  "daylightsaving", "dayname", "daynumberofquarter", "daynumberofyear",
  "daystart", "firstworkdate", "gmt", "hour", "inday", "indaytotime",
  "inlunarweek", "inlunarweektodate", "inmonth", "inmonths",
  "inmonthstodate", "inmonthtodate", "inquarter", "inquartertodate",
  "inweek", "inweektodate", "inyear", "inyeartodate", "lastworkdate",
  "localtime", "lunarweekend", "lunarweekname", "lunarweekstart",
  "makedate", "maketime", "makeweekdate", "minute", "month", "monthend",
  "monthname", "monthsend", "monthsname", "monthsstart", "monthstart",
  "networkdays", "now", "quarter", "quarterend", "quartername",
  "quarterstart", "second", "setdateyear", "setdateyearmonth", "timezone",
  "today", "utc", "week", "weekday", "weekend", "weekname", "weekstart",
  "weekyear", "year", "yearend", "yearname", "yearstart", "yeartodate",

  # --- conditional [docs] ---
  "alt", "class", "coalesce", "if", "match", "mixmatch", "pick", "wildmatch",

  # --- interpretation [docs] (note the "#" forms) ---
  "date#", "interval#", "money#", "num#", "text", "time#", "timestamp#",

  # --- formatting [docs] ---
  "applycodepage", "date", "dual", "interval", "money", "num", "time",
  "timestamp",

  # --- mapping [docs] ---
  "applymap", "mapsubstring",

  # --- NULL [docs] ---
  "emptyisnull", "isnull", "null",

  # --- counter [docs] ---
  "autonumber", "autonumberhash128", "autonumberhash256", "iterno", "recno",
  "rowno",

  # --- table [docs] ---
  "fieldname", "fieldnumber", "nooffields", "noofrows", "nooftables",
  "tablename", "tablenumber",

  # --- inter-record [docs] ---
  "exists", "lookup", "peek", "previous",

  # --- general numeric [docs] ---
  "bitcount", "ceil", "combin", "div", "even", "fabs", "fact", "floor",
  "fmod", "frac", "mod", "odd", "permut", "round", "sign",

  # --- aggregation [docs: basic page] + [curated: the rest] ---
  "firstsortedvalue", "max", "min", "mode", "only", "sum",
  "count", "avg", "concat", "minstring", "maxstring", "median", "stdev",
  "fractile", "correl", "kurtosis", "skew", "sterr", "numericcount",
  "textcount", "nullcount", "missingcount",

  # --- geospatial [docs] ---
  "geoaggrgeometry", "geoboundingbox", "geocountvertex", "geogetboundingbox",
  "geogetpolygoncenter", "geoinvprojectgeometry", "geomakepoint",
  "geoproject", "geoprojectgeometry", "georeducegeometry", "makegeoline",
  "makegeopoint", "makegeopolygon", "makegeoregion",

  # --- logical [curated] ---
  "isnum", "istext",

  # --- exponential, logarithmic and mathematical [curated] ---
  "exp", "log", "log10", "pow", "sqrt", "pi", "rand",

  # --- range [curated] ---
  "rangesum", "rangemax", "rangemin", "rangeavg", "rangecount",
  "rangemissingcount", "rangenullcount", "rangenumericcount",
  "rangetextcount", "rangefractile", "rangemode", "rangeonly", "rangestdev",
  "rangemaxstring", "rangeminstring", "rangeirr", "rangenpv", "rangexirr",
  "rangexnpv",

  # --- field [curated] ---
  "fieldindex", "fieldvalue", "fieldvaluecount",

  # --- system [curated] ---
  "author", "clientplatform", "computername", "documentname", "documentpath",
  "documenttitle", "engineversion", "getcollationlocale", "ispartialreload",
  "osuser", "productversion", "reloadtime", "scripterror",
  "scripterrorcount", "scripterrordetails", "scripterrorlist", "statename",

  # --- file [curated] ---
  "attribute", "connectstring", "filebasename", "filedir", "fileextension",
  "filename", "filepath", "filesize", "filetime", "getfolderpath",
  "qvdcreatetime", "qvdfieldname", "qvdnooffields", "qvdnoofrecords",
  "qvdtablename",

  # --- financial [curated] ---
  "blackandschole", "fv", "nper", "pmt", "pv", "rate",

  # --- trigonometric and hyperbolic [curated] ---
  "cos", "acos", "sin", "asin", "tan", "atan", "atan2", "cosh", "sinh",
  "tanh",

  # --- colour [curated] ---
  "argb", "rgb", "hsl", "color"
)
