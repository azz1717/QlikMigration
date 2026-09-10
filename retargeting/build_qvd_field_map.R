## build_qvd_field_map.R
## Part 2 rebuild + 6-app extension (2026-08-25/26). Unions the original
## 487 literal-app truth rows + 6 pattern-apps (incl. IAM, new) with the
## new-schema lineage CSVs for GPS / IEP01 / IEP01s / FUSION / Geospatial,
## classifies every field against fixtures/views.csv + fixtures/DBfixture1.csv,
## applies four owner correction directives (path normalization; AZURE/IPP
## QUALIFY-prefix fix; IPP retirement; IEP Temp->final pairing), and writes
## retargeting/qvd_field_map.csv.
## Base R only. Deterministic (alphabetical tie-break, disclosed via a
## FLAG count, never silently resolved). Idempotent: run twice -> identical bytes.

root    <- "C:/Rtools"
scratch <- "C:/Users/Adam/AppData/Local/Temp/claude/C--Rtools/94f114db-c53f-4955-a811-2a2668b905cd/scratchpad"

source(file.path(root, "retargeting", "map_upkeep.R"))

## ---------------------------------------------------------------------
## Optional CLI overrides (M5, PLAN-fleet.md section 6). Every default is
## the exact path this script has always hardcoded, so a bare run is
## byte-for-byte what it was; map_refresh.R passes --db/--schemas/--out
## through so a rebuild can be pointed at a fresher extract, or at a
## scratch copy, without editing the script.
## ---------------------------------------------------------------------
.bq_args <- commandArgs(trailingOnly = TRUE)
map_check_flags(.bq_args, c("--db", "--schemas", "--out"))
db_path      <- map_opt(.bq_args, "--db",      file.path(root, "fixtures", "DBfixture1.csv"))
schemas_path <- map_opt(.bq_args, "--schemas", file.path(root, "fixtures", "loaded_schemas.csv"))
out_path     <- map_opt(.bq_args, "--out",     file.path(root, "retargeting", "qvd_field_map.csv"))

up <- function(x) toupper(trimws(x))
KSEP <- "\u0001"

## ---------------------------------------------------------------------
## Directive 1: path normalization. Relative-to-AzureDataLake, forward
## slashes, original case kept. Every lib:// path must strip cleanly at
## '/AzureDataLake/'; Geospatial qvds have NO AzureDataLake segment at all
## (lib://AppData\PROD\Geospatial\...) so a documented fallback strip at
## '/AppData/PROD/' is used for those and counted (n_geo_fallback) -- any
## path matching NEITHER marker is a hard STOP, not a guess.
## ---------------------------------------------------------------------
## The rule itself is qvd_relativize() in retargeting/map_upkeep.R (promoted
## 2026-09-10, M5 -- map_add.R needs the identical normalisation and
## docs/verify_docs.R's twin check forbids a second copy). Only the
## fallback COUNTER stays here, since it is this script's own report line.
n_geo_fallback <- 0L
rp_relativize_vec <- function(xs) qvd_relativize(xs, function() n_geo_fallback <<- n_geo_fallback + 1L)
canon_key <- function(xs) toupper(rp_relativize_vec(xs))

## ---------------------------------------------------------------------
## Correction-pass reader: all lineage CSVs now share an 11-column schema
## (generator_app, qvd_path_raw, qvd_field, db_connection, db_schema,
## db_table, db_column, src_qvd_path, src_qvd_field, status, line).
## ---------------------------------------------------------------------
## Schema constant + reader live in retargeting/map_upkeep.R (MAP_LINEAGE_COLS,
## read_lineage_csv) since M5 -- map_add.R writes the same schema and must
## check it the same way.

## ---------------------------------------------------------------------
## 1. Existing 487 literal-app rows (same union lineage_cloud_join.R used).
##    Only the legacy 8 columns are used; db_connection/src_qvd_path/
##    src_qvd_field are unused here (all AzureDbProdNIAADL / NA anyway) --
##    preserves the ORIGINAL classification path byte-for-byte (G2).
## ---------------------------------------------------------------------
lineage_paths <- c(
  file.path(root, "retargeting", "generator_lineage.csv"),
  file.path(root, "retargeting", "lineage_crm.csv"),
  file.path(root, "retargeting", "lineage_aurion.csv")
)
legacy_cols <- c("generator_app","qvd_path_raw","qvd_field","db_schema","db_table","db_column","status","line")
lineage_all <- do.call(rbind, lapply(lineage_paths, function(p) read_lineage_csv(p)[, legacy_cols]))
existing <- lineage_all[lineage_all$status == "mapped", , drop = FALSE]
existing$qvd_path_temp <- NA_character_
existing$project <- "literal"
cat(sprintf("Existing literal-app 'mapped' rows: %d (expect 487)\n", nrow(existing)))

## ---------------------------------------------------------------------
## fixtures
## ---------------------------------------------------------------------
db1 <- read.csv(db_path, stringsAsFactors = FALSE,
                 check.names = FALSE, colClasses = "character")
qvdlist <- read.csv(file.path(root,"fixtures","qvdlist.csv"), stringsAsFactors = FALSE,
                     check.names = FALSE, colClasses = "character")
views <- read.csv(file.path(root,"fixtures","views.csv"), stringsAsFactors = FALSE,
                   check.names = FALSE, colClasses = "character")
loaded_schemas <- read.csv(schemas_path, stringsAsFactors = FALSE,
                            check.names = FALSE, colClasses = "character")

## on-prem qvd relpaths, normalised (case-insens, forward slashes, no ext-case)
qvd_norm <- toupper(gsub("\\\\","/", trimws(qvdlist$RelPath)))
qvd_exists <- function(relpath) toupper(gsub("\\\\","/", relpath)) %in% qvd_norm

## ---------------------------------------------------------------------
## Shared classification indexes (built once, used by BOTH the existing
## pipeline and the new-app pipeline) + classify_one(): the coordinator's
## Correction Pass 2 three-tier logic, unchanged, refactored into a
## reusable function instead of an inline loop.
## ---------------------------------------------------------------------
views$tbl_col_key <- paste(up(views$TABLE_SCHEMA), up(views$TABLE_NAME), up(views$COLUMN_NAME), sep = KSEP)
cand1_idx <- split(seq_len(nrow(views)), views$tbl_col_key)
## NEW tier-1 authority (Adam 2026-08-26): Qlik Cloud materializes every view
## of every LOADED schema, not just the fixtures/views.csv filtered extract.
## loaded_schema_set is the tier-1 gate below for the self-referential (idx2)
## view lookup -- views.csv is still read/used for the cross-view (idx1)
## candidate lookup, unaffected by this correction.
loaded_schema_set <- unique(up(loaded_schemas$Schema))

db1v <- db1[up(db1$TABLE_TYPE) == "VIEW", , drop = FALSE]
db1v$key <- paste(up(db1v$TABLE_SCHEMA), up(db1v$TABLE_NAME), up(db1v$COLUMN_NAME), sep = KSEP)
cand2_idx <- split(seq_len(nrow(db1v)), db1v$key)              ## view identity+column (self-referential)
db1v$st_key <- paste(up(db1v$TABLE_SCHEMA), up(db1v$TABLE_NAME), sep = KSEP)
cand2b_idx <- split(seq_len(nrow(db1v)), db1v$st_key)           ## view identity only (any column)

db1$st_key_all <- paste(up(db1$TABLE_SCHEMA), up(db1$TABLE_NAME), sep = KSEP)
cand3_idx <- split(seq_len(nrow(db1)), db1$st_key_all)          ## ANY TABLE_TYPE, schema+table only

n_ambig <- 0L
classify_one <- function(scol, sobj, scolm) {
  scol  <- if (is.na(scol))  "" else scol
  sobj  <- if (is.na(sobj))  "" else sobj
  scolm <- if (is.na(scolm)) "" else scolm
  rk  <- paste(up(scol), up(sobj), up(scolm), sep = KSEP)
  stk <- paste(up(scol), up(sobj), sep = KSEP)
  same_named_key <- stk

  idx1 <- cand1_idx[[rk]]
  if (!is.null(idx1) && length(idx1) > 0) {
    same_hit <- idx1[paste(up(views$VIEW_SCHEMA[idx1]), up(views$VIEW_NAME[idx1]), sep = KSEP) == same_named_key]
    if (length(same_hit) > 0) {
      pick <- same_hit[1]
    } else {
      distinct_names <- unique(paste(views$VIEW_SCHEMA[idx1], views$VIEW_NAME[idx1], sep = KSEP))
      if (length(distinct_names) > 1) n_ambig <<- n_ambig + 1L
      ord <- idx1[order(up(views$VIEW_SCHEMA[idx1]), up(views$VIEW_NAME[idx1]))]
      pick <- ord[1]
    }
    return(list(verdict = "in-cloud", cv_schema = views$VIEW_SCHEMA[pick], cv_name = views$VIEW_NAME[pick],
                cv_field = views$COLUMN_NAME[pick], ev_file = "fixtures/views.csv",
                ev = sprintf("VIEW %s.%s has %s (via different view)", views$VIEW_SCHEMA[pick], views$VIEW_NAME[pick], views$COLUMN_NAME[pick])))
  }

  idx2 <- cand2_idx[[rk]]
  if (!is.null(idx2) && length(idx2) > 0) {
    pick <- idx2[1]
    ev_file <- "fixtures/DBfixture1.csv"
    if (up(db1v$TABLE_SCHEMA[pick]) %in% loaded_schema_set) {
      return(list(verdict = "in-cloud", cv_schema = db1v$TABLE_SCHEMA[pick], cv_name = db1v$TABLE_NAME[pick],
                  cv_field = db1v$COLUMN_NAME[pick], ev_file = ev_file,
                  ev = sprintf("VIEW %s.%s has %s (schema loaded)", db1v$TABLE_SCHEMA[pick], db1v$TABLE_NAME[pick], db1v$COLUMN_NAME[pick])))
    } else {
      return(list(verdict = "import-view", cv_schema = db1v$TABLE_SCHEMA[pick], cv_name = db1v$TABLE_NAME[pick],
                  cv_field = db1v$COLUMN_NAME[pick], ev_file = ev_file,
                  ev = sprintf("VIEW %s.%s has %s (schema not loaded)", db1v$TABLE_SCHEMA[pick], db1v$TABLE_NAME[pick], db1v$COLUMN_NAME[pick])))
    }
  }

  idx2b <- cand2b_idx[[stk]]
  if (!is.null(idx2b) && length(idx2b) > 0) {
    return(list(verdict = "extend-view", cv_schema = db1v$TABLE_SCHEMA[idx2b[1]], cv_name = db1v$TABLE_NAME[idx2b[1]],
                cv_field = NA_character_, ev_file = "fixtures/DBfixture1.csv",
                ev = sprintf("VIEW %s.%s lacks %s", db1v$TABLE_SCHEMA[idx2b[1]], db1v$TABLE_NAME[idx2b[1]], scolm)))
  }

  idx3 <- cand3_idx[[stk]]
  if (!is.null(idx3) && length(idx3) > 0) {
    col_hit <- idx3[up(db1$COLUMN_NAME[idx3]) == up(scolm)]
    if (length(col_hit) > 0) {
      pick <- col_hit[1]
      return(list(verdict = "create-view", cv_schema = NA_character_, cv_name = NA_character_, cv_field = NA_character_,
                  ev_file = "fixtures/DBfixture1.csv",
                  ev = sprintf("TABLE %s.%s has %s", db1$TABLE_SCHEMA[pick], db1$TABLE_NAME[pick], db1$COLUMN_NAME[pick])))
    }
  }

  n_table_objs <- if (!is.null(idx3) && length(idx3) > 0) length(unique(paste(db1$TABLE_SCHEMA[idx3], db1$TABLE_NAME[idx3]))) else 0L
  list(verdict = "not-found", cv_schema = NA_character_, cv_name = NA_character_, cv_field = NA_character_,
       ev_file = "(none)", ev = sprintf("searched %s.%s.%s: %d views, %d tables", scol, sobj, scolm, 0L, n_table_objs))
}

## ---------------------------------------------------------------------
## 2. Pattern-app rows -- catalog-instantiated, cross-checked vs qvdlist.
##    UNCHANGED from the delivered script (CDP/TWES/Bushel/Inphinity/
##    AZURE-literal/IPP-literal), + IAM QVD Builder appended (new).
## ---------------------------------------------------------------------
pat_rows <- list()
flagged_no_qvd <- list()   ## catalog candidates with NO matching on-prem qvd

add_table_rows <- function(generator_app, schema, table, qvd_relpath, project="pattern",
                            qvd_field_override = NULL) {
  cols <- db1[up(db1$TABLE_SCHEMA) == up(schema) & up(db1$TABLE_NAME) == up(table), , drop = FALSE]
  if (nrow(cols) == 0) return(invisible(NULL))
  if (!qvd_exists(qvd_relpath)) {
    flagged_no_qvd[[length(flagged_no_qvd)+1]] <<- data.frame(
      generator_app = generator_app, schema = schema, table = table,
      candidate_path = qvd_relpath, stringsAsFactors = FALSE)
    return(invisible(NULL))
  }
  fld <- if (is.null(qvd_field_override)) cols$COLUMN_NAME else qvd_field_override
  d <- data.frame(
    generator_app = generator_app,
    qvd_path_raw  = qvd_relpath,
    qvd_field     = fld,
    db_schema     = cols$TABLE_SCHEMA,
    db_table      = cols$TABLE_NAME,
    db_column     = cols$COLUMN_NAME,
    status        = "mapped",
    line          = NA_integer_,
    qvd_path_temp = NA_character_,
    project       = project,
    stringsAsFactors = FALSE
  )
  pat_rows[[length(pat_rows)+1]] <<- d
}

## -- (a) CDP / TWES
ess_views <- unique(db1[up(db1$TABLE_SCHEMA)=="ESS" & up(db1$TABLE_TYPE)=="VIEW", "TABLE_NAME"])
for (tbl in ess_views) {
  app <- if (grepl("^TWES", tbl, ignore.case = TRUE)) "01 ESS QVD Builder - TWES" else "01 ESS QVD Builder - CDP"
  relpath <- paste0("ESS/CDP/Temp/", tbl, ".QVD")
  add_table_rows(app, "ESS", tbl, relpath, project = "ESS")
}

## -- (b) Bushel
bt_schema <- unique(db1$TABLE_SCHEMA[up(db1$TABLE_SCHEMA)=="BUSHTEL"])
if (length(bt_schema) == 1) {
  bt_views <- unique(db1[up(db1$TABLE_SCHEMA)==up(bt_schema) & up(db1$TABLE_TYPE)=="VIEW", "TABLE_NAME"])
  for (tbl in bt_views) {
    relpath <- paste0(bt_schema, "/", tbl, ".QVD")
    add_table_rows("Bushel QVD Builder", bt_schema, tbl, relpath)
  }
}

## -- (c) Inphinity Forms
add_table_rows("01 QVD Generator - Inphinity Forms", "Forms", "1000 Jobs Tracker",
                "Forms/1000 Jobs Tracker.QVD")
rjed_views <- unique(db1[up(db1$TABLE_SCHEMA)=="FORMS_RJED" & up(db1$TABLE_TYPE)=="VIEW", "TABLE_NAME"])
rjed_views <- rjed_views[up(rjed_views) != "SAMPLE VIEW"]
for (tbl in rjed_views) {
  relpath <- paste0("Forms_RJED/", tbl, ".QVD")
  add_table_rows("01 QVD Generator - Inphinity Forms", "Forms_RJED", tbl, relpath)
}

## -- (d)+(e) AZURE / IPP literal tables (unchanged transcription)
add_literal_table <- function(generator_app, schema, table, qvd_relpath, qf, note = NA_character_) {
  if (!qvd_exists(qvd_relpath)) {
    flagged_no_qvd[[length(flagged_no_qvd)+1]] <<- data.frame(
      generator_app = generator_app, schema = schema, table = table,
      candidate_path = qvd_relpath, stringsAsFactors = FALSE)
    return(invisible(NULL))
  }
  dc <- sub("%$", "", qf)
  d <- data.frame(
    generator_app = generator_app, qvd_path_raw = qvd_relpath, qvd_field = qf,
    db_schema = schema, db_table = table, db_column = dc, status = "mapped",
    line = NA_integer_, qvd_path_temp = NA_character_, project = "pattern-literal",
    stringsAsFactors = FALSE
  )
  pat_rows[[length(pat_rows)+1]] <<- d
}

add_literal_table("AZURE QVD Generator","pmc","University","NIAA/University.qvd", c(
  "University Id","Campus Type","University Group","University","Short Name","Campus",
  "Street Address","Postcode","State/Territory","Latitude","Longitude"))
add_literal_table("AZURE QVD Generator","AGIL","AGIL Location","NIAA/AGIL Location.qvd", c(
  "AGIL Location Id%","Preferred Location Name","Alternative Location Names","State/Territory",
  "Latitude","Longitude","Mesh Block Code 2016","Mesh Block Code 2021","Jurisdictional Link","Date Created"))
add_literal_table("AZURE QVD Generator","AGIL","AGIL Name","NIAA/AGIL Name.qvd", c(
  "AGIL Name Id","AGIL Location Id","AGIL Name Code","AGIL Name","AGIL Name Status"),
  note = "leading LOAD line unconfirmed -- alias status assumed none, flagged")
add_literal_table("AZURE QVD Generator","NIAA","NIAA Site","NIAA/NIAA Site.qvd", c(
  "NIAA Site Id%","Site Number","Site Name","Alternative Name","Aurion Location Code",
  "Is Regional Office","Site Contact","Physical Address","Suburb/Town","State/Territory",
  "Postcode","Phone Number","Toll Free Phone Number","Postal Address","Postal Suburb/Town",
  "Postal State/Territory","Postal Postcode","PMC Region","PMC Staff Number","Staff Present",
  "Hosted Staff on Site","Lease Holder","Possible Logistics Point","Communications","Speed",
  "Provider","Network Kit","Freight Method","Team Travel Route","Wet Season Issues",
  "IP Address at site","GPS Location","Site Report Link","Remoteness Area","SA1 Main Code",
  "Latitude","Longitude","FootPrints Discussion","Hosted by","Hosting","Office Capacity","Type Code"))
add_literal_table("AZURE QVD Generator","NIAA","Logistics Point","NIAA/Logistics Point.qvd", c(
  "Logistics Point Id%","Name","Latitude","Longitude"))
add_literal_table("AZURE QVD Generator","NIAA","NIAA Site - Logistics Point",
  "NIAA/NIAA Site - Logistics Point.qvd", c("NIAA Site Id","Logistics Point Id","Order"))
add_literal_table("AZURE QVD Generator","NDIA","NDIS Funding by Individual",
  "NDIA/NDIS Funding by Individual.QVD", c(
  "Disability Funding Id","Person With Disability Id","Primary Disablity","Indigenous Status","Age",
  "Plan Number","Latest Approval Plan Date","Latest Approval Plan Expiry Date","Remoteness Area",
  "Jurisdiction","LGA","Postcode","Suburb","Location Key","Latitude","Longitude",
  "Capital Budget Amount","Core Budget Amount","Capacity Building Budget Amount","Total Budget Amount",
  "Capital Payments","Core Payments","Capacity Building Payments","Total Payments","Snapshot Date",
  "Is Latest Snapshot"), note = "leading LOAD line unconfirmed, flagged")
add_literal_table("AZURE QVD Generator","NDIA","NDIS Funding by Location",
  "NDIA/NDIS Funding by Location.QVD", c(
  "LGA","Postcode","Suburb","Location Key","Provider Count","Capital Budget Amount",
  "Core Budget Amount","Capacity Building Budget Amount","Total Budget Amount","Capital Payments",
  "Core Payments","Capacity Building Payments","Total Payments","Snapshot Date","Is Latest Snapshot"))

add_literal_table("IPP QVD Generator","IPP","UNSPSC Code","IPP/UNSPSC Code.qvd", c(
  "UNSPSC Code Id%","Description","Full Code","High Level Code","Is High Level Code","Name","Status"))
add_literal_table("IPP QVD Generator","IPP","Financial Year","IPP/Financial Year.qvd", c(
  "Financial Year Id%","Financial Year","Start Date","End Date","Financial Year Type"))
add_literal_table("IPP QVD Generator","IPP","Agency","IPP/Agency.qvd", c(
  "Agency Id%","ABN","Agency Name","Agency IPP Manager","Austender Alias","Austender PO method",
  "Classification","IPP Mandated","Portfolio Id","Type Of Body","Valid From Date","Status Code"))
add_literal_table("IPP QVD Generator","IPP","Organisation","IPP/Organisation.qvd", c(
  "Organisation Id%","Address Line 1","Address Line 2","Address Line 3","Suburab/City","Postcode",
  "State/Territory","Country","ABN Exempt","ABR Entity Name","ABR Extract Date","ABR Postcode",
  "ABR Process Error","ABR State","ACN","Approved For Org Based","Area of Expertise","ASIC Number",
  "Business Description","Current QPR Id","Current QPR","Date ABR Last Updated",
  "Entity Name Effective From","Entity Status","Entity Status Effective From",
  "Entity Status Effective To","Entity Type","Entity Type Code","Geographic Reach",
  "GST Effective From","GST Effective To","ICN","Indigenous Business Indicator","Is Certified",
  "Legal Entity Id","Legal Entity Type","Legal Entity Type Name","ABN","Legal Name","Trading Name",
  "Registration Start Date","Registration End Date","Risk Rating","Source","Supply Nation Contact",
  "Supply Nation Email","Target Supply Cchain","Target Total","Target Workforce","UNSPC",
  "Status Code","SA4 Code","SA4 Name","PMC Region Id"))
add_literal_table("IPP QVD Generator","IPP","Portfolio","IPP/Portfolio.qvd", c(
  "Portfolio Id%","Name","IPP Manager Id","IPP Manager","Primary Contact","Status Code"))
add_literal_table("IPP QVD Generator","IPP","Contract","IPP/Contract.qvd", c(
  "Contract Id%","Contract Notice Id%","Purchase Id%","Source Type","Organisation Id%","CN Id",
  "Agency Id%","Agency","Agency Ref Id","Supplier","Indigenous Business Indicator","Category",
  "Contract Type","Contract Value","Description","Contract Start Date","Contract End Date",
  "Financial Year Id%","Financial Year","Industry Code","Name","Remote Contract",
  "Supplier Registered With Supply Nation","UNSPC Code Id%","UNSPC Code","Status","MMR Exempt",
  "PMC Region Id%"))
add_literal_table("IPP QVD Generator","IPP","Contract Notice","IPP/Contract Notice.qvd", c(
  "Contract Notice Id%","CN Id","Action Required","Agency Assessment Comments","Agency Branch",
  "Agency Contract Manager","Agency Contract Manager Email","Agency Division","Agency Id%","Agency",
  "Agency Name","Agency Ref Id","Category","Assessment Method","Contract Type","Contract Value",
  "Contract Value Range","Delivery Postcode","Contract Description","Contract Start Date",
  "Contract End Date","Financial Year","Indigenous Business Indicator","Industry Code",
  "Limited Tender Exemption","MMR Exempt","Name","Supply Chain Performance to date (%)",
  "Workforce Performance to date (%)","Portfolio Id%","Portfolio","Procurement Method",
  "Publish Date","QPRS","QPRS Date","QPRS State","Remote Area Component","Remote Contract",
  "Supplier ABN","Supplier ABN Exempt","Supplier Address","Supplier City","Supplier Contact Id%",
  "Supplier Contact","Supplier Contact Email","Supplier Country","Supplier Name",
  "Supplier Organisation Id%","Supply Chain Target (%)","Target Total","Workforce Target (%)",
  "Time To Submit QPRS","Total Accumulation Value","UNSPSC Code Id%","UNSPSC Code","Variations","Status"))
add_literal_table("IPP QVD Generator","IPP","Purchase","IPP/Purchase.qvd", c(
  "Purchase Id%","Agency Id%","Agency","Agency Awarding The Contract","Austender Id%",
  "Austender Name","Batch Id%","Batch Name","Confirmed As Indigenous Business",
  "Contract Start Date","Contract End Date","Financial Year Id%","Contract Type","Contract Value",
  "Goods Services Category","Head Contract ABN","Head Contract Austender Id","Head Contract End Date",
  "Head Contractor","Head Contract Start Date","Head Contract Value","Head Organisation Id%",
  "Head Organisation","Name","Organisation Id%","Organisation","Original Head Contractor ABN",
  "Original Supplier ABN","Purchase Austender Id%","Purchase Austender","Remote Contract",
  "Supplier ABN","Supplier","Supplier Registered With Supply Nation","UNSPSC Code Id%",
  "UNSPSC Code","Status"))

## -- (f) IAM QVD Builder (NEW, 7th pattern-app input, no lineage CSV).
##    Verified from retargeting/unbuilt/IAM QVD Builder/script.qvs: LIB
##    CONNECT TO 'AzureDbProdNIAADL' (L45), no QUALIFY anywhere, sys.views
##    catalog query (L54-76) drives the table list, STORE INTO
##    lib://AppDataProd/AzureDataLake/$(vSchema)/$(vTable).QVD (L108-109 /
##    164-165) -- confirms the '<schema>/<table>.QVD' store-path rule.
##    Deliberately NOT run through qvd_exists(): that gate is a legacy
##    check for the OLD 6 pattern apps against qvdlist.csv's historical
##    RelPath convention; IAM's own on-disk history (spot-checked: "NIAA
##    Staff.QVD" lives at ESS/CDP/... backup paths, "ORIC Corporations.qvd"
##    at ORIC/... in qvdlist.csv) does not follow the same convention as
##    its OWN script's store path, so applying that gate here would drop
##    real IAM views for a fixture reason unrelated to IAM's script truth.
##    'ORIC Corporations - 20251223' (script's DGOV exclusion, L76) has NO
##    matching TABLE_NAME in DBfixture1 today (only 'ORIC Corporations',
##    no date suffix) -- exclusion verified but currently a no-op; flagged.
add_iam_rows <- function(schema, table) {
  cols <- db1[up(db1$TABLE_SCHEMA) == up(schema) & up(db1$TABLE_NAME) == up(table) &
                up(db1$TABLE_TYPE) == "VIEW", , drop = FALSE]
  if (nrow(cols) == 0) {
    flagged_no_qvd[[length(flagged_no_qvd)+1]] <<- data.frame(
      generator_app = "IAM QVD Builder", schema = schema, table = table,
      candidate_path = paste0(schema, "/", table, ".QVD"), stringsAsFactors = FALSE)
    return(invisible(NULL))
  }
  d <- data.frame(
    generator_app = "IAM QVD Builder",
    qvd_path_raw  = paste0(schema, "/", table, ".QVD"),
    qvd_field     = cols$COLUMN_NAME,
    db_schema     = cols$TABLE_SCHEMA,
    db_table      = cols$TABLE_NAME,
    db_column     = cols$COLUMN_NAME,
    status        = "mapped",
    line          = NA_integer_,
    qvd_path_temp = NA_character_,
    project       = "IAM",
    stringsAsFactors = FALSE
  )
  pat_rows[[length(pat_rows)+1]] <<- d
}
add_iam_rows("IAM", "NIAA Staff")
add_iam_rows("IAM", "GMU Director")
for (tbl in c("Glossary Detail","NTG Community Footprint","NTG Community Overcrowding",
              "NTG Housing","NTG Planned Housing")) add_iam_rows("NIAA", tbl)
dc_views <- unique(db1$TABLE_NAME[up(db1$TABLE_SCHEMA)=="DATACATALOGUE" & up(db1$TABLE_TYPE)=="VIEW"])
for (tbl in dc_views) add_iam_rows("DataCatalogue", tbl)
dgov_views <- unique(db1$TABLE_NAME[up(db1$TABLE_SCHEMA)=="DGOV" & up(db1$TABLE_TYPE)=="VIEW"])
dgov_excl_hit <- up(dgov_views) == up("ORIC Corporations - 20251223")
cat(sprintf("IAM DGOV exclusion 'ORIC Corporations - 20251223': %d catalog match(es) (expect 0 -- no-op, flagged in header comment).\n", sum(dgov_excl_hit)))
dgov_views <- dgov_views[!dgov_excl_hit]
for (tbl in dgov_views) add_iam_rows("DGOV", tbl)

pattern_all <- if (length(pat_rows)) do.call(rbind, pat_rows) else NULL
flagged_df  <- if (length(flagged_no_qvd)) do.call(rbind, flagged_no_qvd) else
                 data.frame(generator_app=character(),schema=character(),table=character(),
                            candidate_path=character())
cat(sprintf("Pattern-app rows instantiated: %d (from %d distinct catalog tables kept)\n",
            if(!is.null(pattern_all)) nrow(pattern_all) else 0L,
            if(!is.null(pattern_all)) length(unique(paste(pattern_all$db_schema,pattern_all$db_table))) else 0L))
cat(sprintf("Catalog candidates with NO matching on-prem qvd (flagged, NOT emitted): %d\n", nrow(flagged_df)))
if (nrow(flagged_df) > 0) print(utils::head(flagged_df, 15))

## ---------------------------------------------------------------------
## 3. ESS finalisation: Temp -> final path (both 02* apps: vQVD_TGT =
##    REPLACE(vQVD_SRC,'/Temp','')). Applies to project=="ESS" rows only.
##    UNCHANGED.
## ---------------------------------------------------------------------
truth <- rbind(existing[, c("generator_app","qvd_path_raw","qvd_field","db_schema",
                             "db_table","db_column","status","line","qvd_path_temp","project")],
               if (!is.null(pattern_all)) pattern_all else existing[0,])
is_ess <- truth$project == "ESS"
truth$qvd_path_temp[is_ess] <- truth$qvd_path_raw[is_ess]
truth$qvd_path_raw[is_ess]  <- gsub("/Temp", "", truth$qvd_path_raw[is_ess], fixed = TRUE)

truth$line <- suppressWarnings(as.integer(truth$line))
truth$line[truth$generator_app == "01 ESS QVD Builder - CDP"  & is.na(truth$line)] <- 328L
truth$line[truth$generator_app == "01 ESS QVD Builder - TWES" & is.na(truth$line)] <- 229L

## ---------------------------------------------------------------------
## qvdlist-lineage coverage-expansion job -- reads the frozen lineage CSV
## (retargeting/lineage_qvdlist.csv), successor to the earlier RDS-backed
## pass (lineage_usable[_reattempt].rds, both now absent from scratch).
## ---------------------------------------------------------------------
truth$truth_source <- "generator-script"
truth$src_db_override <- NA_character_
lin_csv_path <- file.path(root, "retargeting", "lineage_qvdlist.csv")
lin_all <- if (file.exists(lin_csv_path)) {
  d <- read.csv(lin_csv_path, stringsAsFactors = FALSE, colClasses = "character")
  d$db_schema[d$db_schema == ""] <- NA_character_
  d$db_table[d$db_table  == ""] <- NA_character_
  d
} else NULL
if (!is.null(lin_all) && nrow(lin_all) > 0) {
  if (is.null(lin_all$source_db_captured)) lin_all$source_db_captured <- NA_character_
  lin_truth <- data.frame(
    generator_app = "qvdlist",
    qvd_path_raw  = lin_all$qvd_path_raw,
    qvd_field     = lin_all$qvd_field,
    db_schema     = lin_all$db_schema,
    db_table      = lin_all$db_table,
    db_column     = lin_all$db_column,
    status        = lin_all$status,
    line          = NA_integer_,
    qvd_path_temp = NA_character_,
    project       = "qvdlist-lineage",
    truth_source  = "qvdlist-lineage",
    src_db_override = lin_all$source_db_captured,
    stringsAsFactors = FALSE
  )
  truth <- rbind(truth, lin_truth)
  cat(sprintf("qvdlist-lineage usable rows appended: %d (from %d distinct qvds)\n",
              nrow(lin_truth), length(unique(lin_truth$qvd_path_raw))))
} else {
  cat("qvdlist-lineage: 0 usable rows to append.\n")
}
n <- nrow(truth)

## ---------------------------------------------------------------------
## 4. Verdict classification for the EXISTING truth (vectorized call into
##    the shared classify_one() -- identical logic/output to the delivered
##    script's inline loop).
## ---------------------------------------------------------------------
truth$k_schema <- up(truth$db_schema)
truth$k_table  <- up(truth$db_table)
truth$k_col    <- up(truth$db_column)

cl_existing <- lapply(seq_len(n), function(i) classify_one(truth$db_schema[i], truth$db_table[i], truth$db_column[i]))
verdict  <- vapply(cl_existing, function(x) x$verdict,  character(1))
cv_schema<- vapply(cl_existing, function(x) x$cv_schema,character(1))
cv_name  <- vapply(cl_existing, function(x) x$cv_name,  character(1))
cv_field <- vapply(cl_existing, function(x) x$cv_field, character(1))
ev_file  <- vapply(cl_existing, function(x) x$ev_file,  character(1))
ev       <- vapply(cl_existing, function(x) x$ev,       character(1))

## qvdlist-lineage derived rows (expr-column/multi-source): db_schema/
## db_table/db_column carry the raw source expression, not a real column --
## classify_one() cannot resolve these against fixtures/views.csv or
## DBfixture1.csv, so override to derived-in-generator (mirrors the
## new-app pipeline's treatment of the same statuses, L588-589).
lin_derived <- truth$truth_source == "qvdlist-lineage" & truth$status %in% c("expr-column", "multi-source")
verdict[lin_derived]   <- "derived-in-generator"
cv_schema[lin_derived] <- NA_character_
cv_name[lin_derived]   <- NA_character_
cv_field[lin_derived]  <- NA_character_
ev_file[lin_derived]   <- "retargeting/lineage_qvdlist.csv"
ev[lin_derived]        <- sprintf("status %s (qvdlist LineageStatement)", truth$status[lin_derived])

## ---------------------------------------------------------------------
## 5. source_database -- nearest PRECEDING "LIB CONNECT TO '<name>'" per
##    generator_app's own script.qvs. UNCHANGED (IAM resolves via the
##    sole-connection fallback: its script has exactly one LIB CONNECT TO).
## ---------------------------------------------------------------------
conn_map <- list()
for (app in unique(truth$generator_app)) {
  sp <- file.path(root, "retargeting", "unbuilt", app, "script.qvs")
  if (!file.exists(sp)) { conn_map[[app]] <- data.frame(line = integer(0), conn = character(0)); next }
  ln <- readLines(sp, warn = FALSE, encoding = "UTF-8")
  ln_active <- ln
  ln_active[grepl("^\\s*//", ln)] <- ""
  hits <- grep("LIB\\s+CONNECT\\s+TO", ln_active, ignore.case = TRUE)
  conns <- vapply(hits, function(k) {
    m <- regmatches(ln[k], regexpr("'[^']+'", ln[k]))
    if (length(m) == 0) NA_character_ else gsub("'", "", m)
  }, character(1))
  conn_map[[app]] <- data.frame(line = hits, conn = conns, stringsAsFactors = FALSE)
}

src_db <- character(n); db_flag <- character(n)
for (i in seq_len(n)) {
  if (truth$truth_source[i] == "qvdlist-lineage") {
    src_db[i] <- if (is.na(truth$src_db_override[i]) || truth$src_db_override[i] == "") "unknown" else truth$src_db_override[i]
    next
  }
  app <- truth$generator_app[i]; ln <- truth$line[i]
  cm <- conn_map[[app]]
  if (is.null(cm) || nrow(cm) == 0) { src_db[i] <- "unresolved"; db_flag[i] <- "flag"; next }
  if (!is.na(ln)) {
    preceding <- cm[cm$line <= ln, , drop = FALSE]
    if (nrow(preceding) == 0) { src_db[i] <- "unresolved"; db_flag[i] <- "flag"; next }
    src_db[i] <- preceding$conn[which.max(preceding$line)]
  } else {
    uconn <- unique(cm$conn)
    if (length(uconn) == 1) { src_db[i] <- uconn } else { src_db[i] <- "unresolved"; db_flag[i] <- "flag" }
  }
}
non_niaadl_script  <- truth$truth_source == "generator-script"  & !is.na(src_db) & src_db != "unresolved" & src_db != "AzureDbProdNIAADL"
non_niaadl_lineage <- truth$truth_source == "qvdlist-lineage"   & !is.na(src_db) & src_db != "unknown"    & src_db != "AZDB-AUE-PRD-NIAADL01"
non_niaadl <- non_niaadl_script | non_niaadl_lineage
ev[non_niaadl] <- paste0(ev[non_niaadl], " (non-NIAADL source)")

## ---------------------------------------------------------------------
## 6. Existing-pipeline output, intermediate (working) column names --
##    unified with the new-app pipeline below, final rename happens once.
## ---------------------------------------------------------------------
out_existing <- data.frame(
  source_app      = truth$generator_app,
  qvd_path_raw    = truth$qvd_path_raw,
  qvd_path_temp   = truth$qvd_path_temp,
  onprem_field    = truth$qvd_field,
  verdict         = verdict,
  cv_schema       = cv_schema,
  cv_name         = cv_name,
  cv_field        = cv_field,
  source_database = src_db,
  source_schema   = truth$db_schema,
  source_object   = truth$db_table,
  source_column   = truth$db_column,
  ev_file         = ev_file,
  ev              = ev,
  truth_source    = truth$truth_source,
  stringsAsFactors = FALSE
)
n_lineage_rows <- sum(truth$truth_source == "qvdlist-lineage")
cat(sprintf("Row-count reconciliation (existing pipeline): existing(%d) + pattern(%d) + qvdlist-lineage(%d) = %d ; output rows = %d -- %s\n",
            nrow(existing), if(!is.null(pattern_all)) nrow(pattern_all) else 0L, n_lineage_rows,
            nrow(existing) + (if(!is.null(pattern_all)) nrow(pattern_all) else 0L) + n_lineage_rows, nrow(out_existing),
            ifelse(nrow(out_existing) == nrow(existing) + (if(!is.null(pattern_all)) nrow(pattern_all) else 0L) + n_lineage_rows, "OK", "MISMATCH")))

## =======================================================================
## NEW-APP PIPELINE: GPS / IEP01 / IEP01s / FUSION / Geospatial.
## Row rules (project owner's directives):
##  - non-qvd-store: excluded (counted).
##  - non-empty db_connection != AzureDbProdNIAADL (incl. ALL non-sql-source
##    rows): verdict non-niaa-source, cloud_* empty, source_database =
##    connection, evidence names the connection. Checked BEFORE status
##    routing (catches e.g. a parse-error row that also carries a foreign
##    connection).
##  - status qvd-sourced: resolve via the OTHER lineage files' own emitted
##    map rows, keyed on canonical src path + src field.
##  - expr-column/multi-source/parse-error/no-source-block/
##    load-src-not-in-select/label-mismatch/unresolved-variable (NIAADL or
##    empty connection): verdict derived-in-generator.
##  - mapped/mapped-chained (NIAADL or empty connection): three-tier via
##    classify_one(), same as the existing pipeline.
##  - select-star: instantiate all DBfixture1 columns for (schema,table)
##    when table is a real object (CDP/TWES precedent); exclude entirely
##    when table is a function-call / package-status check, e.g.
##    CheckSSISPackage(...) (SSIS IPP precedent).
## =======================================================================

## FUSION / Geospatial extractor artefact: some FROM clauses are 3-part
## qualified names (db."schema"."table"); the extractor split them as
## db_schema = the quoted DB-name token, db_table = 'schema."Table"'.
## Deterministic, mechanical un-split -- verified against DBfixture1
## (FUSION/AIATSIS/ASGS/NIAA/NNTT/DoH schemas all present); flagged, not
## silently trusted.
n_qualified_fix <- 0L
parse_schema_table_vec <- function(schema, table) {
  out_schema <- schema; out_table <- table
  is_quoted <- !is.na(schema) & grepl('^"', schema)
  if (any(is_quoted)) {
    tb <- table[is_quoted]
    dot <- regexpr(".", tb, fixed = TRUE)
    if (any(dot < 0)) stop("STOP: quoted-schema row(s) with no dot in db_table -- cannot parse: ",
                            paste(unique(tb[dot < 0]), collapse = "; "))
    real_schema <- substr(tb, 1, dot - 1)
    real_table  <- substr(tb, dot + 1, nchar(tb))
    real_table  <- gsub('^"|"$', '', real_table)
    out_schema[is_quoted] <- real_schema
    out_table[is_quoted]  <- real_table
    n_qualified_fix <<- n_qualified_fix + sum(is_quoted)
  }
  list(schema = out_schema, table = out_table)
}

derived_statuses <- c("expr-column","multi-source","parse-error","no-source-block",
                       "load-src-not-in-select","label-mismatch","unresolved-variable")

## per-app run counters, for the G5 report
run_counters <- list()

process_new_lineage <- function(app_key, path, ev_file, temp_split = FALSE, qsrc_lookup = NULL,
                                ev_suffix = NULL, truth_source = "generator-script") {
  d <- read_lineage_csv(path)
  n_in <- nrow(d)
  d$line <- suppressWarnings(as.integer(d$line))
  ## status "manual" (PLAN-fleet.md section 6's word for a hand-added
  ## lineage_manual.csv row) is a synonym of "mapped": it routes three-tier
  ## through classify_one() like any other resolved row. No frozen lineage
  ## file uses it, so this is a no-op for every existing input.
  d$status[d$status == "manual"] <- "mapped"

  conn_blank <- is.na(d$db_connection) | d$db_connection == ""
  is_niaadl  <- !conn_blank & up(d$db_connection) == up("AzureDbProdNIAADL")

  fixed <- parse_schema_table_vec(d$db_schema, d$db_table)
  d$db_schema <- fixed$schema
  d$db_table  <- fixed$table

  is_nonqvd <- d$status == "non-qvd-store"
  is_nonniaa <- !is_nonqvd & !conn_blank & !is_niaadl
  is_qsrc    <- !is_nonqvd & !is_nonniaa & d$status == "qvd-sourced"
  is_derived <- !is_nonqvd & !is_nonniaa & !is_qsrc & d$status %in% derived_statuses
  is_3t      <- !is_nonqvd & !is_nonniaa & !is_qsrc & d$status %in% c("mapped","mapped-chained")
  is_ss      <- !is_nonqvd & !is_nonniaa & !is_qsrc & d$status == "select-star"

  parts <- list()

  if (any(is_nonniaa)) {
    sub <- d[is_nonniaa, , drop = FALSE]
    parts[["nonniaa"]] <- data.frame(
      source_app = sub$generator_app, qvd_path_raw = sub$qvd_path_raw, qvd_path_temp = NA_character_,
      onprem_field = sub$qvd_field, verdict = "non-niaa-source",
      cv_schema = NA_character_, cv_name = NA_character_, cv_field = NA_character_,
      source_database = sub$db_connection, source_schema = sub$db_schema, source_object = sub$db_table,
      source_column = sub$db_column, ev_file = ev_file,
      ev = sprintf("non-NIAA source connection: %s", sub$db_connection),
      truth_source = truth_source, stringsAsFactors = FALSE)
  }

  n_qsrc_matched <- 0L; n_qsrc_unmatched <- 0L
  if (any(is_qsrc)) {
    sub <- d[is_qsrc, , drop = FALSE]
    src_canon <- rp_relativize_vec(sub$src_qvd_path)
    src_key <- paste(toupper(src_canon), trimws(sub$src_qvd_field), sep = KSEP)
    verdicts <- character(nrow(sub)); cvs <- character(nrow(sub)); cvn <- character(nrow(sub))
    cvf <- character(nrow(sub)); sdb <- character(nrow(sub)); ssch <- character(nrow(sub))
    sobj <- character(nrow(sub)); scol <- character(nrow(sub)); evf <- character(nrow(sub)); evx <- character(nrow(sub))
    for (i in seq_len(nrow(sub))) {
      hit <- if (!is.null(qsrc_lookup)) qsrc_lookup[[src_key[i]]] else NULL
      if (!is.null(hit)) {
        n_qsrc_matched <- n_qsrc_matched + 1L
        verdicts[i] <- hit$verdict; cvs[i] <- hit$cv_schema; cvn[i] <- hit$cv_name; cvf[i] <- hit$cv_field
        sdb[i] <- hit$source_database; ssch[i] <- hit$source_schema; sobj[i] <- hit$source_object; scol[i] <- hit$source_column
        evf[i] <- hit$ev_file
        evx[i] <- paste0("via ", src_canon[i], " field ", sub$src_qvd_field[i], " | ", hit$ev)
      } else {
        n_qsrc_unmatched <- n_qsrc_unmatched + 1L
        verdicts[i] <- "not-found"; cvs[i] <- NA_character_; cvn[i] <- NA_character_; cvf[i] <- NA_character_
        sdb[i] <- NA_character_; ssch[i] <- NA_character_; sobj[i] <- NA_character_; scol[i] <- NA_character_
        evf[i] <- ev_file; evx[i] <- "source qvd field not in map"
      }
    }
    parts[["qsrc"]] <- data.frame(
      source_app = sub$generator_app, qvd_path_raw = sub$qvd_path_raw, qvd_path_temp = NA_character_,
      onprem_field = sub$qvd_field, verdict = verdicts, cv_schema = cvs, cv_name = cvn, cv_field = cvf,
      source_database = sdb, source_schema = ssch, source_object = sobj, source_column = scol,
      ev_file = evf, ev = evx, truth_source = truth_source, stringsAsFactors = FALSE)
  }

  if (any(is_derived)) {
    sub <- d[is_derived, , drop = FALSE]
    parts[["derived"]] <- data.frame(
      source_app = sub$generator_app, qvd_path_raw = sub$qvd_path_raw, qvd_path_temp = NA_character_,
      onprem_field = sub$qvd_field, verdict = "derived-in-generator",
      cv_schema = NA_character_, cv_name = NA_character_, cv_field = NA_character_,
      source_database = ifelse(is.na(sub$db_connection) | sub$db_connection == "", NA_character_, sub$db_connection),
      source_schema = sub$db_schema, source_object = sub$db_table, source_column = sub$db_column,
      ev_file = ev_file, ev = sprintf("status %s at line %s", sub$status, sub$line),
      truth_source = truth_source, stringsAsFactors = FALSE)
  }

  if (any(is_3t)) {
    sub <- d[is_3t, , drop = FALSE]
    cl <- lapply(seq_len(nrow(sub)), function(i) classify_one(sub$db_schema[i], sub$db_table[i], sub$db_column[i]))
    parts[["threetier"]] <- data.frame(
      source_app = sub$generator_app, qvd_path_raw = sub$qvd_path_raw, qvd_path_temp = NA_character_,
      onprem_field = sub$qvd_field,
      verdict   = vapply(cl, function(x) x$verdict,   character(1)),
      cv_schema = vapply(cl, function(x) x$cv_schema, character(1)),
      cv_name   = vapply(cl, function(x) x$cv_name,   character(1)),
      cv_field  = vapply(cl, function(x) x$cv_field,  character(1)),
      source_database = sub$db_connection, source_schema = sub$db_schema, source_object = sub$db_table,
      source_column = sub$db_column,
      ev_file = vapply(cl, function(x) x$ev_file, character(1)),
      ev      = vapply(cl, function(x) x$ev,      character(1)),
      truth_source = truth_source, stringsAsFactors = FALSE)
  }

  n_ss_excluded <- 0L; n_ss_instantiated <- 0L
  if (any(is_ss)) {
    sub <- d[is_ss, , drop = FALSE]
    for (i in seq_len(nrow(sub))) {
      if (grepl("(", sub$db_table[i], fixed = TRUE)) {
        n_ss_excluded <- n_ss_excluded + 1L   ## SSIS-package-check precedent: excluded entirely
        next
      }
      cols <- db1[up(db1$TABLE_SCHEMA) == up(sub$db_schema[i]) & up(db1$TABLE_NAME) == up(sub$db_table[i]), , drop = FALSE]
      if (nrow(cols) == 0) {
        flagged_no_qvd[[length(flagged_no_qvd)+1]] <<- data.frame(
          generator_app = sub$generator_app[i], schema = sub$db_schema[i], table = sub$db_table[i],
          candidate_path = sub$qvd_path_raw[i], stringsAsFactors = FALSE)
        next
      }
      cl2 <- lapply(seq_len(nrow(cols)), function(k) classify_one(cols$TABLE_SCHEMA[k], cols$TABLE_NAME[k], cols$COLUMN_NAME[k]))
      n_ss_instantiated <- n_ss_instantiated + nrow(cols)
      parts[[paste0("ss", i)]] <- data.frame(
        source_app = sub$generator_app[i], qvd_path_raw = sub$qvd_path_raw[i], qvd_path_temp = NA_character_,
        onprem_field = cols$COLUMN_NAME,
        verdict   = vapply(cl2, function(x) x$verdict,   character(1)),
        cv_schema = vapply(cl2, function(x) x$cv_schema, character(1)),
        cv_name   = vapply(cl2, function(x) x$cv_name,   character(1)),
        cv_field  = vapply(cl2, function(x) x$cv_field,  character(1)),
        source_database = sub$db_connection[i], source_schema = cols$TABLE_SCHEMA, source_object = cols$TABLE_NAME,
        source_column = cols$COLUMN_NAME,
        ev_file = vapply(cl2, function(x) x$ev_file, character(1)),
        ev      = vapply(cl2, function(x) x$ev,      character(1)),
        truth_source = truth_source, stringsAsFactors = FALSE)
    }
  }

  out_df <- if (length(parts)) do.call(rbind, parts) else NULL
  if (!is.null(ev_suffix) && !is.null(out_df) && nrow(out_df) > 0) out_df$ev <- paste0(out_df$ev, ev_suffix)
  if (temp_split && !is.null(out_df) && nrow(out_df) > 0) {
    canon <- rp_relativize_vec(out_df$qvd_path_raw)
    out_df$qvd_path_temp <- canon
    out_df$qvd_path_raw  <- gsub("/Temp", "", canon, fixed = TRUE)
  }

  run_counters[[app_key]] <<- list(
    n_in = n_in, n_nonqvd = sum(is_nonqvd), n_nonniaa = sum(is_nonniaa),
    n_qsrc = sum(is_qsrc), n_qsrc_matched = n_qsrc_matched, n_qsrc_unmatched = n_qsrc_unmatched,
    n_derived = sum(is_derived), n_3t = sum(is_3t),
    n_ss = sum(is_ss), n_ss_excluded = n_ss_excluded, n_ss_instantiated = n_ss_instantiated,
    n_out = if (is.null(out_df)) 0L else nrow(out_df)
  )
  out_df
}

build_qsrc_lookup <- function(out_df) {
  lk <- list()
  if (is.null(out_df) || nrow(out_df) == 0) return(lk)
  keys <- paste(toupper(rp_relativize_vec(out_df$qvd_path_raw)), trimws(out_df$onprem_field), sep = KSEP)
  for (i in seq_len(nrow(out_df))) {
    lk[[keys[i]]] <- list(verdict = out_df$verdict[i], cv_schema = out_df$cv_schema[i], cv_name = out_df$cv_name[i],
                           cv_field = out_df$cv_field[i], source_database = out_df$source_database[i],
                           source_schema = out_df$source_schema[i], source_object = out_df$source_object[i],
                           source_column = out_df$source_column[i], ev_file = out_df$ev_file[i], ev = out_df$ev[i])
  }
  lk
}

## Order matters: FUSION (and everything else) must be built BEFORE GPS,
## since GPS's 6 qvd-sourced rows resolve against FUSION's emitted rows.
out_fusion <- process_new_lineage("FUSION", file.path(root,"retargeting","lineage_fusion.csv"),
                                   "retargeting/lineage_fusion.csv")
out_geo    <- process_new_lineage("Geospatial", file.path(root,"retargeting","lineage_geospatial.csv"),
                                   "retargeting/lineage_geospatial.csv")
## Dead-code stores (Adam, 2026-08-26): tables defined AFTER the Geospatial
## generator's Exit Script -- never executed today, but their qvds sit on
## disk with live consumers (16-23 each). Field structure read from the dead
## tabs; qualified names anchored on a consumer app's own read of
## "NIAA Region 2020.NIAA Region Code" (app-unbuilt/script.qvs:1195).
out_geo_dead <- process_new_lineage("Geospatial-deadcode",
                                    file.path(root,"retargeting","lineage_geospatial_deadcode.csv"),
                                    "retargeting/lineage_geospatial_deadcode.csv",
                                    ev_suffix = " (DEAD CODE after Exit Script: qvd on disk is stale, written by an earlier generator version)")
out_iep01  <- process_new_lineage("IEP01", file.path(root,"retargeting","lineage_iep01.csv"),
                                   "retargeting/lineage_iep01.csv", temp_split = TRUE)
out_iep01s <- process_new_lineage("IEP01s", file.path(root,"retargeting","lineage_iep01s.csv"),
                                   "retargeting/lineage_iep01s.csv", temp_split = TRUE)

qsrc_lookup <- build_qsrc_lookup(do.call(rbind, Filter(Negate(is.null),
                 list(out_existing, out_fusion, out_geo, out_iep01, out_iep01s))))

out_gps <- process_new_lineage("GPS", file.path(root,"retargeting","lineage_gps.csv"),
                                "retargeting/lineage_gps.csv", qsrc_lookup = qsrc_lookup)

out_newapps <- do.call(rbind, Filter(Negate(is.null), list(out_fusion, out_geo, out_geo_dead, out_iep01, out_iep01s, out_gps)))

## ---------------------------------------------------------------------
## lineage_manual.csv (M5, PLAN-fleet.md section 6, trigger B): the ONE
## hand-edited lineage file, written by retargeting/map_add.R and read
## through the same 11-column contract as every frozen one. Its rows carry
## truth_source "manual" so the map itself says which rows a human asserted;
## classification is otherwise unchanged, and an empty (header-only) file
## contributes nothing. A missing file is not an error -- the map must still
## build on a checkout that predates it.
## ---------------------------------------------------------------------
manual_path <- file.path(root, "retargeting", "lineage_manual.csv")
out_manual <- if (file.exists(manual_path)) {
  process_new_lineage("manual", manual_path, "retargeting/lineage_manual.csv",
                      truth_source = "manual")
} else NULL
cat(sprintf("lineage_manual.csv rows: %d\n", if (is.null(out_manual)) 0L else nrow(out_manual)))

cat(sprintf("\nQualified-name (3-part FROM clause) db_schema/db_table un-split: %d row(s).\n", n_qualified_fix))
cat(sprintf("Path-normalization fallback ('AppData/PROD/', no AzureDataLake segment -- Geospatial only): %d row(s).\n", n_geo_fallback))

## ---------------------------------------------------------------------
## Union, final column assembly, directive 1/2/3 corrections, write.
## ---------------------------------------------------------------------
out_all <- rbind(out_existing, out_newapps, out_manual)

## Directive 1: path normalization, ALL rows.
out_all$qvd_path_raw  <- rp_relativize_vec(out_all$qvd_path_raw)
out_all$qvd_path_temp <- rp_relativize_vec(out_all$qvd_path_temp)

## Directive 2: AZURE / IPP QUALIFY-prefix fix. Verified via script grep
## (retargeting/unbuilt/AZURE QVD Generator/script.qvs L46 QUALIFY *;, no
## UNQUALIFY; retargeting/unbuilt/IPP QVD Generator/script.qvs L42, same)
## that every table's bracketed LOAD label equals its table name exactly
## (University/AGIL Location/AGIL Name/NIAA Site/Logistics Point/NIAA Site
## - Logistics Point/NDIS Funding by Individual/NDIS Funding by Location;
## UNSPSC Code/Financial Year/Agency/Organisation/Portfolio/Contract/
## Contract Notice/Purchase) -- both scripts STORE via TableName($(i)),
## storing every loaded table under its own in-memory name, so the
## QUALIFY-prefixed field name is '<source_object>.<old onprem_field>'.
azure_ipp_mask <- out_all$source_app %in% c("AZURE QVD Generator","IPP QVD Generator")
n_azure_ipp_fixed <- sum(azure_ipp_mask)
out_all$onprem_field[azure_ipp_mask] <- paste0(out_all$source_object[azure_ipp_mask], ".", out_all$onprem_field[azure_ipp_mask])

## Directive 3: IPP retirement (all 206 rows; only source_app==IPP QVD Generator).
ipp_mask <- out_all$source_app == "IPP QVD Generator"
n_ipp_retired <- sum(ipp_mask)
out_all$verdict[ipp_mask]  <- "retired"
out_all$ev_file[ipp_mask]  <- "fixtures/qvdlist.csv"
out_all$ev[ipp_mask]       <- "IPP qvds last written 2023-08-06 (qvdlist); zero consumers (qvd_consumers); all IPP apps archived (appcatalog); source views culled in 2024-03 IPP re-platforming"

## Final 15-col + truth_source assembly (identical formula to the delivered script).
out <- data.frame(
  source_app        = out_all$source_app,
  onprem_qvd        = out_all$qvd_path_raw,
  onprem_qvd_temp   = out_all$qvd_path_temp,
  onprem_field      = out_all$onprem_field,
  verdict           = out_all$verdict,
  cloud_view_schema = out_all$cv_schema,
  cloud_view_name   = out_all$cv_name,
  cloud_qvd         = ifelse(is.na(out_all$cv_name), NA_character_, paste0(out_all$cv_schema, "/", out_all$cv_name, ".qvd")),
  cloud_field       = out_all$cv_field,
  source_database   = out_all$source_database,
  source_schema     = out_all$source_schema,
  source_object     = out_all$source_object,
  source_column     = out_all$source_column,
  evidence_file     = out_all$ev_file,
  evidence          = out_all$ev,
  truth_source      = out_all$truth_source,
  stringsAsFactors = FALSE
)
out <- out[order(out$source_app, out$onprem_qvd, out$onprem_field), ]
rownames(out) <- NULL

## Dedupe (Adam, 2026-09-10, BINDING): "keep the resolvable one when duplicate
## keys disagree". The rewriter takes the FIRST row matching (onprem_qvd
## case-insens, onprem_field case-sens), so a disagreeing duplicate was an
## arbitrary rewrite; map_dedupe() keeps one row per key by verdict preference
## (in-cloud > import-view > extend-view > create-view > anything else), ties
## by file order, and records the drop in the kept row's evidence.
n_before <- nrow(out)
out <- map_dedupe(out)
rownames(out) <- NULL
cat(sprintf("\nDedupe: %d row(s) in, %d out (%d duplicate-key row(s) dropped).\n",
            n_before, nrow(out), n_before - nrow(out)))

## out_path came from the --out override (or its default) at the top.
write.csv(out, out_path, row.names = FALSE)
cat(sprintf("\nWrote %d rows to %s\n", nrow(out), out_path))

## ---------------------------------------------------------------------
## Reporting
## ---------------------------------------------------------------------
cat("\n---- Verdict totals (full new map) ----\n"); print(table(out$verdict))
cat("\n---- source_app totals (full new map) ----\n"); print(table(out$source_app))
cat(sprintf("\nDirective 2 (AZURE/IPP prefix fix) applied to %d rows (expect 116+206=322).\n", n_azure_ipp_fixed))
cat(sprintf("Directive 3 (IPP retirement) applied to %d rows (expect 206).\n", n_ipp_retired))

cat("\n---- Per-new-app G5 arithmetic ----\n")
for (k in names(run_counters)) {
  rc <- run_counters[[k]]
  cat(sprintf("[%s] lineage rows in=%d ; non-qvd-store excluded=%d ; select-star pkg-check excluded=%d ; select-star instantiated=%d (from %d select-star row(s)) ; map rows out=%d\n",
              k, rc$n_in, rc$n_nonqvd, rc$n_ss_excluded, rc$n_ss_instantiated, rc$n_ss, rc$n_out))
  cat(sprintf("   -> non-niaa-source=%d ; qvd-sourced=%d (matched=%d, unmatched=%d) ; derived-in-generator=%d ; three-tier=%d\n",
              rc$n_nonniaa, rc$n_qsrc, rc$n_qsrc_matched, rc$n_qsrc_unmatched, rc$n_derived, rc$n_3t))
  reconcile_ok <- (rc$n_in - rc$n_nonqvd - rc$n_ss + rc$n_ss_instantiated) == rc$n_out
  cat(sprintf("   reconciliation: %d - %d - %d(select-star rows) + %d(instantiated) = %d ; actual out = %d -- %s\n",
              rc$n_in, rc$n_nonqvd, rc$n_ss, rc$n_ss_instantiated, rc$n_in - rc$n_nonqvd - rc$n_ss + rc$n_ss_instantiated,
              rc$n_out, ifelse(reconcile_ok, "OK", "MISMATCH")))
}
n_iam_rows <- sum(out$source_app == "IAM QVD Builder")
cat(sprintf("[IAM] no lineage CSV ; instantiated rows=%d\n", n_iam_rows))

cat(sprintf("\nAmbiguity FLAG: %d 'in-cloud'/'import-view' row(s) resolved to alphabetically-first VIEW_SCHEMA/VIEW_NAME (no same-named match).\n", n_ambig))
cat(sprintf("source_database FLAG (existing pipeline): %d row(s) unresolved.\n", sum(src_db == "unresolved")))

cat("\n---- non-NIAADL source_database rows (existing pipeline), by app+connection ----\n")
nn <- out[out$truth_source=="generator-script" & !is.na(out$source_database) & out$source_database != "unresolved" &
            out$source_database != "AzureDbProdNIAADL" & out$verdict != "non-niaa-source", ]
if (nrow(nn) > 0) print(table(nn$source_app, nn$source_database)) else cat("(none)\n")

cat("\n---- 3 sample rows ----\n")
cat("\n[qvd-sourced, resolved chain -- GPS via FUSION]\n")
gsamp <- out[out$source_app=="GPS QVD Generator" & grepl("^via ", out$evidence), ]
if (nrow(gsamp)>0) print(utils::head(gsamp[,c("source_app","onprem_qvd","onprem_field","verdict","evidence")],1))
cat("\n[non-niaa-source]\n")
nsamp <- out[out$verdict=="non-niaa-source", ]
if (nrow(nsamp)>0) print(utils::head(nsamp[,c("source_app","onprem_qvd","onprem_field","source_database","evidence")],1))
cat("\n[IAM instantiation]\n")
isamp <- out[out$source_app=="IAM QVD Builder", ]
if (nrow(isamp)>0) print(utils::head(isamp[,c("source_app","onprem_qvd","onprem_field","verdict","evidence")],1))
