## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = TRUE
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# remotes::install_github("Boehringer-Ingelheim/DTAtoolsR")
# library(DTAtools)

## -----------------------------------------------------------------------------
library(DTAtools)

## ----quickstart---------------------------------------------------------------
library(DTAtools)

# 1. Load the full DTA definition from YAML
#    (metadata + dataset specs + file handlers + rules — all in one file)
dta <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
print(dta)

# 2. Load the data file into the DTA object
#    load_file() uses the file handler already defined in the YAML
csv_path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
dta <- load_file(dta, dataset = "clinical_data", file = csv_path)

# 3. Validate — check() runs schema + rule validation for all datasets
dta <- check(dta)

# 4. Summarise results (one row per table)
results(dta)

## ----quickstart_dataset-------------------------------------------------------
# Extract one dataset entry so it stands alone (same shape as gf_dataset.yaml)
full_yaml    <- yaml::read_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
dataset_yaml <- tempfile(fileext = ".yaml")
yaml::write_yaml(full_yaml$datasets[[1]], dataset_yaml)

# 1. Load the single-dataset definition — no metadata needed
ds <- read_dataset_from_yaml(dataset_yaml)
ds     # a ready-to-use DTADataSetTabular, specs + file handler already set

# 2. Load the data file — same load_file() call as for a full DTA, just
#    without the `dataset =` argument since `ds` already is the dataset
ds <- load_file(ds, file = csv_path, handler_index = 1)

# 3. Validate and summarise — same check()/results() calls as always
ds <- check(ds)
results(ds)

unlink(dataset_yaml)

## ----column_spec_manual-------------------------------------------------------
# A column with a closed value list
visit_col <- DTAColumnSpec(
  id          = "VISIT",
  label       = "Visit Code",
  type        = "SAS Char",
  length      = 12,
  nullable    = FALSE,
  values      = list("V01", "V02", "V03", "EOT"),
  description = "Planned study visit code"
)
print(visit_col)

# A column with a regex pattern
subjid_col <- DTAColumnSpec(
  id          = "SUBJECT_ID",
  label       = "Subject Identifier",
  type        = "SAS Char",
  nullable    = FALSE,
  pattern     = "^SUBJ[0-9]{4}$",
  description = "Unique subject ID in format SUBJ0001"
)
print(subjid_col)

# A numeric column with no further constraints
age_col <- DTAColumnSpec(
  id       = "AGE",
  label    = "Age (years)",
  type     = "SAS Num",
  nullable = FALSE
)
print(age_col)

# A nullable column (empty / NA allowed)
ae_col <- DTAColumnSpec(
  id       = "AE_TERM",
  label    = "Adverse Event Term",
  type     = "SAS Char",
  nullable = TRUE
)
print(ae_col)

## ----spec_collection----------------------------------------------------------
# Build a collection manually
studyid_col <- DTAColumnSpec(
  id = "STUDYID", type = "SAS Char", nullable = FALSE,
  values = list("1234-5678")
)
gender_col <- DTAColumnSpec(
  id = "GENDER", type = "SAS Char", nullable = FALSE,
  values = list("Male", "Female", "Other")
)

specs <- DTAColumnSpecCollection(
  columns = list(
    STUDYID = studyid_col,
    VISIT   = visit_col,
    AGE     = age_col,
    GENDER  = gender_col,
    AE_TERM = ae_col
  )
)
print(specs)

# Access individual column specs
names(specs)
colspec(specs, "VISIT")

## ----import_yaml--------------------------------------------------------------
# The package ships with a real standalone dataset YAML — import_specs_from_yaml()
# reads only its `columns`/`rules` keys, ignoring name/type/files
yaml_file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
gf_specs <- import_specs_from_yaml(yaml_file)
print(gf_specs)

## ----inspect_specs------------------------------------------------------------
# How many columns?
length(gf_specs@columns)

# Look at an individual spec by column ID
colspec(gf_specs, "GFSTAT")

# Are there rules?
gf_specs@rules

## ----export_specs_yaml--------------------------------------------------------
# Write to YAML
yaml_out <- tempfile(fileext = ".yaml")
write_columns_to_yaml(gf_specs, yaml_out)
cat(paste(readLines(yaml_out, n = 20), collapse = "\n"))
unlink(yaml_out)

## ----export_specs_json--------------------------------------------------------
# Write to JSON
json_out <- tempfile(fileext = ".json")
write_columns_to_json(gf_specs, json_out)
cat(paste(readLines(json_out, n = 25), collapse = "\n"))
unlink(json_out)

## ----create_dataset-----------------------------------------------------------
# Load the clinical DTA specs from a full DTA YAML (metadata + datasets)
clinical_yaml <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
dta_obj       <- read_dta_from_yaml(clinical_yaml)

# The DTA contains one dataset called "clinical_data" — but tables are empty
# until you load data into them
datasets(dta_obj)
ds_spec <- datasets(dta_obj, "clinical_data")
ds_spec   # specs defined, no tables yet

## ----load_data_into_ds--------------------------------------------------------
# Get specs from the full DTA spec
clinical_specs <- specs(ds_spec)

# Read the CSV independently
csv_path   <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
raw_data   <- read.csv(csv_path)

# Create the DTADataSetTabular with specs + data, then validate
ds <- DTADataSetTabular(
  name   = "clinical_data",
  specs  = clinical_specs,
  tables = list(clinical_data = raw_data)
)
ds

## ----inline_dataset-----------------------------------------------------------
# Build specs
col_studyid <- DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE,
                             values = list("STUDY001"))
col_visit   <- DTAColumnSpec(id = "VISIT",   type = "SAS Char", nullable = FALSE,
                             values = list("SCREENING", "BASELINE", "WEEK4"))
col_age     <- DTAColumnSpec(id = "AGE",     type = "SAS Num",  nullable = FALSE)

inline_specs <- DTAColumnSpecCollection(
  columns = list(STUDYID = col_studyid, VISIT = col_visit, AGE = col_age)
)

# Build data
inline_data <- data.frame(
  STUDYID = c("STUDY001", "STUDY001", "STUDY001"),
  VISIT   = c("SCREENING", "BASELINE", "WEEK4"),
  AGE     = c(32, 32, 32)
)

# Combine into a DTADataSetTabular
inline_ds <- DTADataSetTabular(
  name   = "inline_example",
  specs  = inline_specs,
  tables = list(my_table = inline_data)
)
inline_ds

## ----validate_ds--------------------------------------------------------------
ds <- check(ds)

## ----inspect_results----------------------------------------------------------
# Summary per table
results(ds)

# Detailed per-error messages (empty for a passing dataset)
msgs <- messages(ds)
head(msgs)

## ----inspect_example----------------------------------------------------------
# Load a dataset that has schema errors
err_file <- system.file("extdata", "clinical_data_error_schema.csv", package = "DTAtools")
dta_err  <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
dta_err  <- load_file(dta_err, "clinical_data", file = err_file)
dta_err  <- check(dta_err, quiet = TRUE)

# Step 1: see which messages exist and note their ids
msgs_err <- messages(datasets(dta_err, "clinical_data"))
head(msgs_err[, c("id", "source", "row", "column", "message")], 8)

## ----inspect_schema-----------------------------------------------------------
# Step 2: inspect a schema error by id — returns the failing row in context
detail <- inspect(datasets(dta_err, "clinical_data"), id = 1)
# Key fields for schema errors:
detail[, c("id", "message", "type", "why",
           "context_.row", "context_SUBJECT_ID", "context_VISIT",
           "schema_keyword", "schema_message")]

## ----inspect_rule-------------------------------------------------------------
# Rule errors: inspect shows which rows violated the rule
rule_file <- system.file("extdata", "clinical_data_error_rules.csv", package = "DTAtools")
dta_rule  <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
dta_rule  <- load_file(dta_rule, "clinical_data", file = rule_file)
dta_rule  <- check(dta_rule, quiet = TRUE)

msgs_rule <- messages(datasets(dta_rule, "clinical_data"))
msgs_rule[, c("id", "rule_id", "message")]

# Drill into the first rule violation
detail_rule <- inspect(datasets(dta_rule, "clinical_data"), id = 1)
detail_rule[, c("id", "rule_id", "type", "why",
                "failing_row_count", "failing_.row",
                "failing_SUBJECT_ID", "failing_VISIT")]

## ----schema_errors------------------------------------------------------------
dta_obj_e  <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
err_file   <- system.file("extdata", "clinical_data_error_schema.csv", package = "DTAtools")
raw_err    <- read.csv(err_file)

ds_err <- DTADataSetTabular(
  name   = "clinical_data",
  specs  = specs(datasets(dta_obj_e, "clinical_data")),
  tables = list(clinical_data = raw_err)
)
ds_err <- check(ds_err)

# See which tables failed
results(ds_err)

# Get the individual error messages (first 10)
msgs_err <- messages(ds_err)
head(msgs_err[, c("target", "source", "row", "column", "message")], 10)

## ----rule_errors--------------------------------------------------------------
dta_obj_r  <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
rule_file  <- system.file("extdata", "clinical_data_error_rules.csv", package = "DTAtools")
raw_rule   <- read.csv(rule_file)

ds_rule <- DTADataSetTabular(
  name   = "clinical_data",
  specs  = specs(datasets(dta_obj_r, "clinical_data")),
  tables = list(clinical_data = raw_rule)
)
ds_rule <- check(ds_rule)

results(ds_rule)
messages(ds_rule)[, c("target", "source", "rule_id", "message")]

## ----rules_in_R---------------------------------------------------------------
# col_range rule
range_rule <- DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65)

# col_unique rule (composite key)
unique_rule <- DTARuleColUnique(id = "subject_visit_unique", columns = c("SUBJECT_ID", "VISIT"))

# col_condition rule: IF CONSENT == "YES" THEN CONSENT_DATE must not be empty
condition_rule <- DTARuleColCondition(
  id        = "consent_requires_date",
  condition = list(CONSENT = list(equals = "YES")),
  then      = list(CONSENT_DATE = list(empty = FALSE))
)

# Collect rules in a plain list
rule_collection <- list(range_rule, unique_rule, condition_rule)

## ----specs_with_rules---------------------------------------------------------
# Attach rules to a DTAColumnSpecCollection
studyid  <- DTAColumnSpec(id = "STUDYID",      type = "SAS Char", nullable = FALSE,
                          values = list("STUDY001"))
visit2   <- DTAColumnSpec(id = "VISIT",         type = "SAS Char", nullable = FALSE,
                          values = list("SCREENING", "BASELINE", "WEEK4"))
age2     <- DTAColumnSpec(id = "AGE",           type = "SAS Num",  nullable = FALSE)
consent  <- DTAColumnSpec(id = "CONSENT",       type = "SAS Char", nullable = TRUE,
                          values = list("YES", "NO"))
con_date <- DTAColumnSpec(id = "CONSENT_DATE",  type = "SAS Char", nullable = TRUE)
subjid2  <- DTAColumnSpec(id = "SUBJECT_ID",    type = "SAS Char", nullable = FALSE)

specs_with_rules <- DTAColumnSpecCollection(
  columns = list(
    STUDYID = studyid, VISIT = visit2, AGE = age2,
    CONSENT = consent, CONSENT_DATE = con_date, SUBJECT_ID = subjid2
  ),
  rules = rule_collection
)
print(specs_with_rules)

# Validate data that violates the rules
bad_data <- data.frame(
  STUDYID      = c("STUDY001", "STUDY001"),
  VISIT        = c("SCREENING", "BASELINE"),
  AGE          = c(16, 32),         # 16 violates age_range (min 18)
  CONSENT      = c("YES", "NO"),
  CONSENT_DATE = c(NA, NA),          # CONSENT=YES but no date: violates consent_requires_date
  SUBJECT_ID   = c("P01", "P01")    # duplicate SUBJECT_ID: violates subject_visit_unique when VISIT differs too
)

ds_bad <- DTADataSetTabular(
  name   = "demo_rules",
  specs  = specs_with_rules,
  tables = list(demo = bad_data)
)
ds_bad <- check(ds_bad)

messages(ds_bad)[, c("source", "rule_id", "row", "column", "message")]

## ----file_workflow------------------------------------------------------------
# Create a CSV file handler — describes the expected file format
file_handler <- DTAFileCSV(filename = "clinical_data.csv")

# Show what the handler captures
print(file_handler)

# Read the actual file through the handler
csv_path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
tbl      <- read_file(file_handler, csv_path)
cat("Loaded table class:", class(tbl)[1], "\n")
cat("Dimensions:", nrow(tbl), "x", ncol(tbl), "\n")

# The Arrow Table can be passed directly to DTADataSetTabular
simple_specs <- DTAColumnSpecCollection(columns = list(
  STUDYID = DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE,
                          values = list("1234-5678")),
  VISIT   = DTAColumnSpec(id = "VISIT",   type = "SAS Char", nullable = FALSE,
                          values = list("V01","V02","V03","EOT")),
  AGE     = DTAColumnSpec(id = "AGE",     type = "SAS Num",  nullable = FALSE)
))

# Use the Arrow Table directly in the constructor
ds_from_file <- DTADataSetTabular(
  name   = "clinical_from_file",
  specs  = simple_specs,
  files  = list(file_handler),    # document the expected file
  tables = list(clinical_data = tbl)  # provide the loaded data
)
labels(ds_from_file)

## ----file_handlers_from_dta---------------------------------------------------
dta_spec  <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
ds_s      <- datasets(dta_spec, "clinical_data")

# Inspect the file handler defined in the YAML
fh <- files(ds_s, 1)
print(fh)

# Read the file using the handler
csv_path  <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
loaded    <- read_file(fh, csv_path)
cat("Rows loaded:", nrow(loaded), "\n")

## ----dataset_file-------------------------------------------------------------
# Paths to existing files (pass)
delivered_csv <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
delivered_tsv <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

ds_file <- DTADataSetFile(
  name  = "delivery_check",
  paths = c(delivered_csv, delivered_tsv)
)
ds_file <- check(ds_file)
results(ds_file)     # both files pass

## ----dataset_file_missing-----------------------------------------------------
# Introduce a missing file to trigger a failure
ds_missing <- DTADataSetFile(
  name  = "delivery_check_fail",
  paths = c(delivered_csv, "/nonexistent/path/report.pdf")
)
ds_missing <- check(ds_missing)

results(ds_missing)          # one row fails

# The message explains exactly why
messages(ds_missing)[, c("target", "rule_id", "message")]

## ----dta_mixed_types, eval=FALSE----------------------------------------------
# # Tabular dataset (column spec validation)
# ds_tab <- DTADataSetTabular(
#   name   = "clinical_data",
#   specs  = import_specs_from_yaml(
#     system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#   ),
#   tables = list(clinical_data = read.csv(delivered_csv))
# )
# 
# # File dataset (presence check)
# ds_fi <- DTADataSetFile(
#   name  = "delivery_manifest",
#   paths = c(delivered_csv, delivered_tsv)
# )
# 
# dta_mixed <- DTA(
#   datasets = list(ds_tab, ds_fi),
#   metadata = DTAMetaData(title = "Mixed DTA", version = "1.0")
# )
# 
# dta_mixed <- check(dta_mixed)
# results(dta_mixed)   # one row per dataset

## ----metadata-----------------------------------------------------------------
# Minimal metadata
meta_simple <- DTAMetaData(title = "My Clinical Data Transmission", version = "1.0")
print(meta_simple)

# Full metadata with contacts and transmission schedule
meta_full <- DTAMetaData(
  title    = "Clinical Genomics Data Transmission",
  version  = "2.0",
  date     = as.Date("2026-01-15"),
  header   = "Boehringer Ingelheim",
  receiver = list(
    affiliation = list(name = "External CRO", country = "Germany"),
    contacts = list(
      list(name = "Alice Smith",  role = "Lead Data Manager",
           email = "alice@cro.com", signature = TRUE, reviewer = TRUE),
      list(name = "Bob Johnson",  role = "Bioinformatician",
           email = "bob@cro.com",   signature = TRUE)
    )
  ),
  supplier = list(
    affiliation = list(name = "Pharma Company", country = "USA"),
    contacts = list(
      list(name = "Emily Turner", role = "Senior Data Manager",
           email = "emily@pharma.com", signature = TRUE)
    )
  ),
  transmission = list(
    type                = "Secure SFTP",
    frequency           = "One-time",
    notification        = "Email",
    date_first_transfer = as.Date("2026-02-01"),
    date_last_transfer  = as.Date("2026-03-31")
  )
)
print(meta_full)

## ----dta_object---------------------------------------------------------------
# Build from scratch
ds1 <- create_example_DTADataSetTabular(2)  # "demographics" with data
ds2 <- create_example_DTADataSetTabular(3)  # "vitals" with data

dta_obj <- DTA(
  datasets = list(ds1, ds2),
  metadata = meta_simple
)
print(dta_obj)

# Access datasets by name or index
datasets(dta_obj, "demographics")
dta_obj[["vitals"]]
dta_obj[c(1, 2)]        # returns a list of both datasets

## ----read_dta_yaml------------------------------------------------------------
dta_file      <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
dta_from_file <- read_dta_from_yaml(dta_file)
print(dta_from_file)

# Inspect metadata
metadata(dta_from_file)

## ----validate_full_dta--------------------------------------------------------
# Get the specs from the DTA
ds_spec_full <- datasets(dta_from_file, "clinical_data")
clinical_sps <- specs(ds_spec_full)

# Load the CSV data
csv_path_full <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
raw_full      <- read.csv(csv_path_full)

# Create DTADataSetTabular with specs + data
ds_full <- DTADataSetTabular(
  name   = "clinical_data",
  specs  = clinical_sps,
  tables = list(clinical_data = raw_full)
)

# Validate
ds_full <- check(ds_full)
results(ds_full)

## ----write_table--------------------------------------------------------------
# Validate the clinical dataset
csv_path_w    <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
dta_obj_w     <- read_dta_from_yaml(
  system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
)
ds_w <- DTADataSetTabular(
  name   = "clinical_data",
  specs  = specs(datasets(dta_obj_w, "clinical_data")),
  tables = list(clinical_data = read.csv(csv_path_w))
)
ds_w <- check(ds_w, quiet = TRUE)

# Write to TSV (tab-separated, sorted by SUBJECT_ID)
out_tsv <- tempfile(fileext = ".tsv")
write_table_to_file(
  DTADataSetTabular = ds_w,
  table             = "clinical_data",
  filename          = out_tsv,
  sep               = "\t",
  arrange_by        = "SUBJECT_ID",
  get_md5sum        = FALSE
)
# Confirm output
file.exists(out_tsv)
nrow(read.table(out_tsv, header = TRUE, sep = "\t"))
unlink(out_tsv)

## ----export_word, eval=FALSE--------------------------------------------------
# # gf_specs was loaded earlier with import_specs_from_yaml()
# # Export with default columns: Variable Name, Label, Type, Length, Format, Nullable, Description
# docx_out <- tempfile(fileext = ".docx")
# export_specs_table(gf_specs, file = docx_out, overwrite = TRUE)

## ----export_word_layouts, eval=FALSE------------------------------------------
# # Layout 1: without Length column
# export_specs_table(
#   gf_specs,
#   file      = "spec_table_short.docx",
#   overwrite = TRUE,
#   colnames  = c("Variable Name", "Variable Label", "Type", "Format", "Nullable", "Description")
# )
# 
# # Layout 2: with Length column (default)
# export_specs_table(
#   gf_specs,
#   file      = "spec_table_full.docx",
#   overwrite = TRUE,
#   colnames  = c("Variable Name", "Variable Label", "Type", "Length", "Format", "Nullable", "Description")
# )

## ----export_values, eval=FALSE------------------------------------------------
# # Export all allowed VISIT values for the clinical dataset
# docx_values     <- tempfile(fileext = ".docx")
# dta_obj_export  <- read_dta_from_yaml(
#   system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
# )
# clinical_specs_export <- specs(datasets(dta_obj_export, "clinical_data"))
# 
# export_column_value_table(
#   DTAColumnSpecCollection = clinical_specs_export,
#   file = docx_values,
#   id   = "VISIT"
# )

