# The shared data builders behind the Word and Markdown document exports
# (R/documentData.R). Expected values are written by hand from the bundled
# clinical fixture (inst/extdata/clinical_dta.yaml) or from the objects built in
# the test itself, never copied from the builders' output.

.dd_dta <- function() {
  read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
}

.dd_meta <- function(...) DTAMetaData(...)

mid_dot <- "\u00b7"

# ---- pairs builders ----------------------------------------------------------

test_that("pairs builders return key/value text frames", {
  pairs <- .cover_facts(.dd_dta()@metadata)
  expect_s3_class(pairs, "data.frame")
  expect_identical(names(pairs), c("key", "value"))
  expect_type(pairs$key, "character")
  expect_type(pairs$value, "character")

  none <- .transmission_pairs(list())
  expect_identical(names(none), c("key", "value"))
  expect_identical(nrow(none), 0L)
  expect_type(none$key, "character")
})

test_that(".cover_facts lists the facts in cover order and drops empty ones", {
  facts <- .cover_facts(.dd_dta()@metadata)
  # The fixture has no study and no template.
  expect_identical(facts$key, c("Version", "Date", "Supplier", "Receiver"))
  expect_identical(facts$value, c("0.1", "2024-06-19", "Test Company 2", "Test Company"))

  m <- .dd_dta()@metadata
  m@study <- list(id = "ST-9", protocol_id = "P-77")
  m@template <- list(id = "biomarker_gf", label = "Biomarker GF DTS", version = "1.3", date = "2026-11-05")
  full <- .cover_facts(m)
  expect_identical(
    full$key,
    c("Version", "Date", "Study ID", "Protocol ID", "Supplier", "Receiver", "Template")
  )
  expect_identical(full$value[3:4], c("ST-9", "P-77"))
  expect_identical(
    full$value[7],
    paste0("Biomarker GF DTS (biomarker_gf) ", mid_dot, " version 1.3 ", mid_dot, " 2026-11-05")
  )
})

test_that(".cover_facts copes with missing values and takes the date from the history", {
  bare <- .cover_facts(.dd_meta())
  expect_identical(nrow(bare), 0L)

  m <- .dd_meta(
    version = "2.0",
    version_history = list(
      list(version = "1.0", date = "2024-01-01", changes = "Initial"),
      list(version = "2.0", date = "2025-03-04", changes = "Second")
    )
  )
  facts <- .cover_facts(m)
  expect_identical(facts$key, c("Version", "Date"))
  expect_identical(facts$value, c("2.0", "2025-03-04"))
})

test_that(".document_info_pairs gives title, header, version, date and template origin", {
  info <- .document_info_pairs(.dd_dta()@metadata)
  expect_identical(info$key, c("Title", "Document header", "Version", "Date"))
  expect_identical(info$value, c("Clinical Data Specification", "Company", "0.1", "2024-06-19"))

  m <- .dd_dta()@metadata
  m@template <- list(id = "biomarker_gf", label = "Biomarker GF DTS", version = "1.3", date = "2026-11-05")
  with_tpl <- .document_info_pairs(m)
  expect_identical(
    with_tpl$key,
    c("Title", "Document header", "Version", "Date", "Template", "Template version", "Template date")
  )
  expect_identical(
    with_tpl$value[5:7],
    c("Biomarker GF DTS (biomarker_gf)", "1.3", "2026-11-05")
  )

  # A label equal to the id is not repeated; missing parts are dropped.
  m@template <- list(id = "same", label = "same", version = "9")
  same <- .document_info_pairs(m)
  expect_identical(same$key[5:6], c("Template", "Template version"))
  expect_identical(same$value[5:6], c("same", "9"))
  expect_identical(nrow(same), 6L)
})

test_that(".document_info_pairs falls back to the document name without a title", {
  info <- .document_info_pairs(.dd_meta())
  expect_identical(info$key, "Title")
  expect_identical(info$value, "Data Transfer Agreement")
})

test_that(".dataset_facts shows id, type and template origin", {
  ds <- .dd_dta()@datasets[[1]]
  facts <- .dataset_facts(ds)
  expect_identical(facts$key, c("ID", "Type"))
  expect_identical(facts$value, c("clinical_data", "Tabular"))

  gf <- read_dataset_from_yaml(system.file("extdata", "gf_dataset.yaml", package = "DTAtools"))
  gf_facts <- .dataset_facts(gf)
  expect_identical(
    gf_facts$key,
    c("ID", "Type", "Template source", "Template version", "Template date")
  )
  expect_identical(
    gf_facts$value,
    c("gf_data_specs_pattern", "Tabular", "GF domain smrnaseq", "3.0", "2024-12-17")
  )
})

test_that(".transmission_pairs labels the keys and keeps every value", {
  tr <- .transmission_pairs(.dd_dta()@metadata@transmission)
  expect_identical(
    tr$key,
    c(
      "Transfer method", "Frequency", "Notification", "Test upload",
      "Blinded transfer", "First transfer", "Last transfer"
    )
  )
  expect_identical(
    tr$value,
    c("secure S3 bucket", "one-time", "email", "No", "No", "2025-11-25", "2026-01-15")
  )
  expect_null(names(tr$value))
})

test_that(".transmission_pairs keeps nested extension values complete", {
  tr <- .transmission_pairs(list(
    type = "sFTP",
    vendor_extension = list(portal = "sftp.example.org", retention = list(days = "30"))
  ))
  expect_identical(tr$key, c("Transfer method", "Vendor Extension"))
  expect_identical(
    tr$value[2],
    "Portal: sftp.example.org\nRetention: \n    Days: 30"
  )
})

test_that(".party_facts gives the affiliation in a fixed order, then extension fields", {
  d <- .dd_dta()
  supplier <- .party_facts(d@metadata@supplier)
  expect_identical(supplier$key, c("Organization", "Country", "Address"))
  expect_identical(supplier$value, c("Test Company 2", "Test Country", "Test Address 2"))

  receiver <- .party_facts(d@metadata@receiver)
  expect_identical(receiver$key, "Organization")
  expect_identical(receiver$value, "Test Company")

  ext <- .party_facts(list(affiliation = list(name = "Org", custom_field = "custom value")))
  expect_identical(ext$key, c("Organization", "Custom Field"))
  expect_identical(ext$value, c("Org", "custom value"))

  expect_identical(nrow(.party_facts(list())), 0L)
  expect_identical(nrow(.party_facts(NULL)), 0L)
})

# ---- contacts ----------------------------------------------------------------

test_that(".party_contacts_df lists signatories and reviewers first, backups last", {
  contacts <- .dd_dta()@metadata@receiver$contacts
  df <- .party_contacts_df(contacts)
  expect_identical(names(df), c("Name", "Role", "Department", "Contact", "Responsibilities"))
  expect_identical(df$Name, c("Alice Smith", "Bob Johnson", "Carol Lee", "David Kim"))
  expect_identical(
    df$Role,
    c("Lead Data Manager", "Clinical Bioinformatician", "Backup Data Manager", "Backup Bioinformatician")
  )
  expect_identical(
    df$Department,
    c("Data Management", "Bioinformatics", "Data Management", "Bioinformatics")
  )
  expect_identical(
    df$Responsibilities,
    c("Signatory\nReviewer", "Signatory\nReviewer", "Backup", "Backup")
  )
  # email, phone and address, one per line; an absent phone leaves no blank line
  expect_identical(
    df$Contact[1:2],
    c(
      "alice.smith@testcompany.com\n555-123-4567\n123 Main St, Cityville",
      "bob.johnson@testcompany.com\n123 Main St, Cityville"
    )
  )
  # a field the class does not know about is appended with its label
  expect_identical(
    df$Contact[4],
    "david.kim@testcompany.com\n456 Elm St, Townsville\nCompany: Test Company"
  )
})

test_that(".party_contacts_df moves backup-only contacts last, but not signatory backups", {
  contacts <- list(
    list(name = "B1", backup = TRUE),
    list(name = "S1", signature = TRUE, backup = TRUE),
    list(name = "N1"),
    list(name = "B2", backup = TRUE, reviewer = TRUE)
  )
  df <- .party_contacts_df(contacts)
  expect_identical(df$Name, c("S1", "N1", "B1", "B2"))
  expect_identical(df$Responsibilities, c("Signatory\nBackup", "", "Backup", "Reviewer\nBackup"))
})

test_that(".party_contacts_df handles unnamed, plain-text and empty contacts", {
  df <- .party_contacts_df(list(list(role = "Observer"), "Plain Person"))
  expect_identical(df$Name, c("(Unnamed)", "Plain Person"))
  expect_identical(df$Role, c("Observer", ""))
  expect_identical(df$Contact, c("", ""))

  for (empty in list(NULL, list())) {
    none <- .party_contacts_df(empty)
    expect_identical(nrow(none), 0L)
    expect_identical(names(none), c("Name", "Role", "Department", "Contact", "Responsibilities"))
  }
})

test_that(".party_contacts_df joins list-valued fields and keeps text unescaped", {
  df <- .party_contacts_df(list(
    list(name = "A | B *x*", email = c("a@x.org", "b@x.org"), notes = "see <b>1</b>")
  ))
  expect_identical(df$Name, "A | B *x*")
  expect_identical(df$Contact, "a@x.org, b@x.org\nNotes: see <b>1</b>")
})

# ---- files -------------------------------------------------------------------

.dd_files <- function() {
  list(
    DTAFileCSV(
      "data_.*\\.csv$",
      pattern = TRUE, pattern_description = "Monthly extract",
      min_number_of_files = 1, max_number_of_files = 3
    ),
    DTAFileTSV(c("b1.tsv", "b2.tsv"), info = "Sent by lab"),
    DTAFileCSV("c_.*\\.csv$", pattern = TRUE, number_of_files = 2, info = list(data = "x"))
  )
}

test_that(".file_specs_df has one row per file with the five Word columns", {
  df <- .file_specs_df(.dd_files())
  expect_identical(
    names(df),
    c("File Name / Pattern", "Match Type", "Format", "Expected Count", "Description")
  )
  expect_identical(df$`File Name / Pattern`, c("data_.*\\.csv$", "b1.tsv, b2.tsv", "c_.*\\.csv$"))
  expect_identical(df$`Match Type`, c("Regex pattern", "Exact name", "Regex pattern"))
  expect_identical(df$Format, c("CSV", "TSV", "CSV"))
  expect_identical(df$`Expected Count`, c("1 to 3", "1", "2"))
  expect_identical(df$Description, c("Monthly extract", "Sent by lab", "Data: x"))
})

test_that(".file_specs_df appends info to a pattern description and labels other handlers", {
  files <- list(
    DTAFileCSV("p_.*", pattern = TRUE, pattern_description = "Per site", info = "Weekly"),
    DTAFileAny("raw.bin")
  )
  df <- .file_specs_df(files)
  expect_identical(df$Description, c("Per site (Weekly)", ""))
  expect_identical(df$Format, c("CSV", "File"))
})

test_that(".file_specs_df reads the bundled fixture", {
  df <- .file_specs_df(.dd_dta()@datasets[[1]]@files)
  expect_identical(nrow(df), 1L)
  expect_identical(df$`File Name / Pattern`, "clinical_data.*.csv$")
  expect_identical(df$`Match Type`, "Regex pattern")
  expect_identical(df$`Expected Count`, "1")
})

test_that(".file_specs_df and .file_format_df return empty frames without files", {
  for (empty in list(NULL, list())) {
    specs <- .file_specs_df(empty)
    expect_identical(nrow(specs), 0L)
    expect_identical(ncol(specs), 5L)
    format <- .file_format_df(empty)
    expect_identical(nrow(format), 0L)
    expect_identical(ncol(format), 7L)
  }
})

test_that(".file_format_df spells out how each tabular file is parsed", {
  df <- .file_format_df(.dd_dta()@datasets[[1]]@files)
  expect_identical(
    names(df),
    c(
      "File", "Delimiter", "Header row", "Quote", "Missing values", "Encoding",
      "Line breaks in values"
    )
  )
  expect_identical(df$File, "clinical_data.*.csv$")
  expect_identical(df$Delimiter, "Comma ( , )")
  expect_identical(df$`Header row`, "Yes")
  expect_identical(df$Quote, "Double quote ( \" )")
  expect_identical(df$`Missing values`, "(empty string)")
  expect_identical(df$Encoding, "UTF-8")
  expect_identical(df$`Line breaks in values`, "No")
})

test_that(".file_format_df covers delimiters, quotes, codes and skips non-tabular files", {
  files <- list(
    DTAFileTSV("a.tsv"),
    DTAFileAny("raw.bin"),
    DTAFileCSV(
      "c.csv",
      has_header = FALSE, quote = "'", missing_values = c("", "NA"),
      newlines_in_values = TRUE, encoding = "latin1"
    ),
    DTAFileDelim("d.txt", sep = "|", quote = ";")
  )
  df <- .file_format_df(files)
  expect_identical(df$File, c("a.tsv", "c.csv", "d.txt"))
  expect_identical(df$Delimiter, c("Tab", "Comma ( , )", "Pipe ( | )"))
  expect_identical(df$`Header row`, c("Yes", "No", "Yes"))
  expect_identical(df$Quote, c("Double quote ( \" )", "Single quote ( ' )", "Semicolon ( ; )"))
  expect_identical(df$`Missing values`, c("(empty string)", "(empty string), NA", "(empty string)"))
  expect_identical(df$Encoding, c("UTF-8", "latin1", "UTF-8"))
  expect_identical(df$`Line breaks in values`, c("No", "Yes", "No"))

  # only non-tabular files: nothing to show
  expect_identical(nrow(.file_format_df(list(DTAFileAny("raw.bin")))), 0L)
})

test_that(".file_count_sentence states the total count and how to read the names", {
  one <- list(DTAFileCSV("a.csv"))
  expect_identical(
    .file_count_sentence(one),
    paste0(
      "1 file is expected for this dataset in total. Each file name is given ",
      "either as an exact name or as a regular-expression pattern that a ",
      "delivered file must match; the expected count states how many files may match it."
    )
  )
  expect_match(.file_count_sentence(list(DTAFileCSV("a.csv"), DTAFileCSV("b.csv"))), "^2 files are expected for this dataset in total\\.")

  ranged <- list(
    DTAFileCSV("a_.*", pattern = TRUE, min_number_of_files = 1, max_number_of_files = 3),
    DTAFileCSV("b.csv")
  )
  expect_match(.file_count_sentence(ranged), "^2 to 4 files are expected for this dataset in total\\.")

  expect_identical(.file_count_sentence(list()), "")
  expect_identical(.file_count_sentence(NULL), "")
})

# ---- column specifications ---------------------------------------------------

test_that(".column_specs_df lists the fixture's columns in order", {
  df <- .column_specs_df(.dd_dta()@datasets[[1]]@specs)
  expect_identical(
    names(df),
    c("Variable Name", "Label", "Type", "Nullable", "Allowed Values / Pattern", "Description")
  )
  expect_identical(nrow(df), 14L)
  expect_identical(
    df$`Variable Name`[c(1, 2, 3, 5, 14)],
    c("STUDYID", "VISIT", "AGE", "INCLUDE", "SUBJECT_ID")
  )
  expect_identical(df$Label[1:3], c("Study Identifier", "Visit", "Age"))
  expect_identical(df$Type[1:5], c("SAS Char (10)", "SAS Char (12)", "SAS Num", "SAS Char", "SAS Char (1)"))
  expect_identical(df$Nullable[c(1, 5, 10)], c("No", "Yes", "Yes"))
  expect_identical(
    df$`Allowed Values / Pattern`[1:5],
    c("1234-5678", "V01, V02, V03, EOT", "", "Male, Female, Other", "Y, N")
  )
  expect_identical(df$Description[1:3], c("Unique study ID", "Visit code", "Age in years"))
})

test_that(".column_specs_df never shortens the list of allowed values", {
  values <- sprintf("V%02d", 1:12)
  specs <- DTAColumnSpecCollection(columns = list(
    DTAColumnSpec(id = "VISIT", type = "SAS Char", length = 3, values = values)
  ))
  df <- .column_specs_df(specs)
  expect_identical(df$`Allowed Values / Pattern`, paste(values, collapse = ", "))
  expect_false(grepl("...", df$`Allowed Values / Pattern`, fixed = TRUE))
})

test_that(".column_specs_df shows type details, patterns and examples", {
  specs <- DTAColumnSpecCollection(columns = list(
    DTAColumnSpec(id = "A", type = "SAS Char", length = 8, format = "SAS $8.", colclass = "visit_related"),
    DTAColumnSpec(id = "B", pattern = "^[A-Z]{2}$", examples = c("AB", "CD"), description = "Two letters"),
    DTAColumnSpec(id = "C", examples = c("x", "y")),
    DTAColumnSpec(id = "D", label = "Label D", nullable = TRUE)
  ))
  df <- .column_specs_df(specs)
  expect_identical(df$Type[1], "SAS Char (8)\nFormat: $8.\nR class: visit_related")
  expect_identical(df$Type[2:4], rep("unspecified", 3))
  expect_identical(df$`Allowed Values / Pattern`[2], "Pattern: ^[A-Z]{2}$")
  expect_identical(df$Description[2:3], c("Two letters\nExamples: AB, CD", "Examples: x, y"))
  expect_identical(df$Label, c("", "", "", "Label D"))
  expect_identical(df$Nullable[4], "Yes")
})

test_that(".column_specs_df joins values given as a list like a plain vector", {
  specs <- DTAColumnSpecCollection(columns = list(
    DTAColumnSpec(id = "A", values = list("1234", "5678"))
  ))
  expect_identical(.column_specs_df(specs)$`Allowed Values / Pattern`, "1234, 5678")
})

test_that(".column_specs_df shows an unset nullable as 'Not specified'", {
  # KNOWN DEFECT, pinned rather than endorsed: the validation engine treats an
  # unset `nullable` as "No" (columnSpecChecks.R), so "Not specified" misleads.
  # The Markdown redesign changes it to "No (default)"; switch this assertion
  # to that text when the fix lands.
  specs <- DTAColumnSpecCollection(columns = list(DTAColumnSpec(id = "A")))
  expect_identical(.column_specs_df(specs)$Nullable, "Not specified")
})

test_that(".column_specs_df returns an empty frame without columns", {
  for (empty in list(NULL, DTAColumnSpecCollection(columns = list()))) {
    df <- .column_specs_df(empty)
    expect_identical(nrow(df), 0L)
    expect_identical(ncol(df), 6L)
  }
})

# ---- version history ---------------------------------------------------------

test_that(".version_history_df gives one row per entry, with ISO dates", {
  history <- list(
    list(version = "1.0", date = as.Date("2024-01-31"), changes = "Initial"),
    list(version = "2.0", date = "2025-03-04", changes = c("Added files", "Added rules")),
    list(changes = NULL)
  )
  df <- .version_history_df(history)
  expect_identical(names(df), c("Version", "Date", "Changes"))
  expect_identical(df$Version, c("1.0", "2.0", ""))
  expect_identical(df$Date, c("2024-01-31", "2025-03-04", ""))
  expect_identical(df$Changes, c("Initial", "Added files; Added rules", ""))
})

test_that(".version_history_df returns an empty frame without history", {
  for (empty in list(NULL, list())) {
    df <- .version_history_df(empty)
    expect_identical(nrow(df), 0L)
    expect_identical(names(df), c("Version", "Date", "Changes"))
  }
})

# ---- the Word tables carry exactly these data --------------------------------

test_that("the Word tables are built from the data builders", {
  d <- .dd_dta()
  ds <- d@datasets[[1]]

  contacts <- d@metadata@receiver$contacts
  ft <- .build_party_contacts_table(contacts)
  expect_identical(ft$body$dataset$Name, .party_contacts_df(contacts)$Name)
  expect_identical(ft$body$dataset$Contact, .party_contacts_df(contacts)$Contact)
  expect_null(.build_party_contacts_table(NULL))
  expect_null(.build_party_contacts_table(list()))

  files <- .dd_files()
  expect_identical(.build_file_specs_table(files)$body$dataset, .file_specs_df(files))
  expect_null(.build_file_specs_table(list()))

  expect_identical(
    .build_file_format_table(ds@files)$body$dataset,
    .file_format_df(ds@files)
  )
  expect_null(.build_file_format_table(list(DTAFileAny("raw.bin"))))

  expect_identical(
    .build_column_specs_table(ds@specs)$body$dataset,
    .column_specs_df(ds@specs)
  )
  expect_null(.build_column_specs_table(NULL))

  history <- list(list(version = "1.0", date = "2024-01-01", changes = "Initial"))
  expect_identical(
    .build_version_history_table(history)$body$dataset,
    .version_history_df(history)
  )
  expect_null(.build_version_history_table(NULL))
})
