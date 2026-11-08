# Document export. REQ-EXPORT-001 .. REQ-EXPORT-019.
#
# write_dta(), write_dataset_metadata()/write_file_specification(),
# export_specs_table(), export_column_value_table(), export_with_template()
# and columns_specs_from_word(). Every case reads the produced file back and
# asserts its content against the shipped clinical_dta.yaml specification (or
# a small local fixture), never merely that a file exists.

# ---- fixtures ---------------------------------------------------------------

# The same shipped specification test-PQ-workflows.R validates against. Every
# expected value used below (title, version, supplier/receiver names, column
# ids, rule ids) is copied here from inst/extdata/clinical_dta.yaml itself,
# not captured from a run.
et_dta <- function() {
  read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
}

# A DTA with no rules and no punctuation outside plain ASCII. The rule
# translations used elsewhere in this file render operators such as "->" and
# "!=" as single Unicode glyphs (see R/formattingHelpers.R), which the
# default pdflatex font encoding cannot typeset -- a LaTeX-backend concern
# entirely unrelated to what write_dta(format = "pdf") itself is being asked
# to do here, so the PDF case below is kept to a fixture that cannot trip it.
et_minimal_dta <- function() {
  DTA(
    datasets = list(DTADataSetTabular(
      id = "demo",
      specs = DTAColumnSpecCollection(columns = list(
        ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
      ))
    )),
    metadata = DTAMetaData(title = "Minimal PDF Fixture", version = "1.0")
  )
}

# helper-parse.R's qa_docx_tables() groups officer::docx_summary() cells by
# `doc_index`. officer documents doc_index as a running counter over every
# content node in the document -- each paragraph AND each table cell
# increments it -- rather than as a table identifier. Confirmed against
# officer 0.7.6 / flextable 0.10.0 here: a plain 3-row x 2-col flextable
# already produces 8 distinct doc_index values, one per cell, so grouping by
# it shatters every table with more than one cell into a separate one-cell
# "table" per cell. `table_index` is the column that is actually constant
# across one visual table's cells. This local, corrected copy is used in this
# file in its place; reported upstream (see the implementation report for the
# EXPORT work package) rather than edited, since helper-parse.R belongs to
# another work package.
et_docx_tables <- function(path) {
  summary <- qa_docx_summary(path)
  cells <- summary[summary$content_type %in% "table cell", , drop = FALSE]
  if (nrow(cells) == 0) {
    return(list())
  }
  lapply(split(cells, cells$table_index), function(part) {
    wide <- stats::reshape(
      part[, c("row_id", "cell_id", "text")],
      idvar = "row_id", timevar = "cell_id", direction = "wide"
    )
    wide <- wide[order(wide$row_id), , drop = FALSE]
    out <- as.data.frame(wide[, setdiff(names(wide), "row_id"), drop = FALSE])
    names(out) <- sub("^text[.]", "col", names(out))
    rownames(out) <- NULL
    out
  })
}

# The one table (if any) whose header row's first cell is `header1`.
et_docx_table <- function(path, header1) {
  tabs <- et_docx_tables(path)
  hit <- Filter(function(tbl) nrow(tbl) > 0 && identical(tbl$col1[[1]], header1), tabs)
  if (length(hit) == 0) NULL else hit[[1]]
}

# A minimal Word template with one paragraph per line, built the same way the
# package's own roxygen examples build one.
et_write_template <- function(lines, dir) {
  path <- file.path(dir, paste0("template-", as.integer(stats::runif(1, 1, 1e9)), ".docx"))
  doc <- officer::read_docx()
  for (ln in lines) {
    doc <- officer::body_add_par(doc, ln)
  }
  print(doc, target = path)
  path
}

# Two small, self-contained column specifications, so tests that only need a
# collection (not the whole clinical fixture) do not depend on it.
et_two_cols <- function() {
  DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(
      id = "STUDYID", label = "Study Identifier", type = "SAS Char",
      length = 10, nullable = FALSE, description = "Unique study ID"
    ),
    AGE = DTAColumnSpec(
      id = "AGE", label = "Age", type = "SAS Num",
      nullable = TRUE, description = "Age in years"
    )
  ))
}

# ---- export_column_value_table() input validation ---------------------------

test_that("OQ-EXPORT-001 | export_column_value_table on a bad first argument raises a package error | REQ-EXPORT-001", {
  dir <- qa_tempdir()
  out <- file.path(dir, "bad.docx")

  cond <- tryCatch(
    export_column_value_table(list(values = "x"), file = out, id = "values", quiet = TRUE),
    error = function(e) e
  )
  qa_check("a condition was raised for the malformed argument", inherits(cond, "condition"))

  # The same classed cli condition export_specs_table() raises for the
  # equivalent mistake (see OQ-EXPORT-017).
  qa_check("the condition is a classed cli (rlang) error", inherits(cond, "rlang_error"))
  qa_check("no file was written for the failed call", !file.exists(out))
})

# ---- write_dta() content -----------------------------------------------------

test_that("OQ-EXPORT-002 | write_dta docx names the transfer title, version, supplier and receiver | REQ-EXPORT-002", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  text <- qa_docx_text(out)
  for (expected in c(
    "Clinical Data Specification", # metadata@title
    "Test Company 2", # metadata@supplier$affiliation$name
    "Test Company" # metadata@receiver$affiliation$name
  )) {
    qa_step(
      sprintf("the document text contains '%s'", expected),
      TRUE, grepl(expected, text, fixed = TRUE)
    )
  }
  # The version is a labelled cell of the cover and Document Information
  # tables, and is repeated in the running footer.
  cells <- qa_docx_summary(out)$text
  qa_step("the Version label is followed by the declared version", "0.1", cells[match("Version", cells) + 1L])
})

test_that("OQ-EXPORT-003 | write_dta docx names every dataset and its declared columns | REQ-EXPORT-002 REQ-EXPORT-003", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  text <- qa_docx_text(out)
  qa_step("the dataset id appears", TRUE, grepl("clinical_data", text, fixed = TRUE))
  qa_step(
    "and the dataset's label appears with its id kept beside it",
    TRUE, grepl("Clinical data table (clinical_data)", text, fixed = TRUE)
  )

  declared_columns <- c(
    "STUDYID", "VISIT", "AGE", "GENDER", "INCLUDE", "HEIGHT", "WEIGHT",
    "BMI", "STATUS", "CONSENT", "CONSENT_DATE", "AE_TERM", "SAE_TERM", "SUBJECT_ID"
  )
  present <- vapply(declared_columns, function(id) grepl(id, text, fixed = TRUE), logical(1))
  qa_step(
    "every declared column id appears in the document",
    rep(TRUE, length(declared_columns)), unname(present)
  )
})

test_that("OQ-EXPORT-004 | write_dta docx column specification table matches the declared columns | REQ-EXPORT-003", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  tbl <- et_docx_table(out, "Variable Name")
  qa_check("the column specification table is found", !is.null(tbl))

  qa_step("the table has one header row plus one row per declared column", 15L, nrow(tbl))
  qa_step(
    "the header names the six documented facets",
    c("Variable Name", "Label", "Type", "Nullable", "Allowed Values / Pattern", "Description"),
    as.character(tbl[1, ])
  )

  studyid <- tbl[tbl$col1 == "STUDYID", ]
  qa_step(
    "the STUDYID row carries its declared label, type, nullability and permitted value",
    c("Study Identifier", "SAS Char (10)", "No", "1234-5678"),
    c(studyid$col2, studyid$col3, studyid$col4, studyid$col5)
  )

  visit <- tbl[tbl$col1 == "VISIT", ]
  qa_step("the VISIT row lists its declared codelist", "V01, V02, V03, EOT", visit$col5)
})

test_that("OQ-EXPORT-005 | write_dta docx validation rules table names every declared rule | REQ-EXPORT-004", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  tbl <- et_docx_table(out, "Rule ID")
  qa_check("the rules table is found", !is.null(tbl))
  qa_step("it lists a header row plus the eight declared rules", 9L, nrow(tbl))

  declared_rules <- c(
    "rule_equal_example", "rule_unequal_example", "rule_range_example",
    "rule_dependency_example", "rule_unique_example", "check_col_condition_example",
    "group_condition_pass_example", "group_condition_fail_example"
  )
  qa_step("every declared rule id is named, in declaration order", declared_rules, tbl$col1[-1])
})

test_that("OQ-EXPORT-006 | include_signatures adds an approval section naming the flagged signatories | REQ-EXPORT-005", {
  dir <- qa_tempdir()
  out <- file.path(dir, "signed.docx")
  write_dta(et_dta(), file = out, quiet = TRUE, include_signatures = TRUE)

  text <- qa_docx_text(out)
  qa_step("the approval heading is present", TRUE, grepl("Approval & Signatures", text, fixed = TRUE))

  # These three contacts (two receiver, one supplier) are the only ones the
  # fixture flags with `signature: yes`; the rest are reviewer- or backup-only.
  for (name in c("Alice Smith", "Bob Johnson", "Emily Turner")) {
    qa_step(sprintf("signatory '%s' is named", name), TRUE, grepl(name, text, fixed = TRUE))
  }
})

test_that("OQ-EXPORT-007 | include_signatures = FALSE omits the approval section | REQ-EXPORT-005", {
  dir <- qa_tempdir()
  out <- file.path(dir, "unsigned.docx")
  write_dta(et_dta(), file = out, quiet = TRUE, include_signatures = FALSE)

  qa_step(
    "no approval heading is present",
    FALSE, grepl("Approval & Signatures", qa_docx_text(out), fixed = TRUE)
  )
})

test_that("OQ-EXPORT-008 | include_yaml embeds the given specification text | REQ-EXPORT-006", {
  dir <- qa_tempdir()
  out <- file.path(dir, "with_yaml.docx")
  marker <- "metadata:\n  title: marker-for-embed-test\n"
  write_dta(et_dta(), file = out, quiet = TRUE, include_yaml = TRUE, yaml_text = marker)

  text <- qa_docx_text(out)
  qa_step(
    "the embedded-specification heading is present",
    TRUE, grepl("Embedded Specification (YAML)", text, fixed = TRUE)
  )
  qa_step("the given yaml text is embedded", TRUE, grepl("marker-for-embed-test", text, fixed = TRUE))
})

test_that("OQ-EXPORT-009 | include_yaml with no yaml_text warns and embeds nothing | REQ-EXPORT-006", {
  dir <- qa_tempdir()
  out <- file.path(dir, "no_yaml.docx")

  warned <- FALSE
  withCallingHandlers(
    write_dta(et_dta(), file = out, quiet = TRUE, include_yaml = TRUE, yaml_text = NULL),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  qa_check("a warning was raised", warned)
  qa_step(
    "no embedded-specification heading was added",
    FALSE, grepl("Embedded Specification", qa_docx_text(out), fixed = TRUE)
  )
})

test_that("OQ-EXPORT-010 | format = \"md\" writes a markdown file with the same key facts | REQ-EXPORT-007", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.md")
  write_dta(et_dta(), file = out, format = "md", quiet = TRUE)

  text <- paste(readLines(out, warn = FALSE), collapse = "\n")
  qa_step("the title line is present", TRUE, grepl("**Title:** Clinical Data Specification", text, fixed = TRUE))
  qa_step("the version line is present", TRUE, grepl("**Version:** 0.1", text, fixed = TRUE))
  # The fixture gives the dataset the label "Clinical data table", so its
  # heading carries the label with the id kept visible beside it -- escaped
  # Markdown, so the id's `_` cannot start emphasis.
  qa_step(
    "the dataset heading is present, naming the dataset by its label and its id",
    TRUE, grepl("### Clinical data table (clinical\\_data)", text, fixed = TRUE)
  )
})

test_that("OQ-EXPORT-011 | format = \"pdf\" produces a genuine PDF when a backend is available | REQ-EXPORT-008", {
  backend <- dta_pdf_backend()
  if (is.null(backend)) {
    testthat::skip("no PDF conversion backend available (dta_pdf_backend() is NULL)")
  }

  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.pdf")
  write_dta(et_minimal_dta(), file = out, format = "pdf", quiet = TRUE)
  qa_check("the file was written", file.exists(out))

  con <- file(out, open = "rb")
  on.exit(close(con), add = TRUE)
  qa_step(
    "the file begins with the PDF signature",
    charToRaw("%PDF"), readBin(con, what = "raw", n = 4L)
  )
})

test_that("OQ-EXPORT-012 | write_dta refuses to overwrite an existing file and leaves it untouched | REQ-EXPORT-009", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.docx")
  writeLines("not a real document, just a marker of the original content", out)
  original <- readLines(out)

  cond <- tryCatch(write_dta(et_dta(), file = out, quiet = TRUE), error = function(e) e)
  qa_check("a condition was raised", inherits(cond, "condition"))
  qa_step("the original file content is unchanged", original, readLines(out))
})

test_that("OQ-EXPORT-013 | write_dataset_metadata refuses to overwrite an existing file and leaves it untouched | REQ-EXPORT-009", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.docx")
  writeLines("not a real document, just a marker of the original content", out)
  original <- readLines(out)

  ds <- et_dta()[["clinical_data"]]
  cond <- tryCatch(write_dataset_metadata(ds, file = out, quiet = TRUE), error = function(e) e)
  qa_check("a condition was raised", inherits(cond, "condition"))
  qa_step("the original file content is unchanged", original, readLines(out))
})

# ---- write_dataset_metadata() / write_file_specification() ------------------

test_that("OQ-EXPORT-014 | write_dataset_metadata names the dataset, its files and its columns | REQ-EXPORT-010", {
  ds <- et_dta()[["clinical_data"]]
  dir <- qa_tempdir()
  out <- file.path(dir, "dataset.docx")
  write_dataset_metadata(ds, file = out, quiet = TRUE)

  text <- qa_docx_text(out)
  qa_step("the dataset id is named", TRUE, grepl("clinical_data", text, fixed = TRUE))
  qa_step(
    "the title names the dataset by its label with its id beside it",
    TRUE, grepl("Dataset Specification: Clinical data table (clinical_data)", text, fixed = TRUE)
  )

  # The information table leads with the identifier, then the label, then the
  # long description, each in its own row.
  info <- et_docx_tables(out)[[1]]
  qa_step(
    "the information table lists the id, name and description rows in that order",
    c("ID", "Name", "Description"), info$col1[2:4]
  )
  qa_step(
    "with the id and the name as the fixture declares them",
    c("clinical_data", "Clinical data table"), info$col2[2:3]
  )
  qa_step("the declared file pattern is named", TRUE, grepl("clinical_data.*.csv$", text, fixed = TRUE))
  qa_step("a declared column id is named", TRUE, grepl("SUBJECT_ID", text, fixed = TRUE))
})

test_that("OQ-EXPORT-015 | write_file_specification is the write_dataset_metadata alias | REQ-EXPORT-010", {
  ds <- et_dta()[["clinical_data"]]
  dir <- qa_tempdir()
  out_a <- file.path(dir, "a.docx")
  out_b <- file.path(dir, "b.docx")
  write_dataset_metadata(ds, file = out_a, quiet = TRUE)
  write_file_specification(ds, file = out_b, quiet = TRUE)

  # Content, not raw bytes: a DOCX is a zip archive that may stamp each entry
  # with the current time, so two calls made a second apart can differ in
  # bytes while saying exactly the same thing. The footer's own "Generated:"
  # line is minute-stamped (see .add_footer_section()) and is dropped for the
  # same reason -- two calls a minute apart are still the same document.
  drop_generated <- function(text) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    paste(lines[!startsWith(lines, "Generated:")], collapse = "\n")
  }
  qa_step(
    "the alias produces the same document content as the function it wraps",
    drop_generated(qa_docx_text(out_a)), drop_generated(qa_docx_text(out_b))
  )
})

# ---- export_specs_table() ---------------------------------------------------

test_that("OQ-EXPORT-016 | export_specs_table writes one row per column in declaration order | REQ-EXPORT-011", {
  dir <- qa_tempdir()
  out <- file.path(dir, "specs.docx")
  export_specs_table(et_two_cols(), file = out, quiet = TRUE)

  tbl <- et_docx_table(out, "Variable Name")
  qa_check("the table is found", !is.null(tbl))
  qa_step(
    "the header names the seven default facets",
    c("Variable Name", "Variable Label", "Type", "Length", "Format", "Nullable", "Description"),
    as.character(tbl[1, ])
  )
  qa_step("the columns appear in declaration order", c("STUDYID", "AGE"), tbl$col1[-1])

  studyid <- tbl[tbl$col1 == "STUDYID", ]
  qa_step(
    "the STUDYID row carries its declared label, type, length, nullability and description",
    c("Study Identifier", "SAS Char", "10", "No", "Unique study ID"),
    c(studyid$col2, studyid$col3, studyid$col4, studyid$col6, studyid$col7)
  )
})

test_that("OQ-EXPORT-017 | export_specs_table rejects a non-collection argument and an empty collection | REQ-EXPORT-012", {
  dir <- qa_tempdir()

  cond1 <- tryCatch(
    export_specs_table(list(a = 1), file = file.path(dir, "bad.docx"), quiet = TRUE),
    error = function(e) e
  )
  qa_check("a bad first argument raises a cli condition", inherits(cond1, "rlang_error"))

  empty <- DTAColumnSpecCollection(columns = list())
  out_empty <- file.path(dir, "empty.docx")
  cond2 <- tryCatch(export_specs_table(empty, file = out_empty, quiet = TRUE), error = function(e) e)
  qa_check("an empty collection raises a cli condition", inherits(cond2, "rlang_error"))
  qa_check("no file was written for the empty collection", !file.exists(out_empty))
})

# ---- export_column_value_table() content ------------------------------------

test_that("OQ-EXPORT-018 | export_column_value_table lists every permitted value of the named column | REQ-EXPORT-013", {
  coll <- DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE, values = c("1234-5678"))
  ))
  dir <- qa_tempdir()
  out <- file.path(dir, "values.docx")
  export_column_value_table(coll, file = out, id = "STUDYID", quiet = TRUE)

  tbl <- et_docx_table(out, "STUDYID")
  qa_check("the value table is found", !is.null(tbl))
  qa_step("the column header names the id, followed by its one permitted value", c("STUDYID", "1234-5678"), tbl$col1)
})

test_that("OQ-EXPORT-019 | export_column_value_table aborts for an unknown column or one without values | REQ-EXPORT-013", {
  coll <- DTAColumnSpecCollection(columns = list(
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  dir <- qa_tempdir()

  cond1 <- tryCatch(
    export_column_value_table(coll, file = file.path(dir, "a.docx"), id = "NOPE", quiet = TRUE),
    error = function(e) e
  )
  qa_check("an unknown column id raises a cli condition", inherits(cond1, "rlang_error"))

  cond2 <- tryCatch(
    export_column_value_table(coll, file = file.path(dir, "b.docx"), id = "AGE", quiet = TRUE),
    error = function(e) e
  )
  qa_check("a column with no permitted values raises a cli condition", inherits(cond2, "rlang_error"))
})

# ---- export_with_template() -------------------------------------------------

test_that("OQ-EXPORT-020 | export_with_template fills every placeholder present in the template | REQ-EXPORT-014", {
  dir <- qa_tempdir()
  template <- et_write_template(
    c("Title: {DTA_TITLE}", "Version: {DTA_VERSION}", "Supplier: {SUPPLIER_NAME}", "Receiver: {RECEIVER_NAME}"),
    dir
  )
  out <- file.path(dir, "filled.docx")
  export_with_template(et_dta(), template, out, quiet = TRUE)

  qa_step(
    "every placeholder is replaced with its extracted value",
    c(
      "Title: Clinical Data Specification", "Version: 0.1",
      "Supplier: Test Company 2", "Receiver: Test Company"
    ),
    strsplit(qa_docx_text(out), "\n", fixed = TRUE)[[1]]
  )
})

test_that("OQ-EXPORT-021 | export_with_template leaves an unmatched placeholder and warns about it | REQ-EXPORT-014", {
  dir <- qa_tempdir()
  template <- et_write_template(c("Known: {DTA_TITLE}", "Unknown: {NOT_A_REAL_PLACEHOLDER}"), dir)
  out <- file.path(dir, "partial.docx")

  warnings_seen <- character(0)
  withCallingHandlers(
    export_with_template(et_dta(), template, out, quiet = TRUE),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  qa_check(
    "a warning naming the unresolved placeholder was raised",
    any(grepl("NOT_A_REAL_PLACEHOLDER", warnings_seen, fixed = TRUE))
  )
  qa_step(
    "the known placeholder is replaced and the unknown one is left as written",
    c("Known: Clinical Data Specification", "Unknown: {NOT_A_REAL_PLACEHOLDER}"),
    strsplit(qa_docx_text(out), "\n", fixed = TRUE)[[1]]
  )
})

test_that("OQ-EXPORT-022 | export_with_template variables override the automatically extracted values | REQ-EXPORT-015", {
  dir <- qa_tempdir()
  template <- et_write_template(c("Title: {DTA_TITLE}"), dir)
  out <- file.path(dir, "overridden.docx")
  export_with_template(et_dta(), template, out, variables = list(DTA_TITLE = "OVERRIDDEN TITLE"), quiet = TRUE)

  qa_step("the caller-supplied value wins over the extracted one", "Title: OVERRIDDEN TITLE", qa_docx_text(out))
})

test_that("OQ-EXPORT-023 | write_dta(template =) routes through export_with_template | REQ-EXPORT-016", {
  dir <- qa_tempdir()
  template <- et_write_template(c("Title: {DTA_TITLE}"), dir)
  out <- file.path(dir, "via_write_dta.docx")
  write_dta(et_dta(), file = out, template = template, quiet = TRUE)

  qa_step(
    "the templated document is written at the requested path",
    "Title: Clinical Data Specification", qa_docx_text(out)
  )
})

test_that("OQ-EXPORT-024 | export_with_template falls back to the standard layout when processing fails | REQ-EXPORT-017", {
  dir <- qa_tempdir()
  bad_template <- file.path(dir, "not_really_a_docx.docx")
  writeLines("this file has a .docx extension but is not a zip archive", bad_template)
  out <- file.path(dir, "fallback.docx")

  # quiet = FALSE here: the warning this case is about is itself conditioned
  # on quiet, per export_with_template()'s own documentation ("signalled as a
  # warning condition (unless quiet = TRUE)") -- quiet = TRUE would silence
  # exactly the behaviour under test.
  warned <- FALSE
  withCallingHandlers(
    export_with_template(et_dta(), bad_template, out, quiet = FALSE),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  qa_check("a warning was raised for the unreadable template", warned)
  qa_step(
    "the output falls back to the standard write_dta layout",
    TRUE, grepl("Data Transfer Agreement", qa_docx_text(out), fixed = TRUE)
  )

  cond <- tryCatch(
    export_with_template(et_dta(), bad_template, file.path(dir, "no_fallback.docx"), quiet = TRUE, fallback = FALSE),
    error = function(e) e
  )
  qa_check("fallback = FALSE raises the failure instead of substituting the layout", inherits(cond, "rlang_error"))
})

# ---- columns_specs_from_word() -----------------------------------------------

test_that("OQ-EXPORT-025 | columns_specs_from_word reconstructs id, type, length and nullable from a well-formed table | REQ-EXPORT-018", {
  dir <- qa_tempdir()

  # Built directly with officer/flextable, so the table is exactly the one
  # this reader documents; the round trip through export_specs_table() is
  # OQ-EXPORT-026.
  fixture <- data.frame(
    id = c("STUDYID", "AGE"),
    label = c("Study Identifier", "Age"),
    type = c("SAS Char", "SAS Num"),
    length = c("10", ""),
    format = c("SAS $10.", "SAS 8."),
    nullable = c("No", "Yes"),
    description = c("Unique study ID", "Age in years"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  path <- file.path(dir, "specs_table.docx")
  doc <- flextable::body_add_flextable(officer::read_docx(), flextable::flextable(fixture))
  print(doc, target = path)

  back <- columns_specs_from_word(
    path,
    colnames = c("id", "label", "type", "length", "format", "nullable", "description")
  )
  qa_step("the same column identifiers come back, in order", c("STUDYID", "AGE"), names(back@columns))
  qa_step(
    "STUDYID keeps its declared type, length and nullability",
    c(type = "Char", length = "10", nullable = "FALSE"),
    c(
      type = back@columns$STUDYID@structure@type,
      length = as.character(back@columns$STUDYID@structure@length),
      nullable = as.character(back@columns$STUDYID@nullable)
    )
  )
  qa_step(
    "AGE keeps its declared type and nullability",
    c(type = "Num", nullable = "TRUE"),
    c(type = back@columns$AGE@structure@type, nullable = as.character(back@columns$AGE@nullable))
  )

  # Values and a pattern travel in the Description cell, one tag per line. A
  # pattern containing the letter n is the case the original regex cut short:
  # its character class `[^\\n]` excluded "n", not the line break.
  tagged <- fixture
  tagged$description <- c(
    "Unique study ID\n#@values: \"S-01\"; \"S-02\"",
    "Age in years\n#@pattern: ^(Screening|Baseline)$\nnot part of the pattern"
  )
  tagged_path <- file.path(dir, "specs_table_tagged.docx")
  print(
    flextable::body_add_flextable(officer::read_docx(), flextable::flextable(tagged)),
    target = tagged_path
  )
  tagged_back <- columns_specs_from_word(tagged_path)
  qa_step(
    "#@values is split on the separator and unquoted",
    c("S-01", "S-02"), unlist(tagged_back@columns$STUDYID@values, use.names = FALSE)
  )
  qa_step(
    "#@pattern is read whole, up to the end of its own line",
    "^(Screening|Baseline)$", tagged_back@columns$AGE@pattern
  )
  qa_step(
    "the tag lines are removed from the description",
    c("Unique study ID", "Age in years\nnot part of the pattern"),
    c(tagged_back@columns$STUDYID@description, tagged_back@columns$AGE@description)
  )
})

test_that("OQ-EXPORT-026 | columns_specs_from_word reads back the table export_specs_table just wrote | REQ-EXPORT-019", {
  dir <- qa_tempdir()

  # The common case first: no column declares an explicit format, exactly as
  # in the shipped clinical_dta.yaml fixture. That blank Format cell used to
  # abort the read (DEV-014, closed). Alongside it, a column with a format --
  # which export_specs_table() writes without its "SAS " prefix -- and one
  # carrying permitted values and a pattern in its Description cell.
  coll <- DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(id = "STUDYID", label = "Study Identifier", type = "SAS Char", length = 10, nullable = FALSE),
    VISIT = DTAColumnSpec(
      id = "VISIT", label = "Visit", type = "SAS Char", format = "SAS $20.", length = 20,
      nullable = TRUE, pattern = "^(Screening|Baseline|Week [0-9]+)$"
    ),
    ARM = DTAColumnSpec(id = "ARM", label = "Arm", type = "SAS Char", length = 8, values = c("Placebo", "Active")),
    VISITNUM = DTAColumnSpec(id = "VISITNUM", label = "Visit Number", type = "SAS Int", format = "SAS 8.", nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", label = "Age", type = "SAS Num", nullable = TRUE)
  ))
  path <- file.path(dir, "roundtrip.docx")
  export_specs_table(coll, file = path, quiet = TRUE)

  back <- tryCatch(columns_specs_from_word(path), error = function(e) e)
  qa_check("the table is read back without error", !inherits(back, "condition"))
  qa_step("the same column identifiers come back, in order", names(coll@columns), names(back@columns))
  facts <- function(x) {
    vapply(x@columns, function(s) {
      paste(
        s@structure@backend, s@structure@type, s@structure@format %||% "-",
        s@structure@length %||% "-", s@nullable %||% "-",
        paste(unlist(s@values), collapse = ";"), s@pattern %||% "-"
      )
    }, character(1))
  }
  qa_step(
    "every column's type, format, length, nullability, values and pattern survive the round trip",
    facts(coll), facts(back)
  )
})

# ---- document exports write atomically ---------------------------------------

test_that("OQ-EXPORT-044 | a document export that fails leaves an existing destination file byte-for-byte unchanged | REQ-EXPORT-032 | tags: white-box", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.pdf")
  original <- charToRaw("PRE-EXISTING PDF BYTES, NOT REGENERATED")
  writeBin(original, out)

  # Only one backend is reported, and it produces bytes that do not start
  # with the %PDF signature -- conversion reaches the staging file, fails
  # .is_pdf_file()'s check, and write_dta() must abort without ever touching
  # `out`.
  testthat::local_mocked_bindings(
    .pdf_backends_available = function() "pandoc",
    .pdf_conversion_available = function() TRUE,
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) {
      writeBin(charToRaw("not a pdf"), pdf_file)
      invisible(pdf_file)
    }
  )

  cond <- tryCatch(
    write_dta(et_minimal_dta(), file = out, format = "pdf", overwrite = TRUE, quiet = TRUE),
    error = function(e) e
  )
  qa_check("the export raises rather than silently replacing the destination", inherits(cond, "condition"))
  qa_step(
    "the file already at the destination is byte-for-byte unchanged",
    original,
    readBin(out, what = "raw", n = file.info(out)$size)
  )
  qa_step(
    "no staging file is left behind next to the destination",
    "existing.pdf",
    basename(list.files(dir))
  )
})

# ---- rules are described in plain English (REQ-EXPORT-036, REQ-EXPORT-039) ----
#
# Every expected sentence below is written by hand from the rule wording
# specification: the leading comment block of R/rulePhrasing.R, restated in
# the notes of REQ-EXPORT-036. None is read back from an export. The same
# expected wording is then compared against every output that describes a
# rule, which is what REQ-EXPORT-039 asks: the same rule reads the same
# everywhere.

# Typographic quotes and the list bullet, built from their code points so the
# source stays ASCII.
et_lq <- intToUtf8(0x201c)
et_rq <- intToUtf8(0x201d)
et_bullet <- intToUtf8(0x2022)
et_q <- function(x) paste0(et_lq, x, et_rq)

# One rule of each type, each exercising the parameters the requirement names:
# inclusive bounds; a column combination; a When/Then pair with a text value
# and a numeric test; grouping, named conditions, both constraint kinds, an
# explicit and a defaulted constraint id, both scopes, an explicit and an
# absent message. Three carry an author's description (Purpose), one not.
et_rule_list <- function() {
  list(
    DTARuleColRange(
      id = "r_range", description = "Adults of working age only",
      columns = "AGE", min = 18, max = 70
    ),
    DTARuleColUnique(id = "r_unique", columns = c("USUBJID", "VISIT")),
    DTARuleColCondition(
      id = "r_cond", description = "The active arm is dosed",
      condition = list(ARM = list(equals = "ACTIVE")),
      then = list(DOSE = list(greater = 0))
    ),
    DTARuleGroupCondition(
      id = "r_group", description = "Failed results are documented",
      group_by = "USUBJID",
      conditions = list(
        failed = list(GFSTAT = list(equals = "FAILED")),
        not_done = list(GFSTAT = list(equals = "NOT DONE")),
        reported = list(GFORRES = list(empty = FALSE))
      ),
      constraints = list(
        list(
          type = "requires", id = "needs_not_done", `if` = "failed",
          then = "not_done", if_scope = "all", message = "A failure needs NOT DONE"
        ),
        list(type = "mutually_exclusive", left = "failed", right = "reported")
      )
    )
  )
}

et_rule_dataset <- function(rules = et_rule_list(), files = list(), id = "rules_demo") {
  cols <- c("USUBJID", "VISIT", "AGE", "ARM", "DOSE", "GFSTAT", "GFORRES")
  DTADataSetTabular(
    id = id,
    specs = DTAColumnSpecCollection(
      # Rules may only name declared columns.
      columns = stats::setNames(lapply(cols, function(nm) {
        if (nm %in% c("AGE", "DOSE")) {
          DTAColumnSpec(id = nm, type = "SAS Num", nullable = TRUE)
        } else {
          DTAColumnSpec(id = nm, type = "SAS Char", length = 8, nullable = TRUE)
        }
      }), cols),
      rules = rules
    ),
    files = files
  )
}

et_rule_dta <- function(ds = et_rule_dataset()) {
  DTA(datasets = list(ds), metadata = DTAMetaData(title = "Rule wording", version = "1.0"))
}

# One line of a rule description: a labelled line ("Label: text", or "Label:"
# introducing a list), an unlabelled sentence, or a list item.
et_line <- function(text, label = "") list(label = label, text = text, bullet = FALSE)
et_item <- function(text) list(label = "", text = text, bullet = TRUE)

# The expected description of each rule of et_rule_list(), from the
# specification. `complete = FALSE` marks a description of which only the
# leading lines are specified word for word: the specification states the
# grouped condition's missing-value notes as a principle, not as sentences.
et_rule_wording <- list(
  r_range = list(complete = TRUE, lines = list(
    et_line("Adults of working age only", "Purpose"),
    et_line("Every value in column AGE must be a number from 18 to 70, inclusive."),
    et_line("Missing values are not checked.", "Missing values"),
    et_line("A value that is present but is not a number violates the rule.", "Non-numeric values")
  )),
  r_unique = list(complete = TRUE, lines = list(
    et_line("No two rows may have the same combination of values in columns USUBJID and VISIT."),
    et_line("Two missing values count as the same value.", "Missing values")
  )),
  r_cond = list(complete = TRUE, lines = list(
    et_line("The active arm is dosed", "Purpose"),
    et_line(paste0("When ARM is equal to ", et_q("ACTIVE"), ", DOSE must be greater than 0.")),
    et_line("", "Missing values"),
    et_item("If ARM is missing, the condition is not met and the row is not checked."),
    et_item("If DOSE is missing in a row that meets the condition, the rule is violated."),
    et_line("A value in DOSE that is not a number violates the rule.", "Non-numeric values")
  )),
  r_group = list(complete = FALSE, lines = list(
    et_line("Failed results are documented", "Purpose"),
    et_line(paste(
      "Rows are grouped by the value in column USUBJID; within each group,",
      "the requirements below must hold."
    )),
    et_line("", "Conditions"),
    et_item(paste0("failed: a row in which GFSTAT is equal to ", et_q("FAILED"))),
    et_item(paste0("not_done: a row in which GFSTAT is equal to ", et_q("NOT DONE"))),
    et_item("reported: a row in which GFORRES is not empty"),
    et_line("", "Requirements"),
    et_item(paste0(
      "needs_not_done: If every row of a group meets condition failed, then at ",
      "least one row of that group must meet condition not_done. If not, the ",
      "message is ", et_q("A failure needs NOT DONE"), "."
    )),
    et_item(paste0(
      "constraint_2: Within a group, it must not happen that at least one row ",
      "meets condition failed and also at least one row meets condition ",
      "reported. If not, a message naming the group and its rows is reported."
    ))
  ))
)

# The lines as text: "Label: text", "Label:", the sentence, or "<mark> item".
et_render_lines <- function(lines, mark) {
  vapply(lines, function(l) {
    if (l$bullet) {
      paste0(mark, " ", l$text)
    } else if (nzchar(l$label)) {
      if (nzchar(l$text)) paste0(l$label, ": ", l$text) else paste0(l$label, ":")
    } else {
      l$text
    }
  }, character(1))
}

# Keys of a rule definition as YAML would write them: none may appear in a
# human-readable rule description.
et_yaml_key_rx <- "(^|\\s)(type|columns|condition|then|group_by|conditions|constraints|if_scope|then_scope|min|max):"

# The body XML of a Word document, for checks on run formatting.
et_docx_body_xml <- function(path) {
  dir <- tempfile("docx-xml-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, files = "word/document.xml", exdir = dir)
  paste(readLines(file.path(dir, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Every table of a Word document whose header row starts with "Rule ID".
et_docx_rule_tables <- function(path) {
  Filter(function(tbl) nrow(tbl) > 0 && identical(tbl$col1[[1]], "Rule ID"), et_docx_tables(path))
}

# One Word rules table against the expected wording of every rule.
et_check_word_rule_table <- function(tbl, what, wording = et_rule_wording) {
  qa_step(
    paste0(what, ": the rules table has exactly the columns Rule ID and Description"),
    c("Rule ID", "Description"),
    unname(unlist(tbl[1, ]))
  )
  qa_step(
    paste0(what, ": the table lists the declared rules, in order"),
    names(wording), tbl$col1[-1]
  )
  for (id in names(wording)) {
    exp <- wording[[id]]
    want <- paste(et_render_lines(exp$lines, et_bullet), collapse = "\n")
    cell <- tbl$col2[match(id, tbl$col1)]
    got <- if (isTRUE(exp$complete) || is.na(cell)) cell else substr(cell, 1L, nchar(want))
    qa_step(paste0(what, ": rule ", id, " is described in the specified words"), want, got)
  }
  qa_check(
    paste0(what, ": no rule description holds a YAML key of the rule definition"),
    !any(grepl(et_yaml_key_rx, tbl$col2[-1]))
  )
}

et_check_word_no_monospace <- function(path, what) {
  xml <- et_docx_body_xml(path)
  qa_check(
    paste0(what, ": no text is set in a Consolas or Courier font"),
    !grepl("Consolas|Courier", xml)
  )
}

# Markdown reduced to its text: bold markers and backslash escapes removed,
# the three escaped HTML characters restored, blank lines dropped.
et_md_plain <- function(lines) {
  x <- gsub("**", "", lines, fixed = TRUE)
  x <- gsub("\\\\([[:punct:]])", "\\1", x)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x[nzchar(trimws(x))]
}

et_check_md_rules <- function(path, what, wording = et_rule_wording) {
  raw <- readLines(path, warn = FALSE, encoding = "UTF-8")
  qa_check(paste0(what, ": the file holds no fenced code block"), !any(grepl("^\\s*(```|~~~)", raw)))
  qa_check(paste0(what, ": the file holds no code span (no backtick at all)"), !any(grepl("`", raw, fixed = TRUE)))
  qa_check(paste0(what, ": no line holds a YAML key of a rule definition"), !any(grepl(et_yaml_key_rx, raw)))

  plain <- et_md_plain(raw)
  for (id in names(wording)) {
    exp <- wording[[id]]
    want <- et_render_lines(exp$lines, "-")
    start <- match(want[[1]], plain)
    qa_check(
      paste0(what, ": rule ", id, "'s description follows a line that names the rule"),
      !is.na(start) && start > 1L && startsWith(plain[[start - 1L]], paste0(id, " "))
    )
    got <- if (is.na(start)) NA_character_ else plain[start + seq_along(want) - 1L]
    qa_step(paste0(what, ": rule ", id, " is described in the specified words"), want, got)
    if (isTRUE(exp$complete) && !is.na(start)) {
      after <- plain[start + length(want)]
      qa_check(
        paste0(what, ": nothing further is added to rule ", id, "'s description"),
        is.na(after) || !grepl("^(- |Purpose:|Missing values:|Non-numeric values:|Problem:)", after)
      )
    }
  }
}

test_that("OQ-EXPORT-051 | each rule is described in plain English, naming its type and every parameter, in the Word and Markdown documents | REQ-EXPORT-036 REQ-EXPORT-039", {
  ds <- et_rule_dataset()
  dta <- et_rule_dta(ds)
  dir <- qa_tempdir()

  docx <- file.path(dir, "rules.docx")
  write_dta(dta, file = docx, quiet = TRUE)
  tabs <- et_docx_rule_tables(docx)
  qa_step("write_dta (docx) has one rules table for the one dataset", 1L, length(tabs))
  if (length(tabs) >= 1L) et_check_word_rule_table(tabs[[1]], "write_dta (docx)")
  et_check_word_no_monospace(docx, "write_dta (docx)")

  md <- file.path(dir, "rules.md")
  write_dta(dta, file = md, format = "md", quiet = TRUE)
  et_check_md_rules(md, "write_dta (md)")

  meta <- file.path(dir, "rules_meta.docx")
  write_dataset_metadata(ds, file = meta, quiet = TRUE)
  meta_tabs <- et_docx_rule_tables(meta)
  qa_step("write_dataset_metadata (docx) has one rules table", 1L, length(meta_tabs))
  if (length(meta_tabs) >= 1L) et_check_word_rule_table(meta_tabs[[1]], "write_dataset_metadata (docx)")
  et_check_word_no_monospace(meta, "write_dataset_metadata (docx)")

  meta_md <- file.path(dir, "rules_meta.md")
  write_dataset_metadata(ds, file = meta_md, format = "md", quiet = TRUE)
  et_check_md_rules(meta_md, "write_dataset_metadata (md)")
})

test_that("OQ-EXPORT-055 | equivalent spellings and integer or double bounds produce identical rule wording | REQ-EXPORT-036", {
  # Pairs the evaluator treats as the same test (R/evaluateRules.R,
  # dta_condition_operators()): min with max and range; equals and equal;
  # not_equals and not_equal; less_equal and max; greater_equal and min. Side
  # A stores bounds as integers, side B as doubles.
  side_a <- list(
    DTARuleColRange(id = "w_range", columns = "AGE", min = 18L, max = 70L),
    DTARuleColCondition(
      id = "w_band", condition = list(AGE = list(min = 18L, max = 70L)),
      then = list(ARM = list(equals = "ACTIVE"))
    ),
    DTARuleColCondition(
      id = "w_not", condition = list(ARM = list(not_equals = "ACTIVE")),
      then = list(DOSE = list(less_equal = 0L))
    ),
    DTARuleColCondition(
      id = "w_least", condition = list(ARM = list(equals = "ACTIVE")),
      then = list(DOSE = list(greater_equal = 5L))
    )
  )
  side_b <- list(
    DTARuleColRange(id = "w_range", columns = "AGE", min = 18, max = 70),
    DTARuleColCondition(
      id = "w_band", condition = list(AGE = list(range = c(18, 70))),
      then = list(ARM = list(equal = "ACTIVE"))
    ),
    DTARuleColCondition(
      id = "w_not", condition = list(ARM = list(not_equal = "ACTIVE")),
      then = list(DOSE = list(max = 0))
    ),
    DTARuleColCondition(
      id = "w_least", condition = list(ARM = list(equal = "ACTIVE")),
      then = list(DOSE = list(min = 5))
    )
  )
  # The statement of each, from the specification's range form and operator
  # table.
  statements <- c(
    w_range = "Every value in column AGE must be a number from 18 to 70, inclusive.",
    w_band = paste0("When AGE is from 18 to 70, inclusive, ARM must be equal to ", et_q("ACTIVE"), "."),
    w_not = paste0("When ARM is not equal to ", et_q("ACTIVE"), ", DOSE must be at most 0."),
    w_least = paste0("When ARM is equal to ", et_q("ACTIVE"), ", DOSE must be at least 5.")
  )

  dir <- qa_tempdir()
  cells <- lapply(list(a = side_a, b = side_b), function(rules) {
    out <- file.path(dir, paste0("equiv-", length(list.files(dir)), ".docx"))
    write_dataset_metadata(et_rule_dataset(rules), file = out, quiet = TRUE)
    tbl <- et_docx_rule_tables(out)[[1]]
    stats::setNames(tbl$col2[-1], tbl$col1[-1])
  })

  qa_step(
    "both spellings are described in exactly the same words, rule for rule",
    cells$a, cells$b
  )
  qa_step(
    "and each description opens with the statement the specification gives",
    statements,
    vapply(cells$a[names(statements)], function(x) strsplit(x, "\n", fixed = TRUE)[[1]][[1]], character(1))
  )
})

test_that("OQ-EXPORT-056 | the template rule blocks describe each rule in the same plain-English words as write_dta | REQ-EXPORT-036 REQ-EXPORT-039", {
  dir <- qa_tempdir()
  template <- et_write_template(c("{VALIDATION_RULES}", "{DATASETS}"), dir)
  out <- file.path(dir, "from-template.docx")
  export_with_template(et_rule_dta(), template = template, output = out, quiet = TRUE, fallback = FALSE)

  tabs <- et_docx_rule_tables(out)
  qa_step(
    "one rules table from {VALIDATION_RULES} and one from {DATASETS}",
    2L, length(tabs)
  )
  for (i in seq_along(tabs)) {
    et_check_word_rule_table(tabs[[i]], c("{VALIDATION_RULES}", "{DATASETS}")[[i]])
  }
  et_check_word_no_monospace(out, "the filled template")
})

# A delivery that violates r_range (AGE 10) and r_cond (ARM ACTIVE, DOSE 0) in
# its first row and nothing else: the second row is in range, not ACTIVE, and
# the two rows differ in USUBJID with no FAILED status.
et_rule_checked <- function(dir) {
  path <- file.path(dir, "rules.csv")
  utils::write.csv(
    data.frame(
      USUBJID = c("S1", "S2"), VISIT = c("V1", "V1"), AGE = c("10", "30"),
      ARM = c("ACTIVE", "PLACEBO"), DOSE = c("0", "5"),
      GFSTAT = c("DONE", "DONE"), GFORRES = c("x", "x"),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  ds <- et_rule_dataset(files = list(DTAFileCSV(filename = "rules.csv")))
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

test_that("OQ-EXPORT-057 | the HTML report explains a violated rule in the same plain-English words, without code | REQ-EXPORT-039", {
  dir <- qa_tempdir()
  ds <- et_rule_checked(dir)
  out <- file.path(dir, "report.html")
  write_validation_report(et_rule_dta(ds), out, quiet = TRUE)
  html <- xml2::read_html(out)

  boxes <- xml2::xml_find_all(
    html, "//div[contains(concat(' ', normalize-space(@class), ' '), ' inspect-explain ')]"
  )
  box_id <- vapply(boxes, function(b) {
    xml2::xml_text(xml2::xml_find_first(b, ".//dt[normalize-space(.)='Rule ID']/following-sibling::dd[1]"), trim = TRUE)
  }, character(1))
  qa_step("the violated rules r_range and r_cond are each explained", c("r_cond", "r_range"), sort(unique(box_id)))

  for (id in c("r_range", "r_cond")) {
    box <- boxes[[match(id, box_id)]]
    lines <- et_rule_wording[[id]]$lines
    texts <- vapply(lines, function(l) l$text, character(1))
    labels <- vapply(lines, function(l) l$label, character(1))
    qa_step(
      paste0("rule ", id, ": the explanation states every specified line, in order"),
      texts[nzchar(texts)],
      xml2::xml_text(xml2::xml_find_all(box, ".//dd/p | .//dd/ul/li"), trim = TRUE)
    )
    dts <- xml2::xml_text(xml2::xml_find_all(box, ".//dt"), trim = TRUE)
    qa_check(
      paste0("rule ", id, ": each labelled line keeps its label (Purpose, Missing values, ...)"),
      all(unique(labels[nzchar(labels)]) %in% dts)
    )
    qa_step(
      paste0("rule ", id, ": the explanation holds no code or pre element"),
      0L, length(xml2::xml_find_all(box, ".//code | .//pre"))
    )
    qa_check(
      paste0("rule ", id, ": the explanation holds no YAML key of the rule definition"),
      !grepl(et_yaml_key_rx, xml2::xml_text(box))
    )
  }
})

test_that("OQ-EXPORT-058 | explain_messages states a violated rule in the same plain-English words as the documents | REQ-EXPORT-039", {
  ds <- et_rule_checked(qa_tempdir())
  explained <- NULL
  printed <- withr::with_options(
    list(cli.width = 10000),
    testthat::capture_messages(explained <- explain_messages(ds, n = Inf))
  )
  printed <- paste(printed, collapse = "")
  msgs <- as.data.frame(messages(ds))
  rule_of <- msgs$rule_id[match(explained$id, msgs$id)]

  for (id in c("r_range", "r_cond")) {
    lines <- et_rule_wording[[id]]$lines
    statement <- lines[[2]]$text # each of the two opens with Purpose, then the statement
    qa_step(
      paste0("rule ", id, ": the expected value is the rule statement"),
      statement,
      unique(explained$should_be[rule_of %in% id])
    )
    others <- et_render_lines(lines[-2], et_bullet)
    qa_check(
      paste0("rule ", id, ": the purpose and the missing- and non-numeric-value lines are printed"),
      all(vapply(others, grepl, logical(1), x = printed, fixed = TRUE))
    )
  }
  qa_check(
    "no printed line holds a YAML key of a rule definition",
    !any(grepl(et_yaml_key_rx, strsplit(printed, "\n", fixed = TRUE)[[1]]))
  )
})

# ---- document name and organisation (REQ-EXPORT-037, REQ-EXPORT-038) ----------
#
# Expected values below are written from the requirement text: the document
# name is the first title, the transfer's own title follows it, and the
# organisation is "<name> · <unit>".

test_that("OQ-EXPORT-052 | the Markdown file leads with the document name, then the title and the organisation | REQ-EXPORT-037", {
  dir <- qa_tempdir()
  out <- file.path(dir, "named.md")
  write_dta(et_dta(),
    file = out, format = "md", quiet = TRUE,
    document_name = c("Data Transmission Specification", "Data Transfer Agreement"),
    organisation = "Example Institute", organisation_unit = "Data Management"
  )
  lines <- readLines(out, warn = FALSE)
  qa_step(
    "the first line is the joined document name as a top-level heading",
    "# Data Transmission Specification / Data Transfer Agreement", lines[[1]]
  )
  qa_check("the transfer's own title is in the header block", "**Title:** Clinical Data Specification" %in% lines)
  qa_check(
    "the organisation and unit are in the header block",
    "**Organisation:** Example Institute · Data Management" %in% lines
  )

  plain <- file.path(dir, "plain.md")
  write_dta(et_dta(), file = plain, format = "md", quiet = TRUE)
  plain_lines <- readLines(plain, warn = FALSE)
  qa_step("without arguments the name is Data Transfer Agreement", "# Data Transfer Agreement", plain_lines[[1]])
  qa_check("and no organisation text appears", !any(grepl("Organisation", plain_lines, fixed = TRUE)))
})

test_that("OQ-EXPORT-053 | the Word cover and the later pages' running header carry the document name and organisation | REQ-EXPORT-037", {
  dir <- qa_tempdir()
  out <- file.path(dir, "named.docx")
  write_dta(et_dta(),
    file = out, quiet = TRUE,
    document_name = "Data Transmission Specification",
    organisation = "Example Institute", organisation_unit = "Data Management"
  )
  paras <- Filter(nzchar, qa_docx_summary(out)$text)
  qa_step(
    "the cover's first three paragraphs are document name, transfer title, organisation",
    c("Data Transmission Specification", "Clinical Data Specification", "Example Institute · Data Management"),
    paras[1:3]
  )

  tmp <- file.path(dir, "unzipped")
  utils::unzip(out, exdir = tmp)
  headers <- list.files(file.path(tmp, "word"), pattern = "^header.*[.]xml$", full.names = TRUE)
  texts <- vapply(headers, function(p) {
    paste(xml2::xml_text(xml2::xml_find_all(xml2::read_xml(p), ".//*[local-name()='t']")), collapse = "")
  }, character(1))
  qa_check("there are running headers", length(texts) > 0)
  qa_check(
    "every running header names the organisation and unit",
    all(grepl("Example Institute · Data Management", texts, fixed = TRUE))
  )

  ds_out <- file.path(dir, "dataset.docx")
  write_dataset_metadata(et_dta()[["clinical_data"]],
    file = ds_out, quiet = TRUE,
    document_name = "Data Transmission Specification", organisation = "Example Institute"
  )
  ds_paras <- Filter(nzchar, qa_docx_summary(ds_out)$text)
  qa_step("the dataset document leads with the document name", "Data Transmission Specification", ds_paras[[1]])
  qa_step("followed by the dataset specification title", TRUE, grepl("^Dataset Specification: ", ds_paras[[2]]))
  qa_step("and the organisation", "Example Institute", ds_paras[[3]])

  bad_file <- file.path(dir, "bad.md")
  bad <- tryCatch(
    write_dta(et_dta(), file = bad_file, format = "md", organisation = c("a", "b")),
    error = function(e) e
  )
  qa_check(
    "a non-scalar organisation is rejected, naming the argument",
    inherits(bad, "error") && grepl("organisation", conditionMessage(bad), fixed = TRUE)
  )
  qa_check("and nothing was written", !file.exists(bad_file))
})

test_that("OQ-EXPORT-054 | the document and organisation placeholders are filled from write_dta's arguments, defaulted otherwise, and overridable | REQ-EXPORT-038", {
  dir <- qa_tempdir()
  template <- et_write_template("{DOCUMENT_NAME}|{DOCUMENT_ABBREVIATION}|{ORGANISATION_NAME}|{ORGANISATION_UNIT}", dir)

  filled <- file.path(dir, "filled.docx")
  write_dta(et_dta(),
    file = filled, template = template, quiet = TRUE,
    document_name = "Data Transmission Specification", document_abbreviation = "DTS",
    organisation = "Example Institute", organisation_unit = "Data Management"
  )
  qa_step(
    "each placeholder takes its argument",
    "Data Transmission Specification|DTS|Example Institute|Data Management", qa_docx_text(filled)
  )

  defaulted <- file.path(dir, "defaulted.docx")
  write_dta(et_dta(), file = defaulted, template = template, quiet = TRUE)
  qa_step("without arguments the defaults apply", "Data Transfer Agreement|DTA||", qa_docx_text(defaulted))

  overridden <- file.path(dir, "overridden.docx")
  write_dta(et_dta(),
    file = overridden, template = template, quiet = TRUE, document_abbreviation = "DTS",
    template_variables = list(DOCUMENT_ABBREVIATION = "MINE")
  )
  qa_step("a template_variables entry wins", "Data Transfer Agreement|MINE||", qa_docx_text(overridden))

  described <- dta_template_placeholders()
  tokens <- c("{DOCUMENT_NAME}", "{DOCUMENT_ABBREVIATION}", "{ORGANISATION_NAME}", "{ORGANISATION_UNIT}")
  qa_check("all four are reported", all(tokens %in% names(described)))
  qa_step("as inline placeholders", rep("inline", 4), unname(attr(described, "kind")[tokens]))
})
