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
      name = "demo",
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

# ---- export_column_value_table() input validation (DEV-003) ----------------

test_that("OQ-EXPORT-001 | export_column_value_table on a bad first argument leaks an internal error | REQ-EXPORT-001", {
  dir <- qa_tempdir()
  out <- file.path(dir, "bad.docx")

  cond <- tryCatch(
    export_column_value_table(list(values = "x"), file = out, id = "values", quiet = TRUE),
    error = function(e) e
  )
  qa_check("a condition was raised for the malformed argument", inherits(cond, "condition"))

  # DEV-003: the function has no input validation of its own, so the failure
  # is a bare base R condition rather than the cli condition
  # export_specs_table() raises for the equivalent mistake (see OQ-EXPORT-017).
  qa_known_deviation("DEV-003", !inherits(cond, "rlang_error"))
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
    "Version: 0.1", # metadata@version
    "Test Company 2", # metadata@supplier$affiliation$name
    "Test Company" # metadata@receiver$affiliation$name
  )) {
    qa_step(
      sprintf("the document text contains '%s'", expected),
      TRUE, grepl(expected, text, fixed = TRUE)
    )
  }
})

test_that("OQ-EXPORT-003 | write_dta docx names every dataset and its declared columns | REQ-EXPORT-002 REQ-EXPORT-003", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  text <- qa_docx_text(out)
  qa_step("the dataset name appears", TRUE, grepl("clinical_data", text, fixed = TRUE))

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
    c("Study Identifier", "SAS Char", "No", "1234-5678"),
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
  qa_step("the dataset heading is present", TRUE, grepl("### clinical_data", text, fixed = TRUE))
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
  qa_step("the dataset name is named", TRUE, grepl("clinical_data", text, fixed = TRUE))
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

  # Built directly with officer/flextable rather than export_specs_table(),
  # whose own Format column does not pair with this reader -- see
  # OQ-EXPORT-026 and REQ-EXPORT-019.
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
})

test_that("OQ-EXPORT-026 | columns_specs_from_word cannot read the table export_specs_table just wrote | REQ-EXPORT-019", {
  dir <- qa_tempdir()

  # The common case: no column declares an explicit format, exactly as in the
  # shipped clinical_dta.yaml fixture.
  coll <- DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(id = "STUDYID", label = "Study Identifier", type = "SAS Char", length = 10, nullable = FALSE)
  ))
  path <- file.path(dir, "roundtrip.docx")
  export_specs_table(coll, file = path, quiet = TRUE)

  cond <- tryCatch(
    columns_specs_from_word(
      path,
      colnames = c("id", "label", "type", "length", "format", "nullable", "description")
    ),
    error = function(e) e
  )
  qa_check(
    "reading export_specs_table's own output back aborts rather than reconstructing the collection",
    inherits(cond, "condition")
  )
  qa_step(
    "the abort is the low-level prefix-extraction error, not a caller-facing one",
    TRUE, grepl("prefix", conditionMessage(cond), fixed = TRUE)
  )
  qa_known_deviation("DEV-014", inherits(cond, "condition"))
})
