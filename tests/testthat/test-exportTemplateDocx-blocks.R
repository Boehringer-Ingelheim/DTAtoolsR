# Tests for block placeholders in export_with_template() -- placeholders that
# expand to real Word content (tables, headings, bulleted paragraphs) instead
# of a text run. See thoughts/shared/plans/PLAN-word-template-block-export.md
# for the design these tests pin.
#
# helper-docx.R (.make_template(), .docx_text(), .docx_paragraphs(),
# .read_docx_body_xml()) is sourced automatically by testthat before this
# file; it is reused rather than duplicated here.

# officer::docx_summary() of a filled-in template, for structural assertions
# (content_type / doc_index / text). Reused by every test below instead of
# repeating the read_docx() + docx_summary() pair.
.block_docx_summary <- function(path) {
  officer::docx_summary(officer::read_docx(path))
}

# The text of every table cell in a docx_summary() data frame, in document
# order.
.block_table_cells <- function(summary) {
  summary$text[summary$content_type == "table cell"]
}

# Given a docx_summary() that contains exactly one "Rule ID" / "Description"
# table (the shape .build_rules_table() always produces), return the
# Description text keyed by Rule ID. The header row is dropped positionally
# rather than by matching its label text, because a rule id or description
# could legitimately equal the header text itself.
.block_rule_table_descriptions <- function(summary) {
  cells <- .block_table_cells(summary)
  data_cells <- cells[-c(1, 2)]
  ids <- data_cells[c(TRUE, FALSE)]
  descriptions <- data_cells[c(FALSE, TRUE)]
  names(descriptions) <- ids
  descriptions
}

# Build a template whose header (not body) contains header_text as its whole
# paragraph. officer::body_add_par() only reaches the body, and officer's
# blank starter document has no header part at all, so one has to be spliced
# into the OPC package by hand: a word/header1.xml part, a Content_Types
# override, a relationship, and a headerReference inside the body sectPr.
# The relationship id must have a numeric suffix ("rIdNN") -- officer's own
# reader strips "rId" and coerces the remainder to an integer, and warns
# ("invalid id(s)") if that fails.
.make_template_with_header <- function(body_lines, header_text) {
  base_path <- .make_template(body_lines)
  on.exit(unlink(base_path, force = TRUE), add = TRUE)

  ex <- tempfile("block_hdr_build_")
  dir.create(ex)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(base_path, exdir = ex)

  header_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:hdr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:p><w:r><w:t xml:space="preserve">', header_text, "</w:t></w:r></w:p></w:hdr>"
  )
  writeLines(header_xml, file.path(ex, "word", "header1.xml"), useBytes = TRUE)

  ct_path <- file.path(ex, "[Content_Types].xml")
  ct <- paste(readLines(ct_path, warn = FALSE), collapse = "")
  ct <- sub(
    "</Types>",
    paste0(
      '<Override PartName="/word/header1.xml" ',
      'ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>',
      "</Types>"
    ),
    ct,
    fixed = TRUE
  )
  writeLines(ct, ct_path, useBytes = TRUE)

  rel_path <- file.path(ex, "word", "_rels", "document.xml.rels")
  rel <- paste(readLines(rel_path, warn = FALSE), collapse = "")
  rel <- sub(
    "</Relationships>",
    paste0(
      '<Relationship Id="rId900" ',
      'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" ',
      'Target="header1.xml"/></Relationships>'
    ),
    rel,
    fixed = TRUE
  )
  writeLines(rel, rel_path, useBytes = TRUE)

  doc_path <- file.path(ex, "word", "document.xml")
  docxml <- paste(readLines(doc_path, warn = FALSE), collapse = "")
  docxml <- sub(
    "(<w:sectPr[^>]*>)",
    '\\1<w:headerReference w:type="default" r:id="rId900"/>',
    docxml
  )
  writeLines(docxml, doc_path, useBytes = TRUE)

  out <- tempfile(fileext = ".docx")
  .zip_docx_dir(ex, out)
  out
}

test_that("{COLUMN_SPECS} renders a real table in place, between the surrounding paragraphs", {
  template <- .make_template(c("BEFORE", "{COLUMN_SPECS}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  before_idx <- summary$doc_index[summary$content_type == "paragraph" & summary$text == "BEFORE"]
  after_idx <- summary$doc_index[summary$content_type == "paragraph" & summary$text == "AFTER"]
  expect_length(before_idx, 1)
  expect_length(after_idx, 1)

  cell_idx <- summary$doc_index[summary$content_type == "table cell"]
  expect_gt(length(cell_idx), 0)
  expect_true(all(cell_idx > before_idx & cell_idx < after_idx))

  cell_text <- .block_table_cells(summary)
  ids <- unlist(lapply(dta@datasets, function(ds) {
    vapply(ds@specs@columns, function(spec) spec@id, character(1))
  }))
  for (id in ids) {
    expect_true(id %in% cell_text)
  }

  expect_false(grepl("{COLUMN_SPECS}", .docx_text(out), fixed = TRUE))
})

test_that("the column specs table is sized as a percentage of the text column, not fixed inches", {
  template <- .make_template("{COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  xml <- .read_docx_body_xml(out)
  expect_match(xml, 'w:type="pct"', fixed = TRUE)
})

test_that("{VALIDATION_RULES} renders each rule's translate_rule_to_human() description", {
  ds <- create_example_DTADataSetTabular(2)
  range_rule <- DTARuleColRange(id = "age_range_check", columns = "AGE", min = 18, max = 65)
  unique_rule <- DTARuleColUnique(id = "unique_subj_visit", columns = c("SUBJID", "VISIT"))
  ds@specs@rules <- list(range_rule, unique_rule)
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  template <- .make_template(c("BEFORE", "{VALIDATION_RULES}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  cell_text <- .block_table_cells(summary)
  expect_true("age_range_check" %in% cell_text)
  expect_true("unique_subj_visit" %in% cell_text)

  descriptions <- .block_rule_table_descriptions(summary)
  expect_identical(unname(descriptions[["age_range_check"]]), translate_rule_to_human(range_rule))
})

test_that("a DTARuleGroupCondition renders its grouping column and constraints via translate_rule_to_human()", {
  ds <- create_example_DTADataSetTabular(2)
  rule <- DTARuleGroupCondition(
    id = "block_group_condition_example",
    group_by = "SUBJID",
    conditions = list(
      visit_v03 = list(VISIT = list(equals = "V03")),
      adult = list(AGE = list(greater_equal = 18))
    ),
    constraints = list(
      list(type = "requires", `if` = "visit_v03", `then` = "adult")
    )
  )
  ds@specs@rules <- list(rule)
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  template <- .make_template("{VALIDATION_RULES}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  descriptions <- .block_rule_table_descriptions(summary)
  cell_text <- descriptions[["block_group_condition_example"]]

  expect_match(cell_text, "SUBJID", fixed = TRUE)
  expect_match(cell_text, "must hold", fixed = TRUE)
  expect_identical(unname(cell_text), translate_rule_to_human(rule))
})

test_that("{COLUMN_SPECS:NAME} renders only the named dataset", {
  # create_example_DTA() bundles "demographics" (has AGE) and "vitals" (has
  # AVAL instead) -- verified in R/DTADataSetTabular-class.R and
  # R/DTAColumnSpecCollection-class.R.
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS:demographics}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  cell_text <- .block_table_cells(.block_docx_summary(out))
  expect_true("AGE" %in% cell_text)
  expect_false("AVAL" %in% cell_text)
})

test_that("{COLUMN_SPECS:NOPE} is left in place and warns when the dataset is unknown", {
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS:NOPE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  suppressMessages(
    expect_warning(
      export_with_template(dta, template, out),
      "block placeholder",
      ignore.case = TRUE
    )
  )
  expect_match(.docx_text(out), "{COLUMN_SPECS:NOPE}", fixed = TRUE)
})

test_that("a block token inside a sentence is left untouched and does not become a table", {
  dta <- create_example_DTA()
  template <- .make_template("Columns: {COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  suppressMessages(expect_warning(
    export_with_template(dta, template, out),
    regexp = "block placeholders were left unchanged"
  ))

  expect_true("Columns: {COLUMN_SPECS}" %in% .docx_paragraphs(out))
  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("a block token in a header is left untouched and never reaches the body", {
  template <- .make_template_with_header("BODY", "{COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  ex <- tempfile("block_hdr_out_")
  dir.create(ex)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(out, files = "word/header1.xml", exdir = ex)
  header_xml <- paste(readLines(file.path(ex, "word", "header1.xml"), warn = FALSE), collapse = "")
  expect_match(header_xml, "{COLUMN_SPECS}", fixed = TRUE)

  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("a user-supplied variables entry wins over the block renderer", {
  dta <- create_example_DTA()
  template <- .make_template(c("BEFORE", "{COLUMN_SPECS}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(
    dta, template, out,
    variables = list(COLUMN_SPECS = "plain text here"),
    quiet = TRUE
  )

  expect_true("plain text here" %in% .docx_paragraphs(out))
  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("a rendered block token is never reported by the unresolved-placeholder warning", {
  dta <- create_example_DTA()
  template <- .make_template(c("{COLUMN_SPECS}", "{NOT_A_FIELD}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- w
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_length(caught, 1)
  msg <- conditionMessage(caught[[1]])
  expect_match(msg, "NOT_A_FIELD", fixed = TRUE)
  expect_false(grepl("COLUMN_SPECS", msg, fixed = TRUE))
})

test_that("an inline-only template is untouched by the block-rendering pass", {
  dta <- create_example_DTA()
  template <- .make_template("{DTA_TITLE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  expect_true(dta@metadata@title %in% .docx_paragraphs(out))
  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("the bundled clinical template renders the dataset section from plain R (regression)", {
  # Today {DATASETS_DETAIL} is not in the inline catalogue, so plain R usage
  # (no template_variables, i.e. no Shiny app in the loop) leaves it printed
  # literally and the dataset section empty. {DATASETS_DETAIL} is a block
  # alias for {DATASETS}, so this must now render for real.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  tmpl <- system.file("extdata", "templates", "clinical_dta_template.docx", package = "DTAtools")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_dta(dta, file = out, template = tmpl, overwrite = TRUE, quiet = TRUE)

  txt <- .docx_text(out)
  ids <- vapply(dta@datasets[[1]]@specs@columns, function(spec) spec@id, character(1))
  for (id in ids) {
    expect_match(txt, id, fixed = TRUE)
  }
  expect_false(grepl("{DATASETS_DETAIL}", txt, fixed = TRUE))
  # {YAML_EMBEDDED} is a caller-supplied inline value, not a block token, and
  # nobody supplied template_variables here -- it is expected to survive.
})

test_that("SIGNATURES_TABLE, VERSION_HISTORY_TABLE, SUPPLIER_CONTACTS_TABLE, AUTHORIZED_CORRECTIONS_LIST and FILE_SPECS each render identifiable fixture content", {
  # create_example_DTAMetaData(2) carries signatories, version history, a
  # supplier contact and authorized-for-corrections names; the clinical_data
  # dataset from the YAML fixture carries a file handler. Neither fixture
  # alone has all five, so the two are combined.
  yaml_dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  dta <- DTA(datasets = yaml_dta@datasets, metadata = create_example_DTAMetaData(2))

  template <- .make_template(c(
    "{SIGNATURES_TABLE}",
    "{VERSION_HISTORY_TABLE}",
    "{SUPPLIER_CONTACTS_TABLE}",
    "{AUTHORIZED_CORRECTIONS_LIST}",
    "{FILE_SPECS}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)
  txt <- .docx_text(out)

  expect_match(txt, "Emily Turner", fixed = TRUE) # SIGNATURES_TABLE: a signatory name
  expect_match(txt, "1.5", fixed = TRUE) # VERSION_HISTORY_TABLE: a version string
  expect_match(txt, "emily.turner@supplier.com", fixed = TRUE) # SUPPLIER_CONTACTS_TABLE: a contact detail
  expect_match(txt, "Alice Smith", fixed = TRUE) # AUTHORIZED_CORRECTIONS_LIST: an authorized name
  expect_match(txt, "clinical_data.*.csv$", fixed = TRUE) # FILE_SPECS: a filename/pattern
})

test_that("dta_template_placeholders() reports both inline and block tokens with a kind attribute", {
  x <- dta_template_placeholders()
  expect_true("{COLUMN_SPECS}" %in% names(x))

  kind <- attr(x, "kind")
  expect_identical(kind[["{COLUMN_SPECS}"]], "block")
  expect_identical(kind[["{DTA_TITLE}"]], "inline")

  inline_catalog <- .tv_placeholder_catalog()
  expect_identical(x[names(inline_catalog)], inline_catalog)
})

test_that("a substituted value that spells a block token is never mistaken for a placeholder (regression)", {
  # Pass 2 used to identify block placeholders by their text in the FINISHED
  # document -- so a DTA whose title happens to be the literal string
  # "{COLUMN_SPECS}" would have its title paragraph silently replaced by a
  # table. .tv_mark_block_paragraphs() now stamps the template's own
  # paragraphs before any substitution runs, so what the *template* asked for
  # can never be confused with what a substituted *value* happens to spell.
  dta <- create_example_DTA()
  dta@metadata@title <- "{COLUMN_SPECS}"
  template <- .make_template(c("BEFORE", "{DTA_TITLE}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
  expect_true("BEFORE" %in% .docx_paragraphs(out))
  expect_true("{COLUMN_SPECS}" %in% .docx_paragraphs(out))
  expect_true("AFTER" %in% .docx_paragraphs(out))
})

test_that("a one-cell table whose only content is a block token is left untouched (regression)", {
  # officer::cursor_reach() matches the text of TOP-LEVEL body children, and
  # xml2::xml_text() of a <w:tbl> concatenates every cell -- so a one-cell
  # table whose only content is the token used to match as a whole and be
  # replaced, deleting the table it lived in. .tv_mark_block_paragraphs() only
  # stamps real <w:body>/<w:p> children, never a paragraph nested inside a
  # <w:tbl>, so a token sitting in a table cell is left alone instead.
  dta <- create_example_DTA()
  template <- tempfile(fileext = ".docx")
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "BEFORE")
  doc <- officer::body_add_table(doc, data.frame(x = "{COLUMN_SPECS}"), header = FALSE)
  doc <- officer::body_add_par(doc, "AFTER")
  print(doc, target = template)
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  cell_text <- .block_table_cells(summary)
  expect_length(cell_text, 1)
  expect_identical(cell_text, "{COLUMN_SPECS}")
  expect_false(any(grepl("STUDYID", cell_text, fixed = TRUE)))
  expect_true("BEFORE" %in% .docx_paragraphs(out))
  expect_true("AFTER" %in% .docx_paragraphs(out))
})

test_that("{COLUMN_SPECS:} with an empty dataset argument renders all datasets rather than a silent no-op", {
  # An empty `:DATASET` argument is what a deleted dataset name leaves behind
  # in a template. The grammar treats it as "no argument", i.e. every dataset
  # -- not as a token that matches nothing and is silently left in place.
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS:}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  cell_text <- .block_table_cells(.block_docx_summary(out))
  expect_gt(length(cell_text), 0)
  expect_true("STUDYID" %in% cell_text)
})

test_that("a template that defines no heading styles at all still exports, via the direct-formatted fallback", {
  # .tv_block_heading() exists because a user template need not define
  # "heading 2" -- but every template in this suite happens to define
  # heading 1-3, so the fallback branch (.add_bold_subheading()) never runs
  # without help. Force it by making officer report no paragraph styles at
  # all; export_with_template(fallback = TRUE) swallowing this error and
  # silently handing back the built-in layout instead of the user's template
  # is the failure mode this pins against.
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  # Scoped to this local function (not the whole test_that()), so the mock is
  # gone again by the time .block_docx_summary() below calls officer's own
  # styles_info() to build its style_name/style_type columns.
  export_without_heading_styles <- function() {
    testthat::local_mocked_bindings(
      styles_info = function(...) {
        data.frame(style_name = character(0), style_type = character(0), stringsAsFactors = FALSE)
      },
      .package = "officer"
    )
    export_with_template(dta, template, out, quiet = TRUE)
  }
  expect_no_error(export_without_heading_styles())

  nm <- names(dta@datasets)[[1]]
  summary <- .block_docx_summary(out)
  expect_true(
    paste0("Column Specifications \u2014 ", nm) %in% summary$text[summary$content_type == "paragraph"]
  )
  expect_gt(sum(summary$content_type == "table cell"), 0)
})
