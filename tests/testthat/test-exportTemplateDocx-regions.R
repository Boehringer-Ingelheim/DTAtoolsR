# Tests for repeating regions ({#DATASETS} ... {/DATASETS}) in
# export_with_template() -- a span of the document body repeated once per
# dataset in the DTA, with per-dataset placeholders resolved inside each
# repetition and a bare dataset block bound to the current dataset.
#
# helper-docx.R (.make_template(), .docx_text(), .docx_paragraphs(),
# .read_docx_body_xml()) is sourced automatically by testthat before this
# file; it is reused rather than duplicated here.

# officer::docx_summary() of a filled-in template, for structural assertions
# (content_type / doc_index / text). Reused by every test below instead of
# repeating the read_docx() + docx_summary() pair.
.region_docx_summary <- function(path) {
  officer::docx_summary(officer::read_docx(path))
}

# The text of every table cell in a docx_summary() data frame, in document
# order.
.region_table_cells <- function(summary) {
  summary$text[summary$content_type == "table cell"]
}

test_that("a region repeats its content once per dataset, in order, with the markers gone", {
  dta <- create_example_DTA()
  template <- .make_template(c(
    "BEFORE",
    "{#DATASETS}",
    "Dataset: {DATASET_NAME} ({DATASET_TYPE})",
    "{/DATASETS}",
    "AFTER"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  expect_no_warning(export_with_template(dta, template, out, quiet = TRUE))

  expect_identical(
    .docx_paragraphs(out),
    c("BEFORE", "Dataset: demographics (tabular)", "Dataset: vitals (tabular)", "AFTER")
  )
})

test_that("a bare {COLUMN_SPECS} block inside a region binds to each dataset and renders without a heading", {
  # create_example_DTA() bundles "demographics" (has AGE) then "vitals" (has
  # AVAL instead) -- verified in R/DTA-class.R and R/DTADataSetTabular-class.R.
  dta <- create_example_DTA()
  template <- .make_template(c("{#DATASETS}", "{DATASET_NAME}", "{COLUMN_SPECS}", "{/DATASETS}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  expect_no_warning(export_with_template(dta, template, out, quiet = TRUE))

  summary <- .region_docx_summary(out)
  demographics_idx <- summary$doc_index[summary$content_type == "paragraph" & summary$text == "demographics"]
  vitals_idx <- summary$doc_index[summary$content_type == "paragraph" & summary$text == "vitals"]
  age_idx <- summary$doc_index[summary$content_type == "table cell" & summary$text == "AGE"]
  aval_idx <- summary$doc_index[summary$content_type == "table cell" & summary$text == "AVAL"]
  expect_length(demographics_idx, 1)
  expect_length(vitals_idx, 1)
  expect_gt(length(age_idx), 0)
  expect_gt(length(aval_idx), 0)

  expect_true(all(demographics_idx < age_idx))
  expect_true(all(age_idx < vitals_idx))
  expect_true(all(vitals_idx < aval_idx))
  expect_false(any(age_idx > vitals_idx))

  para_text <- summary$text[summary$content_type == "paragraph"]
  expect_false(any(grepl("^Column Specifications", para_text)))

  txt <- .docx_text(out)
  expect_false(grepl("{COLUMN_SPECS}", txt, fixed = TRUE))
  expect_false(grepl("{DATASET_NAME}", txt, fixed = TRUE))
})

test_that("a table inside a region is repeated once per dataset with its cells substituted", {
  dta <- create_example_DTA()
  template <- tempfile(fileext = ".docx")
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "{#DATASETS}")
  doc <- officer::body_add_table(doc, data.frame(x = "Cell {DATASET_NAME}"), header = FALSE)
  doc <- officer::body_add_par(doc, "{/DATASETS}")
  print(doc, target = template)
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  cell_text <- .region_table_cells(.region_docx_summary(out))
  expect_identical(cell_text, c("Cell demographics", "Cell vitals"))
})

test_that("the description, file, column and rule count placeholders resolve for one dataset", {
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  if (is.null(dta@datasets[[1]]@description)) {
    dta@datasets[[1]]@description <- "Demo description"
  }
  ds <- dta@datasets[[1]]

  template <- .make_template(c(
    "{#DATASETS}",
    "{DATASET_DESCRIPTION}|{DATASET_FILE_COUNT}|{DATASET_COLUMN_COUNT}|{DATASET_RULE_COUNT}",
    "{/DATASETS}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  expected <- paste(
    ds@description, length(ds@files), length(ds@specs@columns), length(ds@specs@rules),
    sep = "|"
  )
  expect_identical(.docx_paragraphs(out), expected)
})

test_that("a caller-supplied variables entry for a dataset placeholder wins in every repetition", {
  dta <- create_example_DTA()
  template <- .make_template(c("{#DATASETS}", "{DATASET_NAME}", "{/DATASETS}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, variables = list(DATASET_NAME = "X"), quiet = TRUE)

  expect_identical(.docx_paragraphs(out), c("X", "X"))
})

test_that("an explicit {COLUMN_SPECS:NAME} inside a region is not rebound to the current dataset", {
  dta <- create_example_DTA()
  template <- .make_template(c("{#DATASETS}", "{COLUMN_SPECS:vitals}", "{/DATASETS}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .region_docx_summary(out)
  cell_text <- .region_table_cells(summary)
  expect_equal(sum(cell_text == "AVAL"), 2)
  expect_false("AGE" %in% cell_text)

  heading_text <- summary$text[summary$content_type == "paragraph"]
  expect_equal(sum(heading_text == "Column Specifications — vitals"), 2)
})

test_that("a region and an ordinary block placeholder in the same template both render", {
  dta <- DTA(datasets = create_example_DTA()@datasets, metadata = create_example_DTAMetaData(2))
  template <- .make_template(c("{#DATASETS}", "{COLUMN_SPECS}", "{/DATASETS}", "{SIGNATURES_TABLE}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  cell_text <- .region_table_cells(.region_docx_summary(out))
  expect_true("AGE" %in% cell_text)
  expect_true("AVAL" %in% cell_text)

  txt <- .docx_text(out)
  expect_match(txt, "Emily Turner", fixed = TRUE)
  expect_false(grepl("{COLUMN_SPECS}", txt, fixed = TRUE))
  expect_false(grepl("{SIGNATURES_TABLE}", txt, fixed = TRUE))
  expect_false(grepl("{#DATASETS}", txt, fixed = TRUE))
  expect_false(grepl("{/DATASETS}", txt, fixed = TRUE))
})

test_that("two sequential regions each repeat independently", {
  dta <- create_example_DTA()
  template <- .make_template(c(
    "{#DATASETS}", "A {DATASET_NAME}", "{/DATASETS}",
    "{#DATASETS}", "B {DATASET_NAME}", "{/DATASETS}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  expect_identical(
    .docx_paragraphs(out),
    c("A demographics", "A vitals", "B demographics", "B vitals")
  )
})

test_that("a nested {#DATASETS} marker is treated as region content, not a nested region", {
  dta <- create_example_DTA()
  template <- .make_template(c("{#DATASETS}", "{#DATASETS}", "{DATASET_NAME}", "{/DATASETS}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out, quiet = FALSE),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_identical(
    .docx_paragraphs(out),
    c("{#DATASETS}", "demographics", "{#DATASETS}", "vitals")
  )
  expect_length(caught, 1)
  expect_match(caught[[1]], "{#DATASETS}", fixed = TRUE)
  expect_match(caught[[1]], "regions do not nest", fixed = TRUE)
})

test_that("an unclosed {#DATASETS} region is left in place and reported", {
  dta <- create_example_DTA()
  template <- .make_template(c("BEFORE", "{#DATASETS}", "{DATASET_NAME}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out, quiet = FALSE),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_identical(.docx_paragraphs(out), c("BEFORE", "{#DATASETS}", "{DATASET_NAME}"))
  msgs <- unlist(caught)
  expect_length(caught, 2)
  expect_true(any(grepl("{#DATASETS}", msgs, fixed = TRUE)))
  expect_true(any(grepl("{DATASET_NAME}", msgs, fixed = TRUE)))
})

test_that("a {#DATASETS} marker mid-sentence is left untouched and reported", {
  dta <- create_example_DTA()
  template <- .make_template("Start {#DATASETS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out, quiet = FALSE),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_identical(.docx_paragraphs(out), "Start {#DATASETS}")
  expect_true(any(grepl("{#DATASETS}", unlist(caught), fixed = TRUE)))
})

test_that("a lone {/DATASETS} marker with no opening marker is left untouched and reported", {
  dta <- create_example_DTA()
  template <- .make_template("{/DATASETS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out, quiet = FALSE),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_identical(.docx_paragraphs(out), "{/DATASETS}")
  expect_true(any(grepl("{/DATASETS}", unlist(caught), fixed = TRUE)))
})

test_that("a mistyped marker is named by the unresolved-placeholder warning rather than silently left", {
  # The marker prefix is part of the token grammar for every template, so
  # `{#DATASET}` -- a typo for {#DATASETS} -- is token-shaped: it is neither a
  # marker nor a value, and is reported like any unknown placeholder. The
  # alternative, text that is neither expanded nor reported, would survive
  # unnoticed into a signed document.
  dta <- create_example_DTA()
  template <- .make_template("See {#DATASET} here")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out, quiet = FALSE),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_identical(.docx_paragraphs(out), "See {#DATASET} here")
  expect_length(caught, 1)
  expect_match(caught[[1]], "no matching value", fixed = TRUE)
  expect_match(caught[[1]], "{#DATASET}", fixed = TRUE)
})

test_that("a region vanishes entirely when the DTA has zero datasets", {
  dta <- DTA(datasets = list(), metadata = create_example_DTAMetaData())
  template <- .make_template(c("BEFORE", "{#DATASETS}", "gone {DATASET_NAME}", "{/DATASETS}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  expect_no_warning(export_with_template(dta, template, out, quiet = TRUE))

  expect_identical(.docx_paragraphs(out), c("BEFORE", "AFTER"))
})

test_that("drawings inside a region get distinct docPr ids in each repetition", {
  img <- file.path(R.home("doc"), "html", "logo.jpg")
  skip_if_not(file.exists(img))

  dta <- create_example_DTA()
  template <- tempfile(fileext = ".docx")
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "{#DATASETS}")
  doc <- officer::body_add_img(doc, img, width = 1, height = 0.8)
  doc <- officer::body_add_par(doc, "{/DATASETS}")
  print(doc, target = template)
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  xml <- xml2::read_xml(.read_docx_body_xml(out))
  drawings <- xml2::xml_find_all(xml, ".//*[local-name()='docPr']")
  expect_length(drawings, 2)
  ids <- xml2::xml_attr(drawings, "id")
  expect_equal(anyDuplicated(ids), 0)

  expect_no_error(officer::read_docx(out))
})

test_that("dta_template_placeholders() lists region markers and dataset placeholders with a kind, in sync with the implementation", {
  x <- dta_template_placeholders()
  kind <- attr(x, "kind")
  expect_identical(kind[["{#DATASETS}"]], "region")
  expect_identical(kind[["{/DATASETS}"]], "region")
  expect_identical(kind[["{DATASET_NAME}"]], "dataset")

  dataset_names <- c(
    "{DATASET_NAME}", "{DATASET_TYPE}", "{DATASET_DESCRIPTION}",
    "{DATASET_FILE_COUNT}", "{DATASET_COLUMN_COUNT}", "{DATASET_RULE_COUNT}"
  )
  expect_true(all(dataset_names %in% names(x)))

  dta <- create_example_DTA()
  with_dta <- dta_template_placeholders(dta)
  expect_identical(names(with_dta), names(x))
  expect_identical(with_dta[["{DATASET_NAME}"]], x[["{DATASET_NAME}"]])

  expect_setequal(
    names(.tv_dataset_catalog()),
    names(.tv_dataset_variables(dta@datasets[[1]], "x"))
  )
})
