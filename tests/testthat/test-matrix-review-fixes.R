# Behaviour settled by the review of the matrix format
# (thoughts/shared/plans/PLAN-matrix-format-review.md, section 6).

mrf_label <- function(id = "G", ...) list(id = id, type = "SAS Char", nullable = FALSE, ...)

mrf_ds <- function(row_labels = list(mrf_label()), matrix = list(cells = list(type = "SAS Int"))) {
  DTADataSetFactory(
    type = "matrix", id = "m", files = list(type = "tsv", filename = "m.tsv"),
    row_labels = row_labels, matrix = matrix
  )
}

mrf_file <- function(text, env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "m.tsv")
  writeLines(text, path, sep = "")
  path
}

# cli wraps a condition message, so a phrase is looked for in one line of it.
mrf_msg <- function(cnd) gsub("\\s+", " ", conditionMessage(cnd))

test_that("a header that is not one text or FALSE is refused with its class, whatever its type", {
  spec <- DTAColumnSpec(id = "G", type = "SAS Char", nullable = FALSE)
  for (bad in list(1, TRUE, c("a", "b"), NA_character_, list("a"))) {
    expect_error(DTAColumnSpecRowLabel(spec, header = bad), class = "dta_row_label_header_invalid")
  }
  for (good in list(NULL, "", "gene id", FALSE)) {
    expect_identical(dta_row_label_header(DTAColumnSpecRowLabel(spec, header = good)), good)
  }
})

test_that("the refusal of a row one field too wide does not advise what is already declared", {
  # No header: No yet: declaring it is the remedy.
  e <- expect_error(
    load_file(mrf_ds(), file = mrf_file("S1\tS2\ng1\t1\t2\n"), stream = "never"),
    class = "dta_matrix_header_fewer"
  )
  expect_match(mrf_msg(e), "declare `header: No` on the first row label", fixed = TRUE)

  # Declared, and the row is still too wide: it is the row that is wrong.
  declared <- mrf_ds(list(mrf_label(header = FALSE)))
  e <- expect_error(
    load_file(declared, file = mrf_file("S1\tS2\ng1\t1\t2\t3\n"), stream = "never"),
    class = "dta_matrix_header_fewer"
  )
  expect_match(mrf_msg(e), "field too many", fixed = TRUE)
  expect_false(grepl("declare `header: No`", mrf_msg(e), fixed = TRUE))
})

test_that("the refusal of a row one field too narrow names header: No only where it is declared", {
  declared <- mrf_ds(list(mrf_label(header = FALSE)))
  e <- expect_error(
    load_file(declared, file = mrf_file("\tS1\tS2\ng1\t1\t2\n"), stream = "never"),
    class = "dta_matrix_header_more"
  )
  expect_match(mrf_msg(e), "which declares `header: No`", fixed = TRUE)

  e <- expect_error(
    load_file(mrf_ds(), file = mrf_file("G\tS1\tS2\ng1\t1\n"), stream = "never"),
    class = "dta_matrix_header_more"
  )
  expect_match(mrf_msg(e), "missing a field", fixed = TRUE)
  expect_false(grepl("row_labels", mrf_msg(e), fixed = TRUE))
})

test_that("a column label that repeats a row-label id is said to do that, not to appear twice", {
  finding <- function(text) {
    ds <- check(load_file(mrf_ds(), file = mrf_file(text), stream = "never"), quiet = TRUE, persist = FALSE)
    m <- messages(ds)
    m$message[m$keyword %in% "uniqueNames"]
  }
  # The row label G has the header text "gene_id": "G" stands in the header once.
  ds <- mrf_ds(list(mrf_label(header = "gene_id")))
  ds <- check(
    load_file(ds, file = mrf_file("gene_id\tG\tS2\ng1\t1\t2\n"), stream = "never"),
    quiet = TRUE, persist = FALSE
  )
  m <- messages(ds)
  msg <- m$message[m$keyword %in% "uniqueNames"]
  expect_length(msg, 1L)
  expect_match(msg, "is also the ID of a row label", fixed = TRUE)
  expect_false(grepl("appears", msg, fixed = TRUE))

  # A label repeated among the data columns keeps its count.
  expect_match(finding("G\tS1\tS1\ng1\t1\t2\n"), "appears 2 times", fixed = TRUE)
})

test_that("an error in matrix.columns names matrix.columns, not the row-label key header", {
  e <- expect_error(
    dta_matrix_spec_from_list(list(columns = list(pattern = "(["), cells = list(type = "SAS Int"))),
    class = "dta_spec_invalid_pattern"
  )
  expect_match(mrf_msg(e), "matrix.columns", fixed = TRUE)
  expect_false(grepl("header:", mrf_msg(e), fixed = TRUE))
})

test_that("lint names the row label a header finding belongs to", {
  body <- list(
    id = "m", type = "matrix", files = list(type = "tsv", filename = "m.tsv"),
    row_labels = list(mrf_label("A", header = 7), mrf_label("B", header = FALSE)),
    matrix = list(cells = list(type = "SAS Int"))
  )
  l <- lint_dta(body)
  expect_match(l$message[l$code == "row_label_header_invalid"], "Row label 'A'", fixed = TRUE)
  expect_match(l$message[l$code == "row_label_header_position"], "Row label 'B'", fixed = TRUE)
})

test_that("header on a tabular column is named as a row-label key, and other keys still match partially", {
  body <- function(column) {
    list(
      id = "t", type = "tabular", files = list(type = "csv", filename = "t.csv"),
      columns = list(column)
    )
  }
  l <- lint_dta(body(list(id = "A", type = "SAS Char", header = "a")))
  msg <- l$message[l$code == "column_spec_invalid"]
  expect_length(msg, 1L)
  expect_match(msg, "row label of a matrix dataset", fixed = TRUE)

  # `example:` reaches `examples` by R's partial matching, as it always has.
  l <- lint_dta(body(list(id = "A", type = "SAS Char", example = "x")))
  expect_false("column_spec_invalid" %in% l$code)
})

test_that("a header text of spaces only is an empty field wherever it is put in words", {
  ds <- mrf_ds(list(mrf_label(header = "   ")))
  expect_match(dta_matrix_header_words(ds), "^\\(empty\\), then ")
  expect_identical(dta_row_label_header_words(ds@specs@columns[[1L]]), "empty")
  # ... and it is one for the file: an empty header field agrees with it.
  ds <- check(
    load_file(ds, file = mrf_file("\tS1\ng1\t1\n"), stream = "never"),
    quiet = TRUE, persist = FALSE
  )
  expect_identical(nrow(messages(ds)), 0L)
})

test_that("a wrong header text is explained with what to change, and an empty field is called one", {
  explained <- function(header_line) {
    ds <- mrf_ds(list(mrf_label(header = "gene_id")))
    ds <- check(
      load_file(ds, file = mrf_file(paste0(header_line, "\ng1\t1\n")), stream = "never"),
      quiet = TRUE, persist = FALSE
    )
    suppressMessages(explain_messages(ds))
  }
  wrong <- explained("Gene\tS1")
  expect_match(wrong$should_be, "must hold \"gene_id\" above this row-label column", fixed = TRUE)
  expect_match(wrong$should_be, "change the row label's header in the specification", fixed = TRUE)
  expect_identical(wrong$actual, "Gene")

  empty <- explained("\tS1")
  expect_identical(empty$actual, "(an empty field, or no field)")
})

test_that("row labels of a dataset written without a type keep their header out of the column lint", {
  l <- lint_dta(list(
    id = "m", files = list(type = "tsv", filename = "m.tsv"),
    row_labels = list(mrf_label(header = "gene_id")),
    matrix = list(cells = list(type = "SAS Int"))
  ))
  expect_false(any(grepl("row label of a matrix dataset", l$message, fixed = TRUE)))
})

test_that("row labels that are not a list are refused with the class of a matrix without row labels", {
  expect_error(dta_row_label_specs("GENE_ID"), class = "dta_matrix_invalid_row_labels")
})

# ---- the R vocabulary ------------------------------------------------------------

test_that("a matrix typed in the R vocabulary is current and keeps its types through as.list()", {
  ds <- mrf_ds(
    row_labels = list(list(id = "G", type = "R character", nullable = FALSE)),
    matrix = list(cells = list(type = "R numeric", nullable = FALSE, range = list(min = 0), infinite = TRUE))
  )
  expect_identical(ds@specs@columns[["G"]]@structure@backend, "R")
  label <- as.list(ds@specs@columns[["G"]])
  settings <- as.list(ds@matrix)
  expect_identical(label$type, "R character")
  expect_identical(settings$cells$type, "R numeric")
  expect_true(settings$cells$infinite)
  again <- DTADataSetFactory(
    type = "matrix", id = "m", files = list(type = "tsv", filename = "m.tsv"),
    row_labels = list(label), matrix = settings
  )
  expect_identical(as.list(again@matrix), settings)
  expect_identical(again@specs@columns[["G"]]@structure@type, "character")
})
