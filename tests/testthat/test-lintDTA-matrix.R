# lint_dta() on matrix datasets: the row_labels key, the matrix block and the
# keys that sit on the wrong dataset type. Codes are the contract.

mxl_matrix <- function(...) {
  base <- list(
    id = "m", type = "matrix",
    files = list(filename = "m.tsv", type = "tsv"),
    row_labels = list(list(id = "ID", header = "", type = "SAS Char", nullable = FALSE)),
    matrix = list(
      cells = list(type = "SAS Int", nullable = FALSE)
    )
  )
  over <- list(...)
  for (nm in names(over)) base[nm] <- list(over[[nm]])
  base
}

mxl_codes <- function(d, severity = "error") {
  out <- lint_dta(d)
  sort(out$code[out$severity == severity], method = "radix")
}

test_that("a clean matrix dataset lints clean, at document and dataset level", {
  expect_identical(nrow(lint_dta(mxl_matrix())), 0L)
  expect_identical(nrow(lint_dta(list(datasets = list(mxl_matrix())))), 0L)
})

test_that("a matrix object lints clean through the tabular branch", {
  ds <- dta_dataset_from_list(mxl_matrix())
  expect_identical(nrow(lint_dta(ds)), 0L)
  expect_identical(nrow(lint_dta(DTA(datasets = list(ds)))), 0L)
})

test_that("columns on a matrix is matrix_columns_key", {
  d <- mxl_matrix(columns = list(list(id = "ID", type = "SAS Char")))
  expect_identical(mxl_codes(d), "matrix_columns_key")
})

test_that("row_labels or matrix on another type is row_labels_key / matrix_key", {
  rl <- list(list(id = "ID", type = "SAS Char"))
  tab <- list(id = "t", type = "tabular", files = list(filename = "t.csv", type = "csv"))
  expect_identical(mxl_codes(c(tab, list(row_labels = rl))), "row_labels_key")
  expect_identical(mxl_codes(c(tab, list(matrix = list(cells = list(type = "SAS Int"))))), "matrix_key")
  file <- list(id = "f", type = "file", files = list(filename = "f.pdf", type = "any"))
  expect_identical(mxl_codes(c(file, list(matrix = list()))), "matrix_key")
})

test_that("a matrix without row labels is matrix_row_labels_missing", {
  expect_identical(mxl_codes(mxl_matrix(row_labels = NULL)), "matrix_row_labels_missing")
  expect_identical(mxl_codes(mxl_matrix(row_labels = list())), "matrix_row_labels_missing")
})

test_that("a matrix file that is not csv/tsv or has no header is matrix_file_type", {
  expect_identical(
    mxl_codes(mxl_matrix(files = list(filename = "m.sas7bdat", type = "sas7bdat"))),
    "matrix_file_type"
  )
  expect_identical(
    mxl_codes(mxl_matrix(files = list(filename = "m.tsv", type = "tsv", has_header = FALSE))),
    "matrix_file_type"
  )
  expect_identical(
    mxl_codes(mxl_matrix(files = list(
      list(filename = "a.tsv", type = "tsv"),
      list(filename = "b.csv", type = "csv", has_header = FALSE)
    ))),
    "matrix_file_type"
  )
})

test_that("a missing or malformed matrix block is reported by the class of the problem", {
  expect_identical(mxl_codes(mxl_matrix(matrix = NULL)), "matrix_field_missing")
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(cells = list(type = "SAS Int"), rnage = 1))),
    "matrix_field_unknown"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(cells = list(type = "SAS Int", rnage = 1)))),
    "matrix_field_unknown"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(rows = list(count = 3)))),
    "matrix_field_missing"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(columns = list(pattern = "^S")))),
    "matrix_field_missing"
  )
  # matrix.columns is optional: cells alone are a complete block
  expect_identical(nrow(lint_dta(mxl_matrix(matrix = list(cells = list(type = "SAS Int"))))), 0L)
  # the retired blocks and keys are unknown fields
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(header = list(corner = "blank"), cells = list(type = "SAS Int")))),
    "matrix_field_unknown"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(columns = list(corner = "blank"), cells = list(type = "SAS Int")))),
    "matrix_field_unknown"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(columns = list(count = 0), cells = list(type = "SAS Int")))),
    "matrix_count_invalid"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      cells = list(type = "SAS Char", range = list(min = 1))
    ))),
    "cell_range_type"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      cells = list(type = "SAS Int", range = list(min = 5, max = 1))
    ))),
    "cell_range_invalid"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      cells = list(type = "SAS Int", infinite = TRUE)
    ))),
    "cell_finite_type"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      cells = list(type = "SAS Num", range = list(min = "1e3"))
    ))),
    "matrix_number_text"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(columns = list(values = list(7L)), cells = list(type = "SAS Int")))),
    "matrix_columns_values_text"
  )
})

test_that("the header of a row label is judged: text and No are fine, anything else is not", {
  rl <- function(...) list(list(id = "ID", type = "SAS Char", nullable = FALSE, ...))
  rl2 <- function(h) {
    list(
      list(id = "ID", type = "SAS Char", nullable = FALSE),
      list(id = "DESC", type = "SAS Char", nullable = TRUE, header = h)
    )
  }
  for (ok in list("", "gene_id", "Sample ID", FALSE)) {
    expect_identical(nrow(lint_dta(mxl_matrix(row_labels = rl(header = ok)))), 0L, info = format(ok))
  }
  # no header key: the id is the header
  expect_identical(nrow(lint_dta(mxl_matrix(row_labels = rl()))), 0L)
  # YAML traps: header: 1, header: Y (TRUE), a list
  for (bad in list(1L, 2.5, TRUE, list("a", "b"), c("a", "b"))) {
    out <- lint_dta(mxl_matrix(row_labels = rl(header = bad)))
    expect_identical(out$code[out$severity == "error"], "row_label_header_invalid", info = format(bad))
    expect_identical(out$id[out$code == "row_label_header_invalid"], "ID", info = format(bad))
    expect_identical(out$object[out$code == "row_label_header_invalid"], "column", info = format(bad))
  }
  # No belongs to the first row label only
  out <- lint_dta(mxl_matrix(row_labels = rl2(FALSE)))
  expect_identical(out$code[out$severity == "error"], "row_label_header_position")
  expect_identical(out$id[out$code == "row_label_header_position"], "DESC")
  expect_identical(nrow(lint_dta(mxl_matrix(row_labels = rl2("")))), 0L)
  # `header` is not reported as an unknown column key
  expect_false(any(grepl("header", lint_dta(mxl_matrix(row_labels = rl(header = "x")))$message)))
})

test_that("the retired corner codes are gone", {
  expect_false(any(grepl("corner", c(
    mxl_codes(mxl_matrix(matrix = list(columns = list(corner = "blank"), cells = list(type = "SAS Int")))),
    mxl_codes(mxl_matrix(matrix = list(header = list(corner = list("a", "b")), cells = list(type = "SAS Int"))))
  ))))
})

test_that("cross-checks against the row labels are lint errors, not load-time aborts", {
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      rows = list(key = "NOPE"), cells = list(type = "SAS Int")
    ))),
    "matrix_row_key_invalid"
  )
  expect_identical(
    mxl_codes(mxl_matrix(row_labels = list(list(id = "ID", type = "SAS Char", nullable = TRUE)))),
    "matrix_key_nullable"
  )
})

test_that("a rule on a column that is not a row label is an error", {
  d <- mxl_matrix(rules = list(list(id = "r", type = "col_unique", columns = "S1")))
  expect_identical(mxl_codes(d), "rule_column_undeclared")
})

test_that("a rule id starting with .matrix: is reserved", {
  d <- mxl_matrix(rules = list(list(id = ".matrix:mine", type = "col_unique", columns = "ID")))
  expect_identical(mxl_codes(d), "matrix_rule_id_reserved")
})

test_that("a column spec error under row_labels is reported against the row label", {
  d <- mxl_matrix(row_labels = list(list(id = "ID", type = "SAS Char", nullable = "maybe")))
  out <- lint_dta(d)
  expect_true(any(out$object == "column" & out$id == "ID" & out$severity == "error"))
})

test_that("row_labels must be a list, naming the key in the message", {
  out <- lint_dta(mxl_matrix(row_labels = "ID"))
  expect_true(any(out$code == "column_spec_invalid" & grepl("row_labels", out$message, fixed = TRUE)))
})

test_that("a child template's dataset without a type is judged by the keys present", {
  expect_identical(dta_dataset_spec_key(list(row_labels = list(1))), "row_labels")
  expect_identical(dta_dataset_spec_key(list(columns = list(1))), "columns")
  expect_identical(dta_dataset_spec_key(list(row_labels = list(1), columns = list(1))), "columns")
  expect_identical(dta_dataset_spec_key(list(type = "matrix")), "row_labels")
  expect_identical(dta_dataset_spec_key(list(type = "tabular", row_labels = list(1))), "columns")
})

test_that("a matrix.columns count that cannot be met by its values is its own code", {
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      columns = list(values = list("a", "b"), count = 5), cells = list(type = "SAS Int")
    ))),
    "matrix_columns_count_unsatisfiable"
  )
  expect_identical(
    mxl_codes(mxl_matrix(matrix = list(
      columns = list(values = list("a", "b"), count = 2), cells = list(type = "SAS Int")
    ))),
    character(0)
  )
})

test_that("the cross-checks run even when the block does not build, and every problem is reported", {
  # A bad cell range (build aborts) plus a bad row key, a nullable key column,
  # a `header: No` on the second row label and a reserved rule id: all five.
  d <- mxl_matrix(
    row_labels = list(
      list(id = "ID", type = "SAS Char", nullable = TRUE),
      list(id = "DESC", header = FALSE, type = "SAS Char", nullable = TRUE)
    ),
    matrix = list(
      rows = list(key = "NOPE"),
      cells = list(type = "SAS Char", range = list(min = 1))
    ),
    rules = list(list(id = ".matrix:mine", type = "col_unique", columns = "ID"))
  )
  expect_identical(
    mxl_codes(d),
    c("cell_range_type", "matrix_row_key_invalid", "matrix_rule_id_reserved", "row_label_header_position")
  )

  # The default key (first row label) is still checked for nullability.
  d2 <- mxl_matrix(
    row_labels = list(list(id = "ID", type = "SAS Char", nullable = TRUE)),
    matrix = list(cells = list(type = "SAS Char", range = list(min = 1)))
  )
  expect_identical(mxl_codes(d2), c("cell_range_type", "matrix_key_nullable"))

  # A key that is not plain text is the build's finding alone: nothing to cross-check.
  d3 <- mxl_matrix(matrix = list(
    rows = list(key = list(NULL)), cells = list(type = "SAS Int")
  ))
  expect_identical(mxl_codes(d3), "matrix_key_invalid")
})
