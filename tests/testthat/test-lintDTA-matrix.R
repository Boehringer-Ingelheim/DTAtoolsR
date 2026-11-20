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

# ---- whitespace in a row-label id ----------------------------------------------
#
# A column id may hold spaces (it is the header text); a row-label id may not,
# because the reader names data columns `<column p>` and a spaced row-label id
# could collide with one. The exact header text of a row label is its `header`.

test_that("a row-label id with spaces is matrix_row_label_id_invalid, named on the row label", {
  for (bad in c("Sample ID", "Sample  ID")) {
    d <- mxl_matrix(row_labels = list(list(id = bad, type = "SAS Char", nullable = FALSE)))
    out <- lint_dta(d)
    expect_identical(mxl_codes(d), "matrix_row_label_id_invalid", info = bad)
    expect_identical(out$id[out$code == "matrix_row_label_id_invalid"], bad, info = bad)
    expect_identical(out$object[out$code == "matrix_row_label_id_invalid"], "column", info = bad)
  }
})

test_that("a row-label id with a tab or an edge space is reported by both guards", {
  # No column may have such an id, so the column spec is invalid as well.
  for (bad in c("SAMPLE\tID", " SAMPLE")) {
    d <- mxl_matrix(row_labels = list(list(id = bad, type = "SAS Char", nullable = FALSE)))
    expect_true("matrix_row_label_id_invalid" %in% mxl_codes(d), info = bad)
    expect_true("column_spec_invalid" %in% mxl_codes(d), info = bad)
  }
})

test_that("a spaced row-label id is refused even when it carries a header", {
  d <- mxl_matrix(row_labels = list(
    list(id = "Sample ID", header = "Sample ID", type = "SAS Char", nullable = FALSE)
  ))
  expect_identical(mxl_codes(d), "matrix_row_label_id_invalid")
})

test_that("only the spaced row label is reported, not its well-formed neighbour", {
  d <- mxl_matrix(row_labels = list(
    list(id = "SAMPLE_ID", header = "", type = "SAS Char", nullable = FALSE),
    list(id = "Sample Name", type = "SAS Char", nullable = TRUE)
  ))
  out <- lint_dta(d)
  expect_identical(out$id[out$code == "matrix_row_label_id_invalid"], "Sample Name")
})

test_that("the exact header text goes under header: SAMPLE_ID with header 'Sample ID' lints clean", {
  d <- mxl_matrix(row_labels = list(
    list(id = "SAMPLE_ID", header = "Sample ID", type = "SAS Char", nullable = FALSE)
  ))
  expect_identical(nrow(lint_dta(d)), 0L)
})

test_that("the same spaced id is fine on a tabular column, not on a matrix row label", {
  tab <- list(
    id = "t", type = "tabular", files = list(filename = "t.csv", type = "csv"),
    columns = list(list(id = "Sample ID", type = "SAS Char", nullable = FALSE))
  )
  expect_identical(nrow(lint_dta(tab)), 0L)
})

test_that("a YAML matrix with row label `Sample ID` and no header fails to load with dta_lint_error", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "datasets:",
    "  - id: m",
    "    type: matrix",
    "    files: { filename: \"m.tsv\", type: tsv }",
    "    row_labels:",
    "      - id: Sample ID",
    "        type: SAS Char",
    "        nullable: No",
    "    matrix:",
    "      cells: { type: SAS Int, nullable: No }"
  ), p)

  out <- lint_dta(p)
  expect_identical(out$code[out$severity == "error"], "matrix_row_label_id_invalid")
  expect_identical(out$dataset[out$code == "matrix_row_label_id_invalid"], "m")

  err <- expect_error(suppressWarnings(read_dta_from_yaml(p)), class = "dta_lint_error")
  expect_true("matrix_row_label_id_invalid" %in% err$findings$code)
})

test_that("the same YAML with id SAMPLE_ID and header 'Sample ID' loads", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "datasets:",
    "  - id: m",
    "    type: matrix",
    "    files: { filename: \"m.tsv\", type: tsv }",
    "    row_labels:",
    "      - id: SAMPLE_ID",
    "        header: \"Sample ID\"",
    "        type: SAS Char",
    "        nullable: No",
    "    matrix:",
    "      cells: { type: SAS Int, nullable: No }"
  ), p)

  expect_identical(nrow(lint_dta(p)), 0L)
  dta <- suppressWarnings(suppressMessages(read_dta_from_yaml(p)))
  label <- dta@datasets[["m"]]@specs@columns[["SAMPLE_ID"]]
  expect_identical(label@id, "SAMPLE_ID")
  expect_identical(label@header, "Sample ID")
})

# ---- the R vocabulary ------------------------------------------------------------

mxl_r <- function(cells = list(type = "R numeric", nullable = FALSE), ...) {
  mxl_matrix(
    row_labels = list(list(id = "ID", header = "", type = "R character", nullable = FALSE)),
    matrix = list(cells = cells), ...
  )
}

test_that("a matrix with R row labels and R cells lints clean, as a list and as an object", {
  for (cells in list(
    list(type = "R numeric", nullable = FALSE, range = list(min = 0), infinite = FALSE),
    list(type = "R integer", nullable = TRUE, values = list(0, 1, 2)),
    list(type = "R character", length = 8, pattern = "^[A-Z]+$")
  )) {
    d <- mxl_r(cells)
    expect_identical(nrow(lint_dta(d)), 0L, info = cells$type)
    expect_identical(nrow(lint_dta(dta_dataset_from_list(d))), 0L, info = cells$type)
  }
  # the two vocabularies mix freely between row labels and cells
  expect_identical(nrow(lint_dta(mxl_matrix(matrix = list(cells = list(type = "R numeric"))))), 0L)
  expect_identical(nrow(lint_dta(mxl_r(list(type = "SAS Num")))), 0L)
})

test_that("an R cell that is not a number or text is matrix_cells_type, as a SAS one is", {
  for (type in c("R logical", "R Date", "R POSIXct", "R hms", "SAS Date", "SAS Time", "SAS DateTime")) {
    expect_identical(mxl_codes(mxl_r(list(type = type))), "matrix_cells_type", info = type)
  }
})

test_that("a range or infinite on an R cell that does not take it is the code a SAS cell gets", {
  expect_identical(
    mxl_codes(mxl_r(list(type = "R character", range = list(min = 1)))), "cell_range_type"
  )
  expect_identical(
    mxl_codes(mxl_r(list(type = "R integer", infinite = TRUE))), "cell_finite_type"
  )
  expect_identical(
    mxl_codes(mxl_r(list(type = "R character", infinite = FALSE))), "cell_finite_type"
  )
  expect_identical(
    mxl_codes(mxl_r(list(type = "R integer", range = list(min = 5, max = 1)))), "cell_range_invalid"
  )
  expect_identical(nrow(lint_dta(mxl_r(list(type = "R numeric", infinite = TRUE)))), 0L)
})

test_that("rules on the R row labels of a matrix are judged by family, logical operands included", {
  labels <- list(
    list(id = "ID", header = "", type = "R character", nullable = FALSE),
    list(id = "FLAG", type = "R logical", nullable = TRUE),
    list(id = "N", type = "R integer", nullable = TRUE)
  )
  rule <- function(col, op, value, then = "ID") {
    list(
      id = paste(col, op), type = "col_condition",
      condition = stats::setNames(list(stats::setNames(list(value), op)), col),
      then = stats::setNames(list(list(empty = FALSE)), then)
    )
  }
  d <- function(r) mxl_matrix(row_labels = labels, rules = list(r))
  expect_identical(nrow(lint_dta(d(rule("FLAG", "equals", "T")))), 0L)
  expect_identical(nrow(lint_dta(d(rule("FLAG", "equals", TRUE)))), 0L)
  expect_identical(mxl_codes(d(rule("FLAG", "equals", "maybe"))), "rule_condition_value_unfit")
  expect_identical(mxl_codes(d(rule("N", "greater", "abc"))), "rule_condition_numeric_on_text")
  expect_identical(mxl_codes(d(rule("ID", "greater", 3))), "rule_condition_numeric_on_text")
  expect_identical(
    mxl_codes(mxl_matrix(row_labels = labels, rules = list(
      list(id = "r", type = "col_range", columns = "FLAG", min = 0, max = 1)
    ))),
    "rule_range_non_numeric"
  )
})
