# DTAColumnSpecCell: a column spec that also bounds the value.

num_cell <- function(...) DTAColumnSpecCell(id = "VAL", type = "SAS Num", nullable = TRUE, ...)

test_that("a cell spec is a column spec with a range and a finiteness flag", {
  cell <- num_cell(minimum = 0, maximum = 10, finite = TRUE)
  expect_s3_class(cell, "DTAtools::DTAColumnSpec")
  expect_equal(cell@minimum, 0)
  expect_equal(cell@maximum, 10)
  expect_true(cell@finite)
  expect_equal(DTAColumnSpecCell(type = "SAS Int")@id, "cells")
  expect_null(DTAColumnSpecCell(type = "SAS Int")@minimum)
})

test_that("the validator refuses a contradictory cell spec by class", {
  expect_error(num_cell(minimum = 5, maximum = 1), class = "dta_cell_range_invalid")
  expect_error(num_cell(minimum = c(1, 2)), class = "dta_cell_range_invalid")
  expect_error(num_cell(maximum = NA_real_), class = "dta_cell_range_invalid")
  # equal bounds are allowed
  expect_no_error(num_cell(minimum = 3, maximum = 3))

  expect_error(
    DTAColumnSpecCell(id = "VAL", type = "SAS Char", minimum = 0),
    class = "dta_cell_range_type"
  )
  expect_error(
    DTAColumnSpecCell(id = "VAL", type = "SAS Char", maximum = 0),
    class = "dta_cell_range_type"
  )
  expect_no_error(DTAColumnSpecCell(id = "VAL", type = "SAS Int", minimum = 0))

  expect_error(
    DTAColumnSpecCell(id = "VAL", type = "SAS Int", finite = TRUE),
    class = "dta_cell_finite_type"
  )
  expect_error(
    DTAColumnSpecCell(id = "VAL", type = "SAS Char", finite = TRUE),
    class = "dta_cell_finite_type"
  )
  expect_error(num_cell(finite = NA), class = "dta_cell_finite_type")
})

test_that("as_json_schema() adds exactly the keys that are set", {
  schema <- as_json_schema(num_cell(minimum = 0, maximum = 10, finite = TRUE))
  expect_equal(schema$minimum, 0)
  expect_equal(schema$maximum, 10)
  expect_true(schema$finite)
  expect_equal(schema$type, c("number", "null"))

  bare <- as_json_schema(num_cell())
  expect_false(any(c("minimum", "maximum", "finite") %in% names(bare)))
  # finite = FALSE is "no check", which is no key
  expect_null(as_json_schema(num_cell(finite = FALSE))$finite)
})

test_that("as.list() writes range and infinite, and omits what is unset", {
  out <- as.list(num_cell(minimum = 0, maximum = 10, finite = TRUE))
  expect_equal(out$range, list(min = 0, max = 10))
  expect_false(out$infinite)

  expect_equal(as.list(num_cell(maximum = 2))$range, list(max = 2))
  expect_null(as.list(num_cell())$range)
  expect_true(as.list(num_cell(finite = FALSE))$infinite)

  int <- as.list(DTAColumnSpecCell(id = "VAL", type = "SAS Int", minimum = 1L))
  expect_null(int$infinite)
  expect_equal(int$range, list(min = 1L))
  expect_equal(int$type, "SAS Int")
})

# ---- dta_check_column_spec() --------------------------------------------------

findings <- function(x, ...) {
  out <- dta_check_column_spec("VAL", as_json_schema(num_cell(...)), x)
  if (is.null(out)) {
    return(data.frame(row = integer(0), keyword = character(0)))
  }
  out[order(out$row, out$keyword), c("row", "keyword")]
}

test_that("bounds are inclusive and name the offending rows", {
  out <- findings(c(-1, 0, 5, 10, 11), minimum = 0, maximum = 10)
  expect_equal(out$row, c(1L, 5L))
  expect_equal(out$keyword, c("minimum", "maximum"))

  full <- dta_check_column_spec(
    "VAL", as_json_schema(num_cell(minimum = 0.5, maximum = 10)), c(0, 11)
  )
  expect_equal(full$message, c("must be >= 0.5", "must be <= 10"))
  expect_equal(full$columnspec, c("0.5", "10"))
  expect_equal(full$data, c("0", "11"))
})

test_that("the same bounds apply to an integer column", {
  schema <- as_json_schema(DTAColumnSpecCell(id = "VAL", type = "SAS Int", minimum = 1, maximum = 3))
  out <- dta_check_column_spec("VAL", schema, c(0L, 1L, 3L, 4L))
  expect_equal(out$row, c(1L, 4L))
  expect_equal(out$keyword, c("minimum", "maximum"))
})

test_that("missing values are never range findings", {
  expect_equal(nrow(findings(c(NA, NaN, 5), minimum = 0, maximum = 10, finite = TRUE)), 0)
})

test_that("an infinite value under finite yields the finite finding alone", {
  out <- findings(c(Inf, -Inf, 1), minimum = 0, maximum = 5, finite = TRUE)
  expect_equal(out$row, c(1L, 2L))
  expect_equal(out$keyword, c("finite", "finite"))
  expect_equal(
    dta_check_column_spec("VAL", as_json_schema(num_cell(finite = TRUE)), Inf)$message,
    "must be finite"
  )
})

test_that("without finite, an infinite value is judged against the bounds", {
  out <- findings(c(Inf, -Inf, 1), minimum = 0, maximum = 5)
  expect_equal(out$row, c(1L, 2L))
  expect_equal(out$keyword, c("maximum", "minimum"))
  expect_equal(nrow(findings(c(Inf, -Inf))), 0)
})

test_that("a character column is never given a range finding", {
  schema <- as_json_schema(num_cell(minimum = 0, maximum = 1, finite = TRUE))
  out <- dta_check_column_spec("VAL", schema, c("-5", "99"))
  expect_false(any(c("minimum", "maximum", "finite") %in% out$keyword))
})

# ---- the same findings on both reading paths ----------------------------------

test_that("minimum, maximum and finite agree between the eager and streaming paths", {
  path <- withr::local_tempfile(fileext = ".tsv")
  writeLines(
    c("ID\tVAL", "a\t5", "b\t-1", "c\t11", "d\tInf", "e\t-Inf", "f\t", "g\t10"),
    path
  )
  run <- function(stream) {
    specs <- DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", nullable = FALSE),
      VAL = DTAColumnSpecCell(
        id = "VAL", type = "SAS Num", nullable = TRUE,
        minimum = 0, maximum = 10, finite = TRUE
      )
    ))
    ds <- DTADataSetTabular(
      id = "d", specs = specs,
      files = list(DTAFileTSV(filename = basename(path)))
    )
    ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
    ds <- check(ds, persist = FALSE, quiet = TRUE)
    m <- messages(ds, as_tibble = FALSE)
    m <- m[m$keyword %in% c("minimum", "maximum", "finite"), , drop = FALSE]
    data.frame(
      row = m$row, column = m$column,
      keyword = m$keyword, stringsAsFactors = FALSE
    )
  }
  eager <- run("never")
  streamed <- run("always")
  key <- function(d) d[order(d$row, d$keyword), , drop = FALSE]

  expected <- data.frame(
    row = c(2L, 3L, 4L, 5L),
    column = "VAL",
    keyword = c("minimum", "maximum", "finite", "finite"),
    stringsAsFactors = FALSE
  )
  expect_equal(key(eager), expected, ignore_attr = TRUE)
  expect_equal(key(streamed), expected, ignore_attr = TRUE)
})

test_that("a cell spec is an instance of the subclass, not only of its parent", {
  cell <- num_cell(minimum = 0)
  expect_true(S7::S7_inherits(cell, DTAColumnSpecCell))
  expect_true(S7::S7_inherits(cell, DTAColumnSpec))
  expect_false(S7::S7_inherits(DTAColumnSpec(id = "X", type = "SAS Num"), DTAColumnSpecCell))
})

test_that("print() names the class and shows the bounds and finiteness", {
  out <- paste(
    testthat::capture_messages(print(num_cell(minimum = 0, maximum = 10, finite = TRUE))),
    collapse = " "
  )
  expect_match(out, "DTAColumnSpecCell", fixed = TRUE)
  expect_match(out, "range      : [0, 10]", fixed = TRUE)
  expect_match(out, "finite     : TRUE", fixed = TRUE)
  open <- paste(testthat::capture_messages(print(num_cell(minimum = 2))), collapse = " ")
  expect_match(open, "[2, Inf]", fixed = TRUE)
  expect_false(grepl("finite", open, fixed = TRUE))
})
