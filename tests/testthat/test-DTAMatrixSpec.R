# DTAMatrixSpec and the `matrix:` block reader. Blocks are written as YAML text
# and read the way a specification file is, so the YAML 1.1 traps are real ones.
# The header text above a row label is a property of the row label, not of this
# block: see test-DTADataSetMatrix.R.

mx <- function(text) dta_matrix_spec_from_list(yaml::yaml.load(text))
mx_error <- function(text, class) expect_error(mx(text), class = class)
mx_cells <- function(extra, type = "SAS Num") {
  mx(sprintf("cells: {type: %s, %s}", type, extra))
}

mx_genes <- '
columns:
  label: Sample ID
  pattern: "^S[0-9]{4}$"
  count: { min: 1, max: 384 }
rows:
  key: GENE_ID
  count: { min: 50000 }
cells:
  type: SAS Num
  nullable: No
  range: { min: 0 }
  infinite: false
'
mx_mirna <- '
columns: { pattern: "^hsa-(miR|let)-" }
cells: { type: SAS Int, nullable: No, range: { min: 0 } }
'
mx_geno <- '
columns: { pattern: "^S[0-9]+$" }
cells: { type: SAS Int, nullable: Yes, values: [0, 1, 2] }
'

test_that("the genes x samples example parses to the documented settings", {
  m <- mx(mx_genes)
  expect_equal(m@header@pattern, "^S[0-9]{4}$")
  expect_equal(m@header@label, "Sample ID")
  expect_equal(m@n_columns, c(1, 384))
  expect_equal(m@row_key, "GENE_ID")
  expect_equal(m@n_rows, c(50000, Inf))
  expect_equal(m@cells@minimum, 0)
  expect_null(m@cells@maximum)
  expect_true(m@cells@finite)
  expect_false(m@cells@nullable)
})

test_that("the miRNA example parses, with the defaults filled in", {
  m <- mx(mx_mirna)
  expect_equal(m@header@pattern, "^hsa-(miR|let)-")
  expect_equal(m@n_columns, c(1, Inf))
  expect_equal(m@n_rows, c(1, Inf))
  expect_null(m@row_key)
  expect_equal(m@cells@minimum, 0)
  expect_null(m@cells@finite) # Int: no finiteness
})

test_that("the genotype example parses", {
  m <- mx(mx_geno)
  expect_equal(m@header@pattern, "^S[0-9]+$")
  expect_true(m@cells@nullable)
  expect_equal(m@cells@values, c(0, 1, 2))
})

test_that("the properties of the retired corner are gone from the class", {
  m <- mx(mx_genes)
  expect_false(any(c("corner", "corner_mode") %in% names(S7::props(m))))
  expect_false(any(c("corner", "corner_mode") %in% S7::prop_names(m)))
})

test_that("matrix.columns is optional: cells alone make a spec with the defaults", {
  m <- mx("cells: {type: SAS Num}")
  expect_equal(m@n_columns, c(1, Inf))
  expect_equal(m@n_rows, c(1, Inf))
  expect_null(m@row_key)
  expect_null(m@header@pattern)
  expect_null(m@header@label)
  # an empty block is the same as no block
  expect_identical(mx("columns: {}\ncells: {type: SAS Num}"), m)
})

test_that("a Num cell rejects infinite values unless infinite: true is written", {
  expect_true(mx("cells: {type: SAS Num}")@cells@finite)
  expect_false(mx("cells: {type: SAS Num, infinite: true}")@cells@finite)
  expect_null(mx("cells: {type: SAS Char}")@cells@finite)
})

test_that("a cell nullable that is not written is unset, as for a column", {
  expect_null(mx("cells: {type: SAS Num}")@cells@nullable)
})

test_that("unknown keys abort naming the dotted path", {
  err <- expect_error(
    mx("cells: {type: SAS Num, rnage: {min: 0}}"),
    class = "dta_matrix_field_unknown"
  )
  expect_match(conditionMessage(err), "matrix.cells.rnage", fixed = TRUE)
  err <- expect_error(
    mx("columns: {colour: red}\ncells: {type: SAS Num}"),
    class = "dta_matrix_field_unknown"
  )
  expect_match(conditionMessage(err), "matrix.columns.colour", fixed = TRUE)
  mx_error("rows: {keys: a}\ncells: {type: SAS Num}", "dta_matrix_field_unknown")
  mx_error("cells: {type: SAS Num}\nfooter: 1", "dta_matrix_field_unknown")
  mx_error("cells: {type: SAS Num, range: {mn: 0}}", "dta_matrix_field_unknown")
})

test_that("the retired matrix.header block and its corner key are unknown fields", {
  # `header` is no longer a block of `matrix:`; the header text lives on the row label.
  mx_error("header: {pattern: x}\ncells: {type: SAS Num}", "dta_matrix_field_unknown")
  mx_error("header: {corner: named}\ncells: {type: SAS Num}", "dta_matrix_field_unknown")
  # and `corner` is not a key of matrix.columns
  err <- expect_error(
    mx("columns: {corner: named}\ncells: {type: SAS Num}"),
    class = "dta_matrix_field_unknown"
  )
  expect_match(conditionMessage(err), "matrix.columns.corner", fixed = TRUE)
})

test_that("missing required keys abort by class: matrix.cells only", {
  mx_error("{}", "dta_matrix_field_missing")
  mx_error("columns: {pattern: x}", "dta_matrix_field_missing")
  mx_error("rows: {count: 3}", "dta_matrix_field_missing")
  mx_error("cells: {nullable: No}", "dta_matrix_field_missing")
})

test_that("matrix.columns takes exactly the documented keys", {
  m <- mx("columns: {label: L, description: D, pattern: '^x', length: 4, count: 2}\ncells: {type: SAS Num}")
  expect_equal(m@header@label, "L")
  expect_equal(m@header@description, "D")
  expect_equal(m@header@pattern, "^x")
  expect_equal(m@n_columns, c(2, 2))
  mx_error("columns: {id: x}\ncells: {type: SAS Num}", "dta_matrix_field_unknown")
  mx_error("columns: {type: SAS Num}\ncells: {type: SAS Num}", "dta_matrix_field_unknown")
})

test_that("column labels values must be quoted text", {
  mx_error("columns: {values: [007, 008]}\ncells: {type: SAS Num}", "dta_matrix_columns_values_text")
  mx_error("columns: {values: [Y, N]}\ncells: {type: SAS Num}", "dta_matrix_columns_values_text")
  m <- mx("columns: {values: ['007', '008']}\ncells: {type: SAS Num}")
  expect_equal(m@header@values, c("007", "008"))
  # the message names the new path
  err <- expect_error(
    mx("columns: {values: [007]}\ncells: {type: SAS Num}"),
    class = "dta_matrix_columns_values_text"
  )
  expect_match(conditionMessage(err), "matrix.columns.values", fixed = TRUE)
})

test_that("counts: a scalar is exact, a mapping defaults the missing side", {
  head <- function(count) {
    mx(sprintf("columns: {count: %s}\ncells: {type: SAS Num}", count))
  }
  expect_equal(head("96")@n_columns, c(96, 96))
  expect_equal(head("{min: 3}")@n_columns, c(3, Inf))
  expect_equal(head("{max: 5}")@n_columns, c(1, 5))
  rows <- function(count) {
    mx(sprintf("rows: {count: %s}\ncells: {type: SAS Num}", count))
  }
  expect_equal(rows("0")@n_rows, c(0, 0))
  expect_equal(rows("{max: 9}")@n_rows, c(1, 9))
  expect_equal(rows("{min: 0}")@n_rows, c(0, Inf))
})

test_that("invalid counts abort with the count class", {
  for (count in c("0", "-1", "1.5", "{min: 5, max: 2}", "{min: 0}", "abc", "{max: 0}")) {
    expect_error(
      mx(sprintf("columns: {count: %s}\ncells: {type: SAS Num}", count)),
      class = "dta_matrix_count_invalid",
      info = count
    )
  }
  for (count in c("-1", "1.5", "{min: 5, max: 2}", "{max: 0}", "abc")) {
    expect_error(
      mx(sprintf("rows: {count: %s}\ncells: {type: SAS Num}", count)),
      class = "dta_matrix_count_invalid",
      info = count
    )
  }
})

test_that("the count message names matrix.columns.count", {
  err <- expect_error(
    mx("columns: {count: 0}\ncells: {type: SAS Num}"),
    class = "dta_matrix_count_invalid"
  )
  expect_match(conditionMessage(err), "matrix.columns.count", fixed = TRUE)
})

test_that("range and infinite traps abort by class", {
  # YAML 1.1 reads 1e3 as text
  expect_error(mx_cells("range: {max: 1e3}"), class = "dta_matrix_number_text")
  expect_error(mx_cells("range: {min: '5'}"), class = "dta_matrix_number_text")
  expect_error(mx_cells("range: {min: abc}"), class = "dta_cell_range_invalid")
  expect_error(mx_cells("range: {min: 5, max: 1}"), class = "dta_cell_range_invalid")
  expect_no_error(mx_cells("range: {min: 2, max: 2}"))
  expect_error(mx_cells("range: {min: 0}", "SAS Char"), class = "dta_cell_range_type")
  expect_error(mx_cells("infinite: true", "SAS Int"), class = "dta_cell_finite_type")
  expect_error(mx_cells("infinite: false", "SAS Char"), class = "dta_cell_finite_type")
  expect_error(mx_cells("infinite: maybe"), class = "dta_cell_finite_type")
  expect_equal(mx_cells("range: {min: 1.0e+3}")@cells@minimum, 1000)
})

test_that("the class validator refuses a cell type other than Num, Int, Char", {
  base <- mx("cells: {type: SAS Num}")
  expect_error(
    S7::`prop<-`(base, "cells", value = DTAColumnSpecCell(type = "SAS Date")),
    class = "dta_matrix_cells_type"
  )
  expect_error(S7::`prop<-`(base, "n_rows", value = c(-1, 3)), class = "dta_matrix_count_invalid")
  expect_error(S7::`prop<-`(base, "n_columns", value = c(0, 3)), class = "dta_matrix_count_invalid")
})

test_that("the class validator refuses a column-label spec that is not the header Char spec", {
  base <- mx("cells: {type: SAS Num}")
  expect_error(
    S7::`prop<-`(base, "header", value = DTAColumnSpec(id = "other", type = "SAS Char", nullable = FALSE)),
    class = "dta_matrix_header_invalid"
  )
})

test_that("as.list() writes the YAML shape and omits unset fields", {
  out <- as.list(mx(mx_genes))
  expect_named(out, c("columns", "rows", "cells"))
  expect_equal(out$columns$count, list(min = 1, max = 384))
  expect_equal(out$columns$label, "Sample ID")
  expect_false("corner" %in% names(out$columns))
  expect_equal(out$rows, list(key = "GENE_ID", count = list(min = 50000)))
  expect_equal(out$cells$range, list(min = 0))
  expect_false(out$cells$infinite)
  expect_null(out$cells$id)
  expect_false("label" %in% names(out$cells))

  plain <- as.list(mx(mx_mirna))
  expect_equal(plain$columns$pattern, "^hsa-(miR|let)-")
  expect_null(plain$rows$key)
})

test_that("as.list() round-trips every fixture, including exact counts", {
  fixtures <- c(
    mx_genes, mx_mirna, mx_geno,
    "cells: {type: SAS Num}",
    "columns: {count: 96}\nrows: {count: 0}\ncells: {type: SAS Num, range: {min: -1, max: 1}, infinite: true}",
    "columns: {values: ['a', 'b'], count: 2, label: L, description: D}\ncells: {type: SAS Char, length: 8, label: C, description: E}",
    "rows: {key: [A, B], count: {min: 2, max: 4}}\ncells: {type: SAS Char, pattern: '^x'}"
  )
  for (text in fixtures) {
    spec <- mx(text)
    expect_identical(dta_matrix_spec_from_list(as.list(spec)), spec, info = text)
    # and through real YAML text, as a saved specification does
    again <- dta_matrix_spec_from_list(yaml::yaml.load(yaml::as.yaml(as.list(spec))))
    expect_identical(again, spec, info = text)
  }
  exact <- as.list(mx("columns: {count: 7}\ncells: {type: SAS Num}"))
  expect_equal(exact$columns$count, 7)
})

test_that("print() names the settings, in words, and has no corner line", {
  out <- paste(testthat::capture_messages(print(mx(mx_genes))), collapse = " ")
  expect_match(out, "columns: 1 to 384", fixed = TRUE)
  expect_match(out, "at least 50000", fixed = TRUE)
  expect_match(out, "GENE_ID", fixed = TRUE)
  expect_false(grepl("corner", out, ignore.case = TRUE))
})

test_that("rows.key entries are validated on the raw list", {
  key <- function(k) mx(sprintf("rows: {key: %s}\ncells: {type: SAS Num}", k))
  for (k in c("[a, ~]", "[a, 1]", "1", "[]", "['']", "[a, '']")) {
    expect_error(key(k), class = "dta_matrix_key_invalid", info = k)
  }
  expect_equal(key("[a, b]")@row_key, c("a", "b"))
  expect_equal(key("a")@row_key, "a")
})

test_that("counts written as 1e3 abort as text numbers", {
  head <- function(count) mx(sprintf("columns: {count: %s}\ncells: {type: SAS Num}", count))
  rows <- function(count) mx(sprintf("rows: {count: %s}\ncells: {type: SAS Num}", count))
  for (count in c("5e4", "{min: 5e4}", "{max: 5e4}")) {
    expect_error(head(count), class = "dta_matrix_number_text", info = count)
    expect_error(rows(count), class = "dta_matrix_number_text", info = count)
  }
  expect_equal(rows("5.0e+4")@n_rows, c(5e4, 5e4))
})

test_that("empty ranges are absent; infinite bounds and empty column-label values abort", {
  expect_null(mx_cells("range: []")@cells@minimum)
  expect_null(mx_cells("range: {}")@cells@maximum)
  expect_error(mx_cells("range: {min: .inf}"), class = "dta_matrix_range_invalid")
  expect_error(mx_cells("range: {max: -.inf}"), class = "dta_matrix_range_invalid")
  expect_error(
    mx("columns: {values: []}\ncells: {type: SAS Num}"),
    class = "dta_matrix_columns_values_text"
  )
})
