# The contracts of the small exported accessors: what each returns, and how
# it fails. Each was previously reached only as a side check of some larger
# test, never for its own behaviour.

test_that("rules() returns the collection's rules, and NULL when it has none", {
  cols <- list(A = DTAColumnSpec(id = "A", type = "SAS Char"))
  unique_a <- DTARuleColUnique(id = "unique_a", columns = "A")
  with_rules <- DTAColumnSpecCollection(columns = cols, rules = list(unique_a))
  expect_length(rules(with_rules), 1)
  expect_identical(rules(with_rules)[[1]]@id, "unique_a")
  expect_identical(rules(with_rules), with_rules@rules)

  # An empty list is normalised to NULL by the constructor, so "no rules" has
  # one representation, not two.
  expect_null(rules(DTAColumnSpecCollection(columns = cols)))
  expect_null(rules(DTAColumnSpecCollection(columns = cols, rules = list())))
})

test_that("rules() of a dataset is the rules of its specification", {
  ds <- create_example_DTADataSetTabular(2)
  expect_identical(rules(ds), rules(specs(ds)))
})

test_that("specs() returns the dataset's own specification object", {
  ds <- create_example_DTADataSetTabular(2)
  expect_s3_class(specs(ds), "DTAtools::DTAColumnSpecCollection")
  expect_identical(specs(ds), ds@specs)
  expect_identical(names(specs(ds)@columns), names(columns(ds)))
})

test_that("get_table() returns the same table by name and by position", {
  ds <- create_example_DTADataSetTabular(2)
  by_name <- get_table(ds, "tab1")
  by_index <- get_table(ds, 1)
  expect_identical(as.data.frame(by_name), as.data.frame(by_index))
  expect_identical(as.data.frame(get_table(ds)), as.data.frame(by_index))
  expect_equal(nrow(by_name), 3)
  expect_identical(names(by_name), c("STUDYID", "SUBJID", "VISIT", "AGE"))
})

test_that("get_table() rejects a table that is not there, by package-authored message", {
  ds <- create_example_DTADataSetTabular(2)
  expect_error(get_table(ds, "nope"), "No table named")
  expect_error(get_table(ds, 2), "out of bounds")
  expect_error(get_table(ds, 0), "out of bounds")
  expect_error(get_table(ds, 1.5), "whole number")
  expect_error(get_table(ds, NA_real_), "whole number")
  expect_error(get_table(ds, c("tab1", "tab1")), "single table name")
  expect_error(get_table(ds, NA_character_), "single table name")
  expect_error(get_table(ds, TRUE), "character \\(name\\) or numeric")

  empty <- create_example_DTADataSetTabular(1)
  expect_error(get_table(empty, 1), "No tables found")
})

test_that("column_preview() lists every id up to n, and abbreviates past it", {
  make <- function(k) {
    ids <- sprintf("C%02d", seq_len(k))
    DTAColumnSpecCollection(columns = stats::setNames(
      lapply(ids, function(id) DTAColumnSpec(id = id, type = "SAS Char")), ids
    ))
  }
  expect_identical(column_preview(make(3)), "C01, C02, C03")
  expect_identical(column_preview(make(8)), paste(sprintf("C%02d", 1:8), collapse = ", "))
  expect_identical(
    column_preview(make(9)),
    paste(c(sprintf("C%02d", 1:7), "...", "C09"), collapse = ", ")
  )
  expect_identical(column_preview(make(5), n = 3), "C01, C02, ..., C05")
  expect_identical(column_preview(make(2), n = 2), "C01, C02")
})

test_that("column_preview() reports an empty specification as not set and rejects a bad n", {
  expect_identical(column_preview(DTAColumnSpecCollection(columns = list())), "not set")
  x <- create_example_DTAColumnSpecCollection()
  for (bad in list(1, 0, 2.5, NA_real_, c(3, 4), "8")) {
    expect_error(column_preview(x, n = bad), "at least 2", info = deparse(bad))
  }
})

test_that("write_file_specification() writes what write_dataset_metadata() writes", {
  ds <- create_example_DTADataSetTabular(2)
  via_alias <- withr::local_tempfile(fileext = ".md")
  direct <- withr::local_tempfile(fileext = ".md")
  write_file_specification(ds, file = via_alias, format = "md", quiet = TRUE)
  write_dataset_metadata(ds, file = direct, format = "md", quiet = TRUE)

  # Everything but the generation timestamp on the last line is identical.
  strip_stamp <- function(path) grep("^\\*Generated on:", readLines(path, warn = FALSE), invert = TRUE, value = TRUE)
  alias_lines <- strip_stamp(via_alias)
  expect_identical(alias_lines, strip_stamp(direct))
  expect_equal(alias_lines[1], "# Data Transfer Agreement")
  expect_true(any(grepl("^## ", alias_lines)))
  expect_true(any(grepl("STUDYID", alias_lines, fixed = TRUE)))

  # It refuses to overwrite unless told to, as the function it aliases does.
  expect_error(write_file_specification(ds, file = via_alias, format = "md", quiet = TRUE), "already exists")
  expect_no_error(write_file_specification(ds, file = via_alias, format = "md", overwrite = TRUE, quiet = TRUE))
})
