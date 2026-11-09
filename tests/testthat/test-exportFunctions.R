test_that("export_specs_table validates inputs and writes DOCX output", {
  specs <- create_example_DTAColumnSpecCollection(1)

  expect_error(
    export_specs_table(list(), file = tempfile(fileext = ".docx"), overwrite = TRUE),
    "DTAColumnSpecCollection"
  )

  empty_specs <- DTAColumnSpecCollection(columns = list())
  expect_error(
    export_specs_table(empty_specs, file = tempfile(fileext = ".docx"), overwrite = TRUE),
    "no columns"
  )

  existing <- tempfile(fileext = ".docx")
  file.create(existing)
  on.exit(unlink(existing, force = TRUE), add = TRUE)
  expect_error(export_specs_table(specs, file = existing, overwrite = FALSE), "File exists")

  out_default <- tempfile(fileext = ".docx")
  on.exit(unlink(out_default, force = TRUE), add = TRUE)
  ft_default <- export_specs_table(specs, file = out_default, overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(out_default))
  # The saved document must carry the table: every column header and the id of
  # every specified column, not merely exist on disk.
  default_text <- .docx_text(out_default)
  for (hdr in names(ft_default$body$dataset)) {
    expect_match(default_text, hdr, fixed = TRUE)
  }
  for (id in names(specs@columns)) {
    expect_match(default_text, id, fixed = TRUE)
  }
  expect_s3_class(ft_default, "flextable")
  # file.exists() + class say nothing about the table that was produced.
  expect_equal(
    names(ft_default$body$dataset),
    c(
      "Variable Name",
      "Variable Label",
      "Type",
      "Length",
      "Format",
      "Nullable",
      "Description"
    )
  )

  out_compact <- tempfile(fileext = ".docx")
  on.exit(unlink(out_compact, force = TRUE), add = TRUE)
  ft_compact <- export_specs_table(
    specs,
    file = out_compact,
    overwrite = TRUE,
    colnames = c(
      "Variable Name",
      "Variable Label",
      "Type",
      "Format",
      "Nullable",
      "Description"
    ),
    quiet = TRUE
  )
  expect_true(file.exists(out_compact))
  # The excluded column must be absent from the saved document, too.
  compact_text <- .docx_text(out_compact)
  expect_match(compact_text, "Variable Label", fixed = TRUE)
  expect_no_match(compact_text, "Length", fixed = TRUE)
  expect_s3_class(ft_compact, "flextable")

  # The caller excluded "Length", so the table must carry exactly the six
  # requested columns, in the requested order -- the j = 1..6 widths applied by
  # this branch only line up with a six-column table.
  expect_equal(ncol(ft_compact$body$dataset), 6)
  expect_equal(
    names(ft_compact$body$dataset),
    c(
      "Variable Name",
      "Variable Label",
      "Type",
      "Format",
      "Nullable",
      "Description"
    )
  )
  expect_false("Length" %in% names(ft_compact$body$dataset))
})

test_that("export_specs_table aborts on an unsupported colnames vector", {
  specs <- create_example_DTAColumnSpecCollection(1)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  # Neither of the two supported layouts: the abort branch at the end of the
  # if/else chain. Matched on package-authored text, which is not translated.
  expect_error(
    export_specs_table(specs, file = out, overwrite = TRUE, colnames = c("A", "B"), quiet = TRUE),
    "colnames not supported"
  )
  # The abort happens before any file is written.
  expect_false(file.exists(out))

  # The right names in the wrong order are equally unsupported.
  expect_error(
    export_specs_table(
      specs,
      file = out,
      overwrite = TRUE,
      colnames = c(
        "Variable Label",
        "Variable Name",
        "Type",
        "Format",
        "Nullable",
        "Description"
      ),
      quiet = TRUE
    ),
    "colnames not supported"
  )
})

test_that("export_column_value_table writes DOCX and returns value table", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      VISIT = DTAColumnSpec(
        id = "VISIT",
        type = "SAS Char",
        nullable = FALSE,
        values = c("V01", "EOT")
      )
    )
  )

  # Like export_specs_table(), a non-collection argument aborts with the
  # package's own cli error naming the expected class.
  expect_error(
    export_column_value_table(list(), file = tempfile(fileext = ".docx"), id = "VISIT"),
    regexp = "must be a DTAColumnSpecCollection",
    class = "rlang_error"
  )

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  values_df <- export_column_value_table(specs, file = out, id = "VISIT", quiet = TRUE)
  expect_true(file.exists(out))
  # The saved document lists the allowed values under the column id.
  values_text <- .docx_text(out)
  expect_match(values_text, "VISIT", fixed = TRUE)
  expect_match(values_text, "V01", fixed = TRUE)
  expect_match(values_text, "EOT", fixed = TRUE)
  expect_s3_class(values_df, "data.frame")
  expect_true("VISIT" %in% names(values_df))
  expect_equal(as.character(values_df$VISIT), c("V01", "EOT"))
})

test_that("export_column_value_table() aborts with a clear cli error for a column with no values", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      STUDYID = DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE)
    )
  )

  # Before the fix this raised base R's raw
  # "'names' attribute [1] must be the same length as the vector [0]" out of
  # `colnames(df) <- id`, not a cli_abort naming the column.
  expect_error(
    export_column_value_table(specs, file = tempfile(fileext = ".docx"), id = "STUDYID"),
    regexp = "STUDYID",
    class = "rlang_error"
  )

  # A column id absent from the collection altogether hit the same crash
  # (`specs@values` on NULL); it gets the same clear treatment.
  expect_error(
    export_column_value_table(specs, file = tempfile(fileext = ".docx"), id = "NOT_A_COLUMN"),
    regexp = "NOT_A_COLUMN",
    class = "rlang_error"
  )
})

test_that("write_metadata validates file input and creates optional sidecar", {
  missing_file <- file.path(tempdir(), "missing-export-metadata.csv")
  expect_error(write_metadata(missing_file, data.frame(A = 1), write_to_file = FALSE), "does not exist")

  csv_file <- tempfile(fileext = ".csv")
  metadata_file <- paste0(csv_file, ".md5")
  on.exit(unlink(c(csv_file, metadata_file), force = TRUE), add = TRUE)

  tab <- data.frame(A = 1:3, B = c("x", "y", "z"), stringsAsFactors = FALSE)
  utils::write.csv(tab, csv_file, row.names = FALSE)

  res <- write_metadata(csv_file, tab, write_to_file = TRUE, quiet = TRUE)
  expect_true(is.list(res))
  expect_true(all(c("md5sum", "n_rows", "n_cols") %in% names(res)))
  expect_equal(res$n_rows, 3L)
  expect_equal(res$n_cols, 2L)
  expect_equal(res$md5sum, unname(tools::md5sum(csv_file)))
  expect_true(file.exists(metadata_file))

  # The sidecar's contents were never read, so a regression writing an empty
  # file, the wrong checksum, or swapped row/column labels would have passed.
  expect_equal(
    readLines(metadata_file),
    c(
      paste0("md5sum: ", unname(tools::md5sum(csv_file))),
      "Number of Columns: 2",
      "Number of Rows: 3"
    )
  )
})

test_that("write_metadata does not create a sidecar when write_to_file = FALSE", {
  csv_file <- tempfile(fileext = ".csv")
  metadata_file <- paste0(csv_file, ".md5")
  on.exit(unlink(c(csv_file, metadata_file), force = TRUE), add = TRUE)

  tab <- data.frame(A = 1:3, B = c("x", "y", "z"), stringsAsFactors = FALSE)
  utils::write.csv(tab, csv_file, row.names = FALSE)

  res <- write_metadata(csv_file, tab, write_to_file = FALSE, quiet = TRUE)

  expect_equal(res$n_rows, 3L)
  expect_false(file.exists(metadata_file))
})

test_that("write_table_to_file() returns md5sum as a bare checksum plus n_rows and n_cols", {
  ds <- create_example_DTADataSetTabular(2)
  out <- tempfile(fileext = ".csv")
  on.exit(unlink(c(out, paste0(out, ".md5")), force = TRUE), add = TRUE)

  res <- write_table_to_file(
    ds,
    table = "tab1", filename = out, sep = ",", arrange_by = NULL, quiet = TRUE
  )

  expect_type(res$md5sum, "character")
  expect_length(res$md5sum, 1L)
  expect_identical(res$md5sum, unname(tools::md5sum(out)))

  written <- utils::read.csv(out, colClasses = "character")
  expect_identical(res$n_rows, nrow(written))
  expect_identical(res$n_cols, ncol(written))

  # the documented idiom runs in this branch
  expect_true(!is.na(res$md5sum))
})

test_that("write_table_to_file() with get_md5sum = FALSE returns NA_character_ and still the dimensions", {
  ds <- create_example_DTADataSetTabular(2)
  out <- tempfile(fileext = ".csv")
  on.exit(unlink(c(out, paste0(out, ".md5")), force = TRUE), add = TRUE)

  res <- write_table_to_file(
    ds,
    table = "tab1", filename = out, sep = ",", arrange_by = NULL,
    get_md5sum = FALSE, quiet = TRUE
  )

  expect_identical(res$md5sum, NA_character_)
  expect_false(isTRUE(!is.na(res$md5sum)))
  written <- utils::read.csv(out, colClasses = "character")
  expect_identical(res$n_rows, nrow(written))
  expect_identical(res$n_cols, ncol(written))
  expect_false(file.exists(paste0(out, ".md5")))
})

test_that("export_column_value_table() refuses to overwrite unless asked", {
  specs <- DTAColumnSpecCollection(
    columns = list(VISIT = DTAColumnSpec(id = "VISIT", type = "SAS Char", nullable = FALSE, values = c("V01", "EOT")))
  )
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  writeLines("precious", out)

  expect_error(
    export_column_value_table(specs, file = out, id = "VISIT", quiet = TRUE),
    "File exists",
    class = "rlang_error"
  )
  expect_identical(readLines(out, warn = FALSE), "precious")

  export_column_value_table(specs, file = out, id = "VISIT", quiet = TRUE, overwrite = TRUE)
  expect_true(any(officer::docx_summary(officer::read_docx(out))$text == "EOT"))
})

test_that("export_specs_table() aborts with the cli error for empty colnames", {
  specs <- create_example_DTAColumnSpecCollection()
  out <- tempfile(fileext = ".docx")
  expect_error(
    export_specs_table(specs, file = out, colnames = character(0), quiet = TRUE),
    "colnames not supported",
    class = "rlang_error"
  )
  expect_false(file.exists(out))
})

test_that("export_specs_table() and export_column_value_table() leave an existing file intact when the writer fails", {
  specs <- create_example_DTAColumnSpecCollection()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  writeLines("precious", out)
  testthat::local_mocked_bindings(
    save_as_docx = function(x, path, ...) {
      writeLines("partial", path)
      stop("disk full")
    },
    .package = "flextable"
  )
  expect_error(export_specs_table(specs, file = out, overwrite = TRUE, quiet = TRUE), "disk full")
  expect_identical(readLines(out, warn = FALSE), "precious")
  expect_error(
    export_column_value_table(specs, file = out, id = "STUDYID", overwrite = TRUE, quiet = TRUE),
    "disk full"
  )
  expect_identical(readLines(out, warn = FALSE), "precious")
})
