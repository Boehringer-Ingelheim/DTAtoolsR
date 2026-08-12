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

  # A bare expect_error() accepts any error, including a typo in the test.
  # Assert the error's kind, not its text: export_column_value_table() has no
  # input validation at all, so this is a raw base R error whose message is
  # translated (German on this machine) and cannot be matched portably.
  #
  # KNOWN DEFECT, pinned: unlike export_specs_table(), which aborts with
  # "must be a DTAColumnSpecCollection", this leaks an internal error. Once a
  # cli_abort() is added this SHOULD fail -- switch to
  # expect_error(..., class = "rlang_error").
  err <- expect_error(
    export_column_value_table(list(), file = tempfile(fileext = ".docx"), id = "VISIT")
  )
  expect_false(inherits(err, "rlang_error"))

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  values_df <- export_column_value_table(specs, file = out, id = "VISIT", quiet = TRUE)
  expect_true(file.exists(out))
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
