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

  expect_error(
    export_column_value_table(list(), file = tempfile(fileext = ".docx"), id = "VISIT")
  )

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  values_df <- export_column_value_table(specs, file = out, id = "VISIT", quiet = TRUE)
  expect_true(file.exists(out))
  expect_s3_class(values_df, "data.frame")
  expect_true("VISIT" %in% names(values_df))
  expect_equal(as.character(values_df$VISIT), c("V01", "EOT"))
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
  expect_true(nzchar(res$md5sum))
  expect_true(file.exists(metadata_file))
})
