test_that("write_dta validates inputs and exports markdown/docx", {
  dta <- create_example_DTA()

  expect_error(write_dta(list(), file = tempfile(fileext = ".md"), format = "md"), "DTA object")
  expect_error(write_dta(dta, file = tempfile(fileext = ".txt"), format = "txt"), "must be 'docx', 'pdf', or 'md'")

  existing <- tempfile(fileext = ".md")
  file.create(existing)
  on.exit(unlink(existing, force = TRUE), add = TRUE)
  expect_error(write_dta(dta, file = existing, format = "md", overwrite = FALSE), "already exists")

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(out_md))
  md_lines <- readLines(out_md, warn = FALSE)
  expect_true(any(grepl("^# Data Transfer Agreement Metadata", md_lines)))
  expect_true(any(grepl("^## Datasets", md_lines)))

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(out_docx))
})

test_that("write_dta exports docx when metadata has no title (regression)", {
  # A DTA built from a bare dataset carries default metadata with title = NULL.
  # The docx title section must not pass an empty/zero-length keyword to
  # officer::cursor_reach() (which would abort with "invalid 'pattern' argument").
  dta <- DTA(datasets = create_example_DTADataSetTabular(1))
  expect_null(metadata(dta)@title)

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  expect_no_error(
    write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  )
  expect_true(file.exists(out_docx))
})

test_that("write_dta exports docx when title contains regex metacharacters", {
  # officer::cursor_reach() matches the title as a regex, so a title with
  # metacharacters (unbalanced parenthesis) must not abort the export.
  dta <- DTA(
    datasets = create_example_DTADataSetTabular(1),
    metadata = DTAMetaData(title = "Study (Phase 3", version = "1.0")
  )

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  expect_no_error(
    write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  )
  expect_true(file.exists(out_docx))
})

test_that("write_dataset_metadata and alias export metadata documents", {
  ds <- create_example_DTADataSetTabular(2)

  expect_error(
    write_dataset_metadata(list(), file = tempfile(fileext = ".md"), format = "md"),
    "DTADataSet"
  )
  expect_error(
    write_dataset_metadata(ds, file = tempfile(fileext = ".txt"), format = "txt"),
    "must be 'docx', 'pdf', or 'md'"
  )

  existing <- tempfile(fileext = ".md")
  file.create(existing)
  on.exit(unlink(existing, force = TRUE), add = TRUE)
  expect_error(
    write_dataset_metadata(ds, file = existing, format = "md", overwrite = FALSE),
    "already exists"
  )

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(out_md))
  md_lines <- readLines(out_md, warn = FALSE)
  expect_true(any(grepl("^# Dataset Specification", md_lines)))
  expect_true(any(grepl("^### Approval & Signatures", md_lines)))

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(out_docx))

  alias_out <- tempfile(fileext = ".md")
  on.exit(unlink(alias_out, force = TRUE), add = TRUE)
  write_file_specification(ds, file = alias_out, format = "md", overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(alias_out))
})
