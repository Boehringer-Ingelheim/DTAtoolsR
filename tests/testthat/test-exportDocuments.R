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

# Read a .docx file's word/document.xml as a single string (test helper).
.read_docx_body_xml <- function(path) {
  ex <- file.path(tempdir(), paste0("docxbody_", as.integer(Sys.time()), "_", sample.int(1e6, 1)))
  dir.create(ex, showWarnings = FALSE)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, files = "word/document.xml", exdir = ex)
  paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE), collapse = "")
}

test_that("bundled numbered template ships a 'heading 4' style", {
  tp <- system.file("extdata", "templates", "dta_numbered_template.docx", package = "DTAtools")
  expect_true(nzchar(tp) && file.exists(tp))

  si <- officer::styles_info(officer::read_docx(path = tp))
  expect_true("heading 4" %in% si$style_name)
})

test_that("write_dta docx uses numbered heading hierarchy incl. heading 4", {
  dta <- create_example_DTA()
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  body <- .read_docx_body_xml(out_docx)
  # Titre1..Titre4 are the styleIds for "heading 1".."heading 4"; their presence
  # proves the Datasets -> dataset -> Files / Dataset Specifications hierarchy is
  # rendered with the multilevel-numbered heading styles (not bold subheadings).
  for (sid in c("Titre1", "Titre2", "Titre3", "Titre4")) {
    expect_true(grepl(paste0('w:pStyle w:val="', sid, '"'), body, fixed = TRUE))
  }
  expect_true(grepl("Data Transfer Agreement", body, fixed = TRUE))
  expect_true(grepl("Dataset Specifications", body, fixed = TRUE))
})

test_that("write_dta docx embeds a small-font YAML section only when requested", {
  dta <- create_example_DTA()
  yaml_text <- "datasets:\n  clinical_data:\n    columns:\n      SUBJID:\n        type: string"

  with_yaml <- tempfile(fileext = ".docx")
  on.exit(unlink(with_yaml, force = TRUE), add = TRUE)
  write_dta(dta,
    file = with_yaml, format = "docx", overwrite = TRUE, quiet = TRUE,
    include_yaml = TRUE, yaml_text = yaml_text
  )
  body_yes <- .read_docx_body_xml(with_yaml)
  expect_true(grepl("Embedded Specification (YAML)", body_yes, fixed = TRUE))
  expect_true(grepl("SUBJID", body_yes, fixed = TRUE))
  # 6pt font is stored as half-points (w:sz w:val="12").
  expect_true(grepl('w:sz w:val="12"', body_yes, fixed = TRUE))

  without_yaml <- tempfile(fileext = ".docx")
  on.exit(unlink(without_yaml, force = TRUE), add = TRUE)
  write_dta(dta, file = without_yaml, format = "docx", overwrite = TRUE, quiet = TRUE)
  body_no <- .read_docx_body_xml(without_yaml)
  expect_false(grepl("Embedded Specification (YAML)", body_no, fixed = TRUE))
})
