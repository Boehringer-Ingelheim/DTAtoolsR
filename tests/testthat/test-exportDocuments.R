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

  # Not merely "no error": the empty title must fall back to the literal
  # "Data Transfer Agreement" (R/documentBuilders.R:13) and still be rendered.
  body <- .read_docx_body_xml(out_docx)
  expect_match(body, "<w:t[^>]*>Data Transfer Agreement</w:t>")

  # The body-XML check above is necessary but not sufficient: the document also
  # carries a level-1 section heading with exactly that text, so it would pass
  # even if the title paragraph were dropped. The title is the first non-empty
  # paragraph of the document, so pin that instead.
  paragraphs <- .docx_paragraphs(out_docx)
  expect_equal(Filter(nzchar, paragraphs)[1], "Data Transfer Agreement")
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

  # The metacharacters must survive verbatim into the document text, and the
  # supplied title must not be silently replaced by the empty-title fallback.
  body <- .read_docx_body_xml(out_docx)
  expect_match(body, "<w:t[^>]*>Study \\(Phase 3</w:t>")

  paragraphs <- .docx_paragraphs(out_docx)
  expect_equal(Filter(nzchar, paragraphs)[1], "Study (Phase 3")
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

# .read_docx_body_xml() and .docx_paragraphs() live in helper-docx.R.

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

  # Assert on style_name, not on the underlying styleId. The checked-in template
  # inst/extdata/templates/dta_numbered_template.docx was saved by a French Word
  # install, so its ids happen to be "Titre1".."Titre4"; regenerating it on an
  # English install would yield "Heading1".. with no functional change.
  # .add_heading() (R/documentBuilders.R:61-64) requests "heading <level>", and
  # that is the locale-independent contract worth pinning.
  summary <- officer::docx_summary(officer::read_docx(out_docx))
  for (nm in c("heading 1", "heading 2", "heading 3", "heading 4")) {
    expect_true(nm %in% summary$style_name)
  }

  # The Datasets -> <dataset> -> Files / Dataset Specifications hierarchy must
  # be carried by those heading levels, not by bold body paragraphs.
  headings <- summary[summary$style_name %in% paste("heading", 1:4), ]
  expect_equal(headings$style_name[headings$text == "Datasets"], "heading 2")
  expect_equal(unique(headings$style_name[headings$text == "demographics"]), "heading 3")
  expect_equal(unique(headings$style_name[headings$text == "Dataset Specifications"]), "heading 4")
  expect_true("Data Transfer Agreement" %in% summary$text)
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
  # 6pt font is stored as half-points (w:sz w:val="12").
  expect_true(grepl('w:sz w:val="12"', body_yes, fixed = TRUE))

  # The whole reason .add_embedded_yaml_section() loops line by line
  # (R/documentBuilders.R:99-105) is to convert each leading space to U+00A0 so
  # Word does not collapse the YAML indentation. Assert the converted
  # indentation, not just that "SUBJID" appears somewhere in the document.
  nbsp <- "\u00a0"
  expect_true(grepl(paste0(strrep(nbsp, 2), "clinical_data:"), body_yes, fixed = TRUE))
  expect_true(grepl(paste0(strrep(nbsp, 6), "SUBJID:"), body_yes, fixed = TRUE))
  expect_true(grepl(paste0(strrep(nbsp, 8), "type: string"), body_yes, fixed = TRUE))
  # The indented lines must be whole paragraphs of their own, so the small-font
  # runs really are the YAML block.
  paragraphs_yes <- .docx_paragraphs(with_yaml)
  expect_true(paste0(strrep(nbsp, 6), "SUBJID:") %in% paragraphs_yes)

  without_yaml <- tempfile(fileext = ".docx")
  on.exit(unlink(without_yaml, force = TRUE), add = TRUE)
  write_dta(dta, file = without_yaml, format = "docx", overwrite = TRUE, quiet = TRUE)
  body_no <- .read_docx_body_xml(without_yaml)
  expect_false(grepl("Embedded Specification (YAML)", body_no, fixed = TRUE))
  expect_false(grepl(paste0(strrep(nbsp, 6), "SUBJID:"), body_no, fixed = TRUE))
})

test_that("write_dta with include_yaml = TRUE but no yaml_text drops the section", {
  dta <- create_example_DTA()
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)

  # KNOWN DEFECT, pinned rather than endorsed: `include_yaml = TRUE` with
  # `yaml_text = NULL` is discarded without any diagnostic -- the guard at
  # R/exportDocuments.R:246 requires both, so the caller's explicit request for
  # an embedded specification silently produces a document without one. When
  # R/exportDocuments.R:246 is fixed this SHOULD fail -- change it to
  # expect_warning(write_dta(...), "yaml_text") and keep the expect_false below.
  expect_no_condition(
    write_dta(
      dta,
      file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE,
      include_yaml = TRUE, yaml_text = NULL
    )
  )
  expect_true(file.exists(out_docx))
  body <- .read_docx_body_xml(out_docx)
  expect_false(grepl("Embedded Specification (YAML)", body, fixed = TRUE))
})
