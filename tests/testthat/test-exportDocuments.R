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

test_that("write_dta docx places Process Information before Datasets", {
  # create_example_DTA() carries no transmission/error-handling/corrections
  # metadata, so it produces no Process Information chapter at all. The rich
  # clinical fixture is the one that exercises the ordering.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  summary <- officer::docx_summary(officer::read_docx(out_docx))
  heading2 <- summary[summary$style_name == "heading 2", c("doc_index", "text")]
  idx_process <- heading2$doc_index[which(heading2$text == "Process Information")[1]]
  idx_datasets <- heading2$doc_index[which(heading2$text == "Datasets")[1]]
  expect_true(length(idx_process) == 1 && length(idx_datasets) == 1)
  expect_true(idx_process < idx_datasets)
})

test_that("write_dta docx puts ONLY the specs and rules tables in landscape", {
  # A bare list of section orientations is not enough: officer ends the section
  # for content added BEFORE the call, so putting the break on the wrong side
  # produces the identical orientation sequence with the wrong pages rotated.
  # Every assertion below is therefore text-anchored.
  dta <- create_example_DTA()
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  # The two table headings and the tables themselves are landscape.
  expect_identical(.docx_orientation_of(out_docx, "Column Specifications"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Validation Rules"), "landscape")
  # "Variable Name" is a header cell of the column-spec flextable, so this pins
  # the table and not merely its heading.
  expect_identical(.docx_orientation_of(out_docx, "Variable Name"), "landscape")

  # Everything around them stays portrait: the title page, the metadata
  # narrative, the dataset heading, and the trailing footer.
  expect_identical(.docx_orientation_of(out_docx, "Data Transfer Agreement"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "Document Information"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "Datasets"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "Dataset Specifications"), "portrait")

  blocks <- .docx_blocks_with_orientation(out_docx)
  expect_identical(blocks$orientation[nrow(blocks)], "portrait")

  # A landscape run must not swallow the rest of the document.
  expect_true(any(blocks$orientation == "landscape"))
  expect_true(sum(blocks$orientation == "portrait") > sum(blocks$orientation == "landscape"))
})

test_that("write_dta docx puts a real validation rules table in landscape", {
  # create_example_DTA() declares no rules, so the assertion above can only
  # reach the "Validation Rules" heading. This fixture has actual rules, which
  # is what puts a rules flextable on the page.
  yaml_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  skip_if(!nzchar(yaml_path) || !file.exists(yaml_path), "clinical_dta.yaml fixture missing")

  dta <- read_dta_from_yaml(yaml_path)
  has_rules <- any(vapply(dta@datasets, function(ds) {
    inherits(ds, "DTAtools::DTADataSetTabular") && length(ds@specs@rules) > 0
  }, logical(1)))
  expect_true(has_rules)

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  # "Rule ID" is a header cell of the rules flextable.
  expect_identical(.docx_orientation_of(out_docx, "Rule ID"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Variable Name"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Datasets"), "portrait")
})

test_that("landscape sections are really rotated, keep page size, add no blank pages", {
  # The orient ATTRIBUTE alone proves nothing: a section tagged landscape but
  # carrying portrait w/h renders as a portrait page, and the wide table still
  # overflows. The w > h assertion is what pins the actual rotation.
  # officer's own body_end_section_landscape() also hardcodes A4 and uses
  # type="oddPage", which pads the document with blank pages;
  # .end_section_orientation() reads the dimensions back from the document and
  # uses nextPage instead.
  dta <- create_example_DTA()
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  geom <- .docx_section_geometry(out_docx)
  expect_true(nrow(geom) > 1)
  expect_true(any(geom$orientation == "landscape"))
  expect_true(any(geom$orientation == "portrait"))

  # Rotation, not just a label.
  landscape <- geom[geom$orientation == "landscape", ]
  portrait <- geom[geom$orientation == "portrait", ]
  expect_true(all(landscape$width > landscape$height))
  expect_true(all(portrait$width < portrait$height))

  # No blank filler pages.
  expect_false(any(stats::na.omit(geom$type) == "oddPage"))

  # Landscape pages are the portrait page turned on its side, not a new format.
  expect_equal(unique(pmin(geom$width, geom$height)), min(portrait$width), tolerance = 1)
  expect_equal(unique(pmax(geom$width, geom$height)), max(portrait$height), tolerance = 1)
})

test_that("write_dataset_metadata also lands its specs table in landscape", {
  # The second caller of .add_dataset_specs_section(): heading_level = 2 and an
  # optional rules block, followed by a footer that must stay portrait.
  ds <- create_example_DTADataSetTabular(2)

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  expect_identical(.docx_orientation_of(out_docx, "Column Specifications"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Variable Name"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Dataset Information"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "Generated:"), "portrait")

  blocks <- .docx_blocks_with_orientation(out_docx)
  expect_identical(blocks$orientation[nrow(blocks)], "portrait")

  # include_rules = FALSE leaves a landscape section holding only the column
  # table; the surrounding document must still return to portrait.
  no_rules <- tempfile(fileext = ".docx")
  on.exit(unlink(no_rules, force = TRUE), add = TRUE)
  write_dataset_metadata(ds,
    file = no_rules, format = "docx", overwrite = TRUE, quiet = TRUE,
    include_rules = FALSE
  )
  expect_identical(.docx_orientation_of(no_rules, "Column Specifications"), "landscape")
  expect_length(.docx_orientation_of(no_rules, "Validation Rules"), 0L)
  expect_identical(
    .docx_blocks_with_orientation(no_rules)$orientation[
      nrow(.docx_blocks_with_orientation(no_rules))
    ],
    "portrait"
  )

  geom <- .docx_section_geometry(no_rules)
  landscape <- geom[geom$orientation == "landscape", ]
  expect_true(nrow(landscape) > 0)
  expect_true(all(landscape$width > landscape$height))
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

test_that("write_dta warns when include_yaml = TRUE cannot be honoured", {
  dta <- create_example_DTA()
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)

  # An explicit request for an embedded specification that cannot be honoured
  # must say so; it used to be discarded without any diagnostic at all.
  expect_warning(
    write_dta(
      dta,
      file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE,
      include_yaml = TRUE, yaml_text = NULL
    ),
    "yaml_text"
  )
  expect_true(file.exists(out_docx))
  body <- .read_docx_body_xml(out_docx)
  expect_false(grepl("Embedded Specification (YAML)", body, fixed = TRUE))

  # An empty string is just as unusable as NULL.
  out_empty <- tempfile(fileext = ".docx")
  on.exit(unlink(out_empty, force = TRUE), add = TRUE)
  expect_warning(
    write_dta(
      dta,
      file = out_empty, format = "docx", overwrite = TRUE, quiet = TRUE,
      include_yaml = TRUE, yaml_text = ""
    ),
    "yaml_text"
  )

  # Documented-but-untested: include_yaml is ignored for markdown output. It now
  # warns rather than being dropped in silence.
  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  expect_warning(
    write_dta(
      dta,
      file = out_md, format = "md", overwrite = TRUE, quiet = TRUE,
      include_yaml = TRUE, yaml_text = "columns: {}"
    ),
    "include_yaml"
  )

  # ... and likewise when a template is supplied.
  template <- .make_template("Title: {DTA_TITLE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out_tpl <- tempfile(fileext = ".docx")
  on.exit(unlink(out_tpl, force = TRUE), add = TRUE)
  expect_warning(
    write_dta(
      dta,
      file = out_tpl, template = template, overwrite = TRUE, quiet = TRUE,
      include_yaml = TRUE, yaml_text = "columns: {}"
    ),
    "include_yaml"
  )
})

test_that("write_dta pdf export writes a real PDF or aborts loudly", {
  dta <- create_example_DTA()

  # --- No conversion backend: abort, and leave nothing behind. -------------
  out_missing <- tempfile(fileext = ".pdf")
  on.exit(unlink(out_missing, force = TRUE), add = TRUE)
  sibling_docx <- sub("\\.pdf$", ".docx", out_missing)
  on.exit(unlink(sibling_docx, force = TRUE), add = TRUE)

  local_mocked_bindings(
    .pdf_backends_available = function() character(0),
    .pdf_conversion_available = function() FALSE
  )
  expect_error(
    write_dta(dta, file = out_missing, format = "pdf", overwrite = TRUE, quiet = TRUE),
    class = "rlang_error"
  )
  # The requested path must not hold a DOCX wearing a .pdf extension, and the
  # error handler must not have written to some other path instead.
  expect_false(file.exists(out_missing))
  expect_false(file.exists(sibling_docx))
})

test_that("write_dta pdf export rejects a converter that yields a non-PDF", {
  dta <- create_example_DTA()
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  # A converter that just copies the DOCX across is exactly the old fallback.
  # The backend list is pinned so a real LibreOffice/TinyTeX on the test machine
  # cannot quietly satisfy the export and hide the defect being pinned here.
  local_mocked_bindings(
    .pdf_backends_available = function() "pandoc",
    .pdf_conversion_available = function() TRUE,
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) {
      file.copy(docx_file, pdf_file, overwrite = TRUE)
      invisible(pdf_file)
    }
  )

  expect_error(
    write_dta(dta, file = out, format = "pdf", overwrite = TRUE, quiet = TRUE),
    class = "rlang_error"
  )
  # The ZIP/DOCX masquerading as a PDF must be removed, not handed to the user.
  expect_false(file.exists(out))
})

test_that("write_dta and write_dataset_metadata produce %PDF bytes on success", {
  dta <- create_example_DTA()
  ds <- create_example_DTADataSetTabular(2)

  local_mocked_bindings(
    .pdf_backends_available = function() "pandoc",
    .pdf_conversion_available = function() TRUE,
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) {
      writeBin(charToRaw("%PDF-1.7\n1 0 obj\n<<>>\nendobj\n%%EOF\n"), pdf_file)
      invisible(pdf_file)
    }
  )

  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  expect_no_error(
    write_dta(dta, file = out, format = "pdf", overwrite = TRUE, quiet = TRUE)
  )
  expect_true(file.exists(out))
  expect_identical(readBin(out, what = "raw", n = 4L), charToRaw("%PDF"))

  out_ds <- tempfile(fileext = ".pdf")
  on.exit(unlink(out_ds, force = TRUE), add = TRUE)
  expect_no_error(
    write_dataset_metadata(ds, file = out_ds, format = "pdf", overwrite = TRUE, quiet = TRUE)
  )
  expect_identical(readBin(out_ds, what = "raw", n = 4L), charToRaw("%PDF"))
})

test_that("write_dataset_metadata pdf export aborts when conversion is impossible", {
  ds <- create_example_DTADataSetTabular(2)
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  local_mocked_bindings(
    .pdf_backends_available = function() character(0),
    .pdf_conversion_available = function() FALSE
  )
  expect_error(
    write_dataset_metadata(ds, file = out, format = "pdf", overwrite = TRUE, quiet = TRUE),
    class = "rlang_error"
  )
  expect_false(file.exists(out))
})

test_that("dta_pdf_backend reports a usable backend or NULL", {
  backend <- dta_pdf_backend()

  if (is.null(backend)) {
    # The contract when nothing is installed: NULL, not an empty list or "".
    expect_null(backend)
    expect_length(.pdf_backends_available(), 0L)
    expect_false(.pdf_conversion_available())
  } else {
    expect_type(backend, "list")
    expect_named(backend, c("name", "engine", "available"))
    expect_true(backend$name %in% c("libreoffice", "tinytex", "pandoc"))
    # The reported backend is the first choice, not an arbitrary one.
    expect_identical(backend$name, backend$available[[1]])
    expect_true(all(backend$available %in% c("libreoffice", "tinytex", "pandoc")))
    expect_true(nzchar(backend$engine))
    expect_true(.pdf_conversion_available())
  }
})

test_that("a PDF backend is present wherever PDF export must work", {
  # COMPANION GUARD to the end-to-end test below. That test skips when no
  # backend exists, and a permanently skipped test is invisible dead coverage.
  # On CI a backend is installed by .github/workflows/R-CMD-check.yaml, so its
  # absence is a workflow regression and must fail loudly rather than skip.
  # This test itself never skips: both branches assert.
  backend <- dta_pdf_backend()

  if (identical(Sys.getenv("CI"), "true")) {
    expect_false(
      is.null(backend),
      info = paste(
        "CI must install a DOCX -> PDF backend (see setup-tinytex in",
        "R-CMD-check.yaml); without one the end-to-end PDF test silently skips."
      )
    )
  } else {
    # Off CI a backend is optional, but the probe must still honour its contract.
    expect_true(is.null(backend) || is.list(backend))
  }
})

test_that("write_dta and write_dataset_metadata really convert to PDF end to end", {
  # THE REAL THING: no mocked converter seam. This drives whichever backend the
  # machine actually has and inspects the bytes that land on disk.
  backend <- dta_pdf_backend()
  skip_if(
    is.null(backend),
    "no DOCX -> PDF backend installed (see the CI guard test above)"
  )

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  expect_no_error(
    write_dta(dta, file = out, format = "pdf", overwrite = TRUE, quiet = TRUE)
  )
  expect_true(file.exists(out))

  # A genuine PDF: right magic bytes, ...
  expect_identical(readBin(out, what = "raw", n = 4L), charToRaw("%PDF"))
  # ... not a DOCX (a ZIP, "PK") that slipped through with a .pdf name, ...
  expect_false(identical(readBin(out, what = "raw", n = 2L), charToRaw("PK")))
  # ... and not a stub: a real document of this size is several KB.
  expect_gt(file.info(out)$size, 4000)

  out_ds <- tempfile(fileext = ".pdf")
  on.exit(unlink(out_ds, force = TRUE), add = TRUE)
  ds <- create_example_DTADataSetTabular(2)
  expect_no_error(
    write_dataset_metadata(ds, file = out_ds, format = "pdf", overwrite = TRUE, quiet = TRUE)
  )
  expect_identical(readBin(out_ds, what = "raw", n = 4L), charToRaw("%PDF"))
  expect_false(identical(readBin(out_ds, what = "raw", n = 2L), charToRaw("PK")))
  expect_gt(file.info(out_ds)$size, 4000)
})

test_that("the no-backend abort names the command that fixes it", {
  # An error that only says "no backend available" leaves the user stuck; the
  # message must carry the exact remedy.
  local_mocked_bindings(
    .pdf_backends_available = function() character(0),
    .pdf_conversion_available = function() FALSE
  )

  expect_error(
    .convert_docx_to_pdf(tempfile(fileext = ".docx"), tempfile(fileext = ".pdf")),
    regexp = "tinytex::install_tinytex\\(\\)"
  )
  expect_error(
    .convert_docx_to_pdf(tempfile(fileext = ".docx"), tempfile(fileext = ".pdf")),
    regexp = "LibreOffice"
  )
})

test_that("the abort explains that pandoc alone cannot make a PDF", {
  # The most confusing real-world setup: pandoc IS installed, so the old probe
  # claimed PDF export was available, but pandoc shells out to a PDF engine and
  # there is none. The message must say so rather than just "no backend".
  local_mocked_bindings(.pandoc_pdf_engine = function() "")
  bullets <- .pdf_no_backend_bullets()

  pandoc_here <- requireNamespace("rmarkdown", quietly = TRUE) &&
    isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE))

  if (pandoc_here) {
    expect_true(any(grepl("pandoc cannot write a PDF on its own", bullets, fixed = TRUE)))
  } else {
    # No pandoc: the bullet would be a lie, so it must be absent.
    expect_false(any(grepl("pandoc cannot write a PDF on its own", bullets, fixed = TRUE)))
  }
  # The remedy is named either way.
  expect_true(any(grepl("tinytex::install_tinytex()", bullets, fixed = TRUE)))
})

test_that("conversion falls through to the next backend when the first fails", {
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  print(officer::read_docx(), target = docx)
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  tried <- character(0)
  local_mocked_bindings(
    .pdf_backends_available = function() c("libreoffice", "pandoc"),
    .soffice_docx_to_pdf = function(docx_file, pdf_file) {
      tried <<- c(tried, "libreoffice")
      cli::cli_abort("soffice exploded")
    },
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) {
      tried <<- c(tried, "pandoc")
      writeBin(charToRaw("%PDF-1.7\n1 0 obj\n<<>>\nendobj\n%%EOF\n"), pdf_file)
      invisible(pdf_file)
    }
  )

  expect_no_error(.convert_docx_to_pdf(docx, out))
  expect_identical(tried, c("libreoffice", "pandoc"))
  expect_identical(readBin(out, what = "raw", n = 4L), charToRaw("%PDF"))
})

test_that("the abort reports every backend that failed, with braces intact", {
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  print(officer::read_docx(), target = docx)
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  local_mocked_bindings(
    .pdf_backends_available = function() c("libreoffice", "pandoc"),
    .soffice_docx_to_pdf = function(docx_file, pdf_file) {
      # Braces in external tool output must not be interpolated by cli.
      cli::cli_abort("soffice said {{weird}}")
    },
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) {
      # Succeeds, but writes a DOCX: the signature check must still reject it.
      file.copy(docx_file, pdf_file, overwrite = TRUE)
      invisible(pdf_file)
    }
  )

  err <- expect_error(.convert_docx_to_pdf(docx, out), class = "rlang_error")
  msg <- conditionMessage(err)
  expect_match(msg, "libreoffice")
  expect_match(msg, "pandoc")
  expect_match(msg, "%PDF")
  # Nothing left behind at the requested path.
  expect_false(file.exists(out))
})

test_that(".cli_escape neutralises braces so cli cannot reinterpret tool output", {
  expect_identical(.cli_escape("a {b} c"), "a {{b}} c")
  expect_identical(.cli_escape("no braces"), "no braces")
  # An unbalanced brace from a truncated tool message must still be safe.
  expect_identical(.cli_escape("{oops"), "{{oops")
})

test_that(".pandoc_pdf_engine and .find_soffice return scalar strings", {
  engine <- .pandoc_pdf_engine()
  expect_type(engine, "character")
  expect_length(engine, 1L)

  soffice <- .find_soffice()
  expect_type(soffice, "character")
  expect_length(soffice, 1L)
  # When a path is reported it must actually exist, not be a stale guess.
  if (nzchar(soffice)) {
    expect_true(file.exists(soffice))
  }

  bin_dir <- .tinytex_bin_dir()
  expect_length(bin_dir, 1L)
  if (nzchar(bin_dir)) {
    expect_true(dir.exists(bin_dir))
  }
})

test_that(".is_pdf_file recognises the PDF signature and nothing else", {
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf, force = TRUE), add = TRUE)
  writeBin(charToRaw("%PDF-1.4\n"), pdf)
  expect_true(.is_pdf_file(pdf))

  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  print(officer::read_docx(), target = docx)
  # A DOCX is a ZIP archive: it starts with "PK".
  expect_identical(readBin(docx, what = "raw", n = 2L), charToRaw("PK"))
  expect_false(.is_pdf_file(docx))

  expect_false(.is_pdf_file(tempfile(fileext = ".pdf")))
})

test_that("exported document dates are ISO 8601 under any LC_TIME", {
  # OWNER'S DECISION: dates in exported documents are always YYYY-MM-DD. The
  # month name must never come from LC_TIME, so run under a non-English time
  # locale and assert the international form anyway.
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)
  # Whichever non-English time locale this machine offers; the assertions below
  # hold in every locale, so no skip is needed if none can be set.
  for (loc in c("de_DE.UTF-8", "German_Germany.1252", "fr_FR.UTF-8", "French_France.1252")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", loc)))) break
  }

  dta <- DTA(
    datasets = create_example_DTADataSetTabular(1),
    metadata = DTAMetaData(title = "Locale Test", version = "1.0", date = as.Date("2026-01-15"))
  )

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  paragraphs <- .docx_paragraphs(out_docx)
  expect_true("Date: 2026-01-15" %in% paragraphs)
  # No localized month name may survive anywhere in the document.
  expect_false(any(grepl("Januar|January|janvier", paragraphs)))
})

test_that("write_dta docx opens with the signature block, before any other chapter", {
  # The approval table is what a reader must act on, so it precedes Document
  # Information, the party sections and the datasets.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  summary <- officer::docx_summary(officer::read_docx(out_docx))
  h2 <- summary[summary$style_name == "heading 2", c("doc_index", "text")]
  idx <- function(txt) h2$doc_index[which(h2$text == txt)[1]]

  expect_false(is.na(idx("Approval & Signatures")))
  for (later in c("Document Information", "Receiver Information", "Supplier Information", "Datasets")) {
    expect_lt(idx("Approval & Signatures"), idx(later))
  }

  # Each authorized signatory of the fixture gets exactly one row.
  sig <- .extract_signatories(dta@metadata)
  expect_gt(nrow(sig), 0)
  for (nm in sig$Name) {
    expect_true(any(grepl(nm, summary$text, fixed = TRUE)))
  }
})

test_that("write_dta docx carries no filler signature lines and no meta explanation", {
  for (dta in list(
    create_example_DTA(),
    read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  )) {
    out_docx <- tempfile(fileext = ".docx")
    on.exit(unlink(out_docx, force = TRUE), add = TRUE)
    write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
    txt <- officer::docx_summary(officer::read_docx(out_docx))$text
    txt <- txt[!is.na(txt)]

    expect_false(any(grepl("Approved by", txt, fixed = TRUE)))
    expect_false(any(grepl("Note: signatories listed above", txt, fixed = TRUE)))
    # No row of underscores anywhere: signature/date cells are left blank and
    # sized instead.
    expect_false(any(grepl("_____", txt, fixed = TRUE)))
  }
})

test_that("write_dta docx omits the signature chapter when nobody is authorized to sign", {
  dta <- create_example_DTA()
  expect_equal(NROW(.extract_signatories(dta@metadata)), 0)

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  txt <- officer::docx_summary(officer::read_docx(out_docx))$text
  expect_false("Approval & Signatures" %in% txt)
})

test_that("write_dta docx uses one font family and one table header fill throughout", {
  # The untidy look came from three families (template Cambria body, Calibri
  # headings, flextable's Arial default) and unrelated blues. Everything the
  # package emits explicitly must now name the house family and palette.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  yaml_text <- paste(
    readLines(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"), warn = FALSE),
    collapse = "\n"
  )
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  # include_yaml = TRUE so the monospace block is present too: it is the only
  # place a second family is allowed, and it must be the declared one.
  write_dta(dta,
    file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE,
    include_yaml = TRUE, yaml_text = yaml_text
  )

  xml <- .read_docx_body_xml(out_docx)

  fonts <- unique(unlist(regmatches(
    xml, gregexpr('(?<=w:ascii=")[^"]+', xml, perl = TRUE)
  )))
  expect_setequal(fonts, c(FONTS$primary, FONTS$monospace))

  # Every table header cell is filled with the single house navy.
  fills <- unique(unlist(regmatches(
    xml, gregexpr('(?<=<w:shd )[^>]*w:fill="[^"]+', xml, perl = TRUE)
  )))
  fills <- toupper(sub('.*w:fill="', "", fills))
  fills <- setdiff(fills, c("AUTO", "FFFFFF"))
  expect_setequal(
    fills,
    toupper(sub("^#", "", c(THEME_COLORS$primary_dark, THEME_COLORS$gray_light)))
  )
})

test_that("write_dta docx introduces the supplier before the receiver", {
  # Data flows supplier -> receiver, so that is the order the parties appear in,
  # in both the Word and the Markdown rendering.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  summary <- officer::docx_summary(officer::read_docx(out_docx))
  h2 <- summary[summary$style_name == "heading 2", c("doc_index", "text")]
  idx_sup <- h2$doc_index[which(h2$text == "Supplier Information")[1]]
  idx_rec <- h2$doc_index[which(h2$text == "Receiver Information")[1]]
  expect_false(is.na(idx_sup))
  expect_false(is.na(idx_rec))
  expect_lt(idx_sup, idx_rec)

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out_md, warn = FALSE)
  expect_lt(
    grep("Supplier Information", md)[1],
    grep("Receiver Information", md)[1]
  )
})

test_that("the built-in docx export uses the Boehringer brand palette", {
  # The document palette must match the Shiny app's brand theme; a drift here is
  # exactly the "different colours" problem the redesign removed.
  expect_identical(THEME_COLORS$primary_dark, "#00625B")
  expect_identical(THEME_COLORS$primary, "#00A886")

  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  xml <- .read_docx_body_xml(out_docx)
  expect_false(grepl("1F497D", xml, fixed = TRUE)) # the old template blue
  expect_false(grepl("4F81BD", xml, fixed = TRUE))
  expect_true(grepl(sub("^#", "", THEME_COLORS$primary_dark), xml, fixed = TRUE))
})

test_that("the bundled template renders headings in the brand green", {
  # Heading colour comes from the template's own styles, not from anything the
  # package writes per paragraph, so it has to be pinned on the asset itself --
  # otherwise Word draws black headings above brand-green tables.
  tmpl <- system.file("extdata", "templates", "dta_numbered_template.docx", package = "DTAtools")
  expect_true(nzchar(tmpl))

  ex <- tempfile()
  dir.create(ex)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(tmpl, files = "word/styles.xml", exdir = ex)
  styles <- paste(readLines(file.path(ex, "word", "styles.xml"), warn = FALSE), collapse = "")

  green <- sub("^#", "", THEME_COLORS$primary_dark)
  for (id in paste0("Titre", 1:4)) {
    block <- regmatches(
      styles,
      regexpr(sprintf('<w:style [^>]*w:styleId="%s".*?</w:style>', id), styles, perl = TRUE)
    )
    expect_length(block, 1)
    expect_true(grepl(sprintf('<w:color w:val="%s"/>', green), block, fixed = TRUE))
  }
})

test_that("group_condition rules reach the exported documents fully described", {
  # The formatter is only useful if the rules table actually carries it: the
  # clinical fixture's group rules previously showed a one-line summary with no
  # grouping, condition or constraint detail at all.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  txt <- officer::docx_summary(officer::read_docx(out_docx))$text
  txt <- paste(txt[!is.na(txt)], collapse = "\n")

  expect_match(txt, "group_condition_pass_example", fixed = TRUE)
  expect_match(txt, "Rows are grouped by 'SUBJECT_ID'", fixed = TRUE)
  expect_match(txt, "\"c_has_visit\": a row where 'VISIT' is present", fixed = TRUE)
  expect_match(txt, "must hold for every row in the group", fixed = TRUE)
  expect_false(grepl("no description available", txt, fixed = TRUE))

  # The multi-line description becomes real Word line breaks, not a lost newline.
  xml <- .read_docx_body_xml(out_docx)
  expect_true(grepl("<w:br/>", xml, fixed = TRUE))

  # Markdown: newlines must not break the pipe table, so they become <br> and
  # every rule still occupies exactly one table row.
  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out_md, warn = FALSE)

  rule_rows <- grep("^\\| group_condition_pass_example ", md, value = TRUE)
  expect_length(rule_rows, 1)
  expect_match(rule_rows, "Rows are grouped by 'SUBJECT_ID'", fixed = TRUE)
  expect_match(rule_rows, "<br>", fixed = TRUE)
  # A well-formed pipe row: no stray newline split it into a headless fragment.
  expect_match(rule_rows, "\\|$")
})

test_that("a generated version-history summary does not break the Markdown table (regression guard)", {
  # dta_version_change_summary() (inst/shiny/dta_app/R/versioning.R) renders a
  # version_history entry's `changes` with literal quotes, "->" arrows and
  # "; "-joined detail lines. Only "|" and a real newline are special to a
  # GFM pipe table -- dta_version_sanitise() already strips both before this
  # text goes anywhere near an export -- so this pins that the REST of that
  # punctuation still leaves the Version History table exactly one row per
  # history entry, i.e. that .df_to_md_table() (R/exportDocuments.R ~line
  # 333) is not confused by it.
  skip_if_not_installed("shiny")

  summary_fn <- app_fn("dta_version_change_summary")

  diff1 <- list(
    metadata = data.frame(
      key = c("title", "header"),
      change = c("changed", "changed"),
      from = c("Study 'Alpha' Specification", "Acme Corp"),
      to = c("Study 'Beta' Specification", "Acme Corp Ltd"),
      stringsAsFactors = FALSE
    ),
    datasets = data.frame(
      key = "clinical_data.columns.AGE.type", change = "changed",
      from = "SAS Num", to = "SAS Char",
      stringsAsFactors = FALSE
    )
  )
  diff2 <- list(
    metadata = data.frame(
      key = "title", change = "changed",
      from = "Study 'Beta' Specification", to = "Study 'Gamma' Specification",
      stringsAsFactors = FALSE
    ),
    datasets = data.frame(
      key = character(0), change = character(0),
      from = character(0), to = character(0),
      stringsAsFactors = FALSE
    )
  )

  changes1 <- summary_fn(diff1, note = "Initial revision")
  changes2 <- summary_fn(diff2, note = "Renamed the study")

  # Sanity check on the fixture itself -- this has to actually exercise
  # quotes, an arrow and a semicolon, not an accidentally plain string.
  expect_match(changes1, "'", fixed = TRUE)
  expect_match(changes1, "->", fixed = TRUE)
  expect_match(changes1, ";", fixed = TRUE)

  dta <- create_example_DTA()
  md <- metadata(dta)
  S7::prop(md, "version_history") <- list(
    list(version = "1.1", date = Sys.Date() - 10, changes = changes1),
    list(version = "1.2", date = Sys.Date(), changes = changes2)
  )
  S7::prop(md, "version") <- "1.2"
  dta@metadata <- md

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md_lines <- readLines(out_md, warn = FALSE)

  # Both version numbers are distinctive enough to key on directly, the same
  # way the rule-id test above keys on "group_condition_pass_example".
  v1_rows <- grep("^\\| 1\\.1 \\|", md_lines, value = TRUE)
  v2_rows <- grep("^\\| 1\\.2 \\|", md_lines, value = TRUE)

  # Exactly one table row per version_history entry -- a summary that broke
  # the table would instead split one of these into a headless fragment,
  # changing this count.
  expect_length(v1_rows, 1)
  expect_length(v2_rows, 1)
  expect_match(v1_rows, "Initial revision", fixed = TRUE)
  expect_match(v2_rows, "Renamed the study", fixed = TRUE)
  # Well-formed pipe rows: no stray newline split either into a headless
  # fragment.
  expect_match(v1_rows, "\\|$")
  expect_match(v2_rows, "\\|$")
})
