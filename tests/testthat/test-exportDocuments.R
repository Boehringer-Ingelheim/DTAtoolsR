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
  # The document name is the top heading; the DTA title follows it.
  expect_equal(md_lines[1], "# Data Transfer Agreement")
  expect_identical(md_lines[3], "**Example DTA**")
  expect_false(any(grepl("**Title:**", md_lines, fixed = TRUE)))
  expect_true(any(grepl("^## [0-9]+ Datasets$", md_lines)))

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

  # The document name leads; the supplied title follows it, unreplaced.
  paragraphs <- Filter(nzchar, .docx_paragraphs(out_docx))
  expect_equal(paragraphs[1:2], c("Data Transfer Agreement", "Study (Phase 3"))
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
  expect_equal(md_lines[1], "# Data Transfer Agreement")
  expect_identical(md_lines[3], "**Dataset Specification: Demographics (demographics)**")
  # Without signatories there is no approval chapter (and no blank
  # "Approved by" line to fill in instead).
  expect_false(any(grepl("Approval", md_lines, fixed = TRUE)))
  expect_false(any(grepl("____", md_lines, fixed = TRUE)))

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
  tp <- system.file("extdata", "word-templates", "dta_numbered_template.docx", package = "DTAtools")
  expect_true(nzchar(tp) && file.exists(tp))

  si <- officer::styles_info(officer::read_docx(path = tp))
  expect_true("heading 4" %in% si$style_name)
})

test_that("write_dta docx uses numbered heading hierarchy", {
  dta <- create_example_DTA()
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  # Assert on style_name, not on the underlying styleId. The checked-in template
  # inst/extdata/word-templates/dta_numbered_template.docx was saved by a French Word
  # install, so its ids happen to be "Titre1".."Titre4"; regenerating it on an
  # English install would yield "Heading1".. with no functional change.
  # .add_heading() (R/documentBuilders.R:61-64) requests "heading <level>", and
  # that is the locale-independent contract worth pinning.
  summary <- officer::docx_summary(officer::read_docx(out_docx))
  for (nm in c("heading 1", "heading 2", "heading 3")) {
    expect_true(nm %in% summary$style_name)
  }

  # The Datasets -> <dataset> -> Files / Column Specifications hierarchy must
  # be carried by those heading levels, not by bold body paragraphs. Each
  # chapter is a top-level heading; there is no wrapper chapter above them.
  headings <- summary[summary$style_name %in% paste("heading", 1:4), ]
  expect_equal(headings$style_name[headings$text == "Datasets"], "heading 1")
  # A dataset with a name is headed "<name> (<id>)".
  expect_equal(
    unique(headings$style_name[headings$text == "Demographics (demographics)"]),
    "heading 2"
  )
  expect_equal(unique(headings$style_name[headings$text == "Column Specifications"]), "heading 3")
  expect_false("Dataset Specifications" %in% headings$text)
  expect_false("Data Transfer Agreement" %in% headings$text)
})

# A dataset is headed by its name with the id beside it ("Demographics
# (demographics)"), because the id is what every validation message and
# {COLUMN_SPECS:<id>} refers to; a dataset with no name is headed by the bare
# id. The long description is body text under the heading.
.export_naming_dta <- function() {
  named <- create_example_DTADataSetTabular(2)
  named@description <- "One row per subject, as delivered by the CRO."
  unnamed <- DTADataSetTabular(
    id = "audit_trail",
    specs = create_example_DTAColumnSpecCollection(1),
    description = "Every change made to the records."
  )
  DTA(datasets = list(named, unnamed), metadata = create_example_DTAMetaData())
}

test_that("write_dta markdown heads a dataset by name and id, an unnamed one by its id", {
  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(.export_naming_dta(), file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out_md, warn = FALSE)

  named_at <- match("### 2.1 Demographics (demographics)", md)
  # Escaped Markdown: the `_` would otherwise start emphasis.
  unnamed_at <- match("### 2.2 audit\\_trail", md)
  expect_false(is.na(named_at))
  expect_false(is.na(unnamed_at))
  # The long description is the paragraph under its heading.
  expect_identical(md[named_at + 2], "One row per subject, as delivered by the CRO.")
  expect_identical(md[unnamed_at + 2], "Every change made to the records.")
  # Neither the bare id of a named dataset nor a name-only heading appears.
  expect_false(any(grepl("^### [0-9.]+ demographics$", md)))
  expect_false(any(grepl("^### [0-9.]+ Demographics$", md)))
})

test_that("the Markdown exports escape a dataset's name and description", {
  ds <- create_example_DTADataSetTabular(2)
  ds@name <- "*Draft* #2"
  ds@description <- "# not a heading"
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out_md, warn = FALSE)
  expect_true(any(grepl("^### [0-9.]+ \\\\\\*Draft\\\\\\* \\\\#2 \\(demographics\\)$", md)))
  expect_true("\\# not a heading" %in% md)
  expect_false("# not a heading" %in% md)

  out_ds <- tempfile(fileext = ".md")
  on.exit(unlink(out_ds, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out_ds, format = "md", overwrite = TRUE, quiet = TRUE)
  ds_md <- readLines(out_ds, warn = FALSE)
  expect_true("**Dataset Specification: \\*Draft\\* \\#2 (demographics)**" %in% ds_md)
  expect_true("\\# not a heading" %in% ds_md)
  expect_false("# not a heading" %in% ds_md)
})

test_that("write_dta docx heads a dataset by name and id, with its description as body text", {
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(.export_naming_dta(), file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  summary <- officer::docx_summary(officer::read_docx(out_docx))

  is_heading <- summary$style_name %in% paste("heading", 1:4)
  headings <- summary$text[is_heading]
  expect_true("Demographics (demographics)" %in% headings)
  expect_true("audit_trail" %in% headings)
  expect_false("demographics" %in% headings)

  # The descriptions are paragraphs, not headings and not table cells.
  body <- summary$text[summary$content_type == "paragraph" & !is_heading]
  expect_true("One row per subject, as delivered by the CRO." %in% body)
  expect_true("Every change made to the records." %in% body)
})

test_that("write_dataset_metadata markdown lists the id, name and description of a dataset", {
  ds <- create_example_DTADataSetTabular(2)
  ds@description <- "One row per subject, as delivered by the CRO."
  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out_md, warn = FALSE)

  # The dataset is named (name and id) in the bold subtitle only; with no
  # template the remaining fact, its type, is one sentence. The description is
  # a chapter of its own.
  expect_identical(md[3], "**Dataset Specification: Demographics (demographics)**")
  expect_identical(md[5], "Type: Tabular.")
  expect_false(any(grepl("^\\| (Dataset ID|Name|Type) \\|", md)))
  description_at <- match("## 1 Description", md)
  expect_false(is.na(description_at))
  expect_identical(md[description_at + 2], "One row per subject, as delivered by the CRO.")
  expect_false(any(grepl("^\\*\\*[A-Za-z ]+:\\*\\*", md)))

  # Without a name there is no Name row and the subtitle names the bare id.
  bare <- create_example_DTADataSetTabular(1)
  out_bare <- tempfile(fileext = ".md")
  on.exit(unlink(out_bare, force = TRUE), add = TRUE)
  write_dataset_metadata(bare, file = out_bare, format = "md", overwrite = TRUE, quiet = TRUE)
  md_bare <- readLines(out_bare, warn = FALSE)

  # Escaped Markdown, so the underscores cannot start emphasis.
  expect_identical(
    md_bare[3],
    "**Dataset Specification: example\\_container\\_specs\\_without\\_data**"
  )
  expect_false(any(grepl("^\\| (Dataset ID|Name) \\|", md_bare)))
})

test_that("write_dataset_metadata docx titles a dataset by name and id and tables its facts", {
  ds <- create_example_DTADataSetTabular(2)
  ds@description <- "One row per subject, as delivered by the CRO."
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dataset_metadata(
    ds,
    file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE,
    include_signatures = FALSE
  )
  summary <- officer::docx_summary(officer::read_docx(out_docx))

  paragraphs <- summary$text[summary$content_type == "paragraph"]
  expect_true("Dataset Specification: Demographics (demographics)" %in% paragraphs)

  # The title page carries the fact table of the Markdown head: the
  # identifier first, then the name and the type (create_example_DTADataSetTabular()
  # is a "tabular" dataset without template provenance). No header row.
  cells <- summary$text[summary$content_type == "table cell"]
  expect_identical(
    cells[1:6],
    c("Dataset ID", "demographics", "Name", "Demographics", "Type", "Tabular")
  )
  # The description is a chapter of its own, as in the Markdown document.
  expect_identical(
    paragraphs[which(paragraphs == "Description") + 1L],
    "One row per subject, as delivered by the CRO."
  )
})

test_that("write_dataset_metadata docx has the chapters of the Markdown document, numbered", {
  ds <- create_example_DTADataSetTabular(2)
  ds@description <- "Demographics."
  signatories <- list(list(name = "Jane Roe", role = "Data Manager", organization = "Acme"))

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, out, quiet = TRUE, signature_list = signatories)
  s <- officer::docx_summary(officer::read_docx(out))
  expect_identical(
    s$text[s$style_name %in% "heading 1"],
    c("Approval & Signatures", "Description", "Files", "Column Specifications", "Validation Rules")
  )
  expect_false(any(grepl("^heading [2-9]$", s$style_name)))
  expect_false("File Specifications" %in% s$text)
  expect_false("Dataset Information" %in% s$text)

  # Numbered: the document is built on the bundled numbered template, whose
  # "heading 1" style carries a list numbering (1, 2, 3 ...).
  ex <- tempfile()
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(out, files = "word/styles.xml", exdir = ex)
  styles <- xml2::read_xml(file.path(ex, "word", "styles.xml"))
  h1 <- xml2::xml_find_first(
    styles,
    ".//*[local-name()='style'][*[local-name()='name'][@*[local-name()='val']='heading 1']]"
  )
  expect_length(xml2::xml_find_all(h1, ".//*[local-name()='numPr']"), 1L)

  # include_* switch chapters off; without signatories (or with
  # include_signatures = FALSE) there is no approval chapter.
  out2 <- tempfile(fileext = ".docx")
  on.exit(unlink(out2, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, out2,
    quiet = TRUE, signature_list = signatories,
    include_signatures = FALSE, include_file_specs = FALSE, include_rules = FALSE
  )
  s2 <- officer::docx_summary(officer::read_docx(out2))
  expect_identical(s2$text[s2$style_name %in% "heading 1"], c("Description", "Column Specifications"))
})

test_that("write_dataset_metadata docx is deterministic and carries no time stamp", {
  ds <- create_example_DTADataSetTabular(2)
  ds@template_source <- "demo_template"
  ds@template_version <- "4.2"
  ds@template_date <- "2025-03-14"
  write_parts <- function() {
    out <- tempfile(fileext = ".docx")
    on.exit(unlink(out, force = TRUE), add = TRUE)
    write_dataset_metadata(ds, out, quiet = TRUE, organisation = "Acme Pharma")
    ex <- tempfile()
    on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(out, exdir = ex)
    parts <- list.files(file.path(ex, "word"), "^(document|header[0-9]+|footer[0-9]+)[.]xml$")
    stats::setNames(
      lapply(parts, function(p) readBin(file.path(ex, "word", p), "raw", file.size(file.path(ex, "word", p)))),
      parts
    )
  }
  first <- write_parts()
  second <- write_parts()
  # Byte for byte, without any normalisation.
  expect_identical(first, second)
  expect_true(any(startsWith(names(first), "footer")))

  text <- vapply(first, function(x) gsub("<[^>]+>", "", rawToChar(x)), character(1))
  expect_false(any(grepl("Generated", text, fixed = TRUE)))
  expect_false(any(grepl(format(Sys.Date(), "%Y-%m-%d"), text, fixed = TRUE)))

  # Header and footer name the document and the template it came from.
  footers <- text[startsWith(names(text), "footer")]
  for (f in footers) {
    expect_match(f, "Dataset Specification: Demographics (demographics)", fixed = TRUE)
    expect_match(f, "Template version 4.2", fixed = TRUE)
    expect_match(f, "Template: demo_template \u00b7 version 4.2 \u00b7 2025-03-14", fixed = TRUE)
  }
  headers <- text[startsWith(names(text), "header")]
  for (h in headers) {
    expect_match(h, "Dataset Specification: Demographics (demographics)", fixed = TRUE)
    expect_match(h, "Acme Pharma", fixed = TRUE)
  }
})

test_that("write_dataset_metadata docx shows a long description in full", {
  ds <- create_example_DTADataSetTabular(2)
  long <- paste(
    "Subject-level demographics delivered once per data cut by the CRO, one row per",
    "randomised subject, including screen failures that were never dosed."
  )
  expect_gt(nchar(long), 80)
  ds@description <- long
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, out, quiet = TRUE)
  paragraphs <- .docx_paragraphs(out)
  expect_true(long %in% paragraphs)
  expect_false(any(grepl("...", paragraphs, fixed = TRUE)))
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
  heading1 <- summary[summary$style_name %in% "heading 1", c("doc_index", "text")]
  idx_process <- heading1$doc_index[which(heading1$text == "Process Information")[1]]
  idx_datasets <- heading1$doc_index[which(heading1$text == "Datasets")[1]]
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

  # Everything around them stays portrait: the cover, the metadata narrative,
  # the dataset heading and its Files table.
  expect_identical(.docx_orientation_of(out_docx, "Data Transfer Agreement"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "Document Information"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "Datasets"), "portrait")
  expect_identical(.docx_orientation_of(out_docx, "^Files$", fixed = FALSE), "portrait")

  # The specs headings sit inside the landscape section, so they can never be
  # orphaned at the foot of a portrait page.
  blocks <- .docx_blocks_with_orientation(out_docx)
  expect_identical(blocks$orientation[match("Column Specifications", blocks$text)], "landscape")

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
  # The second caller of .add_dataset_specs_section(): heading_level = 1 and an
  # optional rules block; the chapters before it, and the end, stay portrait.
  ds <- create_example_DTADataSetTabular(2)

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  expect_identical(.docx_orientation_of(out_docx, "Column Specifications"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Variable Name"), "landscape")
  expect_identical(.docx_orientation_of(out_docx, "Files"), "portrait")

  # The tables close the document, as in write_dta(): no "Generated" line or
  # other trailing paragraph follows them on a portrait page of its own.
  blocks <- .docx_blocks_with_orientation(out_docx)
  expect_identical(blocks$orientation[nrow(blocks)], "landscape")

  # include_rules = FALSE leaves a landscape section holding only the column
  # table, and it still closes the document.
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
    "landscape"
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
    expect_true(backend$name %in% c("word", "libreoffice", "tinytex", "pandoc"))
    # The reported backend is the first choice, not an arbitrary one.
    expect_identical(backend$name, backend$available[[1]])
    expect_true(all(backend$available %in% c("word", "libreoffice", "tinytex", "pandoc")))
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

test_that("a failed PDF conversion never touches a valid file already at the destination", {
  # Regression guard: every backend used to unlink() the destination on its own
  # failure, so a conversion attempted against an already-exported PDF (e.g. a
  # re-run with overwrite = TRUE) destroyed that valid file even though the new
  # attempt never produced a replacement.
  test_dir <- tempfile("pdf_export_")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)
  out <- file.path(test_dir, "out.pdf")
  original <- charToRaw("%PDF-1.4\nold valid pdf bytes\n%%EOF\n")
  writeBin(original, out)

  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  print(officer::read_docx(), target = docx)

  local_mocked_bindings(
    .pdf_backends_available = function() c("libreoffice", "pandoc"),
    .soffice_docx_to_pdf = function(docx_file, pdf_file) cli::cli_abort("soffice exploded"),
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) cli::cli_abort("pandoc exploded")
  )

  expect_error(.convert_docx_to_pdf(docx, out), class = "rlang_error")
  expect_identical(readBin(out, what = "raw", n = length(original)), original)
  # No staging file left behind next to the untouched destination.
  expect_identical(list.files(test_dir), "out.pdf")
})

test_that(".dta_write_staged leaves an existing destination untouched when write() fails", {
  test_dir <- tempfile("staged_write_")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dest <- file.path(test_dir, "out.bin")
  original <- charToRaw("original bytes, must survive")
  writeBin(original, dest)

  expect_error(
    .dta_write_staged(dest, write = function(stage) cli::cli_abort("write blew up")),
    class = "rlang_error"
  )
  expect_identical(readBin(dest, what = "raw", n = length(original)), original)
  expect_identical(list.files(test_dir), "out.bin")
})

test_that(".dta_write_staged leaves an existing destination untouched when check() rejects it", {
  test_dir <- tempfile("staged_write_")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dest <- file.path(test_dir, "out.bin")
  original <- charToRaw("original bytes, must survive")
  writeBin(original, dest)

  expect_error(
    .dta_write_staged(
      dest,
      write = function(stage) writeBin(charToRaw("garbage"), stage),
      check = function(stage) cli::cli_abort("rejected")
    ),
    class = "rlang_error"
  )
  expect_identical(readBin(dest, what = "raw", n = length(original)), original)
  expect_identical(list.files(test_dir), "out.bin")
})

test_that(".dta_write_staged replaces the destination and leaves no staging file on success", {
  test_dir <- tempfile("staged_write_")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dest <- file.path(test_dir, "out.bin")

  .dta_write_staged(
    dest,
    write = function(stage) writeBin(charToRaw("new content"), stage),
    check = function(stage) invisible(TRUE)
  )
  expect_identical(readBin(dest, what = "raw", n = 11L), charToRaw("new content"))
  expect_identical(list.files(test_dir), "out.bin")
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

test_that(".tinytex_docx_to_pdf typesets a character pdflatex cannot (retry with xelatex)", {
  skip_if(!nzchar(.tinytex_bin_dir()))
  skip_if_not(rmarkdown::pandoc_available())
  old_path <- Sys.getenv("PATH")
  if (!nzchar(Sys.which("latexmk")) && !nzchar(Sys.which("pdflatex"))) {
    Sys.setenv(PATH = paste(.tinytex_bin_dir(), old_path, sep = .Platform$path.sep))
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  }
  docx <- tempfile(fileext = ".docx")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(docx, pdf), force = TRUE), add = TRUE)
  doc <- officer::body_add_par(officer::read_docx(), "a ≠ b")
  print(doc, target = docx)

  .tinytex_docx_to_pdf(docx, pdf)
  expect_true(.is_pdf_file(pdf))
})

test_that("word is a backend only when Word is found, and then comes first", {
  local_mocked_bindings(.find_word = function() "")
  expect_false("word" %in% .pdf_backends_available())

  local_mocked_bindings(.find_word = function() "C:/fake/WINWORD.EXE")
  expect_identical(.pdf_backends_available()[[1]], "word")
})

test_that(".find_word returns \"\" for a registry path that does not exist, and the path of one that does", {
  skip_if_not(.Platform$OS.type == "windows")
  real <- tempfile(fileext = ".exe")
  file.create(real)
  on.exit(unlink(real), add = TRUE)

  local_mocked_bindings(
    readRegistry = function(...) list(`(Default)` = "C:/no/such/dir/WINWORD.EXE"),
    .package = "utils"
  )
  expect_identical(.find_word(), "")

  local_mocked_bindings(
    readRegistry = function(...) list(`(Default)` = paste0("\"", real, "\"")),
    .package = "utils"
  )
  expect_identical(.find_word(), normalizePath(real, winslash = "/"))
})

test_that(".find_word is empty off Windows", {
  skip_if(.Platform$OS.type == "windows")
  expect_identical(.find_word(), "")
})

.word_pids <- function() {
  out <- suppressWarnings(system2("tasklist",
    c("/FO", "CSV", "/NH", "/FI", shQuote("IMAGENAME eq WINWORD.EXE", type = "cmd")),
    stdout = TRUE, stderr = TRUE
  ))
  hit <- grep("WINWORD.EXE", out, fixed = TRUE, value = TRUE)
  as.integer(vapply(strsplit(hit, "\",\"", fixed = TRUE), `[`, "", 2))
}

.word_test_docx <- function() {
  docx <- tempfile(fileext = ".docx")
  doc <- officer::body_add_par(officer::read_docx(), "Body text")
  print(doc, target = docx)
  docx
}

test_that(".word_docx_to_pdf writes a PDF of plausible size and leaves no Word behind", {
  skip_if_not(.Platform$OS.type == "windows")
  skip_if(!nzchar(.find_word()), "Microsoft Word is not installed")
  skip_if(length(.word_pids()) > 0, "a Word session is already open; see the attach test")

  docx <- .word_test_docx()
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(docx, pdf), force = TRUE), add = TRUE)

  .word_docx_to_pdf(docx, pdf)
  expect_true(.is_pdf_file(pdf))
  expect_gt(file.info(pdf)$size, 1000)
  expect_identical(.word_pids(), integer(0))
})

test_that(".word_docx_to_pdf never ends a Word session that was already running", {
  skip_if_not(.Platform$OS.type == "windows")
  skip_if(!nzchar(.find_word()), "Microsoft Word is not installed")
  skip_if(length(.word_pids()) > 0, "a Word session is already open; will not disturb it")

  # A separate PowerShell holds a Word instance for a minute, as a user's
  # session would be; Word 2013+ may hand it to our New-Object call.
  holder <- tempfile(fileext = ".ps1")
  writeLines(
    c(
      "$w = New-Object -ComObject Word.Application",
      "Start-Sleep -Seconds 60"
    ),
    holder
  )
  docx <- .word_test_docx()
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(holder, docx, pdf), force = TRUE), add = TRUE)
  system2("powershell",
    c("-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass", "-File", shQuote(holder, type = "cmd")),
    wait = FALSE
  )
  existing <- integer(0)
  for (i in seq_len(40)) {
    existing <- .word_pids()
    if (length(existing) > 0) break
    Sys.sleep(0.5)
  }
  on.exit(
    for (p in .word_pids()) tools::pskill(p),
    add = TRUE, after = FALSE
  )
  skip_if(length(existing) == 0, "could not start a stand-in Word session")

  .word_docx_to_pdf(docx, pdf)
  expect_true(.is_pdf_file(pdf))
  expect_true(all(existing %in% .word_pids()))
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

  # The cover and Document Information tables carry the date as a cell.
  cells <- officer::docx_summary(officer::read_docx(out_docx))$text
  expect_true("2026-01-15" %in% cells)
  # The running footer carries it too, and no localized month name may survive
  # anywhere in the document.
  footer <- paste(.docx_part_texts(out_docx, "^footer[0-9]+[.]xml$"), collapse = " ")
  expect_match(footer, "2026-01-15", fixed = TRUE)
  expect_false(any(grepl("Januar|January|janvier", c(cells, footer))))
})

test_that("write_dta docx opens with the signature block, before any other chapter", {
  # The approval table is what a reader must act on, so it precedes Document
  # Information, the party sections and the datasets.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)

  summary <- officer::docx_summary(officer::read_docx(out_docx))
  h1 <- summary[summary$style_name %in% "heading 1", c("doc_index", "text")]
  idx <- function(txt) h1$doc_index[which(h1$text == txt)[1]]

  expect_identical(h1$text[[1]], "Approval & Signatures")
  for (later in c("Document Information", "Parties", "Datasets")) {
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

  # Every table header cell is filled with the single light house tint; key/value
  # label columns use the light grey.
  fills <- unique(unlist(regmatches(
    xml, gregexpr('(?<=<w:shd )[^>]*w:fill="[^"]+', xml, perl = TRUE)
  )))
  fills <- toupper(sub('.*w:fill="', "", fills))
  fills <- setdiff(fills, c("AUTO", "FFFFFF"))
  expect_setequal(
    fills,
    toupper(sub("^#", "", c(THEME_COLORS$primary_light, THEME_COLORS$gray_light)))
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
  h2 <- summary[summary$style_name %in% "heading 2", c("doc_index", "text")]
  idx_sup <- h2$doc_index[which(h2$text == "Supplier")[1]]
  idx_rec <- h2$doc_index[which(h2$text == "Receiver")[1]]
  expect_false(is.na(idx_sup))
  expect_false(is.na(idx_rec))
  expect_lt(idx_sup, idx_rec)

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out_md, warn = FALSE)
  expect_lt(
    grep("^### [0-9]+\\.1 Supplier$", md)[1],
    grep("^### [0-9]+\\.2 Receiver$", md)[1]
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
  tmpl <- system.file("extdata", "word-templates", "dta_numbered_template.docx", package = "DTAtools")
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

# ---- rules in the exported documents: prose only, never code ----------------
#
# Rules appear in Word and Markdown as the complete plain-English wording from
# R/rulePhrasing.R and in no other form: no YAML, no code fence, no code span,
# no monospace font. The expected sentences below are written out by hand from
# the wording specification at the top of R/rulePhrasing.R, never copied from
# rendered output.

# One rule of every type plus values that are awkward in a table cell, in
# Markdown and in YAML (backticks, quotes, a line break, a long value list).
.prose_fixture_rules <- function() {
  list(
    DTARuleColRange(id = "age_range", description = "Adult age", columns = "AGE", min = 18, max = 65),
    DTARuleColRange(id = "open_low", columns = "AGE", min = -Inf, max = 100),
    DTARuleColUnique(id = "unique_keys", columns = c("SUBJID", "VISIT")),
    DTARuleColCondition(
      id = "thirty_values",
      description = "Visit must be one of the planned visits",
      condition = list(SITE = list(`in` = sprintf("S%02d", 1:30)), AGE = list(greater_equal = 18, less = 65)),
      then = list(FLAG = list(equals = "yes"), N = list(equals = "N"))
    ),
    DTARuleColCondition(
      id = "pattern_chars",
      condition = list(CODE = list(pattern = "^(a|b)\\d+'x\"y$")),
      then = list(NOTE = list(equals = "line1\nline2"))
    ),
    DTARuleColCondition(
      id = "three_ticks",
      condition = list(CODE = list(pattern = "a```b")),
      then = list(NOTE = list(empty = TRUE))
    ),
    DTARuleGroupCondition(
      id = "group_all_scopes",
      description = "Every row of a subject",
      group_by = c("SUBJID", "VISIT"),
      conditions = list(
        a = list(X = list(empty = FALSE)),
        b = list(Y = list(equals = "yes"))
      ),
      constraints = list(
        list(
          type = "requires", `if` = "a", `then` = "b",
          if_scope = "all", then_scope = "all", id = "needs_b", message = "a needs b: always"
        ),
        list(type = "mutually_exclusive", left = "a", right = "b")
      )
    )
  )
}

# Hand-written statements that name every property of the fixture rules
# (other than type and id, which the table shows in its own column).
.prose_fixture_expected <- function() {
  sites <- paste0(
    paste0("\u201cS", sprintf("%02d", 1:29), "\u201d", collapse = ", "),
    " or \u201cS30\u201d"
  )
  list(
    age_range = c(
      "Purpose: Adult age",
      "Every value in column AGE must be a number from 18 to 65, inclusive."
    ),
    open_low = "Every value in column AGE must be a number that is at most 100.",
    unique_keys = "No two rows may have the same combination of values in columns SUBJID and VISIT.",
    thirty_values = c(
      "Purpose: Visit must be one of the planned visits",
      "When all of the following are true:",
      paste0("\u2022 SITE is one of ", sites),
      "\u2022 AGE is at least 18 and is less than 65",
      "Then all of the following must hold:",
      "\u2022 FLAG must be equal to \u201cyes\u201d",
      "\u2022 N must be equal to \u201cN\u201d"
    ),
    pattern_chars = paste0(
      "When CODE matches the pattern \u201c^(a|b)\\d+'x\"y$\u201d, ",
      "NOTE must be equal to \u201cline1[line break]line2\u201d."
    ),
    three_ticks = "When CODE matches the pattern \u201ca```b\u201d, NOTE must be empty.",
    group_all_scopes = c(
      "Purpose: Every row of a subject",
      paste(
        "Rows are grouped by the combination of values in columns SUBJID and VISIT;",
        "within each group, the requirements below must hold."
      ),
      "\u2022 a: a row in which X is not empty",
      "\u2022 b: a row in which Y is equal to \u201cyes\u201d",
      paste(
        "\u2022 needs_b: If every row of a group meets condition a, then every row of that",
        "group must meet condition b. A violation is reported with the message",
        "\u201ca needs b: always\u201d."
      ),
      paste(
        "Within a group, it must not happen that at least one row meets condition a and",
        "also at least one row meets condition b. A violation is reported with a message",
        "naming the group and its rows."
      )
    )
  )
}

.prose_fixture_dta <- function(rules = .prose_fixture_rules(), n_datasets = 1L) {
  # The collection checks that rules only name declared columns, so declare
  # every column the fixture rules use (free text, no value lists).
  cols <- lapply(
    c("SUBJID", "VISIT", "SITE", "FLAG", "N", "CODE", "NOTE", "X", "Y"),
    function(id) DTAColumnSpec(id = id, type = "SAS Char")
  )
  cols <- c(list(DTAColumnSpec(id = "AGE", type = "SAS Int")), cols)
  names(cols) <- vapply(cols, function(col) col@id, character(1))
  sets <- lapply(seq_len(n_datasets), function(k) {
    DTADataSetTabular(
      id = paste0("rules_fixture_", k),
      specs = DTAColumnSpecCollection(columns = cols, rules = rules)
    )
  })
  names(sets) <- vapply(sets, function(ds) ds@id, character(1))
  DTA(datasets = sets, metadata = create_example_DTAMetaData())
}

# Every rule of every tabular dataset, in document order.
.dta_rules <- function(dta) {
  unlist(
    lapply(dta@datasets, function(ds) {
      if (inherits(ds, "DTAtools::DTADataSetTabular")) ds@specs@rules else list()
    }),
    recursive = FALSE
  )
}

.rule_ids <- function(rules) vapply(rules, function(r) r@id, character(1), USE.NAMES = FALSE)

# Each table of a docx whose first cell is "Rule ID": all of its cells, its
# header cells and its body as a (rules x 2) character matrix. Cells are read
# positionally, two to a row, so a rule id equal to a header label cannot
# confuse it.
.docx_rules_tables <- function(path) {
  summary <- officer::docx_summary(officer::read_docx(path))
  cells <- summary[summary$content_type == "table cell", ]
  tables <- lapply(unique(cells$table_index), function(ti) {
    text <- cells$text[cells$table_index == ti]
    if (!identical(text[[1]], "Rule ID")) {
      return(NULL)
    }
    list(
      cells = text,
      header = text[1:2],
      body = matrix(text[-(1:2)], ncol = 2, byrow = TRUE),
      first_index = min(cells$doc_index[cells$table_index == ti])
    )
  })
  Filter(Negate(is.null), tables)
}

# Font (w:rFonts/@w:ascii) of every text run inside the rules tables of a docx;
# NA for a run that names no font.
.docx_rules_table_fonts <- function(path) {
  doc <- xml2::read_xml(.read_docx_body_xml(path))
  tables <- xml2::xml_find_all(doc, "//*[local-name()='tbl']")
  is_rules <- vapply(tables, function(tbl) {
    first <- xml2::xml_find_first(tbl, ".//*[local-name()='tc']")
    identical(xml2::xml_text(first), "Rule ID")
  }, logical(1))
  runs <- unlist(lapply(tables[is_rules], function(tbl) {
    xml2::xml_find_all(tbl, ".//*[local-name()='r'][*[local-name()='t']]")
  }), recursive = FALSE)
  vapply(runs, function(run) {
    f <- xml2::xml_find_all(
      run, "./*[local-name()='rPr']/*[local-name()='rFonts']/@*[local-name()='ascii']"
    )
    if (length(f) == 0) NA_character_ else xml2::xml_text(f[[1]])
  }, character(1))
}

# Assertions shared by every "no code" test: no YAML key line, no code fence,
# no backtick outside a quoted value, no non-breaking-space indentation.
.expect_no_rule_code <- function(text, info = NULL) {
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  expect_false(
    any(grepl("^\\s*(type|condition|conditions|then|columns|constraints|group_by|min|max):", lines)),
    info = info
  )
  expect_false(any(grepl("^\\s*`{3,}", lines)), info = info)
  expect_false(any(grepl("\u00a0", lines, fixed = TRUE)), info = info)
}

test_that("the docx rules table has two columns, Rule ID and the shared description", {
  rules <- .prose_fixture_rules()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(.prose_fixture_dta(rules), file = out, format = "docx", overwrite = TRUE, quiet = TRUE)

  tables <- .docx_rules_tables(out)
  expect_length(tables, 1)
  expect_identical(tables[[1]]$header, c("Rule ID", "Description"))
  expect_false("Definition" %in% tables[[1]]$cells)
  expect_identical(tables[[1]]$body[, 1], .rule_ids(rules))
  # The Word cell is the shared wording, line for line.
  for (i in seq_along(rules)) {
    expect_identical(tables[[1]]$body[i, 2], .rule_text(rules[[i]]), info = rules[[i]]@id)
  }
})

test_that("every property of a rule appears in its Word description", {
  rules <- .prose_fixture_rules()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(.prose_fixture_dta(rules), file = out, format = "docx", overwrite = TRUE, quiet = TRUE)

  body <- .docx_rules_tables(out)[[1]]$body
  cells <- stats::setNames(body[, 2], body[, 1])
  expected <- .prose_fixture_expected()
  for (id in names(expected)) {
    for (fragment in expected[[id]]) {
      expect_match(cells[[id]], fragment, fixed = TRUE, info = id)
    }
  }
})

test_that("no rule is written as code in the Word export", {
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(.prose_fixture_dta(), file = out, format = "docx", overwrite = TRUE, quiet = TRUE)

  # No run of the rules table is set in a monospace font.
  fonts <- .docx_rules_table_fonts(out)
  expect_gt(length(fonts), 0)
  expect_false(any(fonts %in% c(FONTS$monospace, "Consolas", "Courier", "Courier New")))
  # Without include_yaml the document holds no monospace text at all.
  xml <- .read_docx_body_xml(out)
  expect_false(grepl("Consolas|Courier", xml))

  cells <- .docx_rules_tables(out)[[1]]$body[, 2]
  for (cell in cells) .expect_no_rule_code(cell)
})

test_that("the Word rule descriptions use bold labels, not separate code", {
  rule <- DTARuleColRange(id = "age_range", description = "Adult age", columns = "AGE", min = 18, max = 65)
  ft <- .build_rules_table(list(rule))
  expect_identical(ft$col_keys, c("Rule ID", "Description"))
  # The composed cell: the labels are bold runs, the statements are not.
  chunks <- flextable::information_data_chunk(ft)
  chunks <- chunks[chunks$.part == "body" & chunks$.col_id == "Description", ]
  bold_text <- chunks$txt[chunks$bold %in% TRUE]
  expect_true(all(c("Purpose:", "Missing values:", "Non-numeric values:") %in% bold_text))
  expect_false(any(grepl("Every value", bold_text, fixed = TRUE)))
  # flextable reports a line-break chunk as "<br>".
  expect_identical(gsub("<br>", "\n", paste(chunks$txt, collapse = ""), fixed = TRUE), .rule_text(rule))
})

test_that("the reading guide appears once per Word document, before the first rules table", {
  guide <- .rule_reading_guide()
  check <- function(path) {
    summary <- officer::docx_summary(officer::read_docx(path))
    paras <- summary[summary$content_type == "paragraph", ]
    at <- paras$doc_index[paras$text == guide[[1]]]
    expect_length(at, 1)
    for (sentence in guide) {
      expect_identical(sum(paras$text == sentence), 1L, info = sentence)
    }
    tables <- .docx_rules_tables(path)
    expect_gt(length(tables), 0)
    expect_lt(at, min(vapply(tables, function(t) t$first_index, numeric(1))))
  }

  dta <- .prose_fixture_dta(n_datasets = 2L)
  out_dta <- tempfile(fileext = ".docx")
  out_ds <- tempfile(fileext = ".docx")
  on.exit(unlink(c(out_dta, out_ds), force = TRUE), add = TRUE)
  write_dta(dta, file = out_dta, format = "docx", overwrite = TRUE, quiet = TRUE)
  expect_length(.docx_rules_tables(out_dta), 2)
  check(out_dta)

  write_dataset_metadata(dta@datasets[[1]], file = out_ds, overwrite = TRUE, quiet = TRUE)
  check(out_ds)
})

test_that("a document without rules carries no reading guide", {
  dta <- .prose_fixture_dta(rules = list())
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(dta, file = out, format = "docx", overwrite = TRUE, quiet = TRUE)
  txt <- officer::docx_summary(officer::read_docx(out))$text
  expect_false(.rule_reading_guide()[[1]] %in% txt)
  expect_true("No validation rules specified." %in% txt)
})

test_that("the Markdown exports describe each rule in prose, with no code", {
  rules <- .prose_fixture_rules()
  dta <- .prose_fixture_dta(rules)
  out_dta <- tempfile(fileext = ".md")
  out_ds <- tempfile(fileext = ".md")
  on.exit(unlink(c(out_dta, out_ds), force = TRUE), add = TRUE)
  write_dta(dta, file = out_dta, format = "md", overwrite = TRUE, quiet = TRUE)
  write_dataset_metadata(dta@datasets[[1]], file = out_ds, format = "md", overwrite = TRUE, quiet = TRUE)

  # Rule headings sit one level below the Validation Rules chapter: level 5
  # in the DTA document (chapter 1.1.1), level 3 in the dataset document.
  for (case in list(list(path = out_dta, level = 5L), list(path = out_ds, level = 3L))) {
    path <- case$path
    md <- readLines(path, warn = FALSE, encoding = "UTF-8")
    text <- paste(md, collapse = "\n")
    .expect_no_rule_code(text, info = path)
    # Backticks only as escaped characters of a value, never as a code span.
    expect_false(any(grepl("(?<!\\\\)`", md, perl = TRUE)), info = path)
    expect_false(any(grepl("Exact rule definitions", md, fixed = TRUE)), info = path)
    # One block per rule, and the reading guide once, before the first block.
    for (rule in rules) {
      block <- paste(.md_rule_block(rule, level = case$level), collapse = "\n")
      expect_true(grepl(block, text, fixed = TRUE), info = paste(path, rule@id))
    }
    guide_at <- which(md == paste("-", .md_escape(.rule_reading_guide()[[1]])))
    expect_length(guide_at, 1)
    first_rule <- which(md == paste(strrep("#", case$level), .md_escape(rules[[1]]@id)))[[1]]
    expect_lt(guide_at, first_rule)
  }
})

test_that("every rule of the bundled clinical example is described in prose in docx and md", {
  yaml_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  skip_if(!nzchar(yaml_path) || !file.exists(yaml_path), "clinical_dta.yaml fixture missing")
  dta <- read_dta_from_yaml(yaml_path)
  rules <- .dta_rules(dta)
  expect_gt(length(rules), 0)

  out_docx <- tempfile(fileext = ".docx")
  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(c(out_docx, out_md), force = TRUE), add = TRUE)
  write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)

  tables <- .docx_rules_tables(out_docx)
  for (tbl in tables) {
    expect_identical(tbl$header, c("Rule ID", "Description"))
  }
  ids <- unlist(lapply(tables, function(t) t$body[, 1]))
  cells <- unlist(lapply(tables, function(t) t$body[, 2]))
  expect_identical(ids, .rule_ids(rules))
  expect_identical(unname(cells), unname(vapply(rules, .rule_text, character(1))))

  # The grouped rule is spelled out: grouping column, conditions, requirement.
  grouped <- cells[[match("group_condition_pass_example", ids)]]
  expect_match(grouped, "Rows are grouped by the value in column SUBJECT_ID;", fixed = TRUE)
  expect_match(grouped, "\u2022 c_has_visit: a row in which VISIT is not empty", fixed = TRUE)
  expect_match(
    grouped,
    paste(
      "\u2022 visit_requires_status: If at least one row of a group meets condition c_has_visit,",
      "then every row of that group must meet condition c_has_status."
    ),
    fixed = TRUE
  )
  # Multi-line descriptions become real Word line breaks.
  expect_true(grepl("<w:br/>", .read_docx_body_xml(out_docx), fixed = TRUE))

  md <- paste(readLines(out_md, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  .expect_no_rule_code(md)
  for (rule in rules) {
    expect_true(grepl(paste(.md_rule_block(rule), collapse = "\n"), md, fixed = TRUE), info = rule@id)
  }
})

test_that("a generated version-history summary does not break the Markdown table (regression guard)", {
  # dta_version_change_summary() (inst/shiny/dta_app/R/versioning.R) renders a
  # version_history entry's `changes` with literal quotes, "->" arrows and
  # "; "-joined detail lines. Only "|" and a real newline are special to a
  # GFM pipe table -- dta_version_sanitise() already strips both before this
  # text goes anywhere near an export -- so this pins that the REST of that
  # punctuation still leaves the Version History table exactly one row per
  # history entry, i.e. that .md_table() (R/exportMarkdown.R) is not
  # confused by it.
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

test_that("the numbered docx base has the heading styles the exporters use", {
  # write_dta() adds level-4 headings (.add_heading(level = 4)), and officer's
  # stock template has no 'heading 4' style: with it the export fails.
  doc <- .new_numbered_docx()

  expect_true("heading 4" %in% officer::styles_info(doc)$style_name)
  expect_no_error(.add_heading(doc, "Deep heading", level = 4))
})

test_that(".new_numbered_docx refuses to fall back to a template that lacks heading 4", {
  local_mocked_bindings(.numbered_docx_path = function() "")

  expect_error(.new_numbered_docx(), "reinstall", ignore.case = TRUE, class = "rlang_error")
})

test_that("a rule id with backticks or markup stays plain text in Markdown", {
  ids <- c("a`b", "`lead", "trail`", "x``y", "a|b*c")
  rules <- lapply(ids, function(id) DTARuleColUnique(id = id, columns = "SUBJID"))
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(.prose_fixture_dta(rules), file = out, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- readLines(out, warn = FALSE)
  # Each id is escaped character by character in the rule's heading (level 5,
  # below chapter 1.1.3 Validation Rules), its kind in italics beneath; no
  # code span, so no unescaped backtick anywhere.
  expected <- c(
    "##### a\\`b", "##### \\`lead", "##### trail\\`", "##### x\\`\\`y", "##### a\\|b\\*c"
  )
  for (label in expected) {
    expect_identical(sum(md == label), 1L, info = label)
    expect_identical(md[match(label, md) + 2L], "*Unique*", info = label)
  }
  expect_false(any(grepl("(?<!\\\\)`", md, perl = TRUE)))
})

# ---- document_name, document_abbreviation, organisation -----------------------

test_that("write_dta puts document_name first in Markdown, with the DTA title and organisation beneath", {
  dta <- create_example_DTA()
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(dta, out,
    format = "md", quiet = TRUE,
    document_name = c("Data Transmission Specification", "Data Transfer Agreement"),
    organisation = "Acme Pharma", organisation_unit = "Biometrics"
  )
  lines <- readLines(out, warn = FALSE)
  expect_equal(lines[1], "# Data Transmission Specification / Data Transfer Agreement")
  # Title in bold, organisation as a plain line beneath it: no "Label:" lines.
  expect_identical(lines[3], "**Example DTA**")
  expect_identical(lines[5], "Acme Pharma \u00b7 Biometrics")
  # Both sit in the header block, before the contents and the first chapter.
  expect_lt(5L, match("## Contents", lines))
})

test_that("write_dta shows nothing for a NULL or empty organisation", {
  dta <- create_example_DTA()
  plain <- tempfile(fileext = ".md")
  blank <- tempfile(fileext = ".md")
  on.exit(unlink(c(plain, blank), force = TRUE), add = TRUE)
  write_dta(dta, plain, format = "md", quiet = TRUE)
  write_dta(dta, blank, format = "md", quiet = TRUE, organisation = "", organisation_unit = "  ")
  # The document carries no time stamp, so the two files are byte-identical.
  expect_identical(readBin(blank, "raw", 1e6), readBin(plain, "raw", 1e6))
  expect_false(any(grepl("Organisation", readLines(plain, warn = FALSE), fixed = TRUE)))
})

test_that("the Word cover leads with the document name, then the DTA title, then the organisation", {
  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(dta, out,
    format = "docx", quiet = TRUE,
    document_name = "Data Transmission Specification",
    organisation = "Acme Pharma", organisation_unit = "Biometrics"
  )
  paras <- Filter(nzchar, .docx_paragraphs(out))
  expect_equal(paras[1:3], c(
    "Data Transmission Specification", "Example DTA",
    "Acme Pharma · Biometrics"
  ))
})

test_that("the Word cover shows only the document name when the DTA has no title", {
  dta <- DTA(datasets = create_example_DTADataSetTabular(1))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(dta, out, format = "docx", quiet = TRUE, document_name = "Data Transmission Specification")
  paras <- Filter(nzchar, .docx_paragraphs(out))
  expect_equal(paras[1], "Data Transmission Specification")
  expect_false("Data Transfer Agreement" %in% paras[1:2])
})

test_that("the organisation is in the running header of the pages after the cover, only when given", {
  dta <- create_example_DTA()
  with_org <- tempfile(fileext = ".docx")
  without <- tempfile(fileext = ".docx")
  on.exit(unlink(c(with_org, without), force = TRUE), add = TRUE)
  write_dta(dta, with_org, format = "docx", quiet = TRUE, organisation = "Acme Pharma", organisation_unit = "Biometrics")
  write_dta(dta, without, format = "docx", quiet = TRUE)
  headers <- .docx_part_texts(with_org, "^header")
  expect_gt(length(headers), 0)
  expect_true(all(grepl("Acme Pharma · Biometrics", headers, fixed = TRUE)))
  expect_false(any(grepl("Acme Pharma", .docx_part_texts(without, "^header"), fixed = TRUE)))
})

test_that("write_dta validates the document and organisation arguments", {
  dta <- create_example_DTA()
  out <- tempfile(fileext = ".md")
  expect_error(write_dta(dta, out, format = "md", organisation = c("a", "b")), "organisation")
  expect_error(write_dta(dta, out, format = "md", organisation = 1), "organisation")
  expect_error(write_dta(dta, out, format = "md", document_name = NA_character_), "document_name")
  expect_error(write_dta(dta, out, format = "md", document_abbreviation = c("a", "b")), "document_abbreviation")
  expect_false(file.exists(out))
})

test_that("export context does not leak from one export into the next", {
  dta <- create_example_DTA()
  first <- tempfile(fileext = ".md")
  second <- tempfile(fileext = ".md")
  on.exit(unlink(c(first, second), force = TRUE), add = TRUE)
  write_dta(dta, first, format = "md", quiet = TRUE, document_name = "Named", organisation = "Acme")
  write_dta(dta, second, format = "md", quiet = TRUE)
  expect_equal(readLines(second, warn = FALSE)[1], "# Data Transfer Agreement")
  expect_false(any(grepl("Acme", readLines(second, warn = FALSE), fixed = TRUE)))

  # The context itself is cleared on return ...
  expect_identical(dta_template_placeholders(dta)[["{ORGANISATION_NAME}"]], "")
  expect_identical(dta_template_placeholders(dta)[["{DOCUMENT_NAME}"]], "Data Transfer Agreement")

  # ... and when the writer aborts after the context was set (bad format).
  expect_error(
    write_dta(dta, first, format = "xyz", quiet = TRUE, organisation = "Acme", document_name = "Named"),
    "'format' must be"
  )
  expect_identical(dta_template_placeholders(dta)[["{ORGANISATION_NAME}"]], "")
  expect_identical(dta_template_placeholders(dta)[["{DOCUMENT_NAME}"]], "Data Transfer Agreement")
})

test_that("newlines in export text are collapsed, so they cannot inject Markdown", {
  dta <- create_example_DTA()
  md <- tempfile(fileext = ".md")
  on.exit(unlink(md, force = TRUE), add = TRUE)
  write_dta(dta, md, format = "md", quiet = TRUE, organisation = "Acme\n## Injected", document_name = "Doc\r\n# Evil")
  lines <- readLines(md, warn = FALSE)
  expect_false(any(grepl("^#+ (Injected|Evil)", lines)))
  expect_equal(lines[1], "# Doc \\# Evil") # the inline # is escaped, not a heading
  expect_true("Acme \\#\\# Injected" %in% lines)

  ds <- tempfile(fileext = ".md")
  on.exit(unlink(ds, force = TRUE), add = TRUE)
  write_dataset_metadata(create_example_DTADataSetTabular(), ds,
    format = "md", quiet = TRUE,
    organisation = "Acme\n## Injected"
  )
  dlines <- readLines(ds, warn = FALSE)
  expect_false(any(grepl("^## Injected", dlines)))
  expect_equal(dlines[3], "**Dataset Specification: example\\_container\\_specs\\_without\\_data**")
  expect_true("Acme \\#\\# Injected" %in% dlines)
})

test_that("write_dataset_metadata leads with document_name and shows the organisation", {
  ds <- create_example_DTADataSetTabular()
  md <- tempfile(fileext = ".md")
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(c(md, docx), force = TRUE), add = TRUE)
  write_dataset_metadata(ds, md,
    format = "md", quiet = TRUE,
    document_name = "Data Transmission Specification", organisation = "Acme Pharma"
  )
  lines <- readLines(md, warn = FALSE)
  expect_equal(lines[1], "# Data Transmission Specification")
  expect_identical(lines[5], "Acme Pharma")

  write_dataset_metadata(ds, docx,
    format = "docx", quiet = TRUE,
    document_name = "Data Transmission Specification", organisation = "Acme Pharma"
  )
  paras <- Filter(nzchar, .docx_paragraphs(docx))
  expect_equal(paras[1], "Data Transmission Specification")
  expect_match(paras[2], "^Dataset Specification: ")
  expect_equal(paras[3], "Acme Pharma")
})

test_that("a template export to a path relative to the working directory lands there (DEV-017)", {
  dir <- withr::local_tempdir()
  template <- .make_template("Title: {DTA_TITLE}")
  withr::defer(unlink(template, force = TRUE))
  withr::local_dir(dir)
  dta <- create_example_DTA()
  # The fixture's own title (R/DTAMetaData-class.R), not exporter output.
  expected <- "Title: Example DTA"

  export_with_template(dta, template, "relative.docx", quiet = TRUE, fallback = FALSE)
  expect_identical(.docx_paragraphs(file.path(dir, "relative.docx")), expected)

  write_dta(dta, file = "viawrite.docx", template = template, quiet = TRUE)
  expect_identical(.docx_paragraphs(file.path(dir, "viawrite.docx")), expected)
})
