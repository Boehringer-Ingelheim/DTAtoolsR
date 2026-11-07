# Page furniture, cover and field coverage of the built-in write_dta() layout.

.layout_dta <- function() {
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  m <- dta@metadata
  m@version <- "0.2"
  m@date <- as.Date("2024-06-19")
  m@study <- list(id = "ST-9", protocol_id = "P-77")
  m@template <- list(id = "biomarker_gf", label = "Biomarker GF DTS", version = "1.3", date = "2026-11-05")
  m@transmission$vendor_extension <- list(portal = "sftp.example.org", retention = list(days = "30"))
  dta@metadata <- m
  ds <- dta@datasets[[1]]
  ds@template_version <- "2.1"
  dta@datasets[[1]] <- ds
  dta
}

.layout_docx <- function(dta = .layout_dta()) {
  out <- tempfile(fileext = ".docx")
  write_dta(dta, out, quiet = TRUE)
  out
}


test_that("every page after the cover carries the running header and footer", {
  out <- .layout_docx()
  on.exit(unlink(out), add = TRUE)

  headers <- .docx_part_texts(out, "^header[0-9]+\\.xml$")
  expect_gt(length(headers), 0)
  for (h in headers) {
    expect_match(h, "Clinical Data Specification", fixed = TRUE)
    expect_match(h, "Study ST-9", fixed = TRUE)
    expect_match(h, "Page\\s*PAGE\\s*of\\s*NUMPAGES")
  }

  footers <- .docx_part_texts(out, "^footer[0-9]+\\.xml$")
  expect_gt(length(footers), 0)
  for (f in footers) {
    expect_match(f, "Clinical Data Specification", fixed = TRUE)
    expect_match(f, "Version 0.2", fixed = TRUE)
    expect_match(f, "2024-06-19", fixed = TRUE)
    expect_match(f, "Template: Biomarker GF DTS (biomarker_gf) · version 1.3 · 2026-11-05", fixed = TRUE)
  }

  # The cover is the first section and has no furniture; every later section
  # (portrait and landscape) names a default AND an even-page header and
  # footer, because officer turns on evenAndOddHeaders.
  sect <- xml2::xml_find_all(
    xml2::read_xml(.read_docx_body_xml(out)),
    ".//*[local-name()='sectPr']"
  )
  refs <- lapply(sect, function(s) {
    nodes <- xml2::xml_find_all(s, "./*[local-name()='headerReference' or local-name()='footerReference']")
    paste(xml2::xml_name(nodes), xml2::xml_attr(nodes, "type"))
  })
  expect_length(refs[[1]], 0)
  for (r in refs[-1]) {
    expect_setequal(r, c(
      "headerReference default", "headerReference even",
      "footerReference default", "footerReference even"
    ))
  }
})

test_that("the footer names the built-in layout when the DTA has no template", {
  dta <- .layout_dta()
  dta@metadata@template <- list()
  dta@metadata@study <- list()
  out <- .layout_docx(dta)
  on.exit(unlink(out), add = TRUE)
  expect_match(
    .docx_part_texts(out, "^footer[0-9]+\\.xml$")[[1]],
    paste0("Built-in layout · DTAtools ", utils::packageVersion("DTAtools")),
    fixed = TRUE
  )
  expect_false(grepl("Study", .docx_part_texts(out, "^header[0-9]+\\.xml$")[[1]], fixed = TRUE))
})

test_that("the template line omits parts the provenance does not record", {
  expect_identical(
    .template_provenance_text(list(id = "t1", label = "T One", date = "2026-01-02")),
    paste0("Template: T One (t1) ", intToUtf8(0xB7), " 2026-01-02")
  )
  expect_identical(.template_provenance_text(list(id = "t1")), "Template: t1")
})

test_that("export_with_template keeps its tables at their built widths", {
  specs <- create_example_DTAColumnSpecCollection(2)
  natural <- sum(.build_column_specs_table(specs, width = NULL)$body$colwidths)
  expect_equal(sum(.build_column_specs_table(specs)$body$colwidths), TEXT_WIDTH$landscape)
  expect_false(isTRUE(all.equal(natural, TEXT_WIDTH$landscape)))
})

test_that("chapters are top-level headings and the title opens the cover", {
  out <- .layout_docx()
  on.exit(unlink(out), add = TRUE)
  s <- officer::docx_summary(officer::read_docx(out))
  cover <- Filter(nzchar, s$text[s$content_type == "paragraph"])
  expect_identical(cover[1], "Data Transfer Agreement") # the document name
  expect_identical(cover[2], "Clinical Data Specification") # the DTA title beneath it
  h1 <- s$text[s$style_name %in% "heading 1"]
  expect_identical(
    h1,
    c(
      "Approval & Signatures", "Document Information", "Study", "Parties",
      "Process Information", "Datasets"
    )
  )
  h2 <- s$text[s$style_name %in% "heading 2"]
  expect_true(all(c("Supplier", "Receiver", "Transmission Details", "Clinical data table (clinical_data)") %in% h2))
  h3 <- s$text[s$style_name %in% "heading 3"]
  expect_identical(unique(h3), c("Files", "Column Specifications", "Validation Rules"))
})

test_that("fields the YAML carries all reach the document", {
  out <- .layout_docx()
  on.exit(unlink(out), add = TRUE)
  text <- .docx_text(out)
  for (value in c(
    "456 Elm St, Townsville", # contact address
    "Company: Test Company", # contact extension field
    "SAS Char (10)", # column length
    "UTF-8", # file encoding
    "Comma ( , )", # delimiter
    "2.1", # dataset template version
    "Biomarker GF DTS (biomarker_gf)", # template provenance
    "First transfer", # labelled transmission key
    "Portal: sftp.example.org", # nested extension value, every leaf
    "Days: 30"
  )) {
    expect_match(text, value, fixed = TRUE)
  }
  expect_false(grepl("date_first_transfer", text, fixed = TRUE))
})

test_that("the column table lists every allowed value and the length", {
  # Previously cut off after ten values with "...": a specification that hides
  # its eleventh permitted value is wrong.
  values <- sprintf("V%02d", 1:15)
  specs <- DTAColumnSpecCollection(columns = list(
    DTAColumnSpec(id = "VISIT", type = "SAS Char", length = 3, values = values),
    DTAColumnSpec(id = "CODE", type = "SAS Char", pattern = "^[A-Z]{2}$")
  ))
  cells <- .build_column_specs_table(specs)$body$dataset
  expect_identical(cells$Type, c("SAS Char (3)", "SAS Char"))
  expect_identical(cells$`Allowed Values / Pattern`, c(paste(values, collapse = ", "), "Pattern: ^[A-Z]{2}$"))
})

test_that("the bundled template numbers headings flush left", {
  tpl <- system.file("extdata", "word-templates", "dta_numbered_template.docx", package = "DTAtools")
  ex <- tempfile()
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(tpl, files = "word/numbering.xml", exdir = ex)
  xml <- xml2::read_xml(file.path(ex, "word", "numbering.xml"))
  lvls <- xml2::xml_find_all(
    xml,
    ".//*[local-name()='abstractNum'][@*[local-name()='abstractNumId']='0']/*[local-name()='lvl']"
  )
  expect_length(lvls, 9)
  ind <- xml2::xml_find_all(lvls, "./*[local-name()='pPr']/*[local-name()='ind']")
  expect_true(all(xml2::xml_attr(ind, "w:left", ns = xml2::xml_ns(xml)) == "0"))
  suff <- xml2::xml_find_all(lvls, "./*[local-name()='suff']")
  expect_true(all(xml2::xml_attr(suff, "w:val", ns = xml2::xml_ns(xml)) == "space"))
  expect_length(suff, 9)
})
