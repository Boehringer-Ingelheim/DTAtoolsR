# Build a minimal Word template containing the given lines as paragraphs.
.make_template <- function(lines) {
  path <- tempfile(fileext = ".docx")
  doc <- officer::read_docx()
  for (ln in lines) {
    doc <- officer::body_add_par(doc, ln, style = "Normal")
  }
  print(doc, target = path)
  path
}

# Return the concatenated visible text of a Word document.
.docx_text <- function(path) {
  summary <- officer::docx_summary(officer::read_docx(path))
  paste(summary$text, collapse = "\n")
}

test_that(".extract_template_variables maps DTA metadata to placeholders", {
  ds <- create_example_DTADataSetTabular(2)
  n_cols <- length(ds@specs@columns)
  dta <- DTA(
    datasets = list(ds),
    metadata = create_example_DTAMetaData(2)
  )

  vars <- .extract_template_variables(dta)

  expect_type(vars, "list")
  expect_true(all(grepl("^\\{.*\\}$", names(vars))))

  expect_equal(vars[["{DTA_TITLE}"]], "Clinical Data Transfer Agreement")
  expect_equal(vars[["{DTA_VERSION}"]], "2.0")
  expect_equal(vars[["{DTA_HEADER}"]], "Boehringer Ingelheim")

  expect_equal(vars[["{SUPPLIER_NAME}"]], "Supplier Company Inc.")
  expect_equal(vars[["{SUPPLIER_COUNTRY}"]], "Germany")
  expect_equal(vars[["{SUPPLIER_ADDRESS}"]], "123 Data Street, City")
  expect_equal(vars[["{SUPPLIER_EMAIL}"]], "emily.turner@supplier.com")
  expect_equal(vars[["{SUPPLIER_CONTACTS}"]], "Emily Turner")

  expect_equal(vars[["{RECEIVER_NAME}"]], "Test Company")
  expect_equal(vars[["{RECEIVER_COUNTRY}"]], "USA")
  expect_equal(vars[["{RECEIVER_CONTACTS}"]], "Alice Smith, Bob Johnson")

  expect_equal(vars[["{TRANSMISSION_TYPE}"]], "Secure SFTP server")
  expect_equal(vars[["{TRANSMISSION_FREQUENCY}"]], "One-time transfer")
  expect_equal(vars[["{TEST_UPLOAD}"]], "No")
  expect_equal(vars[["{BLINDED_TRANSFER}"]], "No")

  expect_equal(vars[["{DATASET_COUNT}"]], "1")
  expect_equal(vars[["{DATASET_TYPES}"]], "tabular")
  expect_equal(vars[["{TOTAL_COLUMNS}"]], as.character(n_cols))
  expect_equal(vars[["{AUTHORIZED_CORRECTIONS}"]], "Alice Smith, Bob Johnson")
  expect_match(vars[["{VERSION_HISTORY}"]], "1.0 \\(2025-10-01\\)")
})

test_that(".extract_template_variables is robust to empty metadata", {
  ds <- create_example_DTADataSetTabular(2)
  dta <- DTA(datasets = list(ds), metadata = DTAMetaData())

  vars <- .extract_template_variables(dta)

  expect_equal(vars[["{DTA_TITLE}"]], "")
  expect_equal(vars[["{SUPPLIER_NAME}"]], "")
  expect_equal(vars[["{RECEIVER_CONTACTS}"]], "")
  expect_equal(vars[["{DATASET_COUNT}"]], "1")
  expect_equal(vars[["{TEST_UPLOAD}"]], "No")
})

test_that("export_with_template fills placeholders in a Word template", {
  template <- .make_template(c(
    "Title: {DTA_TITLE}",
    "Version: {DTA_VERSION}",
    "Supplier: {SUPPLIER_NAME}",
    "Receiver contacts: {RECEIVER_CONTACTS}",
    "Datasets: {DATASET_COUNT} ({DATASET_TYPES})"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = create_example_DTAMetaData(2)
  )
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  res <- export_with_template(dta, template, out, quiet = TRUE)
  expect_equal(res, out)
  expect_true(file.exists(out))

  text <- .docx_text(out)
  expect_match(text, "Title: Clinical Data Transfer Agreement")
  expect_match(text, "Version: 2.0")
  expect_match(text, "Supplier: Supplier Company Inc.", fixed = TRUE)
  expect_match(text, "Receiver contacts: Alice Smith, Bob Johnson")
  expect_match(text, "Datasets: 1 (tabular)", fixed = TRUE)
  # Placeholders must be gone
  expect_false(grepl("\\{DTA_TITLE\\}", text))
})

test_that("export_with_template escapes XML special characters", {
  template <- .make_template("Study: {DTA_TITLE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  special <- "R&D <Clinical> \"Data\" 'trial'"
  export_with_template(
    dta,
    template,
    out,
    variables = list(DTA_TITLE = special),
    quiet = TRUE
  )

  # The document must remain a valid DOCX and contain the literal text
  expect_s3_class(officer::read_docx(out), "rdocx")
  expect_match(.docx_text(out), special, fixed = TRUE)
})

test_that("user variables override extracted values and add new placeholders", {
  template <- .make_template(c("A: {DTA_TITLE}", "B: {CUSTOM_FIELD}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(
    dta,
    template,
    out,
    variables = list("{DTA_TITLE}" = "Overridden", CUSTOM_FIELD = "Extra"),
    quiet = TRUE
  )

  text <- .docx_text(out)
  expect_match(text, "A: Overridden")
  expect_match(text, "B: Extra")
})

test_that(".replace_placeholders_in_xml handles placeholders split across runs", {
  xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:body><w:p>",
    "<w:r><w:t>Title: {DTA_</w:t></w:r>",
    "<w:r><w:t>TITLE}</w:t></w:r>",
    "</w:p></w:body></w:document>"
  )
  path <- tempfile(fileext = ".xml")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeLines(xml, path)

  unresolved <- .replace_placeholders_in_xml(path, list("{DTA_TITLE}" = "Hello"))
  expect_length(unresolved, 0)

  d <- xml2::read_xml(path)
  txt <- paste(
    xml2::xml_text(xml2::xml_find_all(d, ".//*[local-name()='t']")),
    collapse = ""
  )
  expect_equal(txt, "Title: Hello")
})

test_that("export_with_template warns about unresolved placeholders", {
  template <- .make_template(c("Known: {DTA_TITLE}", "Unknown: {NOT_A_FIELD}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  expect_warning(
    export_with_template(dta, template, out, quiet = TRUE),
    "placeholders"
  )
  expect_match(.docx_text(out), "Unknown: \\{NOT_A_FIELD\\}")
})

test_that("export_with_template validates its inputs", {
  dta <- create_example_DTA()

  expect_error(
    export_with_template(list(), tempfile(fileext = ".docx"), tempfile()),
    "must be a DTA object"
  )
  expect_error(
    export_with_template(dta, tempfile(fileext = ".docx"), tempfile(fileext = ".docx")),
    "Template file not found"
  )

  txt_template <- tempfile(fileext = ".txt")
  file.create(txt_template)
  on.exit(unlink(txt_template, force = TRUE), add = TRUE)
  expect_error(
    export_with_template(dta, txt_template, tempfile(fileext = ".docx")),
    "must be a"
  )
})

test_that("export_with_template falls back to the standard layout on failure", {
  # A .docx that is not a valid ZIP archive triggers the fallback path
  bad_template <- tempfile(fileext = ".docx")
  writeLines("this is not a docx", bad_template)
  on.exit(unlink(bad_template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  # fallback = TRUE (default): a valid document is still produced
  suppressWarnings(
    export_with_template(dta, bad_template, out, quiet = TRUE, fallback = TRUE)
  )
  expect_true(file.exists(out))
  expect_s3_class(officer::read_docx(out), "rdocx")

  # fallback = FALSE: the failure is raised
  out2 <- tempfile(fileext = ".docx")
  on.exit(unlink(out2, force = TRUE), add = TRUE)
  expect_error(
    export_with_template(dta, bad_template, out2, quiet = TRUE, fallback = FALSE),
    "Template processing failed"
  )
})

test_that("write_dta routes to the template engine when template is supplied", {
  template <- .make_template(c("Title: {DTA_TITLE}", "Version: {DTA_VERSION}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  res <- write_dta(
    dta,
    file = out,
    template = template,
    overwrite = TRUE,
    quiet = TRUE
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  expect_match(.docx_text(out), "Title: Example DTA")
})

test_that("write_dta rejects templates for markdown output", {
  dta <- create_example_DTA()
  expect_error(
    write_dta(
      dta,
      file = tempfile(fileext = ".md"),
      format = "md",
      template = "dummy.docx"
    ),
    "only supported for 'docx' or 'pdf'"
  )
})

test_that("template variables are extracted from a bundled YAML fixture", {
  yaml_file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  skip_if(yaml_file == "", "clinical_dta.yaml fixture not available")

  dta <- read_dta_from_yaml(yaml_file)
  vars <- .extract_template_variables(dta)

  expect_equal(vars[["{DTA_TITLE}"]], "Clinical Data Specification")
  expect_equal(vars[["{RECEIVER_NAME}"]], "Test Company")
  expect_equal(vars[["{SUPPLIER_NAME}"]], "Test Company 2")
  expect_equal(vars[["{SUPPLIER_COUNTRY}"]], "Test Country")
  expect_match(vars[["{DATASET_NAMES}"]], "clinical_data")
  expect_equal(vars[["{TRANSMISSION_TYPE}"]], "secure S3 bucket")
})
