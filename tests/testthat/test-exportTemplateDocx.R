# .make_template(), .docx_text() and .docx_paragraphs() live in helper-docx.R.

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

test_that("date placeholders render in ISO 8601, not a localized month name", {
  # OWNER'S DECISION: every date in an exported document is YYYY-MM-DD.
  # .tv_scalar() used to format Date values with "%B %d, %Y", whose month name
  # comes from LC_TIME, so the very same DTA produced "Januar 15, 2026" on a
  # German workstation and "January 15, 2026" on an English CI runner.
  ds <- create_example_DTADataSetTabular(2)
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData(2))
  vars <- .extract_template_variables(dta)

  expect_equal(vars[["{DTA_DATE}"]], "2026-01-15")
  expect_equal(vars[["{TRANSMISSION_FIRST_TRANSFER}"]], "2026-02-01")
  expect_equal(vars[["{TRANSMISSION_LAST_TRANSFER}"]], "2026-03-31")
  # {GENERATED_DATE} is "today", so pin its shape and its agreement with the
  # ISO format string rather than a fixed literal.
  expect_equal(vars[["{GENERATED_DATE}"]], format(Sys.Date(), "%Y-%m-%d"))
  expect_match(vars[["{GENERATED_DATE}"]], "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")

  # create_example_DTADataSetTabular(2) declares no validation rules.
  expect_equal(length(ds@specs@rules), 0L)
  expect_equal(vars[["{TOTAL_RULES}"]], "0")

  # The same values must reach the rendered document.
  template <- .make_template(c(
    "Agreement date: {DTA_DATE}",
    "Window: {TRANSMISSION_FIRST_TRANSFER} to {TRANSMISSION_LAST_TRANSFER}",
    "Generated: {GENERATED_DATE}",
    "Rules: {TOTAL_RULES}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  export_with_template(dta, template, out, quiet = TRUE)

  text <- .docx_text(out)
  expect_match(text, "Agreement date: 2026-01-15", fixed = TRUE)
  expect_match(text, "Window: 2026-02-01 to 2026-03-31", fixed = TRUE)
  expect_match(text, paste0("Generated: ", format(Sys.Date(), "%Y-%m-%d")), fixed = TRUE)
  expect_match(text, "Rules: 0", fixed = TRUE)
})

test_that("date placeholders are identical under English and non-English LC_TIME", {
  # The whole point of the ISO decision: the rendered agreement must not depend
  # on the workstation's LC_TIME. This fails against the old "%B %d, %Y" code.
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)

  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = create_example_DTAMetaData(2)
  )

  # Whichever non-English time locale this machine offers. The assertions hold
  # in every locale, so nothing is skipped when none can be set.
  for (loc in c("de_DE.UTF-8", "German_Germany.1252", "fr_FR.UTF-8", "French_France.1252")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", loc)))) break
  }
  vars_local <- .extract_template_variables(dta)

  expect_equal(vars_local[["{DTA_DATE}"]], "2026-01-15")
  expect_equal(vars_local[["{TRANSMISSION_FIRST_TRANSFER}"]], "2026-02-01")
  expect_match(vars_local[["{GENERATED_DATE}"]], "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  expect_false(grepl("[A-Za-z]", vars_local[["{DTA_DATE}"]]))

  # ... and identical to what the C locale produces.
  Sys.setlocale("LC_TIME", "C")
  vars_c <- .extract_template_variables(dta)
  expect_identical(vars_local[["{DTA_DATE}"]], vars_c[["{DTA_DATE}"]])
  expect_identical(
    vars_local[["{TRANSMISSION_LAST_TRANSFER}"]],
    vars_c[["{TRANSMISSION_LAST_TRANSFER}"]]
  )
  expect_identical(vars_local[["{GENERATED_DATE}"]], vars_c[["{GENERATED_DATE}"]])
})

test_that("TRUE transmission flags render Yes and phrase dates pass through", {
  # create_example_DTAMetaData(3) is the only fixture that sets test_upload and
  # blinded_transfer to TRUE. Without it an implementation that returned a
  # constant "No" would satisfy the entire suite.
  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = create_example_DTAMetaData(3)
  )
  vars <- .extract_template_variables(dta)

  expect_equal(vars[["{TEST_UPLOAD}"]], "Yes")
  expect_equal(vars[["{BLINDED_TRANSFER}"]], "Yes")

  # Transfer "dates" are frequently free-text phrases rather than Date objects;
  # .tv_scalar() must pass those through verbatim and not coerce or reformat.
  expect_equal(vars[["{TRANSMISSION_FIRST_TRANSFER}"]], "2 weeks after approval")
  expect_equal(vars[["{TRANSMISSION_LAST_TRANSFER}"]], "Final transfer by 2026-12-31")
  expect_equal(vars[["{TRANSMISSION_TYPE}"]], "Secure cloud storage")
  expect_equal(vars[["{TRANSMISSION_FREQUENCY}"]], "Monthly transfers")

  template <- .make_template(c(
    "Test upload: {TEST_UPLOAD}",
    "Blinded: {BLINDED_TRANSFER}",
    "First transfer: {TRANSMISSION_FIRST_TRANSFER}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  export_with_template(dta, template, out, quiet = TRUE)

  text <- .docx_text(out)
  expect_match(text, "Test upload: Yes", fixed = TRUE)
  expect_match(text, "Blinded: Yes", fixed = TRUE)
  expect_match(text, "First transfer: 2 weeks after approval", fixed = TRUE)
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

test_that("braces arriving from a substituted value are left alone", {
  # Substitution is a single pass over the original text, so a value that
  # legitimately contains braces is emitted verbatim: never re-substituted, and
  # never reported as a placeholder the template did not contain.
  res <- .substitute_placeholder_text(
    "T: {DTA_TITLE}",
    list("{DTA_TITLE}" = "Study {ARM_A}")
  )
  expect_equal(res$text, "T: Study {ARM_A}")
  expect_length(res$unresolved, 0)

  # The same end to end: a DTA whose title contains braces exports cleanly and
  # warns about nothing.
  template <- .make_template("Title: {DTA_TITLE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(title = "Study {A} vs {B}", version = "1.0")
  )
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  expect_no_warning(export_with_template(dta, template, out, quiet = TRUE))
  # The document itself is fine: the title is rendered verbatim.
  expect_match(.docx_text(out), "Title: Study {A} vs {B}", fixed = TRUE)
})

test_that("the placeholder catalogue and the extracted variables cannot drift", {
  # The catalogue is what dta_template_placeholders() and the roxygen advertise;
  # .extract_template_variables() is what actually gets substituted. If the two
  # disagree, the package documents a placeholder it never fills, or fills one
  # nobody can discover.
  dta <- create_example_DTA()
  expect_setequal(
    names(.tv_placeholder_catalog()),
    names(.extract_template_variables(dta))
  )
})

test_that("dta_template_placeholders reports tokens and resolves them", {
  tokens <- dta_template_placeholders()
  expect_true("{DTA_TITLE}" %in% names(tokens))
  # With no DTA the values are descriptions, not resolved text.
  expect_false(any(grepl("^\\{", unname(tokens))))

  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(
      title = "A Study",
      version = "2.0",
      supplier = list(affiliation = list(name = "Acme Labs"))
    )
  )
  resolved <- dta_template_placeholders(dta)
  expect_equal(unname(resolved[["{DTA_TITLE}"]]), "A Study")
  expect_equal(unname(resolved[["{SUPPLIER_NAME}"]]), "Acme Labs")
  expect_setequal(names(resolved), names(tokens))

  expect_error(dta_template_placeholders("not a dta"), "must be a DTA object")
})

test_that("run formatting survives a placeholder that sits inside one run", {
  # "Vendor: " (plain) + "{SUPPLIER_NAME}" (bold) + " (confidential)" (plain).
  # The placeholder is wholly inside the bold run, so substitution must happen
  # run by run and every run must keep its own formatting. The old
  # implementation joined the paragraph, wrote the result into run 1 and blanked
  # the rest, which silently discarded the bold and the trailing plain run.
  template <- .make_template_rich(list(
    list(text = "Vendor: ", bold = FALSE),
    list(text = "{SUPPLIER_NAME}", bold = TRUE),
    list(text = " (confidential)", bold = FALSE)
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(
      title = "T",
      version = "1.0",
      supplier = list(affiliation = list(name = "Acme Labs"))
    )
  )
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  expect_match(.docx_text(out), "Vendor: Acme Labs (confidential)", fixed = TRUE)
  # The substituted value is still the bold run -- and the only bold run.
  expect_equal(.docx_bold_run_texts(out), "Acme Labs")
})

test_that("a value containing another key's token is not re-substituted", {
  # The old implementation mutated `text` in place while looping over keys, so
  # a value that happened to contain another key's token was substituted again
  # on a later iteration. Here the literal {DTA_VERSION} inside the title value
  # must survive, while the real {DTA_VERSION} in the template is filled.
  res <- .substitute_placeholder_text(
    "T: {DTA_TITLE} V: {DTA_VERSION}",
    list(
      "{DTA_TITLE}" = "Report referencing {DTA_VERSION}",
      "{DTA_VERSION}" = "3.0"
    )
  )
  expect_equal(res$text, "T: Report referencing {DTA_VERSION} V: 3.0")
  expect_length(res$unresolved, 0)
})

test_that(".tv_escape_regex escapes every metacharacter it claims to", {
  # REGRESSION: the first attempt at this was a gsub whose own pattern put {}
  # inside a character class, which TRE rejects with "Invalid contents of {}",
  # taking out every template substitution test in the suite.
  escaped <- .tv_escape_regex("a.b{c}d[e]f(g)h|i^j$k*l+m?n\\o")
  expect_match(escaped, "a\\.b", fixed = TRUE)
  # The escaped form must compile AND match the original literally.
  expect_true(grepl(escaped, "a.b{c}d[e]f(g)h|i^j$k*l+m?n\\o", perl = TRUE))
  expect_false(grepl(escaped, "aXbXcXd", perl = TRUE))
  expect_equal(.tv_escape_regex(character(0)), character(0))
})

test_that("a variable name containing regex metacharacters is still substituted", {
  # `variables` names are caller-supplied and arbitrary. \Q...\E quoting stops
  # at a literal \E, which dropped such a key from the match set and then
  # reported it as an unresolved placeholder instead of substituting it.
  res <- .substitute_placeholder_text(
    "A: {A\\Eb} B: {C.D}",
    list("{A\\Eb}" = "one", "{C.D}" = "two")
  )
  expect_equal(res$text, "A: one B: two")
  expect_length(res$unresolved, 0)
})

test_that("mixed-case placeholders with no value are reported as unresolved", {
  # The paragraph gate and the leftover scan used to use different character
  # classes, so a lower- or mixed-case token was left untouched AND never
  # warned about, contradicting the documented warning contract.
  res <- .substitute_placeholder_text(
    "Hello {customField} and {DTA_TITLE}",
    list("{DTA_TITLE}" = "X")
  )
  expect_equal(res$text, "Hello {customField} and X")
  expect_equal(res$unresolved, "{customField}")
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

  # fallback = TRUE (default) with quiet = TRUE: the fallback happens silently.
  expect_no_warning(
    export_with_template(dta, bad_template, out, quiet = TRUE, fallback = TRUE)
  )
  expect_true(file.exists(out))
  expect_s3_class(officer::read_docx(out), "rdocx")

  # "A valid rdocx" is satisfied by an empty document, so assert that the
  # fallback really produced the standard write_dta() layout: the DTA title,
  # the metadata and dataset sections, the numbered heading hierarchy, and the
  # metadata/spec tables.
  paragraphs <- .docx_paragraphs(out)
  expect_true("Example DTA" %in% paragraphs)
  expect_true("Data Transfer Agreement Metadata" %in% paragraphs)
  expect_true("Dataset Specifications" %in% paragraphs)
  summary <- officer::docx_summary(officer::read_docx(out))
  expect_true(all(paste("heading", 1:4) %in% summary$style_name))
  expect_true(any(!is.na(summary$table_index)))

  # quiet = FALSE: the fallback must be announced as a real warning CONDITION
  # (cli::cli_warn), not a cli message, so a caller can trap it.
  out_loud <- tempfile(fileext = ".docx")
  on.exit(unlink(out_loud, force = TRUE), add = TRUE)
  # suppressMessages() wraps the outside so the cli alerts this call emits
  # ("Document saved to ...") do not leak into the test log.
  suppressMessages(
    expect_warning(
      export_with_template(dta, bad_template, out_loud, quiet = FALSE, fallback = TRUE),
      "Falling back to the standard document format"
    )
  )
  expect_true(file.exists(out_loud))

  # fallback = FALSE: the failure is raised
  out2 <- tempfile(fileext = ".docx")
  on.exit(unlink(out2, force = TRUE), add = TRUE)
  expect_error(
    export_with_template(dta, bad_template, out2, quiet = TRUE, fallback = FALSE),
    "Template processing failed"
  )
})

test_that("the template fallback is detectable programmatically", {
  # The Shiny app (and any script) must be able to notice that the requested
  # template was silently replaced by the built-in layout. cli_alert_warning()
  # only signalled a message of class "cliMessage", which no
  # withCallingHandlers(warning = ) / tryCatch(warning = ) can see.
  bad_template <- tempfile(fileext = ".docx")
  writeLines("this is not a docx", bad_template)
  on.exit(unlink(bad_template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- NULL
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, bad_template, out, quiet = FALSE, fallback = TRUE),
      warning = function(w) {
        caught <<- w
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_s3_class(caught, "warning")
  expect_s3_class(caught, "rlang_warning")
  expect_match(conditionMessage(caught), "Falling back to the standard document format")

  # tryCatch(warning = ) sees it too, which is what a Shiny observer uses.
  trapped <- suppressMessages(tryCatch(
    export_with_template(dta, bad_template, out, quiet = FALSE, fallback = TRUE),
    warning = function(w) "trapped"
  ))
  expect_equal(trapped, "trapped")

  # quiet = TRUE keeps its meaning: the fallback still happens, silently.
  out_quiet <- tempfile(fileext = ".docx")
  on.exit(unlink(out_quiet, force = TRUE), add = TRUE)
  expect_no_warning(
    export_with_template(dta, bad_template, out_quiet, quiet = TRUE, fallback = TRUE)
  )
  expect_true(file.exists(out_quiet))
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

  # The only fixture with validation rules, so the only place where a non-zero
  # {TOTAL_RULES} / {TOTAL_COLUMNS} is exercised.
  expect_equal(
    vars[["{TOTAL_RULES}"]],
    as.character(sum(vapply(dta@datasets, function(d) length(d@specs@rules), integer(1))))
  )
  expect_equal(vars[["{TOTAL_RULES}"]], "6")
  expect_equal(vars[["{TOTAL_COLUMNS}"]], "14")
})
