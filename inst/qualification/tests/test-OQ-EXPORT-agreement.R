qagreement_export_transmission <- function() {
  list(
    extent = paste0(
      "Deliver all cumulative records from the agreed collection period, including corrections ",
      "to previously delivered records and complete records for every scheduled and unscheduled visit. ",
      "The receiving team must receive the entire statement, including this final scope clause."
    ),
    scope = "All planned and unscheduled visits.",
    packaging = "One archive per delivery with a file inventory.",
    security = "Agreed secure channel",
    checksum = "Provide a SHA-256 digest for every payload.",
    test_requirements = "Use a synthetic sample before the first live delivery.",
    blinding_requirements = "Keep masked treatment assignments out of the transfer.",
    legacy_vendor_extension = "Retain the legacy vendor instruction without dropping its final clause."
  )
}

qagreement_export_dta <- function() {
  DTA(
    metadata = DTAMetaData(
      title = "Agreement export probe",
      study = list(id = "DEMO-001", protocol_id = "P-02"),
      sections = list(
        z_scope = list(title = "Scope clause", text = "First paragraph.\n\nSecond paragraph."),
        a_review = list(title = "Review clause", text = "Keep {literal} text.")
      ),
      transmission = qagreement_export_transmission()
    ),
    datasets = list(DTADataSetTabular(
      id = "probe_data",
      specs = DTAColumnSpecCollection(columns = list(
        ROW = DTAColumnSpec(id = "ROW", type = "SAS Char")
      ))
    ))
  )
}

test_that("OQ-EXPORT-045 | default documents include study and ordered agreement clauses | REQ-EXPORT-033", {
  dta <- qagreement_export_dta()
  dir <- qa_tempdir()
  word <- file.path(dir, "agreement.docx")
  markdown <- file.path(dir, "agreement.md")
  write_dta(dta, file = word, format = "docx")
  write_dta(dta, file = markdown, format = "md")
  word_text <- qa_docx_text(word)
  markdown_text <- paste(readLines(markdown, warn = FALSE), collapse = "\n")
  for (format in c("docx", "md")) {
    text <- if (format == "docx") word_text else markdown_text
    for (value in c("DEMO-001", "P-02", "First paragraph.", "Second paragraph.", "Agreed secure channel")) {
      qa_check(paste(format, "retains", value), grepl(value, text, fixed = TRUE))
    }
    transmission <- qagreement_export_transmission()
    for (field in names(transmission)) {
      qa_check(
        paste(format, "retains the complete transmission statement", field),
        grepl(transmission[[field]], text, fixed = TRUE)
      )
    }
    for (flag in c("test_upload", "blinded_transfer")) {
      qa_check(paste(format, "does not invent an absent decision", flag), !grepl(flag, text, fixed = TRUE))
    }
    positions <- vapply(c("Scope clause", "Review clause", "probe_data"), function(value) {
      regexpr(value, text, fixed = TRUE)[[1]]
    }, integer(1))
    qa_check(paste(format, "renders declared section order before datasets"), all(positions > 0L) && all(diff(positions) > 0L))
  }
  paragraphs <- qa_docx_summary(word)$text
  qa_check("Word retains separate narrative paragraphs", all(c("First paragraph.", "Second paragraph.") %in% paragraphs))
  qa_check("Markdown retains paragraph separation", grepl("First paragraph.\n\nSecond paragraph.", markdown_text, fixed = TRUE))

  dta@metadata@transmission <- c(
    qagreement_export_transmission(), list(test_upload = FALSE, blinded_transfer = FALSE)
  )
  false_word <- file.path(dir, "false-flags.docx")
  false_markdown <- file.path(dir, "false-flags.md")
  write_dta(dta, file = false_word, format = "docx")
  write_dta(dta, file = false_markdown, format = "md")
  summary <- qa_docx_summary(false_word)
  cells <- summary$text[summary$content_type == "table cell"]
  false_markdown_lines <- readLines(false_markdown, warn = FALSE)
  for (flag in c("test_upload", "blinded_transfer")) {
    qa_step(paste("Word renders explicit FALSE as No for", flag), "No", cells[match(flag, cells) + 1L])
    qa_check(
      paste("Markdown renders explicit FALSE as No for", flag),
      paste0("- **", flag, ":** No") %in% false_markdown_lines
    )
  }
})

test_that("OQ-EXPORT-046 | custom layouts opt into study and agreement blocks | REQ-EXPORT-034", {
  dir <- qa_tempdir()
  template <- file.path(dir, "template.docx")
  output <- file.path(dir, "output.docx")
  doc <- officer::read_docx()
  for (token in c(
    "{STUDY_ID}", "{PROTOCOL_ID}", "{STUDY_TABLE}", "{AGREEMENT_SECTIONS}",
    "{PROCESS_INFORMATION}", "Test upload: [{TEST_UPLOAD}]", "Blinded transfer: [{BLINDED_TRANSFER}]"
  )) {
    doc <- officer::body_add_par(doc, token)
  }
  print(doc, target = template)
  dta <- qagreement_export_dta()
  export_with_template(dta, template = template, output = output, fallback = FALSE)
  text <- qa_docx_text(output)
  qa_check("study identifier resolves", grepl("DEMO-001", text, fixed = TRUE))
  qa_check("protocol identifier resolves", grepl("P-02", text, fixed = TRUE))
  qa_check("literal braces in a clause remain literal", grepl("Keep {literal} text.", text, fixed = TRUE))
  transmission <- qagreement_export_transmission()
  for (field in names(transmission)) {
    qa_check(
      paste("PROCESS_INFORMATION retains the complete transmission statement", field),
      grepl(transmission[[field]], text, fixed = TRUE)
    )
  }
  paragraphs <- qa_docx_summary(output)$text
  qa_step(
    "absent transfer decisions render blank rather than No",
    c("Test upload: []", "Blinded transfer: []"),
    paragraphs[grepl("^(Test upload|Blinded transfer): \\[", paragraphs)]
  )
  positions <- vapply(c("Scope clause", "First paragraph.", "Second paragraph.", "Review clause"), function(value) {
    regexpr(value, text, fixed = TRUE)[[1]]
  }, integer(1))
  qa_check("block clauses preserve order and paragraphs", all(positions > 0L) && all(diff(positions) > 0L))
  tokens <- names(dta_template_placeholders())
  qa_check("discovery lists all new tokens", all(c(
    "{STUDY_ID}", "{PROTOCOL_ID}", "{STUDY_TABLE}", "{AGREEMENT_SECTIONS}"
  ) %in% tokens))

  dta@metadata@transmission <- c(
    qagreement_export_transmission(), list(test_upload = FALSE, blinded_transfer = FALSE)
  )
  false_output <- file.path(dir, "false-output.docx")
  export_with_template(dta, template = template, output = false_output, fallback = FALSE)
  false_paragraphs <- qa_docx_summary(false_output)$text
  qa_step(
    "explicit FALSE transfer decisions render No",
    c("Test upload: [No]", "Blinded transfer: [No]"),
    false_paragraphs[grepl("^(Test upload|Blinded transfer): \\[", false_paragraphs)]
  )

  plain <- file.path(dir, "plain.docx")
  plain_output <- file.path(dir, "plain-output.docx")
  print(officer::body_add_par(officer::read_docx(), "Custom layout"), target = plain)
  export_with_template(dta, template = plain, output = plain_output, fallback = FALSE)
  qa_step("custom layouts are not implicitly extended", "Custom layout", qa_docx_text(plain_output))
})
