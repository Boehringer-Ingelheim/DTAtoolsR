.agreement_export_dta <- function() {
  DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(
      title = "Synthetic agreement",
      date = as.Date("2026-01-15"),
      study = list(id = "ST-01 {DTA_TITLE} | `id` \u00fc\nA", protocol_id = "P-02"),
      sections = list(
        responsibilities = list(
          title = "Responsibilities | `literal` {PROTOCOL_ID}",
          text = "First paragraph {DTA_TITLE} | `code` \u00fc\nSecond line\n\nSecond paragraph <b>literal</b> & {STUDY_TABLE}"
        ),
        scope = list(title = "Scope", text = "Keep this last."),
        draft = list(title = "Unfinished clause", text = "")
      )
    )
  )
}

test_that("default Word exports study identity and ordered plain agreement paragraphs", {
  dta <- .agreement_export_dta()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  write_dta(dta, out, quiet = TRUE, include_signatures = FALSE)

  summary <- officer::docx_summary(officer::read_docx(out))
  cells <- summary$text[summary$content_type == "table cell"]
  expect_true(all(c("Study ID", "Protocol ID", dta@metadata@study$id, "P-02") %in% cells))
  paragraphs <- summary$text[summary$content_type == "paragraph"]
  headings <- c(
    "Document Information", "Study", "Agreement Sections",
    "Responsibilities | `literal` {PROTOCOL_ID}", "Scope", "Unfinished clause", "Datasets"
  )
  expect_true(all(headings %in% paragraphs))
  expect_true(all(diff(match(headings, paragraphs)) > 0))
  expect_true("First paragraph {DTA_TITLE} | `code` \u00fc\nSecond line" %in% paragraphs)
  expect_true("Second paragraph <b>literal</b> & {STUDY_TABLE}" %in% paragraphs)
  xml <- .read_docx_body_xml(out)
  expect_match(xml, "&lt;b&gt;literal&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>literal</b>", xml, fixed = TRUE))
})

test_that("Markdown study and sections retain order and escape plain narrative", {
  dta <- .agreement_export_dta()
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out), add = TRUE)
  write_dta(dta, out, quiet = TRUE)
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  md <- paste(lines, collapse = "\n")
  headings <- c(
    "## Document Information", "## Study", "## Agreement Sections",
    "### Responsibilities \\| \\`literal\\` \\{PROTOCOL\\_ID\\}",
    "### Scope", "### Unfinished clause", "## Datasets"
  )
  expect_true(all(headings %in% lines))
  expect_true(all(diff(match(headings, lines)) > 0))
  expect_match(md, "| Study ID | ST-01 \\{DTA\\_TITLE\\} \\| \\`id\\` \u00fc<br>A |", fixed = TRUE)
  expect_match(md, "First paragraph \\{DTA\\_TITLE\\} \\| \\`code\\` \u00fc\nSecond line\n\nSecond paragraph &lt;b&gt;literal&lt;/b&gt; &amp; \\{STUDY\\_TABLE\\}", fixed = TRUE)
  expect_match(md, "### Unfinished clause\n\n", fixed = TRUE)
  expect_false(grepl("<b>literal</b>", md, fixed = TRUE))
})

test_that("legacy metadata does not acquire empty study or agreement headings", {
  dta <- DTA(datasets = list(create_example_DTADataSetTabular(2)), metadata = DTAMetaData())
  for (ext in c(".docx", ".md")) {
    out <- tempfile(fileext = ext)
    on.exit(unlink(out), add = TRUE)
    write_dta(dta, out, quiet = TRUE)
    text <- if (ext == ".docx") .docx_text(out) else paste(readLines(out), collapse = "\n")
    expect_false(grepl("Agreement Sections", text, fixed = TRUE))
    expect_false(grepl("Study ID", text, fixed = TRUE))
    expect_false(grepl("Protocol ID", text, fixed = TRUE))
    expect_match(text, "Datasets", fixed = TRUE)
  }
})

test_that("study tokens are discoverable inline values and standalone blocks", {
  tokens <- dta_template_placeholders()
  names_expected <- c("{STUDY_ID}", "{PROTOCOL_ID}", "{STUDY_TABLE}", "{AGREEMENT_SECTIONS}")
  expect_true(all(names_expected %in% names(tokens)))
  expect_identical(unname(attr(tokens, "kind")[names_expected]), c("inline", "inline", "block", "block"))

  resolved <- dta_template_placeholders(.agreement_export_dta())
  expect_identical(resolved[["{STUDY_ID}"]], "ST-01 {DTA_TITLE} | `id` \u00fc\nA")
  expect_identical(resolved[["{PROTOCOL_ID}"]], "P-02")
  expect_true(.tv_is_block_token("{STUDY_TABLE}"))
  expect_true(.tv_is_block_token("{AGREEMENT_SECTIONS}"))
  expect_false(.tv_is_block_token("{STUDY_TABLE:dataset}"))
  expect_false(.tv_is_block_token("{AGREEMENT_SECTIONS:scope}"))

  template <- .make_template(c(names_expected, "{#DATASETS}", "{DATASET_NAME}", "{/DATASETS}"))
  on.exit(unlink(template), add = TRUE)
  expect_setequal(
    .tv_scan_block_tokens(template),
    c("{STUDY_TABLE}", "{AGREEMENT_SECTIONS}", "{#DATASETS}", "{/DATASETS}")
  )
})

test_that("custom study blocks share Word rendering without interpreting metadata tokens", {
  dta <- .agreement_export_dta()
  template <- .make_template(c(
    "BEFORE", "{STUDY_ID}", "{PROTOCOL_ID}", "{STUDY_TABLE}",
    "{AGREEMENT_SECTIONS}", "AFTER"
  ))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(template, out)), add = TRUE)
  expect_no_warning(export_with_template(dta, template, out, quiet = TRUE, fallback = FALSE))
  summary <- officer::docx_summary(officer::read_docx(out))
  text <- summary$text
  expect_true(dta@metadata@study$id %in% text)
  expect_true("P-02" %in% text)
  expected <- c(
    "BEFORE", "Study", "Agreement Sections",
    "Responsibilities | `literal` {PROTOCOL_ID}",
    "First paragraph {DTA_TITLE} | `code` \u00fc\nSecond line",
    "Second paragraph <b>literal</b> & {STUDY_TABLE}",
    "Scope", "Keep this last.", "Unfinished clause", "AFTER"
  )
  expect_true(all(expected %in% text))
  expect_true(all(diff(match(expected, text)) > 0))
  # The study table is a key/value table without a header row: two rows
  # (study id, protocol id) of label and value.
  expect_identical(sum(summary$content_type == "table cell"), 4L)
  expect_false(any(text == "Datasets"))
})

test_that("custom templates opt in and empty metadata consumes only its new tokens", {
  dta <- .agreement_export_dta()
  template <- .make_template(c("BEFORE", "{DTA_TITLE}", "AFTER"))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(template, out)), add = TRUE)
  export_with_template(dta, template, out, quiet = TRUE, fallback = FALSE)
  expect_identical(.docx_paragraphs(out), c("BEFORE", "Synthetic agreement", "AFTER"))

  empty_template <- .make_template(c(
    "BEFORE", "{STUDY_ID}", "{PROTOCOL_ID}", "{STUDY_TABLE}", "{AGREEMENT_SECTIONS}", "AFTER"
  ))
  on.exit(unlink(empty_template), add = TRUE)
  dta@metadata <- DTAMetaData()
  expect_no_warning(export_with_template(dta, empty_template, out, quiet = TRUE, fallback = FALSE))
  text <- .docx_paragraphs(out)
  expect_identical(text[nzchar(text)], c("BEFORE", "AFTER"))
})

test_that("full transmission clauses reach default exports and process placeholders", {
  transmission <- list(
    extent = paste(strrep("Complete data scope. ", 8), "EXTENT-END"),
    scope = "All visits including unscheduled visits.",
    packaging = "One archive | manifest per transfer.",
    security = "Encrypt before transfer.",
    checksum = "A checksum accompanies each file.",
    test_requirements = "Synthetic sample before first transfer.",
    blinding_requirements = "Preserve the agreed treatment mask.",
    vendor_extension = "Keep vendor extensions."
  )
  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(transmission = transmission)
  )
  for (ext in c(".docx", ".md")) {
    out <- tempfile(fileext = ext)
    on.exit(unlink(out), add = TRUE)
    write_dta(dta, out, quiet = TRUE)
    text <- if (ext == ".docx") .docx_text(out) else paste(readLines(out), collapse = "\n")
    for (value in transmission) {
      # Markdown output escapes a literal pipe so it cannot be read as a table
      # cell delimiter; DOCX carries the text unchanged.
      if (ext == ".md") value <- gsub("|", "\\|", value, fixed = TRUE)
      expect_match(text, value, fixed = TRUE)
    }
  }
  template <- .make_template("{PROCESS_INFORMATION}")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(template, out)), add = TRUE)
  export_with_template(dta, template, out, quiet = TRUE, fallback = FALSE)
  text <- .docx_text(out)
  for (value in transmission) {
    expect_match(text, value, fixed = TRUE)
  }
})

test_that("missing transfer decisions stay blank while explicit FALSE remains No", {
  dta <- DTA(datasets = list(create_example_DTADataSetTabular(2)), metadata = DTAMetaData())
  keys <- c("{TEST_UPLOAD}", "{BLINDED_TRANSFER}")
  expect_identical(unname(unlist(.extract_template_variables(dta)[keys])), c("", ""))
  dta@metadata@transmission <- list(test_upload = FALSE, blinded_transfer = FALSE)
  expect_identical(unname(unlist(.extract_template_variables(dta)[keys])), c("No", "No"))
  dta@metadata@transmission <- list(test_upload = TRUE, blinded_transfer = TRUE)
  expect_identical(unname(unlist(.extract_template_variables(dta)[keys])), c("Yes", "Yes"))
})

test_that("Markdown narrative does not turn list markers or rules into formatting", {
  dta <- .agreement_export_dta()
  dta@metadata@sections <- list(
    plain = list(
      title = "# [Plain] & <safe>",
      text = "# Heading\n\n- item\n\n1. numbered\n\n---\n\n~~~\n\nunderlined\n===\n\n[link](url)"
    )
  )
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out), add = TRUE)
  write_dta(dta, out, quiet = TRUE)
  md <- paste(readLines(out, encoding = "UTF-8"), collapse = "\n")
  expect_match(md, "### \\# \\[Plain\\] &amp; &lt;safe&gt;", fixed = TRUE)
  expect_match(md, "\\# Heading\n\n\\- item\n\n1\\. numbered\n\n\\---\n\n\\~\\~\\~\n\nunderlined\n\\=\\=\\=\n\n\\[link\\](url)", fixed = TRUE)
})

test_that("NA transfer decisions stay unspecified in scalar and process placeholders", {
  dta <- .agreement_export_dta()
  dta@metadata@transmission <- list(test_upload = NA, blinded_transfer = NA)
  template <- .make_template(c(
    "Test: [{TEST_UPLOAD}]", "Blinded: [{BLINDED_TRANSFER}]", "{PROCESS_INFORMATION}"
  ))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(template, out)), add = TRUE)
  export_with_template(dta, template, out, quiet = TRUE, fallback = FALSE)
  paragraphs <- .docx_paragraphs(out)
  expect_true("Test: []" %in% paragraphs)
  expect_true("Blinded: []" %in% paragraphs)
  expect_false(any(grepl("Test Upload: No", paragraphs, fixed = TRUE)))
  expect_false(any(grepl("Blinded Transfer: No", paragraphs, fixed = TRUE)))

  dta@metadata@transmission <- list(test_upload = FALSE, blinded_transfer = FALSE)
  export_with_template(dta, template, out, quiet = TRUE, fallback = FALSE)
  paragraphs <- .docx_paragraphs(out)
  expect_true("Test: [No]" %in% paragraphs)
  expect_true("Blinded: [No]" %in% paragraphs)
  expect_true(any(grepl("Test Upload: No", paragraphs, fixed = TRUE)))
  expect_true(any(grepl("Blinded Transfer: No", paragraphs, fixed = TRUE)))

  export_with_template(
    dta, template, out,
    quiet = TRUE, fallback = FALSE,
    variables = list(TEST_UPLOAD = NA, BLINDED_TRANSFER = NA)
  )
  paragraphs <- .docx_paragraphs(out)
  expect_true("Test: []" %in% paragraphs)
  expect_true("Blinded: []" %in% paragraphs)
})

test_that("rendered Markdown preserves indented narrative without code blocks or escape artifacts", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is required to render Markdown")
  dta <- .agreement_export_dta()
  dta@metadata@sections <- list(
    spaces = list(title = "Spaces", text = "    <b>literal</b> *text*"),
    tab = list(title = "Tab", text = "\t<b>tabbed</b> *text*"),
    mixed = list(title = "Mixed", text = "  \t<b>mixed</b> *text*")
  )
  markdown <- tempfile(fileext = ".md")
  html <- tempfile(fileext = ".html")
  on.exit(unlink(c(markdown, html)), add = TRUE)
  write_dta(dta, markdown, quiet = TRUE)
  rmarkdown::pandoc_convert(
    markdown,
    from = "commonmark", to = "html", output = html,
    options = "--wrap=none"
  )
  rendered <- xml2::read_html(html)
  clauses <- xml2::xml_find_all(
    rendered,
    ".//h3[.='Spaces' or .='Tab' or .='Mixed']/following-sibling::*[1]"
  )
  expect_identical(xml2::xml_name(clauses), rep("p", 3))
  paragraphs <- xml2::xml_text(clauses)
  for (text in c("<b>literal</b> *text*", "<b>tabbed</b> *text*", "<b>mixed</b> *text*")) {
    expect_true(paste0(strrep("\u00a0", 4), text) %in% paragraphs)
  }
  expect_length(xml2::xml_find_all(clauses, ".//code|.//b|.//em"), 0L)
})
