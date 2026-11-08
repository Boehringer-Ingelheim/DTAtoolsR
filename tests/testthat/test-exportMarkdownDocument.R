# The Markdown documents of write_dta() and write_dataset_metadata(): chapter
# order and numbering, contents links, determinism, escaping of author text,
# rules as prose only, and the include_* switches. Every expectation below is
# written by hand from the specification of the document (heading names,
# order, table headers), never copied from an export.

# A valid regular expression full of Markdown and HTML syntax.
.mdd_pattern <- "^a|b`c`*_[$]<i>&{2}#$"

# A DTA that fills every chapter. `txt(tag)` gives the author text of the field
# named `tag`, so the same fixture can be built with plain or hostile text.
.mdd_fixture <- function(txt = function(tag) paste("Plain", tag),
                         signatures = TRUE, study = TRUE, sections = TRUE,
                         datasets = TRUE, parties = TRUE, process = TRUE) {
  contact <- function(who, signature) {
    list(
      name = txt(paste(who, "name")), role = txt(paste(who, "role")),
      department = txt(paste(who, "department")), email = txt(paste(who, "email")),
      phone = txt(paste(who, "phone")), signature = signature, reviewer = TRUE
    )
  }
  party <- function(who) {
    list(
      affiliation = list(
        name = txt(paste(who, "organization")), country = txt(paste(who, "country")),
        address = txt(paste(who, "address"))
      ),
      contacts = list(contact(who, signatures))
    )
  }
  meta <- DTAMetaData(
    title = txt("title"),
    version = "2.1",
    date = as.Date("2026-03-04"),
    header = txt("header"),
    version_history = list(list(version = "2.1", date = as.Date("2026-03-04"), changes = txt("changes"))),
    supplier = if (parties) party("supplier") else list(),
    receiver = if (parties) party("receiver") else list(),
    transmission = if (process) list(type = txt("transfer method"), frequency = txt("frequency")) else list(),
    error_handling = if (process) txt("error handling") else NULL,
    authorized_for_corrections = if (process) list(txt("corrector")) else NULL,
    study = if (study) list(id = txt("study id"), protocol_id = txt("protocol id")) else list(),
    sections = if (sections) {
      list(
        first = list(title = txt("section one title"), text = txt("section one text")),
        second = list(title = txt("section two title"), text = txt("section two text"))
      )
    } else {
      list()
    }
  )
  sets <- list()
  if (datasets) {
    columns <- list(
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", label = txt("age label"), description = txt("age description")),
      CODE = DTAColumnSpec(
        id = "CODE", type = "SAS Char", label = txt("code label"),
        values = c(txt("code value"), "B")
      ),
      NOTE = DTAColumnSpec(id = "NOTE", type = "SAS Char", pattern = .mdd_pattern),
      FREE = DTAColumnSpec(id = "FREE", type = "SAS Char")
    )
    rules <- list(
      # A rule id holds no white space.
      DTARuleColUnique(id = gsub("\\s+", "", txt("unique rule")), columns = "CODE"),
      DTARuleColCondition(
        id = gsub("\\s+", "", txt("condition rule")),
        condition = list(FREE = list(equals = txt("condition value"))),
        then = list(AGE = list(greater_equal = 18))
      )
    )
    ds <- DTADataSetTabular(
      id = "trial_data",
      name = txt("dataset name"),
      description = txt("dataset description"),
      files = list(DTAFileCSV(filename = txt("file name"), pattern_description = txt("file description"))),
      specs = DTAColumnSpecCollection(columns = columns, rules = rules)
    )
    sets <- list(trial_data = ds)
  }
  DTA(datasets = sets, metadata = meta)
}

# Write a DTA as Markdown and return the path.
.mdd_write <- function(dta, ..., env = parent.frame()) {
  out <- tempfile(fileext = ".md")
  withr::defer(unlink(out, force = TRUE), envir = env)
  write_dta(dta, file = out, format = "md", overwrite = TRUE, quiet = TRUE, ...)
  out
}

# Numbered chapter headings ("## 1 Title") of a Markdown file, number removed.
.mdd_chapters <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hits <- grep("^## [0-9]+ ", lines, value = TRUE)
  .md_unescape(sub("^## [0-9]+ ", "", hits))
}

# Render a Markdown file with pandoc (GFM reader) to an xml2 HTML document.
.mdd_html <- function(path) {
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html, force = TRUE), add = TRUE)
  rmarkdown::pandoc_convert(
    normalizePath(path),
    from = "gfm", to = "html", output = html, options = "--wrap=none"
  )
  xml2::read_html(html, encoding = "UTF-8")
}

.mdd_skip_without_pandoc <- function() {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("xml2")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required to render Markdown")
}

test_that("the chapters of the DTA Markdown follow the Word layout, numbered without gaps", {
  dta <- .mdd_fixture()
  md_path <- .mdd_write(dta)
  expected <- c(
    "Approval & Signatures", "Document Information", "Study", "Parties",
    "Process Information", "Agreement Sections", "Datasets"
  )
  expect_identical(.mdd_chapters(md_path), expected)

  lines <- readLines(md_path, warn = FALSE, encoding = "UTF-8")
  expect_identical(
    grep("^## [0-9]+ ", lines, value = TRUE),
    c(
      "## 1 Approval &amp; Signatures", "## 2 Document Information", "## 3 Study", "## 4 Parties",
      "## 5 Process Information", "## 6 Agreement Sections", "## 7 Datasets"
    )
  )
  expect_true(all(c(
    "### 2.1 Version History", "### 4.1 Supplier", "### 4.2 Receiver",
    "### 5.1 Transmission Details", "### 5.2 Error Handling", "### 5.3 Authorized for Corrections",
    "### 6.1 Plain section one title", "### 6.2 Plain section two title",
    "### 7.1 Plain dataset name (trial\\_data)",
    "#### 7.1.1 Files", "#### 7.1.2 Column Specifications", "#### 7.1.3 Validation Rules"
  ) %in% lines))

  # The same chapters, in the same order, as the level-1 headings of Word.
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  write_dta(dta, file = docx, format = "docx", quiet = TRUE)
  summary <- officer::docx_summary(officer::read_docx(docx))
  expect_identical(summary$text[summary$style_name %in% "heading 1"], expected)
})

test_that("chapters without content are left out and the numbers close up", {
  no_study <- .mdd_write(.mdd_fixture(study = FALSE))
  expect_identical(
    grep("^## [0-9]+ ", readLines(no_study, warn = FALSE), value = TRUE),
    c(
      "## 1 Approval &amp; Signatures", "## 2 Document Information", "## 3 Parties",
      "## 4 Process Information", "## 5 Agreement Sections", "## 6 Datasets"
    )
  )

  bare <- .mdd_write(.mdd_fixture(
    signatures = FALSE, study = FALSE, sections = FALSE, datasets = FALSE, process = FALSE
  ))
  bare_lines <- readLines(bare, warn = FALSE)
  expect_identical(
    grep("^## [0-9]+ ", bare_lines, value = TRUE),
    c("## 1 Document Information", "## 2 Parties")
  )
  expect_true(all(c("### 1.1 Version History", "### 2.1 Supplier", "### 2.2 Receiver") %in% bare_lines))
  # The contents list names exactly these chapters.
  toc <- grep("^ *- \\[", bare_lines, value = TRUE)
  expect_identical(
    toc,
    c(
      "- [1 Document Information](#1-document-information)",
      "  - [1.1 Version History](#11-version-history)",
      "- [2 Parties](#2-parties)",
      "  - [2.1 Supplier](#21-supplier)",
      "  - [2.2 Receiver](#22-receiver)"
    )
  )
})

test_that("the head carries name, title, organisation, header and the cover facts; nothing as 'Label:' lines", {
  path <- .mdd_write(.mdd_fixture(), organisation = "Acme", organisation_unit = "Biometrics")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expect_identical(lines[1:7], c(
    "# Data Transfer Agreement", "",
    "**Plain title**", "",
    "Acme \u00b7 Biometrics", "",
    "Plain header"
  ))
  facts <- c(
    "| Item | Details |", "| --- | --- |", "| Version | 2.1 |", "| Date | 2026-03-04 |",
    "| Study ID | Plain study id |", "| Protocol ID | Plain protocol id |",
    "| Supplier | Plain supplier organization |", "| Receiver | Plain receiver organization |"
  )
  expect_identical(lines[9:16], facts)
  expect_identical(lines[18], "## Contents")
  expect_false(any(grepl("^\\*\\*[A-Za-z ]+:\\*\\*", lines[1:18])))
})

test_that("the footer states title, version and date and the template, with no time stamp", {
  dta <- .mdd_fixture()
  dta@metadata@template <- list(id = "tpl_x", label = "Template X", version = "4", date = "2026-01-02")
  lines <- readLines(.mdd_write(dta), warn = FALSE, encoding = "UTF-8")
  n <- length(lines)
  expect_identical(lines[(n - 4):n], c(
    "---", "",
    "Plain title \u00b7 Version 2.1 \u00b7 2026-03-04", "",
    "Template: Template X (tpl\\_x) \u00b7 version 4 \u00b7 2026-01-02"
  ))
  # A line above the rule is separated by a blank line (no Setext heading).
  expect_identical(lines[n - 5], "")
})

test_that("the Markdown is deterministic: same bytes on every run and in every locale, LF only", {
  dta <- .mdd_fixture()
  first <- .mdd_write(dta)
  second <- withr::with_locale(c(LC_TIME = "C"), .mdd_write(dta))
  bytes <- readBin(first, "raw", file.size(first))
  expect_identical(bytes, readBin(second, "raw", file.size(second)))

  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  expect_false(grepl("\r", text, fixed = TRUE))
  expect_true(endsWith(text, "\n"))
  expect_false(endsWith(text, "\n\n"))
  expect_false(grepl("Generated", text, fixed = TRUE))
  expect_false(grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", text))
  expect_false(grepl("[ \t]\n", text))
  # No date but the ones the fixture carries (2026-03-04) and today is absent.
  dates <- unique(regmatches(text, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", text))[[1]])
  expect_identical(dates, "2026-03-04")

  ds <- .mdd_fixture()@datasets[[1]]
  a <- tempfile(fileext = ".md")
  b <- tempfile(fileext = ".md")
  on.exit(unlink(c(a, b), force = TRUE), add = TRUE)
  write_dataset_metadata(ds, a, format = "md", quiet = TRUE)
  withr::with_locale(c(LC_TIME = "C"), write_dataset_metadata(ds, b, format = "md", quiet = TRUE))
  expect_identical(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b)))
  expect_false(grepl("Generated", paste(readLines(a, warn = FALSE), collapse = "\n"), fixed = TRUE))
})

test_that("every contents link points at the id pandoc gives the heading", {
  .mdd_skip_without_pandoc()
  hostile <- function(tag) paste(tag, "& <x> *y* `z` [a](b) #1 | $q$ :smile: ~t~ \u00fc")
  for (dta in list(.mdd_fixture(), .mdd_fixture(hostile))) {
    path <- .mdd_write(dta)
    html <- .mdd_html(path)
    ids <- xml2::xml_attr(xml2::xml_find_all(html, "//h1|//h2|//h3|//h4"), "id")
    hrefs <- xml2::xml_attr(xml2::xml_find_all(html, "//a[starts-with(@href, '#')]"), "href")
    links <- vapply(sub("^#", "", hrefs), utils::URLdecode, character(1), USE.NAMES = FALSE)
    expect_gt(length(links), 10)
    expect_true(all(links %in% ids), info = paste(setdiff(links, ids), collapse = ", "))
    # Contents covers chapter levels 1 and 2, i.e. every h2 and h3 but itself.
    numbered <- xml2::xml_find_all(html, "//h2[not(@id='contents')]|//h3")
    expect_identical(sort(links), sort(xml2::xml_attr(numbered, "id")))
  }
})

test_that("author text cannot change the structure of the document and is shown literally", {
  .mdd_skip_without_pandoc()
  nasty <- paste(
    "# *em* _u_ `code` <i>raw</i> &amp; $m$ [l](u) | p \\ ~~s~~ :smile: {X} 1. - > ! ** __",
    "<!-- c --> &copy; ---"
  )
  hostile <- function(tag) paste0("[", tag, "] ", nasty)
  plain_html <- .mdd_html(.mdd_write(.mdd_fixture()))
  hostile_path <- .mdd_write(.mdd_fixture(hostile))
  hostile_html <- .mdd_html(hostile_path)

  structure_counts <- function(html) {
    tags <- c("h1", "h2", "h3", "h4", "table", "tr", "td", "th", "ul", "li", "p", "strong", "em", "code", "pre", "a", "hr", "blockquote", "del", "img")
    vapply(tags, function(t) length(xml2::xml_find_all(html, paste0("//", t))), integer(1))
  }
  expect_identical(structure_counts(hostile_html), structure_counts(plain_html))
  expect_identical(structure_counts(hostile_html)[c("code", "pre", "em", "del", "blockquote", "img")], c(code = 0L, pre = 0L, em = 0L, del = 0L, blockquote = 0L, img = 0L))

  # Every field's text appears verbatim in the rendered document.
  rendered <- gsub("\\s+", " ", xml2::xml_text(xml2::xml_find_first(hostile_html, "//body")))
  tags <- c(
    "title", "header", "changes", "supplier organization", "supplier country", "supplier address",
    "supplier name", "supplier role", "supplier department", "supplier email", "supplier phone",
    "receiver organization", "receiver name", "transfer method", "frequency", "error handling",
    "corrector", "study id", "protocol id", "section one title", "section one text",
    "section two title", "dataset name", "dataset description", "file name", "file description",
    "age label", "age description", "code label", "code value", "condition value"
  )
  expect_true(grepl(paste("Pattern:", .mdd_pattern), rendered, fixed = TRUE))
  for (tag in tags) {
    expect_true(grepl(hostile(tag), rendered, fixed = TRUE), info = tag)
  }
  for (tag in c("unique rule", "condition rule")) {
    expect_true(grepl(gsub("\\s+", "", hostile(tag)), rendered, fixed = TRUE), info = tag)
  }
})

test_that("every row of a Markdown table has as many cells as its header", {
  .mdd_skip_without_pandoc()
  hostile <- function(tag) paste(tag, "a | b \\| c \\\\| d\nsecond | line")
  html <- .mdd_html(.mdd_write(.mdd_fixture(hostile)))
  tables <- xml2::xml_find_all(html, "//table")
  expect_gt(length(tables), 8)
  for (table in tables) {
    width <- length(xml2::xml_find_all(table, ".//thead/tr/th"))
    rows <- xml2::xml_find_all(table, ".//tbody/tr")
    cells <- vapply(rows, function(r) length(xml2::xml_find_all(r, "./td")), integer(1))
    expect_true(all(cells == width))
  }
  # The same holds for the Markdown source: unescaped pipes per table line.
  lines <- readLines(.mdd_write(.mdd_fixture(hostile)), warn = FALSE, encoding = "UTF-8")
  rows <- grep("^\\|", lines, value = TRUE)
  pipes <- lengths(regmatches(rows, gregexpr("(?<!\\\\)(\\\\\\\\)*\\|", rows, perl = TRUE)))
  blocks <- cumsum(c(TRUE, diff(which(grepl("^\\|", lines))) > 1))
  for (b in unique(blocks)) {
    expect_length(unique(pipes[blocks == b]), 1L)
  }
})

test_that("rules are written as prose: no code, no YAML, the reading guide once", {
  rules <- list(
    DTARuleColUnique(id = "tick`id|$x$<y>&z", columns = "CODE"),
    DTARuleColCondition(
      id = "fence```rule",
      condition = list(CODE = list(`in` = c("a`b", "c|d", "$5", "<b>", "x&y"))),
      then = list(AGE = list(greater_equal = 18))
    )
  )
  columns <- list(
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num"),
    CODE = DTAColumnSpec(id = "CODE", type = "SAS Char")
  )
  sets <- lapply(c("first_set", "second_set"), function(id) {
    DTADataSetTabular(id = id, specs = DTAColumnSpecCollection(columns = columns, rules = rules))
  })
  names(sets) <- c("first_set", "second_set")
  dta <- DTA(datasets = sets, metadata = DTAMetaData(title = "Rules"))
  path <- .mdd_write(dta)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- paste(lines, collapse = "\n")

  expect_false(any(grepl("(?<!\\\\)`", lines, perl = TRUE)))
  expect_false(any(grepl("^ *(```|~~~)", lines)))
  expect_false(any(grepl("^ *(type|columns|condition|then|id): ", lines)))
  expect_false(grepl("<code>", text, fixed = TRUE))
  expect_true("**tick\\`id\\|\\$x\\$&lt;y&gt;&amp;z** \u00b7 Unique" %in% lines)
  expect_identical(sum(lines == "**fence\\`\\`\\`rule** \u00b7 Conditional (IF/THEN)"), 2L)

  guide <- .rule_reading_guide()
  expect_identical(sum(lines == guide[[1]]), 1L)
  for (sentence in guide) {
    expect_identical(sum(lines == .md_escape(sentence)), 1L, info = sentence)
  }
  expect_lt(match(guide[[1]], lines), match("**tick\\`id\\|\\$x\\$&lt;y&gt;&amp;z** \u00b7 Unique", lines))
  expect_lt(match("#### 2.1.3 Validation Rules", lines), match(guide[[1]], lines))

  if (rmarkdown::pandoc_available()) {
    html <- .mdd_html(path)
    expect_length(xml2::xml_find_all(html, "//code|//pre"), 0L)
    rendered <- xml2::xml_text(html)
    for (value in c("a`b", "c|d", "$5", "<b>", "x&y", "tick`id|$x$<y>&z")) {
      expect_true(grepl(value, rendered, fixed = TRUE), info = value)
    }
  }

  # Without any rule there is no reading guide.
  no_rules <- DTA(datasets = list(DTADataSetTabular(
    id = "plain", specs = DTAColumnSpecCollection(columns = columns)
  )), metadata = DTAMetaData(title = "No rules"))
  plain_lines <- readLines(.mdd_write(no_rules), warn = FALSE)
  expect_false(guide[[1]] %in% plain_lines)
  expect_true("No validation rules specified." %in% plain_lines)
})

test_that("allowed values are listed in full", {
  values <- sprintf("V%02d", 1:12)
  ds <- DTADataSetTabular(
    id = "visits",
    specs = DTAColumnSpecCollection(columns = list(
      VISIT = DTAColumnSpec(id = "VISIT", type = "SAS Char", values = values)
    ))
  )
  dta <- DTA(datasets = list(visits = ds), metadata = DTAMetaData(title = "Visits"))
  lines <- readLines(.mdd_write(dta), warn = FALSE)
  row <- grep("^\\| VISIT \\|", lines, value = TRUE)
  expect_length(row, 1L)
  expect_match(row, paste(values, collapse = ", "), fixed = TRUE)
  expect_false(grepl("more", row, fixed = TRUE))
})

test_that("the approval table lists the signatories, and only exists when there are any", {
  signers <- readLines(.mdd_write(.mdd_fixture()), warn = FALSE)
  at <- match("## 1 Approval &amp; Signatures", signers)
  expect_identical(signers[at + 2:6], c(
    "| Organization | Name | Role | Signature | Date |",
    "| --- | --- | --- | --- | --- |",
    "| Plain receiver organization | Plain receiver name | Plain receiver role | <br><br> | <br><br> |",
    "| Plain supplier organization | Plain supplier name | Plain supplier role | <br><br> | <br><br> |",
    ""
  ))
  expect_false(any(grepl("____", signers, fixed = TRUE)))

  # signature_list adds a signatory, as in Word.
  listed <- readLines(.mdd_write(
    .mdd_fixture(),
    signature_list = list(list(name = "Extra Person", role = "QA", organization = "Audit Ltd"))
  ), warn = FALSE)
  expect_true("| Audit Ltd | Extra Person | QA | <br><br> | <br><br> |" %in% listed)

  # No signatory: no chapter; include_signatures = FALSE: no chapter either.
  for (path in list(
    .mdd_write(.mdd_fixture(signatures = FALSE)),
    .mdd_write(.mdd_fixture(), include_signatures = FALSE)
  )) {
    lines <- readLines(path, warn = FALSE)
    expect_false(any(grepl("Approval", lines, fixed = TRUE)))
    expect_false(any(grepl("| Signature |", lines, fixed = TRUE)))
    expect_identical(lines[grep("^## [0-9]", lines)[1]], "## 1 Document Information")
  }
})

test_that("write_dataset_metadata Markdown honours include_signatures, include_file_specs and include_rules", {
  ds <- .mdd_fixture()@datasets[[1]]
  write <- function(...) {
    out <- tempfile(fileext = ".md")
    withr::defer(unlink(out, force = TRUE), envir = parent.frame(2))
    write_dataset_metadata(ds, out, format = "md", quiet = TRUE, ...)
    readLines(out, warn = FALSE, encoding = "UTF-8")
  }
  signer <- list(list(name = "Ann Lee", role = "Lead", organization = "Acme"))
  chapters <- function(lines) grep("^## [0-9]+ ", lines, value = TRUE)

  full <- write(signature_list = signer)
  expect_identical(chapters(full), c(
    "## 1 Approval &amp; Signatures", "## 2 Description", "## 3 Files",
    "## 4 Column Specifications", "## 5 Validation Rules"
  ))
  expect_true("| Acme | Ann Lee | Lead | <br><br> | <br><br> |" %in% full)
  expect_true(.rule_reading_guide()[[1]] %in% full)
  expect_identical(full[3], "**Dataset Specification: Plain dataset name (trial\\_data)**")

  expect_identical(
    chapters(write(signature_list = signer, include_signatures = FALSE)),
    c("## 1 Description", "## 2 Files", "## 3 Column Specifications", "## 4 Validation Rules")
  )
  no_files <- write(include_file_specs = FALSE)
  expect_identical(
    chapters(no_files),
    c("## 1 Description", "## 2 Column Specifications", "## 3 Validation Rules")
  )
  expect_false(any(grepl("Plain file name", no_files, fixed = TRUE)))
  no_rules <- write(include_rules = FALSE)
  expect_identical(chapters(no_rules), c("## 1 Description", "## 2 Files", "## 3 Column Specifications"))
  expect_false(.rule_reading_guide()[[1]] %in% no_rules)
  expect_false(any(grepl("Plainuniquerule", no_rules, fixed = TRUE)))

  # No signatories: no approval chapter and no blank line to sign on.
  expect_false(any(grepl("Approv|____", write())))
  # Footer: deterministic, no "Generated".
  n <- length(full)
  expect_identical(full[n - 4], "---")
  expect_identical(full[n - 2], "Dataset Specification: Plain dataset name (trial\\_data)")
  expect_identical(full[n], paste0("Built-in layout \u00b7 DTAtools ", utils::packageVersion("DTAtools")))
})

test_that("a file's nested info is listed one entry per line in Markdown", {
  yaml_path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  skip_if(!nzchar(yaml_path), "gf_dataset.yaml missing")
  ds <- read_dataset_from_yaml(yaml_path)
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, out, format = "md", quiet = TRUE)
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  expect_true(
    "| gf\\_data\\_small\\_smirna.tsv | Exact name | TSV | 1 | Data: smrnaseq<br>Code: smr |" %in% lines
  )
  expect_false(any(grepl("\u2022", lines, fixed = TRUE)))

  expect_identical(.md_info_lines(list(list(data = "x"), list(code = "y"))), c("Data: x", "Code: y"))
  expect_identical(.md_info_lines(list(outer = list(inner = TRUE))), "Outer / Inner: Yes")
  expect_identical(.md_info_lines(c("free", "text")), c("free", "text"))
  expect_identical(.md_info_lines(NULL), character(0))
})
