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
    "Approval and Signatures", "Document Information", "Study", "Parties",
    "Process Information", "Agreement Sections", "Datasets"
  )
  expect_identical(.mdd_chapters(md_path), expected)

  lines <- readLines(md_path, warn = FALSE, encoding = "UTF-8")
  expect_identical(
    grep("^## [0-9]+ ", lines, value = TRUE),
    c(
      "## 1 Approval and Signatures", "## 2 Document Information", "## 3 Study", "## 4 Parties",
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

  # The same chapters, in the same order, as the level-1 headings of Word
  # (where the first one is spelt "Approval & Signatures").
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  write_dta(dta, file = docx, format = "docx", quiet = TRUE)
  summary <- officer::docx_summary(officer::read_docx(docx))
  expect_identical(
    summary$text[summary$style_name %in% "heading 1"],
    c("Approval & Signatures", expected[-1])
  )
})

test_that("chapters without content are left out and the numbers close up", {
  no_study <- .mdd_write(.mdd_fixture(study = FALSE))
  expect_identical(
    grep("^## [0-9]+ ", readLines(no_study, warn = FALSE), value = TRUE),
    c(
      "## 1 Approval and Signatures", "## 2 Document Information", "## 3 Parties",
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

test_that("the head carries name, title, organisation and the cover facts; nothing as 'Label:' lines", {
  path <- .mdd_write(.mdd_fixture(), organisation = "Acme", organisation_unit = "Biometrics")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expect_identical(lines[1:6], c(
    "# Data Transfer Agreement", "",
    "**Plain title**", "",
    "Acme \u00b7 Biometrics", ""
  ))
  facts <- c(
    "| Item | Details |", "| --- | --- |", "| Version | 2.1 |", "| Date | 2026-03-04 |",
    "| Study ID | Plain study id |", "| Protocol ID | Plain protocol id |",
    "| Supplier | Plain supplier organization |", "| Receiver | Plain receiver organization |"
  )
  expect_identical(lines[7:14], facts)
  expect_identical(lines[16], "## Contents")
  expect_false(any(grepl("^\\*\\*[A-Za-z ]+:\\*\\*", lines[1:16])))
  # The document header is no loose paragraph of the head: it is a row of
  # Document Information.
  expect_false("Plain header" %in% lines[1:16])
})

test_that("Document Information holds only what the head does not show", {
  dta <- .mdd_fixture()
  dta@metadata@template <- list(id = "tpl_x", label = "Template X", version = "4", date = "2026-01-02")
  lines <- readLines(.mdd_write(dta), warn = FALSE, encoding = "UTF-8")
  at <- match("## 2 Document Information", lines)
  expect_identical(lines[at + 1:9], c(
    "",
    "| Item | Details |", "| --- | --- |",
    "| Document header | Plain header |",
    "| Template | Template X (tpl\\_x) |", "| Template version | 4 |", "| Template date | 2026-01-02 |",
    "",
    "### 2.1 Version History"
  ))
  expect_false(any(grepl("^\\| (Title|Version|Date) \\|", lines[at:(at + 9)])))
  # Title, version and date are in the head, once.
  expect_identical(sum(lines == "| Version | 2.1 |"), 1L)
  expect_identical(sum(lines == "| Date | 2026-03-04 |"), 1L)
  expect_false(any(grepl("^\\| Title \\|", lines)))

  # Nothing left (no header, no template, no history): no chapter.
  bare <- .mdd_fixture()
  bare@metadata@header <- NULL
  bare@metadata@version_history <- list()
  bare_lines <- readLines(.mdd_write(bare), warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl("Document Information", bare_lines, fixed = TRUE)))
})

test_that("a DTA without a title has no invented title in Markdown or Word", {
  dta <- .mdd_fixture()
  dta@metadata@title <- NULL
  lines <- readLines(.mdd_write(dta), warn = FALSE, encoding = "UTF-8")
  # No bold subtitle under the document name, no Title row anywhere.
  expect_identical(lines[1:3], c("# Data Transfer Agreement", "", "| Item | Details |"))
  expect_false(any(grepl("^\\| Title \\|", lines)))

  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx, force = TRUE), add = TRUE)
  write_dta(dta, file = docx, format = "docx", quiet = TRUE)
  summary <- officer::docx_summary(officer::read_docx(docx))
  cells <- summary[summary$content_type %in% "table cell", ]
  expect_false("Title" %in% cells$text)
  # Word keeps version and date in Document Information.
  expect_true(all(c("Document header", "Version", "Date") %in% cells$text))
})

test_that("the built-in layout's footer states title, version and date only", {
  lines <- readLines(.mdd_write(.mdd_fixture()), warn = FALSE, encoding = "UTF-8")
  n <- length(lines)
  expect_identical(lines[(n - 2):n], c("---", "", "Plain title \u00b7 Version 2.1 \u00b7 2026-03-04"))
  # Nothing that changes with the package version.
  expect_false(any(grepl("Built-in layout|DTAtools", lines)))
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

test_that("the Markdown is deterministic: same bytes on every run, LF only", {
  dta <- .mdd_fixture()
  first <- .mdd_write(dta)
  second <- .mdd_write(dta)
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
  # No date but the ones the fixture carries (2026-03-04) and today is absent
  # (example dates in the fixed text of the rule reading guide aside).
  dates <- unique(regmatches(text, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", text))[[1]])
  guide <- paste(.rule_reading_guide(), collapse = " ")
  dates <- setdiff(dates, regmatches(guide, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", guide))[[1]])
  expect_identical(dates, "2026-03-04")

  ds <- .mdd_fixture()@datasets[[1]]
  a <- tempfile(fileext = ".md")
  b <- tempfile(fileext = ".md")
  on.exit(unlink(c(a, b), force = TRUE), add = TRUE)
  write_dataset_metadata(ds, a, format = "md", quiet = TRUE)
  write_dataset_metadata(ds, b, format = "md", quiet = TRUE)
  expect_identical(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b)))
  expect_false(grepl("Generated", paste(readLines(a, warn = FALSE), collapse = "\n"), fixed = TRUE))
})

# An installed LC_TIME locale whose month names are not the English ones of
# "C", or NULL when the machine has none (CI on Linux often has only C).
.mdd_other_time_locale <- function() {
  old <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old)), add = TRUE)
  for (loc in c("de_DE.UTF-8", "de_DE.utf8", "fr_FR.UTF-8", "fr_FR.utf8", "German_Germany.1252", "de_DE")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", loc)))) {
      return(loc)
    }
  }
  NULL
}

test_that("the Markdown is the same under C and under another LC_TIME locale", {
  other <- .mdd_other_time_locale()
  skip_if(is.null(other), "no LC_TIME locale other than C is installed: nothing to compare C with")
  dta <- .mdd_fixture()
  ds <- dta@datasets[[1]]
  in_c <- withr::with_locale(c(LC_TIME = "C"), .mdd_write(dta))
  in_other <- withr::with_locale(c(LC_TIME = other), .mdd_write(dta))
  expect_identical(readBin(in_c, "raw", file.size(in_c)), readBin(in_other, "raw", file.size(in_other)))
  a <- tempfile(fileext = ".md")
  b <- tempfile(fileext = ".md")
  on.exit(unlink(c(a, b), force = TRUE), add = TRUE)
  withr::with_locale(c(LC_TIME = "C"), write_dataset_metadata(ds, a, format = "md", quiet = TRUE))
  withr::with_locale(c(LC_TIME = other), write_dataset_metadata(ds, b, format = "md", quiet = TRUE))
  expect_identical(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b)))
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

test_that("contents links reach headings that hold web and e-mail addresses (every pandoc found)", {
  pandocs <- unique(normalizePath(
    Filter(nzchar, c(Sys.which("pandoc"), "/usr/bin/pandoc", "/opt/r45/bin/pandoc", "/usr/local/bin/pandoc")),
    mustWork = FALSE
  ))
  pandocs <- pandocs[file.exists(pandocs)]
  skip_if(length(pandocs) == 0L, "pandoc is not installed")
  skip_if_not_installed("xml2")
  # Chapter, dataset and rule titles with a "www." address (shown as
  # "http://www..."), a URL and an e-mail address.
  addresses <- function(tag) paste(tag, "www.ex.com/path_x, https://ex.com/a_b and john_doe+x@ex.org.")
  path <- .mdd_write(.mdd_fixture(addresses))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # Written by hand: the anchor of the heading as shown, the link text as
  # shown without a link of its own.
  expect_true(paste0(
    "  - [6.1 section one title http&#58;//www&#46;ex.com/path\\_x, https&#58;//ex.com/a\\_b and ",
    "john\\_doe+x&#64;ex.org.](#61-section-one-title-httpwwwexcompath_x-httpsexcoma_b-and-john_doexexorg)"
  ) %in% lines)
  for (pandoc in pandocs) {
    html_file <- tempfile(fileext = ".html")
    status <- system2(pandoc, c("-f", "gfm", "-t", "html", "--wrap=none", "-o", html_file, shQuote(path)))
    expect_identical(status, 0L, info = pandoc)
    html <- xml2::read_html(html_file, encoding = "UTF-8")
    unlink(html_file)
    heads <- xml2::xml_find_all(html, "//h1|//h2|//h3|//h4|//h5")
    head_text <- stats::setNames(xml2::xml_text(heads), xml2::xml_attr(heads, "id"))
    toc <- xml2::xml_find_all(html, "//li/a[starts-with(@href, '#')]")
    expect_gt(length(toc), 10)
    for (a in toc) {
      target <- utils::URLdecode(sub("^#", "", xml2::xml_attr(a, "href")))
      expect_true(target %in% names(head_text), info = paste(pandoc, target))
      # The entry shows the heading's text, and is one link, not several.
      expect_identical(xml2::xml_text(a), unname(head_text[target]), info = paste(pandoc, target))
      expect_length(xml2::xml_find_all(a, ".//a"), 0L)
    }
    addressed <- grep("www", xml2::xml_text(toc), value = TRUE, fixed = TRUE)
    expect_true(
      "6.1 section one title http://www.ex.com/path_x, https://ex.com/a_b and john_doe+x@ex.org." %in% addressed,
      info = pandoc
    )
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
    tags <- c("h1", "h2", "h3", "h4", "h5", "table", "tr", "td", "th", "ul", "li", "p", "strong", "em", "code", "pre", "a", "hr", "blockquote", "del", "img")
    vapply(tags, function(t) length(xml2::xml_find_all(html, paste0("//", t))), integer(1))
  }
  expect_identical(structure_counts(hostile_html), structure_counts(plain_html))
  expect_identical(structure_counts(hostile_html)[c("code", "pre", "del", "blockquote", "img")], c(code = 0L, pre = 0L, del = 0L, blockquote = 0L, img = 0L))
  # The only emphasis is the kind of each of the two rules, in italics.
  expect_identical(structure_counts(hostile_html)[["em"]], 2L)

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
  # One unnumbered heading per rule (level 5, below 1.1.3 Validation Rules),
  # the kind in italics beneath. "&" before "z" starts no entity: unescaped.
  tick <- "##### tick\\`id\\|\\$x\\$&lt;y&gt;&z"
  expect_identical(sum(lines == tick), 2L)
  expect_identical(lines[match(tick, lines) + 2L], "*Unique*")
  fence <- "##### fence\\`\\`\\`rule"
  expect_identical(sum(lines == fence), 2L)
  expect_identical(lines[match(fence, lines) + 2L], "*Conditional (IF/THEN)*")

  # The reading guide once in the document: a bold title, one bullet per point.
  guide <- .rule_reading_guide()
  title_at <- which(lines == "**How to read the rules**")
  expect_length(title_at, 1L)
  expect_identical(lines[title_at + 1L + seq_along(guide)], paste("-", .md_escape(guide)))
  for (sentence in guide) {
    expect_identical(sum(lines == paste("-", .md_escape(sentence))), 1L, info = sentence)
  }
  expect_lt(title_at, match(tick, lines))
  # A single level-1 chapter (Datasets; Document Information has nothing the
  # head does not show): no Contents list.
  expect_false("## Contents" %in% lines)
  expect_lt(match("#### 1.1.3 Validation Rules", lines), title_at)

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
  expect_false("**How to read the rules**" %in% plain_lines)
  expect_false(paste("-", .md_escape(guide[[1]])) %in% plain_lines)
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
  at <- match("## 1 Approval and Signatures", signers)
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
    "## 1 Approval and Signatures", "## 2 Description", "## 3 Files",
    "## 4 Column Specifications", "## 5 Validation Rules"
  ))
  expect_true("| Acme | Ann Lee | Lead | <br><br> | <br><br> |" %in% full)
  expect_true(paste("-", .md_escape(.rule_reading_guide()[[1]])) %in% full)
  # Rule headings one level below the Validation Rules chapter (## 5).
  expect_true("### Plainuniquerule" %in% full)
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
  expect_false("**How to read the rules**" %in% no_rules)
  expect_false(any(grepl("Plainuniquerule", no_rules, fixed = TRUE)))

  # No signatories: no approval chapter and no blank line to sign on.
  expect_false(any(grepl("Approv|____", write())))
  # Footer: deterministic, no "Generated", nothing that depends on the package
  # version; without a template only the document's name.
  n <- length(full)
  expect_identical(full[(n - 2):n], c("---", "", "Dataset Specification: Plain dataset name (trial\\_data)"))
  expect_false(any(grepl("Built-in layout|DTAtools", full)))
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

test_that("a dataset's facts in write_dta do not repeat its id: a sentence, or a table with the template", {
  lines <- readLines(.mdd_write(.mdd_fixture()), warn = FALSE, encoding = "UTF-8")
  at <- match("### 7.1 Plain dataset name (trial\\_data)", lines)
  expect_identical(lines[at + 1:5], c("", "Plain dataset description", "", "Type: Tabular.", ""))
  expect_false(any(grepl("^\\| ID \\|", lines)))

  dta <- .mdd_fixture()
  ds <- dta@datasets[[1]]
  ds@template_source <- "GF domain"
  ds@template_version <- "3.0"
  ds@template_date <- "2024-12-17"
  dta@datasets <- list(trial_data = ds)
  lines <- readLines(.mdd_write(dta), warn = FALSE, encoding = "UTF-8")
  at <- match("### 7.1 Plain dataset name (trial\\_data)", lines)
  expect_identical(lines[at + 3:9], c(
    "", "| Item | Details |", "| --- | --- |", "| Type | Tabular |",
    "| Template source | GF domain |", "| Template version | 3.0 |", "| Template date | 2024-12-17 |"
  ))
})

test_that("how to read the file table is explained once, the count is given per dataset", {
  dta <- .mdd_fixture()
  second <- dta@datasets[[1]]
  second@id <- "other_data"
  second@files <- list(DTAFileCSV(filename = "a.csv"), DTAFileCSV(filename = "b.csv"))
  dta@datasets <- list(trial_data = dta@datasets[[1]], other_data = second)
  lines <- readLines(.mdd_write(dta), warn = FALSE, encoding = "UTF-8")
  explanation <- paste0(
    "Each file name is given either as an exact name or as a regular-expression ",
    "pattern that a delivered file must match; the expected count states how many files may match it."
  )
  expect_identical(sum(grepl(explanation, lines, fixed = TRUE)), 1L)
  expect_true(paste("1 file is expected for this dataset in total.", explanation) %in% lines)
  expect_true("2 files are expected for this dataset in total." %in% lines)
  # The table is explained in the first Files chapter.
  expect_lt(
    match("#### 7.1.1 Files", lines),
    grep(explanation, lines, fixed = TRUE)
  )
  expect_lt(grep(explanation, lines, fixed = TRUE), match("#### 7.2.1 Files", lines))
})

test_that("files without content in a column leave the column out; 'File format' is a heading", {
  lines <- readLines(.mdd_write(.mdd_fixture()), warn = FALSE, encoding = "UTF-8")
  # FREE has neither label, values nor description, but other columns do.
  expect_true("| Variable Name | Label | Type | Nullable | Allowed Values / Pattern | Description |" %in% lines)
  # The contact table: every contact has every field here.
  expect_true("| Name | Role | Department | Contact | Responsibilities |" %in% lines)
  expect_true("##### File format" %in% lines)
  expect_false("**File format**" %in% lines)

  # A file table whose files have no description has no Description column.
  ds <- create_example_DTADataSetTabular(2)
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  ds@files <- list(DTAFileCSV(filename = "x.csv"))
  write_dataset_metadata(ds, out, format = "md", quiet = TRUE)
  ds_lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  expect_true("| File Name / Pattern | Match Type | Format | Expected Count |" %in% ds_lines)
  expect_true("### File format" %in% ds_lines)
})

test_that("a rule block is a heading with an anchor, its kind, and its text without stray lists", {
  .mdd_skip_without_pandoc()
  rule <- DTARuleColUnique(id = "check_1*a", columns = "CODE", description = "Intro\n\n1.\n\n2)\nnext line")
  block <- .md_rule_block(rule, level = 5L)
  expect_identical(block[1:3], c("##### check\\_1\\*a", "", "*Unique*"))
  # The purpose keeps its paragraphs and line breaks; "1." and "2)" are text.
  expect_identical(block[5:9], c("**Purpose:** Intro", "", "1\\.", "", "2\\)\\"))
  expect_identical(block[10], "next line")

  columns <- list(CODE = DTAColumnSpec(id = "CODE", type = "SAS Char"))
  dta <- DTA(
    datasets = list(d = DTADataSetTabular(id = "d", specs = DTAColumnSpecCollection(columns = columns, rules = list(rule)))),
    metadata = DTAMetaData(title = "Rules", version = "1")
  )
  path <- .mdd_write(dta)
  # Through a file: stdout = TRUE re-encodes pandoc's UTF-8 in the native
  # encoding on Windows and breaks the JSON.
  json_file <- tempfile(fileext = ".json")
  on.exit(unlink(json_file, force = TRUE), add = TRUE)
  system2(Sys.which("pandoc"), c("-f", "gfm", "-t", "json", "-o", shQuote(json_file), shQuote(path)))
  ast <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)$blocks
  types <- vapply(ast, function(b) b$t, character(1))
  expect_false("OrderedList" %in% types)
  heads <- Filter(function(b) b$t == "Header" && b$c[[1]] == 5L, ast)
  expect_length(heads, 1L)
  expect_identical(heads[[1]]$c[[2]][[1]], .md_slug("check_1*a")$slug)
  expect_identical(heads[[1]]$c[[2]][[1]], "check_1a")
})

test_that("the dataset document's footer is one line, with the template when there is one", {
  yaml_path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  skip_if(!nzchar(yaml_path), "gf_dataset.yaml missing")
  ds <- read_dataset_from_yaml(yaml_path)
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, out, format = "md", quiet = TRUE)
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  n <- length(lines)
  expect_identical(lines[(n - 2):n], c(
    "---", "",
    paste(
      "Dataset Specification: Genomic Findings data table (gf\\_data\\_specs\\_pattern)",
      "Template: GF domain smrnaseq", "version 3.0", "2024-12-17",
      sep = " \u00b7 "
    )
  ))
  # The head names the dataset once (subtitle) and tables the rest.
  expect_identical(lines[3], "**Dataset Specification: Genomic Findings data table (gf\\_data\\_specs\\_pattern)**")
  expect_identical(lines[5:10], c(
    "| Item | Details |", "| --- | --- |", "| Type | Tabular |",
    "| Template source | GF domain smrnaseq |", "| Template version | 3.0 |", "| Template date | 2024-12-17 |"
  ))
})
