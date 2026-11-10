# Document export. REQ-EXPORT-001 .. REQ-EXPORT-019, REQ-EXPORT-032 and REQ-EXPORT-036 .. REQ-EXPORT-042.
#
# write_dta(), write_dataset_metadata()/write_file_specification(),
# export_specs_table(), export_column_value_table(), export_with_template()
# and columns_specs_from_word(). Every case reads the produced file back and
# asserts its content against the shipped clinical_dta.yaml specification (or
# a small local fixture), never merely that a file exists.

# ---- fixtures ---------------------------------------------------------------

# The same shipped specification test-PQ-workflows.R validates against. Every
# expected value used below (title, version, supplier/receiver names, column
# ids, rule ids) is copied here from inst/extdata/clinical_dta.yaml itself,
# not captured from a run.
et_dta <- function() {
  read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
}

# A DTA with no rules and no punctuation outside plain ASCII. The rule
# translations used elsewhere in this file render operators such as "->" and
# "!=" as single Unicode glyphs (see R/formattingHelpers.R), which the
# default pdflatex font encoding cannot typeset -- a LaTeX-backend concern
# entirely unrelated to what write_dta(format = "pdf") itself is being asked
# to do here, so the PDF case below is kept to a fixture that cannot trip it.
et_minimal_dta <- function() {
  DTA(
    datasets = list(DTADataSetTabular(
      id = "demo",
      specs = DTAColumnSpecCollection(columns = list(
        ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
      ))
    )),
    metadata = DTAMetaData(title = "Minimal PDF Fixture", version = "1.0")
  )
}

# helper-parse.R's qa_docx_tables() groups officer::docx_summary() cells by
# `doc_index`. officer documents doc_index as a running counter over every
# content node in the document -- each paragraph AND each table cell
# increments it -- rather than as a table identifier. Confirmed against
# officer 0.7.6 / flextable 0.10.0 here: a plain 3-row x 2-col flextable
# already produces 8 distinct doc_index values, one per cell, so grouping by
# it shatters every table with more than one cell into a separate one-cell
# "table" per cell. `table_index` is the column that is actually constant
# across one visual table's cells. This local, corrected copy is used in this
# file in its place; reported upstream (see the implementation report for the
# EXPORT work package) rather than edited, since helper-parse.R belongs to
# another work package.
et_docx_tables <- function(path) {
  summary <- qa_docx_summary(path)
  cells <- summary[summary$content_type %in% "table cell", , drop = FALSE]
  if (nrow(cells) == 0) {
    return(list())
  }
  lapply(split(cells, cells$table_index), function(part) {
    wide <- stats::reshape(
      part[, c("row_id", "cell_id", "text")],
      idvar = "row_id", timevar = "cell_id", direction = "wide"
    )
    wide <- wide[order(wide$row_id), , drop = FALSE]
    out <- as.data.frame(wide[, setdiff(names(wide), "row_id"), drop = FALSE])
    names(out) <- sub("^text[.]", "col", names(out))
    rownames(out) <- NULL
    out
  })
}

# The one table (if any) whose header row's first cell is `header1`.
et_docx_table <- function(path, header1) {
  tabs <- et_docx_tables(path)
  hit <- Filter(function(tbl) nrow(tbl) > 0 && identical(tbl$col1[[1]], header1), tabs)
  if (length(hit) == 0) NULL else hit[[1]]
}

# A minimal Word template with one paragraph per line, built the same way the
# package's own roxygen examples build one.
et_write_template <- function(lines, dir) {
  path <- file.path(dir, paste0("template-", as.integer(stats::runif(1, 1, 1e9)), ".docx"))
  doc <- officer::read_docx()
  for (ln in lines) {
    doc <- officer::body_add_par(doc, ln)
  }
  print(doc, target = path)
  path
}

# Two small, self-contained column specifications, so tests that only need a
# collection (not the whole clinical fixture) do not depend on it.
et_two_cols <- function() {
  DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(
      id = "STUDYID", label = "Study Identifier", type = "SAS Char",
      length = 10, nullable = FALSE, description = "Unique study ID"
    ),
    AGE = DTAColumnSpec(
      id = "AGE", label = "Age", type = "SAS Num",
      nullable = TRUE, description = "Age in years"
    )
  ))
}

# ---- reading a Markdown document back ---------------------------------------
#
# These helpers only take the file apart along the syntax GitHub-flavoured
# Markdown defines (ATX headings, pipe tables, list items, autolinks,
# backslash escapes and the three HTML entities); they know nothing of how the
# package builds the file. Every expected value is written in the tests
# themselves.

et_md_lines <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")

# A URL or e-mail autolink, "<scheme://...>" or "<local@domain>", as the
# CommonMark specification defines them (its content is taken literally).
et_md_autolink_rx <- "<((?:https?|ftp)://[^<> ]*|[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+)>"

# Escaped Markdown text as a reader sees it: a raw "<br>" is a line break, an
# autolink is the address it shows, a backslash escape is the character it
# escapes, the entities their character. An autolink's content is literal, so
# it is set aside before the escapes are resolved.
et_md_unescape <- function(x) {
  vapply(x, et_md_unescape_one, character(1), USE.NAMES = FALSE)
}

et_md_unescape_one <- function(x) {
  x <- gsub("<br>", "\n", x, fixed = TRUE)
  links <- regmatches(x, gregexpr(et_md_autolink_rx, x, perl = TRUE))[[1]]
  parts <- regmatches(x, gregexpr(et_md_autolink_rx, x, perl = TRUE), invert = TRUE)[[1]]
  parts <- vapply(parts, et_md_unescape_text, character(1), USE.NAMES = FALSE)
  links <- substr(links, 2L, nchar(links) - 1L)
  # In a table the cell boundary is read first, so a "|" inside an autolink
  # is written "\|" there too.
  links <- gsub("\\|", "|", links, fixed = TRUE)
  paste0(parts, c(links, ""), collapse = "")
}

et_md_unescape_text <- function(x) {
  x <- gsub("\\\\([[:punct:]])", "\\1", x)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# The ATX headings: level (number of #), chapter number ("" if none) and the
# unescaped title.
et_md_headings <- function(lines) {
  hits <- regmatches(lines, regexec("^(#{1,6}) (.*)$", lines))
  hits <- Filter(length, hits)
  level <- vapply(hits, function(m) nchar(m[[2]]), integer(1))
  text <- vapply(hits, function(m) m[[3]], character(1))
  numbered <- grepl("^[0-9]+(\\.[0-9]+)* ", text)
  number <- ifelse(numbered, sub(" .*$", "", text), "")
  title <- ifelse(numbered, sub("^[^ ]+ ", "", text), text)
  data.frame(level = level, number = number, title = et_md_unescape(title), stringsAsFactors = FALSE)
}

# Every pipe table: a list of tables, each a list of rows (header row first,
# the delimiter row dropped), each row the unescaped cells. An escaped pipe
# is written "\|", so a cell boundary is a pipe with a blank on both sides.
et_md_tables <- function(lines) {
  is_row <- grepl("^\\| .* \\|$", lines) | grepl("^\\|  ?\\|$", lines)
  runs <- rle(is_row)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  out <- list()
  for (k in which(runs$values)) {
    rows <- lines[starts[[k]]:ends[[k]]]
    rows <- rows[!grepl("^\\|( :?-{3,}:? \\|)+$", rows)]
    out[[length(out) + 1L]] <- lapply(rows, function(r) {
      inner <- substr(r, 3L, nchar(r) - 2L)
      et_md_unescape(strsplit(inner, " | ", fixed = TRUE)[[1]])
    })
  }
  out
}

# All two-cell rows of all tables, as "Item = Details" strings.
et_md_pairs <- function(lines) {
  rows <- unlist(et_md_tables(lines), recursive = FALSE)
  rows <- Filter(function(r) length(r) == 2L, rows)
  vapply(rows, function(r) paste(r[[1]], "=", r[[2]]), character(1))
}

# The table whose header row is exactly `header`, as a character matrix of its
# data rows (NULL when there is none).
et_md_table <- function(lines, header) {
  hit <- Filter(function(t) identical(t[[1]], header), et_md_tables(lines))
  if (length(hit) == 0) {
    return(NULL)
  }
  do.call(rbind, hit[[1]][-1])
}

# The entries of the "## Contents" list: indentation depth, link text and
# link target.
et_md_toc <- function(lines) {
  start <- match("## Contents", lines)
  if (is.na(start)) {
    return(NULL)
  }
  rest <- lines[-seq_len(start)]
  stop_at <- match(TRUE, grepl("^#", rest))
  block <- rest[seq_len(if (is.na(stop_at)) length(rest) else stop_at - 1L)]
  hits <- Filter(length, regmatches(block, regexec("^( *)- \\[(.*)\\]\\(#([^)]*)\\)$", block)))
  data.frame(
    depth = vapply(hits, function(m) nchar(m[[2]]) %/% 2L + 1L, integer(1)),
    text = et_md_unescape(vapply(hits, function(m) m[[3]], character(1))),
    target = vapply(hits, function(m) m[[4]], character(1)),
    stringsAsFactors = FALSE
  )
}

# The headings of a Word document: level (from the "heading N" style) and text.
et_word_headings <- function(path) {
  s <- qa_docx_summary(path)
  s <- s[!is.na(s$style_name) & grepl("^heading [0-9]$", s$style_name), , drop = FALSE]
  data.frame(
    level = as.integer(sub("heading ", "", s$style_name, fixed = TRUE)),
    title = s$text, stringsAsFactors = FALSE
  )
}

# The XML parts of a .docx that carry what a reader sees: the body and every
# running header and footer, by part name.
et_docx_parts <- function(path) {
  dir <- tempfile("docx-parts-", tmpdir = qa_tempdir())
  utils::unzip(path, exdir = dir)
  files <- list.files(file.path(dir, "word"), pattern = "^(document|header[0-9]*|footer[0-9]*)[.]xml$")
  stats::setNames(lapply(files, function(f) readBin(file.path(dir, "word", f), "raw", 1e7)), files)
}

et_raw <- function(path) readBin(path, "raw", file.size(path))

# ---- export_column_value_table() input validation ---------------------------

test_that("OQ-EXPORT-001 | export_column_value_table on a bad first argument raises a package error | REQ-EXPORT-001", {
  dir <- qa_tempdir()
  out <- file.path(dir, "bad.docx")

  cond <- tryCatch(
    export_column_value_table(list(values = "x"), file = out, id = "values", quiet = TRUE),
    error = function(e) e
  )
  qa_check("a condition was raised for the malformed argument", inherits(cond, "condition"))

  # The same classed cli condition export_specs_table() raises for the
  # equivalent mistake (see OQ-EXPORT-017).
  qa_check("the condition is a classed cli (rlang) error", inherits(cond, "rlang_error"))
  qa_check("no file was written for the failed call", !file.exists(out))
})

# ---- write_dta() content -----------------------------------------------------

test_that("OQ-EXPORT-002 | write_dta docx names the transfer title, version, supplier and receiver | REQ-EXPORT-002", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  text <- qa_docx_text(out)
  for (expected in c(
    "Clinical Data Specification", # metadata@title
    "Test Company 2", # metadata@supplier$affiliation$name
    "Test Company" # metadata@receiver$affiliation$name
  )) {
    qa_step(
      sprintf("the document text contains '%s'", expected),
      TRUE, grepl(expected, text, fixed = TRUE)
    )
  }
  # The version is a labelled cell of the cover and Document Information
  # tables, and is repeated in the running footer.
  cells <- qa_docx_summary(out)$text
  qa_step("the Version label is followed by the declared version", "0.1", cells[match("Version", cells) + 1L])
})

test_that("OQ-EXPORT-003 | write_dta docx names every dataset and its declared columns | REQ-EXPORT-002 REQ-EXPORT-003", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  text <- qa_docx_text(out)
  qa_step("the dataset id appears", TRUE, grepl("clinical_data", text, fixed = TRUE))
  qa_step(
    "and the dataset's label appears with its id kept beside it",
    TRUE, grepl("Clinical data table (clinical_data)", text, fixed = TRUE)
  )

  declared_columns <- c(
    "STUDYID", "VISIT", "AGE", "GENDER", "INCLUDE", "HEIGHT", "WEIGHT",
    "BMI", "STATUS", "CONSENT", "CONSENT_DATE", "AE_TERM", "SAE_TERM", "SUBJECT_ID"
  )
  present <- vapply(declared_columns, function(id) grepl(id, text, fixed = TRUE), logical(1))
  qa_step(
    "every declared column id appears in the document",
    rep(TRUE, length(declared_columns)), unname(present)
  )
})

test_that("OQ-EXPORT-004 | write_dta docx column specification table matches the declared columns | REQ-EXPORT-003", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  tbl <- et_docx_table(out, "Variable Name")
  qa_check("the column specification table is found", !is.null(tbl))

  qa_step("the table has one header row plus one row per declared column", 15L, nrow(tbl))
  qa_step(
    "the header names the six documented facets",
    c("Variable Name", "Label", "Type", "Nullable", "Allowed Values / Pattern", "Description"),
    as.character(tbl[1, ])
  )

  studyid <- tbl[tbl$col1 == "STUDYID", ]
  qa_step(
    "the STUDYID row carries its declared label, type, nullability and permitted value",
    c("Study Identifier", "SAS Char (10)", "No", "1234-5678"),
    c(studyid$col2, studyid$col3, studyid$col4, studyid$col5)
  )

  visit <- tbl[tbl$col1 == "VISIT", ]
  qa_step("the VISIT row lists its declared codelist", "V01, V02, V03, EOT", visit$col5)
})

test_that("OQ-EXPORT-005 | write_dta docx validation rules table names every declared rule | REQ-EXPORT-004", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = out, quiet = TRUE)

  tbl <- et_docx_table(out, "Rule ID")
  qa_check("the rules table is found", !is.null(tbl))
  qa_step("it lists a header row plus the eight declared rules", 9L, nrow(tbl))

  declared_rules <- c(
    "rule_equal_example", "rule_unequal_example", "rule_range_example",
    "rule_dependency_example", "rule_unique_example", "check_col_condition_example",
    "group_condition_pass_example", "group_condition_fail_example"
  )
  qa_step("every declared rule id is named, in declaration order", declared_rules, tbl$col1[-1])
})

test_that("OQ-EXPORT-006 | include_signatures adds an approval section naming the flagged signatories | REQ-EXPORT-005", {
  dir <- qa_tempdir()
  out <- file.path(dir, "signed.docx")
  write_dta(et_dta(), file = out, quiet = TRUE, include_signatures = TRUE)

  text <- qa_docx_text(out)
  qa_step("the approval heading is present", TRUE, grepl("Approval & Signatures", text, fixed = TRUE))

  # These three contacts (two receiver, one supplier) are the only ones the
  # fixture flags with `signature: yes`; the rest are reviewer- or backup-only.
  for (name in c("Alice Smith", "Bob Johnson", "Emily Turner")) {
    qa_step(sprintf("signatory '%s' is named", name), TRUE, grepl(name, text, fixed = TRUE))
  }
  qa_step("the approval chapter is the first chapter", "Approval & Signatures", et_word_headings(out)$title[[1]])

  md <- file.path(dir, "signed.md")
  write_dta(et_dta(), file = md, format = "md", quiet = TRUE, include_signatures = TRUE)
  lines <- et_md_lines(md)
  heads <- et_md_headings(lines)
  chapters <- heads[nzchar(heads$number), , drop = FALSE]
  # REQ-EXPORT-005: the Markdown heading spells the "&" out.
  qa_step(
    "Markdown: the first chapter is 1 Approval and Signatures",
    c("1", "Approval and Signatures"), c(chapters$number[[1]], chapters$title[[1]])
  )
  sig <- et_md_table(lines, c("Organization", "Name", "Role", "Signature", "Date"))
  qa_step(
    "Markdown: the approval table names exactly the three flagged signatories",
    c("Alice Smith", "Bob Johnson", "Emily Turner"), sort(sig[, 2])
  )
})

test_that("OQ-EXPORT-007 | include_signatures = FALSE omits the approval section | REQ-EXPORT-005", {
  dir <- qa_tempdir()
  out <- file.path(dir, "unsigned.docx")
  write_dta(et_dta(), file = out, quiet = TRUE, include_signatures = FALSE)

  qa_step(
    "no approval heading is present",
    FALSE, grepl("Approval & Signatures", qa_docx_text(out), fixed = TRUE)
  )

  md <- file.path(dir, "unsigned.md")
  write_dta(et_dta(), file = md, format = "md", quiet = TRUE, include_signatures = FALSE)
  lines <- et_md_lines(md)
  qa_step("Markdown: no approval text is present", FALSE, any(grepl("Approval", lines, fixed = TRUE)))
  qa_step("Markdown: chapter 1 is then Document Information", "1 Document Information", {
    heads <- et_md_headings(lines)
    paste(heads$number[nzchar(heads$number)][[1]], heads$title[nzchar(heads$number)][[1]])
  })
})

test_that("OQ-EXPORT-008 | include_yaml embeds the given specification text | REQ-EXPORT-006", {
  dir <- qa_tempdir()
  out <- file.path(dir, "with_yaml.docx")
  marker <- "metadata:\n  title: marker-for-embed-test\n"
  write_dta(et_dta(), file = out, quiet = TRUE, include_yaml = TRUE, yaml_text = marker)

  text <- qa_docx_text(out)
  qa_step(
    "the embedded-specification heading is present",
    TRUE, grepl("Embedded Specification (YAML)", text, fixed = TRUE)
  )
  qa_step("the given yaml text is embedded", TRUE, grepl("marker-for-embed-test", text, fixed = TRUE))
})

test_that("OQ-EXPORT-009 | include_yaml with no yaml_text warns and embeds nothing | REQ-EXPORT-006", {
  dir <- qa_tempdir()
  out <- file.path(dir, "no_yaml.docx")

  warned <- FALSE
  withCallingHandlers(
    write_dta(et_dta(), file = out, quiet = TRUE, include_yaml = TRUE, yaml_text = NULL),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  qa_check("a warning was raised", warned)
  qa_step(
    "no embedded-specification heading was added",
    FALSE, grepl("Embedded Specification", qa_docx_text(out), fixed = TRUE)
  )
})

test_that("OQ-EXPORT-010 | format = \"md\" writes a numbered GFM file with the Word chapters, a linked contents list and the same facts | REQ-EXPORT-007", {
  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.md")
  write_dta(et_dta(), file = out, format = "md", quiet = TRUE)
  docx <- file.path(dir, "transfer.docx")
  write_dta(et_dta(), file = docx, quiet = TRUE)

  # Encoding and line endings, on the bytes.
  bytes <- et_raw(out)
  n <- length(bytes)
  qa_check("the file is valid UTF-8", validUTF8(rawToChar(bytes)))
  qa_step("no carriage return anywhere (LF line endings)", 0L, sum(bytes == as.raw(13L)))
  qa_check(
    "the file ends with exactly one line ending",
    n >= 2L && bytes[[n]] == as.raw(10L) && bytes[[n - 1L]] != as.raw(10L)
  )

  lines <- et_md_lines(out)
  heads <- et_md_headings(lines)
  qa_step("the document name is the only level-1 heading", "Data Transfer Agreement", heads$title[heads$level == 1L])

  # The chapters the fixture gives content to, in the order and at the
  # levels REQ-EXPORT-007 lists: it has signatories (include_signatures
  # defaults to TRUE), a document header (so the Markdown Document
  # Information chapter keeps a row), a supplier and a receiver,
  # transmission details and one dataset, and no version history, study,
  # error handling, correction list or agreement clause.
  titles <- c(
    "Approval & Signatures", "Document Information", "Parties", "Supplier", "Receiver",
    "Process Information", "Transmission Details", "Datasets",
    "Clinical data table (clinical_data)", "Files", "Column Specifications", "Validation Rules"
  )
  md_titles <- replace(titles, 1L, "Approval and Signatures")
  levels <- c(1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 3L, 3L, 3L)
  expected <- data.frame(
    level = levels + 1L,
    number = c("1", "2", "3", "3.1", "3.2", "4", "4.1", "5", "5.1", "5.1.1", "5.1.2", "5.1.3"),
    title = md_titles,
    stringsAsFactors = FALSE
  )
  chapters <- heads[nzchar(heads$number), , drop = FALSE]
  rownames(chapters) <- NULL
  qa_step("the numbered chapters, their numbers and levels", expected, chapters)
  qa_step(
    "and the Word document has the same chapters, in the same order, one level higher",
    data.frame(level = levels, title = titles, stringsAsFactors = FALSE),
    et_word_headings(docx)
  )

  # The contents list: chapters of the first two levels, each linked to the
  # anchor GitHub derives from the heading text (lower case, punctuation other
  # than "-" and "_" removed, each space a "-").
  toc <- et_md_toc(lines)
  qa_check("a Contents list comes before the first chapter", match("## Contents", lines) < match("## 1 Approval and Signatures", lines))
  qa_step(
    "the Contents list links the chapters of the first two levels",
    c(
      "1 Approval and Signatures", "2 Document Information", "3 Parties", "3.1 Supplier", "3.2 Receiver",
      "4 Process Information", "4.1 Transmission Details", "5 Datasets",
      "5.1 Clinical data table (clinical_data)"
    ),
    toc$text
  )
  qa_step("each at its own depth", c(1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L), toc$depth)
  qa_step(
    "to the anchor GitHub computes for the heading",
    c(
      "1-approval-and-signatures", "2-document-information", "3-parties", "31-supplier", "32-receiver",
      "4-process-information", "41-transmission-details", "5-datasets", "51-clinical-data-table-clinical_data"
    ),
    toc$target
  )

  # Facts, each from inst/extdata/clinical_dta.yaml.
  qa_check("the transfer's title follows the document name in bold", "**Clinical Data Specification**" %in% lines)
  pairs <- et_md_pairs(lines)
  # The head shows version and date; Document Information keeps only what
  # the head does not show (REQ-EXPORT-007): here, the document header.
  chapter_2 <- lines[seq(match("## 2 Document Information", lines), match("## 3 Parties", lines) - 1L)]
  qa_step(
    "Document Information holds the document header and nothing the head shows",
    list(c("Item", "Details"), c("Document header", "Company")),
    et_md_tables(chapter_2)[[1]]
  )
  qa_step("and holds that one table only", 1L, length(et_md_tables(chapter_2)))
  for (pair in c("Version = 0.1", "Date = 2024-06-19", "Organization = Test Company 2", "Organization = Test Company")) {
    qa_check(paste0("the fact '", pair, "' is in a fact table"), pair %in% pairs)
  }

  contacts <- rbind(
    et_md_table(lines, c("Name", "Role", "Department", "Contact", "Responsibilities"))
  )
  all_contacts <- do.call(rbind, lapply(
    Filter(function(t) identical(t[[1]], c("Name", "Role", "Department", "Contact", "Responsibilities")), et_md_tables(lines)),
    function(t) do.call(rbind, t[-1])
  ))
  qa_step(
    "both contact tables together list all six contacts",
    sort(c("Alice Smith", "Bob Johnson", "Carol Lee", "David Kim", "Emily Turner", "Michael Brown")),
    sort(all_contacts[, 1])
  )
  emily <- all_contacts[all_contacts[, 1] == "Emily Turner", ]
  qa_step(
    "a contact row carries role, department, e-mail address and phone number",
    c("Senior Data Manager", "Data Management", "emily.turner@testcompany2.com\n321-654-0987"),
    unname(emily[2:4])
  )
  qa_check("the supplier's contact table is the first one", identical(contacts[1, 1], "Emily Turner"))

  # The transmission details under the labels the Word document gives them;
  # the same pairs are checked in the Word document's cells.
  transmission <- c(
    "Transfer method" = "secure S3 bucket", "Frequency" = "one-time", "Notification" = "email",
    "Test upload" = "No", "Blinded transfer" = "No",
    "First transfer" = "2025-11-25", "Last transfer" = "2026-01-15"
  )
  cells <- qa_docx_summary(docx)
  cells <- cells$text[cells$content_type %in% "table cell"]
  for (label in names(transmission)) {
    qa_check(
      paste0("Markdown states '", label, "' as ", transmission[[label]]),
      paste(label, "=", transmission[[label]]) %in% pairs
    )
    qa_step(paste0("and so does the Word document"), transmission[[label]], cells[match(label, cells) + 1L])
  }

  # The fixture's file has no description, so the file table has no
  # Description column (REQ-EXPORT-007: a column no row fills is left out).
  files <- et_md_table(lines, c("File Name / Pattern", "Match Type", "Format", "Expected Count"))
  qa_step("the declared file pattern is listed", "clinical_data.*.csv$", files[1, 1])
  qa_check(
    "no table has a Description column that no row fills",
    !any(vapply(et_md_tables(lines), function(t) {
      col <- match("Description", t[[1]])
      !is.na(col) && length(t) > 1L && all(vapply(t[-1], function(r) !nzchar(r[[col]]), logical(1)))
    }, logical(1)))
  )

  cols <- et_md_table(lines, c("Variable Name", "Label", "Type", "Nullable", "Allowed Values / Pattern", "Description"))
  qa_step("the column table has one row per declared column", 14L, nrow(cols))
  qa_step(
    "the STUDYID row carries label, type, nullability, permitted value and description",
    c("STUDYID", "Study Identifier", "SAS Char (10)", "No", "1234-5678", "Unique study ID"),
    unname(cols[cols[, 1] == "STUDYID", ])
  )
  qa_step("the VISIT row lists its codelist", "V01, V02, V03, EOT", unname(cols[cols[, 1] == "VISIT", 5]))

  # A codelist far longer than a table cell comfortably holds is still listed
  # in full.
  codes <- sprintf("CODE%02d", 1:40)
  long <- DTA(
    datasets = list(DTADataSetTabular(
      id = "long_list",
      specs = DTAColumnSpecCollection(columns = list(
        CODE = DTAColumnSpec(id = "CODE", type = "SAS Char", length = 6, nullable = FALSE, values = codes)
      ))
    )),
    metadata = DTAMetaData(title = "Long list", version = "1.0")
  )
  long_md <- file.path(dir, "long.md")
  write_dta(long, file = long_md, format = "md", quiet = TRUE)
  # The column declares neither label nor description: those two columns of
  # the table are left out.
  long_cols <- et_md_table(et_md_lines(long_md), c("Variable Name", "Type", "Nullable", "Allowed Values / Pattern"))
  qa_step("the column table keeps the four filled columns", 4L, ncol(long_cols))
  qa_step("all 40 permitted values are listed, none cut off", paste(codes, collapse = ", "), unname(long_cols[1, 4]))
  # Dataset facts beneath the heading: the id is in the heading, the type is
  # the only fact left, so it is a sentence (REQ-EXPORT-007).
  qa_check("the dataset's only remaining fact is the sentence 'Type: Tabular.'", "Type: Tabular." %in% lines)
  qa_check("and no fact row repeats the dataset id", !any(grepl("^ID = ", pairs)))
  qa_check("the long-list dataset reads the same", "Type: Tabular." %in% et_md_lines(long_md))
})

test_that("OQ-EXPORT-011 | format = \"pdf\" produces a genuine PDF when a backend is available | REQ-EXPORT-008", {
  backend <- dta_pdf_backend()
  if (is.null(backend)) {
    testthat::skip("no PDF conversion backend available (dta_pdf_backend() is NULL)")
  }

  dir <- qa_tempdir()
  out <- file.path(dir, "transfer.pdf")
  write_dta(et_minimal_dta(), file = out, format = "pdf", quiet = TRUE)
  qa_check("the file was written", file.exists(out))

  con <- file(out, open = "rb")
  on.exit(close(con), add = TRUE)
  qa_step(
    "the file begins with the PDF signature",
    charToRaw("%PDF"), readBin(con, what = "raw", n = 4L)
  )
})

test_that("OQ-EXPORT-012 | write_dta refuses to overwrite an existing file and leaves it untouched | REQ-EXPORT-009", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.docx")
  writeLines("not a real document, just a marker of the original content", out)
  original <- readLines(out)

  cond <- tryCatch(write_dta(et_dta(), file = out, quiet = TRUE), error = function(e) e)
  qa_check("a condition was raised", inherits(cond, "condition"))
  qa_step("the original file content is unchanged", original, readLines(out))
})

test_that("OQ-EXPORT-013 | write_dataset_metadata refuses to overwrite an existing file and leaves it untouched | REQ-EXPORT-009", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.docx")
  writeLines("not a real document, just a marker of the original content", out)
  original <- readLines(out)

  ds <- et_dta()[["clinical_data"]]
  cond <- tryCatch(write_dataset_metadata(ds, file = out, quiet = TRUE), error = function(e) e)
  qa_check("a condition was raised", inherits(cond, "condition"))
  qa_step("the original file content is unchanged", original, readLines(out))
})

# ---- write_dataset_metadata() / write_file_specification() ------------------

test_that("OQ-EXPORT-014 | write_dataset_metadata names the dataset, its files and its columns | REQ-EXPORT-010", {
  ds <- et_dta()[["clinical_data"]]
  dir <- qa_tempdir()
  out <- file.path(dir, "dataset.docx")
  write_dataset_metadata(ds, file = out, quiet = TRUE)

  # Expected values from inst/extdata/clinical_dta.yaml: id clinical_data,
  # name "Clinical data table", type tabular, the folded description below.
  description <- paste(
    "Subject-level clinical observations for the study, one row per subject and",
    "visit, exported from the EDC system as CSV. The transfer is checked",
    "against the column specification and validation rules below."
  )
  chapters <- c("Description", "Files", "Column Specifications", "Validation Rules")

  paras <- qa_docx_summary(out)$text
  qa_check(
    "the title names the dataset by its label with its id beside it",
    "Dataset Specification: Clinical data table (clinical_data)" %in% paras
  )
  # The title block's fact table has no header row: its first row is the
  # Dataset ID row.
  facts <- et_docx_tables(out)[[1]]
  qa_step(
    "the fact table lists Dataset ID, Name and Type, in that order",
    c("Dataset ID", "Name", "Type"), facts$col1
  )
  qa_step(
    "with the id, the name and the type as the fixture declares them",
    c("clinical_data", "Clinical data table", "Tabular"), facts$col2
  )
  heads <- et_word_headings(out)
  qa_step("the chapters, in order (no signatories, so no approval chapter)", chapters, heads$title)
  qa_step("all of them top-level chapters", rep(1L, 4), heads$level)
  qa_check("the Description chapter gives the description in full", description %in% paras)
  file_tab <- et_docx_table(out, "File Name / Pattern")
  qa_step("the declared file pattern is listed", "clinical_data.*.csv$", file_tab$col1[[2]])
  col_tab <- et_docx_table(out, "Variable Name")
  qa_step("the column table has one row per declared column", 15L, nrow(col_tab))

  md <- file.path(dir, "dataset.md")
  write_dataset_metadata(ds, file = md, format = "md", quiet = TRUE)
  lines <- et_md_lines(md)
  qa_check(
    "Markdown: the title names the dataset by its label with its id beside it",
    "**Dataset Specification: Clinical data table (clinical\\_data)**" %in% lines
  )
  # REQ-EXPORT-010: the title names id and name, so the Markdown head leaves
  # them out; the type is then the only fact (the fixture records no
  # template), stated as a sentence. The head is everything before the
  # contents list.
  head_block <- lines[seq_len(match("## Contents", lines) - 1L)]
  qa_step(
    "Markdown: the head is the document name, the title and the type sentence",
    c(
      "# Data Transfer Agreement", "**Dataset Specification: Clinical data table (clinical\\_data)**",
      "Type: Tabular."
    ),
    head_block[nzchar(head_block)]
  )
  qa_step("Markdown: the head holds no fact table", 0L, length(et_md_tables(head_block)))

  # With a recorded template there is more than the type to state: a fact
  # table, still without Dataset ID and Name.
  tpl_ds <- DTADataSetTabular(
    id = "tpl_set", name = "Template set", template_source = "Example source", template_version = "2.0",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
    ))
  )
  tpl_md <- file.path(dir, "tpl.md")
  write_dataset_metadata(tpl_ds, file = tpl_md, format = "md", quiet = TRUE)
  tpl_lines <- et_md_lines(tpl_md)
  tpl_head <- tpl_lines[seq_len(match("## Contents", tpl_lines) - 1L)]
  tpl_tab <- et_md_tables(tpl_head)
  qa_step("Markdown with a template: the head holds one fact table", 1L, length(tpl_tab))
  tpl_docx <- file.path(dir, "tpl.docx")
  write_dataset_metadata(tpl_ds, file = tpl_docx, quiet = TRUE)
  word_facts <- et_docx_tables(tpl_docx)[[1]]
  word_rows <- Map(c, word_facts$col1, word_facts$col2)
  word_rows <- unname(Filter(function(r) !r[[1]] %in% c("Dataset ID", "Name"), word_rows))
  qa_step(
    "and its rows are the Word fact table's rows without Dataset ID and Name",
    word_rows, if (length(tpl_tab) > 0) tpl_tab[[1]][-1] else NULL
  )
  qa_check(
    "which state the type, the template source and its version",
    all(c("Tabular", "Example source", "2.0") %in% vapply(word_rows, `[[`, "", 2L))
  )
  md_heads <- et_md_headings(lines)
  md_heads <- md_heads[nzchar(md_heads$number), , drop = FALSE]
  qa_step("Markdown: the same chapters, numbered 1 to 4", paste(1:4, chapters), paste(md_heads$number, md_heads$title))
  qa_check("Markdown: the Description chapter gives the description in full", description %in% lines)
  # No file has a description: the Description column is left out.
  md_files <- et_md_table(lines, c("File Name / Pattern", "Match Type", "Format", "Expected Count"))
  qa_step("Markdown: the declared file pattern is listed", "clinical_data.*.csv$", md_files[1, 1])
  md_cols <- et_md_table(lines, c("Variable Name", "Label", "Type", "Nullable", "Allowed Values / Pattern", "Description"))
  qa_step("Markdown: the column table has one row per declared column", 14L, nrow(md_cols))
})

test_that("OQ-EXPORT-015 | write_file_specification is the write_dataset_metadata alias | REQ-EXPORT-010 REQ-EXPORT-040", {
  ds <- et_dta()[["clinical_data"]]
  dir <- qa_tempdir()
  out_a <- file.path(dir, "a.docx")
  out_b <- file.path(dir, "b.docx")
  write_dataset_metadata(ds, file = out_a, quiet = TRUE)
  write_file_specification(ds, file = out_b, quiet = TRUE)

  # The parts a reader sees, not the archive's bytes: a DOCX is a zip archive
  # whose entries carry the time they were packed, so two calls made a second
  # apart differ in bytes while saying exactly the same thing. The document
  # itself carries no time (REQ-EXPORT-040), so the parts must be identical.
  parts_a <- et_docx_parts(out_a)
  parts_b <- et_docx_parts(out_b)
  qa_check("the document has a body, a running header and a running footer", all(c("document.xml") %in% names(parts_a)) &&
    any(startsWith(names(parts_a), "header")) && any(startsWith(names(parts_a), "footer")))
  qa_step("the alias writes the same parts", names(parts_a), names(parts_b))
  qa_check(
    "and every body, header and footer part is identical",
    identical(parts_a, parts_b)
  )

  md_a <- file.path(dir, "a.md")
  md_b <- file.path(dir, "b.md")
  write_dataset_metadata(ds, file = md_a, format = "md", quiet = TRUE)
  write_file_specification(ds, file = md_b, format = "md", quiet = TRUE)
  qa_check("the Markdown documents are byte-identical", identical(et_raw(md_a), et_raw(md_b)))
})

# ---- export_specs_table() ---------------------------------------------------

test_that("OQ-EXPORT-016 | export_specs_table writes one row per column in declaration order | REQ-EXPORT-011", {
  dir <- qa_tempdir()
  out <- file.path(dir, "specs.docx")
  export_specs_table(et_two_cols(), file = out, quiet = TRUE)

  tbl <- et_docx_table(out, "Variable Name")
  qa_check("the table is found", !is.null(tbl))
  qa_step(
    "the header names the seven default facets",
    c("Variable Name", "Variable Label", "Type", "Length", "Format", "Nullable", "Description"),
    as.character(tbl[1, ])
  )
  qa_step("the columns appear in declaration order", c("STUDYID", "AGE"), tbl$col1[-1])

  studyid <- tbl[tbl$col1 == "STUDYID", ]
  qa_step(
    "the STUDYID row carries its declared label, type, length, nullability and description",
    c("Study Identifier", "SAS Char", "10", "No", "Unique study ID"),
    c(studyid$col2, studyid$col3, studyid$col4, studyid$col6, studyid$col7)
  )
})

test_that("OQ-EXPORT-017 | export_specs_table rejects a non-collection argument and an empty collection | REQ-EXPORT-012", {
  dir <- qa_tempdir()

  cond1 <- tryCatch(
    export_specs_table(list(a = 1), file = file.path(dir, "bad.docx"), quiet = TRUE),
    error = function(e) e
  )
  qa_check("a bad first argument raises a cli condition", inherits(cond1, "rlang_error"))

  empty <- DTAColumnSpecCollection(columns = list())
  out_empty <- file.path(dir, "empty.docx")
  cond2 <- tryCatch(export_specs_table(empty, file = out_empty, quiet = TRUE), error = function(e) e)
  qa_check("an empty collection raises a cli condition", inherits(cond2, "rlang_error"))
  qa_check("no file was written for the empty collection", !file.exists(out_empty))
})

# ---- export_column_value_table() content ------------------------------------

test_that("OQ-EXPORT-018 | export_column_value_table lists every permitted value of the named column | REQ-EXPORT-013", {
  coll <- DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE, values = c("1234-5678"))
  ))
  dir <- qa_tempdir()
  out <- file.path(dir, "values.docx")
  export_column_value_table(coll, file = out, id = "STUDYID", quiet = TRUE)

  tbl <- et_docx_table(out, "STUDYID")
  qa_check("the value table is found", !is.null(tbl))
  qa_step("the column header names the id, followed by its one permitted value", c("STUDYID", "1234-5678"), tbl$col1)
})

test_that("OQ-EXPORT-019 | export_column_value_table aborts for an unknown column or one without values | REQ-EXPORT-013", {
  coll <- DTAColumnSpecCollection(columns = list(
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  dir <- qa_tempdir()

  cond1 <- tryCatch(
    export_column_value_table(coll, file = file.path(dir, "a.docx"), id = "NOPE", quiet = TRUE),
    error = function(e) e
  )
  qa_check("an unknown column id raises a cli condition", inherits(cond1, "rlang_error"))

  cond2 <- tryCatch(
    export_column_value_table(coll, file = file.path(dir, "b.docx"), id = "AGE", quiet = TRUE),
    error = function(e) e
  )
  qa_check("a column with no permitted values raises a cli condition", inherits(cond2, "rlang_error"))
})

# ---- export_with_template() -------------------------------------------------

test_that("OQ-EXPORT-020 | export_with_template fills every placeholder present in the template | REQ-EXPORT-014", {
  dir <- qa_tempdir()
  template <- et_write_template(
    c("Title: {DTA_TITLE}", "Version: {DTA_VERSION}", "Supplier: {SUPPLIER_NAME}", "Receiver: {RECEIVER_NAME}"),
    dir
  )
  out <- file.path(dir, "filled.docx")
  export_with_template(et_dta(), template, out, quiet = TRUE)

  qa_step(
    "every placeholder is replaced with its extracted value",
    c(
      "Title: Clinical Data Specification", "Version: 0.1",
      "Supplier: Test Company 2", "Receiver: Test Company"
    ),
    strsplit(qa_docx_text(out), "\n", fixed = TRUE)[[1]]
  )
})

test_that("OQ-EXPORT-021 | export_with_template leaves an unmatched placeholder and warns about it | REQ-EXPORT-014", {
  dir <- qa_tempdir()
  template <- et_write_template(c("Known: {DTA_TITLE}", "Unknown: {NOT_A_REAL_PLACEHOLDER}"), dir)
  out <- file.path(dir, "partial.docx")

  warnings_seen <- character(0)
  withCallingHandlers(
    export_with_template(et_dta(), template, out, quiet = TRUE),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  qa_check(
    "a warning naming the unresolved placeholder was raised",
    any(grepl("NOT_A_REAL_PLACEHOLDER", warnings_seen, fixed = TRUE))
  )
  qa_step(
    "the known placeholder is replaced and the unknown one is left as written",
    c("Known: Clinical Data Specification", "Unknown: {NOT_A_REAL_PLACEHOLDER}"),
    strsplit(qa_docx_text(out), "\n", fixed = TRUE)[[1]]
  )
})

test_that("OQ-EXPORT-022 | export_with_template variables override the automatically extracted values | REQ-EXPORT-015", {
  dir <- qa_tempdir()
  template <- et_write_template(c("Title: {DTA_TITLE}"), dir)
  out <- file.path(dir, "overridden.docx")
  export_with_template(et_dta(), template, out, variables = list(DTA_TITLE = "OVERRIDDEN TITLE"), quiet = TRUE)

  qa_step("the caller-supplied value wins over the extracted one", "Title: OVERRIDDEN TITLE", qa_docx_text(out))
})

test_that("OQ-EXPORT-023 | write_dta(template =) routes through export_with_template | REQ-EXPORT-016", {
  dir <- qa_tempdir()
  template <- et_write_template(c("Title: {DTA_TITLE}"), dir)
  out <- file.path(dir, "via_write_dta.docx")
  write_dta(et_dta(), file = out, template = template, quiet = TRUE)

  qa_step(
    "the templated document is written at the requested path",
    "Title: Clinical Data Specification", qa_docx_text(out)
  )
})

test_that("OQ-EXPORT-024 | export_with_template falls back to the standard layout when processing fails | REQ-EXPORT-017", {
  dir <- qa_tempdir()
  bad_template <- file.path(dir, "not_really_a_docx.docx")
  writeLines("this file has a .docx extension but is not a zip archive", bad_template)
  out <- file.path(dir, "fallback.docx")

  # quiet = FALSE here: the warning this case is about is itself conditioned
  # on quiet, per export_with_template()'s own documentation ("signalled as a
  # warning condition (unless quiet = TRUE)") -- quiet = TRUE would silence
  # exactly the behaviour under test.
  warned <- FALSE
  withCallingHandlers(
    export_with_template(et_dta(), bad_template, out, quiet = FALSE),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  qa_check("a warning was raised for the unreadable template", warned)
  qa_step(
    "the output falls back to the standard write_dta layout",
    TRUE, grepl("Data Transfer Agreement", qa_docx_text(out), fixed = TRUE)
  )

  cond <- tryCatch(
    export_with_template(et_dta(), bad_template, file.path(dir, "no_fallback.docx"), quiet = TRUE, fallback = FALSE),
    error = function(e) e
  )
  qa_check("fallback = FALSE raises the failure instead of substituting the layout", inherits(cond, "rlang_error"))
})

# ---- columns_specs_from_word() -----------------------------------------------

test_that("OQ-EXPORT-025 | columns_specs_from_word reconstructs id, type, length and nullable from a well-formed table | REQ-EXPORT-018", {
  dir <- qa_tempdir()

  # Built directly with officer/flextable, so the table is exactly the one
  # this reader documents; the round trip through export_specs_table() is
  # OQ-EXPORT-026.
  fixture <- data.frame(
    id = c("STUDYID", "AGE"),
    label = c("Study Identifier", "Age"),
    type = c("SAS Char", "SAS Num"),
    length = c("10", ""),
    format = c("SAS $10.", "SAS 8."),
    nullable = c("No", "Yes"),
    description = c("Unique study ID", "Age in years"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  path <- file.path(dir, "specs_table.docx")
  doc <- flextable::body_add_flextable(officer::read_docx(), flextable::flextable(fixture))
  print(doc, target = path)

  back <- columns_specs_from_word(
    path,
    colnames = c("id", "label", "type", "length", "format", "nullable", "description")
  )
  qa_step("the same column identifiers come back, in order", c("STUDYID", "AGE"), names(back@columns))
  qa_step(
    "STUDYID keeps its declared type, length and nullability",
    c(type = "Char", length = "10", nullable = "FALSE"),
    c(
      type = back@columns$STUDYID@structure@type,
      length = as.character(back@columns$STUDYID@structure@length),
      nullable = as.character(back@columns$STUDYID@nullable)
    )
  )
  qa_step(
    "AGE keeps its declared type and nullability",
    c(type = "Num", nullable = "TRUE"),
    c(type = back@columns$AGE@structure@type, nullable = as.character(back@columns$AGE@nullable))
  )

  # Values and a pattern travel in the Description cell, one tag per line. A
  # pattern containing the letter n is the case the original regex cut short:
  # its character class `[^\\n]` excluded "n", not the line break.
  tagged <- fixture
  tagged$description <- c(
    "Unique study ID\n#@values: \"S-01\"; \"S-02\"",
    "Age in years\n#@pattern: ^(Screening|Baseline)$\nnot part of the pattern"
  )
  tagged_path <- file.path(dir, "specs_table_tagged.docx")
  print(
    flextable::body_add_flextable(officer::read_docx(), flextable::flextable(tagged)),
    target = tagged_path
  )
  tagged_back <- columns_specs_from_word(tagged_path)
  qa_step(
    "#@values is split on the separator and unquoted",
    c("S-01", "S-02"), unlist(tagged_back@columns$STUDYID@values, use.names = FALSE)
  )
  qa_step(
    "#@pattern is read whole, up to the end of its own line",
    "^(Screening|Baseline)$", tagged_back@columns$AGE@pattern
  )
  qa_step(
    "the tag lines are removed from the description",
    c("Unique study ID", "Age in years\nnot part of the pattern"),
    c(tagged_back@columns$STUDYID@description, tagged_back@columns$AGE@description)
  )
})

test_that("OQ-EXPORT-026 | columns_specs_from_word reads back the table export_specs_table just wrote | REQ-EXPORT-019", {
  dir <- qa_tempdir()

  # The common case first: no column declares an explicit format, exactly as
  # in the shipped clinical_dta.yaml fixture. That blank Format cell used to
  # abort the read (DEV-014, closed). Alongside it, a column with a format --
  # which export_specs_table() writes without its "SAS " prefix -- and one
  # carrying permitted values and a pattern in its Description cell.
  coll <- DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(id = "STUDYID", label = "Study Identifier", type = "SAS Char", length = 10, nullable = FALSE),
    VISIT = DTAColumnSpec(
      id = "VISIT", label = "Visit", type = "SAS Char", format = "SAS $20.", length = 20,
      nullable = TRUE, pattern = "^(Screening|Baseline|Week [0-9]+)$"
    ),
    ARM = DTAColumnSpec(id = "ARM", label = "Arm", type = "SAS Char", length = 8, values = c("Placebo", "Active")),
    VISITNUM = DTAColumnSpec(id = "VISITNUM", label = "Visit Number", type = "SAS Int", format = "SAS 8.", nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", label = "Age", type = "SAS Num", nullable = TRUE)
  ))
  path <- file.path(dir, "roundtrip.docx")
  export_specs_table(coll, file = path, quiet = TRUE)

  back <- tryCatch(columns_specs_from_word(path), error = function(e) e)
  qa_check("the table is read back without error", !inherits(back, "condition"))
  qa_step("the same column identifiers come back, in order", names(coll@columns), names(back@columns))
  facts <- function(x) {
    vapply(x@columns, function(s) {
      paste(
        s@structure@backend, s@structure@type, s@structure@format %||% "-",
        s@structure@length %||% "-", s@nullable %||% "-",
        paste(unlist(s@values), collapse = ";"), s@pattern %||% "-"
      )
    }, character(1))
  }
  qa_step(
    "every column's type, format, length, nullability, values and pattern survive the round trip",
    facts(coll), facts(back)
  )
})

# ---- document exports write atomically ---------------------------------------

test_that("OQ-EXPORT-044 | a document export that fails leaves an existing destination file byte-for-byte unchanged | REQ-EXPORT-032 | tags: white-box", {
  dir <- qa_tempdir()
  out <- file.path(dir, "existing.pdf")
  original <- charToRaw("PRE-EXISTING PDF BYTES, NOT REGENERATED")
  writeBin(original, out)

  # Only one backend is reported, and it produces bytes that do not start
  # with the %PDF signature -- conversion reaches the staging file, fails
  # .is_pdf_file()'s check, and write_dta() must abort without ever touching
  # `out`.
  testthat::local_mocked_bindings(
    .pdf_backends_available = function() "pandoc",
    .pdf_conversion_available = function() TRUE,
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) {
      writeBin(charToRaw("not a pdf"), pdf_file)
      invisible(pdf_file)
    }
  )

  cond <- tryCatch(
    write_dta(et_minimal_dta(), file = out, format = "pdf", overwrite = TRUE, quiet = TRUE),
    error = function(e) e
  )
  qa_check("the export raises rather than silently replacing the destination", inherits(cond, "condition"))
  qa_step(
    "the file already at the destination is byte-for-byte unchanged",
    original,
    readBin(out, what = "raw", n = file.info(out)$size)
  )
  qa_step(
    "no staging file is left behind next to the destination",
    "existing.pdf",
    basename(list.files(dir))
  )
})

# ---- rules are described in plain English (REQ-EXPORT-036, REQ-EXPORT-039) ----
#
# Every expected sentence below is written by hand from the rule wording
# specification: the leading comment block of R/rulePhrasing.R, restated in
# the notes of REQ-EXPORT-036. None is read back from an export. The same
# expected wording is then compared against every output that describes a
# rule, which is what REQ-EXPORT-039 asks: the same rule reads the same
# everywhere.

# Typographic quotes and the list bullet, built from their code points so the
# source stays ASCII.
et_lq <- intToUtf8(0x201c)
et_rq <- intToUtf8(0x201d)
et_bullet <- intToUtf8(0x2022)
et_q <- function(x) paste0(et_lq, x, et_rq)

# One rule of each type, each exercising the parameters the requirement names:
# inclusive bounds; a column combination; a When/Then pair with a text value
# and a numeric test; grouping, named conditions, both constraint kinds, an
# explicit and a defaulted constraint id, both scopes, an explicit and an
# absent message. Three carry an author's description (Purpose), one not.
et_rule_list <- function() {
  list(
    DTARuleColRange(
      id = "r_range", description = "Adults of working age only",
      columns = "AGE", min = 18, max = 70
    ),
    DTARuleColUnique(id = "r_unique", columns = c("USUBJID", "VISIT")),
    DTARuleColCondition(
      id = "r_cond", description = "The active arm is dosed",
      condition = list(ARM = list(equals = "ACTIVE")),
      then = list(DOSE = list(greater = 0))
    ),
    DTARuleGroupCondition(
      id = "r_group", description = "Failed results are documented",
      group_by = "USUBJID",
      conditions = list(
        failed = list(GFSTAT = list(equals = "FAILED")),
        not_done = list(GFSTAT = list(equals = "NOT DONE")),
        reported = list(GFORRES = list(empty = FALSE))
      ),
      constraints = list(
        list(
          type = "requires", id = "needs_not_done", `if` = "failed",
          then = "not_done", if_scope = "all", message = "A failure needs NOT DONE"
        ),
        list(type = "mutually_exclusive", left = "failed", right = "reported")
      )
    )
  )
}

et_rule_dataset <- function(rules = et_rule_list(), files = list(), id = "rules_demo") {
  cols <- c("USUBJID", "VISIT", "AGE", "ARM", "DOSE", "GFSTAT", "GFORRES")
  DTADataSetTabular(
    id = id,
    specs = DTAColumnSpecCollection(
      # Rules may only name declared columns.
      columns = stats::setNames(lapply(cols, function(nm) {
        if (nm %in% c("AGE", "DOSE")) {
          DTAColumnSpec(id = nm, type = "SAS Num", nullable = TRUE)
        } else {
          DTAColumnSpec(id = nm, type = "SAS Char", length = 8, nullable = TRUE)
        }
      }), cols),
      rules = rules
    ),
    files = files
  )
}

et_rule_dta <- function(ds = et_rule_dataset()) {
  DTA(datasets = list(ds), metadata = DTAMetaData(title = "Rule wording", version = "1.0"))
}

# One line of a rule description: a labelled line ("Label: text", or "Label:"
# introducing a list), an unlabelled sentence, or a list item.
et_line <- function(text, label = "") list(label = label, text = text, bullet = FALSE)
et_item <- function(text) list(label = "", text = text, bullet = TRUE)

# The expected description of each rule of et_rule_list(), from the
# specification, with the rule type label it gives each kind of rule.
# `complete = FALSE` marks a description of which only the
# leading lines are specified word for word: the specification states the
# grouped condition's missing-value notes as a principle, not as sentences.
et_rule_wording <- list(
  r_range = list(complete = TRUE, kind = "Range", lines = list(
    et_line("Adults of working age only", "Purpose"),
    et_line("Every value in column AGE must be a number from 18 to 70, inclusive."),
    et_line("Not checked.", "Missing values"),
    et_line("A value that is present but is not a number violates the rule.", "Non-numeric values")
  )),
  r_unique = list(complete = TRUE, kind = "Unique", lines = list(
    et_line("No two rows may have the same combination of values in columns USUBJID and VISIT."),
    et_line(
      paste0(
        "Two missing values are equal; so are two ",
        et_q("not a number"), " (NaN) values, which are not missing."
      ),
      "Missing values"
    )
  )),
  r_cond = list(complete = TRUE, kind = "Conditional (IF/THEN)", lines = list(
    et_line("The active arm is dosed", "Purpose"),
    et_line(paste0("When ARM is equal to ", et_q("ACTIVE"), ", DOSE must be greater than 0.")),
    et_line("", "Missing values"),
    et_item("If ARM is missing, the condition is not met and the row is not checked."),
    et_item("If DOSE is missing in a row that meets the condition, the rule is violated."),
    et_line(
      "A value in DOSE that is not a number, in a row that meets the condition, violates the rule.",
      "Non-numeric values"
    )
  )),
  r_group = list(complete = FALSE, kind = "Grouped condition", lines = list(
    et_line("Failed results are documented", "Purpose"),
    et_line(paste(
      "Rows are grouped by the value in column USUBJID; within each group,",
      "the requirements below must hold."
    )),
    et_line("", "Conditions"),
    et_item(paste0("failed: a row in which GFSTAT is equal to ", et_q("FAILED"))),
    et_item(paste0("not_done: a row in which GFSTAT is equal to ", et_q("NOT DONE"))),
    et_item("reported: a row in which GFORRES is not empty"),
    et_line("", "Requirements"),
    et_item(paste0(
      "needs_not_done: If every row of a group meets condition failed, then at ",
      "least one row of that group must meet condition not_done. A violation is ",
      "reported with the message ", et_q("A failure needs NOT DONE"), "."
    )),
    et_item(paste0(
      "constraint_2: Within a group, it must not happen that at least one row ",
      "meets condition failed and also at least one row meets condition ",
      "reported. A violation is reported with a message naming the group and its rows."
    ))
  ))
)

# The lines as text: "Label: text", "Label:", the sentence, or "<mark> item".
et_render_lines <- function(lines, mark) {
  vapply(lines, function(l) {
    if (l$bullet) {
      paste0(mark, " ", l$text)
    } else if (nzchar(l$label)) {
      if (nzchar(l$text)) paste0(l$label, ": ", l$text) else paste0(l$label, ":")
    } else {
      l$text
    }
  }, character(1))
}

# Keys of a rule definition as YAML would write them: none may appear in a
# human-readable rule description.
et_yaml_key_rx <- "(^|\\s)(type|columns|condition|then|group_by|conditions|constraints|if_scope|then_scope|min|max):"

# The body XML of a Word document, for checks on run formatting.
et_docx_body_xml <- function(path) {
  dir <- tempfile("docx-xml-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, files = "word/document.xml", exdir = dir)
  paste(readLines(file.path(dir, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Every table of a Word document whose header row starts with "Rule ID".
et_docx_rule_tables <- function(path) {
  Filter(function(tbl) nrow(tbl) > 0 && identical(tbl$col1[[1]], "Rule ID"), et_docx_tables(path))
}

# One Word rules table against the expected wording of every rule.
et_check_word_rule_table <- function(tbl, what, wording = et_rule_wording) {
  qa_step(
    paste0(what, ": the rules table has exactly the columns Rule ID and Description"),
    c("Rule ID", "Description"),
    unname(unlist(tbl[1, ]))
  )
  qa_step(
    paste0(what, ": the table lists the declared rules, in order"),
    names(wording), tbl$col1[-1]
  )
  for (id in names(wording)) {
    exp <- wording[[id]]
    want <- paste(et_render_lines(exp$lines, et_bullet), collapse = "\n")
    cell <- tbl$col2[match(id, tbl$col1)]
    got <- if (isTRUE(exp$complete) || is.na(cell)) cell else substr(cell, 1L, nchar(want))
    qa_step(paste0(what, ": rule ", id, " is described in the specified words"), want, got)
  }
  qa_check(
    paste0(what, ": no rule description holds a YAML key of the rule definition"),
    !any(grepl(et_yaml_key_rx, tbl$col2[-1]))
  )
}

et_check_word_no_monospace <- function(path, what) {
  xml <- et_docx_body_xml(path)
  qa_check(
    paste0(what, ": no text is set in a Consolas or Courier font"),
    !grepl("Consolas|Courier", xml)
  )
}

# Markdown reduced to its text: bold markers and backslash escapes removed,
# the three escaped HTML characters restored, blank lines dropped.
et_md_plain <- function(lines) {
  x <- gsub("**", "", lines, fixed = TRUE)
  x <- gsub("\\\\([[:punct:]])", "\\1", x)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x[nzchar(trimws(x))]
}

# REQ-EXPORT-042: each rule under a heading of `level` naming its id, the
# rule type in italics directly beneath it, then the description.
et_check_md_rules <- function(path, what, level, wording = et_rule_wording) {
  raw <- readLines(path, warn = FALSE, encoding = "UTF-8")
  qa_check(paste0(what, ": the file holds no fenced code block"), !any(grepl("^\\s*(```|~~~)", raw)))
  qa_check(paste0(what, ": the file holds no code span (no backtick at all)"), !any(grepl("`", raw, fixed = TRUE)))
  qa_check(paste0(what, ": no line holds a YAML key of a rule definition"), !any(grepl(et_yaml_key_rx, raw)))

  plain <- et_md_plain(raw)
  for (id in names(wording)) {
    exp <- wording[[id]]
    want <- et_render_lines(exp$lines, "-")
    start <- match(want[[1]], plain)
    qa_step(
      paste0(what, ": rule ", id, "'s description follows its heading and its type in italics"),
      c(paste(strrep("#", level), id), paste0("*", exp$kind, "*")),
      if (is.na(start) || start < 3L) NA_character_ else plain[start - 2:1]
    )
    got <- if (is.na(start)) NA_character_ else plain[start + seq_along(want) - 1L]
    qa_step(paste0(what, ": rule ", id, " is described in the specified words"), want, got)
    if (isTRUE(exp$complete) && !is.na(start)) {
      after <- plain[start + length(want)]
      qa_check(
        paste0(what, ": nothing further is added to rule ", id, "'s description"),
        is.na(after) || !grepl("^(- |Purpose:|Missing values:|Non-numeric values:|Problem:)", after)
      )
    }
  }
}

test_that("OQ-EXPORT-051 | each rule is described in plain English, naming its type and every parameter, in the Word and Markdown documents | REQ-EXPORT-036 REQ-EXPORT-039 REQ-EXPORT-042", {
  ds <- et_rule_dataset()
  dta <- et_rule_dta(ds)
  dir <- qa_tempdir()

  docx <- file.path(dir, "rules.docx")
  write_dta(dta, file = docx, quiet = TRUE)
  tabs <- et_docx_rule_tables(docx)
  qa_step("write_dta (docx) has one rules table for the one dataset", 1L, length(tabs))
  if (length(tabs) >= 1L) et_check_word_rule_table(tabs[[1]], "write_dta (docx)")
  et_check_word_no_monospace(docx, "write_dta (docx)")

  md <- file.path(dir, "rules.md")
  write_dta(dta, file = md, format = "md", quiet = TRUE)
  et_check_md_rules(md, "write_dta (md)", level = 5L)

  meta <- file.path(dir, "rules_meta.docx")
  write_dataset_metadata(ds, file = meta, quiet = TRUE)
  meta_tabs <- et_docx_rule_tables(meta)
  qa_step("write_dataset_metadata (docx) has one rules table", 1L, length(meta_tabs))
  if (length(meta_tabs) >= 1L) et_check_word_rule_table(meta_tabs[[1]], "write_dataset_metadata (docx)")
  et_check_word_no_monospace(meta, "write_dataset_metadata (docx)")

  meta_md <- file.path(dir, "rules_meta.md")
  write_dataset_metadata(ds, file = meta_md, format = "md", quiet = TRUE)
  et_check_md_rules(meta_md, "write_dataset_metadata (md)", level = 3L)
})

test_that("OQ-EXPORT-055 | equivalent spellings and integer or double bounds produce identical rule wording | REQ-EXPORT-036", {
  # Pairs the evaluator treats as the same test (R/evaluateRules.R,
  # dta_condition_operators()): min with max and range; equals and equal;
  # not_equals and not_equal; less_equal and max; greater_equal and min. Side
  # A stores bounds as integers, side B as doubles.
  side_a <- list(
    DTARuleColRange(id = "w_range", columns = "AGE", min = 18L, max = 70L),
    DTARuleColCondition(
      id = "w_band", condition = list(AGE = list(min = 18L, max = 70L)),
      then = list(ARM = list(equals = "ACTIVE"))
    ),
    DTARuleColCondition(
      id = "w_not", condition = list(ARM = list(not_equals = "ACTIVE")),
      then = list(DOSE = list(less_equal = 0L))
    ),
    DTARuleColCondition(
      id = "w_least", condition = list(ARM = list(equals = "ACTIVE")),
      then = list(DOSE = list(greater_equal = 5L))
    )
  )
  side_b <- list(
    DTARuleColRange(id = "w_range", columns = "AGE", min = 18, max = 70),
    DTARuleColCondition(
      id = "w_band", condition = list(AGE = list(range = c(18, 70))),
      then = list(ARM = list(equal = "ACTIVE"))
    ),
    DTARuleColCondition(
      id = "w_not", condition = list(ARM = list(not_equal = "ACTIVE")),
      then = list(DOSE = list(max = 0))
    ),
    DTARuleColCondition(
      id = "w_least", condition = list(ARM = list(equal = "ACTIVE")),
      then = list(DOSE = list(min = 5))
    )
  )
  # The statement of each, from the specification's range form and operator
  # table.
  statements <- c(
    w_range = "Every value in column AGE must be a number from 18 to 70, inclusive.",
    w_band = paste0("When AGE is from 18 to 70, inclusive, ARM must be equal to ", et_q("ACTIVE"), "."),
    w_not = paste0("When ARM is not equal to ", et_q("ACTIVE"), ", DOSE must be at most 0."),
    w_least = paste0("When ARM is equal to ", et_q("ACTIVE"), ", DOSE must be at least 5.")
  )

  dir <- qa_tempdir()
  cells <- lapply(list(a = side_a, b = side_b), function(rules) {
    out <- file.path(dir, paste0("equiv-", length(list.files(dir)), ".docx"))
    write_dataset_metadata(et_rule_dataset(rules), file = out, quiet = TRUE)
    tbl <- et_docx_rule_tables(out)[[1]]
    stats::setNames(tbl$col2[-1], tbl$col1[-1])
  })

  qa_step(
    "both spellings are described in exactly the same words, rule for rule",
    cells$a, cells$b
  )
  qa_step(
    "and each description opens with the statement the specification gives",
    statements,
    vapply(cells$a[names(statements)], function(x) strsplit(x, "\n", fixed = TRUE)[[1]][[1]], character(1))
  )
})

test_that("OQ-EXPORT-056 | the template rule blocks describe each rule in the same plain-English words as write_dta | REQ-EXPORT-036 REQ-EXPORT-039", {
  dir <- qa_tempdir()
  template <- et_write_template(c("{VALIDATION_RULES}", "{DATASETS}"), dir)
  out <- file.path(dir, "from-template.docx")
  export_with_template(et_rule_dta(), template = template, output = out, quiet = TRUE, fallback = FALSE)

  tabs <- et_docx_rule_tables(out)
  qa_step(
    "one rules table from {VALIDATION_RULES} and one from {DATASETS}",
    2L, length(tabs)
  )
  for (i in seq_along(tabs)) {
    et_check_word_rule_table(tabs[[i]], c("{VALIDATION_RULES}", "{DATASETS}")[[i]])
  }
  et_check_word_no_monospace(out, "the filled template")
})

# A delivery that violates r_range (AGE 10) and r_cond (ARM ACTIVE, DOSE 0) in
# its first row and nothing else: the second row is in range, not ACTIVE, and
# the two rows differ in USUBJID with no FAILED status.
et_rule_checked <- function(dir) {
  path <- file.path(dir, "rules.csv")
  utils::write.csv(
    data.frame(
      USUBJID = c("S1", "S2"), VISIT = c("V1", "V1"), AGE = c("10", "30"),
      ARM = c("ACTIVE", "PLACEBO"), DOSE = c("0", "5"),
      GFSTAT = c("DONE", "DONE"), GFORRES = c("x", "x"),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  ds <- et_rule_dataset(files = list(DTAFileCSV(filename = "rules.csv")))
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

test_that("OQ-EXPORT-057 | the HTML report explains a violated rule in the same plain-English words, without code | REQ-EXPORT-039", {
  dir <- qa_tempdir()
  ds <- et_rule_checked(dir)
  out <- file.path(dir, "report.html")
  write_validation_report(et_rule_dta(ds), out, quiet = TRUE)
  html <- xml2::read_html(out)

  boxes <- xml2::xml_find_all(
    html, "//div[contains(concat(' ', normalize-space(@class), ' '), ' inspect-explain ')]"
  )
  box_id <- vapply(boxes, function(b) {
    xml2::xml_text(xml2::xml_find_first(b, ".//dt[normalize-space(.)='Rule ID']/following-sibling::dd[1]"), trim = TRUE)
  }, character(1))
  qa_step("the violated rules r_range and r_cond are each explained", c("r_cond", "r_range"), sort(unique(box_id)))

  for (id in c("r_range", "r_cond")) {
    box <- boxes[[match(id, box_id)]]
    lines <- et_rule_wording[[id]]$lines
    texts <- vapply(lines, function(l) l$text, character(1))
    labels <- vapply(lines, function(l) l$label, character(1))
    qa_step(
      paste0("rule ", id, ": the explanation states every specified line, in order"),
      texts[nzchar(texts)],
      xml2::xml_text(xml2::xml_find_all(box, ".//dd/p | .//dd/ul/li"), trim = TRUE)
    )
    dts <- xml2::xml_text(xml2::xml_find_all(box, ".//dt"), trim = TRUE)
    qa_check(
      paste0("rule ", id, ": each labelled line keeps its label (Purpose, Missing values, ...)"),
      all(unique(labels[nzchar(labels)]) %in% dts)
    )
    qa_step(
      paste0("rule ", id, ": the explanation holds no code or pre element"),
      0L, length(xml2::xml_find_all(box, ".//code | .//pre"))
    )
    qa_check(
      paste0("rule ", id, ": the explanation holds no YAML key of the rule definition"),
      !grepl(et_yaml_key_rx, xml2::xml_text(box))
    )
  }
})

test_that("OQ-EXPORT-058 | explain_messages states a violated rule in the same plain-English words as the documents | REQ-EXPORT-039", {
  ds <- et_rule_checked(qa_tempdir())
  explained <- NULL
  printed <- withr::with_options(
    list(cli.width = 10000),
    testthat::capture_messages(explained <- explain_messages(ds, n = Inf))
  )
  printed <- paste(printed, collapse = "")
  msgs <- as.data.frame(messages(ds))
  rule_of <- msgs$rule_id[match(explained$id, msgs$id)]

  for (id in c("r_range", "r_cond")) {
    lines <- et_rule_wording[[id]]$lines
    statement <- lines[[2]]$text # each of the two opens with Purpose, then the statement
    qa_step(
      paste0("rule ", id, ": the expected value is the rule statement"),
      statement,
      unique(explained$should_be[rule_of %in% id])
    )
    others <- et_render_lines(lines[-2], et_bullet)
    qa_check(
      paste0("rule ", id, ": the purpose and the missing- and non-numeric-value lines are printed"),
      all(vapply(others, grepl, logical(1), x = printed, fixed = TRUE))
    )
  }
  qa_check(
    "no printed line holds a YAML key of a rule definition",
    !any(grepl(et_yaml_key_rx, strsplit(printed, "\n", fixed = TRUE)[[1]]))
  )
})

# ---- document name and organisation (REQ-EXPORT-037, REQ-EXPORT-038) ----------
#
# Expected values below are written from the requirement text: the document
# name is the first title, the transfer's own title follows it, and the
# organisation is "<name> · <unit>".

test_that("OQ-EXPORT-052 | the Markdown file leads with the document name, then the title and the organisation | REQ-EXPORT-037", {
  dir <- qa_tempdir()
  out <- file.path(dir, "named.md")
  write_dta(et_dta(),
    file = out, format = "md", quiet = TRUE,
    document_name = c("Data Transmission Specification", "Data Transfer Agreement"),
    organisation = "Example Institute", organisation_unit = "Data Management"
  )
  lines <- readLines(out, warn = FALSE)
  qa_step(
    "the first line is the joined document name as a top-level heading",
    "# Data Transmission Specification / Data Transfer Agreement", lines[[1]]
  )
  # The header block is everything before the "## Contents" list.
  head_block <- lines[seq_len(match("## Contents", lines) - 1L)]
  qa_step(
    "the transfer's own title follows directly, in bold",
    "**Clinical Data Specification**", head_block[nzchar(head_block)][[2]]
  )
  qa_check(
    "the organisation and unit are a line of the header block",
    paste0("Example Institute ", intToUtf8(0xb7), " Data Management") %in% head_block
  )

  plain <- file.path(dir, "plain.md")
  write_dta(et_dta(), file = plain, format = "md", quiet = TRUE)
  plain_lines <- readLines(plain, warn = FALSE)
  qa_step("without arguments the name is Data Transfer Agreement", "# Data Transfer Agreement", plain_lines[[1]])
  qa_check("and no organisation text appears", !any(grepl("Organisation", plain_lines, fixed = TRUE)))
})

test_that("OQ-EXPORT-053 | the Word cover and the later pages' running header carry the document name and organisation | REQ-EXPORT-037", {
  dir <- qa_tempdir()
  out <- file.path(dir, "named.docx")
  write_dta(et_dta(),
    file = out, quiet = TRUE,
    document_name = "Data Transmission Specification",
    organisation = "Example Institute", organisation_unit = "Data Management"
  )
  paras <- Filter(nzchar, qa_docx_summary(out)$text)
  qa_step(
    "the cover's first three paragraphs are document name, transfer title, organisation",
    c("Data Transmission Specification", "Clinical Data Specification", "Example Institute · Data Management"),
    paras[1:3]
  )

  tmp <- file.path(dir, "unzipped")
  utils::unzip(out, exdir = tmp)
  headers <- list.files(file.path(tmp, "word"), pattern = "^header.*[.]xml$", full.names = TRUE)
  texts <- vapply(headers, function(p) {
    paste(xml2::xml_text(xml2::xml_find_all(xml2::read_xml(p), ".//*[local-name()='t']")), collapse = "")
  }, character(1))
  qa_check("there are running headers", length(texts) > 0)
  qa_check(
    "every running header names the organisation and unit",
    all(grepl("Example Institute · Data Management", texts, fixed = TRUE))
  )

  ds_out <- file.path(dir, "dataset.docx")
  write_dataset_metadata(et_dta()[["clinical_data"]],
    file = ds_out, quiet = TRUE,
    document_name = "Data Transmission Specification", organisation = "Example Institute"
  )
  ds_paras <- Filter(nzchar, qa_docx_summary(ds_out)$text)
  qa_step("the dataset document leads with the document name", "Data Transmission Specification", ds_paras[[1]])
  qa_step("followed by the dataset specification title", TRUE, grepl("^Dataset Specification: ", ds_paras[[2]]))
  qa_step("and the organisation", "Example Institute", ds_paras[[3]])

  bad_file <- file.path(dir, "bad.md")
  bad <- tryCatch(
    write_dta(et_dta(), file = bad_file, format = "md", organisation = c("a", "b")),
    error = function(e) e
  )
  qa_check(
    "a non-scalar organisation is rejected, naming the argument",
    inherits(bad, "error") && grepl("organisation", conditionMessage(bad), fixed = TRUE)
  )
  qa_check("and nothing was written", !file.exists(bad_file))
})

test_that("OQ-EXPORT-054 | the document and organisation placeholders are filled from write_dta's arguments, defaulted otherwise, and overridable | REQ-EXPORT-038", {
  dir <- qa_tempdir()
  template <- et_write_template("{DOCUMENT_NAME}|{DOCUMENT_ABBREVIATION}|{ORGANISATION_NAME}|{ORGANISATION_UNIT}", dir)

  filled <- file.path(dir, "filled.docx")
  write_dta(et_dta(),
    file = filled, template = template, quiet = TRUE,
    document_name = "Data Transmission Specification", document_abbreviation = "DTS",
    organisation = "Example Institute", organisation_unit = "Data Management"
  )
  qa_step(
    "each placeholder takes its argument",
    "Data Transmission Specification|DTS|Example Institute|Data Management", qa_docx_text(filled)
  )

  defaulted <- file.path(dir, "defaulted.docx")
  write_dta(et_dta(), file = defaulted, template = template, quiet = TRUE)
  qa_step("without arguments the defaults apply", "Data Transfer Agreement|DTA||", qa_docx_text(defaulted))

  overridden <- file.path(dir, "overridden.docx")
  write_dta(et_dta(),
    file = overridden, template = template, quiet = TRUE, document_abbreviation = "DTS",
    template_variables = list(DOCUMENT_ABBREVIATION = "MINE")
  )
  qa_step("a template_variables entry wins", "Data Transfer Agreement|MINE||", qa_docx_text(overridden))

  described <- dta_template_placeholders()
  tokens <- c("{DOCUMENT_NAME}", "{DOCUMENT_ABBREVIATION}", "{ORGANISATION_NAME}", "{ORGANISATION_UNIT}")
  qa_check("all four are reported", all(tokens %in% names(described)))
  qa_step("as inline placeholders", rep("inline", 4), unname(attr(described, "kind")[tokens]))
})

# ---- deterministic documents, author text cannot change structure ------------
# (REQ-EXPORT-040), include_* in every format (REQ-EXPORT-041)

# The first locale besides "C" this machine can set LC_TIME to, one whose
# month names differ from the C locale's; NULL when none is installed.
et_other_time_locale <- function() {
  old <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old)), add = TRUE)
  for (loc in c("de_DE.UTF-8", "de_DE.utf8", "fr_FR.UTF-8", "fr_FR.utf8", "German_Germany.1252", "de_DE")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", loc)))) {
      return(loc)
    }
  }
  NULL
}

# Run `code` with LC_TIME set to `loc` (NULL leaves it as it is).
et_with_time_locale <- function(loc, code) {
  if (is.null(loc)) {
    return(force(code))
  }
  old <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old)), add = TRUE)
  suppressWarnings(Sys.setlocale("LC_TIME", loc))
  force(code)
}

# Any rendering of today's date or of a clock time.
et_time_stamp_rx <- function() {
  today <- Sys.Date()
  forms <- unique(c(
    format(today, "%Y-%m-%d"), format(today, "%d.%m.%Y"), format(today, "%m/%d/%Y"),
    withr::with_locale(c(LC_TIME = "C"), c(format(today, "%d %B %Y"), format(today, "%B %d, %Y")))
  ))
  paste0("(", paste(gsub(".", "[.]", forms, fixed = TRUE), collapse = "|"), ")|[0-9]{1,2}:[0-9]{2}|Generated")
}

test_that("OQ-EXPORT-059 | Markdown and dataset Word documents carry no time stamp and are identical when written again, under another LC_TIME | REQ-EXPORT-040", {
  dir <- qa_tempdir()
  dta <- et_dta()
  ds <- dta[["clinical_data"]]
  other <- et_other_time_locale()
  # Evidence only: which locale the repeat runs under is a fact of the machine.
  qa_check(
    "the LC_TIME locale of the repeat is recorded",
    TRUE,
    detail = if (is.null(other)) "no second locale installed: the repeat is written under the current LC_TIME" else other
  )
  stamp <- et_time_stamp_rx()

  writers <- list(
    write_dta = function(file) write_dta(dta, file = file, format = "md", quiet = TRUE),
    write_dataset_metadata = function(file) write_dataset_metadata(ds, file = file, format = "md", quiet = TRUE)
  )
  for (what in names(writers)) {
    first <- file.path(dir, paste0(what, "-1.md"))
    second <- file.path(dir, paste0(what, "-2.md"))
    et_with_time_locale("C", writers[[what]](first))
    et_with_time_locale(other, writers[[what]](second))
    qa_check(paste0(what, " (md): the repeat is byte-identical"), identical(et_raw(first), et_raw(second)))
    lines <- et_md_lines(first)
    qa_step(paste0(what, " (md): no line holds today's date, a clock time or 'Generated'"), character(0), grep(stamp, lines, value = TRUE))
  }

  first <- file.path(dir, "dataset-1.docx")
  second <- file.path(dir, "dataset-2.docx")
  et_with_time_locale("C", write_dataset_metadata(ds, file = first, quiet = TRUE))
  et_with_time_locale(other, write_dataset_metadata(ds, file = second, quiet = TRUE))
  parts_1 <- et_docx_parts(first)
  parts_2 <- et_docx_parts(second)
  qa_step("write_dataset_metadata (docx): the repeat writes the same parts", names(parts_1), names(parts_2))
  qa_check("write_dataset_metadata (docx): body, header and footer parts are identical", identical(parts_1, parts_2))
  texts <- vapply(parts_1, function(p) {
    paste(xml2::xml_text(xml2::xml_find_all(xml2::read_xml(p), ".//*[local-name()='t']")), collapse = " ")
  }, character(1))
  qa_step(
    "write_dataset_metadata (docx): no part holds today's date, a clock time or 'Generated'",
    character(0), names(texts)[grepl(stamp, texts)]
  )
})

# ---- REQ-EXPORT-040: author text cannot change the structure ------------------
#
# Two transfers that differ only in their author text: one written with the
# characters that mean something in Markdown or HTML, one with plain words in
# the same places, with the same number of lines and paragraphs. The
# requirement says the first must have exactly the structure of the second,
# and must show its text as written.

et_hostile_text <- list(
  title = "Q*1* _x_ `c` | <b>t</b> $5 # no heading",
  header = "# Header line\n- not a list\n> not a quote",
  contact = "Ann *Bold* | Pipe",
  role = "<i>role</i> & `lead`",
  org = "Org_*name* <x>",
  label = "Label `code` | $x$",
  col_description = "*desc* _x_ <br> # h",
  pattern = "^[A-Z]{2}_[0-9]+$|^`x`[*]$",
  value = "A*1|`b`<c>$",
  purpose = "Purpose `x` | *y* # z",
  rule_value = "R*|`$<q>",
  ds_name = "Set *x* | `y` #1",
  ds_description = "1. not a list\n\n---\n\n```\nnot code\n```",
  clause_title = "Clause # <x> *y*",
  clause_text = "text with | pipe and $math$ and `tick` and <tag>",
  method = "SFTP <host> | `port` *22*"
)

et_plain_text <- list(
  title = "Q1 x c t 5 no heading",
  header = "Header line\nnot a list\nnot a quote",
  contact = "Ann Bold Pipe",
  role = "role lead",
  org = "Org name x",
  label = "Label code x",
  col_description = "desc x br h",
  pattern = "patternone",
  value = "Avalue",
  purpose = "Purpose x y z",
  rule_value = "Rvalue",
  ds_name = "Set x y 1",
  ds_description = "one not a list\n\ndash\n\nfence\nnot code\nfence",
  clause_title = "Clause x y",
  clause_text = "text with pipe and math and tick and tag",
  method = "SFTP host port 22"
)

et_text_dta <- function(t) {
  DTA(
    metadata = DTAMetaData(
      title = t$title, version = "1.0", header = t$header,
      study = list(id = "S-1", protocol_id = "P-1"),
      supplier = list(
        affiliation = list(name = t$org),
        contacts = list(list(
          name = t$contact, role = t$role, email = "ann@example.org",
          department = t$org, signature = TRUE
        ))
      ),
      transmission = list(type = t$method),
      sections = list(clause = list(title = t$clause_title, text = t$clause_text))
    ),
    datasets = list(DTADataSetTabular(
      id = "text_set", name = t$ds_name, description = t$ds_description,
      files = list(DTAFileCSV(filename = "text_set.csv")),
      specs = DTAColumnSpecCollection(
        columns = list(
          CODE = DTAColumnSpec(
            id = "CODE", label = t$label, type = "SAS Char", nullable = TRUE,
            description = t$col_description, values = c(t$value, "B")
          ),
          REF = DTAColumnSpec(id = "REF", label = t$label, type = "SAS Char", nullable = TRUE, pattern = t$pattern),
          NOTE = DTAColumnSpec(id = "NOTE", type = "SAS Char", nullable = TRUE)
        ),
        # The tested column declares neither values nor a pattern, so any
        # rule value is consistent with the specification.
        rules = list(DTARuleColCondition(
          id = "text_rule", description = t$purpose,
          condition = list(NOTE = list(equals = t$rule_value)),
          then = list(REF = list(empty = FALSE))
        ))
      )
    ))
  )
}

# pandoc's reading of a GFM file, as a parsed JSON tree.
et_pandoc_json <- function(path) {
  qa_requires("rmarkdown", "jsonlite")
  if (!isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE))) {
    testthat::skip("pandoc absent: the Markdown structure cannot be read independently")
  }
  # Through a file: stdout = TRUE re-encodes pandoc's UTF-8 in the native
  # encoding on Windows and breaks the JSON.
  json_file <- tempfile(fileext = ".json")
  on.exit(unlink(json_file, force = TRUE), add = TRUE)
  system2(rmarkdown::pandoc_exec(), c("-f", "gfm", "-t", "json", "-o", shQuote(json_file), shQuote(path)))
  jsonlite::fromJSON(json_file, simplifyVector = FALSE)
}

et_is_node <- function(x) is.list(x) && !is.null(names(x)) && "t" %in% names(x)

# The text of a list of inline nodes: a soft break or a space is a blank, a
# hard break or a raw "<br>" a line break; formatting is dropped.
et_inline_text <- function(inlines) {
  paste(vapply(inlines, function(n) {
    if (!et_is_node(n)) {
      return(et_inline_text(n))
    }
    switch(n$t,
      Str = n$c,
      Space = " ",
      SoftBreak = " ",
      LineBreak = "\n",
      RawInline = if (identical(n$c[[2]], "<br>")) "\n" else n$c[[2]],
      Code = n$c[[2]],
      Math = n$c[[2]],
      Link = et_inline_text(n$c[[2]]),
      Image = et_inline_text(n$c[[2]]),
      Quoted = et_inline_text(n$c[[2]]),
      Span = et_inline_text(n$c[[2]]),
      if (is.list(n$c)) et_inline_text(n$c) else ""
    )
  }, character(1)), collapse = "")
}

# The structure of the tree: every node type in document order except the
# text tokens (Str, Space, SoftBreak), a heading with its level, a table with
# its column count and its rows' cell counts, a raw node with its content.
et_structure <- function(x) {
  out <- character(0)
  walk <- function(n) {
    if (et_is_node(n)) {
      out <<- c(out, switch(n$t,
        Str = NULL,
        Space = NULL,
        SoftBreak = NULL,
        Header = paste0("Header", n$c[[1]]),
        RawInline = paste0("RawInline:", n$c[[2]]),
        RawBlock = paste0("RawBlock:", n$c[[2]]),
        Table = {
          head_rows <- n$c[[4]][[2]]
          body_rows <- unlist(lapply(n$c[[5]], function(b) b[[4]]), recursive = FALSE)
          cells <- vapply(c(head_rows, body_rows), function(r) length(r[[2]]), integer(1))
          paste0("Table:", length(n$c[[3]]), " cols:", paste(cells, collapse = ","))
        },
        n$t
      ))
      if (is.list(n$c)) walk(n$c)
    } else if (is.list(n)) {
      for (m in n) walk(m)
    }
  }
  walk(x$blocks)
  out
}

# The text of every paragraph, plain block (table cell, list item) and heading.
et_block_texts <- function(x) {
  out <- character(0)
  walk <- function(n) {
    if (et_is_node(n)) {
      if (n$t %in% c("Para", "Plain")) out <<- c(out, et_inline_text(n$c))
      if (n$t == "Header") out <<- c(out, et_inline_text(n$c[[3]]))
      if (is.list(n$c)) walk(n$c)
    } else if (is.list(n)) {
      for (m in n) walk(m)
    }
  }
  walk(x$blocks)
  out
}

test_that("OQ-EXPORT-060 | author text with Markdown and HTML characters is shown as written and cannot change the document's structure | REQ-EXPORT-040", {
  dir <- qa_tempdir()
  hostile <- file.path(dir, "hostile.md")
  plain <- file.path(dir, "plain.md")
  write_dta(et_text_dta(et_hostile_text), file = hostile, format = "md", quiet = TRUE)
  write_dta(et_text_dta(et_plain_text), file = plain, format = "md", quiet = TRUE)
  h <- et_pandoc_json(hostile)
  p <- et_pandoc_json(plain)
  hs <- et_structure(h)

  qa_step(
    "the document has the structure of the one with plain author text, node for node",
    et_structure(p), hs
  )
  # Headings, from REQ-EXPORT-007's chapter list and this fixture: the
  # document name; Contents and the chapters 1 Approval and Signatures,
  # 2 Document Information (the fixture has a document header), 3 Study,
  # 4 Parties, 5 Process Information, 6 Agreement Sections, 7 Datasets;
  # 4.1 Supplier, 5.1 Transmission Details, 6.1 the clause, 7.1 the dataset;
  # 7.1.1 Files, 7.1.2 Column Specifications, 7.1.3 Validation Rules; and
  # at level 5 (REQ-EXPORT-042) the "File format" heading of the one CSV
  # file and the heading of the one rule.
  levels <- sub("^Header", "", hs[startsWith(hs, "Header")])
  qa_step(
    "headings per level",
    c(`1` = 1L, `2` = 8L, `3` = 4L, `4` = 3L, `5` = 2L, `6` = 0L),
    vapply(c(`1` = "1", `2` = "2", `3` = "3", `4` = "4", `5` = "5", `6` = "6"), function(l) sum(levels == l), integer(1))
  )
  for (kind in c("CodeBlock", "Code", "Math", "BlockQuote", "OrderedList", "RawBlock")) {
    qa_step(paste0("no ", kind, " anywhere"), 0L, sum(hs == kind))
  }
  # The only italics are the type of the one rule (REQ-EXPORT-042); the
  # author's asterisks and underscores make none.
  qa_step("exactly one Emph, the rule type", 1L, sum(hs == "Emph"))
  qa_step(
    "no raw HTML other than the line breaks inside table cells",
    character(0), unique(setdiff(hs[startsWith(hs, "RawInline:")], "RawInline:<br>"))
  )
  qa_step("one horizontal rule, the footer's", 1L, sum(hs == "HorizontalRule"))
  qa_check(
    "every table row has as many cells as its table has columns",
    all(vapply(hs[startsWith(hs, "Table:")], function(s) {
      n <- as.integer(sub("^Table:([0-9]+) .*$", "\\1", s))
      all(as.integer(strsplit(sub("^.* cols:", "", s), ",", fixed = TRUE)[[1]]) == n)
    }, logical(1)))
  )

  texts <- et_block_texts(h)
  single <- et_hostile_text[c(
    "title", "contact", "role", "org", "label", "col_description", "clause_title",
    "clause_text", "method", "purpose", "ds_name"
  )]
  for (nm in names(single)) {
    qa_check(
      paste0("the ", nm, " text is shown as written"),
      any(grepl(single[[nm]], texts, fixed = TRUE)), single[[nm]]
    )
  }
  qa_check("the pattern is shown as written", any(grepl(et_hostile_text$pattern, texts, fixed = TRUE)))
  qa_check("the permitted value is shown as written", any(grepl(et_hostile_text$value, texts, fixed = TRUE)))
  qa_check(
    "the rule's text value is shown as written, in typographic quotes",
    any(grepl(et_q(et_hostile_text$rule_value), texts, fixed = TRUE))
  )
  qa_check("the header's lines are shown as written", "# Header line\n- not a list\n> not a quote" %in% texts)
  qa_check(
    "the dataset description's paragraphs are shown as written",
    all(c("1. not a list", "---", "```\nnot code\n```") %in% texts)
  )

  # The links of the Contents list reach the heading whose text they show.
  heads <- list()
  for (b in h$blocks) {
    if (identical(b$t, "Header")) heads[[b$c[[2]][[1]]]] <- et_inline_text(b$c[[3]])
  }
  contents <- match("Contents", vapply(h$blocks, function(b) if (identical(b$t, "Header")) et_inline_text(b$c[[3]]) else "", ""))
  links <- list()
  walk <- function(n) {
    if (et_is_node(n) && identical(n$t, "Link")) {
      links[[length(links) + 1L]] <<- list(text = et_inline_text(n$c[[2]]), target = n$c[[3]][[1]])
    } else if (is.list(n)) {
      for (m in n) walk(m)
    }
  }
  walk(h$blocks[[contents + 1L]])
  qa_step("the Contents list has a link per chapter of the first two levels", 11L, length(links))
  qa_step(
    "each link's target is the id pandoc gives the heading it names",
    vapply(links, function(l) l$text, ""),
    vapply(links, function(l) heads[[sub("^#", "", l$target)]] %||% NA_character_, "")
  )
  qa_check(
    "the dataset heading keeps its characters in the Contents list",
    any(vapply(links, function(l) grepl(et_hostile_text$ds_name, l$text, fixed = TRUE), logical(1)))
  )
})

test_that("OQ-EXPORT-061 | include_file_specs, include_rules and include_signatures act alike in Word and Markdown | REQ-EXPORT-041 REQ-EXPORT-010", {
  ds <- et_dta()[["clinical_data"]]
  dir <- qa_tempdir()
  signers <- list(list(name = "Ann Example", role = "Reviewer", organization = "Example Org"))

  # Expected chapter lists, from REQ-EXPORT-010 and REQ-EXPORT-041.
  cases <- list(
    all = list(
      args = list(include_signatures = TRUE, include_file_specs = TRUE, include_rules = TRUE, signature_list = signers),
      chapters = c("Approval & Signatures", "Description", "Files", "Column Specifications", "Validation Rules")
    ),
    none = list(
      args = list(include_signatures = FALSE, include_file_specs = FALSE, include_rules = FALSE, signature_list = signers),
      chapters = c("Description", "Column Specifications")
    ),
    no_files = list(
      args = list(include_file_specs = FALSE, signature_list = signers),
      chapters = c("Approval & Signatures", "Description", "Column Specifications", "Validation Rules")
    ),
    no_rules = list(
      args = list(include_rules = FALSE),
      chapters = c("Description", "Files", "Column Specifications")
    )
  )
  for (case in names(cases)) {
    cfg <- cases[[case]]
    docx <- file.path(dir, paste0(case, ".docx"))
    md <- file.path(dir, paste0(case, ".md"))
    do.call(write_dataset_metadata, c(list(ds, file = docx, quiet = TRUE), cfg$args))
    do.call(write_dataset_metadata, c(list(ds, file = md, format = "md", quiet = TRUE), cfg$args))

    qa_step(paste0(case, ": Word chapters"), cfg$chapters, et_word_headings(docx)$title)
    heads <- et_md_headings(et_md_lines(md))
    heads <- heads[nzchar(heads$number), , drop = FALSE]
    # The Markdown approval heading spells the "&" out (REQ-EXPORT-041).
    md_chapters <- sub("Approval & Signatures", "Approval and Signatures", cfg$chapters, fixed = TRUE)
    qa_step(
      paste0(case, ": Markdown chapters, numbered without gaps"),
      paste(seq_along(md_chapters), md_chapters), paste(heads$number, heads$title)
    )
    if ("Approval & Signatures" %in% cfg$chapters) {
      sig <- et_md_table(et_md_lines(md), c("Organization", "Name", "Role", "Signature", "Date"))
      qa_step(paste0(case, ": the Markdown approval table names the signature_list entry"), c("Example Org", "Ann Example", "Reviewer"), unname(sig[1, 1:3]))
      word_sig <- et_docx_table(docx, "Organization")
      qa_step(paste0(case, ": and so does the Word one"), c("Example Org", "Ann Example", "Reviewer"), unname(unlist(word_sig[2, 1:3])))
    }
  }
})

test_that("OQ-EXPORT-062 | a template export to a path relative to the working directory writes the same document as the absolute form | REQ-EXPORT-016 REQ-EXPORT-014", {
  dir <- normalizePath(qa_tempdir())
  template <- et_write_template("Title: {DTA_TITLE}", dir)

  # The absolute form of the path works, and is the behaviour the relative
  # form should match.
  absolute <- file.path(dir, "absolute.docx")
  export_with_template(et_dta(), template = template, output = absolute, quiet = TRUE, fallback = FALSE)
  qa_step("with an absolute path the filled document is written", "Title: Clinical Data Specification", qa_docx_text(absolute))

  # The same call with a path relative to the working directory names the
  # same file as its absolute form.
  withr::with_dir(dir, export_with_template(
    et_dta(),
    template = template, output = "relative.docx", quiet = TRUE, fallback = FALSE
  ))
  qa_step(
    "with a path relative to the working directory the filled document is written there",
    "Title: Clinical Data Specification", qa_docx_text(file.path(dir, "relative.docx"))
  )
})

# ---- Markdown: links, locale-independent anchors, footer, rule headings -------
# (REQ-EXPORT-040, REQ-EXPORT-042). pandoc reads each file independently of
# the package; every expected value is written from the requirement text and
# the fixture.

# Every link of a pandoc tree: its shown text and its target.
et_pandoc_links <- function(tree) {
  links <- list()
  walk <- function(n) {
    if (et_is_node(n) && identical(n$t, "Link")) {
      links[[length(links) + 1L]] <<- c(text = et_inline_text(n$c[[2]]), target = n$c[[3]][[1]])
    } else if (is.list(n)) {
      for (m in n) walk(m)
    }
  }
  walk(tree$blocks)
  if (length(links) == 0L) {
    return(data.frame(text = character(0), target = character(0), stringsAsFactors = FALSE))
  }
  as.data.frame(do.call(rbind, links), stringsAsFactors = FALSE)
}

# Every heading of a pandoc tree: level, id and text, in document order.
et_pandoc_headings <- function(tree) {
  heads <- Filter(function(b) identical(b$t, "Header"), tree$blocks)
  data.frame(
    level = vapply(heads, function(b) as.integer(b$c[[1]]), integer(1)),
    id = vapply(heads, function(b) b$c[[2]][[1]], character(1)),
    text = vapply(heads, function(b) et_inline_text(b$c[[3]]), character(1)),
    stringsAsFactors = FALSE
  )
}

test_that("OQ-EXPORT-063 | web and e-mail addresses in author text are links showing the address as written | REQ-EXPORT-040", {
  # Addresses with characters Markdown would otherwise escape ("_", "&"),
  # each in a sentence whose punctuation follows it directly.
  url <- "https://www.genenames.org/tools?q=a_b&x=1"
  mail <- "data_desk@example.com"
  ftp <- "ftp://files.example.org/data_v1/"
  contact_mail <- "john_doe@example.org"
  col_description <- paste0("See the HGNC database (", url, "). Mail ", mail, ", then stop.")
  clause <- paste0("Files are also mirrored at ", ftp, ".")
  dta <- DTA(
    metadata = DTAMetaData(
      title = "Link test", version = "1.0",
      supplier = list(
        affiliation = list(name = "Org"),
        contacts = list(list(name = "John Doe", role = "Lead", email = contact_mail))
      ),
      sections = list(mirror = list(title = "Mirror", text = clause))
    ),
    datasets = list(DTADataSetTabular(
      id = "links",
      specs = DTAColumnSpecCollection(columns = list(
        GENE = DTAColumnSpec(id = "GENE", type = "SAS Char", nullable = TRUE, description = col_description)
      ))
    ))
  )
  dir <- qa_tempdir()
  out <- file.path(dir, "links.md")
  write_dta(dta, file = out, format = "md", quiet = TRUE)
  tree <- et_pandoc_json(out)

  links <- et_pandoc_links(tree)
  external <- links[!startsWith(links$target, "#"), , drop = FALSE]
  expected <- data.frame(
    text = c(contact_mail, ftp, url, mail),
    target = c(paste0("mailto:", contact_mail), ftp, url, paste0("mailto:", mail)),
    stringsAsFactors = FALSE
  )
  ord <- order(external$text)
  got <- external[ord, , drop = FALSE]
  rownames(got) <- NULL
  want <- expected[order(expected$text), , drop = FALSE]
  rownames(want) <- NULL
  qa_step("every address is a link to itself, showing the address as written, and nothing else is", want, got)

  texts <- et_block_texts(tree)
  qa_check(
    "the column description reads as written, the closing parenthesis and full stop outside the link",
    any(grepl(col_description, texts, fixed = TRUE)), col_description
  )
  qa_check("the clause reads as written, the full stop outside the link", clause %in% texts, clause)
  raw <- et_md_lines(out)
  for (address in c(url, mail, ftp, contact_mail)) {
    qa_check(
      paste0("the file holds '", address, "' unescaped"),
      any(grepl(address, raw, fixed = TRUE))
    )
  }
})

test_that("OQ-EXPORT-064 | the Contents anchors do not depend on the session's LC_CTYPE, also for a heading with letters outside ASCII | REQ-EXPORT-040 REQ-EXPORT-007", {
  # "Ubersicht Daten" with U+00DC (U with diaeresis) for the U: an upper-case
  # letter outside ASCII, which every reader lower-cases to U+00FC whatever
  # its own locale.
  name <- paste0(intToUtf8(0xDC), "bersicht Daten")
  dta <- DTA(
    metadata = DTAMetaData(title = "Locale test", version = "1.0", header = "Org"),
    datasets = list(DTADataSetTabular(
      id = "ue_set", name = name,
      specs = DTAColumnSpecCollection(columns = list(
        ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
      ))
    ))
  )
  available <- function(loc) {
    old <- Sys.getlocale("LC_CTYPE")
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
    nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))
  }
  utf8 <- Filter(available, c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8", "English_United States.utf8"))
  if (length(utf8) == 0L) {
    testthat::skip("no UTF-8 LC_CTYPE locale installed to compare with the C locale")
  }
  qa_check("the two LC_CTYPE locales compared are recorded", TRUE, detail = c("C", utf8[[1]]))

  dir <- qa_tempdir()
  in_c <- file.path(dir, "ctype-c.md")
  in_utf8 <- file.path(dir, "ctype-utf8.md")
  withr::with_locale(c(LC_CTYPE = "C"), write_dta(dta, file = in_c, format = "md", quiet = TRUE))
  withr::with_locale(c(LC_CTYPE = utf8[[1]]), write_dta(dta, file = in_utf8, format = "md", quiet = TRUE))
  qa_check("the two files are byte-identical", identical(et_raw(in_c), et_raw(in_utf8)))

  # Chapters: 1 Document Information (the document header), 2 Datasets,
  # 2.1 the dataset. GitHub's anchor: lower case, "(" and ")" dropped, each
  # space a "-".
  toc <- et_md_toc(et_md_lines(in_c))
  entry <- paste0("2.1 ", name, " (ue_set)")
  qa_step(
    "written under LC_CTYPE=C, the dataset's Contents link has the anchor GitHub computes",
    paste0("21-", intToUtf8(0xFC), "bersicht-daten-ue_set"),
    toc$target[match(entry, toc$text)]
  )

  tree <- et_pandoc_json(in_c)
  heads <- et_pandoc_headings(tree)
  links <- et_pandoc_links(tree)
  internal <- links[startsWith(links$target, "#"), , drop = FALSE]
  qa_step("the Contents list links the three chapters", c("1 Document Information", "2 Datasets", entry), internal$text)
  qa_step(
    "and every link reaches the heading it names, as pandoc reads the file",
    internal$text,
    heads$text[match(sub("^#", "", internal$target), heads$id)]
  )
})

test_that("OQ-EXPORT-065 | the Markdown footer names title, version and date, the template only when there is one, and never the package | REQ-EXPORT-040", {
  dir <- qa_tempdir()
  version <- as.character(utils::packageVersion("DTAtools"))
  dot <- paste0(" ", intToUtf8(0xB7), " ")
  footer <- function(lines) {
    rule <- max(which(lines == "---"))
    rest <- lines[seq.int(rule + 1L, length(lines))]
    rest[nzchar(rest)]
  }
  no_package <- function(lines) !any(grepl("DTAtools", lines, fixed = TRUE)) && !any(grepl(version, lines, fixed = TRUE))

  # From inst/extdata/clinical_dta.yaml: title, version 0.1, date 2024-06-19;
  # no template.
  plain <- file.path(dir, "plain.md")
  write_dta(et_dta(), file = plain, format = "md", quiet = TRUE)
  lines <- et_md_lines(plain)
  qa_step(
    "write_dta: after the closing rule, one line: title, version and date",
    paste0("Clinical Data Specification", dot, "Version 0.1", dot, "2024-06-19"),
    et_md_unescape(footer(lines))
  )
  qa_check("write_dta: the file names neither the package nor its version", no_package(lines))
  qa_check("write_dta: and no built-in layout", !any(grepl("Built-in layout", lines, fixed = TRUE)))

  # The same transfer created from a template: a second footer line naming it.
  tpl <- et_dta()
  meta <- tpl@metadata
  meta@template <- list(id = "example_tpl", version = "2.1")
  tpl@metadata <- meta
  with_tpl <- file.path(dir, "template.md")
  write_dta(tpl, file = with_tpl, format = "md", quiet = TRUE)
  tpl_footer <- et_md_unescape(footer(et_md_lines(with_tpl)))
  qa_step("write_dta with a template: two footer lines", 2L, length(tpl_footer))
  qa_step(
    "the first still title, version and date",
    paste0("Clinical Data Specification", dot, "Version 0.1", dot, "2024-06-19"), tpl_footer[[1]]
  )
  qa_check(
    "the second names the template and its version",
    length(tpl_footer) == 2L && grepl("example_tpl", tpl_footer[[2]], fixed = TRUE) &&
      grepl("2.1", tpl_footer[[2]], fixed = TRUE),
    detail = tpl_footer
  )
  qa_check("and the file still names neither the package nor its version", no_package(et_md_lines(with_tpl)))

  # The dataset document: one line, its title; the fixture's dataset records
  # no template.
  ds_md <- file.path(dir, "dataset.md")
  write_dataset_metadata(et_dta()[["clinical_data"]], file = ds_md, format = "md", quiet = TRUE)
  ds_lines <- et_md_lines(ds_md)
  qa_step(
    "write_dataset_metadata: after the closing rule, one line: the document's title",
    "Dataset Specification: Clinical data table (clinical_data)",
    et_md_unescape(footer(ds_lines))
  )
  qa_check("write_dataset_metadata: the file names neither the package nor its version", no_package(ds_lines))
})

test_that("OQ-EXPORT-066 | every rule has its own heading with an anchor, its type in italics beneath; the reading guide appears once | REQ-EXPORT-042", {
  csv <- function(name) list(DTAFileCSV(filename = name))
  spec <- function(rules) {
    DTAColumnSpecCollection(
      columns = list(
        X = DTAColumnSpec(id = "X", type = "SAS Char", length = 4, nullable = TRUE),
        N = DTAColumnSpec(id = "N", type = "SAS Num", nullable = TRUE)
      ),
      rules = rules
    )
  }
  # The first rule's id is the text of the second dataset's chapter heading
  # ("2.2 b_set" reads "22-b_set" as an anchor), and it comes first in the
  # document: GitHub gives the rule heading the anchor and the chapter
  # "22-b_set-1", which the Contents list must use.
  a_set <- DTADataSetTabular(
    id = "a_set", files = csv("a.csv"),
    specs = spec(list(
      DTARuleColUnique(id = "22-b_set", columns = "X"),
      DTARuleColRange(id = "r_range", columns = "N", min = 1, max = 2)
    ))
  )
  b_set <- DTADataSetTabular(
    id = "b_set", files = csv("b.csv"),
    specs = spec(list(DTARuleColUnique(id = "b_unique", columns = "X")))
  )
  dta <- DTA(
    metadata = DTAMetaData(title = "Rule headings", version = "1.0", header = "Org"),
    datasets = list(a_set = a_set, b_set = b_set)
  )
  dir <- qa_tempdir()
  out <- file.path(dir, "rules.md")
  write_dta(dta, file = out, format = "md", quiet = TRUE)
  lines <- et_md_lines(out)
  heads <- et_md_headings(lines)

  # Level 5: per dataset the File format heading in its Files chapter, then
  # its rules, one heading each.
  qa_step(
    "the level-5 headings: File format, the rules of a_set, File format, the rule of b_set",
    c("File format", "22-b_set", "r_range", "File format", "b_unique"),
    heads$title[heads$level == 5L]
  )
  numbered <- heads[nzchar(heads$number), , drop = FALSE]
  qa_step(
    "each one under the chapter it belongs to",
    c("2.1.1 Files", "2.1.3 Validation Rules", "2.1.3 Validation Rules", "2.2.1 Files", "2.2.3 Validation Rules"),
    vapply(which(heads$level == 5L), function(i) {
      above <- which(nzchar(heads$number) & seq_len(nrow(heads)) < i)
      last <- above[[length(above)]]
      paste(heads$number[[last]], heads$title[[last]])
    }, character(1))
  )
  qa_check("no numbered heading is deeper than level 4", all(numbered$level <= 4L))

  # Beneath each rule heading, the type in italics (the specification's
  # labels): Unique, Range, Unique.
  after <- vapply(c("##### 22-b\\_set", "##### r\\_range", "##### b\\_unique"), function(h) {
    rest <- lines[-seq_len(match(h, lines))]
    rest[nzchar(rest)][[1]]
  }, character(1))
  qa_step("each rule heading is followed by its type in italics", c("*Unique*", "*Range*", "*Unique*"), unname(after))

  # The reading guide: once, before the first rule heading, inside the first
  # Validation Rules chapter.
  guide <- which(lines == "**How to read the rules**")
  qa_step("the reading guide's title appears once", 1L, length(guide))
  qa_check(
    "between the first Validation Rules heading and the first rule heading",
    length(guide) == 1L && guide > match("#### 2.1.3 Validation Rules", lines) && guide < match("##### 22-b\\_set", lines)
  )
  qa_check("and is followed by its points as a bullet list", length(guide) == 1L && startsWith(lines[guide + 2L], "- "))

  # Anchors, as pandoc reads the file.
  tree <- et_pandoc_json(out)
  p_heads <- et_pandoc_headings(tree)
  qa_step(
    "each rule heading has its own anchor",
    c("22-b_set", "r_range", "b_unique"),
    p_heads$id[match(c("22-b_set", "r_range", "b_unique"), p_heads$text)]
  )
  qa_step("no two headings share an anchor", 0L, sum(duplicated(p_heads$id)))
  links <- et_pandoc_links(tree)
  internal <- links[startsWith(links$target, "#"), , drop = FALSE]
  qa_step(
    "the Contents link to 2.2 b_set is the anchor the chapter gets after the rule took 22-b_set",
    "#22-b_set-1", internal$target[match("2.2 b_set", internal$text)]
  )
  qa_step(
    "and every Contents link reaches the heading it names",
    internal$text, p_heads$text[match(sub("^#", "", internal$target), p_heads$id)]
  )

  # The dataset document: the same layout one chapter level higher, so rule
  # headings at level 3; the guide once.
  ds_out <- file.path(dir, "a_set.md")
  write_dataset_metadata(a_set, file = ds_out, format = "md", quiet = TRUE)
  ds_lines <- et_md_lines(ds_out)
  ds_heads <- et_md_headings(ds_lines)
  qa_step(
    "dataset document: level-3 headings File format and the two rules",
    c("File format", "22-b_set", "r_range"), ds_heads$title[ds_heads$level == 3L]
  )
  qa_step("dataset document: the reading guide appears once", 1L, sum(ds_lines == "**How to read the rules**"))

  # A document without rules has no reading guide.
  bare <- file.path(dir, "bare.md")
  write_dta(et_minimal_dta(), file = bare, format = "md", quiet = TRUE)
  qa_step("a document without rules has no reading guide", 0L, sum(grepl("How to read the rules", et_md_lines(bare), fixed = TRUE)))
})


# ---- Markdown escaping and missing values (REQ-EXPORT-043, REQ-EXPORT-044) ----
#
# Expected strings are built from the requirement text: a backslash before each
# listed character, entities for < and >, escaped list markers, non-breaking
# spaces for indentation. Where a piece of text sits in the document (a bold
# title line, a table cell, a paragraph) is REQ-EXPORT-040's business, so
# these tests look for the escaped text anywhere in the written lines.

et_md_written <- function(dta, name, ...) {
  out <- file.path(qa_tempdir(), name)
  write_dta(dta, file = out, format = "md", quiet = TRUE, overwrite = TRUE, ...)
  readLines(out, warn = FALSE, encoding = "UTF-8")
}

et_with_meta <- function(...) {
  dta <- et_dta()
  md <- metadata(dta)
  vals <- list(...)
  for (nm in names(vals)) S7::prop(md, nm) <- vals[[nm]]
  dta@metadata <- md
  dta
}

test_that("OQ-EXPORT-067 | every Markdown-significant character in a title is written escaped | REQ-EXPORT-043", {
  bs <- "\\"
  specials <- c("\\", "`", "*", "_", "{", "}", "[", "]", "#", "!", "~", "$", "|")
  for (ch in specials) {
    lines <- et_md_written(et_with_meta(title = paste0("a", ch, "b")), "esc.md")
    qa_check(
      paste0("'", ch, "' is preceded by a backslash"),
      any(grepl(paste0("a", bs, ch, "b"), lines, fixed = TRUE))
    )
  }
  lines <- et_md_written(et_with_meta(title = "x <z>"), "ent.md")
  qa_check("< and > become entities", any(grepl("x &lt;z&gt;", lines, fixed = TRUE)))
})

test_that("OQ-EXPORT-068 | list markers and indentation in narrative text cannot start Markdown structures | REQ-EXPORT-043", {
  bs <- "\\"
  nbsp <- " "
  lines <- et_md_written(
    et_with_meta(error_handling = "- a\n+ b\n1. c\n2) d\n    e\nplain - dash"),
    "lists.md"
  )
  start <- grep("Error Handling$", lines)
  start <- start[length(start)]
  # A line break inside a paragraph is written as a trailing backslash
  # (REQ-EXPORT-040's structure); it is not part of what is compared here.
  body <- sub("\\\\$", "", lines[(start + 2L):(start + 7L)])
  qa_step(
    "markers escaped, indentation became four non-breaking spaces, mid-line dash untouched",
    c(
      paste0(bs, "- a"), paste0(bs, "+ b"), paste0("1", bs, ". c"), paste0("2", bs, ") d"),
      paste0(strrep(nbsp, 4), "e"), "plain - dash"
    ),
    body
  )
  qa_check("no unescaped bullet line was written", !any(lines %in% c("- a", "+ b", "1. c")))
})

test_that("OQ-EXPORT-069 | text without special characters is written unchanged | REQ-EXPORT-043", {
  txt <- "Plain study title 2024, with words (and parentheses)."
  lines <- et_md_written(et_with_meta(title = txt, header = "Header text"), "plain.md")
  qa_check("title unchanged", any(grepl(txt, lines, fixed = TRUE)))
  qa_check("header unchanged", any(grepl("| Header text |", lines, fixed = TRUE)))
})

test_that("OQ-EXPORT-070 | transmission details and dataset name and description are written escaped | REQ-EXPORT-043", {
  bs <- "\\"
  lines <- et_md_written(et_with_meta(transmission = list(method = "sftp *fast* <b>")), "trans.md")
  qa_check(
    "transmission value escaped",
    any(grepl(paste0("sftp ", bs, "*fast", bs, "* &lt;b&gt;"), lines, fixed = TRUE))
  )

  ds <- et_dta()@datasets[[1]]
  S7::prop(ds, "name") <- "Set [1]"
  S7::prop(ds, "description") <- "Uses *stars* and _bars_"
  out <- file.path(qa_tempdir(), "ds.md")
  write_dataset_metadata(ds, file = out, format = "md", quiet = TRUE, overwrite = TRUE)
  dlines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  qa_check("name escaped", any(grepl(paste0("Set ", bs, "[1", bs, "]"), dlines, fixed = TRUE)))
  qa_check(
    "description escaped",
    paste0("Uses ", bs, "*stars", bs, "* and ", bs, "_bars", bs, "_") %in% dlines
  )
})

test_that("OQ-EXPORT-071 | a missing metadata field is shown as (not specified) in the Markdown file | REQ-EXPORT-044", {
  lines <- et_md_written(et_with_meta(transmission = list(method = NA_character_, frequency = "weekly")), "na.md")
  qa_check("the NA field reads (not specified)", any(grepl("^\\| Method \\| \\(not specified\\) \\|$", lines, ignore.case = TRUE)))
  qa_check("a present sibling is unaffected", any(grepl("^\\| Frequency \\| weekly \\|$", lines, ignore.case = TRUE)))
  qa_check("the text NA is never written as a value", !any(grepl("\\| NA \\|", lines)))
})

test_that("OQ-EXPORT-072 | export_with_template keeps an existing output unless overwrite = TRUE | REQ-EXPORT-045", {
  dir <- qa_tempdir()
  template <- et_write_template("Title: {DTA_TITLE}", dir)
  out <- file.path(dir, "existing.docx")
  writeLines("original", out)

  refused <- tryCatch(
    {
      export_with_template(et_dta(), template, out, quiet = TRUE)
      FALSE
    },
    error = function(e) grepl("already exists", conditionMessage(e), fixed = TRUE)
  )
  qa_check("a call without overwrite aborts naming the existing file", refused)
  qa_step("the existing file is untouched", "original", readLines(out, warn = FALSE))

  export_with_template(et_dta(), template, out, quiet = TRUE, overwrite = TRUE)
  qa_step("overwrite = TRUE replaces it", "Title: Clinical Data Specification", qa_docx_text(out))
})
