# Shared Word/DOCX helpers for the export test files
# (test-exportDocuments.R and test-exportTemplateDocx.R).
# testthat sources helper-*.R before any test file, so these are available
# everywhere in the suite.

# Read a .docx file's word/document.xml as a single string (test helper).
.read_docx_body_xml <- function(path) {
  ex <- file.path(tempdir(), paste0("docxbody_", as.integer(Sys.time()), "_", sample.int(1e6, 1)))
  dir.create(ex, showWarnings = FALSE)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, files = "word/document.xml", exdir = ex)
  paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE), collapse = "")
}

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

# Return the visible text of every paragraph of a Word document, in document
# order. Unlike .docx_text() this keeps paragraph boundaries, so a test can
# assert that a specific string is a whole paragraph rather than a substring
# of a longer one.
.docx_paragraphs <- function(path) {
  officer::docx_summary(officer::read_docx(path))$text
}
