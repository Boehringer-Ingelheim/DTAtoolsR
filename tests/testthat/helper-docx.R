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

# Build a Word template whose single paragraph is made of SEVERAL runs with
# different formatting. `parts` is a list of list(text = , bold = ) entries.
# .make_template() emits one plain run per paragraph, so it cannot express the
# case that matters here: a placeholder sitting in a formatted run next to
# differently-formatted text.
.make_template_rich <- function(parts) {
  path <- tempfile(fileext = ".docx")
  chunks <- lapply(parts, function(p) {
    officer::ftext(p$text, officer::fp_text(bold = isTRUE(p$bold)))
  })
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, do.call(officer::fpar, chunks))
  print(doc, target = path)
  path
}

# Text of every run that carries bold, in document order. Used to assert that
# per-run formatting survived a substitution. Tolerates either spelling of "not
# bold": an absent <w:b/> or an explicit <w:b w:val="false"/>.
.docx_bold_run_texts <- function(path) {
  doc <- xml2::read_xml(.read_docx_body_xml(path))
  runs <- xml2::xml_find_all(doc, ".//*[local-name()='r']")
  bold <- vapply(seq_along(runs), function(i) {
    b <- xml2::xml_find_all(
      runs[[i]],
      "./*[local-name()='rPr']/*[local-name()='b']"
    )
    if (length(b) == 0) {
      return(FALSE)
    }
    val <- xml2::xml_attr(b[[1]], "val")
    is.na(val) || !(val %in% c("false", "0"))
  }, logical(1))
  texts <- vapply(seq_along(runs), function(i) {
    paste0(
      xml2::xml_text(xml2::xml_find_all(runs[[i]], "./*[local-name()='t']")),
      collapse = ""
    )
  }, character(1))
  texts[bold]
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

# Page geometry of every <w:sectPr> that carries one, in document order, as a
# data frame of orient/width/height in twips. Word stores orientation as an
# attribute AND as the w/h pair, and only the pair decides how the page really
# renders, so a test that wants to prove a page is landscape must check both.
.docx_section_geometry <- function(path) {
  doc <- xml2::read_xml(.read_docx_body_xml(path))
  sects <- xml2::xml_find_all(doc, ".//*[local-name()='sectPr']")

  rows <- lapply(seq_along(sects), function(i) {
    pg <- xml2::xml_find_first(sects[[i]], "./*[local-name()='pgSz']")
    if (inherits(pg, "xml_missing")) {
      return(NULL)
    }
    type <- xml2::xml_find_first(sects[[i]], "./*[local-name()='type']")
    data.frame(
      orientation = .sectpr_orientation(sects[[i]]),
      width = as.numeric(xml2::xml_attr(pg, "w")),
      height = as.numeric(xml2::xml_attr(pg, "h")),
      type = if (inherits(type, "xml_missing")) NA_character_ else xml2::xml_attr(type, "val"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), rows))
}

# Orientation of a single <w:sectPr>; an absent orient attribute is portrait,
# which is also Word's default for a section that declares no page size at all.
.sectpr_orientation <- function(node) {
  if (is.null(node) || inherits(node, "xml_missing")) {
    return("portrait")
  }
  pg <- xml2::xml_find_first(node, "./*[local-name()='pgSz']")
  if (inherits(pg, "xml_missing")) {
    return("portrait")
  }
  orient <- xml2::xml_attr(pg, "orient")
  if (is.na(orient) || !nzchar(orient)) "portrait" else orient
}

# Map every body-level block (paragraph or table) to the orientation of the
# section it is rendered in. A bare list of section orientations is not enough
# to pin the layout: officer terminates the section for the content added
# BEFORE the call, so a document whose landscape/portrait blocks are inverted
# has exactly the same orientation sequence as a correct one. Tying the
# orientation to the visible text is what makes the distinction.
.docx_blocks_with_orientation <- function(path) {
  doc <- xml2::read_xml(.read_docx_body_xml(path))
  body <- xml2::xml_find_first(doc, ".//*[local-name()='body']")
  children <- xml2::xml_children(body)

  # Content after the last section break falls under the body-level sectPr.
  final_orient <- .sectpr_orientation(
    xml2::xml_find_first(body, "./*[local-name()='sectPr']")
  )

  texts <- character(0)
  orientations <- character(0)
  sections <- integer(0)
  buffer <- character(0)
  section_index <- 1L

  for (i in seq_along(children)) {
    child <- children[[i]]
    if (identical(xml2::xml_name(child), "sectPr")) next

    buffer <- c(buffer, paste0(
      xml2::xml_text(xml2::xml_find_all(child, ".//*[local-name()='t']")),
      collapse = ""
    ))

    # A section break lives in the pPr of the last paragraph of that section.
    sect <- xml2::xml_find_first(
      child,
      "./*[local-name()='pPr']/*[local-name()='sectPr']"
    )
    if (!inherits(sect, "xml_missing")) {
      texts <- c(texts, buffer)
      orientations <- c(orientations, rep(.sectpr_orientation(sect), length(buffer)))
      sections <- c(sections, rep(section_index, length(buffer)))
      buffer <- character(0)
      section_index <- section_index + 1L
    }
  }

  if (length(buffer) > 0) {
    texts <- c(texts, buffer)
    orientations <- c(orientations, rep(final_orient, length(buffer)))
    sections <- c(sections, rep(section_index, length(buffer)))
  }

  data.frame(
    text = texts,
    orientation = orientations,
    section = sections,
    stringsAsFactors = FALSE
  )
}

# Orientation(s) of the section(s) in which a given block text is rendered.
.docx_orientation_of <- function(path, text, fixed = TRUE) {
  blocks <- .docx_blocks_with_orientation(path)
  hit <- grepl(text, blocks$text, fixed = fixed)
  unique(blocks$orientation[hit])
}
