# Readers for the documents the package produces.
#
# A test that only checks a file exists has checked almost nothing: an empty
# file exists too. These helpers open what was written and return its content,
# so the qualification can assert what a document says rather than that a
# document happened.

# ---- Word documents ---------------------------------------------------------

qa_docx_summary <- function(path) {
  officer::docx_summary(officer::read_docx(path))
}

qa_docx_text <- function(path) {
  summary <- qa_docx_summary(path)
  paste(summary$text[!is.na(summary$text)], collapse = "\n")
}

# One data frame per table in the document, in document order, so a test can
# assert the contents of a specification table rather than its presence.
qa_docx_tables <- function(path) {
  summary <- qa_docx_summary(path)
  cells <- summary[summary$content_type %in% "table cell", , drop = FALSE]
  if (nrow(cells) == 0) {
    return(list())
  }
  # Grouped by `table_index`, which identifies the table, and NOT by
  # `doc_index`, which is a running counter over every content node in the
  # document and increments once per cell. Grouping by the counter shatters
  # each table into one-cell tables, so a test asserting a table's rows would
  # be comparing something that never existed.
  lapply(split(cells, cells$table_index), function(part) {
    wide <- stats::reshape(
      part[, c("row_id", "cell_id", "text")],
      idvar = "row_id", timevar = "cell_id", direction = "wide"
    )
    wide <- wide[order(wide$row_id), , drop = FALSE]
    out <- as.data.frame(wide[, setdiff(names(wide), "row_id"), drop = FALSE])
    names(out) <- sub("^text\\.", "col", names(out))
    rownames(out) <- NULL
    out
  })
}

# ---- the standalone HTML validation report ----------------------------------

# The report's own structure is part of its contract: the message table carries
# one `tr.msg-row` per reported message and the summary cards carry the counts,
# so a test can compare what the document tells a reader against what
# `messages()` and `results()` say.
qa_html_report <- function(path) {
  doc <- xml2::read_html(path)
  rows <- xml2::xml_find_all(doc, "//tr[contains(@class, 'msg-row')]")

  cells <- lapply(rows, function(row) {
    xml2::xml_text(xml2::xml_find_all(row, "./td"), trim = TRUE)
  })
  width <- if (length(cells)) max(lengths(cells)) else 0L
  messages <- if (length(cells)) {
    as.data.frame(
      do.call(rbind, lapply(cells, function(x) c(x, rep(NA_character_, width - length(x))))),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }

  # Matched on the whole class token, not on a substring: the cards sit inside
  # a container whose own class is "report-summary-cards", and a substring
  # match counts that container as a fourth card whose count duplicates the
  # first real one.
  cards <- xml2::xml_find_all(
    doc,
    "//*[contains(concat(' ', normalize-space(@class), ' '), ' summary-card ')]"
  )
  counts <- vapply(cards, function(card) {
    value <- xml2::xml_text(
      xml2::xml_find_first(card, ".//*[contains(@class, 'summary-count')]"),
      trim = TRUE
    )
    suppressWarnings(as.integer(value))
  }, integer(1))
  names(counts) <- vapply(cards, function(card) {
    sub("^summary-card\\s*", "", xml2::xml_attr(card, "class"))
  }, character(1))

  list(
    title = xml2::xml_text(xml2::xml_find_first(doc, "//title"), trim = TRUE),
    n_messages = length(rows),
    messages = messages,
    counts = counts,
    n_inspect_panels = length(
      xml2::xml_find_all(doc, "//*[starts-with(@id, 'inspect-panel-')]")
    ),
    # A report that reaches for the network is not standalone: on a validated
    # system it would render differently, or not at all, depending on what the
    # machine opening it can reach.
    external_refs = grep(
      "^(https?:)?//",
      c(
        xml2::xml_attr(xml2::xml_find_all(doc, "//script"), "src"),
        xml2::xml_attr(xml2::xml_find_all(doc, "//link"), "href"),
        xml2::xml_attr(xml2::xml_find_all(doc, "//img"), "src")
      ),
      value = TRUE
    )
  )
}

# ---- YAML -------------------------------------------------------------------

qa_read_yaml <- function(path) {
  yaml::read_yaml(path)
}

# The bundled example specifications, by the name they ship under.
qa_example_path <- function(...) {
  path <- system.file("extdata", ..., package = "DTAtools")
  if (!nzchar(path)) {
    testthat::skip(paste0("bundled example not found: ", paste(..., collapse = "/")))
  }
  path
}
