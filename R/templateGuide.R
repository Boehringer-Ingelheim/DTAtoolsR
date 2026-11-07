#' Write a readable guide to a template library
#'
#' @title Write a readable guide to a template library
#'
#' @description
#' Turns a folder of DTA templates into one Word (or PDF) handbook that anyone
#' can read, without starting the DTAtools app and without reading YAML. For
#' every template the guide says what it is for, what it will ask the person
#' creating a DTA, what it fills in automatically, what is fixed, and which
#' datasets and columns it produces. Dataset templates, term lists and party
#' profiles get their own chapters. Files that could not be read are listed in
#' a final chapter instead of stopping the guide.
#'
#' @param path The template library: its root folder (the one holding
#'   `templates/`, `parties/` and `vocabulary/`), or a single template file
#'   inside one of those folders (the guide then covers that template and
#'   everything it refers to).
#' @param file Where to write the guide. The extension (`.docx` or `.pdf`)
#'   decides the format unless `format` is given.
#' @param format `"docx"` or `"pdf"`. `NULL` (default) uses the extension of
#'   `file`, and Word when there is none. PDF needs a converter; see
#'   [dta_pdf_backend()].
#' @param templates Optional template ids. Only these templates are described,
#'   together with everything they refer to (the template they are based on,
#'   building blocks, dataset templates, term lists and party profiles).
#' @param overwrite Replace `file` if it already exists? Default `FALSE`.
#' @param quiet Suppress the confirmation message? Default `FALSE`.
#'
#' @return `file`, invisibly.
#'
#' @seealso [validate_template()], [write_dta()], [dta_pdf_backend()]
#'
#' @export
#' @examples
#' guide_file <- tempfile(fileext = ".docx")
#' write_template_guide(
#'   system.file("extdata", "library", package = "DTAtools"),
#'   guide_file,
#'   quiet = TRUE
#' )
#' file.exists(guide_file)
write_template_guide <- function(
  path,
  file,
  format = NULL,
  templates = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {
  if (is.null(format)) {
    format <- tolower(tools::file_ext(file))
    if (format == "") format <- "docx"
  }
  format <- tolower(format)
  if (!format %in% c("docx", "pdf")) {
    cli::cli_abort(
      "'format' must be 'docx' or 'pdf'.",
      class = "dta_template_guide_format"
    )
  }
  if (file.exists(file) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "File '{file}' already exists. Set overwrite = TRUE to replace.",
      class = "dta_template_guide_exists"
    )
  }

  guide <- .template_guide_collect(path, templates = templates)
  doc <- .template_guide_docx(guide)

  if (format == "docx") {
    .dta_write_staged(file, function(stage) print(doc, target = stage))
  } else {
    temp_docx <- tempfile(fileext = ".docx")
    on.exit(unlink(temp_docx, force = TRUE), add = TRUE)
    print(doc, target = temp_docx)
    .convert_docx_to_pdf(temp_docx, file)
  }

  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Template guide saved to {file}")
  }
  invisible(file)
}

# ---- rendering helpers ------------------------------------------------------

.tg_n <- function(x) if (is.data.frame(x)) nrow(x) else 0L
.tg_has <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

# A data.frame as a house-style table, or one italic sentence when it has no rows.
.tg_table <- function(doc, df, widths = NULL, small = FALSE, empty = NULL) {
  if (.tg_n(df) == 0L) {
    if (!is.null(empty)) {
      doc <- .add_body_par(doc, empty, italic = TRUE, color = THEME_COLORS$gray_mid)
    }
    return(doc)
  }
  df[] <- lapply(df, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- ""
    col
  })
  ft <- .style_table(flextable::flextable(df))
  if (small) {
    ft <- flextable::fontsize(ft, size = 8, part = "all")
  }
  w <- if (is.null(widths)) rep(1, ncol(df)) else widths
  ft <- flextable::width(ft, j = seq_len(ncol(df)), width = w)
  ft <- .fit_table_width(ft)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  .add_spacer(doc)
}

.tg_kv <- function(doc, df) {
  if (.tg_n(df) == 0L) {
    return(doc)
  }
  doc <- flextable::body_add_flextable(doc, .kv_table(df), align = "left")
  .add_spacer(doc)
}

.tg_note <- function(doc, text) {
  if (.tg_has(text)) {
    doc <- .add_body_par(doc, text, italic = TRUE, color = THEME_COLORS$gray_mid)
  }
  doc
}

.tg_technical <- function(doc, text) {
  if (.tg_has(text)) {
    doc <- .add_body_par(doc, text, size = 8, color = THEME_COLORS$gray_mid)
  }
  doc
}

.tg_glossary <- function() {
  data.frame(
    key = c(
      "DTA template", "Building block", "Based on", "Question",
      "Pre-filled answer", "Appears in the DTA as", "Must answer", "Fixed",
      "Dataset", "Column", "Can be empty", "Allowed values", "Term list",
      "Party profile"
    ),
    value = c(
      "A recipe for a Data Transfer Agreement (DTA). It asks a few questions and creates a complete DTA from the answers.",
      "A template that cannot be used on its own. Other templates are based on it, so shared settings are kept in one place.",
      "The template this one starts from. It keeps everything of the original except what this guide lists as changed.",
      "Something the person creating the DTA is asked, for example 'Who is the supplier?'.",
      "The answer already entered when the question appears. You can change it.",
      "Where the answer ends up in the finished DTA, for example 'Transmission \u203a Frequency'.",
      "A question that must be answered before the DTA can be created.",
      "Fixed: a value that templates based on this one are not allowed to change. You can still see it here.",
      "One data table that is transferred, usually one file.",
      "One field of a dataset, such as a subject identifier or a date.",
      "Yes means a value may be left blank; No means every row needs a value.",
      "The only values a column may contain. 'Any' means no restriction.",
      "A fixed list of codes and their meanings that questions or columns pick from.",
      "A saved organisation (address and contact people) that can be chosen as supplier or receiver."
    ),
    stringsAsFactors = FALSE
  )
}

.tg_ov_sentence <- function(overview) {
  n <- .tg_n(overview)
  if (n == 0L) {
    return("This library contains no templates.")
  }
  # Overview order (DTA templates first), not alphabetical; "DTA" keeps its case.
  kinds <- table(factor(overview$Kind, levels = unique(overview$Kind)))
  nouns <- sub("^dta ", "DTA ", tolower(names(kinds)))
  parts <- paste0(as.integer(kinds), " ", nouns, ifelse(kinds == 1L, "", "s"))
  paste0(
    "This library contains ", n, " ", if (n == 1L) "entry" else "entries", ": ",
    paste(parts, collapse = ", "), "."
  )
}

.tg_dataset_block <- function(doc, ds) {
  title <- ds[["title"]]
  if (.tg_has(ds[["variant"]])) title <- paste0(title, " \u2013 ", ds[["variant"]])
  doc <- .add_heading(doc, title, 4)
  doc <- .tg_kv(doc, ds[["about"]])
  if (.tg_has(ds[["source"]])) {
    doc <- .add_body_par(doc, paste0("Based on the dataset template '", ds[["source"]], "'."))
  }
  doc <- .tg_table(doc, ds[["columns"]],
    widths = c(1.2, 0.9, 0.6, 1.5, 2.1), small = TRUE,
    empty = if (.tg_has(ds[["same_columns"]])) NULL else "This dataset defines no columns."
  )
  doc <- .tg_note(doc, ds[["same_columns"]])
  if (length(ds[["removed_columns"]]) > 0) {
    doc <- .add_body_par(doc, paste0(
      "Columns removed: ", paste(ds[["removed_columns"]], collapse = ", "), "."
    ))
  }
  if (.tg_n(ds[["rules"]]) > 0L) {
    doc <- .add_body_par(doc, "Rules the data must follow", bold = TRUE)
  }
  doc <- .tg_table(doc, ds[["rules"]],
    widths = c(2, 4.3),
    empty = if (.tg_has(ds[["same_rules"]])) NULL else "This dataset has no rules."
  )
  .tg_note(doc, ds[["same_rules"]])
}

.tg_head <- function(doc, e) {
  doc <- .add_heading(doc, e[["title"]], 2)
  if (.tg_has(e[["summary"]])) doc <- .add_body_par(doc, e[["summary"]], bold = TRUE)
  if (.tg_has(e[["description"]])) doc <- .add_body_par(doc, e[["description"]])
  .tg_kv(doc, e[["at_a_glance"]])
}

.tg_creation <- function(doc, e) {
  doc <- .tg_head(doc, e)
  parent <- e[["parent"]]
  if (.tg_n(e[["changes"]]) > 0L) {
    doc <- .add_heading(doc, paste0("What it changes compared to ", parent), 3)
    doc <- .tg_table(doc, e[["changes"]], widths = c(2.2, 4.1))
    doc <- .add_body_par(doc, paste0("Everything else is the same as in ", parent, "."))
  }
  doc <- .add_heading(doc, "Questions you will be asked", 3)
  doc <- .tg_table(doc, e[["questions"]],
    widths = c(1.1, 0.8, 1.0, 0.9, 1.0, 1.0, 0.5), small = TRUE,
    empty = "This template asks no questions."
  )
  if (.tg_n(e[["variants"]]) > 0L) {
    doc <- .add_heading(doc, "Variants", 3)
    doc <- .tg_table(doc, e[["variants"]], widths = c(1.8, 1.5, 3))
  }
  doc <- .add_heading(doc, "Choices made when creating the DTA", 3)
  doc <- .tg_table(doc, e[["parties"]],
    widths = c(1.3, 1.6, 2.5, 0.9),
    empty = "No supplier or receiver is chosen from saved party profiles."
  )
  doc <- .tg_table(doc, e[["term_choices"]],
    widths = c(1, 1.3, 1, 1.1, 1, 0.9), small = TRUE,
    empty = "No term lists are chosen when creating the DTA."
  )
  doc <- .add_heading(doc, "Filled in automatically", 3)
  doc <- .tg_table(doc, e[["prefilled"]],
    widths = c(2, 4.3),
    empty = "This template fills in no fields automatically."
  )
  if (.tg_n(e[["locked"]]) > 0L) {
    doc <- .add_heading(doc, "Fixed and mandatory fields", 3)
    doc <- .tg_table(doc, e[["locked"]], widths = c(2.5, 3.8))
  }
  doc <- .add_heading(doc, "Datasets it creates", 3)
  if (length(e[["datasets"]]) == 0L) {
    doc <- .add_body_par(doc, "This template creates no datasets.",
      italic = TRUE, color = THEME_COLORS$gray_mid
    )
  }
  for (ds in e[["datasets"]]) doc <- .tg_dataset_block(doc, ds)
  doc <- .tg_note(doc, e[["build_note"]])
  .tg_technical(doc, e[["technical"]])
}

.tg_dataset_template <- function(doc, e) {
  doc <- .tg_head(doc, e)
  doc <- .add_heading(doc, "Settings a DTA template can fill in", 3)
  doc <- .tg_table(doc, e[["settings"]],
    widths = c(1.3, 1, 1.2, 1.5, 1.3), small = TRUE,
    empty = "This dataset template has no settings."
  )
  doc <- .add_heading(doc, "Columns", 3)
  doc <- .tg_table(doc, e[["columns"]],
    widths = c(1.2, 0.9, 0.6, 1.5, 2.1), small = TRUE, empty = "No columns."
  )
  doc <- .add_heading(doc, "Rules", 3)
  doc <- .tg_table(doc, e[["rules"]], widths = c(2, 4.3), empty = "No rules.")
  .tg_technical(doc, e[["technical"]])
}

.tg_vocabulary <- function(doc, e) {
  doc <- .tg_head(doc, e)
  doc <- .add_heading(doc, "Terms", 3)
  doc <- .tg_table(doc, e[["terms"]],
    widths = c(1.2, 2, 3.1), small = TRUE, empty = "This term list has no terms."
  )
  .tg_technical(doc, e[["technical"]])
}

.tg_party <- function(doc, e) {
  doc <- .tg_head(doc, e)
  doc <- .add_heading(doc, "Contacts", 3)
  doc <- .tg_table(doc, e[["contacts"]], empty = "This profile lists no contacts.")
  .tg_technical(doc, e[["technical"]])
}

.tg_chapter <- function(doc, title, entries, renderer, none) {
  doc <- .add_heading(doc, title, 1)
  if (length(entries) == 0L) {
    return(.add_body_par(doc, none, italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (e in entries) doc <- renderer(doc, e)
  doc
}

#' Render the template guide data (see `.template_guide_collect()`) as a Word
#' document. Pure rendering: no file I/O.
#' @param guide The list returned by `.template_guide_collect()`.
#' @return An `rdocx` object.
#' @keywords internal
.template_guide_docx <- function(guide) {
  doc <- .new_numbered_docx()
  lib <- guide[["library"]]

  # 1. Cover
  doc <- .add_body_par(doc, "Template guide",
    size = FONT_SIZES$title, bold = TRUE, color = THEME_COLORS$primary_dark
  )
  doc <- .add_body_par(doc, lib[["name"]],
    size = FONT_SIZES$subtitle, color = THEME_COLORS$gray_mid
  )
  doc <- .add_body_par(
    doc,
    paste0("Generated ", lib[["generated"]], " with DTAtools ", lib[["package_version"]]),
    color = THEME_COLORS$gray_mid
  )
  doc <- .add_spacer(doc)
  doc <- .add_body_par(doc, paste(
    "This guide describes every template in the library: what each one asks,",
    "what it fills in for you, and which datasets it creates. You do not need",
    "the DTAtools app or any programming knowledge to read it."
  ))
  doc <- .add_body_par(doc, paste(
    "Reading tip: most readers only need the chapter 'DTA templates'. The other",
    "chapters are reference material."
  ), italic = TRUE, color = THEME_COLORS$gray_mid)
  doc <- officer::body_add_break(doc)

  # 2. Table of contents
  doc <- .add_body_par(doc, "Contents",
    size = FONT_SIZES$heading1, bold = TRUE, color = THEME_COLORS$primary_dark
  )
  doc <- officer::body_add_toc(doc, level = 2)
  doc <- .add_body_par(
    doc, "If page numbers are missing, right-click the table and choose 'Update field' in Word.",
    size = 8, italic = TRUE, color = THEME_COLORS$gray_mid
  )
  doc <- officer::body_add_break(doc)

  # 3. How to read
  doc <- .add_heading(doc, "How to read this guide", 1)
  doc <- flextable::body_add_flextable(doc, .kv_table(.tg_glossary()), align = "left")
  doc <- .add_spacer(doc)

  # 4. Overview
  doc <- .add_heading(doc, "What is in this library", 1)
  doc <- .add_body_par(doc, .tg_ov_sentence(guide[["overview"]]))
  doc <- .tg_table(doc, guide[["overview"]], widths = c(1.8, 1, 1.3, 0.6, 1.6), small = TRUE)

  # 5-6. Chapters
  doc <- .tg_chapter(
    doc, "DTA templates", guide[["creation"]], .tg_creation,
    "This library has no DTA templates."
  )
  doc <- .tg_chapter(
    doc, "Dataset templates", guide[["datasets"]], .tg_dataset_template,
    "This library has no dataset templates."
  )
  doc <- .tg_chapter(
    doc, "Term lists", guide[["vocabularies"]], .tg_vocabulary,
    "This library has no term lists."
  )
  doc <- .tg_chapter(
    doc, "Party profiles", guide[["parties"]], .tg_party,
    "This library has no party profiles."
  )

  # 7. Problems
  if (.tg_n(guide[["problems"]]) > 0L) {
    doc <- .add_heading(doc, "Problems found", 1)
    doc <- .add_body_par(doc, "These files could not be fully described.")
    doc <- .tg_table(doc, guide[["problems"]], widths = c(2, 4.3))
  }
  doc
}
