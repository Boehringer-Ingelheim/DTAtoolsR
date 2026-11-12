#' @keywords internal
#' @noRd
NULL

#' Title page of the dataset document (write_dataset_metadata())
#'
#' Title, subtitle, organisation line and the fact table, closed as a section
#' of its own without running header or footer, as the cover of write_dta().
#' Nothing on it depends on the day the document is written.
#' @param facts A key/value data.frame (see .pairs_df()); zero rows for none.
#' @keywords internal
.add_title_section <- function(doc, title, subtitle = NULL, facts = NULL) {
  # A DTA may legitimately carry datasets without metadata (e.g. a standalone
  # dataset YAML), in which case `title` is NULL/empty. Normalize it to a single,
  # safe display string so we never pass a zero-length or NA value to officer.
  title_chr <- if (is.null(title) || length(title) == 0) "" else as.character(title)[1]
  if (is.na(title_chr)) title_chr <- ""
  display_title <- if (nzchar(title_chr)) title_chr else .export_document_name()

  for (i in seq_len(6)) {
    doc <- .add_spacer(doc)
  }

  # Title. (Previously this reached the cursor back to the title via
  # officer::cursor_reach(keyword = title) before inserting the blank line, but
  # that matches the title as a regex and aborted the whole export when the
  # title was empty/NULL or contained regex metacharacters. The cursor already
  # sits on the freshly added paragraph, so a direct insert is equivalent and
  # robust.)
  doc <- .add_body_par(
    doc, display_title,
    size = FONT_SIZES$title, bold = TRUE, color = THEME_COLORS$primary_dark
  )

  # Hairline rule under the title, drawn as a bottom-bordered empty paragraph.
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("", .house_fp_text(size = FONT_SIZES$body)),
      fp_p = officer::fp_par(
        border.bottom = officer::fp_border(color = THEME_COLORS$primary, width = 1.5),
        padding.bottom = 6
      )
    )
  )

  if (!is.null(subtitle) && nzchar(subtitle)) {
    doc <- .add_body_par(
      doc, subtitle,
      size = FONT_SIZES$subtitle, color = THEME_COLORS$gray_mid
    )
  }
  if (nzchar(.export_org_line())) {
    doc <- .add_body_par(
      doc, .export_org_line(),
      bold = TRUE, color = THEME_COLORS$gray_dark
    )
  }

  for (i in seq_len(3)) {
    doc <- .add_spacer(doc)
  }

  if (!is.null(facts) && nrow(facts) > 0) {
    ft <- .kv_table(facts, width = 4.6, label_width = 1.3)
    doc <- flextable::body_add_flextable(doc, ft, align = "left")
  }

  .end_section_orientation(doc, "portrait")
}

#' Running header and footer of the dataset document
#'
#' The frame of write_dta()'s (`.page_furniture()`), filled from the dataset:
#' header and footer name the document ("Dataset Specification: ..."), the
#' footer adds the template version and date and, on a small second line, the
#' template the dataset was taken from. No date of the run.
#' @param dataset A `DTADataSet`.
#' @param title The document title, e.g. "Dataset Specification: Demographics (demographics)".
#' @return A function of the text-column width returning list(header, footer),
#'   as `.section_props(furniture =)` takes it.
#' @keywords internal
.dataset_furniture <- function(dataset, title) {
  text <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "" else as.character(x[[1]])
  source <- text(dataset@template_source)
  version <- text(dataset@template_version)
  template <- if (nzchar(source)) list(id = source, version = version, date = dataset@template_date) else list()
  bits <- c(
    if (nzchar(version)) paste("Template version", version),
    .format_document_date(dataset@template_date)
  )
  bits <- bits[nzchar(bits)]
  function(width) {
    .running_furniture(
      header_title = title,
      footer_left = title,
      footer_right = paste(bits, collapse = paste0("  ", intToUtf8(0xB7), "  ")),
      footer_small = .template_provenance_text(template),
      width = width
    )
  }
}

#' Author text made safe for a Word part: a vertical tab or form feed reads as a
#' line break, and every other C0 control character (all but tab, LF, CR) is
#' dropped, because XML 1.0 forbids them and libxml aborts the whole export
#' on one. Mirrors `.tv_split_word_chunks()`, which does the same for templates.
#' @keywords internal
.xml_safe_text <- function(text) {
  text <- gsub("[\v\f]", "\n", text, perl = TRUE)
  # U+FFFE and U+FFFF are not XML characters either.
  gsub("[\x01-\x08\x0E-\x1F￾￿]", "", text, perl = TRUE)
}

#' `.xml_safe_text()` applied to every piece of text inside an object about to
#' be written to Word: a DTA, a dataset, a list of template variables. Author
#' text reaches a document through dozens of sinks (headings, every table, the
#' contact blocks, template placeholders), so it is made safe once, where an
#' export starts, instead of in each of them. Bound data (`tables`) and
#' validation state are never written to a document and are left alone.
#' @keywords internal
.xml_safe_deep <- function(x, depth = 0L) {
  if (depth > 50L) {
    return(x)
  }
  if (is.character(x)) {
    if (!isTRUE(any(grepl("[\x01-\x08\x0B\x0C\x0E-\x1F￾￿]", x, perl = TRUE)))) {
      return(x)
    }
    x[] <- .xml_safe_text(x) # x[] keeps names and other attributes
    return(x)
  }
  if (S7::S7_inherits(x)) {
    skip <- c("tables", "validation_store", "validation_index", "file_paths", "file_handlers")
    for (p in setdiff(S7::prop_names(x), skip)) {
      v <- tryCatch(S7::prop(x, p), error = function(e) NULL)
      if (is.null(v)) next
      nv <- .xml_safe_deep(v, depth + 1L)
      if (!identical(v, nv)) {
        # check = FALSE: only text changes, and a read-only property is skipped.
        x <- tryCatch(
          {
            S7::prop(x, p, check = FALSE) <- nv
            x
          },
          error = function(e) x
        )
      }
    }
    return(x)
  }
  if (is.data.frame(x) || (is.list(x) && !is.object(x))) {
    x[] <- lapply(x, .xml_safe_deep, depth = depth + 1L)
  }
  x
}

#' Author text as paragraphs: a blank line separates two paragraphs, a single
#' line break inside a paragraph stays a line break.
#' @keywords internal
.add_text_paragraphs <- function(doc, text) {
  text <- .xml_safe_text(text)
  text <- gsub("\r\n|\r", "\n", text)
  paragraphs <- strsplit(paste0(text, "\n\n"), "\n\n", fixed = TRUE)[[1]]
  for (paragraph in paragraphs) {
    lines <- strsplit(paste0(paragraph, "\n"), "\n", fixed = TRUE)[[1]]
    chunks <- list()
    for (i in seq_along(lines)) {
      if (i > 1L) chunks <- c(chunks, list(officer::run_linebreak()))
      chunks <- c(chunks, list(officer::ftext(lines[[i]], .house_fp_text())))
    }
    doc <- officer::body_add_fpar(
      doc,
      do.call(officer::fpar, c(chunks, list(fp_p = officer::fp_par(padding.bottom = 2))))
    )
  }
  doc
}

#' The display title of a DTA: its title, or the document name ("Data Transfer
#' Agreement" unless the caller supplied one) when it has none (a DTA may carry
#' datasets without metadata).
#' @keywords internal
.dta_display_title <- function(title, fallback = .export_document_name()) {
  title_chr <- if (is.null(title) || length(title) == 0) "" else as.character(title)[1]
  if (is.na(title_chr) || !nzchar(trimws(title_chr))) fallback else title_chr
}

# Document naming and organisation for one export. The writers set these once
# (see .set_export_ctx) and the builders read them, so the values reach the
# cover, the running header and the footer without being threaded through every
# builder signature. ponytail: process-global state, fine because R is
# single-threaded and the context is cleared when the writer returns.
# Known limitation: writers do not nest. An inner writer overwrites the outer
# context and its on.exit clears it, so the outer export would finish with
# defaults. The only nested path today is export_with_template(fallback = TRUE).
.export_ctx <- new.env(parent = emptyenv())

.export_document_name <- function() .export_ctx$document_name %||% "Data Transfer Agreement"

# "Organisation (middle dot) Unit", either part optional; "" when neither is given.
.export_org_line <- function(organisation = .export_ctx$organisation,
                             unit = .export_ctx$organisation_unit) {
  parts <- trimws(c(organisation, unit))
  paste(parts[nzchar(parts)], collapse = paste0(" ", intToUtf8(0xB7), " "))
}

# Validate one optional export text argument: NULL, or a single non-NA string
# (a character vector when `several`, joined with " / "). Blank means NULL.
.check_export_text <- function(x, arg, several = FALSE, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  ok <- is.character(x) && length(x) >= 1 && !anyNA(x) && (several || length(x) == 1)
  if (!ok) {
    cli::cli_abort(
      "{.arg {arg}} must be {if (several) 'a character vector' else 'a single string'} or {.code NULL}.",
      call = call
    )
  }
  x <- trimws(gsub("\\s+", " ", x)) # one line: a newline would start a Markdown block
  x <- x[nzchar(x)]
  if (length(x) == 0) NULL else paste(x, collapse = " / ")
}

# Validate the three arguments and install them for the calling writer; they
# are cleared again when that writer returns.
.set_export_ctx <- function(document_name = NULL, organisation = NULL, organisation_unit = NULL,
                            document_abbreviation = NULL, frame = parent.frame()) {
  document_name <- .check_export_text(document_name, "document_name", several = TRUE, call = frame)
  organisation <- .check_export_text(organisation, "organisation", call = frame)
  organisation_unit <- .check_export_text(organisation_unit, "organisation_unit", call = frame)
  document_abbreviation <- .check_export_text(document_abbreviation, "document_abbreviation", call = frame)
  .export_ctx$document_name <- document_name
  .export_ctx$document_abbreviation <- document_abbreviation
  .export_ctx$organisation <- organisation
  .export_ctx$organisation_unit <- organisation_unit
  do.call(
    base::on.exit,
    list(quote(rm(list = ls(.export_ctx), envir = .export_ctx)), TRUE),
    envir = frame
  )
  invisible(NULL)
}

#' Date of the current document version: `metadata@date`, else the date of the
#' version-history entry for the current version, else of the last entry.
#' @keywords internal
.current_version_date <- function(meta) {
  if (!is.null(meta@date) && length(meta@date) == 1 && !is.na(meta@date)) {
    return(meta@date)
  }
  vh <- meta@version_history
  if (length(vh) == 0) {
    return(NULL)
  }
  hit <- Filter(function(r) identical(as.character(r$version %||% ""), as.character(meta@version %||% "")), vh)
  rec <- if (length(hit) > 0) hit[[length(hit)]] else vh[[length(vh)]]
  rec$date
}

#' One human-readable line describing the template a DTA was generated from,
#' e.g. "Template: Biomarker GF DTS (biomarker_gf) - version 1.0 - 2026-11-05".
#' A DTA without provenance names the built-in layout and package version.
#' @keywords internal
.template_provenance_text <- function(template) {
  if (length(template) == 0 || !nzchar(template$id %||% "")) {
    return(paste0("Built-in layout ", intToUtf8(0xB7), " DTAtools ", utils::packageVersion("DTAtools")))
  }
  name <- template$label %||% template$id
  if (!identical(name, template$id)) name <- paste0(name, " (", template$id, ")")
  parts <- c(
    paste("Template:", name),
    if (nzchar(template$version %||% "")) paste("version", template$version) else "",
    .format_document_date(template$date)
  )
  paste(parts[nzchar(parts)], collapse = paste0(" ", intToUtf8(0xB7), " "))
}

#' Running header and footer of the built-in DTA layout
#'
#' Header: DTA title on the left; study and "Page X of Y" right-aligned on the
#' text-column edge. Footer: document name on the left, version and version
#' date on the right, and the template the document was generated from on a
#' small second line. The page numbers are real Word PAGE / NUMPAGES fields.
#' @param meta A `DTAMetaData`.
#' @param width Text-column width in inches (where the right tab stop sits).
#' @return A list with `header` and `footer` officer block lists.
#' @keywords internal
.page_furniture <- function(meta, width = TEXT_WIDTH$portrait) {
  title <- .dta_display_title(meta@title)
  study <- meta@study$id %||% ""
  version_bits <- c(
    if (!is.null(meta@version) && nzchar(meta@version)) paste("Version", meta@version),
    .format_document_date(.current_version_date(meta))
  )
  version_bits <- version_bits[nzchar(version_bits)]
  .running_furniture(
    header_title = title,
    header_right_prefix = if (nzchar(study)) paste0("Study ", study, "    |    ") else "",
    footer_left = title,
    footer_right = paste(version_bits, collapse = paste0("  ", intToUtf8(0xB7), "  ")),
    footer_small = .template_provenance_text(meta@template),
    width = width
  )
}

#' Generic running header and footer
#'
#' Header: optional organisation line (small), then `header_title` on the left
#' and `header_right_prefix` plus "Page X of Y" (real PAGE / NUMPAGES fields)
#' right-aligned at `width`. Footer: `footer_left` and `footer_right` on one
#' line, an optional small second line. `.page_furniture()` and the template
#' guide are both thin callers.
#' @param org_line Organisation line; `""` or `NULL` means none.
#' @return A list with `header` and `footer` officer block lists.
#' @keywords internal
.running_furniture <- function(header_title, header_right_prefix = "", footer_left,
                               footer_right, footer_small = NULL,
                               org_line = .export_org_line(), width = TEXT_WIDTH$portrait) {
  ink <- .house_fp_text(size = FONT_SIZES$footer, bold = TRUE, color = THEME_COLORS$primary_dark)
  muted <- .house_fp_text(size = FONT_SIZES$footer, color = THEME_COLORS$gray_mid)
  tiny <- .house_fp_text(size = FONT_SIZES$footer - 1, color = THEME_COLORS$gray_mid)
  tabs <- officer::fp_tabs(officer::fp_tab(pos = width, style = "right"))
  hairline <- officer::fp_border(color = THEME_COLORS$gray_border, width = 0.75)

  right <- list(
    officer::ftext("Page ", muted),
    officer::run_word_field("PAGE", prop = muted),
    officer::ftext(" of ", muted),
    officer::run_word_field("NUMPAGES", prop = muted)
  )
  if (nzchar(header_right_prefix %||% "")) {
    right <- c(list(officer::ftext(header_right_prefix, muted)), right)
  }
  header_blocks <- list(do.call(officer::fpar, c(
    list(officer::ftext(header_title, ink), officer::run_tab()),
    right,
    list(fp_p = officer::fp_par(tabs = tabs, border.bottom = hairline, padding.bottom = 4))
  )))
  if (nzchar(org_line %||% "")) {
    header_blocks <- c(list(officer::fpar(officer::ftext(org_line, tiny))), header_blocks)
  }
  header <- do.call(officer::block_list, header_blocks)

  footer_blocks <- list(officer::fpar(
    officer::ftext(footer_left, ink),
    officer::run_tab(),
    officer::ftext(footer_right, muted),
    fp_p = officer::fp_par(tabs = tabs, border.top = hairline, padding.top = 4)
  ))
  if (nzchar(footer_small %||% "")) {
    footer_blocks <- c(footer_blocks, list(officer::fpar(
      officer::ftext(footer_small, tiny),
      fp_p = officer::fp_par(padding.top = 1)
    )))
  }
  list(header = header, footer = do.call(officer::block_list, footer_blocks))
}

#' Cover page of the built-in DTA layout, closed as its own section without
#' running header or footer.
#' @keywords internal
.add_cover_page <- function(doc, meta) {
  # The document name is the main title; the DTA's own title sits beneath it.
  title <- .export_document_name()
  dta_title <- .dta_display_title(meta@title, fallback = "")
  for (i in seq_len(6)) {
    doc <- .add_spacer(doc)
  }
  doc <- .add_body_par(doc, title, size = FONT_SIZES$title, bold = TRUE, color = THEME_COLORS$primary_dark)
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("", .house_fp_text(size = FONT_SIZES$body)),
      fp_p = officer::fp_par(
        border.bottom = officer::fp_border(color = THEME_COLORS$primary, width = 2),
        padding.bottom = 6
      )
    )
  )
  if (nzchar(dta_title)) {
    doc <- .add_body_par(doc, dta_title, size = FONT_SIZES$subtitle, color = THEME_COLORS$gray_mid)
  }
  if (nzchar(.export_org_line())) {
    doc <- .add_body_par(doc, .export_org_line(), size = FONT_SIZES$body, bold = TRUE, color = THEME_COLORS$gray_dark)
  }
  if (!is.null(meta@header) && nzchar(meta@header)) {
    doc <- .add_body_par(doc, meta@header, size = FONT_SIZES$body, bold = TRUE, color = THEME_COLORS$gray_dark)
  }
  for (i in seq_len(3)) {
    doc <- .add_spacer(doc)
  }

  facts <- .cover_facts(meta)
  if (nrow(facts) > 0) {
    ft <- .kv_table(facts, width = 4.6, label_width = 1.3)
    doc <- flextable::body_add_flextable(doc, ft, align = "left")
  }

  .end_section_orientation(doc, "portrait")
}

#' Add a heading to the document
#' @keywords internal
.add_heading <- function(doc, text, level = 1) {
  style <- paste0("heading ", level)
  officer::body_add_par(doc, text, style = style)
}

# Location of the bundled numbered template; "" when it is not installed. A
# function of its own so a test can simulate the missing asset without
# mocking base::system.file, which officer calls internally as well.
.numbered_docx_path <- function() {
  system.file(
    "extdata", "word-templates", "dta_numbered_template.docx",
    package = "DTAtools"
  )
}

#' Open the bundled numbered reference template so document headings 1-4
#' auto-number as 1 / 1.1 / 1.1.1 / 1.1.1.1 (true Word list fields that renumber
#' when the document is edited). There is no fallback: officer's stock template
#' has no 'heading 4' style, so an export would fail later, on its first level-4
#' heading, with an error about a style. A missing bundled asset is a broken
#' installation and is reported as one.
#' @keywords internal
.new_numbered_docx <- function() {
  path <- .numbered_docx_path()
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(c(
      "The bundled numbered Word template is missing.",
      "i" = "Reinstall {.pkg DTAtools}."
    ))
  }
  officer::read_docx(path = path)
}

#' Append the machine-readable YAML specification as a final, very small-font
#' monospace section. Leading spaces are converted to non-breaking spaces so the
#' YAML indentation survives Word's whitespace collapsing.
#' @keywords internal
.add_embedded_yaml_section <- function(doc, yaml_text, level = 2) {
  doc <- .add_heading(doc, "Embedded Specification (YAML)", level = level)
  doc <- .add_body_par(
    doc,
    "The machine-readable YAML specification is embedded below for reference."
  )
  doc <- .add_spacer(doc)

  fp <- .house_fp_text(size = FONT_SIZES$code, color = THEME_COLORS$gray_dark, font = FONTS$monospace)
  lines <- strsplit(gsub("\r\n", "\n", yaml_text, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) lines <- ""
  for (ln in lines) {
    # Preserve indentation: replace each leading space with a non-breaking space.
    lead <- attr(regexpr("^ *", ln), "match.length")
    if (is.null(lead) || is.na(lead)) lead <- 0L
    disp <- paste0(strrep("\u00a0", lead), substring(ln, lead + 1))
    if (!nzchar(disp)) disp <- "\u00a0"
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(disp, fp)))
  }
  doc <- .add_spacer(doc)
  doc
}

#' Add study identity, sharing the built-in and template-block layout
#' @keywords internal
.add_study_section <- function(doc, study, heading = .add_heading, level = 2) {
  if (length(study) == 0) {
    return(doc)
  }
  doc <- heading(doc, "Study", level = level)
  df <- .study_to_df(study)
  ft <- .kv_table(data.frame(key = df$Item, value = df$Details))
  .add_spacer(flextable::body_add_flextable(doc, ft, align = "left"))
}

#' Add ordered agreement clauses as literal headings and narrative paragraphs
#' @keywords internal
.add_agreement_sections <- function(doc, sections, heading = .add_heading, level = 2) {
  if (length(sections) == 0) {
    return(doc)
  }
  doc <- heading(doc, "Agreement Sections", level = level)
  for (section in sections) {
    doc <- heading(doc, section$title, level = level + 1)
    doc <- .add_text_paragraphs(doc, section$text)
    doc <- .add_spacer(doc)
  }
  doc
}

#' Build the version-history table (flextable). Shared by the built-in layout and
#' by the `{VERSION_HISTORY_TABLE}` template block.
#'
#' The rows come from .version_history_df().
#'
#' @param version_history A list of `list(version =, date =, changes =)` records,
#'   as `DTAMetaData@version_history` holds them, or `NULL`.
#' @return A flextable, or `NULL` when there is no history.
#' @keywords internal
.build_version_history_table <- function(version_history) {
  if (is.null(version_history) || length(version_history) == 0) {
    return(NULL)
  }

  version_data <- .version_history_df(version_history)

  ft <- flextable::flextable(version_data)
  ft <- flextable::width(ft, j = 1, width = 1.0)
  ft <- flextable::width(ft, j = 2, width = 1.2)
  ft <- flextable::width(ft, j = 3, width = 4.0)
  .fit_table_width(.style_table(ft, center_cols = c(1, 2)))
}

#' Add a full organization section (affiliation + individual contact blocks).
#' Backup-only contacts are grouped last. Signature lines are NOT drawn here:
#' authorised signatories are collected into the single "Approval & Signatures"
#' table at the front of the document, so repeating an underline per contact
#' would be a second, competing place to sign.
#' @keywords internal
.add_organization_section <- function(doc, title, org_list, level = 2) {
  doc <- .add_heading(doc, title, level = level)

  if (is.null(org_list) || length(org_list) == 0) {
    doc <- .add_body_par(doc, "No information specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    return(doc)
  }

  # Affiliation details (name / country / address / any extension field)
  pairs <- .party_facts(org_list)
  if (nrow(pairs) > 0) {
    ft <- .kv_table(pairs)
    doc <- flextable::body_add_flextable(doc, ft, align = "left")
    doc <- .add_spacer(doc)
  }

  ft <- .build_party_contacts_table(org_list$contacts)
  if (!is.null(ft)) {
    doc <- flextable::body_add_flextable(doc, ft, align = "left")
    doc <- .add_spacer(doc)
  } else if (nrow(pairs) == 0) {
    doc <- .add_body_par(doc, "No contacts specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
  }

  doc
}

#' One table of a party's contacts: Name (with role beneath), Department,
#' Contact (email, phone, address and every other field the YAML carries),
#' Responsibilities. Backup-only contacts are listed last.
#' @keywords internal
.build_party_contacts_table <- function(contacts) {
  df <- .party_contacts_df(contacts)
  if (nrow(df) == 0) {
    return(NULL)
  }

  ft <- flextable::flextable(df, col_keys = c("Name", "Department", "Contact", "Responsibilities"))
  ft <- flextable::width(ft, j = 1:4, width = c(1.7, 1.25, 2.35, 1.0))
  ft <- .style_table(ft)
  # Composed after styling: the house style recolours every chunk, and the
  # role line has to stay muted.
  flextable::compose(
    ft,
    j = "Name",
    value = flextable::as_paragraph(
      flextable::as_chunk(Name, props = .house_fp_text(size = FONT_SIZES$table_body, bold = TRUE)),
      flextable::as_chunk(ifelse(nzchar(Role), paste0("\n", Role), ""),
        props = .house_fp_text(size = FONT_SIZES$table_body, color = THEME_COLORS$gray_mid)
      )
    ),
    part = "body"
  )
}

#' Add "Authorized for Corrections" section
#' @keywords internal
.add_authorized_for_corrections_section <- function(doc, authorized_for_corrections, level = 2) {
  names_vec <- .format_authorized_for_corrections_lines(authorized_for_corrections)
  if (length(names_vec) == 0) {
    return(doc)
  }

  doc <- .add_heading(doc, "Authorized for Corrections", level = level)
  for (nm in names_vec) {
    doc <- .add_body_par(doc, paste0("\u2022  ", nm))
  }
  doc <- .add_spacer(doc)
  doc
}

#' Build the approval/signature table (flextable). Shared by the built-in layout
#' and by the `{SIGNATURES_TABLE}` template block.
#' @param signatories Anything `.normalize_signatories()` accepts, or NULL.
#' @keywords internal
.build_signature_table <- function(signatories) {
  sig_df <- .normalize_signatories(signatories)
  if (is.null(sig_df) || nrow(sig_df) == 0) {
    return(NULL)
  }

  sig_data <- data.frame(
    Organization = sig_df$Organization,
    Name = sig_df$Name,
    Role = sig_df$Role,
    Signature = "",
    Date = "",
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(sig_data)
  ft <- flextable::width(ft, j = 1, width = 1.3)
  ft <- flextable::width(ft, j = 2, width = 1.3)
  ft <- flextable::width(ft, j = 3, width = 1.5)
  ft <- flextable::width(ft, j = 4, width = 1.4)
  ft <- flextable::width(ft, j = 5, width = 0.8)
  ft <- .style_table(ft)
  ft <- flextable::bold(ft, j = 2, bold = TRUE, part = "body")
  # Signature/Date are filled in by hand: give them room and a ruled baseline
  # instead of a row of underscores.
  ft <- flextable::height(ft, height = 0.42, part = "body")
  ft <- flextable::hrule(ft, rule = "atleast", part = "body")
  ft <- .fit_table_width(ft)

  ft
}

#' Add signature approval section.
#' @description
#' Accepts either a data.frame(Organization, Name, Role) (as returned by
#' \code{.extract_signatories()}), a plain list of
#' list(name=, role=, organization=) records, or NULL. One signature row is rendered per authorized signatory.
#'
#' When no signatory is known the section is omitted entirely rather than
#' padded with blank "Approved by / Signature" rules: an anonymous underline
#' is not something a reader can act on, and it left the document with a
#' heading whose content was pure filler.
#' @return The document, unchanged when there is nothing to sign.
#' @keywords internal
.add_signature_section <- function(doc, signatories = NULL, level = 2) {
  ft <- .build_signature_table(signatories)
  if (is.null(ft)) {
    return(doc)
  }
  doc <- .add_heading(doc, "Approval & Signatures", level = level)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc <- .add_spacer(doc)
  doc
}

#' Build the expected-files table (flextable). Shared by the built-in layout and
#' by the `{FILE_SPECS}` template block.
#' @keywords internal
.build_file_specs_table <- function(files, width = TEXT_WIDTH$portrait) {
  if (is.null(files) || length(files) == 0) {
    return(NULL)
  }

  file_data <- .file_specs_df(files)

  ft <- flextable::flextable(file_data)
  ft <- flextable::width(ft, j = 1, width = 1.8)
  ft <- flextable::width(ft, j = 2, width = 1.0)
  ft <- flextable::width(ft, j = 3, width = 0.9)
  ft <- flextable::width(ft, j = 4, width = 1.0)
  ft <- flextable::width(ft, j = 5, width = 2.5)
  if (ncol(file_data) >= 6) ft <- flextable::width(ft, j = 6, width = 1.2)
  ft <- .fit_table_width(.style_table(ft, center_cols = c(2, 3, 4)), width)

  ft
}

#' Add file specifications section
#' @keywords internal
.add_file_specifications <- function(doc, files, title = "File Specifications", heading_level = 2) {
  doc <- if (is.null(heading_level)) .add_bold_subheading(doc, title) else .add_heading(doc, title, level = heading_level)

  if (is.null(files) || length(files) == 0) {
    doc <- .add_body_par(doc, "No files specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    return(doc)
  }
  doc <- .add_body_par(doc, .file_count_sentence(files))
  doc <- .add_spacer(doc)

  ft <- .build_file_specs_table(files)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc <- .add_spacer(doc)

  ft_format <- .build_file_format_table(files)
  if (!is.null(ft_format)) {
    doc <- .add_bold_subheading(doc, "File format")
    doc <- flextable::body_add_flextable(doc, ft_format, align = "left")
    doc <- .add_spacer(doc)
  }

  doc
}

#' How each tabular file is to be parsed: delimiter, header row, quoting,
#' missing-value codes, encoding and embedded line breaks. These are part of
#' the specification a supplier must meet, so they belong in the document.
#' @keywords internal
.build_file_format_table <- function(files) {
  df <- .file_format_df(files)
  if (nrow(df) == 0) {
    return(NULL)
  }
  ft <- flextable::flextable(df)
  ft <- flextable::width(ft, j = 1:7, width = c(1.5, 0.9, 0.6, 0.9, 1.1, 0.7, 0.7))
  .fit_table_width(.style_table(ft))
}

#' Friendly, human-readable format label for a DTAFile object, used in the
#' "Files" section table (e.g. CSV / TSV / Delimited / SAS transport / SAS
#' dataset).
#' @keywords internal
.file_format_label <- function(f) {
  if (inherits(f, "DTAtools::DTAFileXPT")) {
    "SAS transport (.xpt)"
  } else if (inherits(f, "DTAtools::DTAFileCSV")) {
    "CSV"
  } else if (inherits(f, "DTAtools::DTAFileTSV")) {
    "TSV"
  } else if (inherits(f, "DTAtools::DTAFileDelim")) {
    "Delimited"
  } else if (inherits(f, "DTAtools::DTAFileTabular")) {
    "Tabular"
  } else if (inherits(f, "DTAtools::DTAFileSAS7BDAT")) {
    "SAS dataset (.sas7bdat)"
  } else {
    "File"
  }
}

#' Add a bold subheading paragraph — used where heading styles cannot be
#' relied on: officer's default template only defines "heading 1" through
#' "heading 3"
#' (write_dta() uses .new_numbered_docx(), which provides headings 1-4).
#' @keywords internal
.add_bold_subheading <- function(doc, text) {
  fp <- .house_fp_text(size = FONT_SIZES$heading3, bold = TRUE, color = THEME_COLORS$primary_dark)
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(text, fp)))
}

#' Build a validation rules table (flextable). Shared by write_dataset_metadata()
#' and write_dta()'s per-dataset sections.
#'
#' Two columns, `Rule ID | Description`. The description is the complete
#' plain-English wording from `R/rulePhrasing.R` (bold labels, bulleted
#' lists); a rule is never shown as code, YAML or in a monospace font.
#' @param rules_collection A list of DTARule objects (e.g. `specs@rules`) or `NULL`.
#' @param width Total table width in inches, or `NULL` to keep the built widths.
#' @keywords internal
.build_rules_table <- function(rules_collection, width = TEXT_WIDTH$landscape) {
  rules <- if (!is.null(rules_collection)) rules_collection else list()
  if (length(rules) == 0) {
    return(NULL)
  }

  # The plain text fills the cell first so the data frame (and with it
  # ft$body$dataset) carries the description; compose() then lays the same
  # text out with bold labels.
  rule_data <- data.frame(
    `Rule ID` = vapply(rules, function(rule) rule@id, character(1)),
    `Description` = vapply(rules, .rule_text, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  ft <- flextable::flextable(rule_data)
  for (i in seq_along(rules)) {
    ft <- flextable::compose(
      ft,
      i = i, j = 2, part = "body",
      value = .rule_docx_paragraph(rules[[i]])
    )
  }
  # Relative widths only: .fit_table_width() scales them to `width`.
  ft <- flextable::width(ft, j = 1, width = 1.4)
  ft <- flextable::width(ft, j = 2, width = 6.0)
  ft <- .style_table(ft)
  ft <- flextable::bold(ft, j = 1, bold = TRUE, part = "body")
  .fit_table_width(ft, width)
}

#' Does the document already hold the rule reading guide?
#'
#' The guide is shown once per document, before the first rules table. Every
#' caller that adds a rules table asks the document itself, so the guide
#' appears once however many datasets or blocks the document repeats.
#' @keywords internal
.doc_has_rule_reading_guide <- function(doc) {
  first <- .rule_reading_guide()[[1]]
  body <- officer::docx_body_xml(doc)
  texts <- xml2::xml_text(xml2::xml_find_all(body, ".//*[local-name()='p']"))
  any(texts == first)
}

#' Add the rule reading guide, unless the document already holds it
#' @keywords internal
.add_rule_reading_guide <- function(doc) {
  if (.doc_has_rule_reading_guide(doc)) {
    return(doc)
  }
  # A bold title, then one paragraph per point.
  doc <- .add_body_par(doc, RULE_READING_GUIDE_TITLE, bold = TRUE)
  for (text in .rule_reading_guide()) {
    doc <- .add_body_par(doc, text)
  }
  .add_spacer(doc)
}

# Title of the rule reading guide, in Word and Markdown alike.
RULE_READING_GUIDE_TITLE <- "How to read the rules"

# Is the cursor on a section break, i.e. was nothing added since the last one?
.doc_at_section_start <- function(doc) {
  node <- officer::docx_current_block_xml(doc)
  if (inherits(node, "xml_missing")) {
    return(FALSE)
  }
  length(xml2::xml_find_all(node, "./*[local-name()='pPr']/*[local-name()='sectPr']")) > 0
}

#' Close the current document section with the given page orientation.
#'
#' officer's `body_end_section_*()` helpers terminate the content added *before*
#' the call, so a landscape block is produced by ending the preceding content as
#' portrait, adding the block, and only then ending that block as landscape.
#' Page size and margins are read back from the document rather than hardcoded,
#' so a Letter-based template is not silently forced onto A4, and the section
#' type is `nextPage` rather than officer's `oddPage` default, which would pad
#' the document with blank pages.
#' @keywords internal
.end_section_orientation <- function(doc, orient = c("portrait", "landscape"), furniture_meta = NULL,
                                     furniture = NULL) {
  orient <- match.arg(orient)
  officer::body_end_block_section(
    doc,
    officer::block_section(.section_props(doc, orient, furniture_meta, type = "nextPage", furniture = furniture))
  )
}

#' Section properties for one orientation: the template's page size and
#' margins, plus the running header/footer when `furniture_meta` is given.
#'
#' Every section after the cover carries the furniture explicitly. Word only
#' inherits a header from the *previous* section, and the cover has none.
#' @keywords internal
.section_props <- function(doc, orient = c("portrait", "landscape"), furniture_meta = NULL, type = "nextPage",
                           furniture = NULL) {
  orient <- match.arg(orient)

  dims <- officer::docx_dim(doc)
  short_side <- min(dims$page[["width"]], dims$page[["height"]])
  long_side <- max(dims$page[["width"]], dims$page[["height"]])

  size <- if (identical(orient, "landscape")) {
    officer::page_size(width = long_side, height = short_side, orient = "landscape")
  } else {
    officer::page_size(width = short_side, height = long_side, orient = "portrait")
  }

  # docx_dim() reports top/bottom/left/right/header/footer only; gutter is absent.
  gutter <- if ("gutter" %in% names(dims$margins)) dims$margins[["gutter"]] else 0

  margins <- officer::page_mar(
    top = dims$margins[["top"]],
    bottom = dims$margins[["bottom"]],
    left = dims$margins[["left"]],
    right = dims$margins[["right"]],
    header = dims$margins[["header"]],
    footer = dims$margins[["footer"]],
    gutter = gutter
  )

  if (is.null(furniture_meta) && is.null(furniture)) {
    return(officer::prop_section(page_size = size, page_margins = margins, type = type))
  }
  page_w <- if (identical(orient, "landscape")) long_side else short_side
  text_w <- page_w - dims$margins[["left"]] - dims$margins[["right"]]
  # `furniture` is function(width) -> list(header, footer); it is used when no
  # DTA metadata is given (the template guide).
  furniture <- if (!is.null(furniture_meta)) {
    .page_furniture(furniture_meta, width = text_w)
  } else {
    furniture(text_w)
  }
  # officer switches on Word's "different odd and even pages" setting
  # (evenAndOddHeaders) as soon as a section has a header, so even pages need
  # the same furniture explicitly or they print bare.
  officer::prop_section(
    page_size = size, page_margins = margins, type = type,
    header_default = furniture$header, footer_default = furniture$footer,
    header_even = furniture$header, footer_even = furniture$footer
  )
}

#' Add Column Specifications + Validation Rules sections for one dataset (DOCX).
#' Used standalone by write_dataset_metadata() (heading_level = 1) and embedded
#' within write_dta()'s per-dataset "Datasets" section (heading_level = 3);
#' heading_level = NULL uses bold subheadings instead of doc heading styles, so
#' it also works on a template such as officer's default that lacks the deeper
#' heading styles. The sections it closes carry the running header and footer
#' of `furniture_meta` (a DTAMetaData) or of `furniture` (see .section_props()).
#' @keywords internal
.add_dataset_specs_section <- function(doc, dataset, include_rules = TRUE, heading_level = 2,
                                       furniture_meta = NULL, furniture = NULL) {
  if (!inherits(dataset, "DTAtools::DTADataSetTabular")) {
    return(doc)
  }

  add_section_heading <- function(doc, text) {
    if (is.null(heading_level)) .add_bold_subheading(doc, text) else .add_heading(doc, text, level = heading_level)
  }

  # The column specifications table is too wide for the ~6.3in text column of
  # an A4 portrait page, so the table block gets landscape pages of its own,
  # headings included (a heading left behind on the portrait page would be an
  # orphan). officer terminates a section for the content *already added*,
  # hence: close the preceding portrait content first, add the tables, then
  # close them as landscape.
  # Nothing added since the last section break (e.g. a dataset document with
  # neither description, signatories nor file table after its title page):
  # closing again would leave an empty portrait page.
  if (!.doc_at_section_start(doc)) {
    doc <- .end_section_orientation(doc, "portrait", furniture_meta, furniture = furniture)
  }

  doc <- add_section_heading(doc, "Column Specifications")
  ft <- .build_column_specs_table(dataset@specs)
  if (!is.null(ft)) {
    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    doc <- .add_body_par(doc, "No column specifications available.", italic = TRUE, color = THEME_COLORS$gray_mid)
  }
  doc <- .add_spacer(doc)

  if (include_rules) {
    doc <- add_section_heading(doc, "Validation Rules")
    ft_rules <- .build_rules_table(dataset@specs@rules)
    if (!is.null(ft_rules)) {
      doc <- .add_rule_reading_guide(doc)
      doc <- flextable::body_add_flextable(doc, ft_rules)
    } else {
      doc <- .add_body_par(doc, "No validation rules specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    }
    doc <- .add_spacer(doc)
  }

  doc <- .end_section_orientation(doc, "landscape", furniture_meta, furniture = furniture)

  doc
}

#' Format column specifications as a professional table
#' @keywords internal
.build_column_specs_table <- function(specs_collection, width = TEXT_WIDTH$landscape) {
  if (is.null(specs_collection) || length(specs_collection@columns) == 0) {
    return(NULL)
  }

  column_data <- .column_specs_df(specs_collection)

  # Create flextable
  ft <- flextable::flextable(column_data)

  # Set column widths
  ft <- flextable::width(ft, j = 1, width = 1.2) # Variable Name
  ft <- flextable::width(ft, j = 2, width = 1.2) # Label
  ft <- flextable::width(ft, j = 3, width = 1.0) # Type
  ft <- flextable::width(ft, j = 4, width = 0.8) # Nullable
  ft <- flextable::width(ft, j = 5, width = 2.0) # Values/Pattern
  ft <- flextable::width(ft, j = 6, width = 2.2) # Description

  # Formatting. Column specs are dense, so this table takes the compact variant
  # of the house style: same palette and family, one point smaller.
  ft <- .style_table(ft, center_cols = 4)
  ft <- flextable::fontsize(ft, size = FONT_SIZES$table_body - 1, part = "body")
  ft <- flextable::bold(ft, j = 1, bold = TRUE, part = "body")
  ft <- .fit_table_width(ft, width)

  ft
}
