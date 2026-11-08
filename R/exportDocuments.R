#' @title Export DTA Metadata as Professional Document
#' @description
#' Creates a professionally formatted DOCX, PDF, or Markdown document from a DTA object,
#' including study identity, ordered agreement sections, version history,
#' transmission details, and approval signatures. Study and agreement sections
#' are omitted when absent. Agreement text is rendered as plain narrative.
#'
#' The Markdown output (`format = "md"`) is a complete GitHub-flavoured
#' Markdown document with the chapters of the Word layout in the same order
#' (Approval & Signatures, Document Information, Study, Parties, Process
#' Information, Agreement Sections, Datasets), numbered 1, 1.1, 1.1.1 and
#' listed in a linked table of contents. Chapters without content are left
#' out and the numbers close up. Validation rules are described in plain
#' English, never as code. All text from the DTA is escaped, so it cannot
#' change the structure of the document. The file is deterministic: UTF-8
#' with LF line endings and no time stamp, so the same DTA always gives the
#' same bytes.
#'
#' @import S7
#' @importFrom officer read_docx body_add_par body_add_break
#' @importFrom flextable flextable body_add_flextable
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom tools file_ext
#'
#' @param x A DTA object to export
#' @param file Character. Output file path (with .docx, .pdf, or .md extension)
#' @param format Character. Output format: "docx" (default), "pdf", or "md"
#'   (GitHub-flavoured Markdown, see Details). If NULL, inferred from file
#'   extension.
#' @param overwrite Logical. Whether to overwrite existing files. Default: FALSE.
#' @param include_signatures Logical. Whether to include the "Approval &
#'   Signatures" section (DOCX, PDF and Markdown). It lists every contact
#'   flagged `signature: yes` and the entries of `signature_list`, and is left
#'   out when there is nobody to sign. Default: TRUE. Ignored when a
#'   `template` is supplied.
#' @param signature_list List of additional signatories, each a list with
#'   `name`, `role` and optionally `organization` (or `org`) fields; or a
#'   data.frame with columns `Organization`, `Name`, `Role`. They are added to
#'   the signatories taken from the contacts (duplicates by name and
#'   organization are skipped) in the built-in DOCX/PDF layout and in
#'   Markdown; ignored with a `template`.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @param include_yaml Logical. If `TRUE`, append the machine-readable YAML
#'   specification as a final, small-font section of the built-in DOCX/PDF
#'   layout. Requires `yaml_text`. Ignored -- with a warning -- for
#'   `format = "md"`, when a `template` is supplied, and when `yaml_text` is
#'   `NULL` or empty. Default: `FALSE`.
#' @param yaml_text Optional character scalar holding the YAML specification to
#'   embed when `include_yaml = TRUE`. Default: `NULL`.
#' @param template Optional character path to a Word (`.docx`) template
#'   containing placeholder markers such as `{DTA_TITLE}`. When supplied, the
#'   document is produced by filling the template via [export_with_template()]
#'   instead of the built-in layout. Only valid for `format = "docx"` or
#'   `"pdf"`. Default: `NULL`.
#' @param template_variables Optional named list of additional or overriding
#'   placeholder values passed to [export_with_template()] when `template` is
#'   supplied. Default: `NULL`.
#' @param document_name Optional character: what this kind of document is
#'   called, shown as the main title of the export (Word/PDF cover, Markdown
#'   heading); the DTA's own title follows beneath it. A vector is joined with
#'   `" / "`. `NULL` (the default) uses "Data Transfer Agreement". Also fills
#'   the `{DOCUMENT_NAME}` template placeholder.
#' @param document_abbreviation Optional single string, the abbreviation of
#'   `document_name`. `NULL` (the default) uses "DTA". Fills the
#'   `{DOCUMENT_ABBREVIATION}` template placeholder.
#' @param organisation,organisation_unit Optional single strings naming the
#'   organisation (and unit within it) issuing the document. When given they are
#'   shown on the cover and in the running header of every later page (Word/PDF)
#'   or in the top header block (Markdown), and fill the `{ORGANISATION_NAME}`
#'   and `{ORGANISATION_UNIT}` template placeholders. `NULL` or empty (the
#'   default) shows nothing.
#'
#' @return Invisibly. For `format = "docx"` or `"pdf"` with the built-in layout,
#'   the `officer` `rdocx` document object; when a `template` is used, the
#'   output file path; for `format = "md"`, `NULL`.
#'
#' @seealso [export_with_template()] for the full list of supported placeholders.
#'
#' @export
#' @examples
#' dta <- create_example_DTA()
#' out <- tempfile(fileext = ".docx")
#' write_dta(dta, file = out, quiet = TRUE)
#' file.exists(out)
#' unlink(out)
#'
#' # Fill a user-authored Word template instead of the built-in layout
#' template <- tempfile(fileext = ".docx")
#' doc <- officer::read_docx()
#' doc <- officer::body_add_par(doc, "Title: {DTA_TITLE}")
#' doc <- officer::body_add_par(doc, "Version: {DTA_VERSION}")
#' print(doc, target = template)
#'
#' out_from_template <- tempfile(fileext = ".docx")
#' write_dta(dta, file = out_from_template, template = template, quiet = TRUE)
#' file.exists(out_from_template)
#' unlink(c(template, out_from_template))
#'
#' # Name the document and the organisation issuing it
#' out_named <- tempfile(fileext = ".md")
#' write_dta(
#'   dta,
#'   file = out_named, quiet = TRUE,
#'   document_name = "Data Transmission Specification",
#'   document_abbreviation = "DTS",
#'   organisation = "Example Pharma", organisation_unit = "Biometrics"
#' )
#' readLines(out_named, n = 3)
#' unlink(out_named)
write_dta <- function(
  x,
  file,
  format = NULL,
  overwrite = FALSE,
  include_signatures = TRUE,
  signature_list = NULL,
  quiet = FALSE,
  include_yaml = FALSE,
  yaml_text = NULL,
  template = NULL,
  template_variables = NULL,
  document_name = NULL,
  document_abbreviation = NULL,
  organisation = NULL,
  organisation_unit = NULL
) {
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("'x' must be a DTA object.")
  }
  .set_export_ctx(document_name, organisation, organisation_unit, document_abbreviation)

  if (is.null(format)) {
    format <- tolower(tools::file_ext(file))
    if (format == "") format <- "docx"
  }

  format <- tolower(format)

  if (!format %in% c("docx", "pdf", "md")) {
    cli::cli_abort("'format' must be 'docx', 'pdf', or 'md'.")
  }

  if (file.exists(file) && !overwrite) {
    cli::cli_abort("File '{file}' already exists. Set overwrite = TRUE to replace.")
  }

  # Template-based export: fill a user-provided Word template instead of the
  # built-in layout. Only meaningful for the DOCX/PDF outputs.
  if (!is.null(template)) {
    if (!format %in% c("docx", "pdf")) {
      cli::cli_abort(
        "Template-based export is only supported for 'docx' or 'pdf' output, not '{format}'."
      )
    }
    if (isTRUE(include_yaml)) {
      cli::cli_warn(c(
        "{.arg include_yaml} is ignored when a {.arg template} is supplied.",
        i = "The embedded YAML section belongs to the built-in layout only.",
        i = "Add the specification to the template yourself, or drop {.arg template}."
      ))
    }
    target_docx <- if (format == "docx") file else tempfile(fileext = ".docx")
    if (format == "pdf") {
      on.exit(unlink(target_docx, force = TRUE), add = TRUE)
    }
    export_with_template(
      x,
      template = template,
      output = target_docx,
      variables = template_variables,
      quiet = TRUE,
      fallback = FALSE
    )
    if (format == "pdf") {
      .convert_docx_to_pdf(target_docx, file)
    }
    if (!isTRUE(quiet)) {
      cli::cli_alert_success("Document saved to {file}")
    }
    return(invisible(file))
  }

  # Create document
  doc <- NULL
  if (format == "docx") {
    doc <- .write_dta_docx(x, include_signatures, signature_list, include_yaml, yaml_text)
    .dta_write_staged(file, function(stage) print(doc, target = stage))
  } else if (format == "pdf") {
    # First create DOCX, then convert to PDF
    temp_docx <- tempfile(fileext = ".docx")
    on.exit(unlink(temp_docx, force = TRUE), add = TRUE)
    doc <- .write_dta_docx(x, include_signatures, signature_list, include_yaml, yaml_text)
    print(doc, target = temp_docx)
    .convert_docx_to_pdf(temp_docx, file)
  } else if (format == "md") {
    if (isTRUE(include_yaml)) {
      cli::cli_warn(c(
        "{.arg include_yaml} is ignored for {.code format = \"md\"}.",
        i = "The embedded YAML section is only produced for the DOCX/PDF layout."
      ))
    }
    .write_dta_markdown(x, file, include_signatures, signature_list)
  }

  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Document saved to {file}")
  }
  invisible(doc)
}

#' Write a document to a staging file, then move it into place
#'
#' Every document writer in this file, and the template writer in
#' `exportTemplateDocx.R`, produces its output through this helper instead of
#' writing straight to the destination. `officer::print(target = )`,
#' `writeLines()` and the PDF backends all truncate or create their target
#' before the write finishes, so a failure partway used to leave a corrupt (or
#' entirely wiped) file where a good export -- possibly one nobody has touched
#' in months -- used to be. That is worse than aborting with nothing written:
#' the destination must come out of a failed export exactly as it went in.
#'
#' Mirrors the stage-then-rename pattern already used by
#' `write_table_to_file()` (`R/DTADataSetTabular-class.R`): the staging file is
#' created in `dirname(path)` so the final step is a rename within one
#' filesystem rather than a copy across two, and it is named with the same
#' extension as `path` because officer and pandoc both dispatch on the
#' extension of their output path.
#'
#' @param path Character. Final destination.
#' @param write Function of one argument (the staging path) that produces the
#'   file there. Its return value is ignored.
#' @param check Optional function of one argument (the staging path) that
#'   signals an error (typically via `cli::cli_abort()`) if the staged file is
#'   unacceptable. Runs after `write()` and before the file is moved into
#'   place.
#' @return Invisibly returns `path`.
#' @keywords internal
.dta_write_staged <- function(path, write, check = NULL) {
  ext <- tools::file_ext(path)
  stage <- tempfile(tmpdir = dirname(path), fileext = if (nzchar(ext)) paste0(".", ext) else "")
  on.exit(unlink(stage, force = TRUE), add = TRUE)

  write(stage)
  if (!is.null(check)) {
    check(stage)
  }

  if (!file.rename(stage, path)) {
    cli::cli_abort(c(
      "Could not move the finished export into place at {.path {path}}.",
      i = "The previous contents of {.path {path}}, if any, are unchanged."
    ))
  }
  invisible(path)
}

#' @keywords internal
.write_dta_docx <- function(dta, include_signatures, signature_list,
                            include_yaml = FALSE, yaml_text = NULL) {
  # Open the bundled reference template so headings 1-4 auto-number as
  # 1 / 1.1 / 1.1.1 / 1.1.1.1 (true Word fields that renumber if edited).
  doc <- .new_numbered_docx()

  meta <- dta@metadata

  # Cover page: its own section, without running header or footer. Every later
  # section carries the header (DTA title, study, page X of Y) and footer
  # (document name, version, version date, template) explicitly.
  doc <- .add_cover_page(doc, meta)

  # Each chapter is a top-level heading (1, 2, 3 ...), so the numbering stays
  # shallow: a dataset's column table is 7.1.2, not 1.10.1.2.

  # Approval comes first: the signature block is what a reader has to act on.
  if (isTRUE(include_signatures)) {
    doc <- .add_signature_section(
      doc,
      signatories = .extract_signatories(meta, signature_list = signature_list),
      level = 1
    )
  }

  # Document information, including where the document came from.
  doc <- .add_heading(doc, "Document Information", level = 1)
  doc <- flextable::body_add_flextable(doc, .kv_table(.document_info_pairs(meta)), align = "left")
  doc <- .add_spacer(doc)

  ft_history <- .build_version_history_table(meta@version_history)
  if (!is.null(ft_history)) {
    doc <- .add_heading(doc, "Version History", level = 2)
    doc <- flextable::body_add_flextable(doc, ft_history, align = "left")
    doc <- .add_spacer(doc)
  }

  doc <- .add_study_section(doc, meta@study, level = 1)

  # Parties. Supplier comes first: the data flows supplier -> receiver, so that
  # is the order the parties are introduced in.
  if (length(meta@supplier) > 0 || length(meta@receiver) > 0) {
    doc <- .add_heading(doc, "Parties", level = 1)
    if (length(meta@supplier) > 0) {
      doc <- .add_organization_section(doc, "Supplier", meta@supplier, level = 2)
    }
    if (length(meta@receiver) > 0) {
      doc <- .add_organization_section(doc, "Receiver", meta@receiver, level = 2)
    }
  }

  # Process Information precedes the agreement clauses and dataset details,
  # and appears only when there is genuine process content to show.
  has_process_information <- length(meta@transmission) > 0 ||
    !is.null(meta@error_handling) ||
    length(.format_authorized_for_corrections_lines(meta@authorized_for_corrections)) > 0

  if (has_process_information) {
    doc <- .add_heading(doc, "Process Information", level = 1)

    if (length(meta@transmission) > 0) {
      doc <- .add_heading(doc, "Transmission Details", level = 2)
      ft <- .kv_table(.transmission_pairs(meta@transmission))
      doc <- flextable::body_add_flextable(doc, ft, align = "left")
      doc <- .add_spacer(doc)
    }

    if (!is.null(meta@error_handling)) {
      doc <- .add_heading(doc, "Error Handling", level = 2)
      doc <- .add_body_par(doc, meta@error_handling)
      doc <- .add_spacer(doc)
    }

    doc <- .add_authorized_for_corrections_section(doc, meta@authorized_for_corrections, level = 2)
  }

  doc <- .add_agreement_sections(doc, meta@sections, level = 1)

  # Datasets: facts, files, column specifications and validation rules.
  if (length(dta@datasets) > 0) {
    doc <- .add_heading(doc, "Datasets", level = 1)

    for (ds_name in names(dta@datasets)) {
      dataset <- dta@datasets[[ds_name]]

      doc <- .add_heading(doc, dta_dataset_heading(dataset), level = 2)
      if (!is.null(dataset@description) && nzchar(dataset@description)) {
        doc <- .add_body_par(doc, dataset@description)
        doc <- .add_spacer(doc)
      }
      doc <- flextable::body_add_flextable(doc, .kv_table(.dataset_facts(dataset)), align = "left")
      doc <- .add_spacer(doc)

      doc <- .add_file_specifications(doc, dataset@files, title = "Files", heading_level = 3)
      doc <- .add_dataset_specs_section(doc, dataset,
        include_rules = TRUE, heading_level = 3, furniture_meta = meta
      )
    }
  }

  # Embedded machine-readable YAML, appended as a final small-font chapter.
  if (isTRUE(include_yaml)) {
    has_yaml <- !is.null(yaml_text) &&
      length(yaml_text) > 0 &&
      !anyNA(yaml_text) &&
      any(nzchar(yaml_text))
    if (has_yaml) {
      doc <- .add_embedded_yaml_section(doc, yaml_text, level = 1)
    } else {
      cli::cli_warn(c(
        "{.arg include_yaml} is {.code TRUE} but {.arg yaml_text} is empty; no specification was embedded.",
        i = "Pass the YAML specification via {.arg yaml_text} to embed it."
      ))
    }
  }

  # The final section (portrait) carries the running header and footer too.
  officer::body_set_default_section(doc, .section_props(doc, "portrait", meta, type = "nextPage"))
}

# The Markdown document of write_dta(): a complete, numbered GitHub-flavoured
# Markdown file with the chapters of the Word layout in the same order --
# Approval & Signatures, Document Information (+ Version History), Study,
# Parties, Process Information, Agreement Sections, Datasets -- a linked
# "Contents" list, and a footer without any time stamp. Empty chapters are left
# out and the numbers close up (.md_render()). Every piece of author text goes
# through .md_escape() / .md_table(), so it cannot change the structure. The
# facts come from the shared builders in R/documentData.R, which also feed the
# Word layout.
#' @keywords internal
.write_dta_markdown <- function(dta, file, include_signatures = TRUE, signature_list = NULL) {
  meta <- dta@metadata

  sections <- .md_children(
    if (isTRUE(include_signatures)) {
      .md_section(
        "Approval & Signatures",
        .md_signature_table(.extract_signatories(meta, signature_list = signature_list))
      )
    },
    .md_section(
      "Document Information",
      .md_kv_table(.document_info_pairs(meta)),
      children = .md_children(
        .md_section("Version History", .md_table(.version_history_df(meta@version_history)))
      )
    ),
    if (length(meta@study) > 0) {
      .md_section("Study", .md_kv_table(.study_to_df(meta@study)))
    },
    .md_parties_section(meta),
    .md_process_section(meta),
    .md_agreement_section(meta@sections),
    .md_datasets_section(dta@datasets)
  )

  lines <- .md_render(.md_dta_head(meta), sections, .md_dta_footer(meta))
  .md_write(lines, file)
}

# Drop NULL entries, so optional chapters can be written inline as `if (...)`.
.md_children <- function(...) {
  Filter(Negate(is.null), list(...))
}

# One line of author text, escaped: runs of white space (line breaks included)
# become a single space. "" when there is nothing to show.
.md_inline <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  x <- as.character(x)
  x <- .md_clean_title(paste(x[!is.na(x)], collapse = " "))
  if (!nzchar(x)) "" else .md_escape(x)
}

# Author text as escaped paragraphs: a blank line separates two paragraphs, a
# single line break inside a paragraph stays a line break (a hard break, as in
# the Word layout). character(0) when there is no text.
.md_paragraphs <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(character(0))
  }
  text <- as.character(text)
  lines <- .md_lines(paste(text[!is.na(text)], collapse = "\n\n"))
  lines <- .md_trim_blank(sub("[ \t]+$", "", lines))
  if (length(lines) == 0L) {
    return(character(0))
  }
  paragraph <- cumsum(!nzchar(lines))
  blocks <- lapply(unique(paragraph[nzchar(lines)]), function(p) {
    escaped <- .md_lines(.md_escape(paste(lines[paragraph == p & nzchar(lines)], collapse = "\n")))
    n <- length(escaped)
    if (n > 1L) {
      escaped[-n] <- paste0(escaped[-n], "\\")
    }
    escaped
  })
  do.call(.md_compose, blocks)
}

# Head of the DTA document: document name as the only level-1 heading, the
# DTA title in bold, the organisation line, the document header and the cover
# facts (version, date, study, parties, template) as a table.
.md_dta_head <- function(meta) {
  title <- .dta_display_title(meta@title, fallback = "")
  .md_compose(
    paste("#", .md_inline(.export_document_name())),
    if (nzchar(.md_inline(title))) paste0("**", .md_inline(title), "**"),
    .md_inline(.export_org_line()),
    .md_paragraphs(meta@header),
    .md_kv_table(.cover_facts(meta))
  )
}

# Footer of the DTA document, as in the Word footer: title, version and version
# date, then the template the document was generated from. Nothing in it
# depends on the time or the locale of the run.
.md_dta_footer <- function(meta) {
  bits <- c(
    .dta_display_title(meta@title),
    if (!is.null(meta@version) && nzchar(meta@version)) paste("Version", meta@version),
    .format_document_date(.current_version_date(meta))
  )
  .md_footer(bits, .template_provenance_text(meta@template))
}

# A horizontal rule, then one line of facts joined by a middle dot and one
# provenance line.
.md_footer <- function(bits, provenance) {
  bits <- bits[nzchar(bits)]
  .md_compose(
    "---",
    .md_inline(paste(bits, collapse = " \u00b7 ")),
    .md_inline(provenance)
  )
}

# The approval table: Organization | Name | Role | Signature | Date, one row
# per signatory. The Signature and Date cells are left empty, with two line
# breaks so a printed page has room to sign. character(0) without signatories.
.md_signature_table <- function(signatories) {
  sig <- .normalize_signatories(signatories)
  if (is.null(sig) || nrow(sig) == 0) {
    return(character(0))
  }
  table <- .md_table(data.frame(
    Organization = .md_cell_chr(sig$Organization),
    Name = .md_cell_chr(sig$Name),
    Role = .md_cell_chr(sig$Role),
    stringsAsFactors = FALSE
  ))
  space <- "<br><br>"
  c(
    sub("\\|$", "| Signature | Date |", table[1L]),
    sub("\\|$", "| --- | --- |", table[2L]),
    sub("\\|$", paste0("| ", space, " | ", space, " |"), table[-(1:2)])
  )
}

# "Parties": Supplier, then Receiver (the direction the data flows), each with
# its affiliation table and its contact table (backups last).
.md_parties_section <- function(meta) {
  party <- function(title, org) {
    if (length(org) == 0) {
      return(NULL)
    }
    body <- .md_compose(
      .md_kv_table(.party_facts(org)),
      .md_table(.party_contacts_df(org$contacts))
    )
    .md_section(title, if (length(body) > 0) body else "No contacts specified.")
  }
  children <- .md_children(party("Supplier", meta@supplier), party("Receiver", meta@receiver))
  if (length(children) == 0) {
    return(NULL)
  }
  .md_section("Parties", children = children)
}

# "Process Information": transmission details, error handling and who is
# authorised to make corrections.
.md_process_section <- function(meta) {
  children <- .md_children(
    if (length(meta@transmission) > 0) {
      .md_section("Transmission Details", .md_kv_table(.transmission_pairs(meta@transmission)))
    },
    if (!is.null(meta@error_handling)) {
      .md_section("Error Handling", .md_paragraphs(meta@error_handling))
    },
    .md_section(
      "Authorized for Corrections",
      .md_list(.format_authorized_for_corrections_lines(meta@authorized_for_corrections))
    )
  )
  .md_section("Process Information", children = children)
}

# "Agreement Sections": one sub-chapter per clause, its text as paragraphs.
.md_agreement_section <- function(sections) {
  if (length(sections) == 0) {
    return(NULL)
  }
  children <- lapply(sections, function(section) {
    title <- .md_clean_title(paste(as.character(section$title %||% ""), collapse = " "))
    # A clause without text keeps its heading, as in Word: the missing text
    # is shown like every other missing value.
    text <- .md_paragraphs(section$text)
    .md_section(
      if (nzchar(title)) title else "Untitled section",
      if (length(text) > 0) text else .md_escape(MISSING_VALUE_DISPLAY)
    )
  })
  .md_section("Agreement Sections", children = children)
}

# "Datasets": per dataset its description, its fact table and the Files,
# Column Specifications and Validation Rules sub-chapters. The rule reading
# guide is shown once, before the first list of rules in the document.
.md_datasets_section <- function(datasets) {
  if (length(datasets) == 0) {
    return(NULL)
  }
  guide <- TRUE
  children <- list()
  for (ds_name in names(datasets)) {
    dataset <- datasets[[ds_name]]
    parts <- .md_dataset_parts(dataset, guide = guide)
    guide <- guide && !parts$guide_shown
    children[[length(children) + 1L]] <- .md_section(
      dta_dataset_heading(dataset),
      .md_compose(.md_paragraphs(dataset@description), .md_kv_table(.dataset_facts(dataset))),
      children = parts$sections
    )
  }
  .md_section("Datasets", children = children)
}

# The Files, Column Specifications and Validation Rules chapters of one dataset
# (the latter two only for a tabular dataset). `guide = TRUE` puts the rule
# reading guide before the rules, if the dataset has any. Returns
# list(sections =, guide_shown =).
.md_dataset_parts <- function(dataset, include_file_specs = TRUE, include_rules = TRUE, guide = TRUE) {
  tabular <- inherits(dataset, "DTAtools::DTADataSetTabular")
  rules <- if (tabular) dataset@specs@rules %||% list() else list()
  guide_shown <- isTRUE(guide) && isTRUE(include_rules) && length(rules) > 0

  columns <- if (tabular) .md_table(.column_specs_df(dataset@specs)) else character(0)
  sections <- .md_children(
    if (isTRUE(include_file_specs)) .md_section("Files", .md_files_body(dataset@files)),
    if (tabular) {
      .md_section(
        "Column Specifications",
        if (length(columns) > 0) columns else "No column specifications available."
      )
    },
    if (tabular && isTRUE(include_rules)) {
      .md_section("Validation Rules", .md_rules_body(rules, guide = guide_shown))
    }
  )
  list(sections = sections, guide_shown = guide_shown)
}

# Body of a "Files" chapter: how many files are expected, the file table and,
# for tabular files, how each file is to be parsed.
.md_files_body <- function(files) {
  if (is.null(files) || length(files) == 0) {
    return("No files specified.")
  }
  format_table <- .md_table(.file_format_df(files))
  .md_compose(
    .md_escape(.file_count_sentence(files)),
    .md_table(.md_file_specs_df(files)),
    if (length(format_table) > 0) "**File format**",
    format_table
  )
}

# .file_specs_df() for Markdown: the Description cell lists the pattern
# description and then every `info` entry on a line of its own ("Data:
# smrnaseq"), instead of the nested bullet text the Word table shows.
.md_file_specs_df <- function(files) {
  df <- .file_specs_df(files)
  df$Description <- vapply(files, function(f) {
    desc <- f@pattern_description
    desc <- if (length(desc) == 0 || is.na(desc[[1]])) "" else as.character(desc[[1]])
    info <- tryCatch(f@info, error = function(e) NULL)
    lines <- c(if (nzchar(desc)) desc, .md_info_lines(info))
    paste(lines, collapse = "\n")
  }, character(1))
  df
}

# Flatten a (possibly nested) `info` value to display lines: a named element
# reads "Name: value", a nested name "Outer / Inner: value", an unnamed one
# just its value. Scalars are shown like every other metadata value.
.md_info_lines <- function(x, label = "") {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  if (!is.list(x)) {
    values <- vapply(seq_along(x), function(i) .format_scalar_value(x[i], truncate = FALSE), character(1))
    if (nzchar(label)) {
      return(paste0(label, ": ", paste(values, collapse = ", ")))
    }
    return(values)
  }
  nms <- names(x) %||% rep("", length(x))
  nms[is.na(nms)] <- ""
  unlist(lapply(seq_along(x), function(i) {
    own <- if (nzchar(nms[[i]])) .title_case_field(nms[[i]]) else ""
    sub_label <- paste(c(label, own)[nzchar(c(label, own))], collapse = " / ")
    .md_info_lines(x[[i]], sub_label)
  }), use.names = FALSE)
}

# Body of a "Validation Rules" chapter: optionally the reading guide, then one
# block per rule in the shared plain-English wording -- never as code.
.md_rules_body <- function(rules, guide = FALSE) {
  if (length(rules) == 0) {
    return("No validation rules specified.")
  }
  blocks <- c(
    if (isTRUE(guide)) as.list(.md_escape(.rule_reading_guide())),
    lapply(rules, .md_rule_block)
  )
  do.call(.md_compose, blocks)
}

# One rule as Markdown: .rule_md() plus the two escapes .md_escape() adds to
# the plain-text escaping .rule_md() applies ("$" and emoji short codes), so a
# value in a rule is as literal as any other author text.
.md_rule_block <- function(rule) {
  text <- .rule_md(rule, heading = TRUE)
  text <- gsub("$", "\\$", text, fixed = TRUE)
  gsub(":(?=[A-Za-z0-9_+-]+:)", "\\\\:", text, perl = TRUE)
}

#' Locate Microsoft Word (`WINWORD.EXE`)
#'
#' Windows only. Reads the default value of the `App Paths` registry key that
#' Office registers, in the 64-bit then 32-bit view of `HKEY_LOCAL_MACHINE`,
#' then of `HKEY_CURRENT_USER`.
#' @return The path to `WINWORD.EXE`, or `""` when Word is not installed or the
#'   platform is not Windows.
#' @keywords internal
.find_word <- function() {
  if (.Platform$OS.type != "windows") {
    return("")
  }
  key <- "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Winword.exe"
  for (hive in c("HLM", "HCU")) {
    for (view in c("64-bit", "32-bit")) {
      value <- tryCatch(
        utils::readRegistry(key, hive = hive, view = view)[["(Default)"]],
        error = function(e) NULL,
        warning = function(w) NULL
      )
      if (is.character(value) && length(value) == 1 && !is.na(value)) {
        value <- gsub("\"", "", value, fixed = TRUE)
        if (nzchar(value) && file.exists(value)) {
          return(normalizePath(value, winslash = "/", mustWork = FALSE))
        }
      }
    }
  }
  ""
}

#' Locate a LibreOffice (`soffice`) binary
#'
#' Searches the `PATH` first, then the conventional install locations on
#' Windows, Linux and macOS.
#' @return The path to the binary, or `""` when LibreOffice is not installed.
#' @keywords internal
.find_soffice <- function() {
  found <- Sys.which(c("soffice", "libreoffice"))
  found <- found[nzchar(found)]
  if (length(found) > 0) {
    return(unname(found[[1]]))
  }
  guesses <- c(
    "C:/Program Files/LibreOffice/program/soffice.exe",
    "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
    "/usr/bin/soffice",
    "/usr/bin/libreoffice",
    "/usr/local/bin/soffice",
    "/opt/libreoffice/program/soffice",
    "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  )
  hit <- guesses[file.exists(guesses)]
  if (length(hit) > 0) hit[[1]] else ""
}

#' Locate the TinyTeX binary directory
#'
#' A fresh `tinytex::install_tinytex()` puts `pdflatex` on the *user's* `PATH`,
#' but an R session started before (or outside) that change does not see it --
#' `tinytex::is_tinytex()` is `TRUE` while `Sys.which("pdflatex")` is empty, and
#' pandoc then dies with "pdflatex not found". Resolving the directory through
#' the exported [tinytex::tinytex_root()] lets us put it back on the `PATH` for
#' the duration of a conversion.
#'
#' @return The directory holding the TinyTeX binaries, or `""`.
#' @keywords internal
.tinytex_bin_dir <- function() {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    return("")
  }
  root <- tryCatch(tinytex::tinytex_root(), error = function(e) "")
  if (length(root) != 1 || is.na(root) || !nzchar(root) || !dir.exists(root)) {
    return("")
  }
  exe <- if (.Platform$OS.type == "windows") "pdflatex.exe" else "pdflatex"
  candidates <- list.dirs(file.path(root, "bin"), recursive = FALSE)
  hit <- candidates[file.exists(file.path(candidates, exe))]
  if (length(hit) > 0) hit[[1]] else ""
}

#' Find a PDF engine that pandoc can drive
#'
#' pandoc cannot produce a PDF on its own -- it shells out to a PDF engine, and
#' reports a bare "pdflatex not found" when none exists. Probing first turns
#' that into an actionable error.
#' @return The engine name, or `""` when no engine is installed.
#' @keywords internal
.pandoc_pdf_engine <- function() {
  engines <- c(
    "pdflatex", "xelatex", "lualatex", "tectonic",
    "typst", "wkhtmltopdf", "weasyprint", "context", "pdfroff"
  )
  for (engine in engines) {
    if (nzchar(Sys.which(engine))) {
      return(engine)
    }
  }
  # TinyTeX may be installed but absent from this session's PATH.
  if (nzchar(.tinytex_bin_dir())) {
    return("pdflatex")
  }
  ""
}

#' Which DOCX -> PDF backends can run on this machine, best first
#'
#' The single seam every other PDF function consults, so backend detection can
#' be exercised (and mocked) independently of the conversion itself.
#'
#' Microsoft Word comes first (Windows only): it is the native renderer, keeps
#' running headers and footers, and updates the table of contents and page
#' fields. LibreOffice is next: it renders the DOCX the way Word does, preserving
#' the flextable column widths, shading and the auto-numbered headings. The
#' pandoc routes re-parse the DOCX into pandoc's AST and re-lay it out through
#' LaTeX, which is a faithful *text* conversion but loses much of the layout.
#'
#' `"tinytex"` outranks `"pandoc"` because it compiles the intermediate LaTeX
#' with [tinytex::latexmk()], which installs missing LaTeX packages on demand. A
#' freshly installed TinyTeX has no `caption.sty`, so letting pandoc drive
#' `pdflatex` directly aborts on the first document that needs one.
#'
#' @return A character vector of backend names in priority order, possibly
#'   empty. One or more of `"word"`, `"libreoffice"`, `"tinytex"` and `"pandoc"`.
#' @keywords internal
.pdf_backends_available <- function() {
  backends <- character(0)
  if (nzchar(.find_word())) {
    backends <- c(backends, "word")
  }
  if (nzchar(.find_soffice())) {
    backends <- c(backends, "libreoffice")
  }
  pandoc_ok <- requireNamespace("rmarkdown", quietly = TRUE) &&
    isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE))
  if (pandoc_ok && nzchar(.tinytex_bin_dir())) {
    backends <- c(backends, "tinytex")
  }
  if (pandoc_ok && nzchar(.pandoc_pdf_engine())) {
    backends <- c(backends, "pandoc")
  }
  backends
}

#' Report whether any DOCX -> PDF conversion backend is usable
#'
#' @return `TRUE` when at least one backend in [.pdf_backends_available()] can run.
#' @keywords internal
.pdf_conversion_available <- function() {
  length(.pdf_backends_available()) > 0
}

#' Report the DOCX to PDF conversion backend this machine will use
#'
#' `write_dta(format = "pdf")` and `write_dataset_metadata(format = "pdf")`
#' build a Word document and then convert it. That conversion needs an external
#' tool, which R cannot supply on its own. Call this to check the setup *before*
#' relying on a PDF export -- for example at the top of a batch script.
#'
#' Four backends are supported, tried in this order:
#'
#' * **Microsoft Word** -- Windows only, found through the registry. Preferred
#'   because it is the native renderer: it keeps running page headers and
#'   footers and updates the table of contents and page-number fields.
#' * **LibreOffice** -- a `soffice` binary on the `PATH` or in a standard
#'   install location. It renders the document much as Word does and preserves
#'   table shading, column widths and heading numbering.
#' * **TinyTeX** -- pandoc converts the DOCX to LaTeX and [tinytex::latexmk()]
#'   compiles it, installing any missing LaTeX packages on demand. Page headers
#'   and footers are lost.
#' * **pandoc** -- pandoc driving some other PDF engine already on the `PATH`
#'   (a full LaTeX distribution, `typst`, `wkhtmltopdf`, ...). Page headers and
#'   footers are lost.
#'
#' If none is present, install a LaTeX engine from R with
#' `tinytex::install_tinytex()`; it needs no administrator rights and works on
#' Windows, macOS and Linux.
#'
#' @return `NULL` when no backend is available. Otherwise a list with:
#'   \describe{
#'     \item{`name`}{`"word"`, `"libreoffice"`, `"tinytex"` or `"pandoc"`.}
#'     \item{`engine`}{The Word or `soffice` path, the TinyTeX binary directory, or the
#'       pandoc PDF engine name.}
#'     \item{`available`}{Every usable backend, in priority order.}
#'   }
#'
#' @seealso [write_dta()], [write_dataset_metadata()]
#' @export
#' @examples
#' backend <- dta_pdf_backend()
#' if (is.null(backend)) {
#'   message("No PDF backend; run tinytex::install_tinytex() to add one.")
#' } else {
#'   message("PDF export will use: ", backend$name)
#' }
dta_pdf_backend <- function() {
  backends <- .pdf_backends_available()
  if (length(backends) == 0) {
    return(NULL)
  }
  list(
    name = backends[[1]],
    engine = switch(backends[[1]],
      word = .find_word(),
      libreoffice = .find_soffice(),
      tinytex = .tinytex_bin_dir(),
      .pandoc_pdf_engine()
    ),
    available = backends
  )
}

#' Run the LibreOffice DOCX -> PDF conversion
#'
#' Converts in a private profile directory (`-env:UserInstallation`) so a
#' LibreOffice window already open on the user's desktop cannot make the
#' headless run fail or hang.
#' @param docx_file Character. Existing `.docx` to convert.
#' @param pdf_file Character. Path of the `.pdf` to create.
#' @return Invisibly returns `pdf_file`.
#' @keywords internal
.soffice_docx_to_pdf <- function(docx_file, pdf_file) {
  soffice <- .find_soffice()
  out_dir <- tempfile("dta_pdf_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  profile_dir <- tempfile("dta_lo_profile_")
  on.exit(unlink(profile_dir, recursive = TRUE, force = TRUE), add = TRUE)

  suppressWarnings(system2(
    soffice,
    c(
      paste0(
        "-env:UserInstallation=file:///",
        gsub("^/+", "", gsub("\\\\", "/", normalizePath(profile_dir, winslash = "/", mustWork = FALSE)))
      ),
      "--headless", "--norestore", "--convert-to", "pdf",
      "--outdir", shQuote(normalizePath(out_dir, winslash = "/", mustWork = TRUE)),
      shQuote(normalizePath(docx_file, winslash = "/", mustWork = TRUE))
    ),
    stdout = TRUE,
    stderr = TRUE,
    timeout = 180
  ))

  produced <- file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(docx_file)), ".pdf")
  )
  if (!file.exists(produced)) {
    cli::cli_abort("LibreOffice did not write a PDF for {.file {docx_file}}.")
  }
  if (!file.copy(produced, pdf_file, overwrite = TRUE)) {
    cli::cli_abort("Could not move the converted PDF to {.file {pdf_file}}.")
  }
  invisible(pdf_file)
}

#' Run the Microsoft Word DOCX -> PDF conversion
#'
#' Drives Word through COM from a throw-away PowerShell script. Word updates the
#' table of contents and the page fields while exporting, and keeps the running
#' headers and footers.
#'
#' Word 2013+ may hand `New-Object -ComObject Word.Application` the instance the
#' user already has open, so the script compares the `WINWORD` processes before
#' and after. A *new* process is ours: it is hidden, silenced and quit. If none
#' appeared the script attached to the user's session, whose `Visible` and
#' `DisplayAlerts` it leaves alone; it opens the document invisibly, closes only
#' that document and never quits Word. Our process id goes to a file so that R
#' can kill a Word that outlives a timeout or a failure.
#' @param docx_file Character. Existing `.docx` to convert.
#' @param pdf_file Character. Path of the `.pdf` to create.
#' @return Invisibly returns `pdf_file`.
#' @keywords internal
.word_docx_to_pdf <- function(docx_file, pdf_file) {
  ps1 <- tempfile("dta_word_", fileext = ".ps1")
  tmp_pdf <- tempfile("dta_word_", fileext = ".pdf")
  pid_file <- tempfile("dta_word_", fileext = ".pid")
  on.exit(.kill_created_word(pid_file), add = TRUE)
  on.exit(unlink(c(ps1, tmp_pdf, pid_file), force = TRUE), add = TRUE)
  writeLines(
    c(
      "param([string]$In, [string]$Out, [string]$PidFile)",
      "$ErrorActionPreference = 'Stop'",
      "$m = [Type]::Missing",
      "$before = @(Get-Process WINWORD -ErrorAction SilentlyContinue | ForEach-Object { $_.Id })",
      "$w = $null",
      "$d = $null",
      "$ours = $false",
      "try {",
      "  $w = New-Object -ComObject Word.Application",
      "  $new = @(Get-Process WINWORD -ErrorAction SilentlyContinue | Where-Object { $before -notcontains $_.Id })",
      "  if ($new.Count -gt 0) {",
      "    $ours = $true",
      "    ($new | ForEach-Object { $_.Id }) | Set-Content -Path $PidFile -Encoding ASCII",
      "    $w.Visible = $false",
      "    $w.DisplayAlerts = 0",
      "  }",
      "  $d = $w.Documents.Open($In, $false, $true, $false, $m, $m, $m, $m, $m, $m, $m, $false)",
      "  foreach ($t in $d.TablesOfContents) { $t.Update() }",
      "  $d.Fields.Update() | Out-Null",
      "  $d.ExportAsFixedFormat($Out, 17)",
      "  $d.Close(0)",
      "  $d = $null",
      "} catch {",
      "  [Console]::Error.WriteLine($_.Exception.Message)",
      "  exit 1",
      "} finally {",
      "  if ($d -ne $null) { try { $d.Close(0) } catch {} }",
      "  if ($w -ne $null) {",
      "    if ($ours) { try { $w.Quit() } catch {} }",
      "    [void][Runtime.InteropServices.Marshal]::ReleaseComObject($w)",
      "  }",
      "}"
    ),
    ps1
  )
  # suppressWarnings: a non-zero exit (or the 124 of a timeout) would leak a
  # localized warning even when the next backend succeeds; status is checked below.
  out <- suppressWarnings(system2(
    "powershell",
    c(
      "-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
      "-File", shQuote(ps1, type = "cmd"),
      "-In", shQuote(normalizePath(docx_file, winslash = "\\", mustWork = TRUE), type = "cmd"),
      "-Out", shQuote(normalizePath(tmp_pdf, winslash = "\\", mustWork = FALSE), type = "cmd"),
      "-PidFile", shQuote(normalizePath(pid_file, winslash = "\\", mustWork = FALSE), type = "cmd")
    ),
    stdout = TRUE,
    stderr = TRUE,
    timeout = 300
  ))
  status <- attr(out, "status")
  if ((!is.null(status) && status != 0) || !file.exists(tmp_pdf)) {
    cli::cli_abort(c(
      "Microsoft Word did not write a PDF for {.file {docx_file}}.",
      x = paste(.cli_escape(out), collapse = "\n")
    ))
  }
  if (!file.copy(tmp_pdf, pdf_file, overwrite = TRUE)) {
    cli::cli_abort("Could not move the converted PDF to {.file {pdf_file}}.")
  }
  invisible(pdf_file)
}

#' Kill the Word process a conversion started, if it is still alive
#'
#' Reads the process ids the PowerShell script recorded and ends each one that
#' is still a `WINWORD.EXE`, so a timed-out or failed conversion cannot leave an
#' invisible Word behind. A Word the user already had is never in the file.
#' @param pid_file Character. File of process ids, one per line; may not exist.
#' @return Invisibly `NULL`.
#' @keywords internal
.kill_created_word <- function(pid_file) {
  if (!file.exists(pid_file)) {
    return(invisible(NULL))
  }
  pids <- suppressWarnings(as.integer(readLines(pid_file, warn = FALSE)))
  for (pid in pids[!is.na(pids)]) {
    listing <- suppressWarnings(system2(
      "tasklist",
      c("/FI", shQuote(paste("PID eq", pid), type = "cmd"), "/FI", shQuote("IMAGENAME eq WINWORD.EXE", type = "cmd")),
      stdout = TRUE, stderr = TRUE
    ))
    if (any(grepl("WINWORD.EXE", listing, fixed = TRUE))) {
      tools::pskill(pid)
    }
  }
  invisible(NULL)
}

#' Run the TinyTeX DOCX -> PDF conversion
#'
#' Two steps rather than one: pandoc writes a standalone LaTeX document, then
#' [tinytex::latexmk()] compiles it. Going through `latexmk` is the whole point
#' -- it installs missing LaTeX packages on demand, so a minimal TinyTeX grows
#' whatever the document needs instead of aborting on a missing `.sty`.
#' @param docx_file Character. Existing `.docx` to convert.
#' @param pdf_file Character. Path of the `.pdf` to create.
#' @return Invisibly returns `pdf_file`.
#' @keywords internal
.tinytex_docx_to_pdf <- function(docx_file, pdf_file) {
  # Resolved before the setwd() below, so a relative target still lands in the
  # caller's working directory rather than in the scratch directory.
  target <- file.path(
    normalizePath(dirname(pdf_file), winslash = "/", mustWork = TRUE),
    basename(pdf_file)
  )
  work_dir <- tempfile("dta_tex_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  tex_file <- file.path(work_dir, "dta_document.tex")
  rmarkdown::pandoc_convert(
    input = normalizePath(docx_file, winslash = "/", mustWork = TRUE),
    to = "latex",
    output = normalizePath(tex_file, winslash = "/", mustWork = FALSE),
    options = c(
      "--standalone",
      "--extract-media", normalizePath(work_dir, winslash = "/", mustWork = TRUE)
    )
  )
  if (!file.exists(tex_file)) {
    cli::cli_abort("pandoc did not write the intermediate LaTeX document.")
  }

  # latexmk resolves \includegraphics paths relative to the working directory.
  old_wd <- setwd(work_dir)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  # pdflatex cannot typeset characters outside its font encodings -- the
  # rule translator's "≠", for one -- so a document using them is retried
  # with xelatex, which reads UTF-8 natively. pdflatex stays first because it
  # is the faster engine and every document it can compile is unchanged.
  # Only an encoding failure is worth the retry; any other failure is re-raised.
  produced <- tryCatch(
    tinytex::latexmk(basename(tex_file), engine = "pdflatex"),
    error = function(e) {
      log_file <- file.path(work_dir, sub("\\.tex$", ".log", basename(tex_file)))
      log_text <- if (file.exists(log_file)) {
        paste(readLines(log_file, warn = FALSE), collapse = "\n")
      } else {
        ""
      }
      if (!grepl("Unicode character|inputenc", paste(conditionMessage(e), log_text), useBytes = TRUE)) {
        rlang::cnd_signal(e)
      }
      unlink(list.files(work_dir, pattern = "\\.(aux|toc|out)$", full.names = TRUE))
      tryCatch(
        tinytex::latexmk(basename(tex_file), engine = "xelatex"),
        error = function(e2) {
          cli::cli_abort(c(
            "Neither pdflatex nor xelatex could compile the document.",
            i = "pdflatex: {conditionMessage(e)}",
            i = "xelatex: {conditionMessage(e2)}"
          ))
        }
      )
    }
  )

  produced <- file.path(work_dir, basename(produced))
  if (!file.exists(produced)) {
    cli::cli_abort("TinyTeX did not write a PDF for {.file {docx_file}}.")
  }
  if (!file.copy(produced, target, overwrite = TRUE)) {
    cli::cli_abort("Could not move the converted PDF to {.file {pdf_file}}.")
  }
  invisible(pdf_file)
}

#' Run the pandoc DOCX -> PDF conversion
#'
#' Thin wrapper around [rmarkdown::pandoc_convert()]. The PDF engine is resolved
#' by [.pandoc_pdf_engine()] and passed explicitly, so pandoc uses the engine we
#' verified rather than defaulting to a `pdflatex` that may not exist.
#' @param docx_file Character. Existing `.docx` to convert.
#' @param pdf_file Character. Path of the `.pdf` to create.
#' @return Invisibly returns `pdf_file`.
#' @keywords internal
.pandoc_docx_to_pdf <- function(docx_file, pdf_file) {
  engine <- .pandoc_pdf_engine()

  # pandoc resolves the engine through the PATH it inherits, so a TinyTeX that
  # this session cannot see must be put back on the PATH for the call.
  bin_dir <- .tinytex_bin_dir()
  if (nzchar(bin_dir) && !nzchar(Sys.which(engine))) {
    old_path <- Sys.getenv("PATH")
    Sys.setenv(PATH = paste(bin_dir, old_path, sep = .Platform$path.sep))
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  }

  rmarkdown::pandoc_convert(
    input = normalizePath(docx_file, winslash = "/", mustWork = TRUE),
    to = "pdf",
    output = normalizePath(pdf_file, winslash = "/", mustWork = FALSE),
    options = if (nzchar(engine)) c("--pdf-engine", engine) else NULL
  )
  invisible(pdf_file)
}

#' Does a file on disk begin with the `%PDF` signature?
#' @param path Character. File to inspect.
#' @return `TRUE` when the first four bytes are `%PDF`.
#' @keywords internal
.is_pdf_file <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  identical(readBin(con, what = "raw", n = 4L), charToRaw("%PDF"))
}

#' Escape braces in text that cli will interpolate
#'
#' External tools report errors containing `{` and `}`; cli would treat those as
#' interpolation markers and fail while formatting the abort.
#' @param x Character vector.
#' @return `x` with braces doubled.
#' @keywords internal
.cli_escape <- function(x) {
  gsub("\\}", "}}", gsub("\\{", "{{", as.character(x)))
}

#' Bullets naming the exact commands that make PDF export work
#'
#' Kept separate so the "no backend at all" abort stays actionable rather than
#' merely descriptive.
#' @return A named character vector of cli bullets.
#' @keywords internal
.pdf_no_backend_bullets <- function() {
  pandoc_but_no_engine <- requireNamespace("rmarkdown", quietly = TRUE) &&
    isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)) &&
    !nzchar(.pandoc_pdf_engine())

  bullets <- c(
    "Cannot export to PDF: no DOCX to PDF conversion backend is available.",
    x = paste(
      "{.pkg DTAtools} builds a Word document and converts it with Microsoft Word",
      "(Windows), LibreOffice, or pandoc plus a PDF engine. None was found."
    )
  )
  if (pandoc_but_no_engine) {
    bullets <- c(bullets, x = paste(
      "pandoc is installed, but pandoc cannot write a PDF on its own -",
      "it needs a separate PDF engine, and no engine was found."
    ))
  }
  c(
    bullets,
    i = "Easiest fix, from R and without administrator rights: {.run tinytex::install_tinytex()}.",
    i = "Or use a Windows machine with Microsoft Word installed.",
    i = "Or install LibreOffice ({.url https://www.libreoffice.org/}) so {.code soffice} is on the PATH.",
    i = "Check the result with {.run DTAtools::dta_pdf_backend()}.",
    i = "Or export with {.code format = \"docx\"} or {.code format = \"md\"}, which need no external tools."
  )
}

#' Convert a DOCX to a PDF, or fail loudly
#'
#' Tries every backend reported by [.pdf_backends_available()] in priority order
#' and returns as soon as one yields a genuine PDF. Each attempt is written to
#' its own staging file beside `pdf_file` (via [.dta_write_staged()]); the
#' destination is only ever touched by the final rename, once
#' [.is_pdf_file()] has accepted the staged output. A file already at
#' `pdf_file` -- valid or not -- is therefore left completely alone by every
#' failed attempt; only a successful conversion replaces it.
#'
#' @param docx_file Character. Existing `.docx` to convert.
#' @param pdf_file Character. Path of the `.pdf` to create.
#' @return Invisibly returns `pdf_file`.
#' @keywords internal
.convert_docx_to_pdf <- function(docx_file, pdf_file) {
  backends <- .pdf_backends_available()

  if (!.pdf_conversion_available() || length(backends) == 0) {
    cli::cli_abort(.pdf_no_backend_bullets())
  }

  failures <- character(0)
  for (backend in backends) {
    err <- tryCatch(
      {
        .dta_write_staged(
          pdf_file,
          write = function(stage) {
            switch(backend,
              word = .word_docx_to_pdf(docx_file, stage),
              libreoffice = .soffice_docx_to_pdf(docx_file, stage),
              tinytex = .tinytex_docx_to_pdf(docx_file, stage),
              .pandoc_docx_to_pdf(docx_file, stage)
            )
            invisible(NULL)
          },
          check = function(stage) {
            if (!.is_pdf_file(stage)) {
              cli::cli_abort("conversion ran but the output did not start with the %PDF signature")
            }
          }
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )

    if (is.null(err)) {
      return(invisible(pdf_file))
    }

    detail <- paste0(backend, ": ", .cli_escape(err))
    names(detail) <- "x"
    failures <- c(failures, detail)
  }

  cli::cli_abort(c(
    "PDF conversion failed; nothing was written to {.file {pdf_file}}.",
    failures,
    i = "Check the backend in use with {.run DTAtools::dta_pdf_backend()}.",
    i = "Install a PDF engine with {.run tinytex::install_tinytex()}, or export with {.code format = \"docx\"} instead."
  ))
}

#' @title Export Dataset Metadata as Professional Document
#' @description
#' Creates a professionally formatted DOCX, PDF, or Markdown document from a DTADataSet object,
#' including metadata, file specifications, column specifications with allowed values,
#' and validation rules in human-readable format.
#'
#' The Markdown output (`format = "md"`) is a complete GitHub-flavoured
#' Markdown document: a fact table, a linked table of contents and numbered
#' chapters (Approval & Signatures, Description, Files, Column
#' Specifications, Validation Rules; chapters without content are left out).
#' It is deterministic: UTF-8 with LF line endings and no time stamp.
#'
#' @import S7
#' @importFrom officer read_docx
#' @importFrom cli cli_abort cli_alert_success
#'
#' @param x A DTADataSet, DTADataSetTabular, or DTADataSetFile object
#' @param file Character. Output file path
#' @param format Character. Output format: "docx", "pdf", or "md". If NULL
#'   (the default), inferred from the file extension, falling back to "docx"
#'   when the file has none.
#' @param overwrite Logical. Whether to overwrite existing files. Default: FALSE.
#' @param include_signatures Logical. Whether to include the signature section.
#'   Default: TRUE. In every format the section appears only when
#'   `signature_list` is non-empty.
#' @param include_file_specs Logical. Whether to include file specifications.
#'   Default: TRUE. Applies to every format.
#' @param include_rules Logical. Whether to include validation rules
#'   (DTADataSetTabular only). Default: TRUE. Applies to every format.
#' @param signature_list List of signatories, each a list with `name`, `role`
#'   and optionally `organization` (or `org`) fields, or a data.frame with
#'   columns `Organization`, `Name`, `Role`.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @param document_name Optional character: what this kind of document is
#'   called, shown as the main title of the export; the dataset specification
#'   follows beneath it. A vector is joined with `" / "`. `NULL` (the default)
#'   uses "Data Transfer Agreement".
#' @param organisation,organisation_unit Optional single strings naming the
#'   organisation (and unit within it) issuing the document. When given they are
#'   shown on the title page (Word/PDF) or in the top header block (Markdown);
#'   the dataset document has no running header. `NULL` or empty (the default)
#'   shows nothing.
#'
#' @return Invisibly `NULL`. The document is written to `file`.
#'
#' @export
#' @examples
#' ds <- create_example_DTADataSetTabular()
#' out <- tempfile(fileext = ".docx")
#' write_dataset_metadata(ds, file = out, quiet = TRUE)
#' file.exists(out)
#' unlink(out)
write_dataset_metadata <- function(
  x,
  file,
  format = NULL,
  overwrite = FALSE,
  include_signatures = TRUE,
  include_file_specs = TRUE,
  include_rules = TRUE,
  signature_list = NULL,
  quiet = FALSE,
  document_name = NULL,
  organisation = NULL,
  organisation_unit = NULL
) {
  if (!inherits(x, c("DTAtools::DTADataSet", "DTAtools::DTADataSetTabular", "DTAtools::DTADataSetFile"))) {
    cli::cli_abort("'x' must be a DTADataSet, DTADataSetTabular, or DTADataSetFile object.")
  }
  .set_export_ctx(document_name, organisation, organisation_unit)

  if (is.null(format)) {
    format <- tolower(tools::file_ext(file))
    if (format == "") format <- "docx"
  }

  format <- tolower(format)

  if (!format %in% c("docx", "pdf", "md")) {
    cli::cli_abort("'format' must be 'docx', 'pdf', or 'md'.")
  }

  if (file.exists(file) && !overwrite) {
    cli::cli_abort("File '{file}' already exists. Set overwrite = TRUE to replace.")
  }

  if (format == "docx") {
    doc <- .write_dataset_docx(x, include_signatures, include_file_specs, include_rules, signature_list)
    .dta_write_staged(file, function(stage) print(doc, target = stage))
  } else if (format == "pdf") {
    temp_docx <- tempfile(fileext = ".docx")
    on.exit(unlink(temp_docx, force = TRUE), add = TRUE)
    doc <- .write_dataset_docx(x, include_signatures, include_file_specs, include_rules, signature_list)
    print(doc, target = temp_docx)
    .convert_docx_to_pdf(temp_docx, file)
  } else if (format == "md") {
    .write_dataset_markdown(
      x, file, include_signatures, signature_list,
      include_file_specs = include_file_specs, include_rules = include_rules
    )
  }

  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Document saved to {file}")
  }
  invisible(NULL)
}

#' @keywords internal
.write_dataset_docx <- function(dataset, include_signatures, include_file_specs, include_rules, signature_list) {
  doc <- officer::read_docx()

  # Title section
  doc <- .add_title_section(
    doc,
    title = .export_document_name(),
    subtitle = paste0("Dataset Specification: ", dta_dataset_heading(dataset)),
    date = Sys.Date(),
    version = dataset@template_version
  )

  # Approval signatures come before the descriptive content, matching write_dta().
  if (include_signatures) {
    doc <- .add_signature_section(doc, signature_list)
  }

  # Basic metadata
  metadata <- list("ID" = dataset@id)
  if (!is.null(dataset@name)) metadata[["Name"]] <- dataset@name
  if (!is.null(dataset@description)) metadata[["Description"]] <- dataset@description
  if (!is.null(dataset@template_source)) metadata[["Template Source"]] <- dataset@template_source
  if (!is.null(dataset@template_version)) metadata[["Template Version"]] <- dataset@template_version
  if (!is.null(dataset@template_date)) metadata[["Template Date"]] <- dataset@template_date

  doc <- .add_metadata_section(doc, "Dataset Information", metadata)

  # File specifications
  if (include_file_specs && length(dataset@files) > 0) {
    doc <- .add_file_specifications(doc, dataset@files)
  }

  # Column specifications (DTADataSetTabular only)
  if (inherits(dataset, "DTAtools::DTADataSetTabular")) {
    doc <- .add_dataset_specs_section(doc, dataset, include_rules = include_rules, heading_level = 2)
  }

  # Footer
  doc <- .add_footer_section(
    doc,
    version = dataset@template_version,
    template_version = dataset@template_version,
    document_date = Sys.Date()
  )

  doc
}

# The Markdown document of write_dataset_metadata(): the same frame as the DTA
# document -- head with a fact table, a linked "Contents" list, numbered
# chapters and a footer without any time stamp. The chapters are Approval &
# Signatures (only with signatories), Description, Files, Column
# Specifications and Validation Rules; `include_*` switch them as in Word.
#' @keywords internal
.write_dataset_markdown <- function(dataset, file, include_signatures = TRUE, signature_list = NULL,
                                    include_file_specs = TRUE, include_rules = TRUE) {
  heading <- dta_dataset_heading(dataset)
  parts <- .md_dataset_parts(
    dataset,
    include_file_specs = include_file_specs, include_rules = include_rules, guide = TRUE
  )
  sections <- c(
    .md_children(
      if (isTRUE(include_signatures)) {
        .md_section("Approval & Signatures", .md_signature_table(signature_list))
      },
      .md_section("Description", .md_paragraphs(dataset@description))
    ),
    parts$sections
  )

  head <- .md_compose(
    paste("#", .md_inline(.export_document_name())),
    paste0("**", .md_inline(paste0("Dataset Specification: ", heading)), "**"),
    .md_inline(.export_org_line()),
    .md_kv_table(.dataset_document_facts(dataset))
  )

  template <- if (!is.null(dataset@template_source) && nzchar(dataset@template_source)) {
    list(
      id = dataset@template_source,
      version = dataset@template_version %||% "",
      date = dataset@template_date
    )
  } else {
    list()
  }
  footer <- .md_footer(
    c(
      paste0("Dataset Specification: ", heading),
      if (!is.null(dataset@template_version) && nzchar(dataset@template_version)) {
        paste("Template version", dataset@template_version)
      },
      .format_document_date(dataset@template_date)
    ),
    .template_provenance_text(template)
  )

  .md_write(.md_render(head, sections, footer), file)
}

# Fact table of the dataset document: id, name, type and the template the
# dataset was taken from. Empty values are dropped.
.dataset_document_facts <- function(dataset) {
  text <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "" else as.character(x[[1]])
  facts <- c(
    `Dataset ID` = text(dataset@id),
    Name = text(dataset@name),
    Type = .title_case_field(text(dataset@type)),
    `Template source` = text(dataset@template_source),
    `Template version` = text(dataset@template_version),
    `Template date` = .format_document_date(dataset@template_date)
  )
  .pairs_df(facts[nzchar(facts)])
}

#' @title Alias: Export File Dataset Specifications
#' @description Convenience alias for write_dataset_metadata()
#' @param x A DTADataSet, DTADataSetTabular, or DTADataSetFile object
#' @param file Character. Output file path
#' @param ... Additional arguments passed on to \code{write_dataset_metadata()}
#'   (\code{format}, \code{overwrite}, \code{include_signatures},
#'   \code{include_file_specs}, \code{include_rules}, \code{signature_list},
#'   \code{quiet}, \code{document_name}, \code{organisation},
#'   \code{organisation_unit}). The `include_*` switches apply to every
#'   format, Markdown included.
#' @return Invisibly `NULL`, as [write_dataset_metadata()].
#' @export
#' @examples
#' ds <- create_example_DTADataSetTabular()
#' out <- tempfile(fileext = ".docx")
#' write_file_specification(ds, file = out, quiet = TRUE)
#' file.exists(out)
#' unlink(out)
write_file_specification <- function(x, file, ...) {
  write_dataset_metadata(x, file, ...)
}
