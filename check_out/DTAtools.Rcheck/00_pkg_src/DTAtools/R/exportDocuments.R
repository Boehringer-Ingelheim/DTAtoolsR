#' @title Export DTA Metadata as Professional Document
#' @description
#' Creates a professionally formatted DOCX, PDF, or Markdown document from a DTA object,
#' including metadata, version history, transmission details, and approval signatures.
#'
#' @import S7
#' @importFrom officer read_docx body_add_par body_add_break
#' @importFrom flextable flextable body_add_flextable
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom tools file_ext
#'
#' @param x A DTA object to export
#' @param file Character. Output file path (with .docx, .pdf, or .md extension)
#' @param format Character. Output format: "docx" (default), "pdf", or "md".
#'   If NULL, inferred from file extension.
#' @param overwrite Logical. Whether to overwrite existing files. Default: FALSE.
#' @param include_signatures Logical. Whether to include signature approval section. Default: TRUE.
#' @param signature_list List of signatories with name and role fields.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @param template Optional character path to a Word (`.docx`) template
#'   containing placeholder markers such as `{DTA_TITLE}`. When supplied, the
#'   document is produced by filling the template via [export_with_template()]
#'   instead of the built-in layout. Only valid for `format = "docx"` or
#'   `"pdf"`. Default: `NULL`.
#' @param template_variables Optional named list of additional or overriding
#'   placeholder values passed to [export_with_template()] when `template` is
#'   supplied. Default: `NULL`.
#'
#' @return Invisibly returns the document object (for chaining), or the output
#'   file path when a `template` is used.
#'
#' @seealso [export_with_template()] for the full list of supported placeholders.
#'
#' @export
#' @examples
#' \dontrun{
#' dta <- DTA(
#'   title = "Clinical Data Transfer",
#'   version = "1.0",
#'   date = Sys.Date()
#' )
#' write_dta(dta, file = "dta_metadata.docx")
#'
#' # Fill a user-authored Word template instead of the built-in layout
#' write_dta(dta, file = "dta_from_template.docx", template = "my_template.docx")
#' }
write_dta <- function(
  x,
  file,
  format = NULL,
  overwrite = FALSE,
  include_signatures = TRUE,
  signature_list = NULL,
  quiet = FALSE,
  template = NULL,
  template_variables = NULL
) {
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("'x' must be a DTA object.")
  }

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
    target_docx <- if (format == "docx") file else tempfile(fileext = ".docx")
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
      unlink(target_docx)
    }
    if (!isTRUE(quiet)) {
      cli::cli_alert_success("Document saved to {file}")
    }
    return(invisible(file))
  }

  # Create document
  doc <- NULL
  if (format == "docx") {
    doc <- .write_dta_docx(x, include_signatures, signature_list)
    print(doc, target = file)
  } else if (format == "pdf") {
    # First create DOCX, then convert to PDF
    temp_docx <- tempfile(fileext = ".docx")
    doc <- .write_dta_docx(x, include_signatures, signature_list)
    print(doc, target = temp_docx)
    .convert_docx_to_pdf(temp_docx, file)
    unlink(temp_docx)
  } else if (format == "md") {
    .write_dta_markdown(x, file, include_signatures, signature_list)
  }

  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Document saved to {file}")
  }
  invisible(doc)
}

#' @keywords internal
.write_dta_docx <- function(dta, include_signatures, signature_list) {
  doc <- officer::read_docx()

  # Extract metadata - note that DTA object stores metadata, not direct properties
  meta <- dta@metadata

  # Title section
  doc <- .add_title_section(
    doc,
    title = meta@title,
    subtitle = "Data Transfer Agreement Metadata",
    date = if (!is.null(meta@date)) meta@date else Sys.Date(),
    version = meta@version
  )

  # Metadata overview
  metadata <- list()
  if (!is.null(meta@header)) metadata[["Header"]] <- meta@header
  if (!is.null(meta@version)) metadata[["Version"]] <- meta@version
  if (!is.null(meta@date)) metadata[["Date"]] <- as.character(meta@date)

  doc <- .add_metadata_section(doc, "Document Information", metadata)

  # Version history
  if (length(meta@version_history) > 0) {
    doc <- .add_heading(doc, "Version History", level = 2)

    version_data <- data.frame(
      Version = character(),
      Date = character(),
      Changes = character(),
      stringsAsFactors = FALSE
    )

    for (vh in meta@version_history) {
      version_data <- rbind(version_data, data.frame(
        Version = vh$version,
        Date = if (inherits(vh$date, "Date")) as.character(vh$date) else vh$date,
        Changes = vh$changes,
        stringsAsFactors = FALSE
      ))
    }

    ft <- flextable::flextable(version_data)
    ft <- flextable::width(ft, j = 1, width = 1.0)
    ft <- flextable::width(ft, j = 2, width = 1.2)
    ft <- flextable::width(ft, j = 3, width = 4.0)
    ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
    ft <- flextable::bold(ft, part = "header")

    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  # Transmission details
  if (length(meta@transmission) > 0) {
    doc <- .add_heading(doc, "Transmission Details", level = 2)
    trans_meta <- as.list(meta@transmission)
    doc <- .add_metadata_section(doc, "", trans_meta)
  }

  # Error handling
  if (!is.null(meta@error_handling)) {
    doc <- .add_heading(doc, "Error Handling", level = 2)
    doc <- officer::body_add_par(doc, meta@error_handling, style = "Normal")
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  # Receiver and Supplier information (affiliation + individually listed contacts)
  if (length(meta@receiver) > 0) {
    doc <- .add_organization_section(doc, "Receiver Information", meta@receiver)
  }

  if (length(meta@supplier) > 0) {
    doc <- .add_organization_section(doc, "Supplier Information", meta@supplier)
  }

  # Authorized for corrections
  doc <- .add_authorized_for_corrections_section(doc, meta@authorized_for_corrections)

  # Datasets: file specifications, column specifications, and validation rules per dataset
  if (length(dta@datasets) > 0) {
    doc <- .add_heading(doc, "Datasets", level = 2)

    for (ds_name in names(dta@datasets)) {
      dataset <- dta@datasets[[ds_name]]

      doc <- .add_heading(doc, ds_name, level = 3)
      if (!is.null(dataset@description) && nzchar(dataset@description)) {
        doc <- officer::body_add_par(doc, dataset@description, style = "Normal")
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }

      if (length(dataset@files) > 0) {
        doc <- .add_file_specifications(doc, dataset@files, heading_level = NULL)
      }

      doc <- .add_dataset_specs_section(doc, dataset, include_rules = TRUE, heading_level = NULL)
    }
  }

  # Footer
  doc <- .add_footer_section(
    doc,
    version = meta@version,
    template_version = NULL,
    document_date = if (!is.null(meta@date)) meta@date else Sys.Date()
  )

  doc
}

#' @keywords internal
.write_dta_markdown <- function(dta, file, include_signatures = TRUE, signature_list = NULL) {
  meta <- dta@metadata

  lines <- c(
    "# Data Transfer Agreement Metadata",
    "",
    paste("**Title:**", meta@title),
    if (!is.null(meta@version)) paste("**Version:**", meta@version) else "",
    if (!is.null(meta@date)) paste("**Date:**", format(meta@date, "%Y-%m-%d")) else "",
    if (!is.null(meta@header)) paste("**Header:**", meta@header) else "",
    ""
  )

  # Document Information (mirrors the DOCX "Document Information" table)
  doc_info <- list()
  if (!is.null(meta@header)) doc_info[["Header"]] <- meta@header
  if (!is.null(meta@version)) doc_info[["Version"]] <- meta@version
  if (!is.null(meta@date)) doc_info[["Date"]] <- meta@date
  if (length(doc_info) > 0) {
    lines <- c(lines, "## Document Information", "")
    doc_info_df <- .format_metadata_pairs(doc_info)
    names(doc_info_df) <- c("Item", "Details")
    lines <- c(lines, .df_to_md_table(doc_info_df), "")
  }

  if (length(meta@version_history) > 0) {
    lines <- c(lines, "## Version History", "")
    vh_df <- get_version_history_df(meta)
    vh_df$date <- format(vh_df$date, "%Y-%m-%d")
    names(vh_df) <- c("Version", "Date", "Changes")
    lines <- c(lines, .df_to_md_table(vh_df), "")
  }

  if (length(meta@transmission) > 0) {
    lines <- c(lines, "## Transmission Details", "")
    trans_pairs <- as.list(meta@transmission)
    lines <- c(lines, .kv_bullets_md(trans_pairs), "")
  }

  if (!is.null(meta@error_handling)) {
    lines <- c(lines, "## Error Handling", "", meta@error_handling, "")
  }

  # Receiver and Supplier information (affiliation + individually listed contacts)
  lines <- c(lines, .organization_to_md_lines("Receiver Information", meta@receiver))
  lines <- c(lines, .organization_to_md_lines("Supplier Information", meta@supplier))

  # Authorized for corrections
  auth_names <- .format_authorized_for_corrections_lines(meta@authorized_for_corrections)
  if (length(auth_names) > 0) {
    lines <- c(
      lines, "## Authorized for Corrections", "",
      paste0("- ", auth_names), ""
    )
  }

  # Approval & Signatures section is intentionally omitted from write_dta():
  # individual contacts already carry inline signature lines in the
  # Receiver / Supplier sections above. Datasets section follows.

  # Datasets: file specifications, column specifications, and validation rules per dataset
  if (length(dta@datasets) > 0) {
    lines <- c(lines, "## Datasets", "")

    for (ds_name in names(dta@datasets)) {
      dataset <- dta@datasets[[ds_name]]

      lines <- c(lines, paste("###", ds_name), "")
      if (!is.null(dataset@description) && nzchar(dataset@description)) {
        lines <- c(lines, dataset@description, "")
      }

      lines <- c(lines, .file_specs_to_md_lines(dataset@files, heading = "#### File Specifications"))
      lines <- c(lines, .dataset_specs_to_md_lines(dataset, include_rules = TRUE, base_level = 4))
    }
  }

  lines <- c(
    lines,
    "",
    "---",
    paste("*Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "*")
  )

  writeLines(lines, con = file)
}

#' Render an organization block (affiliation + individual contact blocks) as Markdown lines.
#' Signatories come first with a signature underline; backup-only contacts are grouped last.
#' @keywords internal
.organization_to_md_lines <- function(title, org_list) {
  if (is.null(org_list) || length(org_list) == 0) {
    return(character(0))
  }

  lines <- c(paste0("## ", title), "")

  pairs <- .affiliation_pairs(org_list$affiliation)
  if (length(pairs) > 0) {
    lines <- c(lines, .kv_bullets_md(pairs), "")
  }

  contacts <- org_list$contacts
  if (!is.null(contacts) && length(contacts) > 0) {
    lines <- c(lines, "### Contacts", "")
    lines <- c(lines, .contacts_to_md_lines(contacts, heading_level = 4))
  }

  lines
}

#' Convert DOCX to PDF using pandoc or similar
#' @keywords internal
.convert_docx_to_pdf <- function(docx_file, pdf_file) {
  # Try using rmarkdown, which wraps the pandoc binary bundled with R/RStudio
  tryCatch(
    {
      if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
        rmarkdown::pandoc_convert(input = docx_file, to = "pdf", output = pdf_file)
      } else {
        # Fallback: just copy DOCX as is and warn user
        cli::cli_warn("PDF conversion not available. Saving as DOCX instead.")
        file.copy(docx_file, pdf_file, overwrite = TRUE)
      }
    },
    error = function(e) {
      cli::cli_warn("Could not convert to PDF: {e$message}. Saving as DOCX instead.")
      file.copy(docx_file, gsub("\\.pdf$", ".docx", pdf_file), overwrite = TRUE)
    }
  )
}

#' @title Export Dataset Metadata as Professional Document
#' @description
#' Creates a professionally formatted DOCX, PDF, or Markdown document from a DTADataSet object,
#' including metadata, file specifications, column specifications with allowed values,
#' and validation rules in human-readable format.
#'
#' @import S7
#' @importFrom officer read_docx
#' @importFrom cli cli_abort cli_alert_success
#'
#' @param x A DTADataSet, DTADataSetTabular, or DTADataSetFile object
#' @param file Character. Output file path
#' @param format Character. Output format: "docx" (default), "pdf", or "md"
#' @param overwrite Logical. Whether to overwrite existing files. Default: FALSE.
#' @param include_signatures Logical. Whether to include signature section. Default: TRUE.
#' @param include_file_specs Logical. Whether to include file specifications. Default: TRUE.
#' @param include_rules Logical. Whether to include validation rules (DTADataSetTabular only). Default: TRUE.
#' @param signature_list List of signatories
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#'
#' @return Invisibly returns the document object
#'
#' @export
#' @examples
#' \dontrun{
#' ds <- DTADataSetTabular(
#'   name = "example_dataset",
#'   specs = create_example_DTAColumnSpecCollection(1)
#' )
#' write_dataset_metadata(ds, file = "dataset_spec.docx")
#' }
write_dataset_metadata <- function(
  x,
  file,
  format = NULL,
  overwrite = FALSE,
  include_signatures = TRUE,
  include_file_specs = TRUE,
  include_rules = TRUE,
  signature_list = NULL,
  quiet = FALSE
) {
  if (!inherits(x, c("DTAtools::DTADataSet", "DTAtools::DTADataSetTabular", "DTAtools::DTADataSetFile"))) {
    cli::cli_abort("'x' must be a DTADataSet, DTADataSetTabular, or DTADataSetFile object.")
  }

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
    print(doc, target = file)
  } else if (format == "pdf") {
    temp_docx <- tempfile(fileext = ".docx")
    doc <- .write_dataset_docx(x, include_signatures, include_file_specs, include_rules, signature_list)
    print(doc, target = temp_docx)
    .convert_docx_to_pdf(temp_docx, file)
    unlink(temp_docx)
  } else if (format == "md") {
    .write_dataset_markdown(x, file, include_signatures, signature_list)
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
    title = paste0("Dataset Specification: ", dataset@name),
    subtitle = "Data Transfer Agreement - Dataset Metadata",
    date = Sys.Date(),
    version = dataset@template_version
  )

  # Basic metadata
  metadata <- list()
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

  # Approval signatures
  if (include_signatures) {
    doc <- .add_signature_section(doc, signature_list)
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

#' @keywords internal
.write_dataset_markdown <- function(dataset, file, include_signatures = TRUE, signature_list = NULL) {
  lines <- c(
    "# Dataset Specification",
    paste("## ", dataset@name),
    "",
    "### Overview",
    if (!is.null(dataset@description)) paste("**Description:**", dataset@description) else "",
    if (!is.null(dataset@template_source)) paste("**Template Source:**", dataset@template_source) else "",
    if (!is.null(dataset@template_version)) paste("**Template Version:**", dataset@template_version) else "",
    ""
  )

  lines <- c(lines, .file_specs_to_md_lines(dataset@files))

  # Column specs + validation rules for tabular datasets
  lines <- c(lines, .dataset_specs_to_md_lines(dataset, include_rules = TRUE, base_level = 3))

  # Approval & Signatures
  if (include_signatures) {
    sig_df <- .normalize_signatories(signature_list)
    lines <- c(lines, "### Approval & Signatures", "")
    if (!is.null(sig_df) && nrow(sig_df) > 0) {
      sig_df$Signature <- "_________________"
      sig_df$Date <- "____________"
      lines <- c(lines, .df_to_md_table(sig_df), "")
    } else {
      lines <- c(
        lines,
        "Approved by: _____________________________     Date: ______________", "",
        "Signature:   _____________________________", ""
      )
    }
  }

  lines <- c(
    lines,
    "---",
    paste("*Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "*")
  )

  writeLines(lines, con = file)
}

#' @title Alias: Export File Dataset Specifications
#' @description Convenience alias for write_dataset_metadata()
#' @param x A DTADataSet, DTADataSetTabular, or DTADataSetFile object
#' @param file Character. Output file path
#' @param ... Additional arguments passed on to \code{write_dataset_metadata()}
#'   (\code{format}, \code{overwrite}, \code{include_signatures},
#'   \code{include_file_specs}, \code{include_rules}, \code{signature_list}).
#' @return Invisibly returns the document object
#' @export
write_file_specification <- function(x, file, ...) {
  write_dataset_metadata(x, file, ...)
}
