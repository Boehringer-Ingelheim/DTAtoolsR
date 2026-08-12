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
  include_yaml = FALSE,
  yaml_text = NULL,
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
    print(doc, target = file)
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

#' @keywords internal
.write_dta_docx <- function(dta, include_signatures, signature_list,
                            include_yaml = FALSE, yaml_text = NULL) {
  # Open the bundled reference template so headings 1-4 auto-number as
  # 1 / 1.1 / 1.1.1 / 1.1.1.1 (true Word fields that renumber if edited).
  doc <- .new_numbered_docx()

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

  # Single top-level chapter that anchors the multilevel heading numbering, so
  # every section below renders as 1.x (and datasets as 1.x.y.z).
  doc <- .add_heading(doc, "Data Transfer Agreement", level = 1)

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

      # <chapter>.<section>.<dataset>.1 Files
      doc <- .add_file_specifications(doc, dataset@files, title = "Files", heading_level = 4)

      # <chapter>.<section>.<dataset>.2 Dataset Specifications
      # (Column Specifications + Validation Rules nested as bold subheadings).
      doc <- .add_heading(doc, "Dataset Specifications", level = 4)
      doc <- .add_dataset_specs_section(doc, dataset, include_rules = TRUE, heading_level = NULL)
    }
  }

  # Embedded machine-readable YAML, appended as a final small-font section.
  if (isTRUE(include_yaml)) {
    has_yaml <- !is.null(yaml_text) &&
      length(yaml_text) > 0 &&
      !anyNA(yaml_text) &&
      any(nzchar(yaml_text))
    if (has_yaml) {
      doc <- .add_embedded_yaml_section(doc, yaml_text)
    } else {
      cli::cli_warn(c(
        "{.arg include_yaml} is {.code TRUE} but {.arg yaml_text} is empty; no specification was embedded.",
        i = "Pass the YAML specification via {.arg yaml_text} to embed it."
      ))
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
#' LibreOffice comes first: it renders the DOCX the way Word does, preserving
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
#'   empty. One or more of `"libreoffice"`, `"tinytex"` and `"pandoc"`.
#' @keywords internal
.pdf_backends_available <- function() {
  backends <- character(0)
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
#' Three backends are supported, tried in this order:
#'
#' * **LibreOffice** -- a `soffice` binary on the `PATH` or in a standard
#'   install location. Preferred, because it renders the document as Word does
#'   and preserves table shading, column widths and heading numbering.
#' * **TinyTeX** -- pandoc converts the DOCX to LaTeX and [tinytex::latexmk()]
#'   compiles it, installing any missing LaTeX packages on demand.
#' * **pandoc** -- pandoc driving some other PDF engine already on the `PATH`
#'   (a full LaTeX distribution, `typst`, `wkhtmltopdf`, ...).
#'
#' If none is present, install a LaTeX engine from R with
#' `tinytex::install_tinytex()`; it needs no administrator rights and works on
#' Windows, macOS and Linux.
#'
#' @return `NULL` when no backend is available. Otherwise a list with:
#'   \describe{
#'     \item{`name`}{`"libreoffice"`, `"tinytex"` or `"pandoc"`.}
#'     \item{`engine`}{The `soffice` path, the TinyTeX binary directory, or the
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

  system2(
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
  )

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
  produced <- tinytex::latexmk(basename(tex_file), engine = "pdflatex")

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
      "{.pkg DTAtools} builds a Word document and converts it with LibreOffice,",
      "or with pandoc plus a PDF engine. Neither was found."
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
    i = "Or install LibreOffice ({.url https://www.libreoffice.org/}) so {.code soffice} is on the PATH.",
    i = "Check the result with {.run DTAtools::dta_pdf_backend()}.",
    i = "Or export with {.code format = \"docx\"} or {.code format = \"md\"}, which need no external tools."
  )
}

#' Convert a DOCX to a PDF, or fail loudly
#'
#' Tries every backend reported by [.pdf_backends_available()] in priority order
#' and returns as soon as one yields a genuine PDF. Produces a real PDF or
#' aborts: a DOCX is never renamed to `.pdf`, and no file is ever written to a
#' path other than the requested `pdf_file`. Any partial output is removed
#' before the abort so the caller is not left with a misnamed or truncated
#' document.
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
        switch(backend,
          libreoffice = .soffice_docx_to_pdf(docx_file, pdf_file),
          tinytex = .tinytex_docx_to_pdf(docx_file, pdf_file),
          .pandoc_docx_to_pdf(docx_file, pdf_file)
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )

    if (is.null(err) && .is_pdf_file(pdf_file)) {
      return(invisible(pdf_file))
    }

    # Never leave a failed or non-PDF artefact at the requested path.
    unlink(pdf_file, force = TRUE)
    detail <- paste0(
      backend, ": ",
      if (is.null(err)) {
        "conversion ran but the output did not start with the %PDF signature"
      } else {
        .cli_escape(err)
      }
    )
    names(detail) <- "x"
    failures <- c(failures, detail)
  }

  cli::cli_abort(c(
    "PDF conversion failed; {.file {pdf_file}} was not created.",
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
    on.exit(unlink(temp_docx, force = TRUE), add = TRUE)
    doc <- .write_dataset_docx(x, include_signatures, include_file_specs, include_rules, signature_list)
    print(doc, target = temp_docx)
    .convert_docx_to_pdf(temp_docx, file)
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
