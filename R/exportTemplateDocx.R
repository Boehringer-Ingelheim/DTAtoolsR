#' @title Export a DTA Using a User-Provided Word Template
#' @description
#' Fills a user-authored Word (`.docx`) template by replacing placeholder
#' markers with values extracted from a [DTA] object. Placeholders use a
#' single-brace, upper-case convention (for example `{DTA_TITLE}` or
#' `{SUPPLIER_NAME}`) and may appear in the document body, headers, footers and
#' table cells.
#'
#' The template is treated as an Open Packaging Conventions (OPC) archive: it is
#' unzipped, the WordprocessingML text of each paragraph is substituted at the
#' XML level (which transparently escapes any special characters such as `&`,
#' `<` and `>`), and the parts are repackaged into a new `.docx`. Because
#' substitution operates on the concatenated text of a whole paragraph, it is
#' robust to Word splitting a single placeholder across multiple runs.
#'
#' @details
#' The following placeholders are populated automatically from the DTA metadata
#' and datasets. Any placeholder without a corresponding value is left untouched
#' and reported via a warning.
#'
#' \describe{
#'   \item{Agreement}{`{DTA_TITLE}`, `{DTA_VERSION}`, `{DTA_DATE}`, `{DTA_HEADER}`}
#'   \item{Supplier}{`{SUPPLIER_NAME}`, `{SUPPLIER_COUNTRY}`, `{SUPPLIER_ADDRESS}`,
#'     `{SUPPLIER_EMAIL}`, `{SUPPLIER_PHONE}`, `{SUPPLIER_CONTACTS}`}
#'   \item{Receiver}{`{RECEIVER_NAME}`, `{RECEIVER_COUNTRY}`, `{RECEIVER_ADDRESS}`,
#'     `{RECEIVER_EMAIL}`, `{RECEIVER_PHONE}`, `{RECEIVER_CONTACTS}`}
#'   \item{Transmission}{`{TRANSMISSION_TYPE}`, `{TRANSMISSION_FREQUENCY}`,
#'     `{TRANSMISSION_NOTIFICATION}`, `{TRANSMISSION_FIRST_TRANSFER}`,
#'     `{TRANSMISSION_LAST_TRANSFER}`, `{TEST_UPLOAD}`, `{BLINDED_TRANSFER}`}
#'   \item{Data content}{`{DATASET_COUNT}`, `{DATASET_NAMES}`, `{DATASET_TYPES}`,
#'     `{TOTAL_COLUMNS}`, `{TOTAL_RULES}`}
#'   \item{Process}{`{ERROR_HANDLING}`, `{AUTHORIZED_CORRECTIONS}`,
#'     `{SIGNATORIES}`, `{PROCESS_INFORMATION}`, `{VERSION_HISTORY}`,
#'     `{GENERATED_DATE}`}
#' }
#'
#' Additional or overriding values can be supplied through `variables`; names may
#' be given with or without the surrounding braces (`"DTA_TITLE"` and
#' `"{DTA_TITLE}"` are equivalent). User-supplied values take precedence over the
#' automatically extracted ones and may introduce entirely new placeholders.
#'
#' @param dta A [DTA] object.
#' @param template Character. Path to the template `.docx` file.
#' @param output Character. Path to the `.docx` file to create.
#' @param variables Optional named list of additional or overriding placeholder
#'   values. Names may include or omit the surrounding braces. Default: `NULL`.
#' @param quiet Logical. If `TRUE`, suppresses console output. Default: `FALSE`.
#' @param fallback Logical. If `TRUE` (default) and template processing fails,
#'   fall back to the standard programmatic document produced by [write_dta()].
#'   The substitution is signalled as a **warning** condition (unless `quiet =
#'   TRUE`), so it can be trapped programmatically with
#'   `withCallingHandlers(warning = )`. If `FALSE`, a failure is raised as an
#'   error.
#'
#' @return Invisibly returns the `output` path.
#'
#' @seealso [write_dta()], which accepts a `template` argument to route through
#'   this function.
#'
#' @importFrom cli cli_abort cli_alert_success cli_warn
#' @importFrom tools file_ext
#'
#' @examples
#' # Build a minimal template with officer, then fill it from a DTA.
#' template <- tempfile(fileext = ".docx")
#' doc <- officer::read_docx()
#' doc <- officer::body_add_par(doc, "Title: {DTA_TITLE}")
#' doc <- officer::body_add_par(doc, "Version: {DTA_VERSION}")
#' doc <- officer::body_add_par(doc, "Supplier: {SUPPLIER_NAME}")
#' print(doc, target = template)
#'
#' dta <- read_dta_from_yaml(
#'   system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
#' )
#' out <- tempfile(fileext = ".docx")
#' export_with_template(dta, template, out)
#'
#' unlink(c(template, out), force = TRUE)
#' @export
export_with_template <- function(
  dta,
  template,
  output,
  variables = NULL,
  quiet = FALSE,
  fallback = TRUE
) {
  if (!inherits(dta, "DTAtools::DTA")) {
    cli::cli_abort("{.arg dta} must be a DTA object.")
  }
  if (!is.character(template) || length(template) != 1L) {
    cli::cli_abort("{.arg template} must be a single file path.")
  }
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort("{.arg output} must be a single file path.")
  }
  if (!file.exists(template)) {
    cli::cli_abort("Template file not found: {.file {template}}")
  }
  if (tolower(tools::file_ext(template)) != "docx") {
    cli::cli_abort("{.arg template} must be a {.file .docx} file, not {.file {template}}.")
  }
  if (!is.null(variables) && (!is.list(variables) || is.null(names(variables)))) {
    cli::cli_abort("{.arg variables} must be a named list or NULL.")
  }

  dta_vars <- .extract_template_variables(dta)
  user_vars <- .normalize_template_variables(variables, dta = dta)
  all_vars <- utils::modifyList(dta_vars, user_vars)

  tryCatch(
    {
      .replace_docx_placeholders(template, all_vars, output)
      if (!isTRUE(quiet)) {
        cli::cli_alert_success("Document exported with template to {.file {output}}")
      }
      invisible(output)
    },
    error = function(e) {
      if (isTRUE(fallback)) {
        if (!isTRUE(quiet)) {
          # A real warning condition (not cli_alert_warning(), which only
          # signals a message): callers -- notably the Shiny app -- must be able
          # to trap the silent substitution of the built-in layout with
          # withCallingHandlers(warning = ) / tryCatch(warning = ).
          cli::cli_warn(c(
            "Template processing failed: {conditionMessage(e)}",
            i = "Falling back to the standard document format."
          ))
        }
        write_dta(dta, output, format = "docx", overwrite = TRUE, quiet = quiet)
        invisible(output)
      } else {
        cli::cli_abort("Template processing failed: {conditionMessage(e)}")
      }
    }
  )
}


#' Every placeholder a Word template may use, with a one-line description
#'
#' The single source of truth for which tokens exist.
#' [.extract_template_variables()] must produce exactly these names, and a test
#' asserts that it does -- otherwise the documented set and the implemented set
#' drift apart silently, which is the failure this catalogue exists to prevent.
#' @keywords internal
.tv_placeholder_catalog <- function() {
  c(
    "{DTA_TITLE}" = "Agreement title",
    "{DTA_VERSION}" = "Agreement version",
    "{DTA_DATE}" = "Agreement date (ISO 8601)",
    "{DTA_HEADER}" = "Document header line",
    "{SUPPLIER_NAME}" = "Supplier organisation name",
    "{SUPPLIER_COUNTRY}" = "Supplier country",
    "{SUPPLIER_ADDRESS}" = "Supplier address",
    "{SUPPLIER_EMAIL}" = "Email of the first supplier contact",
    "{SUPPLIER_PHONE}" = "Phone of the first supplier contact",
    "{SUPPLIER_CONTACTS}" = "Detailed supplier contact block",
    "{RECEIVER_NAME}" = "Receiver organisation name",
    "{RECEIVER_COUNTRY}" = "Receiver country",
    "{RECEIVER_ADDRESS}" = "Receiver address",
    "{RECEIVER_EMAIL}" = "Email of the first receiver contact",
    "{RECEIVER_PHONE}" = "Phone of the first receiver contact",
    "{RECEIVER_CONTACTS}" = "Detailed receiver contact block",
    "{TRANSMISSION_TYPE}" = "How the data is transferred",
    "{TRANSMISSION_FREQUENCY}" = "How often data is transferred",
    "{TRANSMISSION_NOTIFICATION}" = "How a transfer is notified",
    "{TRANSMISSION_FIRST_TRANSFER}" = "Date or phrase for the first transfer",
    "{TRANSMISSION_LAST_TRANSFER}" = "Date or phrase for the last transfer",
    "{TEST_UPLOAD}" = "Whether a test upload is required (Yes/No)",
    "{BLINDED_TRANSFER}" = "Whether the transfer is blinded (Yes/No)",
    "{DATASET_COUNT}" = "Number of datasets",
    "{DATASET_NAMES}" = "Names of all datasets",
    "{DATASET_TYPES}" = "Types of all datasets",
    "{TOTAL_COLUMNS}" = "Total column count across tabular datasets",
    "{TOTAL_RULES}" = "Total validation-rule count across tabular datasets",
    "{ERROR_HANDLING}" = "Agreed error-handling procedure",
    "{AUTHORIZED_CORRECTIONS}" = "Who may authorise corrections",
    "{SIGNATORIES}" = "Detailed signatory block with signature lines",
    "{PROCESS_INFORMATION}" = "Combined process-information block",
    "{VERSION_HISTORY}" = "Version history as a single line",
    "{GENERATED_DATE}" = "Date this document was generated (ISO 8601)"
  )
}


#' List the placeholders a Word template can use
#'
#' [export_with_template()] fills a fixed set of `{PLACEHOLDER}` markers from a
#' [DTA] object. Call this to discover which markers exist -- and, given a
#' `DTA`, what each one would currently expand to -- rather than exporting a
#' document to find out, or reading the list out of the documentation by hand.
#'
#' @param dta Optional [DTA] object. When supplied, the returned values are the
#'   resolved text for that object. When `NULL` (the default) they are short
#'   descriptions of what each placeholder means.
#'
#' @return A named character vector whose names are the brace-delimited
#'   placeholder tokens (`"{DTA_TITLE}"` and so on), in documentation order.
#'   The values are \describe{
#'     \item{descriptions}{when `dta` is `NULL`.}
#'     \item{the resolved text}{when `dta` is a [DTA].}
#'   }
#'
#' @seealso [export_with_template()], [write_dta()]
#' @export
#' @examples
#' # What can a template refer to?
#' head(dta_template_placeholders())
#'
#' # What would those become for a real DTA?
#' dta <- read_dta_from_yaml(
#'   system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
#' )
#' dta_template_placeholders(dta)[c("{DTA_TITLE}", "{SUPPLIER_NAME}")]
dta_template_placeholders <- function(dta = NULL) {
  catalog <- .tv_placeholder_catalog()
  if (is.null(dta)) {
    return(catalog)
  }
  if (!inherits(dta, "DTAtools::DTA")) {
    cli::cli_abort("{.arg dta} must be a DTA object or {.code NULL}.")
  }
  vars <- .extract_template_variables(dta)
  vapply(
    names(catalog),
    function(k) {
      v <- vars[[k]]
      if (is.null(v) || length(v) == 0) "" else as.character(v)[[1]]
    },
    character(1)
  )
}


#' Extract template placeholder values from a DTA object
#'
#' Builds the named list of `{PLACEHOLDER}` values consumed by
#' [export_with_template()]. All values are single-line character scalars.
#'
#' @param dta A [DTA] object.
#' @return A named list keyed by brace-delimited placeholder tokens.
#' @keywords internal
.extract_template_variables <- function(dta) {
  meta <- dta@metadata
  if (is.null(meta)) {
    meta <- DTAMetaData()
  }

  receiver <- meta@receiver
  supplier <- meta@supplier
  trans <- meta@transmission
  datasets <- dta@datasets

  tabular <- Filter(
    function(ds) inherits(ds, "DTAtools::DTADataSetTabular"),
    datasets
  )
  total_columns <- if (length(tabular) > 0) {
    sum(vapply(tabular, function(ds) length(ds@specs@columns), integer(1)))
  } else {
    0L
  }
  total_rules <- if (length(tabular) > 0) {
    sum(vapply(
      tabular,
      function(ds) {
        rules <- ds@specs@rules
        if (is.null(rules)) 0L else length(rules)
      },
      integer(1)
    ))
  } else {
    0L
  }

  version_history_str <- .tv_version_history(meta@version_history)
  auth <- .format_authorized_for_corrections_lines(meta@authorized_for_corrections)
  auth_str <- if (length(auth) > 0) paste(auth, collapse = ", ") else ""

  dataset_names <- if (length(datasets) > 0) {
    paste(names(datasets), collapse = ", ")
  } else {
    ""
  }
  dataset_types <- if (length(datasets) > 0) {
    paste(vapply(datasets, function(ds) ds@type, character(1)), collapse = ", ")
  } else {
    ""
  }

  list(
    "{DTA_TITLE}" = .tv_scalar(meta@title),
    "{DTA_VERSION}" = .tv_scalar(meta@version),
    "{DTA_DATE}" = .tv_scalar(meta@date),
    "{DTA_HEADER}" = .tv_scalar(meta@header),
    "{SUPPLIER_NAME}" = .tv_scalar(.tv_get(supplier, "affiliation", "name")),
    "{SUPPLIER_COUNTRY}" = .tv_scalar(.tv_get(supplier, "affiliation", "country")),
    "{SUPPLIER_ADDRESS}" = .tv_scalar(.tv_get(supplier, "affiliation", "address")),
    "{SUPPLIER_EMAIL}" = .tv_scalar(.tv_first_contact_field(supplier, "email")),
    "{SUPPLIER_PHONE}" = .tv_scalar(.tv_first_contact_field(supplier, "phone")),
    "{SUPPLIER_CONTACTS}" = .tv_contacts_block(supplier),
    "{RECEIVER_NAME}" = .tv_scalar(.tv_get(receiver, "affiliation", "name")),
    "{RECEIVER_COUNTRY}" = .tv_scalar(.tv_get(receiver, "affiliation", "country")),
    "{RECEIVER_ADDRESS}" = .tv_scalar(.tv_get(receiver, "affiliation", "address")),
    "{RECEIVER_EMAIL}" = .tv_scalar(.tv_first_contact_field(receiver, "email")),
    "{RECEIVER_PHONE}" = .tv_scalar(.tv_first_contact_field(receiver, "phone")),
    "{RECEIVER_CONTACTS}" = .tv_contacts_block(receiver),
    "{TRANSMISSION_TYPE}" = .tv_scalar(.tv_get(trans, "type")),
    "{TRANSMISSION_FREQUENCY}" = .tv_scalar(.tv_get(trans, "frequency")),
    "{TRANSMISSION_NOTIFICATION}" = .tv_scalar(.tv_get(trans, "notification")),
    "{TRANSMISSION_FIRST_TRANSFER}" = .tv_scalar(.tv_get(trans, "date_first_transfer")),
    "{TRANSMISSION_LAST_TRANSFER}" = .tv_scalar(.tv_get(trans, "date_last_transfer")),
    "{TEST_UPLOAD}" = if (isTRUE(.tv_get(trans, "test_upload"))) "Yes" else "No",
    "{BLINDED_TRANSFER}" = if (isTRUE(.tv_get(trans, "blinded_transfer"))) "Yes" else "No",
    "{DATASET_COUNT}" = as.character(length(datasets)),
    "{DATASET_NAMES}" = dataset_names,
    "{DATASET_TYPES}" = dataset_types,
    "{TOTAL_COLUMNS}" = as.character(total_columns),
    "{TOTAL_RULES}" = as.character(total_rules),
    "{ERROR_HANDLING}" = .tv_scalar(meta@error_handling),
    "{AUTHORIZED_CORRECTIONS}" = auth_str,
    "{SIGNATORIES}" = .tv_signatories_block(meta),
    "{PROCESS_INFORMATION}" = .tv_process_information_block(meta),
    "{VERSION_HISTORY}" = version_history_str,
    "{GENERATED_DATE}" = .format_document_date(Sys.Date())
  )
}


#' Coerce a metadata value to a single-line display string
#'
#' Dates render in ISO 8601 (`YYYY-MM-DD`) so a template filled on a German
#' workstation and one filled on an English CI runner produce identical text.
#' @keywords internal
.tv_scalar <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    out <- .format_document_date(x)
    return(if (nzchar(out)) out else default)
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "Yes" else "No")
  }
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(default)
  }
  paste(x, collapse = ", ")
}


#' Safely walk a nested list by a sequence of keys, returning NULL if any level
#' is missing.
#' @keywords internal
.tv_get <- function(lst, ...) {
  keys <- c(...)
  cur <- lst
  for (k in keys) {
    if (is.null(cur) || !is.list(cur)) {
      return(NULL)
    }
    cur <- cur[[k]]
  }
  cur
}


#' Return a field of the first contact record of an organization list
#' @keywords internal
.tv_first_contact_field <- function(org, field) {
  contacts <- .tv_get(org, "contacts")
  if (is.null(contacts) || length(contacts) == 0) {
    return(NULL)
  }
  first <- contacts[[1]]
  if (!is.list(first)) {
    return(NULL)
  }
  first[[field]]
}


#' Join the names of all contacts of an organization list
#' @keywords internal
.tv_contact_names <- function(org) {
  contacts <- .tv_get(org, "contacts")
  if (is.null(contacts) || length(contacts) == 0) {
    return("")
  }
  nms <- vapply(
    contacts,
    function(ct) {
      if (is.list(ct) && !is.null(ct$name)) as.character(ct$name)[1] else ""
    },
    character(1)
  )
  nms <- nms[nzchar(nms)]
  paste(nms, collapse = ", ")
}


#' Render a detailed contact block with signature lines for signers
#' @keywords internal
.tv_contacts_block <- function(org) {
  contacts <- .tv_get(org, "contacts")
  if (is.null(contacts) || length(contacts) == 0) {
    return("")
  }
  lines <- character(0)
  for (ct in contacts) {
    if (!is.list(ct)) {
      next
    }
    nm <- if (!is.null(ct$name) && nzchar(ct$name)) as.character(ct$name)[[1]] else "(Unnamed)"
    lines <- c(lines, paste0("- ", nm))
    if (!is.null(ct$role) && nzchar(ct$role)) lines <- c(lines, paste0("  - Role: ", ct$role))
    if (!is.null(ct$department) && nzchar(ct$department)) lines <- c(lines, paste0("  - Department: ", ct$department))
    if (!is.null(ct$email) && nzchar(ct$email)) lines <- c(lines, paste0("  - Email: ", ct$email))
    if (!is.null(ct$phone) && nzchar(ct$phone)) lines <- c(lines, paste0("  - Phone: ", ct$phone))
    if (isTRUE(ct$reviewer)) lines <- c(lines, "  - Reviewer: yes")
    if (isTRUE(ct$backup)) lines <- c(lines, "  - Backup: yes")
    if (isTRUE(ct$signature)) {
      lines <- c(lines, "  - Signature: ______________________________   Date: ______________")
    }
  }
  paste(lines, collapse = "\n")
}


#' Render signatories as a detailed block with signature lines
#' @keywords internal
.tv_signatories_block <- function(meta) {
  sig <- .extract_signatories(meta, signature_list = NULL)
  if (is.null(sig) || nrow(sig) == 0) {
    return("")
  }
  lines <- character(0)
  for (i in seq_len(nrow(sig))) {
    nm <- as.character(sig$Name[[i]])
    lines <- c(lines, paste0("- ", nm))
    if ("Organization" %in% names(sig) && nzchar(as.character(sig$Organization[[i]]))) {
      lines <- c(lines, paste0("  - Organization: ", as.character(sig$Organization[[i]])))
    }
    if ("Role" %in% names(sig) && nzchar(as.character(sig$Role[[i]]))) {
      lines <- c(lines, paste0("  - Role: ", as.character(sig$Role[[i]])))
    }
    if ("Department" %in% names(sig) && nzchar(as.character(sig$Department[[i]]))) {
      lines <- c(lines, paste0("  - Department: ", as.character(sig$Department[[i]])))
    }
    if ("Email" %in% names(sig) && nzchar(as.character(sig$Email[[i]]))) {
      lines <- c(lines, paste0("  - Email: ", as.character(sig$Email[[i]])))
    }
    if ("Phone" %in% names(sig) && nzchar(as.character(sig$Phone[[i]]))) {
      lines <- c(lines, paste0("  - Phone: ", as.character(sig$Phone[[i]])))
    }
    lines <- c(lines, "  - Signature: ______________________________   Date: ______________")
  }
  paste(lines, collapse = "\n")
}


#' Render process information block (transmission, handling, corrections)
#' @keywords internal
.tv_process_information_block <- function(meta) {
  lines <- character(0)
  trans <- meta@transmission
  if (!is.null(trans) && length(trans) > 0) {
    lines <- c(lines, "- Transmission")
    if (!is.null(trans$type) && nzchar(as.character(trans$type))) {
      lines <- c(lines, paste0("  - Type: ", as.character(trans$type)))
    }
    if (!is.null(trans$frequency) && nzchar(as.character(trans$frequency))) {
      lines <- c(lines, paste0("  - Frequency: ", as.character(trans$frequency)))
    }
    if (!is.null(trans$notification) && nzchar(as.character(trans$notification))) {
      lines <- c(lines, paste0("  - Notification: ", as.character(trans$notification)))
    }
    if (!is.null(trans$date_first_transfer) && nzchar(as.character(trans$date_first_transfer))) {
      lines <- c(lines, paste0("  - First transfer: ", as.character(trans$date_first_transfer)))
    }
    if (!is.null(trans$date_last_transfer) && nzchar(as.character(trans$date_last_transfer))) {
      lines <- c(lines, paste0("  - Last transfer: ", as.character(trans$date_last_transfer)))
    }
  }
  if (!is.null(meta@error_handling) && nzchar(as.character(meta@error_handling))) {
    lines <- c(lines, "- Error handling", paste0("  - ", as.character(meta@error_handling)))
  }
  auth <- .format_authorized_for_corrections_lines(meta@authorized_for_corrections)
  if (length(auth) > 0) {
    lines <- c(lines, "- Authorized for corrections")
    lines <- c(lines, paste0("  - ", auth))
  }
  sig <- .tv_signatories_block(meta)
  if (nzchar(sig)) {
    lines <- c(lines, "- Signatories", strsplit(sig, "\n", fixed = TRUE)[[1]])
  }
  paste(lines, collapse = "\n")
}


#' Summarize a version-history list as a single line
#' @keywords internal
.tv_version_history <- function(version_history) {
  if (is.null(version_history) || length(version_history) == 0) {
    return("")
  }
  parts <- vapply(
    version_history,
    function(v) {
      d <- v$date
      d <- if (inherits(d, "Date")) format(d, "%Y-%m-%d") else as.character(d)
      ver <- if (!is.null(v$version)) as.character(v$version) else ""
      if (length(d) == 0 || is.na(d)) paste0(ver) else paste0(ver, " (", d, ")")
    },
    character(1)
  )
  paste(parts, collapse = "; ")
}


#' Normalize a user-supplied variable list to brace-delimited character values
#' @keywords internal
.normalize_template_variables <- function(variables, dta = NULL) {
  if (is.null(variables) || length(variables) == 0) {
    return(list())
  }
  keys <- names(variables)
  keys <- vapply(
    keys,
    function(k) if (grepl("^\\{.*\\}$", k)) k else paste0("{", k, "}"),
    character(1)
  )
  vals <- lapply(seq_along(variables), function(i) {
    .tv_template_value(
      variables[[i]],
      dta = dta,
      markdown_cleanup = !.tv_needs_yaml_style(keys[[i]])
    )
  })
  names(vals) <- keys
  vals
}

#' Coerce a user-supplied template value while preserving line structure
#'
#' Unlike [.tv_scalar()], this keeps caller-provided line breaks and tabs so a
#' custom placeholder can intentionally render as a multi-line block.
#' @keywords internal
.tv_template_value <- function(x, default = "", markdown_cleanup = TRUE, dta = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    out <- .format_document_date(x)
    return(if (nzchar(out)) out else default)
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "Yes" else "No")
  }
  vals <- as.character(x)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(default)
  }
  vals <- gsub("\r\n", "\n", vals, fixed = TRUE)
  vals <- gsub("\r", "\n", vals, fixed = TRUE)
  if (isTRUE(markdown_cleanup)) {
    vals <- vapply(
      vals,
      function(v) .tv_template_markdown_to_text(v, dta = dta),
      character(1)
    )
  }
  if (length(vals) == 1) {
    return(vals[[1]])
  }
  paste(vals, collapse = "\n")
}


#' Convert markdown-like placeholder text to Word-friendly plain text
#'
#' Template placeholders are inserted as Word text runs, not a markdown parser.
#' Strip common markdown markers so headings/bold/lists do not render literally.
#' @keywords internal
.tv_template_markdown_to_text <- function(text, dta = NULL) {
  if (!.tv_looks_markdown(text)) {
    return(text)
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  out <- vapply(
    lines,
    function(ln) {
      x <- gsub("^\\s{0,3}#{1,6}\\s+", "", ln, perl = TRUE)
      x <- .tv_reformat_dataset_bullet(x, dta = dta)
      x <- .tv_markdown_bullet_to_word_bullet(x)
      x <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", x, perl = TRUE)
      x <- gsub("`([^`]+)`", "\\1", x, perl = TRUE)
      x
    },
    character(1)
  )
  paste(out, collapse = "\n")
}


#' Heuristic: does text look like markdown syntax?
#' @keywords internal
.tv_looks_markdown <- function(text) {
  grepl("(?m)^\\s{0,3}#{1,6}\\s+|\\*\\*[^*]+\\*\\*|(?m)^\\s*[-*+]\\s+", text, perl = TRUE)
}


#' Convert markdown list markers to visible Word bullet glyphs
#' @keywords internal
.tv_markdown_bullet_to_word_bullet <- function(line) {
  parts <- strsplit(line, "\n", fixed = TRUE)[[1]]
  parts <- vapply(
    parts,
    function(ln) {
      m <- regexec("^([ \t]*)([-*+])[ ]+(.*)$", ln, perl = TRUE)
      hit <- regmatches(ln, m)[[1]]
      if (length(hit) == 0) {
        return(ln)
      }

      indent_raw <- gsub("\t", "  ", hit[[2]], fixed = TRUE)
      indent_n <- nchar(indent_raw)
      level <- floor(indent_n / 2)
      symbol <- .tv_list_symbol_for_level(level)
      lead <- strrep(" ", level * 4)
      paste0(lead, symbol, " ", hit[[4]])
    },
    character(1)
  )
  paste(parts, collapse = "\n")
}


#' Bullet symbol by nesting level for Word-friendly visual hierarchy
#' @keywords internal
.tv_list_symbol_for_level <- function(level) {
  if (level <= 0) {
    return("\u2022")
  }
  if (level == 1) {
    return("\u25e6")
  }
  "\u25aa"
}


#' Expand dense dataset/rule bullets into a readable multiline block
#' @keywords internal
.tv_reformat_dataset_bullet <- function(line, dta = NULL) {
  body <- sub("^\\s*[-*+]\\s+", "", line, perl = TRUE)
  m <- regexec("^\\*\\*([^*]+)\\*\\*(.*)$", body, perl = TRUE)
  hits <- regmatches(body, m)[[1]]
  if (length(hits) == 0) {
    return(line)
  }
  if (length(hits) < 3) {
    return(line)
  }

  name <- sub(":\\s*$", "", trimws(hits[[2]]))
  tail <- trimws(hits[[3]])
  meta <- ""
  if (grepl("^\\[[^\\]]+\\]", tail, perl = TRUE)) {
    meta <- regmatches(tail, regexec("^(\\[[^\\]]+\\])", tail, perl = TRUE))[[1]][[2]]
    tail <- trimws(sub("^\\[[^\\]]+\\]", "", tail, perl = TRUE))
  }
  detail <- sub("^:\\s*", "", tail, perl = TRUE)
  if (length(detail) == 0) {
    return(line)
  }

  vals <- strsplit(detail, "\\|\\s*values\\s*:", perl = TRUE)[[1]]
  if (length(vals) == 0) {
    return(line)
  }
  desc <- trimws(vals[[1]])
  values <- if (length(vals) > 1) trimws(paste(vals[-1], collapse = "| values: ")) else ""

  header <- paste0("- ", name)
  extra <- character(0)
  if (nzchar(meta)) {
    if (nzchar(desc)) {
      extra <- c(extra, paste0("  - Description: ", desc))
    }
    meta_info <- .tv_parse_column_meta(meta)
    if (nzchar(meta_info$type)) {
      extra <- c(extra, paste0("  - Type: ", meta_info$type))
    }
    if (nzchar(meta_info$nullable)) {
      extra <- c(extra, paste0("  - Nullable: ", meta_info$nullable))
    }
    if (nzchar(meta_info$length)) {
      extra <- c(extra, paste0("  - Length: ", meta_info$length))
    }
    if (nzchar(values)) {
      value_items <- trimws(unlist(strsplit(values, ",", fixed = TRUE)))
      value_items <- value_items[nzchar(value_items)]
      if (length(value_items) > 0) {
        extra <- c(extra, "  - Values:")
        extra <- c(extra, paste0("    - ", value_items))
      } else {
        extra <- c(extra, paste0("  - Values: ", values))
      }
    }
    return(paste(c(header, extra), collapse = "\n"))
  }

  if (.tv_is_group_condition_summary(desc)) {
    extra <- .tv_expand_group_condition_summary(desc, dta = dta, rule_id = name)
  } else if (nzchar(desc)) {
    extra <- c(extra, paste0("  - ", desc))
  }

  paste(c(header, extra), collapse = "\n")
}


#' Parse the bracket metadata of a column bullet into named fields
#' @keywords internal
.tv_parse_column_meta <- function(meta) {
  raw <- gsub("^\\[|\\]$", "", trimws(meta))
  parts <- trimws(unlist(strsplit(raw, ",", fixed = TRUE)))
  parts <- parts[nzchar(parts)]

  out <- list(type = "", nullable = "", length = "")
  if (length(parts) == 0) {
    return(out)
  }

  is_nullable <- grepl("^(nullable|not null)$", parts, ignore.case = TRUE)
  is_length <- grepl("^length\\s+", parts, ignore.case = TRUE)
  type_parts <- parts[!(is_nullable | is_length)]
  if (length(type_parts) > 0) {
    out$type <- paste(type_parts, collapse = ", ")
  }

  nullable_part <- parts[is_nullable]
  if (length(nullable_part) > 0) {
    tok <- tolower(nullable_part[[1]])
    out$nullable <- if (identical(tok, "not null")) "no" else "yes"
  }

  length_part <- parts[is_length]
  if (length(length_part) > 0) {
    out$length <- trimws(sub("^length\\s+", "", length_part[[1]], ignore.case = TRUE))
  }

  out
}


#' Does a rule description look like group_condition summary output?
#' @keywords internal
.tv_is_group_condition_summary <- function(text) {
  grepl("^group\\([^\\)]+\\):\\s*[0-9]+\\s*condition\\(s\\),\\s*[0-9]+\\s*constraint\\(s\\)", text)
}


#' Expand group_condition summary into a clearer premise-oriented outline
#' @keywords internal
.tv_expand_group_condition_summary <- function(text, dta = NULL, rule_id = "") {
  m <- regexec(
    "^group\\(([^\\)]+)\\):\\s*([0-9]+)\\s*condition\\(s\\),\\s*([0-9]+)\\s*constraint\\(s\\)\\s*[\u2014-]?\\s*(.*)$",
    text,
    perl = TRUE
  )
  hits <- regmatches(text, m)[[1]]
  if (length(hits) == 0) {
    return(paste0("  - ", text))
  }

  group_by <- trimws(hits[[2]])
  n_cond <- trimws(hits[[3]])
  n_constr <- trimws(hits[[4]])
  note <- trimws(hits[[5]])

  out <- c(
    paste0("  - Grouped by: ", group_by),
    "  - Conditions:",
    "    - See detailed condition definitions below.",
    "  - Constraints:",
    "    - See detailed constraint definitions below.",
    "  - Premise:",
    paste0("    - Rows are grouped by ", group_by, "."),
    "    - Condition checks are evaluated within each group.",
    "    - The listed constraints must hold for the same grouped rows."
  )
  if (nzchar(note)) {
    out <- c(out, paste0("  - Context: ", note))
  }
  detailed <- .tv_expand_group_condition_from_dta(dta, rule_id)
  if (length(detailed) > 0) {
    out <- c(out, detailed)
  }
  out
}


#' Build detailed group_condition rule breakdown from the DTA rule object
#' @keywords internal
.tv_expand_group_condition_from_dta <- function(dta, rule_id) {
  if (is.null(dta) || !inherits(dta, "DTAtools::DTA") || !nzchar(rule_id)) {
    return(character(0))
  }
  rule <- .tv_find_group_rule(dta, rule_id)
  if (is.null(rule)) {
    return(character(0))
  }

  out <- "  - Detailed rule definition:"

  conds <- rule@conditions
  out <- c(out, "    - Conditions:")
  for (nm in names(conds)) {
    out <- c(out, paste0("      - ", nm, ": ", .tv_condition_to_text(conds[[nm]])))
  }

  csts <- rule@constraints
  out <- c(out, "    - Constraints:")
  for (cst in csts) {
    out <- c(out, paste0("      - ", .tv_constraint_to_text(cst)))
  }

  out
}


#' Find a group_condition rule object by id within a DTA
#' @keywords internal
.tv_find_group_rule <- function(dta, rule_id) {
  for (ds in dta@datasets) {
    if (!inherits(ds, "DTAtools::DTADataSetTabular")) {
      next
    }
    rules <- ds@specs@rules
    if (is.null(rules) || length(rules) == 0) {
      next
    }
    for (r in rules) {
      if (is.null(r)) {
        next
      }
      rid <- tryCatch(as.character(r@id), error = function(e) "")
      rtype <- tryCatch(as.character(r@type), error = function(e) "")
      if (identical(rid, rule_id) && rtype %in% c("check_group_condition", "group_condition")) {
        return(r)
      }
    }
  }
  NULL
}


#' Render one named condition map to plain text
#' @keywords internal
.tv_condition_to_text <- function(cond) {
  if (is.null(cond) || !is.list(cond) || length(cond) == 0) {
    return("no condition details")
  }
  col_parts <- vapply(
    names(cond),
    function(col) {
      checks <- cond[[col]]
      if (!is.list(checks) || length(checks) == 0) {
        return(col)
      }
      check_parts <- vapply(
        names(checks),
        function(op) {
          val <- checks[[op]]
          val_txt <- if (length(val) > 1) {
            paste(as.character(val), collapse = ", ")
          } else {
            as.character(val)[[1]]
          }
          paste(col, op, val_txt)
        },
        character(1)
      )
      paste(check_parts, collapse = " AND ")
    },
    character(1)
  )
  paste(col_parts, collapse = " AND ")
}


#' Render one group_condition constraint to plain text
#' @keywords internal
.tv_constraint_to_text <- function(cst) {
  if (!is.list(cst) || is.null(cst$type)) {
    return("unknown constraint")
  }
  ctype <- as.character(cst$type)
  if (identical(ctype, "mutually_exclusive")) {
    left <- cst$left %||% "?"
    right <- cst$right %||% "?"
    ls <- cst$left_scope %||% "any"
    rs <- cst$right_scope %||% "any"
    core <- paste0(
      "mutually_exclusive: ", left, " (scope=", ls, ") and ",
      right, " (scope=", rs, ") must not both hold"
    )
  } else if (identical(ctype, "requires")) {
    ifn <- cst[["if"]] %||% "?"
    thn <- cst[["then"]] %||% "?"
    ifs <- cst$if_scope %||% "any"
    ths <- cst$then_scope %||% "any"
    core <- paste0(
      "requires: if ", ifn, " (scope=", ifs, ") then ",
      thn, " (scope=", ths, ")"
    )
  } else {
    core <- paste0("constraint type ", ctype)
  }
  if (!is.null(cst$message) && nzchar(cst$message)) {
    paste0(core, " \u2014 ", cst$message)
  } else {
    core
  }
}


#' Replace placeholders in a DOCX template and write a new DOCX
#'
#' Unzips `template_path`, substitutes placeholders in the main document part and
#' any header/footer parts, repackages the archive to `output_path`, and verifies
#' the result opens as a valid DOCX.
#'
#' @param template_path Character. Path to the template `.docx`.
#' @param variables Named list of brace-delimited placeholder values.
#' @param output_path Character. Path to the `.docx` to create.
#' @return Invisibly returns `output_path`.
#' @keywords internal
.replace_docx_placeholders <- function(template_path, variables, output_path) {
  # No requireNamespace() guards here: xml2, zip and officer are all hard
  # Imports, so they cannot be missing at this point.
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) {
    cli::cli_abort("Output directory does not exist: {.file {out_dir}}")
  }

  temp_dir <- tempfile("dta_template_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  extracted <- tryCatch(
    utils::unzip(template_path, exdir = temp_dir),
    warning = function(w) character(0),
    error = function(e) character(0)
  )
  if (length(extracted) == 0) {
    cli::cli_abort("Template is not a readable DOCX (ZIP) archive: {.file {template_path}}")
  }

  word_dir <- file.path(temp_dir, "word")
  parts <- character(0)
  main <- file.path(word_dir, "document.xml")
  if (file.exists(main)) {
    parts <- c(parts, main)
  }
  parts <- c(
    parts,
    list.files(
      word_dir,
      pattern = "^(header|footer)[0-9]*\\.xml$",
      full.names = TRUE
    )
  )

  if (length(parts) == 0) {
    cli::cli_abort("Template does not contain a Word document part ({.file word/document.xml}).")
  }

  unresolved <- character(0)
  for (part in parts) {
    unresolved <- c(unresolved, .replace_placeholders_in_xml(part, variables))
  }
  unresolved <- unique(unresolved)
  if (length(unresolved) > 0) {
    # Double braces so cli/glue renders the tokens literally instead of trying
    # to interpolate e.g. `{NOT_A_FIELD}` as an R expression.
    items <- gsub("}", "}}", gsub("{", "{{", unresolved, fixed = TRUE), fixed = TRUE)
    names(items) <- rep("*", length(items))
    cli::cli_warn(c(
      "!" = "Some template placeholders had no matching value and were left unchanged:",
      items
    ))
  }

  .zip_docx_dir(temp_dir, output_path)

  valid <- tryCatch(
    {
      officer::read_docx(output_path)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!isTRUE(valid)) {
    cli::cli_abort("The generated document is not a valid DOCX: {.file {output_path}}")
  }

  invisible(output_path)
}


#' Substitute placeholders within a single WordprocessingML XML part
#'
#' Operates paragraph by paragraph. Within each paragraph the text of all runs is
#' concatenated before substitution so that placeholders split across runs are
#' still matched; the replacement text is written to the first run and the
#' remaining runs of that paragraph are blanked. Setting text through `xml2`
#' escapes XML special characters automatically.
#'
#' @param xml_path Character. Path to the XML part to modify in place.
#' @param variables Named list of brace-delimited placeholder values.
#' @return Character vector of unresolved placeholder tokens found in the part.
#' @keywords internal
.replace_placeholders_in_xml <- function(xml_path, variables) {
  doc <- xml2::read_xml(xml_path)

  # Namespace-agnostic XPath: match by local element name so we do not depend on
  # the 'w' prefix being declared in a particular way.
  paras <- xml2::xml_find_all(doc, ".//*[local-name()='p']")
  unresolved <- character(0)

  for (p in paras) {
    t_nodes <- xml2::xml_find_all(
      p,
      ".//*[local-name()='r']/*[local-name()='t']"
    )
    if (length(t_nodes) == 0) {
      next
    }
    texts <- xml2::xml_text(t_nodes)
    combined <- paste0(texts, collapse = "")
    if (!.tv_has_candidate(combined, variables)) {
      next
    }

    if (.tv_spans_runs(combined, texts, variables)) {
      # A placeholder crosses a run boundary, so it can only be matched against
      # the joined text. Writing the result into the first run and blanking the
      # rest loses this paragraph's per-run formatting -- accepted here because
      # it is the only way to resolve the placeholder at all.
      res <- .substitute_placeholder_text(combined, variables)
      unresolved <- c(unresolved, res$unresolved)
      if (identical(res$text, combined)) {
        next
      }
      run_node <- xml2::xml_parent(t_nodes[[1]])
      .tv_set_run_text(t_nodes[[1]], res$text)
      if (.tv_should_apply_yaml_style(combined, res$replaced_keys)) {
        .tv_set_yaml_run_style(run_node)
      }
      for (i in seq_along(t_nodes)[-1]) {
        .tv_set_run_text(t_nodes[[i]], "")
      }
      next
    }

    # Every placeholder sits inside a single run, so substitute run by run and
    # leave each run's w:rPr -- its bold, italic, colour, size -- untouched.
    for (i in seq_along(t_nodes)) {
      res <- .substitute_placeholder_text(texts[[i]], variables)
      unresolved <- c(unresolved, res$unresolved)
      if (identical(res$text, texts[[i]])) {
        next
      }
      run_node <- xml2::xml_parent(t_nodes[[i]])
      .tv_set_run_text(t_nodes[[i]], res$text)
      if (.tv_should_apply_yaml_style(texts[[i]], res$replaced_keys)) {
        .tv_set_yaml_run_style(run_node)
      }
    }
  }

  xml2::write_xml(doc, xml_path)
  unique(unresolved)
}


#' The canonical placeholder-token grammar
#'
#' Read by both the "is this paragraph worth rewriting" gate and the "which
#' tokens had no value" report, so the two cannot drift apart. They previously
#' did: the gate matched `[A-Za-z0-9_]+` while the leftover scan matched
#' `[A-Z][A-Z0-9_]*`, so a mixed-case token such as `{customField}` was left
#' untouched *and* never reported, silently breaking the documented warning
#' contract.
#' @keywords internal
.tv_token_pattern <- function() "\\{[A-Za-z_][A-Za-z0-9_]*\\}"


#' Is there anything in this text that could possibly be substituted?
#'
#' A fast skip for paragraphs with no placeholder at all. Both halves matter:
#' user-supplied `variables` names are arbitrary and need not fit the token
#' grammar, so a literal key match has to count too.
#' @keywords internal
.tv_has_candidate <- function(text, variables) {
  if (grepl(.tv_token_pattern(), text)) {
    return(TRUE)
  }
  keys <- names(variables)
  length(keys) > 0 &&
    any(vapply(keys, function(k) grepl(k, text, fixed = TRUE), logical(1)))
}


#' Escape every regex metacharacter in a literal string
#'
#' Deliberately character-by-character rather than a regex of its own. Both
#' obvious alternatives are broken here: `\Q...\E` quoting stops at the first
#' literal `\E` in a caller-supplied name, silently dropping that key from the
#' match set; and the usual
#' `gsub("([\\^$.|?*+()\\[\\]{}])", ...)` spelling is itself a pattern with `{}`
#' inside a character class, which TRE rejects outright. This version has
#' neither failure mode, and `variables` names are arbitrary so neither can be
#' ruled out upstream.
#' @keywords internal
.tv_escape_regex <- function(x) {
  meta <- c(
    "\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}"
  )
  vapply(
    strsplit(x, "", fixed = TRUE),
    function(chars) {
      paste0(ifelse(chars %in% meta, paste0("\\", chars), chars), collapse = "")
    },
    character(1)
  )
}


#' Locate every substitutable span in a string
#'
#' The single source of the match set, shared by the substituter and by the
#' run-splitting check, so the two always agree on what counts as a placeholder.
#' The set is "any known key" *or* "anything token-shaped": known keys come
#' first so an exact variable name wins over the generic grammar, and so that
#' keys which do not fit the grammar still match.
#'
#' @param text Character scalar to scan.
#' @param variables Named list of brace-delimited placeholder values.
#' @return A list with integer vectors `start` and `length`, both empty when
#'   nothing matches.
#' @keywords internal
.tv_placeholder_matches <- function(text, variables) {
  # A zero-length name could never be a token, and would contribute an empty
  # alternative that matches at every position.
  keys <- names(variables)
  keys <- keys[nzchar(keys)]
  pattern <- paste(
    c(
      if (length(keys) > 0) .tv_escape_regex(keys),
      .tv_token_pattern()
    ),
    collapse = "|"
  )
  m <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (length(m) == 1L && m[[1]] == -1L) {
    return(list(start = integer(0), length = integer(0)))
  }
  list(start = as.integer(m), length = attr(m, "match.length"))
}


#' Does any placeholder in this paragraph straddle a run boundary?
#'
#' Run-local substitution is only safe when every match sits wholly inside a
#' single run. Word splits a typed placeholder across runs freely (spell-check
#' state, revision ids, a stray formatting toggle), which is the whole reason
#' the joined fallback exists.
#'
#' @param combined Character scalar: the concatenated text of the paragraph.
#' @param texts Character vector: the text of each run, in order.
#' @param variables Named list of brace-delimited placeholder values.
#' @return `TRUE` if at least one match crosses a run boundary.
#' @keywords internal
.tv_spans_runs <- function(combined, texts, variables) {
  m <- .tv_placeholder_matches(combined, variables)
  if (length(m$start) == 0) {
    return(FALSE)
  }
  ends <- cumsum(nchar(texts))
  run_of <- function(pos) {
    idx <- which(pos <= ends)
    if (length(idx) == 0) length(ends) else idx[[1]]
  }
  any(vapply(
    seq_along(m$start),
    function(i) {
      first <- m$start[[i]]
      last <- first + m$length[[i]] - 1L
      run_of(first) != run_of(last)
    },
    logical(1)
  ))
}


#' Replace known placeholders in a text string and report unresolved tokens
#'
#' One left-to-right pass over the **original** text. Every match is either
#' mapped to its value or emitted verbatim, and the result is accumulated in a
#' separate buffer, so a substituted value is never rescanned. That is what
#' stops a value which itself contains braces -- a title like
#' `"Study {A} vs {B}"` -- from being re-substituted or falsely reported as an
#' unresolved placeholder the template never contained.
#'
#' The match set is "any known key" *or* "anything token-shaped". Known keys
#' come first so that an exact variable name wins over the generic grammar, and
#' so that keys which do not fit the grammar still match.
#'
#' @param text Character scalar to substitute into.
#' @param variables Named list of brace-delimited placeholder values.
#' @return A list with `text` (the substituted string) and `unresolved` (unique
#'   token-shaped matches that had no value).
#' @keywords internal
.substitute_placeholder_text <- function(text, variables) {
  keys <- names(variables)
  m <- .tv_placeholder_matches(text, variables)
  if (length(m$start) == 0) {
    return(list(
      text = text,
      unresolved = character(0),
      replaced_keys = character(0)
    ))
  }
  starts <- m$start
  lens <- m$length

  out <- character(0)
  unresolved <- character(0)
  replaced_keys <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    start <- starts[[i]]
    end <- start + lens[[i]] - 1L
    token <- substr(text, start, end)
    if (start > pos) {
      out <- c(out, substr(text, pos, start - 1L))
    }
    if (token %in% keys) {
      value <- variables[[token]]
      out <- c(out, if (length(value) == 0) "" else as.character(value)[[1]])
      replaced_keys <- c(replaced_keys, token)
    } else {
      out <- c(out, token)
      unresolved <- c(unresolved, token)
    }
    pos <- end + 1L
  }
  if (pos <= nchar(text)) {
    out <- c(out, substr(text, pos, nchar(text)))
  }

  list(
    text = paste0(out, collapse = ""),
    unresolved = unique(unresolved),
    replaced_keys = unique(replaced_keys)
  )
}


#' Set xml:space="preserve" on a text node, ignoring failures
#' @keywords internal
.tv_set_preserve_space <- function(node) {
  tryCatch(
    xml2::xml_set_attr(node, "xml:space", "preserve"),
    error = function(e) NULL
  )
  invisible(node)
}


#' Set run content from text, preserving tabs/newlines as Word elements
#' @keywords internal
.tv_set_run_text <- function(t_node, text) {
  run <- xml2::xml_parent(t_node)
  if (is.null(run)) {
    return(invisible(NULL))
  }

  kids <- xml2::xml_children(run)
  rpr_idx <- which(vapply(
    kids,
    function(ch) grepl("(^|:)rPr$", xml2::xml_name(ch)),
    logical(1)
  ))
  if (length(kids) > 0) {
    if (length(rpr_idx) > 0) {
      drop_idx <- setdiff(seq_along(kids), rpr_idx[[1]])
      if (length(drop_idx) > 0) {
        xml2::xml_remove(kids[drop_idx])
      }
    } else {
      xml2::xml_remove(kids)
    }
  }

  chunks <- .tv_split_word_chunks(text)
  if (length(chunks) == 0) {
    chunks <- list(list(type = "text", value = ""))
  }

  for (chunk in chunks) {
    if (identical(chunk$type, "break")) {
      xml2::xml_add_child(run, "w:br")
      next
    }
    if (identical(chunk$type, "tab")) {
      xml2::xml_add_child(run, "w:tab")
      next
    }
    t_new <- xml2::xml_add_child(run, "w:t", chunk$value)
    .tv_set_preserve_space(t_new)
  }

  invisible(run)
}


#' Split text into WordprocessingML chunks (text, line break, tab)
#' @keywords internal
.tv_split_word_chunks <- function(text) {
  if (length(text) == 0 || is.null(text) || is.na(text)) {
    return(list(list(type = "text", value = "")))
  }
  text <- as.character(text)[[1]]
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)

  marks <- gregexpr("[\n\t]", text, perl = TRUE)[[1]]
  if (length(marks) == 1L && marks[[1]] == -1L) {
    return(list(list(type = "text", value = text)))
  }

  chunks <- list()
  pos <- 1L
  for (idx in seq_along(marks)) {
    at <- marks[[idx]]
    if (at > pos) {
      chunks <- c(chunks, list(list(
        type = "text",
        value = substr(text, pos, at - 1L)
      )))
    }
    marker <- substr(text, at, at)
    if (identical(marker, "\n")) {
      chunks <- c(chunks, list(list(type = "break", value = "")))
    } else {
      chunks <- c(chunks, list(list(type = "tab", value = "")))
    }
    pos <- at + 1L
  }

  if (pos <= nchar(text)) {
    chunks <- c(chunks, list(list(
      type = "text",
      value = substr(text, pos, nchar(text))
    )))
  }

  chunks
}


#' Should a substituted placeholder be styled as embedded YAML code?
#' @keywords internal
.tv_needs_yaml_style <- function(keys) {
  if (length(keys) == 0) {
    return(FALSE)
  }
  any(grepl("yaml", keys, ignore.case = TRUE))
}


#' Apply YAML style only when the whole source run was the YAML placeholder
#' @keywords internal
.tv_should_apply_yaml_style <- function(source_text, keys) {
  if (length(keys) != 1) {
    return(FALSE)
  }
  key <- keys[[1]]
  .tv_needs_yaml_style(key) && identical(trimws(source_text), key)
}


#' Force small monospace style on a run (for embedded YAML placeholders)
#'
#' Matches the built-in embedded YAML section styling (small, monospace).
#' @keywords internal
.tv_set_yaml_run_style <- function(run_node) {
  if (is.null(run_node) || length(run_node) == 0) {
    return(invisible(NULL))
  }
  rpr <- .tv_ensure_run_props(run_node)
  .tv_set_or_add_rpr_child(rpr, "w:rFonts", c(
    "w:ascii" = "Consolas",
    "w:hAnsi" = "Consolas",
    "w:cs" = "Consolas"
  ))
  .tv_set_or_add_rpr_child(rpr, "w:sz", c("w:val" = "12"))
  .tv_set_or_add_rpr_child(rpr, "w:szCs", c("w:val" = "12"))
  invisible(run_node)
}


#' Ensure a run has a rPr child and return it
#' @keywords internal
.tv_ensure_run_props <- function(run_node) {
  kids <- xml2::xml_children(run_node)
  rpr_idx <- which(vapply(
    kids,
    function(ch) grepl("(^|:)rPr$", xml2::xml_name(ch)),
    logical(1)
  ))
  if (length(rpr_idx) > 0) {
    return(kids[[rpr_idx[[1]]]])
  }
  xml2::xml_add_child(run_node, "w:rPr", .where = 0)
}


#' Upsert a single run-property child element with attributes
#' @keywords internal
.tv_set_or_add_rpr_child <- function(rpr_node, child_name, attrs) {
  kids <- xml2::xml_children(rpr_node)
  idx <- which(vapply(
    kids,
    function(ch) grepl(paste0("(^|:)", sub("^w:", "", child_name), "$"), xml2::xml_name(ch)),
    logical(1)
  ))
  node <- if (length(idx) > 0) kids[[idx[[1]]]] else xml2::xml_add_child(rpr_node, child_name)
  for (nm in names(attrs)) {
    xml2::xml_set_attr(node, nm, attrs[[nm]])
  }
  invisible(node)
}


#' Repackage a directory tree as a DOCX (ZIP) archive
#'
#' Uses the `zip` package so no external `zip` executable is required, which
#' matters on locked-down systems. Hidden files such as `_rels/.rels` are
#' included, and paths are stored relative to `source_dir`.
#'
#' @param source_dir Character. Directory whose contents become the archive root.
#' @param output_path Character. Path to the archive to create.
#' @return Invisibly returns `output_path`.
#' @keywords internal
.zip_docx_dir <- function(source_dir, output_path) {
  files <- list.files(
    source_dir,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (length(files) == 0) {
    cli::cli_abort("No files to package into a DOCX archive.")
  }
  if (file.exists(output_path)) {
    unlink(output_path)
  }
  zip::zip(
    zipfile = output_path,
    files = files,
    root = source_dir,
    include_directories = FALSE
  )
  invisible(output_path)
}
