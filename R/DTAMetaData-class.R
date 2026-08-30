#' @title DTAMetaData Class
#' @description This class holds comprehensive metadata information for Data Transfer Agreements,
#' including version history, transmission details, error handling, and contact information.
#' @import S7
#' @export
#'
#' @param title title (optional; a DTA may carry datasets without metadata)
#' @param version current version string
#' @param date current date
#' @param header header/organization name
#' @param version_history list of version records; each record must be a list with:
#'   \itemize{
#'     \item \code{version}: version string (e.g. "1.0")
#'     \item \code{date}: Date object for this version
#'     \item \code{changes}: character description of what was changed in this version
#'   }
#' @param receiver list with affiliation and contacts (authorized to request corrections)
#' @param supplier list with affiliation and contacts
#' @param transmission list with type, frequency, notification, dates (first/last transfer), and flags
#' @param error_handling character description of error handling procedures
#' @param authorized_for_corrections character or list indicating BI contact(s) authorized to request corrections
#' @param template A machine-owned provenance record written by the template
#'   engine that generated this document, e.g. \code{list(id = "...", version = "...")}.
#'   Never set by a specification author. When non-empty, \code{id} and
#'   \code{version} are each required to be a single non-empty character.
#' @return An object of class DTAMetaData.
#'
#' @details
#' Date-valued metadata fields (\code{date}, \code{transmission$date_first_transfer}
#' and \code{transmission$date_last_transfer}) may be supplied as strings. A string
#' that is exactly an ISO date becomes a \code{Date}. A string that has no ISO date
#' at its start (\code{"after approval"}, \code{"2 weeks after approval"},
#' \code{"final transfer by 2026-12-31"}) is a legitimate free-text phrase and is
#' kept verbatim as character.
#'
#' A string that *starts* with an ISO date but carries trailing text
#' (\code{"2026-12-31 at the earliest"}) is the dangerous case: \code{as.Date()}
#' silently discards the trailing words, turning a qualified statement in a data
#' transfer agreement into a committed date. Such a value is still converted to the
#' \code{Date} - \code{validate_transmission_dates()} and the exported documents
#' need a real \code{Date} - but the loss is recorded as an import issue in
#' \code{@import_issues}, carrying the original string verbatim. Import issues make
#' \code{check()} on the enclosing \code{DTA} fail, so the discarded qualification
#' can never pass silently.
#'
#' @examples
#'
#' DTAMetaData(title = "Clinical Data Transfer", version = "1.0")
DTAMetaData <- S7::new_class(
  "DTAMetaData",
  constructor = function(
    title = NULL,
    version = NULL,
    date = NULL,
    header = NULL,
    version_history = list(),
    receiver = list(),
    supplier = list(),
    transmission = list(),
    error_handling = NULL,
    authorized_for_corrections = NULL,
    template = list()
  ) {
    import_issues <- list()

    if (is.character(date)) {
      parsed <- .parse_metadata_date(date, field = "date", require_date = TRUE)
      date <- parsed$value
      import_issues <- c(import_issues, parsed$issues)
    }

    # Process transmission dates: convert character dates to Date, keep phrases
    # as-is, and record an import issue for any date that had to drop trailing
    # text to become a Date.
    if (length(transmission) > 0) {
      processed <- .process_transmission_dates(transmission)
      transmission <- processed$transmission
      import_issues <- c(import_issues, processed$issues)
    }

    new_object(
      S7_object(),
      title = title,
      version = version,
      date = date,
      header = header,
      version_history = version_history,
      receiver = receiver,
      supplier = supplier,
      transmission = transmission,
      error_handling = error_handling,
      authorized_for_corrections = authorized_for_corrections,
      template = template,
      import_issues = import_issues
    )
  },
  properties = list(
    title = class_character_or_null,
    version = class_character_or_null,
    date = class_Date_or_null,
    header = class_character_or_null,
    version_history = class_list,
    receiver = class_list,
    supplier = class_list,
    transmission = class_list,
    error_handling = class_character_or_null,
    authorized_for_corrections = class_character_or_list_or_null,
    template = S7::new_property(S7::class_list, default = list()),
    import_issues = S7::new_property(S7::class_list, default = list())
  ),
  validator = function(self) {
    errors <- c()

    if (!is.null(self@version) && self@version == "") {
      errors <- c(errors, "'version' cannot be an empty string.")
    }
    if (!is.null(self@title) && self@title == "") {
      errors <- c(errors, "'title' cannot be an empty string.")
    }

    # Validate version_history structure
    if (length(self@version_history) > 0) {
      for (i in seq_along(self@version_history)) {
        hist <- self@version_history[[i]]
        if (!is.list(hist)) {
          errors <- c(errors, paste0("version_history[[", i, "]] must be a list."))
        } else {
          required_fields <- c("version", "date", "changes")
          missing_fields <- setdiff(required_fields, names(hist))
          if (length(missing_fields) > 0) {
            errors <- c(errors, paste0(
              "version_history[[", i, "]] missing required fields: ",
              paste(missing_fields, collapse = ", "),
              ". Each version record must have: version (string), date (Date), changes (description)."
            ))
          }
          # Validate that changes is not empty
          if (!is.null(hist$changes) && is.character(hist$changes) && nchar(hist$changes) == 0) {
            errors <- c(errors, paste0("version_history[[", i, "]]$changes cannot be an empty string."))
          }
        }
      }
    }

    # Validate transmission structure if provided
    if (length(self@transmission) > 0) {
      # date_first_transfer and date_last_transfer can be either Date or character
      if (!is.null(self@transmission$date_first_transfer)) {
        val <- self@transmission$date_first_transfer
        if (!is.character(val) && !inherits(val, "Date")) {
          errors <- c(errors, "transmission$date_first_transfer must be a Date or character string.")
        }
      }
      if (!is.null(self@transmission$date_last_transfer)) {
        val <- self@transmission$date_last_transfer
        if (!is.character(val) && !inherits(val, "Date")) {
          errors <- c(errors, "transmission$date_last_transfer must be a Date or character string.")
        }
      }
    }

    # Validate template provenance: a partially written record is worse than
    # none, because the rebase feature trusts it. When present, `id` and
    # `version` must each be a single non-empty character.
    if (length(self@template) > 0) {
      tid <- self@template$id
      tversion <- self@template$version
      if (!is.character(tid) || length(tid) != 1 || !nzchar(tid)) {
        errors <- c(errors, "template$id must be a single non-empty character string.")
      }
      if (!is.character(tversion) || length(tversion) != 1 || !nzchar(tversion)) {
        errors <- c(errors, "template$version must be a single non-empty character string.")
      }
    }

    if (length(errors) > 0) {
      paste(errors, collapse = "\n")
    }
  }
)


#' @title Split an ISO Date Prefix from a String
#' @description
#' Returns the leading \code{YYYY-MM-DD} date of a string together with whatever
#' text follows it. Unlike \code{as.Date(x, format = "\%Y-\%m-\%d")}, which parses
#' the prefix and throws the rest away without a word, the trailing text is
#' handed back so the caller can decide what to do with it.
#' @param value A single character string.
#' @return A list with \code{date} (a \code{Date}, or \code{NA} when the string
#'   does not start with a real calendar date) and \code{residue} (the trimmed
#'   trailing text, \code{""} when the string is exactly a date).
#' @keywords internal
.split_date_prefix <- function(value) {
  no_date <- list(date = as.Date(NA), residue = NA_character_)

  matched <- regmatches(
    value,
    regexec("^\\s*(\\d{4}-\\d{2}-\\d{2})(.*)$", value)
  )[[1]]

  if (length(matched) == 0) {
    return(no_date)
  }

  parsed <- suppressWarnings(as.Date(matched[2], format = "%Y-%m-%d"))
  if (is.na(parsed)) {
    # Digit-shaped but not a real calendar date (e.g. "2026-02-30 foo").
    return(no_date)
  }

  list(date = parsed, residue = trimws(matched[3]))
}


#' @title Import Issue Record for a Metadata Field
#' @description
#' Builds a one-row record in the canonical import-error shape
#' (\code{dta_empty_import_errors()}). Metadata values have no row/column
#' coordinates, so the metadata field path is recorded in \code{column} and
#' \code{row} stays \code{NA}.
#' @param field Character. Field path, e.g. \code{"transmission$date_last_transfer"}.
#' @param raw Character. The original value, verbatim.
#' @param reason Character. Machine-readable reason code.
#' @param declared_type Character. The type the value was coerced into.
#' @return A one-row data.frame with columns \code{row}, \code{column},
#'   \code{raw}, \code{declared_type} and \code{reason}.
#' @keywords internal
.metadata_import_issue <- function(field, raw, reason, declared_type = "Date") {
  rbind(
    dta_empty_import_errors(),
    data.frame(
      row = NA_integer_,
      column = field,
      raw = raw,
      declared_type = declared_type,
      reason = reason,
      stringsAsFactors = FALSE
    )
  )
}


#' @title Parse a Metadata Date Field
#' @description
#' Converts a metadata date string to a \code{Date} without silently discarding
#' anything. A bare ISO date converts cleanly; a date followed by qualifying text
#' converts but reports an import issue carrying the original string; a string
#' with no leading ISO date is a free-text phrase and is returned untouched.
#' @param value The supplied value.
#' @param field Character. Field path used in the import issue record.
#' @param require_date Logical. When \code{TRUE} the field is typed \code{Date}
#'   and a phrase cannot be stored, so an unparseable string is coerced the way
#'   \code{as.Date()} would coerce it (to \code{NA}).
#' @return A list with \code{value} (the value to store) and \code{issues} (a
#'   possibly empty list of import issue records).
#' @keywords internal
.parse_metadata_date <- function(value, field, require_date = FALSE) {
  none <- list(value = value, issues = list())

  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    return(none)
  }

  split <- .split_date_prefix(value)

  if (is.na(split$date)) {
    # A phrase with no parseable date prefix ("after approval", "2 weeks after
    # approval", "final transfer by 2026-12-31"). Documented, legitimate input:
    # keep it verbatim and do NOT report an import issue.
    if (isTRUE(require_date)) {
      return(list(
        value = suppressWarnings(as.Date(value, format = "%Y-%m-%d")),
        issues = list()
      ))
    }
    return(none)
  }

  issues <- if (nzchar(split$residue)) {
    list(.metadata_import_issue(field, value, "trailing_residue"))
  } else {
    list()
  }

  list(value = split$date, issues = issues)
}


#' @title Process Transmission Dates
#' @description Helper to convert transmission date fields from character to Date if they're valid dates
#' @param transmission A transmission list.
#' @return A list with \code{transmission} (the processed list) and \code{issues}
#'   (a possibly empty list of import issue records).
#' @keywords internal
.process_transmission_dates <- function(transmission) {
  date_fields <- c("date_first_transfer", "date_last_transfer")
  issues <- list()

  for (field in date_fields) {
    if (is.null(transmission[[field]]) || !is.character(transmission[[field]])) {
      next
    }

    parsed <- .parse_metadata_date(
      transmission[[field]],
      field = paste0("transmission$", field)
    )
    transmission[[field]] <- parsed$value
    issues <- c(issues, parsed$issues)
  }

  list(transmission = transmission, issues = issues)
}


#' @title Coerce a Version History Date to a Date
#' @description
#' Version history records reach this package from three routes - built in R
#' (a \code{Date}), loaded from YAML (a character string), or omitted entirely
#' (\code{NULL}) - and the validator accepts all three. This helper normalises
#' any of them to a length-one \code{Date}, using \code{NA} for a missing or
#' unparseable value rather than raising, so that a single odd record cannot take
#' down a whole history.
#' @param value A \code{Date}, number, character string, \code{NULL}, or anything
#'   else found in a \code{version_history} record.
#' @return A length-one \code{Date}, possibly \code{NA}.
#' @keywords internal
.history_date <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(as.Date(NA))
  }

  value <- value[[1]]

  if (inherits(value, "Date")) {
    return(value)
  }
  if (is.numeric(value)) {
    return(as.Date(value, origin = "1970-01-01"))
  }
  if (is.character(value)) {
    return(suppressWarnings(as.Date(value, format = "%Y-%m-%d")))
  }

  as.Date(NA)
}


#' @title Print DTAMetaData Object
#' @description
#' Print method for DTAMetadata objects.
#' @param x An object of class DTAMetadata
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_div cli_text
#' @examples
#' library(DTAtools)
#' print(create_example_DTAMetaData())
#'
#' @name print
#' @export
method(print, DTAMetaData) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAMetaData}>")

  print_info(x)

  invisible(x)
}

#' @title Print Info DTAMetaData Object
#' @description
#' Print method for DTAMetadata objects.
#' @importFrom cli cli_alert_info cli_alert cli_text
#'
#' @param x An object of class DTAMetadata
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @examples
#' library(DTAtools)
#' print(create_example_DTAMetaData())
#'
#' @name print_info
#' @export
if (!exists("print_info", mode = "function")) {
  print_info <- new_generic("print_info", "x")
}
method(print_info, DTAMetaData) <- function(x, ...) {
  if (!is.null(x@title)) {
    cli::cli_alert_info("Title: {x@title}")
  }
  if (!is.null(x@version)) {
    cli::cli_alert_info("Version: {x@version}")
  }
  if (!is.null(x@date)) {
    cli::cli_alert_info("Date: {x@date}")
  }
  if (!is.null(x@header)) {
    cli::cli_alert_info("Header: {x@header}")
  }

  # Version History
  if (length(x@version_history) > 0) {
    cli::cli_alert_info("Version History:")
    for (i in seq_along(x@version_history)) {
      hist <- x@version_history[[i]]
      version_str <- hist$version %||% "N/A"
      # `format(NULL, "%Y-%m-%d")` returns the string "NULL", which `%||%` does
      # not catch, so a record without a date used to print "v1.0 (NULL)".
      hist_date <- .history_date(hist$date)
      date_str <- if (is.na(hist_date)) "N/A" else format(hist_date, "%Y-%m-%d")
      cli::cli_alert("  [{i}] v{version_str} ({date_str})")
      if (!is.null(hist$changes)) {
        cli::cli_text("       {hist$changes}")
      }
    }
  }

  # Receiver
  if (length(x@receiver) > 0) {
    cli::cli_alert_info("Receiver:")
    if (!is.null(x@receiver$affiliation)) {
      aff <- x@receiver$affiliation
      if (is.list(aff)) {
        cli::cli_alert("  Affiliation:")
        for (aff_field in names(aff)) {
          cli::cli_text("    {aff_field}: {aff[[aff_field]]}")
        }
      }
    }
    if (!is.null(x@receiver$contacts)) {
      cli::cli_alert("  Contacts:")
      for (nc in seq_along(x@receiver$contacts)) {
        contact <- x@receiver$contacts[[nc]]
        contact_str <- .format_contact(contact)
        cli::cli_text("    [{nc}] {contact_str}")
      }
    }
  }

  # Supplier
  if (length(x@supplier) > 0) {
    cli::cli_alert_info("Supplier:")
    if (!is.null(x@supplier$affiliation)) {
      aff <- x@supplier$affiliation
      if (is.list(aff)) {
        cli::cli_alert("  Affiliation:")
        for (aff_field in names(aff)) {
          cli::cli_text("    {aff_field}: {aff[[aff_field]]}")
        }
      }
    }
    if (!is.null(x@supplier$contacts)) {
      cli::cli_alert("  Contacts:")
      for (nc in seq_along(x@supplier$contacts)) {
        contact <- x@supplier$contacts[[nc]]
        contact_str <- .format_contact(contact)
        cli::cli_text("    [{nc}] {contact_str}")
      }
    }
  }

  # Transmission
  if (length(x@transmission) > 0) {
    cli::cli_alert_info("Transmission:")
    for (nm in names(x@transmission)) {
      val <- x@transmission[[nm]]
      if (inherits(val, "Date")) {
        val_str <- format(val, "%Y-%m-%d")
      } else if (is.logical(val)) {
        val_str <- if (val) "Yes" else "No"
      } else {
        val_str <- as.character(val)
      }
      cli::cli_alert("  {nm}: {val_str}")
    }
  }

  # Error Handling
  if (!is.null(x@error_handling)) {
    cli::cli_alert_info("Error Handling: {x@error_handling}")
  }

  # Authorized for Corrections
  if (!is.null(x@authorized_for_corrections)) {
    if (is.list(x@authorized_for_corrections)) {
      auth_str <- paste(unlist(x@authorized_for_corrections), collapse = ", ")
    } else {
      auth_str <- x@authorized_for_corrections
    }
    cli::cli_alert_info("Authorized for Corrections: {auth_str}")
  }

  invisible(x)
}


#' @title Format Contact Information
#' @description Helper to format contact list as readable string
#' @keywords internal
.format_contact <- function(contact) {
  if (!is.list(contact)) {
    return(as.character(contact))
  }

  parts <- c()
  if (!is.null(contact$name)) parts <- c(parts, contact$name)
  if (!is.null(contact$role)) parts <- c(parts, paste0("(", contact$role, ")"))
  if (!is.null(contact$email)) parts <- c(parts, contact$email)

  # Add special flags
  flags <- c()
  if (isTRUE(contact$signature)) flags <- c(flags, "signature")
  if (isTRUE(contact$reviewer)) flags <- c(flags, "reviewer")
  if (isTRUE(contact$backup)) flags <- c(flags, "backup")
  if (length(flags) > 0) {
    parts <- c(parts, paste0("[", paste(flags, collapse = ", "), "]"))
  }

  paste(parts, collapse = " ")
}


#' @title Print short info from DTAMetaData Object
#' @description
#' Print short info method for DTAMetadata objects.
#' @param x An object of class DTAMetadata
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info
#' @examples
#' library(DTAtools)
#' print_short_info(create_example_DTAMetaData())
#'
#' @name print_short_info
#' @export
if (!exists("print_short_info", mode = "function")) {
  print_short_info <- new_generic("print_short_info", "x")
}
method(print_short_info, DTAMetaData) <- function(x, ...) {
  message <- "Metadata: {x@title}"

  if (!is.null(x@version)) {
    message <- paste0(message, " ", x@version)
  }
  if (!is.null(x@date)) {
    message <- paste0(message, " ", format(x@date, "%Y-%m-%d"))
  }

  cli::cli_alert_info(message)

  invisible(x)
}


#' @title Create Example DTAMetaData Object
#' @description This function creates example \code{DTAMetaData}
#' objects with various levels of completeness and structure.
#'
#' @param index Numeric. 1 = basic example, 2 = full example with history and transmission,
#'   3 = example with date phrases for transfers
#'
#' @importFrom cli cli_abort
#'
#' @return An object of class \code{DTAMetaData} with example metadata.
#' @examples
#' library(DTAtools)
#' example_metadata <- create_example_DTAMetaData()
#' print(example_metadata)
#' @export
create_example_DTAMetaData <- function(index = 1) {
  # nolint
  switch(as.character(index),
    `1` = DTAMetaData(
      title = "Example DTA",
      version = "1.0",
      date = Sys.Date(),
      header = "Example Company header"
    ),
    `2` = DTAMetaData(
      title = "Clinical Data Transfer Agreement",
      version = "2.0",
      date = as.Date("2026-01-15"),
      header = "Boehringer Ingelheim",
      version_history = list(
        list(
          version = "1.0",
          date = as.Date("2025-10-01"),
          changes = "Initial version"
        ),
        list(
          version = "1.5",
          date = as.Date("2025-12-01"),
          changes = "Added error handling procedures"
        ),
        list(
          version = "2.0",
          date = as.Date("2026-01-15"),
          changes = "Final review and approval"
        )
      ),
      receiver = list(
        affiliation = list(
          name = "Test Company",
          country = "USA"
        ),
        contacts = list(
          list(
            name = "Alice Smith",
            role = "Lead Data Manager",
            email = "alice.smith@testcompany.com",
            department = "Data Management",
            signature = TRUE,
            reviewer = TRUE
          ),
          list(
            name = "Bob Johnson",
            role = "Clinical Bioinformatician",
            email = "bob.johnson@testcompany.com",
            department = "Bioinformatics",
            signature = TRUE,
            reviewer = TRUE
          )
        )
      ),
      supplier = list(
        affiliation = list(
          name = "Supplier Company Inc.",
          address = "123 Data Street, City",
          country = "Germany"
        ),
        contacts = list(
          list(
            name = "Emily Turner",
            role = "Senior Data Manager",
            email = "emily.turner@supplier.com",
            department = "Data Management",
            phone = "321-654-0987",
            signature = TRUE
          )
        )
      ),
      transmission = list(
        type = "Secure SFTP server",
        frequency = "One-time transfer",
        notification = "Email notification",
        test_upload = FALSE,
        blinded_transfer = FALSE,
        date_first_transfer = as.Date("2026-02-01"),
        date_last_transfer = as.Date("2026-03-31")
      ),
      error_handling = "Critical errors must be reported within 24 hours. BI contacts (marked as 'reviewer') are authorized to request corrections without prior consultation.",
      authorized_for_corrections = c("Alice Smith", "Bob Johnson")
    ),
    `3` = DTAMetaData(
      title = "Genomic Findings Data Transfer",
      version = "1.0",
      date = Sys.Date(),
      header = "Biomarker Data Transfer",
      transmission = list(
        type = "Secure cloud storage",
        frequency = "Monthly transfers",
        notification = "Email + Slack notification",
        test_upload = TRUE,
        blinded_transfer = TRUE,
        date_first_transfer = "2 weeks after approval",
        date_last_transfer = "Final transfer by 2026-12-31"
      ),
      error_handling = "Non-critical errors: 5 business day correction window. Critical errors block transfer and require immediate contact.",
      authorized_for_corrections = "BI Data Management Team"
    ),
    cli::cli_abort("Invalid index: {index}.")
  )
}
