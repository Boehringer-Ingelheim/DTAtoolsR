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
#' @return An object of class DTAMetaData.
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
    authorized_for_corrections = NULL
  ) {
    if (is.character(date)) {
      date <- as.Date(date, format = "%Y-%m-%d")
    }
    
    # Process transmission dates: convert character dates to Date, keep phrases as-is
    if (length(transmission) > 0) {
      transmission <- .process_transmission_dates(transmission)
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
      authorized_for_corrections = authorized_for_corrections
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
    authorized_for_corrections = class_character_or_list_or_null
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
            errors <- c(errors, paste0("version_history[[", i, "]] missing required fields: ", 
                                      paste(missing_fields, collapse = ", "), 
                                      ". Each version record must have: version (string), date (Date), changes (description)."))
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
    
    if (length(errors) > 0) {
      paste(errors, collapse = "\n")
    }
  }
)


#' @title Process Transmission Dates
#' @description Helper to convert transmission date fields from character to Date if they're valid dates
#' @keywords internal
.process_transmission_dates <- function(transmission) {
  date_fields <- c("date_first_transfer", "date_last_transfer")
  
  for (field in date_fields) {
    if (!is.null(transmission[[field]]) && is.character(transmission[[field]])) {
      # Try to parse as date; if it fails, keep as character (phrase)
      tryCatch(
        {
          parsed_date <- as.Date(transmission[[field]], format = "%Y-%m-%d")
          if (!is.na(parsed_date)) {
            transmission[[field]] <- parsed_date
          }
        },
        error = function(e) {
          # Keep as character if parsing fails
        }
      )
    }
  }
  transmission
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
      date_str <- format(hist$date, "%Y-%m-%d") %||% "N/A"
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
  switch(
    as.character(index),
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
