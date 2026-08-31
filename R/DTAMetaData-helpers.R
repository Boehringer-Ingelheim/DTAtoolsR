#' @title Render Date Values as ISO Strings
#' @description
#' \code{yaml::write_yaml()} has no date type: it writes a \code{Date} as its
#' underlying number of days (\code{date: 20468.0}), and reading that back gives
#' a double, which the \code{DTAMetaData} constructor rejects because \code{@date}
#' is typed \code{Date}. Writing the ISO string instead produces YAML the
#' constructor already knows how to parse, so the object survives the round trip
#' it exists for.
#' @param value Any value; only \code{Date} values are changed.
#' @return The value, with \code{Date} rendered as \code{"YYYY-MM-DD"}.
#' @keywords internal
.date_to_iso <- function(value) {
  if (inherits(value, "Date")) {
    return(format(value, "%Y-%m-%d"))
  }
  value
}


#' @title Convert DTAMetaData to List
#' @description Convert a DTAMetaData object to a nested list structure suitable for YAML export
#' or programmatic access.
#'
#' @details
#' Every \code{Date} in the result - the top-level \code{date}, each
#' \code{version_history[[i]]$date}, and the transmission transfer dates - is
#' rendered as an ISO \code{"YYYY-MM-DD"} string so that a YAML dump can be read
#' straight back through \code{DTAMetaData()}.
#'
#' \code{@import_issues} is deliberately not exported: it is a runtime record of
#' what was lost while coercing the input, not part of the specification.
#' \code{@template} IS exported (when non-empty): it is a provenance record that
#' must survive a save/reload cycle so the rebase feature can trust it later.
#'
#' This method is hand-written field by field rather than iterating S7
#' properties, so it does NOT pick up a new \code{DTAMetaData} property
#' automatically. Adding a property to the class without adding it here means
#' it is silently dropped on the next save -- if you add a property, add it
#' to this function too.
#'
#' @param x An object of class DTAMetaData
#' @param ... Additional arguments (not used)
#'
#' @return A list with all metadata fields
#'
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' md_list <- as.list(md)
#'
#' @name as.list
#' @export
method(as.list, DTAMetaData) <- function(x, ...) {
  result <- list(
    title = x@title,
    version = x@version,
    date = .date_to_iso(x@date),
    header = x@header
  )

  # Add version_history if present
  if (length(x@version_history) > 0) {
    result$version_history <- lapply(x@version_history, function(record) {
      if (is.list(record) && !is.null(record$date)) {
        record$date <- .date_to_iso(record$date)
      }
      record
    })
  }

  # Add receiver if present
  if (length(x@receiver) > 0) {
    result$receiver <- x@receiver
  }

  # Add supplier if present
  if (length(x@supplier) > 0) {
    result$supplier <- x@supplier
  }

  # Add transmission if present
  if (length(x@transmission) > 0) {
    result$transmission <- lapply(x@transmission, .date_to_iso)
  }

  # Add error_handling if present
  if (!is.null(x@error_handling)) {
    result$error_handling <- x@error_handling
  }

  # Add authorized_for_corrections if present
  if (!is.null(x@authorized_for_corrections)) {
    result$authorized_for_corrections <- x@authorized_for_corrections
  }

  # Add template (machine-owned provenance) if present. Unlike import_issues,
  # this MUST round-trip: it is what the rebase feature will trust.
  if (length(x@template) > 0) {
    result$template <- x@template
  }

  result
}


#' @title Get Authorized Contacts for Corrections
#' @description Retrieve list of contacts authorized to request corrections from receiver
#'
#' @param x A DTAMetaData object
#'
#' @return A character vector of names or list of contacts authorized for corrections
#'
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' get_authorized_for_corrections(md)
#'
#' @export
get_authorized_for_corrections <- function(x) {
  tryCatch(
    x@authorized_for_corrections,
    error = function(e) {
      cli::cli_abort("x must be a DTAMetaData object with @authorized_for_corrections property")
    }
  )
}


#' @title Get Receiver Contacts with Reviewer Role
#' @description Retrieve receiver contacts who have the reviewer role (authorized to request corrections)
#'
#' @param x A DTAMetaData object
#' @param name_only Logical. If TRUE, return only names; if FALSE, return full contact objects
#'
#' @return A character vector of reviewer names or list of reviewer contact objects
#'
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' get_receiver_reviewers(md)
#'
#' @export
get_receiver_reviewers <- function(x, name_only = TRUE) {
  tryCatch(
    {
      if (is.null(x@receiver) || length(x@receiver) == 0) {
        return(NULL)
      }

      contacts <- x@receiver$contacts
      if (is.null(contacts)) {
        return(NULL)
      }

      reviewers <- Filter(function(c) isTRUE(c$reviewer), contacts)

      if (name_only) {
        sapply(reviewers, function(c) c$name %||% NA_character_)
      } else {
        reviewers
      }
    },
    error = function(e) {
      cli::cli_abort("x must be a DTAMetaData object with receiver and contacts")
    }
  )
}


#' @title Get Transmission Start and End Dates
#' @description Extract transmission transfer dates, whether they are Date objects or character phrases
#'
#' @param x A DTAMetaData object
#'
#' @return A list with elements first_transfer and last_transfer
#'
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' get_transmission_dates(md)
#'
#' @export
get_transmission_dates <- function(x) {
  tryCatch(
    {
      if (is.null(x@transmission) || length(x@transmission) == 0) {
        return(list(first_transfer = NULL, last_transfer = NULL))
      }

      list(
        first_transfer = x@transmission$date_first_transfer,
        last_transfer = x@transmission$date_last_transfer
      )
    },
    error = function(e) {
      cli::cli_abort("x must be a DTAMetaData object with @transmission property")
    }
  )
}


#' @title Get Version History as Data Frame
#' @description Convert version history to a data frame for easier inspection
#'
#' @param x A DTAMetaData object
#'
#' @return A data frame with columns: version, date, changes
#'
#' @details
#' Version history records may carry their date as a \code{Date} (built in R), as
#' a character string (loaded from YAML), or not at all. Each record's date is
#' therefore converted individually via \code{.history_date()}; a missing or
#' unparseable date becomes \code{NA} on that row instead of aborting the whole
#' call.
#'
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' get_version_history_df(md)
#'
#' @export
get_version_history_df <- function(x) {
  # Only the property access is guarded. Wrapping the whole body, as this
  # function used to, turned every internal failure into the same "x must be a
  # DTAMetaData object" message - which was flatly untrue for a well-formed
  # object whose history simply mixed Date and character dates.
  history <- tryCatch(
    x@version_history,
    error = function(e) {
      cli::cli_abort(
        "{.arg x} must be a DTAMetaData object with a {.field version_history} property."
      )
    }
  )

  if (!is.list(history)) {
    cli::cli_abort(
      "{.field version_history} must be a list, not {.cls {class(history)}}."
    )
  }

  if (length(history) == 0) {
    return(data.frame(
      version = character(0),
      date = as.Date(character(0)),
      changes = character(0)
    ))
  }

  field_as_character <- function(record, field) {
    value <- if (is.list(record)) record[[field]] else NULL
    if (is.null(value) || length(value) == 0) {
      return(NA_character_)
    }
    as.character(value)[[1]]
  }

  data.frame(
    version = vapply(history, field_as_character, character(1), field = "version"),
    date = do.call(c, lapply(history, function(record) {
      .history_date(if (is.list(record)) record$date else NULL)
    })),
    changes = vapply(history, field_as_character, character(1), field = "changes"),
    stringsAsFactors = FALSE
  )
}


#' @title Validate Transmission Dates
#' @description Check that transmission dates are valid (either Date objects or non-empty character phrases)
#'
#' @param x A DTAMetaData object
#'
#' @return A list with elements is_valid (logical) and messages (character vector)
#'
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' validate_transmission_dates(md)
#'
#' @export
validate_transmission_dates <- function(x) {
  tryCatch(
    {
      messages <- c()
      is_valid <- TRUE

      if (is.null(x@transmission) || length(x@transmission) == 0) {
        return(list(is_valid = TRUE, messages = "No transmission data to validate"))
      }

      first <- x@transmission$date_first_transfer
      last <- x@transmission$date_last_transfer

      if (!is.null(first) && !inherits(first, "Date") && (is.character(first) && nchar(first) == 0)) {
        messages <- c(messages, "date_first_transfer cannot be empty character")
        is_valid <- FALSE
      }

      if (!is.null(last) && !inherits(last, "Date") && (is.character(last) && nchar(last) == 0)) {
        messages <- c(messages, "date_last_transfer cannot be empty character")
        is_valid <- FALSE
      }

      if (!is.null(first) && !is.null(last) && inherits(first, "Date") && inherits(last, "Date")) {
        if (first > last) {
          messages <- c(messages, "date_first_transfer cannot be after date_last_transfer")
          is_valid <- FALSE
        }
      }

      if (is_valid && length(messages) == 0) {
        messages <- "All transmission dates are valid"
      }

      list(is_valid = is_valid, messages = messages)
    },
    error = function(e) {
      cli::cli_abort("Error validating transmission dates: {e$message}")
    }
  )
}


#' @title Metadata Import Errors
#' @description
#' Returns the values that could not be represented in their declared metadata
#' type, in the same shape as the per-table import errors on the validation
#' axis (\code{row}, \code{column}, \code{raw}, \code{declared_type},
#' \code{reason}).
#'
#' Metadata has no rows or columns, so \code{row} is always \code{NA} and
#' \code{column} carries the metadata field path, e.g.
#' \code{"transmission$date_last_transfer"}.
#'
#' @param x A DTAMetaData object
#'
#' @return A data.frame with one row per import error; zero rows when the
#'   metadata imported cleanly.
#'
#' @examples
#' library(DTAtools)
#' md <- DTAMetaData(
#'   title = "Qualified Date",
#'   transmission = list(date_last_transfer = "2026-12-31 at the earliest")
#' )
#' metadata_import_errors(md)
#'
#' @export
metadata_import_errors <- function(x) {
  issues <- tryCatch(
    x@import_issues,
    error = function(e) {
      cli::cli_abort(
        "{.arg x} must be a DTAMetaData object with an {.field import_issues} property."
      )
    }
  )

  if (!is.list(issues) || length(issues) == 0) {
    return(dta_empty_import_errors())
  }

  out <- do.call(rbind, c(list(dta_empty_import_errors()), issues))
  rownames(out) <- NULL
  out
}


#' @title Human-Readable Metadata Import Error Messages
#' @description
#' The per-table equivalent, \code{dta_import_error_messages()}, ends every
#' message with "imported as NA", which is not what happens to metadata: the
#' value *was* stored, as a \code{Date}, and what went missing is the text around
#' it. The message therefore names both the part that was kept and the part that
#' was dropped, and quotes the original verbatim.
#' @param import_errors A data.frame in the shape of
#'   \code{dta_empty_import_errors()}.
#' @return A character vector, one message per row.
#' @keywords internal
dta_metadata_import_error_messages <- function(import_errors) {
  if (!is.data.frame(import_errors) || nrow(import_errors) == 0) {
    return(character(0))
  }

  vapply(
    seq_len(nrow(import_errors)),
    function(i) {
      raw <- as.character(import_errors$raw[[i]])
      field <- as.character(import_errors$column[[i]])
      declared_type <- as.character(import_errors$declared_type[[i]])
      reason <- as.character(import_errors$reason[[i]])

      if (identical(reason, "trailing_residue")) {
        kept <- sub("^\\s*(\\d{4}-\\d{2}-\\d{2}).*$", "\\1", raw)
        dropped <- trimws(sub("^\\s*\\d{4}-\\d{2}-\\d{2}", "", raw))
        return(sprintf(
          paste0(
            "metadata field '%s' was written as '%s'; only '%s' fits the ",
            "declared type '%s', so the qualification '%s' was dropped"
          ),
          field, raw, kept, declared_type, dropped
        ))
      }

      sprintf(
        "metadata field '%s' with value '%s' cannot be represented as declared type '%s' (%s)",
        field, raw, declared_type, reason
      )
    },
    character(1)
  )
}


#' @title Metadata Import Messages
#' @description
#' Converts the metadata import axis into message rows carrying exactly the same
#' columns, in the same order, as the per-table message frames, so the two can be
#' bound together. \code{dataset} is \code{NA} because metadata belongs to the
#' \code{DTA} as a whole rather than to any one dataset.
#' @param x A DTAMetaData object.
#' @return A data.frame of messages, or the empty message frame.
#' @keywords internal
dta_metadata_messages_to_df <- function(x) {
  errors <- metadata_import_errors(x)

  if (nrow(errors) == 0) {
    return(dta_empty_messages())
  }

  out <- data.frame(
    dataset = rep(NA_character_, nrow(errors)),
    target = rep("metadata", nrow(errors)),
    severity = rep("error", nrow(errors)),
    source = rep("import", nrow(errors)),
    rule_id = rep(NA_character_, nrow(errors)),
    row = suppressWarnings(as.numeric(errors$row)),
    column = as.character(errors$column),
    keyword = as.character(errors$reason),
    message = dta_metadata_import_error_messages(errors),
    stringsAsFactors = FALSE
  )

  dta_attach_message_ids(out)
}


# `messages()` method for DTAMetaData: reports the metadata import axis, i.e. the
# values whose declared type could only be reached by discarding part of what was
# written. Metadata belongs to the DTA as a whole rather than to a dataset, so it
# is queried through the metadata object: `messages(metadata(dta))`.
#' @export
S7::method(messages, DTAMetaData) <- function(x, as_tibble = TRUE) {
  dta_to_tibble_if_available(
    dta_metadata_messages_to_df(x),
    as_tibble = as_tibble
  )
}
