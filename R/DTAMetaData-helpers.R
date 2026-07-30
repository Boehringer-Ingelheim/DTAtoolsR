#' @title Convert DTAMetaData to List
#' @description Convert a DTAMetaData object to a nested list structure suitable for YAML export
#' or programmatic access.
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
    date = x@date,
    header = x@header
  )

  # Add version_history if present
  if (length(x@version_history) > 0) {
    result$version_history <- x@version_history
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
    result$transmission <- x@transmission
  }

  # Add error_handling if present
  if (!is.null(x@error_handling)) {
    result$error_handling <- x@error_handling
  }

  # Add authorized_for_corrections if present
  if (!is.null(x@authorized_for_corrections)) {
    result$authorized_for_corrections <- x@authorized_for_corrections
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
#' @examples
#' library(DTAtools)
#' md <- create_example_DTAMetaData(2)
#' get_version_history_df(md)
#'
#' @export
get_version_history_df <- function(x) {
  tryCatch(
    {
      if (length(x@version_history) == 0) {
        return(data.frame(
          version = character(0),
          date = as.Date(character(0)),
          changes = character(0)
        ))
      }

      versions <- sapply(x@version_history, function(h) h$version %||% NA_character_)
      dates <- sapply(x@version_history, function(h) h$date %||% NA)
      changes <- sapply(x@version_history, function(h) h$changes %||% NA_character_)

      data.frame(
        version = versions,
        date = as.Date(dates, origin = "1970-01-01"),
        changes = changes,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      cli::cli_abort("x must be a DTAMetaData object with @version_history property")
    }
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
