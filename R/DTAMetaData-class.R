#' @title DTAMetaData Class
#' @description This class holds metadata information
#' @import S7
#' @export
#'
#' @param version version
#' @param author author
#' @return An object of class DTAMetaData.
#'
#' @examples
#'
#' \dontrun{
#' DTAMetaData(version = "1.0", author = "John Doe")
#' }
DTAMetaData <- S7::new_class(
  "DTAMetaData",
  constructor = function(
    title,
    version = NULL,
    date = NULL,
    header = NULL,
    receiver = list(),
    supplier = list(),
    transmission = list()
  ) {
    if (is.character(date)) {
      date <- as.Date(date, format = "%Y-%m-%d")
    }
    new_object(
      S7_object(),
      title = title,
      version = version,
      date = date,
      header = header,
      receiver = receiver,
      supplier = supplier,
      transmission = transmission
    )
  },
  properties = list(
    title = class_character,
    version = class_character_or_null,
    date = class_Date,
    header = class_character_or_null,
    receiver = class_list,
    supplier = class_list,
    transmission = class_list
  ),
  validator = function(self) {
    if (!is.null(self@version) && self@version == "") {
      "'version' cannot be an empty string."
    }
    if (is.null(self@title) || self@title == "") {
      "'title' cannot be an empty."
    }
  }
)


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

  if (length(x@receiver) > 0) {
    cli::cli_alert_info("Receiver:")
    for (nm in names(x@receiver)) {
      if (nm == "contacts") {
        cli::cli_alert("  {nm}: ")
        contact <- x@receiver[[nm]]
        if (!is.null(contact$signature)) {
          if (contact$signature) {
            contact$signature <- "signature"
          } else {
            contact$signature <- NULL
          }
        }
        # TODO work on proper handling of boolean reviewer and signature field
        if (!is.null(contact$reviewer)) {
          if (contact$reviewer) {
            contact$reviewer <- "reviewer"
          } else {
            contact$reviewer <- NULL
          }
        }

        for (nc in 1:length(x@receiver[[nm]])) {
          cli::cli_text(" -    {.field {nc}}: {contact[[nc]]}")
        }
      } else {
        cli::cli_alert("  {nm}: {x@receiver[[nm]]}")
      }
    }
  }

  if (length(x@supplier) > 0) {
    cli::cli_alert_info("Supplier:")
    for (nm in names(x@supplier)) {
      if (nm == "contacts") {
        cli::cli_alert("  {nm}:")
        for (nc in 1:length(x@supplier[[nm]])) {
          cli::cli_text(" -    {.field {nc}}: {x@supplier[[nm]][[nc]]}")
        }
      } else {
        cli::cli_alert("  {nm}: {x@supplier[[nm]]}")
      }
    }
  }

  if (length(x@transmission) > 0) {
    cli::cli_alert_info("Transmission:")
    for (nm in names(x@transmission)) {
      cli::cli_alert("  {nm}: {x@transmission[[nm]]}")
    }
  }

  invisible(x)
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
#' @description This function creates an example \code{DTAMetaData}
#' object with default values.
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
    index,
    `1` = DTAMetaData(
      title = "Example DTA",
      version = "1.0",
      date = Sys.Date(),
      header = "Example Company header"
    ),
    `2` = DTAMetaData(
      title = "Example DTA",
      version = "2.0"
    ),
    cli::cli_abort("Invalid index: {index}.")
  )
}
