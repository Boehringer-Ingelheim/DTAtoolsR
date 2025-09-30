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
DTAMetaData <- new_class(
  "DTAMetaData",
  constructor = function(
    name,
    version = NULL,
    author = NULL
  ) {
    new_object(
      S7_object(),
      name = name,
      version = version,
      author = author
    )
  },
  properties = list(
    name = class_character,
    version = class_character_or_null,
    author = class_character_or_null
  ),
  validator = function(self) {
    if (!is.null(self@version) && self@version == "") {
      "'version' cannot be an empty string."
    }
    if (is.null(self@name) || self@name == "") {
      "'name' cannot be an empty."
    }
    if (!is.null(self@author) && self@author == "") {
      "'author' cannot be an empty string."
    }
  }
)



#' @title Print DTAMetaData Object
#' @description
#' Print method for DTAMetadata objects.
#' @param x An object of class DTAMetadata
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_alert
#' @examples
#' library(DTAtools)
#' print(create_example_DTAMetaData())
#'
#' @name print
#' @export
method(print, DTAMetaData) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAMetaData}>")

  cli_alert_info("Version: {x@version}")
  cli_alert("Author: {x@author}")

  invisible(x)
}


#' @title Create Example DTAMetaData Object
#' @description This function creates an example \code{DTAMetaData}
#' object with default values.
#'
#' @return An object of class \code{DTAMetaData} with example metadata.
#' @examples
#' library(DTAtools)
#' example_metadata <- create_example_DTAMetaData()
#' print(example_metadata)
#' @export
create_example_DTAMetaData <- function(index = 1) {
  switch(index,
    `1` = DTAMetaData(
      name = "Example DTA",
      version = "1.0",
      author = "John Doe"
    ),
    `2` = DTAMetaData(
      name = "Example DTA",
      version = "2.0",
      author = "Jane Smith"
    ),
    cli::cli_abort("Invalid index: {index}.")
  )
}
