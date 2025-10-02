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
    title,
    version = NULL,
    date = NULL,
    header = NULL,
    receiver = list(),
    supplier = list(),
    transmission = list()
  ) {
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
      title = "Example DTA",
      version = "1.0"
    ),
    `2` = DTAMetaData(
      title = "Example DTA",
      version = "2.0"
    ),
    cli::cli_abort("Invalid index: {index}.")
  )
}
