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
    version = NULL,
    author = NULL
  ) {
    new_object(
      S7_object(),
      version = version,
      author = author
    )
  },
  properties = list(
    version = class_character_or_null,
    author = class_character_or_null
  )
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
  cli_text("<DTAMetadata>")

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
      version = "1.0",
      author = "John Doe"
    ),
    `2` = DTAMetaData(
      version = "2.0",
      author = "Jane Smith"
    ),
    cli::cli_abort("Invalid index: {index}.")
  )
}
