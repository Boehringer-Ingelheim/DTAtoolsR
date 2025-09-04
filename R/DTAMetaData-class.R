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
