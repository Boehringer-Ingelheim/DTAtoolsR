#' @title DTA Column Spec Structure 
#' @description
#' Class for column types
#' @import S7
#' @export
#'
#' @description
#' This class defines the structure of a column in a DTA dataset.
#'
#' @param type Character or NA. The type of the column.
#' @param format Character or NA. The format of the column.
#' @param length Numeric or NA. The max character length.
#' @examples
#' \dontrun{
#'  #TODO
#' }
DTAColumnSpecStructure <- new_class(
  "DTAColumnSpecStructure",
  constructor = function(
    type = NULL,
    format = NULL,
    length = NULL,
    backend = NULL
  ) {

    new_object(
      S7_object(),
      type = type,
      format = format,
      length = length,
      backend = backend
    )
  },
  properties = list(
    type = class_character_or_null,
    format = class_character_or_numeric_or_null,
    length = class_numeric_or_null,
    backend = class_character
  ),
  validator = function(self) {
    # backend cannot be empty character
    if (is.null(self@backend) || self@backend == "") {
      "'backend' must be defined and cannot be an empty character."
    }
  }
)
