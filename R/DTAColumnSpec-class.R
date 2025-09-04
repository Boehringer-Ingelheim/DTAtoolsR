class_null <- S7::new_S3_class("NULL")
class_character_or_null <- class_character | class_null
class_numeric_or_null <- class_numeric | class_null
class_character_or_numeric_or_null <- class_character |
  class_numeric |
  class_null
class_logical_or_null <- class_logical | class_null
class_character_or_null_or_list <- class_character |
  class_null |
  class_list
class_character_or_numeric_or_null_or_list <- class_character |
  class_numeric |
  class_null |
  class_list

#' @title DTA Column Format Class
#' @description
#' Class for column format
#' @import S7
#' @export
#'
#' @description
#' This class defines the format of a column in a DTA dataset.
#'
#' @param id Character. The id of the column.
#' @param label Character or NA. The label of the column.
#' @param type Character or NA. The type of the column.
#' @param format Character or NA. The format of the column.
#' @param nullable Logical or NA. Whether the column can be null.
#' @param pattern Character or NA. The pattern of the column.
#' @param values Any or NA. The values of the column.
#' @param description Character or NA. The description of the column.
#' @return An object of class DTAColumnSpec.
#' @examples
#' col_format <- DTAColumnSpec(id = "STUDYID", type = "Char", nullable = FALSE, values = "1234-1234")
DTAColumnSpec <- new_class(
  "DTAColumnSpec",
  constructor = function(
    id,
    label = NULL,
    type = NULL,
    format = NULL,
    length = NULL,
    nullable = NULL,
    pattern = NULL,
    values = NULL,
    description = NULL
  ) {
    new_object(
      S7_object(),
      id = id,
      label = label,
      type = type,
      format = format,
      length = length,
      nullable = nullable,
      description = description,
      values = values,
      pattern = pattern
    )
  },
  properties = list(
    id = class_character,
    label = class_character_or_null,
    type = class_character_or_null,
    format = class_character_or_numeric_or_null,
    length = class_numeric_or_null,
    nullable = class_logical_or_null,
    description = class_character_or_null,
    values = class_character_or_numeric_or_null_or_list,
    pattern = class_character_or_null
  ),
  validator = function(self) {
    if (any(grepl(self@id, pattern = "\\s") || is.null(self@id))) {
      "@id cannot have whitespaces and needs to be defined."
    } else if (
      !((is.character(self@label) & length(self@label) == 1) ||
        is.null(self@label))
    ) {
      sprintf("@label in %s has to be either character or null.", self@id)
    } else if (
      !((is.character(self@type) & length(self@type) == 1) ||
        is.null(self@type))
    ) {
      sprintf("@type in %s has to be either character or null.", self@id)
    } else if (
      !((is.character(self@pattern) & length(self@pattern) == 1) ||
        is.null(self@pattern))
    ) {
      sprintf("@pattern in %s has to be either character or null.", self@id)
    } else if (
      !((is.character(self@format) & length(self@format) == 1) ||
        is.null(self@format) ||
        is.numeric(self@format))
    ) {
      sprintf(
        "@format in %s has to be either character, numeric, or null.",
        self@id
      )
    } else if (
      !((is.numeric(self@length) & length(self@length) == 1) ||
        is.null(self@length))
    ) {
      sprintf("@length in %s has to be either numeric or null.", self@id)
    } else if (
      !((is.logical(self@nullable) & length(self@nullable) == 1) ||
        is.null(self@nullable))
    ) {
      sprintf("@nullable in %s has to be either character or null.", self@id)
    } else if (
      !((is.character(self@description) & length(self@description) == 1) ||
        is.null(self@description))
    ) {
      sprintf("@description in %s has to be either character or null.", self@id)
    }
  }
)
