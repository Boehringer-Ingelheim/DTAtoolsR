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
#' @param length Numeric or NA. The max character length.
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
    }
  }
)


#' @title Get Arrow Schema Type
#' @description
#' Returns the corresponding Arrow schema type for a given DTAColumnSpec
#' object based on its `type` property.
#' @importFrom glue glue
#' @param x A DTAColumnSpec object.
#' @return A character string representing the Arrow schema type.
#' @examples
#' col <- DTAColumnSpec(id = "AGE", type = "Char")
#' get_arrow_schema_type(col)
#' @export
get_arrow_schema_type <- function(x) {
  if (!inherits(x, "DTAtools::DTAColumnSpec")) {
    stop("Input must be a DTAColumnSpec object.")
  }
  type <- x@type
  if (is.null(type)) {
    stop(glue::glue("Type is not set for {x$id}."))
  }
  switch(
    type,
    "Char" = "utf8",
    "Num" = "double",
    "Int" = "int32",
    "Bool" = "bool",
    NA_character_
  )
}

