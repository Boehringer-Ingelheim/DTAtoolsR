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


#' @title Create Example DTAColumnSpec
#' @description
#' S7 method to create and return an example DTAColumnSpec object.
#' @param index Numeric. Selector for different example objects.
#' @importFrom cli cli_abort
#' @return An example DTAColumnSpec object based on the provided index.
#' @examples
#' library(DTAtools)
#' create_example_DTAColumnSpec()
#' @export
create_example_DTAColumnSpec <- function(index = 1) {
  switch(
    index,
    `1` = {
      DTAtools::DTAColumnSpec(
        id = "STUDYID",
        label = "Study Identifier",
        type = "Char",
        nullable = FALSE,
        values = list("1234", "5678"),
        description = "Unique study identifier"
      )
    },
    `2` = {
      DTAtools::DTAColumnSpec(
        id = "VISIT",
        label = "Visit",
        type = "Char",
        nullable = FALSE,
        values = list("V01", "EOT"),
        description = "Visit code"
      )
    },
    `3` = {
      DTAtools::DTAColumnSpec(
        id = "SUBJID",
        label = "Subject Identifier",
        type = "Char",
        nullable = FALSE,
        values = list("001", "002"),
        description = "Unique subject identifier"
      )
    },
    `4` = {
      DTAtools::DTAColumnSpec(
        id = "AGE",
        label = "Age",
        type = "Int",
        nullable = TRUE,
        pattern = "^[0-9]{1,3}$",
        description = "Age in years"
      )
    },
    `5` = {
      DTAtools::DTAColumnSpec(
        id = "AVAL",
        label = "Analysis Value",
        type = "Int",
        nullable = FALSE,
        pattern = "^[0-9]+(\\.[0-9]{1,2})?$",
        description = "Analysis value"
      )
    },
    {
      cli_abort("Invalid index value for example DTAColumnSpec.")
    }
  )
}


#' @title Print Method for DTAColumnSpec
#' @description
#' S7 print method for DTAColumnSpec objects.
#' @param x A DTAColumnSpec object.
#' @param ... Additional arguments (ignored).
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @name print
#' @export
method(print, DTAColumnSpec) <- function(x) {
  cli_text("<DTAColumnSpec>: {x@id}")
  if (!is.null(x@label))        cli_alert("label      : {x@label}")
  if (!is.null(x@type))         cli_alert("type       : {x@type}")
  if (!is.null(x@format))       cli_alert("format     : {x@format}")
  if (!is.null(x@length))       cli_alert("length     : {x@length}")
  if (!is.null(x@nullable))     cli_alert("nullable   : {ifelse(x@nullable, cli::symbol$tick, cli::symbol$cross)}")
  if (!is.null(x@pattern))      cli_alert("pattern    : {x@pattern}")
  if (!is.null(x@values))       cli_alert("values     : {paste0(capture.output(str(x@values, give.attr = FALSE)), collapse = ' ')}")
  if (!is.null(x@description))  cli_alert("description: {x@description}")
  invisible(x)
}
