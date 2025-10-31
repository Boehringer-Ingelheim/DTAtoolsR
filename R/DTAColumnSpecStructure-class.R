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
DTAColumnSpecStructure <- S7::new_class(
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
#' @title as.list method for as.list.DTAColumnSpecStructure
#' @description
#' Converts a DTAColumnSpecStructure object to a list.
#' @param x A DTAColumnSpecStructure object.
#' @param ... Additional arguments (ignored).
#' @return A named list with the DTAColumnSpecStructure properties.
#' @export
#' @name as.list
#' @rdname as.list-DTAColumnSpecStructure
method(as.list, DTAColumnSpecStructure) <- function(x, ...) {
  list(
    type = paste(x@backend, x@type),
    format = paste(x@format, x@backend),
    length = x@length,
  )
}

#' @title as_json_schema_type
#' @description
#' Converts a DTAColumnSpecStructure to a JSON Schema type.
#' @name as_json_schema_type
#' @rdname as_json_schema_type-DTAColumnSpecStructure
#' @param x An object of class DTAColumnSpecStructure.
#' @export
if (!exists("as_json_schema_type", mode = "function")) {
  as_json_schema_type <- new_generic("as_json_schema_type", "x")
}
method(as_json_schema_type, DTAColumnSpecStructure) <- function(x) {
  cli_abort("as_json_schema_type is not implemented at this level.")
}

#' @description
#' Converts a DTAColumnSpecStructure to a JSON Schema.
#' @name as_json_schema
#' @rdname as_json_schema-DTAColumnSpecStructure
#' @title as_json_schema
#' @export
if (!exists("as_json_schema", mode = "function")) {
  as_json_schema <- new_generic("as_json_schema", "x")
}

method(as_json_schema, DTAColumnSpecStructure) <- function(x) {
  schema <- list()

  if (!is.null(x@type)) {
    schema$type <- as_json_schema_type(x)
  }
  if (!is.null(x@format)) {
    schema$format <- x@format
  }
  if (!is.null(x@length)) {
    schema$maxLength <- x@length
  }

  return(schema)
}


#' @title names
#' @description
#' returns list of names of the column specs
#' @name names
#' @rdname names-DTAColumnSpecStructure
#' @export
if (!exists("names", mode = "function")) {
  names <- new_generic("names", "x")
}
#' @export
method(names, DTAColumnSpecStructure) <- function(x) {
  sapply(x@columns, function(col) col@id)
}


#' @title print info
#' @description
#' prints info of the column spec structure
#' @name print_info
#' @rdname print_info-DTAColumnSpecStructure
#' @export
if (!exists("print_info", mode = "function")) {
  print_info <- new_generic("print_info", "x")
}
#' @export
method(print_info, DTAColumnSpecStructure) <- function(x) {
  if (!is.null(x@type)) {
    cli_alert("type       : {x@type}")
  }
  if (!is.null(x@format)) {
    cli_alert("format     : {x@format}")
  }
  if (!is.null(x@length)) {
    cli_alert("length     : {x@length}")
  }
  if (!is.null(x@backend)) {
    cli_alert("backend    : {x@backend}")
  }
  invisible(x)
}


#' @title print
#' @description
#' prints info of the column spec structure
#' @name print
#' @rdname print-DTAColumnSpecStructure
#' @export
if (!exists("print", mode = "function")) {
  print <- new_generic("print_info", "x")
}
#' @export
method(print, DTAColumnSpecStructure) <- function(x) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAColumnSpecStructure}>")
  print_info(x)
  invisible(x)
}
