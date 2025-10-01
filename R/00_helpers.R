class_null <- S7::new_S3_class("NULL")
class_DTAColumnSpec <- S7::new_class("DTAColumnSpec")
class_DTAColumnSpecCollection <- S7::new_class(
  "DTAColumnSpecCollection"
)
class_DTAColumnSpecStructure <- S7::new_class(
  "DTAColumnSpecStructure"
)
class_DTAColumnSpecStructure_or_null <- class_DTAColumnSpecStructure | class_null
class_DTAContainer <- S7::new_class("DTAContainer")
class_DTAMetadata <- S7::new_class("DTAMetaData")
class_character_or_null <- class_character | class_null
class_numeric_or_null <- class_numeric | class_null
class_character_or_numeric_or_null <- class_character |
  class_numeric |
  class_null
class_logical_or_null <- class_logical | class_null
class_character_or_list <- class_character |
  class_list
class_character_or_numeric_or_null_or_list <- class_character |
  class_numeric |
  class_null |
  class_list
#class_vector_or_null <- class_vector | class_null


`__extract_prefix_and_rest__` <- function(x) {
  if (is.null(x)) return(list(prefix = NULL, rest = NULL))
  parts <- stringr::str_split(x, "\\s+", n = 2)[[1]]
  prefix <- parts[1]
  rest <- if (length(parts) > 1) parts[2] else NULL
  if (is.null(prefix) || prefix == "") {
    cli_abort("No prefix could be extracted from '{x}'.")
  }
  list(prefix = prefix, rest = rest)
}

`__DTAtools_supported_backends__` <- c("SAS")


#' Create a DTAColumnSpecStructure Object
#'
#' Constructs a DTAColumnSpecStructure object for a specified backend (e.g., SAS or R), 
#' based on the provided type, format, and length. The function validates that the 
#' prefixes of type and format are supported and match, then dispatches to the appropriate 
#' backend-specific constructor.
#'
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param format Character. The format specification, potentially prefixed with a backend identifier.
#' @param length Integer or NULL. The length specification for the column (optional).
#'
#' @return An object of class \code{DTAColumnSpecStructureSAS} or \code{DTAColumnSpecStructureR}, 
#' depending on the backend specified.
#'
#' @details
#' The function checks that the prefixes of \code{type} and \code{format} are among the supported 
#' backends and that they match. If both are provided and do not match, an error is thrown. 
#' The backend is determined by the prefix of \code{type} or \code{format}.
#'
#' @examples
#' library(DTAtools)
#' create_DTAColumnSpecStructure(type = "SAS Char", format = "SAS $10.", length = 10)
#'
#' @seealso \code{\link{DTAtools::DTAColumnSpecStructureSAS}}, \code{\link{DTAtools::DTAColumnSpecStructureR}}
#' @export

create_DTAColumnSpecStructure <- function(
    type = NULL,
    format = NULL,
    length = NULL) {

  type_info <- `__extract_prefix_and_rest__`(type)
  format_info <- `__extract_prefix_and_rest__`(format)

  if (!is.null(type_info$prefix) && !type_info$prefix %in% `__DTAtools_supported_backends__`) {
    cli_abort("'type' prefix '{type_info$prefix}'must be one of the supported backends: {str_flatten_comma(self@supported_backends)}")
  } 

  if (!is.null(format_info$prefix) && !format_info$prefix %in% `__DTAtools_supported_backends__`) {
    cli_abort("'format' prefix '{format_info$prefix}' must be one of the supported backends: {str_flatten_comma(self@supported_backends)}")
  } 

  # If both type and format are provided, check backend support and that prefixes match
  if (!is.null(type_info$prefix) && !is.null(format_info$prefix) && type_info$prefix != format_info$prefix) {
    cli_abort("The 'type' and 'format' prefixes must be the same. Got '{type_info$prefix}' and '{format_info$prefix}'.")
  } 

  backend = if (!is.null(type_info$prefix)) type_info$prefix else format_info$prefix

  switch(backend,
    SAS = DTAtools::DTAColumnSpecStructureSAS(
      type = type_info[["rest"]],
      format = format_info[["rest"]],
      length = length
    ),
    R = DTAtools::DTAColumnSpecStructureR(
      type = type_info[["rest"]],
      format = format_info[["rest"]],
      length = length
    ),
    cli::cli_abort("Backend '{backend}' not implemented.")
  )
}

