class_null <- S7::new_S3_class("NULL")
class_DTAColumnSpec <- S7::new_class("DTAColumnSpec")
class_DTAColumnSpecCollection <- S7::new_class(
  "DTAColumnSpecCollection"
)
class_DTAColumnSpecStructure <- S7::new_class(
  "DTAColumnSpecStructure"
)
class_DTAColumnSpecStructure_or_null <- class_DTAColumnSpecStructure |
  class_null
class_DTADataSet <- S7::new_class("DTADataSet")
class_DTAMetaData <- S7::new_class("DTAMetaData")
class_DTARuleCollection <- S7::new_class("DTARuleCollection")
class_DTARuleCollection_or_null <- S7::new_class("DTARuleCollection") | class_null
class_character_or_null <- S7::class_character | class_null
class_numeric_or_null <- S7::class_numeric | class_null
class_character_or_numeric_or_null <- S7::class_character |
  S7::class_numeric |
  class_null
class_logical_or_null <- S7::class_logical | class_null
class_character_or_list <- S7::class_character |
  S7::class_list
class_character_or_list_or_null <- S7::class_character |
  S7::class_list |
  class_null
class_character_or_numeric_or_null_or_list <- S7::class_character |
  S7::class_numeric |
  class_null |
  S7::class_list

`__extract_prefix_and_rest__` <- function(x) {
  if (is.null(x)) {
    return(list(prefix = NULL, rest = NULL))
  }
  parts <- stringr::str_split(x, "\\s+", n = 2)[[1]]
  prefix <- parts[1]
  rest <- if (length(parts) > 1) parts[2] else NULL
  if (is.null(prefix) || prefix == "") {
    cli_abort("No prefix could be extracted from '{x}'.")
  }
  list(prefix = prefix, rest = rest)
}

`__DTAtools_supported_backends__` <- c("SAS")
`__DTAtools_supported_dataset_types__` <- c("tabular")
`__DTAtools_supported_file_types__` <- c("csv", "tsv") # TODO: "sas7bdat", ..

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
#' DTAColumnSpecStructureFactory(type = "SAS Char", format = "SAS $10.", length = 10)
#'
#' @seealso \code{\link{DTAtools::DTAColumnSpecStructureSAS}}, \code{\link{DTAtools::DTAColumnSpecStructureR}}
#' @export
DTAColumnSpecStructureFactory <- function(
  type = NULL,
  format = NULL,
  length = NULL
) {
  type_info <- `__extract_prefix_and_rest__`(type)
  format_info <- `__extract_prefix_and_rest__`(format)

  if (
    !is.null(type_info$prefix) &&
      !type_info$prefix %in% `__DTAtools_supported_backends__`
  ) {
    cli_abort(
      "'type' prefix '{type_info$prefix}'must be one of the supported backends: {str_flatten_comma(`__DTAtools_supported_backends__`)}"
    )
  }

  if (
    !is.null(format_info$prefix) &&
      !format_info$prefix %in% `__DTAtools_supported_backends__`
  ) {
    cli_abort(
      "'format' prefix '{format_info$prefix}' must be one of the supported backends: {str_flatten_comma(`__DTAtools_supported_backends__`)}"
    )
  }

  # If both type and format are provided, check backend support and that prefixes match
  if (
    !is.null(type_info$prefix) &&
      !is.null(format_info$prefix) &&
      type_info$prefix != format_info$prefix
  ) {
    cli_abort(
      "The 'type' and 'format' prefixes must be the same. Got '{type_info$prefix}' and '{format_info$prefix}'."
    )
  }

  backend = if (!is.null(type_info$prefix)) {
    type_info$prefix
  } else {
    format_info$prefix
  }

  switch(
    backend,
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


#' @title Create a DTADataSetFactory Object
#'
#' @description
#' Constructs a DTADataSetFactory object for a specified backend (e.g., SAS or R),
#' based on the provided type.
#'
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param columns A list with column specifications (optional)
#' @param rules A list of rules specifications (optional)
#' @param files A list of for conversion to DTAFile objects (optional)
#' @param ... Character. Arguments passed on to the specific backend constructor.
#' @importFrom cli cli_abort
#' @return An object derived from class \code{DTADataSet} like \code{DTADataSetTabular},
#' depending on the backend specified.
#'
#' @examples
#' library(DTAtools)
#' DTAColumnSpecStructureFactory(type = "tabular", name = "mydataset")
#'
#' @seealso \code{\link{DTAtools::DTADataSet}}, \code{\link{DTAtools::DTADataSetTabular}}
#' @export
DTADataSetFactory <- function(
  type,
  columns = NULL,
  rules = NULL,
  files = NULL,
  ...
) {
  # check that type is not NULL and
  if (!type %in% `__DTAtools_supported_dataset_types__`) {
    cli_abort(
      "'type' prefix '{type}'must be one of the supported: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}"
    )
  }

  switch(
    type,
    "tabular" = {
      return(DTADataSetTabular(
        specs = specs_from_list(columns = columns, rules = rules),
        files = do.call(DTAFileFactory, files),
        ...
      ))
    },
    cli_abort("Dataset type '{type}' not implemented.")
  )
}


#' @title Create a DTAFile Object
#'
#' @description
#' Constructs a DTAFile object for a specified backend (e.g., SAS or R),
#' based on the provided type and file path.
#'
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param ... Additional arguments passed to the specific backend constructor.
#'
#' @return An object derived from class \code{DTAFile}, depending on the backend specified.
#'
#' @examples
#' library(DTAtools)
#' DTAFileFactory(type = "SAS sas7bdat", path = "data/myfile.sas7bdat")
#'
#' @seealso \code{\link{DTAtools::DTAFile}}
#' @export
DTAFileFactory <- function(
  type,
  ...
) {
  if (is.null(type) || type == "") {
    cli_abort("'type' must be a non-empty string.")
  }

  if (!type %in% `__DTAtools_supported_file_types__`) {
    cli_abort(
      "'type' '{type}' must be one of the supported file types: {str_flatten_comma(`__DTAtools_supported_file_types__`)}"
    )
  }

  switch(
    type,
    csv = DTAFileCSV(
      ...
    ),
    tsv = DTAFileTSV(
      ...
    ),
    cli_abort("Filetype '{type}' not implemented.")
  )
}
