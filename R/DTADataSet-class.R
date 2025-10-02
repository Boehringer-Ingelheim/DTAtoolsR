#' @title DTADataSet Class
#' @description Class to handle data from files
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom stringr str_flatten_comma
#' @param name Character. Name of the container.
#' @param type Character. Dataset type, must be one of `__DTAtools_supported_datasets__`.
#' @param fileinfo a list of DTAFile objects specifying input file information.
#' @return An object of class DTADataSet. If validation fails, returns a list containing summarised and full error data frames.
#'
#' @examples
#'
#' \dontrun{
#' # Create sample 
#' }
#' @export
DTADataSet <- new_class(
  "DTADataSet",
  constructor = function(
    name,
    type,
    fileinfo = list()
  ) {
    if(inherits(fileinfo, "DTAtools::DTAFile")) {
      fileinfo = list(fileinfo)
    }

    new_object(
      S7_object(),
      name = name,
      type = type,
      fileinfo = fileinfo
    )
  },
  properties = list(
    name = class_character, 
    type = class_character,
    fileinfo = class_list # list of DTAFile
  ),
  validator = function(self) {
    if (!is.character(self@name) || length(self@name) != 1 || self@name == "") {
      cli_abort("Property 'name' must be a single non-empty string.")
    }
    if (!all(sapply(self@fileinfo, inherits, "DTAtools::DTAFile"))) {
      cli_abort("All elements in 'fileinfo' must be of class 'DTAFile'")
    }
    if (!self@type %in% `__DTAtools_supported_datasets__`) {
      cli_abort("Property 'type' is '{self@type})', must be one of: {str_flatten_comma(`__DTAtools_supported_datasets__`)}")
    }
  }
)

#' @title get max number of files
#' @description
#' Returns the sum of max number of files specified all associated DTAFile
#' objects.
#' @param x An object of class DTADataSet
#' @return numeric: number of files
#' @examples
#' \dontrun{
#' column_format <- max_number_of_files(dtafileinfo)
#' }
#' @name max_number_of_files-DTADataSet
if (!exists("max_number_of_files", mode = "function")) {
  max_number_of_files <- new_generic("max_number_of_files", "x")
}
#' @export
method(max_number_of_files, DTADataSet) <- function(x) {
  sum(unlist(sapply(x@fileinfo, max_number_of_files)))
}


#' @title get min number of files
#' @description
#' Returns the sum of min number of files specified all associated DTAFile
#' objects.
#' @param x An object of class DTADataSet
#' @return numeric: number of files
#' @examples
#' \dontrun{
#' column_format <- min_number_of_files(dtafileinfo)
#' }
#' @name min_number_of_files-DTADataSet
if (!exists("min_number_of_files", mode = "function")) {
  min_number_of_files <- new_generic("min_number_of_files", "x")
}
#' @export
method(min_number_of_files, DTADataSet) <- function(x) {
  sum(unlist(sapply(x@fileinfo, min_number_of_files)))
}

#' @title Print Method for DTADataSet
#' @description Print a summary of a DTADataSet object.
#' @param x A DTADataSet object.
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @importFrom stringr str_c str_glue
#' @examples
#' library(DTAtools)
#' print(create_example_DTADataSetTabular())
#' @name print
#' @export
method(print, DTADataSet) <- function(x) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTADataSet}> : {.field {x@name}}")
  
  if (is.null(x@fileinfo) || length(x@fileinfo) == 0) {
    cli_alert("Fileinfo entries: {.emph none}")
  } else {
    cli_alert_info("Fileinfo entries: {length(x@fileinfo)}")
  }

  invisible(x)
}

