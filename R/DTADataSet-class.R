#' @title DTADataSet Class
#' @description Class to handle data from files
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom stringr str_flatten_comma
#' @param name Character. Name of the container.
#' @param type Character. Dataset type, must be one of `__DTAtools_supported_dataset_types__`.
#' @param files a list of DTAFile objects specifying input file information.
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
    files = list(),
    description = NULL,
    template_source = NULL,
    template_version = NULL,
    template_date = NULL
  ) {
    if(inherits(files, "DTAtools::DTAFile")) {
      files = list(files)
    }

    new_object(
      S7_object(),
      name = name,
      type = type,
      files = files
    )
  },
  properties = list(
    name = class_character, 
    type = class_character,
    files = class_list, # list of DTAFile
    template_source = class_character_or_null,
    template_version = class_character_or_null,
    template_date = class_character_or_null,
    description = class_character_or_null
  ),
  validator = function(self) {
    if (!is.character(self@name) || length(self@name) != 1 || self@name == "") {
      cli_abort("Property 'name' must be a single non-empty string.")
    }
    if (!all(sapply(self@files, inherits, "DTAtools::DTAFile"))) {
      cli_abort("All elements in 'files' must be of class 'DTAFile'")
    }
    if (!self@type %in% `__DTAtools_supported_dataset_types__`) {
      cli_abort("Property 'type' is '{self@type})', must be one of: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}")
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
#' column_format <- max_number_of_files(dtafiles)
#' }
#' @name max_number_of_files-DTADataSet
if (!exists("max_number_of_files", mode = "function")) {
  max_number_of_files <- new_generic("max_number_of_files", "x")
}
#' @export
method(max_number_of_files, DTADataSet) <- function(x) {
  sum(unlist(sapply(x@files, max_number_of_files)))
}


#' @title get min number of files
#' @description
#' Returns the sum of min number of files specified all associated DTAFile
#' objects.
#' @param x An object of class DTADataSet
#' @return numeric: number of files
#' @examples
#' \dontrun{
#' column_format <- min_number_of_files(dtafiles)
#' }
#' @name min_number_of_files-DTADataSet
if (!exists("min_number_of_files", mode = "function")) {
  min_number_of_files <- new_generic("min_number_of_files", "x")
}
#' @export
method(min_number_of_files, DTADataSet) <- function(x) {
  sum(unlist(sapply(x@files, min_number_of_files)))
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
  
  print_dataset_info(x)
  invisible(x)
}

#' @title Print Dataset Information
#' @description
#' Prints information about a \code{DTADataSet} object, including template source, version, date, and file information.
#'
#' @param x A \code{DTADataSet} object whose information is to be printed.
#'
#' @details
#' This method displays the template source, version, and date if available. It also summarizes the file information entries, indicating if none are present.
#'
#' @return
#' No return value. This function is called for its side effects (printing to the console).
#'
#' @seealso
#' \code{\link{DTADataSet}}
#'
#' @examples
#' # Assuming 'ds' is a DTADataSet object:
#' print_dataset_info(ds)
#'
#' @export
print_dataset_info <- new_generic("print_dataset_info", "x")
method(print_dataset_info, DTADataSet) <- function(x) {
  if (!is.null(x@description)) {
    cli_text("- Description: {x@description}")
  }
  
  if (!is.null(x@template_source)) {
    cli_text("- Template source: {.emph {x@template_source}}")
  }
  
  if (!is.null(x@template_source)) {
    cli_text("- Template source: {.emph {x@template_source}}")
  }
  if (!is.null(x@template_version)) {
    cli_text("- Template version: {.emph {x@template_version}}")
  }
  if (!is.null(x@template_date)) {
    cli_text("- Template date: {.emph {x@template_date}}")
  }
  if (is.null(x@files) || length(x@files) == 0) {
    cli_alert("Fileinfo entries: {.emph none}")
  } else {
    cli_alert_info("Fileinfo entries: {length(x@files)}")
  }
}


#' @title Read DTADataSet from YAML
#' @description
#' Constructs a DTADataSet object from a YAML file specification.
#' @param file Path to the YAML file containing DTADataSet specification
#' @importFrom yaml read_yaml
#' @importFrom cli cli_abort
#' @return An object of class DTADataSet
#' @examples
#' require(DTAtools)
#' file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#' dta_obj <- read_dta_dataset_from_yaml(file)
#' @export
read_dta_dataset_from_yaml <- function(file) {
  if (!file.exists(file)) {
    cli_abort("YAML file does not exist: {.file {file}}")
  }
  
  yaml_data <- yaml::read_yaml(file)
  
  dta_dataset_from_list(yaml_data)
}


#' @title DTADataSet from list
#' @description
#' Constructs a DTADataSet object from a list
#' @param x List
#' @param recursive Logical, if TRUE (default) processes nested datasets
#' @importFrom cli cli_abort
#' @return An object of class DTADataSet
#' @examples
#' require(DTAtools)
#' file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#' yaml_data <- yaml::read_yaml(file)
#' dta_obj <- dta_dataset_from_list(yaml_data)
#' @export
dta_dataset_from_list <- function(x, recursive = TRUE) {
  if(is.null(x$name)) {
    if(!is.null(x[[1]]$name)) {
      # there are multiple datasets which need to be processed separately
      return(lapply(x, dta_dataset_from_list, recursive = FALSE))
    } else {
      cli_abort("List must contain a 'name' field or be a list of datasets.")
    }
  }
  
  if (is.null(x$type)) {
    cli_abort("Dataset '{x$name}' must contain a 'type'")
  }

  do.call(DTADataSetFactory, x)
}
