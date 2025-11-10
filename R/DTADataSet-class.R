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
DTADataSet <- S7::new_class(
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
    if (inherits(files, "DTAtools::DTAFile")) {
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
      cli_abort(
        "Property 'type' is '{self@type})', must be one of: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}"
      )
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

  print_info(x)
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
#' print_info(ds)
#' @name print_info
#' @export
if (!exists("print_info", mode = "function")) {
  print_info <- new_generic("print_info", "x")
}
method(print_info, DTADataSet) <- function(x) {
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
    cli_alert_info("Files: {.emph none}")
  } else {
    min_number_of_files <- min_number_of_files(x)
    max_number_of_files <- max_number_of_files(x)
    entry_label <- if (length(x@files) == 1) "entry" else "entries"
    file_label_min <- if (min_number_of_files == 1) "file" else "files"
    file_label_max <- if (max_number_of_files == 1) "file" else "files"
    if (min_number_of_files == max_number_of_files) {
      alert_message <- str_glue(
        "Files: {length(x@files)} {entry_label} with a total of {min_number_of_files} {file_label_min}"
      )
    } else {
      alert_message <- str_glue(
        "Files: {length(x@files)} {entry_label} with a total of {min_number_of_files} to {max_number_of_files} {file_label_max}"
      )
    }
    cli_alert_info(alert_message)
    for (f in x@files) {
      print_short_info(f)
    }
  }
}


#' @title Print Short Information for DTADataset
#' @description
#' Prints short information about a \code{DTADataSet} object.
#'
#' @param x A \code{DTADataSet} object whose information is to be printed.
#'
#' @details
#' This method displays the template source, version, and date if available. It also summarizes the file information entries, indicating if none are present.
#'
#' @importFrom cli cli_alert_info cli_alert
#' @importFrom stringr str_c str_glue
#' @return
#' No return value. This function is called for its side effects (printing to the console).
#'
#' @seealso
#' \code{\link{DTADataSet}}
#'
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' print_short_info(ds)
#' @name print_short_info
#' @export
if (!exists("print_short_info", mode = "function")) {
  print_short_info <- new_generic("print_short_info", "x")
}
method(print_short_info, DTADataSet) <- function(x) {
  min_n <- min_number_of_files(x)
  max_n <- max_number_of_files(x)
  if (max_n == 0) {
    file_info <- "0 files"
  } else if (min_n == max_n) {
    if (min_n == 1) {
      file_info <- "1 file"
    } else {
      file_info <- str_glue("{min_n} files")
    }
  } else {
    file_info <- str_glue("{min_n} to {max_n} files")
  }

  if (max_n == 0) {
    message <- str_c('Files: none associated, type: {x@type}')
  } else {
    message <- paste0(
      "Files: ",
      str_c('{.field ', names(x@name), '}'),
      str_glue(" ({file_info}, {x@type})")
    )
  }

  cli_alert(message)

  return(invisible(x))
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
#' dta <- read_dataset_from_yaml(file)
#' @export
read_dataset_from_yaml <- function(file) {
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
#' yaml_dataset <- yaml::read_yaml(file)
#' dataset <- dta_dataset_from_list(yaml_dataset)
#' @export
dta_dataset_from_list <- function(x, recursive = TRUE) {
  if (is.null(x$name)) {
    if (!is.null(x[[1]]$name)) {
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


#' @title Get files from DTADataSet Object
#' @description
#' Method to get files from DTADataSet object.
#' @param x An object of class DTADataSet.
#' @param name Optional single character or single integer. if NULL, returns a
#' list of all files. If character, returns the datasets with the specified name.
#' If integer, returns the datasets at the specified index.
#' @param ... void
#' @return A DTAColumnSpecCollection object.
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' files(ds)
#' @name files-DTADataSet
#' @export
files <- new_generic("files", "x")

#' @export
method(files, DTADataSet) <- function(x, name = NULL) {
  if (
    !is.null(name) &&
      !is.character(name) &&
      !is.numeric(name) &&
      length(name) != 1
  ) {
    cli_abort(
      "'name' must be a single character vector, single numeric index or NULL."
    )
  }
  all_files <- x@files

  if (is.null(name)) {
    return(all_files)
  }

  if (is.numeric(name)) {
    if (any(name < 1) || any(name > length(all_files))) {
      cli_abort("Numeric 'name' index out of bounds.")
    }
    return(all_files[[name]])
  }

  missing <- setdiff(name, names(all_files))
  if (length(missing) > 0) {
    cli_abort("The following datasets{?s} not found: {.field {missing}}")
  }

  return(all_files[[name]])
}




#' @title Get tables from DTADataSet Object
#' @description
#' Method to get tables from DTADataSet object.
#' @param x An object of class DTADataSet.
#' @param ... void
#' @return A DTAColumnSpecCollection object.
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' tables(ds)
#' @name tables-DTADataSet
#' @export
tables <- new_generic("tables", "x")

#' @export
method(tables, DTADataSet) <- function(x) {
  return(x@tables)
}




