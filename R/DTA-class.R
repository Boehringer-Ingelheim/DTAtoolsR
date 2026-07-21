#' @title DTA Class
#' @description This class helps checking validity of data tables against transmission
#'  specifications in data transfer agreements (DTA). Also it provides a way to
#'  generate DTA/DTS documents from specifications.
#' @import S7
#' @importFrom cli cli_h1
#'
#' @param datasets A names list of DTADataSet objects
#' @param metadata a DTAMetadata object
#' @param ... if metadata is not set, ... arguments will be passed to create DTAMetadata(...)
#' @return An object of class DTA.
#'
#' @examples
#'
#' \dontrun{
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' tables <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTADataSet object
#' data_obj <- DTADataSet(DTAColumnSpecCollection, tables)
#'
#' DTA(datasets = list(data = data_obj))
#' }
#' @export
DTA <- S7::new_class(
  "DTA",
  constructor = function(
    datasets = NULL,
    metadata = NULL,
    ...
  ) {
    if (inherits(datasets, "DTAtools::DTADataSet")) {
      datasets <- list(datasets)
      names(datasets) <- datasets[[1]]@name
    }
    if (is.list(datasets) && !is.null(datasets) && is.null(names(datasets))) {
      names(datasets) <- vapply(datasets, function(x) x@name, character(1))
    }

    if (is.null(metadata)) {
      metadata <- DTAMetaData(...)
    }
    new_object(
      S7_object(),
      datasets = datasets,
      metadata = metadata
    )
  },
  properties = list(
    datasets = class_list,
    metadata = class_DTAMetaData
  )
)


#' @title Get Metadata
#' @description
#' Method to get Metadata from a DTA object.
#' @param x An object of class DTA
#' @return A list with metadata information
#' @examples
#' library(DTAtools)
#' dta_obj <- create_example_DTA()
#' metadata(dta_obj)
#' @name metadata
#' @rdname metadata-DTA
metadata <- new_generic("metadata", "x")
#' @export
method(metadata, DTA) <- function(x) {
  return(x@metadata)
}


#' @title Get datasets
#' @description
#' Method to get one or more datasets from a DTA object.
#' @importFrom cli cli_alert_info cli_abort
#' @param x An object of class DTA.
#' @param name Optional single character or single integer. if NULL, returns a
#' list of all datasets. If character, returns the datasets with the specified name.
#' If integer, returns the datasets at the specified index.
#' @return Either a list of DTADataSet objects or a single DTADataSet.
#' @examples
#' libary(DTAtools)
#' x <- create_example_DTA()
#' datasets(x)
#' datasets(x, "vitals")
#' datasets(x, 1)
#' @name datasets
#' @export
if (!exists("datasets", mode = "function")) {
  datasets <- new_generic("datasets", "x")
}

#' @export
method(datasets, DTA) <- function(x, name = NULL) {
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
  all_datasets <- x@datasets

  if (is.null(name)) {
    return(all_datasets)
  }

  if (is.numeric(name)) {
    if (any(name < 1) || any(name > length(all_datasets))) {
      cli_abort("Numeric 'name' index out of bounds.")
    }
    return(all_datasets[[name]])
  }

  missing <- setdiff(name, names(all_datasets))
  if (length(missing) > 0) {
    cli_abort("The following datasets{?s} not found: {.field {missing}}")
  }

  return(all_datasets[[name]])
}

if (!exists("read_file", mode = "function")) {
  read_file <- new_generic("read_file", "x")
}


#' @title Read file into DTA
#' @description
#' Reads a file into one dataset contained in a \code{DTA} object by dataset
#' name or index.
#' @param x An object of class \code{DTA}.
#' @param dataset Single character dataset name or numeric dataset index.
#' @param file Path to the input file to be read.
#' @param index Single character or numeric index selecting the file handler
#' within the dataset. Defaults to \code{1}.
#' @param name Optional name under which the loaded table should be stored.
#' Defaults to \code{basename(file)}.
#' @param ... Additional arguments passed through.
#' @return The updated \code{DTA} object.
#' @name read_file
#' @export
method(read_file, DTA) <- function(
  x,
  dataset,
  file,
  index = 1,
  name = basename(file),
  ...
) {
  dataset_object <- datasets(x, dataset)
  dataset_object <- read_file(
    dataset_object,
    index = index,
    file = file,
    name = name,
    ...
  )

  if (is.numeric(dataset)) {
    x@datasets[[dataset]] <- dataset_object
  } else {
    x@datasets[[dataset_object@name]] <- dataset_object
  }

  x
}


#' @title Print DTA Object
#' @description
#' Print method for DTA objects.
#' @param x An object of class DTA
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_h1 cli_alert cli_text cli_div
#' @examples
#' \dontrun{
#'   print(dta_obj)
#' }
#' @name print
#' @export
method(print, DTA) <- function(x, ...) {
  cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTA}>")
  print_short_info(x@metadata)

  n_ds <- length(x@datasets)

  if (!is.null(x@datasets) && n_ds > 0) {
    ds_names <- names(x@datasets)

    if (n_ds > 10) {
      shown_names <- c(ds_names[1:9], "...", ds_names[n_ds])
    } else {
      shown_names <- ds_names
    }

    alert_message <- paste0(
      "Datasets (",
      n_ds,
      "): ",
      paste(paste0("{.field ", shown_names, "}"), collapse = ", ")
    )
    cli_alert_info(alert_message)
  } else {
    cli_alert_info("Datasets: {.emph none}")
  }

  invisible(x)
}

#' @title Create Example DTA Object
#' @description
#' Creates an example DTA object for demonstration purposes.
#' @param index index of the example to create
#' @importFrom cli cli_abort
#' @return An object of class DTA with example data
#' @examples
#' \dontrun{
#'   example_dta <- create_example_DTA()
#'   print(example_dta)
#' }
#' @export
create_example_DTA <- function(index = 1) {
  switch(
    index,
    `1` = {
      DTA(
        datasets = list(
          create_example_DTADataSetTabular(2),
          create_example_DTADataSetTabular(3)
        ),
        metadata = create_example_DTAMetaData()
      )
    },
    `2` = {},
    cli_abort("No example found with index {index}.")
  )
}


#' @title Read DTA from YAML
#' @description
#' Constructs a DTA object from a YAML file specification.
#' @param file Path to the YAML file containing DTA specification
#' @importFrom yaml read_yaml
#' @importFrom cli cli_abort cli_alert_warning
#' @return An object of class DTA
#' @examples
#' require(DTAtools)
#' file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
#' dta <- read_dta_from_yaml(file)
#' @export
read_dta_from_yaml <- function(file) {
  if (!file.exists(file)) {
    cli_abort("YAML file does not exist: {.file {file}}")
  }

  yaml_data <- yaml::read_yaml(file)

  # Check required top-level elements
  if (is.null(yaml_data$metadata)) {
    cli_abort("YAML file must contain 'metadata' section")
  }

  dta_from_list(yaml_data)
}


#' @title Read DTA from List
#' @description
#' Constructs a DTA object from a list.
#' @param x list
#' @importFrom cli cli_abort cli_alert_warning
#' @return An object of class DTA
#' @examples
#' require(DTAtools)
#' file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
#' yaml_data <- yaml::read_yaml(file)
#' dta <- dta_from_list(yaml_data)
#'
#' @export
dta_from_list <- function(x) {
  if (!is.list(x)) {
    cli_abort("x is not a list")
  }

  # Check required top-level elements
  if (is.null(x$metadata)) {
    cli_abort("x contain 'metadata' section")
  }

  if (is.null(x$datasets)) {
    cli_alert_warning("No 'datasets' section found in list")
    x$datasets <- list()
  }

  # Validate metadata structure
  if (is.null(x$metadata$title)) {
    cli_abort("Metadata section must contain 'title' field")
  }

  if (is.null(x$metadata$version)) {
    cli_abort("Metadata section must contain 'version' field")
  }

  # Create metadata object
  metadata <- do.call(DTAMetaData, x$metadata)

  # Create dataset objects
  datasets_list <- dta_dataset_from_list(x$datasets)

  # Create and return DTA object
  DTA(
    datasets = datasets_list,
    metadata = metadata
  )
}
