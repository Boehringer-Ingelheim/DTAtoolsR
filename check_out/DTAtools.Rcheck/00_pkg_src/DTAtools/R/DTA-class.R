#' @title DTA Class
#' @description This class helps checking validity of data tables against transmission
#'  specifications in data transfer agreements (DTA). Also it provides a way to
#'  generate DTA/DTS documents from specifications.
#' @import S7
#' @importFrom cli cli_h1
#'
#' @param datasets A named list of DTADataSet objects.
#' @param metadata A DTAMetaData object.
#' @param ... If metadata is not set, additional arguments are passed to
#'   DTAMetaData(...).
#' @return An object of class DTA.
#'
#' @examples
#'
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' tables <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTADataSet object
#' data_obj <- DTADataSetTabular(
#'   name = "example",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   tables = tables
#' )
#'
#' DTA(
#'   datasets = list(data = data_obj),
#'   metadata = create_example_DTAMetaData()
#' )
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
#' @param ... Not used by current methods; reserved for future extensions.
#' @return A list with metadata information
#' @examples
#' library(DTAtools)
#' dta_obj <- create_example_DTA()
#' metadata(dta_obj)
#' @name metadata
#' @export
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
#' library(DTAtools)
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

#' @title Extract single dataset with [[
#' @description
#' Extract a single \code{DTADataSet} from a \code{DTA} object using
#' double-bracket notation.
#' @param x An object of class \code{DTA}.
#' @param i A single character name or single numeric index.
#' @return A single \code{DTADataSet} object.
#' @examples
#'   dta <- create_example_DTA()
#'   dta[[1]]
#'   dta[["demographics"]]
#' @usage x[[i]]
#' @name double-bracket
#' @aliases [[
#' @export
method(`[[`, DTA) <- function(x, i) {
  if (!is.character(i) && !is.numeric(i)) {
    cli_abort("'i' must be a character name or numeric index.")
  }
  if (length(i) != 1) {
    cli_abort("'i' must be a single value. Use '[' to extract multiple datasets.")
  }
  datasets(x, i)
}

#' @title Extract multiple datasets with [
#' @description
#' Extract one or more \code{DTADataSet} objects from a \code{DTA} object using
#' single-bracket notation. Always returns a named list.
#' @param x An object of class \code{DTA}.
#' @param i A character vector of names or a numeric index vector.
#' @return A named list of \code{DTADataSet} objects.
#' @examples
#'   dta <- create_example_DTA()
#'   dta[c(1, 2)]
#'   dta[c("demographics", "vitals")]
#' @usage x[i]
#' @name single-bracket
#' @aliases [
#' @export
method(`[`, DTA) <- function(x, i) {
  if (!is.character(i) && !is.numeric(i)) {
    cli_abort("'i' must be a character vector of names or a numeric index vector.")
  }
  if (is.numeric(i)) {
    if (any(i < 1) || any(i > length(x@datasets))) {
      cli_abort("Numeric index out of bounds.")
    }
    return(x@datasets[i])
  }
  missing_names <- setdiff(i, names(x@datasets))
  if (length(missing_names) > 0) {
    cli_abort("The following dataset{?s} not found: {.field {missing_names}}")
  }
  x@datasets[i]
}

#' @title Load a file into a DTA or DTADataSet object
#' @description
#' S7 generic. Dispatches to the appropriate method based on the class of \code{x}.
#' Use the \code{DTA} method to load a file into a named dataset within a full
#' DTA object; use the \code{DTADataSetTabular} method to load directly into a
#' standalone dataset.
#' @param x A \code{DTA} or \code{DTADataSetTabular} object.
#' @param ... Additional arguments passed to the method.
#' @return The updated object.
#' @name load_file
#' @export
load_file <- new_generic("load_file", "x")


#' @title Load file into DTA object
#' @description
#' Reads a file into one dataset contained in a \code{DTA} object by dataset
#' name or index.
#' @param x An object of class \code{DTA}.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{dataset}{Single character dataset name or numeric dataset index.}
#'     \item{file}{Path to the input file to be read.}
#'     \item{handler_index}{Single character or numeric index selecting the file
#'       handler within the dataset. Defaults to \code{1}.}
#'     \item{name}{Optional name under which the loaded table should be stored.
#'       Defaults to \code{basename(file)}.}
#'   }
#' @return The updated \code{DTA} object.
#' @usage load_file(x, ...)
#' @name load_file
#' @export
method(load_file, DTA) <- function(
  x,
  dataset,
  file,
  handler_index = 1,
  name = tools::file_path_sans_ext(basename(file)),
  ...
) {
  dataset_object <- datasets(x, dataset)
  dataset_object <- load_file(
    dataset_object,
    handler_index = handler_index,
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


#' @title Check DTA Object
#' @description
#' Validates all datasets within a \code{DTA} object, or a specific dataset.
#' Provides comprehensive validation summary across all datasets.
#' @param x An object of class \code{DTA}.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{datasets}{Optional. A character vector of dataset names or numeric
#'       indices to validate. If NULL (default), validates all datasets.}
#'     \item{force}{Logical. If TRUE, forces re-validation even if unchanged.
#'       Default is FALSE.}
#'     \item{persist}{Logical. If TRUE (default), persists validation artifacts
#'       to disk.}
#'     \item{artifact_dir}{Character or NULL. Directory for persisted artifacts.
#'       If NULL, uses default validation artifact directory per dataset.}
#'     \item{quiet}{Logical. If TRUE, suppresses console output. Default is FALSE.}
#'   }
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success cli_alert_danger cli_abort
#' @return Invisibly returns the updated \code{DTA} object \code{x} with all
#'   validated datasets having their \code{validation_index} and
#'   \code{validation_store} populated. A \code{"last_validation_summary"}
#'   attribute is attached with a data.frame of columns: dataset, n_targets,
#'   n_validated, n_valid, n_invalid, n_skipped.
#' @examples
#'   dta <- create_example_DTA()
#'   # Check all datasets
#'   check(dta)
#'   # Check specific dataset by name
#'   check(dta, datasets = "demographics")
#'   # Check by index
#'   check(dta, datasets = 1)
#' @usage check(x, ...)
#' @name check
#' @export
method(check, DTA) <- function(
  x,
  datasets = NULL,
  force = FALSE,
  persist = TRUE,
  artifact_dir = NULL,
  quiet = FALSE,
  validation_run = NULL
) {
  if (is.null(x@datasets) || length(x@datasets) == 0) {
    cli_abort("DTA object has no datasets to check.")
  }

  # Determine which datasets to validate
  target_datasets <- if (is.null(datasets)) {
    names(x@datasets)
  } else {
    if (is.numeric(datasets)) {
      if (any(datasets < 1) || any(datasets > length(x@datasets))) {
        cli_abort("Dataset index out of bounds.")
      }
      names(x@datasets)[datasets]
    } else if (is.character(datasets)) {
      missing <- setdiff(datasets, names(x@datasets))
      if (length(missing) > 0) {
        cli_abort("The following dataset{?s} not found: {.field {missing}}")
      }
      datasets
    } else {
      cli_abort("'datasets' must be NULL, a character vector, or a numeric vector.")
    }
  }

  n_datasets <- length(target_datasets)
  dataset_word <- if (n_datasets == 1) "Dataset" else "Datasets"
  if (!isTRUE(quiet)) {
    cli::cli_alert_info(paste0("Validating ", n_datasets, " ", dataset_word))
  }

  if (is.null(validation_run)) {
    validation_run <- dta_new_validation_run_id()
  }

  summary_rows <- list()

  for (ds_name in target_datasets) {
    ds <- x@datasets[[ds_name]]

    if (!inherits(ds, "DTAtools::DTADataSet")) {
      if (!isTRUE(quiet)) {
        cli_abort(paste0("Dataset '", ds_name, "' is not a DTADataSet object."))
      }
      next
    }

    if (!isTRUE(quiet)) {
      cli::cli_h1(paste0("Dataset: ", ds_name))
    }

    # Check the dataset. `check()` returns a (possibly new) validated copy of
    # `ds` since S7 objects use copy-on-modify semantics, so the result must
    # be captured and written back into `x@datasets` - otherwise the
    # validation state (and any failures) are silently discarded and the
    # subsequent summary is computed from the stale, unvalidated object.
    ds <- check(
      ds,
      tables = NULL,
      force = force,
      persist = persist,
      artifact_dir = artifact_dir,
      quiet = quiet,
      validation_run = validation_run
    )
    x@datasets[[ds_name]] <- ds

    # Get validation summary for this dataset
    val_status <- validation_status(ds)
    n_targets <- nrow(val_status)
    n_validated <- sum(val_status$status == "validated", na.rm = TRUE)
    n_valid <- sum(val_status$ok == TRUE, na.rm = TRUE)
    n_invalid <- sum(val_status$ok == FALSE, na.rm = TRUE)
    n_skipped <- sum(val_status$status == "skipped", na.rm = TRUE)

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      dataset = ds_name,
      n_targets = n_targets,
      n_validated = n_validated,
      n_valid = n_valid,
      n_invalid = n_invalid,
      n_skipped = n_skipped,
      stringsAsFactors = FALSE
    )

    # Print summary for this dataset
    if (!isTRUE(quiet)) {
      if (n_invalid > 0) {
        table_word <- if (n_validated == 1) "table" else "tables"
        cli_alert_danger(
          paste0(n_validated, " ", table_word, " validated: ", n_valid, " valid, ", n_invalid, " INVALID")
        )
      } else {
        table_word <- if (n_validated == 1) "table" else "tables"
        cli_alert_success(
          paste0(n_validated, " ", table_word, " validated: all valid")
        )
      }
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  # Overall summary
  total_invalid <- sum(summary_df$n_invalid, na.rm = TRUE)
  if (!isTRUE(quiet)) {
    cli::cli_rule("Validation Summary")
    if (total_invalid > 0) {
      invalid_word <- if (total_invalid == 1) "table" else "tables"
      cli_alert_danger(paste0("Validation FAILED: ", total_invalid, " ", invalid_word, " with validation errors"))
    } else {
      cli_alert_success("Validation PASSED: All datasets are valid")
    }
  }

  attr(x, "last_validation_summary") <- summary_df
  invisible(x)
}


#' @title Print DTA Object
#' @description
#' Print method for DTA objects.
#' @param x An object of class DTA
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_h1 cli_alert cli_text cli_div
#' @examples
#'   dta_obj <- create_example_DTA()
#'   print(dta_obj)
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
#'   example_dta <- create_example_DTA()
#'   print(example_dta)
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

  # Metadata is optional: a DTA may carry datasets without a metadata section
  # (see dta_from_list()). Do not abort here on a missing 'metadata' element.
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

  if (is.null(x$datasets)) {
    cli_alert_warning("No 'datasets' section found in list")
    x$datasets <- list()
  }

  # Metadata is OPTIONAL: a DTA may carry datasets without any metadata. When the
  # 'metadata' section is absent, build an empty DTAMetaData rather than aborting.
  if (is.null(x$metadata)) {
    cli_alert_warning("No 'metadata' section found; creating a DTA without metadata")
    metadata <- DTAMetaData()
  } else {
    # Validate metadata structure
    if (is.null(x$metadata$title)) {
      cli_abort("Metadata section must contain 'title' field")
    }

    if (is.null(x$metadata$version)) {
      cli_abort("Metadata section must contain 'version' field")
    }

    # Create metadata object
    metadata <- do.call(DTAMetaData, x$metadata)
  }

  # Create dataset objects
  datasets_list <- dta_dataset_from_list(x$datasets)

  # Create and return DTA object
  DTA(
    datasets = datasets_list,
    metadata = metadata
  )
}
