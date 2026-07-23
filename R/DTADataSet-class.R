#' @title DTADataSet Class
#' @description Class to handle data from files
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom stringr str_flatten_comma
#' @param name Character. Name of the container.
#' @param type Character. Dataset type, must be one of `__DTAtools_supported_dataset_types__`.
#' @param files a list of DTAFile objects specifying input file information.
#' @return An object of class DTADataSet.
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
#' @return No return value. This function is called for its side effects
#'   (printing to the console).
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
#' @return No return value. This function is called for its side effects
#'   (printing to the console).
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
#' @param ... Additional arguments (not used).
#' @return A list of DTAFile objects, or a single DTAFile object when a name
#'   or index is provided.
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
#' @param i index: optional single character or integer or vector of characters 
#' or integers to select specific tables. if NULL (default), returns all tables.
#' @param ... Additional arguments (not used).
#' @return A list of tables, or a single table when one index/name is provided.
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' tables(ds)
#' @name tables-DTADataSet
#' @export
tables <- new_generic("tables", "x")

#' @export
method(tables, DTADataSet) <- function(x, i = NULL) {
  if(is.null(i)) {
    return(x@tables)
  } else if(length(i) == 1) {
    return(x@tables[[i]])
  } else {
    return(x@tables[i])
  }
}


if (!exists("load_file", mode = "function")) {
  load_file <- new_generic("load_file", "x")
}


#' @title Load file into DTADataSet
#' @description
#' Convenience wrapper that dispatches to \code{load_file()} for a dataset.
#' @param x An object of class \code{DTADataSet}.
#' @param handler_index Single character or numeric index selecting the file handler
#' within the dataset. Defaults to \code{1}.
#' @param file Path to the input file to be read.
#' @param name Optional name under which the loaded table should be stored.
#' Defaults to \code{basename(file)}.
#' @param ... Additional arguments passed through.
#' @return The updated dataset object.
#' @name load_file
#' @export
method(load_file, DTADataSet) <- function(
  x,
  handler_index = 1,
  file,
  name = tools::file_path_sans_ext(basename(file)),
  ...
) {
  stop("This method needs to be implemented in derived classes.")
}


#' @keywords internal
dta_hash_object <- function(x) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  unname(as.character(tools::md5sum(tmp)))
}

#' @keywords internal
dta_table_id_to_names <- function(x, tables = NULL) {
  all_names <- names(x@tables)

  if (length(all_names) == 0) {
    cli::cli_abort("No tables found in dataset.")
  }

  if (is.null(tables)) {
    return(all_names)
  }

  if (is.numeric(tables)) {
    if (any(tables < 1) || any(tables > length(all_names))) {
      cli::cli_abort("Table index out of bounds.")
    }
    return(all_names[tables])
  }

  if (is.character(tables)) {
    missing <- setdiff(tables, all_names)
    if (length(missing) > 0) {
      cli::cli_abort("Unknown table name(s): {missing}")
    }
    return(tables)
  }

  cli::cli_abort("'tables' must be NULL, numeric, or character.")
}

#' @keywords internal
dta_default_validation_artifact_dir <- function(x) {
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", x@name)
  file.path(tempdir(), "DTAtools_validation", safe_name)
}

#' @keywords internal
dta_validation_result_to_row <- function(table_name, status, index_entry) {
  data.frame(
    table = table_name,
    status = status,
    ok = isTRUE(index_entry$ok),
    validated_at = as.character(index_entry$validated_at),
    run_id = index_entry$run_id,
    n_schema_errors = index_entry$n_schema_errors,
    n_rule_errors = index_entry$n_rule_errors,
    stringsAsFactors = FALSE
  )
}


#' @title Check DTADataSet Tables
#' @description
#' Validates one, many, or all tables in a \code{DTADataSet} subclass against
#' dataset specs. The method expects subclasses to provide table/spec and
#' validation state properties.
#' @param x A \code{DTADataSet} object.
#' @param tables NULL (default), character table names, or numeric table indices.
#' @param force Logical. If \code{FALSE}, validation for unchanged table/spec hash is skipped.
#' @param persist Logical. If \code{TRUE}, full validation result is written as \code{RDS}.
#' @param artifact_dir Character or NULL. Optional output directory for persisted
#' validation artifacts.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @return Invisibly returns \code{x} (modified in place).
#' @name check-DTADataSet
#' @export
S7::method(check, DTADataSet) <- function(
  x,
  tables = NULL,
  force = FALSE,
  persist = TRUE,
  artifact_dir = NULL,
  quiet = FALSE
) {
  has_specs <- !is.null(tryCatch(x@specs, error = function(e) NULL))
  has_tables <- !is.null(tryCatch(x@tables, error = function(e) NULL))
  has_validation_index <- !is.null(tryCatch(x@validation_index, error = function(e) NULL))
  has_validation_store <- !is.null(tryCatch(x@validation_store, error = function(e) NULL))

  if (!has_specs || !has_tables || !has_validation_index || !has_validation_store) {
    cli::cli_abort(
      "check() requires a DTADataSet subclass with properties: specs, tables, validation_index, and validation_store."
    )
  }

  target_tables <- dta_table_id_to_names(x, tables)
  specs_hash <- dta_hash_object(as.list(x@specs))
  output_rows <- list()

  if (persist) {
    if (is.null(artifact_dir)) {
      artifact_dir <- if (!is.null(x@validation_artifact_dir)) {
        x@validation_artifact_dir
      } else {
        dta_default_validation_artifact_dir(x)
      }
    }
    dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
    x@validation_artifact_dir <- artifact_dir
  }

  for (table_name in target_tables) {
    current_table <- x@tables[[table_name]]
    current_df <- as.data.frame(current_table)
    table_hash <- dta_hash_object(current_df)

    previous <- x@validation_index[[table_name]]
    unchanged <- !is.null(previous) &&
      identical(previous$table_hash, table_hash) &&
      identical(previous$specs_hash, specs_hash)

    if (!force && unchanged) {
      output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
        table_name = table_name,
        status = "skipped",
        index_entry = previous
      )
      next
    }

    details <- validate_table_detailed(x@specs, current_df, verbose = !isTRUE(quiet))
    run_id <- format(Sys.time(), "%Y%m%dT%H%M%OS3")
    artifact_path <- NULL

    if (persist) {
      safe_table <- gsub("[^A-Za-z0-9_-]", "_", table_name)
      table_dir <- file.path(artifact_dir, safe_table)
      dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
      artifact_path <- file.path(table_dir, paste0(run_id, ".rds"))
      saveRDS(details, artifact_path)
    }

    index_entry <- list(
      validated_at = Sys.time(),
      ok = isTRUE(details$ok),
      table_hash = table_hash,
      specs_hash = specs_hash,
      n_schema_errors = details$n_schema_errors,
      n_rule_errors = details$n_rule_errors,
      run_id = run_id,
      artifact_path = artifact_path
    )

    x@validation_index[[table_name]] <- index_entry
    x@validation_store[[table_name]] <- details

    output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = index_entry
    )
  }

  summary_df <- do.call(rbind, output_rows)
  attr(x, "last_validation_summary") <- summary_df

  invisible(x)
}

