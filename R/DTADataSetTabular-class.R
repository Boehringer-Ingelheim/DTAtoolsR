#' @title DTADataSetTabular Class
#' @description Handles tabular data with column specifications and rules.
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom arrow arrow_table
#' @importFrom tools md5sum
#' @param name Character. Name of the container.
#' @param specs A DTAColumnSpecCollection object specifying the column specs.
#' @param files A list of DTAFile objects specifying input file information.
#' @param tables A named list of tabular objects; each table is converted to an
#'   Arrow Table and stored in the dataset.
#' @param description Character or NA. Free-text description of the dataset.
#' @param template_source Character or NA. Source of the template used to
#'   generate the dataset specification.
#' @param template_version Character or NA. Version of the template used.
#' @param template_date Character or NA. Date of the template used.
#' @return An object of class DTADataSetTabular
#' @examples
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' tables <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTADataSetTabular object
#' data_obj <- DTADataSetTabular(
#'   name = "example",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   tables = tables
#' )
#' @export
DTADataSetTabular <- S7::new_class(
  "DTADataSetTabular",
  parent = DTADataSet,
  constructor = function(
    name,
    specs,
    tables = list(),
    files = list(),
    description = NULL,
    template_source = NULL,
    template_version = NULL,
    template_date = NULL
  ) {
    if (inherits(files, "DTAtools::DTAFile")) {
      files = list(files)
    }

    # Type every table by its column specs before it is stored, so the declared
    # type -- not the reader's per-column inference -- decides what each column
    # holds. Values that cannot be represented become NA and are recorded as
    # import issues, both here and on the table itself.
    coerced <- lapply(tables, function(tbl) dta_coerce_table_to_specs(tbl, specs))

    # Transform to arrow tables
    tables <- lapply(coerced, function(result) arrow::as_arrow_table(result$table))

    import_issues <- lapply(coerced, function(result) result$issues)
    import_issues <- import_issues[
      vapply(import_issues, function(issues) nrow(issues) > 0, logical(1))
    ]
    # Subsetting a named list keeps the (empty) names attribute, and a named
    # empty list is not `identical()` to `list()`. Normalise, so "no import
    # issues" is one value rather than two.
    if (length(import_issues) == 0) {
      import_issues <- list()
    }

    new_object(
      .parent = DTADataSet(
        name = name,
        type = "tabular",
        files = files,
        template_source = template_source,
        template_version = template_version,
        template_date = template_date,
        description = description
      ),

      specs = specs,
      tables = tables,
      validation_index = list(),
      validation_store = list(),
      import_issues = import_issues,
      validation_artifact_dir = NULL
    )
  },
  properties = list(
    specs = class_DTAColumnSpecCollection,
    tables = S7::new_property(S7::class_list, default = list()),
    validation_index = S7::new_property(S7::class_list, default = list()),
    validation_store = S7::new_property(S7::class_list, default = list()),
    # Import issues detected while typing a table, keyed by table name.
    import_issues = S7::new_property(S7::class_list, default = list()),
    validation_artifact_dir = class_character_or_null
  ),
  validator = function(self) {
    # check if all elements of list self@tables inherit from "Table"
    if (!inherits(self@specs, "DTAtools::DTAColumnSpecCollection")) {
      cli_abort("Property 'specs' must be of class 'DTAColumnSpecCollection'")
    } 

    if(length(self@tables) > 0 && !all(sapply(self@tables, function(x) inherits(x, "Table")))) {
      cli_abort("All elements of 'tables' must be of class 'Table'")
    }

    #if(length(self@tables) > 0 && !all(sapply(self@tables, function(x) inherits(x, "arrow::ArrowTabular")))) {
    #  cli_abort("All elements of 'tables' must be of class 'arrow::ArrowTabular'")
    #}

    # check if list holding the validation index and validation store are of the same length
    #if(length(self@validation_index) != length(self@validation_store)) {
    #  cli_abort("Properties 'validation_index' and 'validation_store' must be of the same length")
    #}

    if(!is.null(self@validation_artifact_dir) && !dir.exists(self@validation_artifact_dir)) {
      cli_abort("Property 'validation_artifact_dir' must be a valid directory path or NULL")
    }

    # if tables are present, check if the column names of the tables match the column names in the specs if specs are present
    #if(length(self@tables) > 0 && !is.null(self@specs)) {
    #  spec_column_names <- sapply(self@specs@columns, function(x) x@name)
    #  for(table in self@tables) {
    #    table_column_names <- colnames(table)
    #    if(!all(spec_column_names %in% table_column_names)) {
    #      cli_abort("Column names in 'specs' do not match column names in 'tables'")
    #    }
    #  }
    #}

    # if list of tables is present then list of validation index and store cannot be larger than the list of tables
    if(length(self@tables) > 0 && (length(self@validation_index) > length(self@tables) || length(self@validation_store) > length(self@tables) || length(self@import_issues) > length(self@tables))) {
      cli_abort("Properties 'validation_index', 'validation_store' and 'import_issues' cannot be larger than the number of tables in 'tables'")
    }

  }
)


#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTADataSetTabular
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{id}{Character. The ID of the column to retrieve.}
#'   }
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' colspec(ds, "STUDYID")
#' @usage colspec(x, ...)
#' @name colspec
# colspec <- new_generic("colspec", "x") # was already initialized

#' @export
method(colspec, DTADataSetTabular) <- function(x, id) {
  return(colspec(x@specs, id))
}


#' @title Get DTAColumnSpecCollection (specs) from DTADataSetTabular Object
#' @description
#' Method to extract the full DTAColumnSpecCollection from a DTADataSetTabular object.
#' @param x An object of class DTADataSet.
#' @param ... Additional arguments (not used).
#' @return A DTAColumnSpecCollection object.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' specs(ds)
#' @name specs-DTADataSetTabular
#' @export
specs <- new_generic("specs", "x")

#' @export
method(specs, DTADataSetTabular) <- function(x) {
  return(x@specs)
}


#' @title Get table from DTADataSetTabular Object
#' @description
#' Extract a table from the tables in a DTADataSetTabular object.
#' @param x An object of class DTADataSet.
#' @param id Character or numeric. Name or index of the table to retrieve.
#' @param ... Not used by current methods; reserved for future extensions.
#' @return An Arrow Table object.
#' @importFrom cli cli_abort
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' get_table(ds, 1)
#' get_table(ds, "tab1")
#' @name get_table-DTADataSetTabular
#' @export
get_table <- new_generic("get_table", "x", function(x, id = 1, ...) {
  S7_dispatch()
})

#' @export
method(get_table, DTADataSetTabular) <- function(x, id = 1) {
  if (!inherits(x, "DTAtools::DTADataSetTabular")) {
    cli::cli_abort("Input must be a DTADataSetTabular object.")
  }

  tables <- x@tables

  if (length(tables) == 0) {
    cli::cli_abort("No tables found in the container.")
  }

  if (is.character(id)) {
    if (!id %in% names(tables)) {
      cli::cli_abort("No table named {id} found in the container.")
    }
    return(tables[[id]])
  }

  if (is.numeric(id)) {
    if (id < 1 || id > length(tables)) {
      cli::cli_abort("Index {id} is out of bounds.")
    }
    return(tables[[id]])
  }

  cli::cli_abort("Argument 'id' must be a character (name) or numeric (index).")
}


#' @title List of tables labels within DTADataSetTabular Object
#' @description
#' Method to get a all tables labels within a DTADataSetTabular Object.
#' @param x An object of class DTADataSetTabular
#' @param ... Additional arguments (not used).
#' @return A character vector with table names.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' labels(ds)
#' @name labels-DTADataSetTabular
labels <- new_generic("labels", "x")
#' @export
method(labels, DTADataSetTabular) <- function(x) {
  return(names(x@tables))
}

#' @title Write DTA Table to File
#' @description
#' Write a named DTA table saved in a DTADataSetTabular object to a file.
#' @importFrom dplyr arrange across everything
#' @importFrom magrittr %>%
#' @importFrom utils write.table
#' @importFrom R.utils gzip
#' @importFrom cli cli_alert_info cli_alert_success format_message cli_abort
#' @importFrom tools md5sum
#' @export
#' @param DTADataSetTabular An object of class DTADataSet.
#' @param table Character. The name of the table within the DTADataSetTabular object to write.
#' @param filename Character. The name of the file to write to.
#' @param arrange_by Character vector. Columns to arrange the table by. NULL, Table won't be arranged. "all" (Default) -> Table will be arranged by all columns.
#' @param arrange_desc Logical. Whether to arrange the table in descending order. Default is FALSE.
#' @param sep Character. The field separator string. Default is ",".
#' @param na Character. The string to use for missing values in the data. Default is "".
#' @param row.names Logical. Row names provided.
#' @param quote Logical. Use of quotes
#' @param overwrite Logical. Whether to overwrite the file if it exists. Default is FALSE.
#' @param compression Character. Compression method, either "none" or "gzip". Default is "none".
#' @param get_md5sum Logical. Whether to calculate and print the MD5 checksum of the file. MD5SUM and number of rows and columns of file will be also saved in an additional file. Default is TRUE.
#' @param write_md5sum_to_file Logical. Whether to calculate and print the MD5 checksum of the file. MD5SUM and number of rows and columns of file will be also saved in an additional file. Default is TRUE.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @param ... Additional arguments passed to write.table.
#' @return NULL. The function writes the table to a file.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' out_file <- tempfile(fileext = ".tsv")
#' write_table_to_file(
#'   ds,
#'   table = "tab1",
#'   filename = out_file,
#'   sep = "\t",
#'   arrange_by = c("STUDYID", "VISIT")
#' )
#' unlink(out_file)
write_table_to_file <- function(
  DTADataSetTabular,
  table,
  filename,
  arrange_by = "all",
  arrange_desc = FALSE,
  sep = "\t",
  na = "",
  row.names = FALSE,
  overwrite = TRUE,
  quote = FALSE,
  compression = c("none", "gzip"),
  get_md5sum = TRUE,
  write_md5sum_to_file = TRUE,
  quiet = FALSE,
  ...
) {
  compression <- match.arg(compression)

  # Check if the table exists in the DTADataSetTabular
  if (!table %in% names(DTADataSetTabular@tables)) {
    cli::cli_abort(c(
      "Table name not found!",
      x = "Table with the name '{table}' not found in the DTADataSetTabular object.",
      i = "Use 'labels(<DTADataSetTabular>)' to print names of all tables in object."
    ))
  }

  table_data <- DTADataSetTabular@tables[[table]]

  # Arrange the table by specified columns
  if (!is.null(arrange_by)) {
    if (length(arrange_by) == 1 && identical(arrange_by, "all")) {
      if (!isTRUE(quiet)) {
        cli::cli_alert_info("Arrange table by all columns.")
      }
      if (isTRUE(arrange_desc)) {
        table_data <- table_data %>%
          dplyr::arrange(dplyr::across(dplyr::everything(), dplyr::desc))
      } else {
        table_data <- table_data %>%
          dplyr::arrange(dplyr::across(dplyr::everything()))
      }
    } else {
      if (!isTRUE(quiet)) {
        cli::cli_alert_info("Arrange table by {arrange_by}.")
      }
      if (isTRUE(arrange_desc)) {
        table_data <- table_data %>%
          dplyr::arrange(dplyr::across(dplyr::all_of(arrange_by), dplyr::desc))
      } else {
        table_data <- table_data %>%
          dplyr::arrange(!!!rlang::syms(arrange_by))
      }
    }
  }

  # Check if the file exists and handle overwrite
  if (file.exists(filename) && !overwrite) {
    cli::cli_abort(c(
      "{filename} already exists!",
      i = "Specify 'overwrite = TRUE' to overwrite the current file."
    ))
  }

  # Write the table to a file
  if (compression == "gzip") {
    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Write table in gzip format to {filename}.")
    }
    temp_file <- tempfile()
    write.table(
      table_data,
      file = temp_file,
      sep = sep,
      na = na,
      row.names = row.names,
      quote = quote,
      ...
    )
    R.utils::gzip(temp_file, destname = filename, overwrite = overwrite)
  } else {
    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Write table to {filename}.")
    }
    write.table(
      table_data,
      file = filename,
      na = na,
      row.names = row.names,
      sep = sep,
      quote = quote,
      ...
    )
  }

  # Print a success message
  if (!isTRUE(quiet)) {
    cli::cli_alert_success("File {filename} written successfully.")
  }

  # Calculate md5sum
  if (get_md5sum) {
    md5sum <- write_metadata(
      filename,
      table_data,
      write_to_file = write_md5sum_to_file,
      quiet = quiet
    )
  } else {
    md5sum <- NA
  }

  invisible(list(
    tables = table_data,
    table = table,
    md5sum = md5sum
  ))
}


#' @title Get column specs from DTADataSetTabular Object
#' @description
#' Method to get columns specifications from DTADataSetTabular
#' @param x An object of class DTADataSetTabular
#' @param ... Not used by current methods; reserved for future extensions.
#' @return A list with metadata information
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' columns(ds)
#' @name columns
#' @export
columns <- new_generic("columns", "x")
#' @export
method(columns, DTADataSetTabular) <- function(x) {
  return(x@specs@columns)
}

#' @title Get Rules
#' @description
#' Method to get Rules from DTADataSet.
#' @param x An object of class DTADataSetTabular
#' @return A list of DTARule objects, or NULL if no rules are defined.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' rules(ds)
#' @name rules
#' @export
method(rules, DTADataSetTabular) <- function(x, ...) {
  return(x@specs@rules)
}


#' @title Create Example DTADataSetTabular
#' @description
#' S7 method to create and return an example DTADataSetTabular object.
#' @importFrom cli cli_abort
#' @param index Integer. Index of the example to create.
#'
#' @return An example DTADataSetTabular object.
#' @examples
#' library(DTAtools)
#' create_example_DTADataSetTabular()
#' @export
create_example_DTADataSetTabular <- function(index = 1) {
  # Create sample tables
  table1 <- arrow_table(data.frame(
    STUDYID = c("STUDY001", "STUDY001", "STUDY001"),
    SUBJID = c("001", "002", "003"),
    VISIT = c("SCREENING", "BASELINE", "WEEK_4"),
    AGE = c(25, 34, 29)
  ))

  table2 <- arrow_table(data.frame(
    STUDYID = c("STUDY001", "STUDY001", "STUDY001"),
    SUBJID = c("001", "002", "003"),
    PARAM = c("HEIGHT", "WEIGHT", "BMI"),
    AVAL = c(175.2, 68.5, 22.3)
  ))

  switch(
    index,
    `1` = {
      DTADataSetTabular(
        name = "example_container_specs_without_data",
        specs = create_example_DTAColumnSpecCollection(1),
      )
    },
    `2` = {
      DTADataSetTabular(
        name = "demographics",
        specs = create_example_DTAColumnSpecCollection(1),
        tables = list("tab1" = table1)
      )
    },
    `3` = {
      DTADataSetTabular(
        name = "vitals",
        specs = create_example_DTAColumnSpecCollection(2),
        tables = list("tab2" = table2)
      )
    },
    cli::cli_abort("No example available for the provided index.")
  )
}

#' @title Print Method for DTADataSetTabular
#' @description Print a summary of a DTADataSetTabular object.
#' @param x A DTADataSetTabular object.
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @importFrom stringr str_c str_glue
#' @examples
#' library(DTAtools)
#' print(create_example_DTADataSetTabular())
#' @name print
#' @export
method(print, DTADataSetTabular) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTADataSetTabular}> : {.field {x@name}}")

  print_info(x)

  if (!is.null(x@specs)) {
    cli_alert_info("Specs:")
    cli_alert("columns ({length(x@specs@columns)}): {column_preview(x@specs)}")
    cli_alert("rules ({length(x@specs@rules)}): {rule_preview(x@specs)}")
  } else {
    cli_alert_info("Specs: none")
  }

  n_targets <- length(x@tables)

  if (n_targets > 0) {
    table_names <- names(x@tables)

    if (n_targets > 5) {
      shown_names <- c(table_names[1:4], "...", table_names[n_targets])
    } else {
      shown_names <- table_names
    }

    # Build the message with proper cli markup, need paste and paste0
    # instead of stringr functions to work with cli
    alert_message <- paste0(
      "Tables (",
      n_targets,
      "): ",
      paste(paste0("{.field ", shown_names, "}"), collapse = ", ")
    )
    cli_alert_info(alert_message)
  } else {
    cli_alert_info("Tables: {.emph none}")
  }

  invisible(x)
}


#' @title Print Short Information for DTADataSetTabular
#' @description
#' Prints short information about a \code{DTADataSetTabular} object.
#'
#' @param x A \code{DTADataSetTabular} object whose information is to be printed.
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
#' \code{\link{DTADataSetTabular}}
#'
#' @examples
#' library(DTAtools)
#' ds <- create_example_DTADataSetTabular()
#' print_short_info(ds)
#' @name print_short_info
#' @export
method(print_short_info, DTADataSetTabular) <- function(x, ...) {
  #super(print_short_info, x)
  method(print_short_info, DTADataSet)(x)
  if (!is.null(x@specs)) {
    cli_alert(
      "Specs: {length(x@specs@columns)} columns, {length(x@specs@rules)}, rules"
    )
  } else {
    cli_alert("Specs: none")
  }

  n_targets <- length(x@tables)

  if (n_targets > 0) {
    cli_alert("Tables: ({n_targets})")
  } else {
    cli_alert("Tables: {.emph none}")
  }

  return(invisible(x))
}


#' @title loads file
#' @description
#' Load the content of the file into dataset
#' @param x An object of class DTADataSet
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{file}{file to be loaded}
#'     \item{handler_index}{of the filehandler in the files list}
#'     \item{name}{file name, base name per default. is used to store the
#'       table under this name}
#'   }
#' @return object of class DTADataSet with loaded data
#' @examples
#' file_handler <- DTAFileCSV(filename = "clinical_data.csv")
#' ds <- DTADataSetTabular(
#'   name = "demo",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   files = list(file_handler)
#' )
#' file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#' ds <- DTAtools:::load_file(ds, file = file, handler_index = 1)
#' names(tables(ds))
#' @usage load_file(x, ...)
#' @rdname load_file
#' @export
method(load_file, DTADataSetTabular) <- function(x, file, handler_index, name = tools::file_path_sans_ext(basename(file))) {

  # check if handler_index is valid and if the file exists in the files list
  if (handler_index < 1 || handler_index > length(x@files)) {
    cli::cli_abort("Invalid handler_index: {handler_index}. Must be between 1 and {length(x@files)}.")
  }

  # This is where a dataset's specs and its file handler meet, so it is the only
  # place that can tell the reader what the columns are: `read_file()` dispatches
  # on the handler alone and a bare `DTAFile` has no specification to consult.
  #
  # The specs are needed in *both* halves of the read, for damage that happens at
  # different times:
  #
  # * At parse time, because the reader infers a column's type from its contents
  #   before any code here sees the data. A column of quoted subject ids reads as
  #   an integer and "007" arrives as 7, with the leading zeros already gone --
  #   which no later guard can undo. Passing the specs pins the columns declared
  #   as text to text.
  # * At coercion time, because inference also runs the other way: one
  #   unparseable cell turns a whole declared-numeric column into text. Applying
  #   the declared type makes the column a number, that one cell NA, and that one
  #   cell an import error.
  coerced <- dta_coerce_table_to_specs(
    files(x, handler_index) |> read_file(file, specs = x@specs),
    x@specs
  )

  x@tables[[ name ]] <- coerced$table

  # Canonical copy on the dataset, keyed by table name. The same frame also
  # rides on the table itself, so a change in the issues changes the table hash
  # and check() cannot skip revalidation with a stale result.
  if (nrow(coerced$issues) > 0) {
    x@import_issues[[ name ]] <- coerced$issues
  } else {
    x@import_issues[[ name ]] <- NULL
  }

  x
}

#' @title Validation Status for DTADataSetTabular
#' @description Returns a compact status table for validated tables.
#' @param x A \'DTADataSetTabular\' object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character table names, or numeric table indices.}
#'   }
#' @return A data.frame with validation status per table.
#' @usage validation_status(x, ...)
#' @name validation_status
#' @export
validation_status <- S7::new_generic("validation_status", "x")

#' @export
S7::method(validation_status, DTADataSetTabular) <- function(x, tables = NULL) {
  target_tables <- dta_table_id_to_names(x, tables)

  rows <- lapply(target_tables, function(table_name) {
    entry <- x@validation_index[[table_name]]
    if (is.null(entry)) {
      return(data.frame(
        table = table_name,
        target_type = "table",
        status = "not_validated",
        ok = NA,
        validated_at = NA_character_,
        run_id = NA_character_,
        validation_run = NA_character_,
        n_schema_errors = NA_integer_,
        n_rule_errors = NA_integer_,
        n_import_errors = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }

    dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = entry
    )
  })

  do.call(rbind, rows)
}


#' @title Retrieve Validation Errors for One Table
#' @description
#' Returns detailed validation output for one table, either from in-memory
#' store or from persisted artifact.
#' @param x A \'DTADataSetTabular\' object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{table}{Character or numeric table identifier.}
#'     \item{source}{Character. One of \'auto\', \'memory\', or \'artifact\'.}
#'   }
#' @return A list with detailed validation output, of class
#'   \code{dta_validation_details}. Use \code{as.data.frame()} on it to get one
#'   row per reported error.
#' @usage validation_errors(x, ...)
#' @name validation_errors
#' @export
validation_errors <- S7::new_generic("validation_errors", "x")

#' @export
S7::method(validation_errors, DTADataSetTabular) <- function(
  x,
  table,
  source = c("auto", "memory", "artifact")
) {
  source <- match.arg(source)
  table_name <- dta_table_id_to_names(x, table)
  table_name <- table_name[[1]]

  if (source %in% c("auto", "memory")) {
    in_memory <- x@validation_store[[table_name]]
    if (!is.null(in_memory)) {
      # Migrated on read as well: a store entry can have been restored from a
      # session that predates the import axis.
      return(dta_as_validation_details(dta_migrate_validation_details(in_memory)))
    }
  }

  entry <- x@validation_index[[table_name]]
  if (is.null(entry) || is.null(entry$artifact_path)) {
    cli::cli_abort(
      "No validation artifact available for table '{table_name}'. Run check() first with persist = TRUE."
    )
  }

  if (!file.exists(entry$artifact_path)) {
    cli::cli_abort(
      "Validation artifact for table '{table_name}' does not exist at '{entry$artifact_path}'."
    )
  }

  # Tagged and migrated on read, so memory and artifact results stay identical.
  dta_as_validation_details(
    dta_migrate_validation_details(readRDS(entry$artifact_path))
  )
}


#' @title Clear Validation State
#' @description Clears in-memory validation state for one or all tables.
#' @param x A \'DTADataSetTabular\' object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character table names, or numeric table indices.}
#'     \item{remove_artifacts}{Logical. If \'TRUE\', delete artifact files for selected tables.}
#'   }
#' @return Invisibly returns \'x\'.
#' @usage clear_validation(x, ...)
#' @name clear_validation
#' @export
clear_validation <- S7::new_generic("clear_validation", "x")

#' @export
S7::method(clear_validation, DTADataSetTabular) <- function(
  x,
  tables = NULL,
  remove_artifacts = FALSE
) {
  target_tables <- dta_table_id_to_names(x, tables)

  for (table_name in target_tables) {
    entry <- x@validation_index[[table_name]]

    if (remove_artifacts && !is.null(entry) && !is.null(entry$artifact_path)) {
      if (file.exists(entry$artifact_path)) {
        unlink(entry$artifact_path)
      }
    }

    x@validation_index[[table_name]] <- NULL
    x@validation_store[[table_name]] <- NULL
    x@import_issues[[table_name]] <- NULL
  }

  invisible(x)
}



#' @title Invalidate Validation Due to Spec Changes
#' @description
#' Marks validation as outdated for specified tables when specs are changed.
#' This is a helper function called automatically when specs are updated.
#' @param x A \'DTADataSetTabular\' object.
#' @param tables NULL (default) to invalidate all tables, or character/numeric
#'   table identifiers.
#' @return Invisibly returns \'x\'.
#' @keywords internal
invalidate_by_spec_change <- function(x, tables = NULL) {
  target_tables <- dta_table_id_to_names(x, tables)

  for (table_name in target_tables) {
    entry <- x@validation_index[[table_name]]
    if (!is.null(entry)) {
      # Mark specs_hash as NULL to force re-validation on next check() call
      entry$specs_hash <- NULL
      x@validation_index[[table_name]] <- entry
    }
    # Import issues were derived under the old specs; drop them with the rest.
    x@import_issues[[table_name]] <- NULL
  }

  invisible(x)
}


#' @title Check DTADataSetTabular Tables
#' @description
#' Validates all tables or a specific table within a DTADataSetTabular object,
#' prints a validation summary to the console, and updates the object's
#' validation state.
#' @param x A \code{DTADataSetTabular} object.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character table names, or numeric table
#'       indices. If NULL and `tab` is also NULL, checks all tables.}
#'     \item{tab}{Character table name or numeric table index (optional). If
#'       provided, checks only this single table. Cannot be used together
#'       with `tables`.}
#'     \item{force}{Logical. If TRUE, forces re-validation even if unchanged.
#'       Default is FALSE.}
#'     \item{persist}{Logical. If TRUE (default), persists validation
#'       artifacts to disk.}
#'     \item{artifact_dir}{Character or NULL. Optional output directory for
#'       persisted validation artifacts.}
#'     \item{quiet}{Logical. If TRUE, suppresses console output. Default is FALSE.}
#'   }
#' @return Invisibly returns the updated \code{DTADataSetTabular} object `x`,
#'   with \code{validation_index}/\code{validation_store} updated and a
#'   \code{"last_validation_summary"} attribute set. Use
#'   \code{validation_status()} on the returned object to obtain the
#'   validation summary data.frame.
#' @importFrom cli cli_h3 cli_alert_info cli_alert_success cli_alert_danger
#' @examples
#'   ds <- create_example_DTADataSetTabular(2)
#'   # Check all tables
#'   ds <- check(ds)
#'   # Check specific table
#'   ds <- check(ds, tables = "tab1")
#' @usage check(x, ...)
#' @name check
#' @export
S7::method(check, DTADataSetTabular) <- function(
  x,
  tables = NULL,
  tab = NULL,
  force = FALSE,
  persist = TRUE,
  artifact_dir = NULL,
  quiet = FALSE,
  validation_run = NULL
) {
  # Handle single table vs multiple tables
  if (!is.null(tab) && !is.null(tables)) {
    cli::cli_abort("Cannot specify both 'tab' and 'tables' parameters. Use 'tab' for single table, 'tables' for multiple.")
  }
  
  # If single table is specified, use it; otherwise use tables parameter
  if (!is.null(tab)) {
    target_table_indices <- dta_table_id_to_names(x, tab)
    tables_to_check <- target_table_indices
    single_table_mode <- TRUE
  } else {
    tables_to_check <- tables
    single_table_mode <- FALSE
  }
  
  # Validate structure (from parent class)
  x <- S7::method(check, DTADataSet)(
    x,
    force = force,
    persist = persist,
    artifact_dir = artifact_dir,
    quiet = quiet,
    validation_run = validation_run
  )

  if (is.null(validation_run)) {
    validation_run <- dta_new_validation_run_id()
  }

  # Table-specific validation logic (override parent)
  target_tables <- dta_table_id_to_names(x, tables_to_check)
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

  for (idx in seq_along(target_tables)) {
    table_name <- target_tables[idx]
    current_table <- x@tables[[table_name]]
    current_df <- as.data.frame(current_table)
    table_hash <- dta_hash_object(current_df)

    previous <- x@validation_index[[table_name]]
    unchanged <- !is.null(previous) &&
      identical(previous$table_hash, table_hash) &&
      identical(previous$specs_hash, specs_hash)

    if (!force && unchanged) {
      previous$validation_run <- validation_run
      x@validation_index[[table_name]] <- previous

      output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
        table_name = table_name,
        status = "skipped",
        index_entry = previous,
        target_type = "table"
      )
      next
    }

    # Output table name/index under investigation
    if (!isTRUE(quiet)) {
      cli::cli_text()
      if (single_table_mode) {
        cli::cli_rule(paste0("Validating table: ", table_name))
      } else {
        cli::cli_rule(paste0("Table ", idx, " of ", length(target_tables), ": ", table_name))
      }
    }

    details <- validate_table_detailed(x@specs, current_df, verbose = !isTRUE(quiet))
    artifact_path <- NULL
    validated_at <- Sys.time()
    run_id <- format(validated_at, "%Y%m%dT%H%M%OS3")

    if (persist) {
      safe_table <- gsub("[^A-Za-z0-9_-]", "_", table_name)
      table_dir <- file.path(artifact_dir, safe_table)
      dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
      artifact_path <- file.path(table_dir, paste0(run_id, ".rds"))
      saveRDS(details, artifact_path)
    }

    index_entry <- list(
      validated_at = validated_at,
      ok = isTRUE(details$ok),
      table_hash = table_hash,
      specs_hash = specs_hash,
      n_schema_errors = details$n_schema_errors,
      n_rule_errors = details$n_rule_errors,
      n_import_errors = details$n_import_errors,
      run_id = run_id,
      validation_run = validation_run,
      artifact_path = artifact_path
    )

    x@validation_index[[table_name]] <- index_entry
    x@validation_store[[table_name]] <- details
    
    # Attach details for single table mode
    if (single_table_mode) {
      attr(x, "last_validation_details") <- details
    }

    output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = index_entry,
      target_type = "table"
    )
  }

  summary_df <- do.call(rbind, output_rows)
  attr(x, "last_validation_summary") <- summary_df

  # Summary output
  val_status <- validation_status(x, tables = tables_to_check)

  if (!isTRUE(quiet)) {
    if (nrow(val_status) > 0) {
      n_valid <- sum(val_status$ok == TRUE, na.rm = TRUE)
      n_invalid <- sum(val_status$ok == FALSE, na.rm = TRUE)
      n_total <- nrow(val_status)
      
      if (!single_table_mode) {
        cli::cli_text()
      }
      
      if (n_invalid > 0) {
        table_word <- if (n_total == 1) "table" else "tables"
        cli::cli_alert_danger(
          paste0("", n_valid, " of ", n_total, " ", table_word, " valid")
        )
      } else {
        table_word <- if (n_total == 1) "table" else "tables"
        cli::cli_alert_success(
          paste0("", n_total, " ", table_word, " passed validation")
        )
      }
    }
  }

  # Return the updated dataset so validation state is not lost
  invisible(x)
}

