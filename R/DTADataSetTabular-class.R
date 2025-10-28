#' @title DTADataSetTabular Class
#' @description Handles tabular data with column specifications and rules.
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom arrow arrow_table
#' @param name Character. Name of the container.
#' @param specs A DTAColumnSpecCollection object specifying the column specs.
#' @param files a list of DTAFile objects specifying input file information.
#' @param tables List. A named list of tables to be validated and included in the DTADataSetTabular object.
#' @return An object of class DTADataSetTabular
#' @examples
#' \dontrun{
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' tables <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTADataSetTabular object
#' data_obj <- DTADataSetTabular(DTAColumnSpecCollection, tables)
#' }
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

    # Transform to arrow tables
    tables <- lapply(tables, function(x) arrow::as_arrow_table)

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
      tables = tables
    )
  },
  properties = list(
    specs = class_DTAColumnSpecCollection,
    tables = class_list # list of tables - can be arrow tables etc
  ),
  validator = function(self) {
    # check if all elements of list self@tables inherit from "Table"
    if (!inherits(self@specs, "DTAtools::DTAColumnSpecCollection")) {
      cli_abort("Property 'specs' must be of class 'DTAColumnSpecCollection'")
    }
  }
)


#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTADataSetTabular
#' @param id Character. The ID of the column to retrieve.
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' \dontrun{
#' column_format <- column(dtadata, "STUDYID")
#' }
#' @name colspec-DTADataSetTabular
#' @export
colspec <- new_generic("colspec", "x")

#' @export
method(colspec, DTADataSetTabular) <- function(x, id) {
  return(colspec(x@specs, id))
}


#' @title Get DTAColumnSpecCollection (specs) from DTADataSetTabular Object
#' @description
#' Method to extract the full DTAColumnSpecCollection from a DTADataSetTabular object.
#' @param x An object of class DTADataSet.
#' @param ... void
#' @return A DTAColumnSpecCollection object.
#' @examples
#' \dontrun{
#'   specs(container)
#' }
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
#' @return A Table object
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' tables(container)           # returns first table
#' tables(container, "lab")   # returns table named "lab"
#' }
#' @name table-DTADataSetTabular
#' @export
table <- new_generic("table", "x", function(x, id = 1, ...) {
  S7_dispatch()
})

#' @export
method(table, DTADataSetTabular) <- function(x, id = 1) {
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
#' @param ... void
#' @return A vector
#' @examples
#' \dontrun{
#' labels <- labels(dtadata)
#' }
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
#' @param ... Additional arguments passed to write.table.
#' @return NULL. The function writes the table to a file.
#' @examples
#' \dontrun{
#' write_table_to_file(dtadata, table = "my_table", filename = "table.tsv.gz",
#'                  sep = "\t", arrange_by = c("STUDYID", "VISIT"))
#' }
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
    if (arrange_by == "all") {
      cli::cli_alert_info("Arrange table by all columns.")
      table_data <- table_data %>%
        dplyr::arrange(dplyr::across(dplyr::everything()), desc = arrange_desc)
    } else {
      cli::cli_alert_info("Arrange table by {arrange_by}.")
      table_data <- table_data %>%
        dplyr::arrange(!!!rlang::syms(arrange_by))
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
    cli::cli_alert_info("Write table in gzip format to {filename}.")
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
    cli::cli_alert_info("Write table to {filename}.")
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
  cli::cli_alert_success("File {filename} written successfully.")

  # Calculate md5sum
  if (get_md5sum) {
    md5sum <- write_metadata(
      filename,
      table_data,
      write_to_file = write_md5sum_to_file
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


#' @title Get Metadata
#' @description
#' Method to get Metadata from DTADataSet.
#' @param x An object of class DTADataSetTabular
#' @return A list with metadata information
#' @examples
#' \dontrun{
#' metadata(DTADataSetTabular)
#' }
#' @name metadata
#' @rdname metadata-DTADataSetTabular
#' @export
method(metadata, DTADataSetTabular) <- function(x) {
  return(x@specs@metadata)
}

#' @title Get Rules
#' @description
#' Method to get Rules from DTADataSet.
#' @param x An object of class DTADataSetTabular
#' @return A list with rules information
#' @examples
#' \dontrun{
#' rules(DTADataSetTabular)
#' }
#' @name rules-DTADataSetTabular
#' @export
rules <- new_generic("rules", "x")

#' @export
method(rules, DTADataSetTabular) <- function(x) {
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
method(print, DTADataSetTabular) <- function(x) {
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

  n_tables <- length(x@tables)

  if (n_tables > 0) {
    table_names <- names(x@tables)

    if (n_tables > 5) {
      shown_names <- c(table_names[1:4], "...", table_names[n_tables])
    } else {
      shown_names <- table_names
    }

    # Build the message with proper cli markup, need paste and paste0
    # instead of stringr functions to work with cli
    alert_message <- paste0(
      "Tables (",
      n_tables,
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
#' @return
#' No return value. This function is called for its side effects (printing to the console).
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
method(print_short_info, DTADataSetTabular) <- function(x) {
  #super(print_short_info, x)
  method(print_short_info, DTADataSet)(x)
  if (!is.null(x@specs)) {
    cli_alert(
      "Specs: {length(x@specs@columns)} columns, {length(x@specs@rules)}, rules"
    )
  } else {
    cli_alert("Specs: none")
  }

  n_tables <- length(x@tables)

  if (n_tables > 0) {
    cli_alert("Tables: ({n_tables})")
  } else {
    cli_alert("Tables: {.emph none}")
  }

  return(invisible(x))
}
