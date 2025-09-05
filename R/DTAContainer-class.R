#' @title DTAContainer Class
#' @description Handles tables  based on column specifications. Every table will be validated using the column specifications.
#' @import S7
#' @importFrom cli cli_h1
#'
#' @param specs A DTAColumnSpecCollection object specifying the column specs.
#' @param data List. A list of tables to be validated and included in the DTAContainer object.
#' @return An object of class DTAContainer. If validation fails, returns a list containing summarised and full error data frames.
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
#' # Create the DTAContainer object
#' data_obj <- DTAContainer(DTAColumnSpecCollection, tables)
#' }
#' @export
DTAContainer <- new_class(
  "DTAContainer",
  constructor = function(
    specs,
    data
  ) {
    names <- names(data)
    for (name in names(data)) {
      data_entry <- data[[name]]
      data_entry[data_entry == ""] <- NA
      cli::cli_h1("Checking {name} data entry")
      data_entry <- validateTable(
        specs,
        data_entry
      )
      data[[name]] <- data_entry
    }
    new_object(
      S7_object(),
      specs = specs,
      data = data
    )
  },
  properties = list(
    specs = class_any,
    data = class_list
  )
)


#' @title Generate DTAContainer Object
#' @description
#' This function generates a DTAContainer object from a DTAColumnSpecCollection object and a list of tables.
#' @export
#'
#' @param specs A DTAColumnSpecCollection object specifying the column specs.
#' @param data List. A list of tables to be validated and included in the DTAContainer object.
#' @return An object of class DTAContainer.
#' @examples
#' #'
#' \dontrun{
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' data <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTAContainer object
#' data_obj <- makeDTAContainer(specs = NULL, data)
#' }
makeDTAContainer <- function(specs, data) {
  # Create and return the DTAContainer object
  DTAContainer(specs, data)
}

#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTAContainer
#' @param id Character. The ID of the column to retrieve.
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' \dontrun{
#' column_format <- getColumn(dtadata, "STUDYID")
#' }
# Define the generic only if it doesn't already exist
#' @name getColumn-DTAContainer
if (!exists("getColumn", mode = "function")) {
  getColumn <- new_generic("getColumn", "x")
}
#' @export
method(getColumn, DTAContainer) <- function(x, id) {
  return(x@specs@columns[[id]])
}


#' @title Get DTAColumnSpecCollection (specs) from DTAContainer Object
#' @description
#' Method to extract the full DTAColumnSpecCollection from a DTAContainer object.
#' @param x An object of class DTAContainer.
#' @return A DTAColumnSpecCollection object.
#' @examples
#' \dontrun {
#'   getSpecs(container)
#' }
#' @name getSpecs-DTAContainer
getSpecs <- new_generic("getSpecs", "x")

#' @export
method(getSpecs, DTAContainer) <- function(x) {
  return(x@specs)
}

#' @title Get table from DTAContainer Object
#' @description
#' Extract a table from the tables in a DTAContainer object.
#' @param x An object of class DTAContainer.
#' @param id Character or numeric. Name or index of the table to retrieve.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' getData(container)           # returns first table
#' getData(container, "lab")   # returns table named "lab"
#' }
#' @name getData-DTAContainer
if (!exists("getData", mode = "function")) {
  getData <- new_generic("getData", "x")
}

#' @export
method(getData, DTAContainer) <- function(x, id = 1) {
  if (!inherits(x, "DTAtools::DTAContainer")) {
    cli::cli_abort("Input must be a DTAContainer object.")
  }

  tables <- x@data

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


#' @title List of data labels within DTAContainer Object
#' @description
#' Method to get a all data labels within a DTAContainer Object.
#' @param x An object of class DTAContainer
#' @return A vector
#' @examples
#' \dontrun{
#' labels <- labels(dtadata)
#' }
#' @name labels-DTAContainer
labels <- new_generic("labels", "x")
#' @export
method(labels, DTAContainer) <- function(x) {
  return(names(x@data))
}

#' @title Write DTA Table to File
#' @description
#' Write a named DTA table saved in a DTAContainer object to a file.
#' @importFrom dplyr arrange across
#' @importFrom magrittr %>%
#' @importFrom utils write.table
#' @importFrom R.utils gzip
#' @importFrom cli cli_alert_info cli_alert_success format_message cli_abort
#' @importFrom tools md5sum
#' @export
#' @param DTAContainer An object of class DTAContainer.
#' @param table Character. The name of the table within the DTAContainer object to write.
#' @param filename Character. The name of the file to write to.
#' @param arrange_by Character vector. Columns to arrange the table by. NULL, Table won't be arranged. "all" (Default) -> Table will be arranged by all columns.
#' @param arrange_desc Logical. Whether to arrange the table in descending order. Default is FALSE.
#' @param sep Character. The field separator string. Default is ",".
#' @param na Character. The string to use for missing values in the data. Default is "".
#' @param overwrite Logical. Whether to overwrite the file if it exists. Default is FALSE.
#' @param compression Character. Compression method, either "none" or "gzip". Default is "none".
#' @param get_md5sum Logical. Whether to calculate and print the MD5 checksum of the file. MD5SUM and number of rows and columns of file will be also saved in an additional file. Default is TRUE.
#' @param write_md5sum_to_file Logical. Whether to calculate and print the MD5 checksum of the file. MD5SUM and number of rows and columns of file will be also saved in an additional file. Default is TRUE.
#' @param return_metadata Logical. Whether to return metadata (MD5 checksum and dimensions) as a list. Default is FALSE.
#' @param ... Additional arguments passed to write.table.
#' @return NULL. The function writes the table to a file.

#' @examples
#' \dontrun{
#' writeTableToFile(dtadata, table = "my_table", filename = "table.tsv.gz", sep = "\t", arrange_by = c("STUDYID", "VISIT"))
#' }
writeTableToFile <- function(
  DTAContainer,
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

  # Check if the table exists in the DTAContainer
  if (!table %in% names(DTAContainer@data)) {
    cli::cli_abort(c(
      "Table name not found!",
      x = "Table with the name '{table}' not found in the DTAContainer object.",
      i = "Use 'labels(<DTAContainer>)' to print names of all tables in object."
    ))
  }

  table_data <- DTAContainer@data[[table]]

  # Arrange the table by specified columns
  if (!is.null(arrange_by)) {
    if (arrange_by == "all") {
      cli::cli_alert_info("Arrange table by all columns.")
      table_data <- table_data %>%
        dplyr::arrange(dplyr::across(everything()), desc = arrange_desc)
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
    md5sum <- writeMetadata(
      filename,
      table_data,
      write_to_file = write_md5sum_to_file
    )
  } else {
    md5sum <- NA
  }

  invisible(list(
    data = table_data,
    table = table,
    md5sum = md5sum
  ))
}

#' @title Get Metadata
#' @description
#' Method to get Metadata from DTAContainer.
#' @param x An object of class DTAContainer
#' @return A list with metadata information
#' @examples
#' \dontrun{
#' getMetadata(DTAContainer)
#' }
#' @name getMetadata-DTAContainer
if (!exists("getMetadata", mode = "function")) {
  getMetadata <- new_generic("getMetadata", "x")
}
#' @export
method(getMetadata, DTAContainer) <- function(x) {
  return(x@specs@metadata)
}

#' @title Get Rules
#' @description
#' Method to get Rules from DTAContainer.
#' @param x An object of class DTAContainer
#' @return A list with rules information
#' @examples
#' \dontrun{
#' getRules(DTAContainer)
#' }
#' @name getRules-DTAContainer
if (!exists("getRules", mode = "function")) {
  getRules <- new_generic("getRules", "x")
}
#' @export
method(getRules, DTAContainer) <- function(x) {
  return(x@specs@rules)
}
