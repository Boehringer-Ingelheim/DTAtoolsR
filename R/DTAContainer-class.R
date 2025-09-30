#' @title DTAContainer Class
#' @description Handles tables  based on column specifications. Every table will be validated using the column specifications.
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom arrow arrow_table
#' @param name Character. Name of the container.
#' @param specs A DTAColumnSpecCollection object specifying the column specs.
#' @param fileinfo a list of DTAFileInfo objects specifying input file information.
#' @param data List. A list of arrow Table to be validated and included in the DTAContainer object.
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
    name,
    specs,
    data = list(),
    fileinfo = list()
  ) {
    if(inherits(fileinfo, "DTAtools::DTAFileInfo")) {
      fileinfo = list(fileinfo)
    }

    if(inherits(data, "Table")) {
      data = list(data)
    }

    # names <- names(data)
    # for (name in names(data)) {
    #   data_entry <- data[[name]]
    #   #data_entry[data_entry == ""] <- NA
    #   cli_alert_info("Checking {name} data entry")
    #   data_entry <- validate_table(
    #     specs,
    #     data_entry
    #   )
    #   data[[name]] <- data_entry
    # }
    new_object(
      S7_object(),
      name = name,
      specs = specs,
      data = data,
      fileinfo = fileinfo
    )
  },
  properties = list(
    name = class_character, 
    specs = class_DTAColumnSpecCollection,
    data = class_list, # list of arrow Table
    fileinfo = class_list # list of DTAFileInfo
  ),
  validator = function(self) {
    if (!is.character(self@name) || length(self@name) != 1 || self@name == "") {
      cli_abort("Property 'name' must be a single non-empty string.")
    }
    # check if all elements of list self@data inherit from "Table"
    if (!all(sapply(self@data, inherits, "Table"))) {
      cli_abort("All elements in 'data' must be of class 'Table'")
    }
    if (!inherits(self@specs, "DTAtools::DTAColumnSpecCollection")) {
      cli_abort("Property 'specs' must be of class 'DTAColumnSpecCollection'")
    }
    if (!all(sapply(self@fileinfo, inherits, "DTAtools::DTAFileInfo"))) {
      cli_abort("All elements in 'fileinfo' must be of class 'DTAFileInfo'")
    }
  }
)



#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTAContainer
#' @param id Character. The ID of the column to retrieve.
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' \dontrun{
#' column_format <- column(dtadata, "STUDYID")
#' }
#' @name colspec-DTAContainer
#' @export
colspec <- new_generic("colspec", "x")

#' @export
method(colspec, DTAContainer) <- function(x, id) {
  return(colspec(x@specs, id))
}


#' @title Get DTAColumnSpecCollection (specs) from DTAContainer Object
#' @description
#' Method to extract the full DTAColumnSpecCollection from a DTAContainer object.
#' @param x An object of class DTAContainer.
#' @param ... void
#' @return A DTAColumnSpecCollection object.
#' @examples
#' \dontrun{
#'   specs(container)
#' }
#' @name specs-DTAContainer
#' @export
specs <- new_generic("specs", "x")

#' @export
method(specs, DTAContainer) <- function(x) {
  return(x@specs)
}


#' @title Get table from DTAContainer Object
#' @description
#' Extract a table from the tables in a DTAContainer object.
#' @param x An object of class DTAContainer.
#' @param id Character or numeric. Name or index of the table to retrieve.
#' @return A Table object
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' data(container)           # returns first table
#' data(container, "lab")   # returns table named "lab"
#' }
#' @name data-DTAContainer
#' @export
data <- new_generic("data", "x", function(x, id = 1, ...) {
  S7_dispatch()
})

#' @export
method(data, DTAContainer) <- function(x, id = 1) {
  if (!inherits(x, "DTAtools::DTAContainer")) {
    cli::cli_abort("Input must be a DTAContainer object.")
  }

  tables <- x@data

  if(length(tables) == 0) {
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


#' @title List of data labels within DTAContainer Object
#' @description
#' Method to get a all data labels within a DTAContainer Object.
#' @param x An object of class DTAContainer
#' @param ... void
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
#' @importFrom dplyr arrange across everything
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
#' metadata(DTAContainer)
#' }
#' @name metadata
#' @rdname metadata-DTAContainer
#' @export
method(metadata, DTAContainer) <- function(x) {
  return(x@specs@metadata)
}

#' @title Get Rules
#' @description
#' Method to get Rules from DTAContainer.
#' @param x An object of class DTAContainer
#' @return A list with rules information
#' @examples
#' \dontrun{
#' rules(DTAContainer)
#' }
#' @name rules-DTAContainer
#' @export
rules <- new_generic("rules", "x")

#' @export
method(rules, DTAContainer) <- function(x) {
  return(x@specs@rules)
}


#' @title get max number of files
#' @description
#' Returns the sum of max number of files specified all associated DTAFileInfo
#' objects.
#' @param x An object of class DTAContainer
#' @return numeric: number of files
#' @examples
#' \dontrun{
#' column_format <- max_number_of_files(dtafileinfo)
#' }
#' @name max_number_of_files-DTAContainer
if (!exists("max_number_of_files", mode = "function")) {
  max_number_of_files <- new_generic("max_number_of_files", "x")
}
#' @export
method(max_number_of_files, DTAContainer) <- function(x) {
  sum(unlist(sapply(x@fileinfo, max_number_of_files)))
}


#' @title get min number of files
#' @description
#' Returns the sum of min number of files specified all associated DTAFileInfo
#' objects.
#' @param x An object of class DTAContainer
#' @return numeric: number of files
#' @examples
#' \dontrun{
#' column_format <- min_number_of_files(dtafileinfo)
#' }
#' @name min_number_of_files-DTAContainer
if (!exists("min_number_of_files", mode = "function")) {
  min_number_of_files <- new_generic("min_number_of_files", "x")
}
#' @export
method(min_number_of_files, DTAContainer) <- function(x) {
  sum(unlist(sapply(x@fileinfo, min_number_of_files)))
}


#' @title Create Example DTAContainer
#' @description
#' S7 method to create and return an example DTAContainer object.
#' @importFrom cli cli_abort
#' @param index Integer. Index of the example to create.
#'
#' @return An example DTAContainer object.
#' @examples
#' library(DTAtools)
#' create_example_DTAContainer()
#' @export
create_example_DTAContainer <- function(index = 1) {
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
  
  switch(index,
    `1` = {
      DTAtools::DTAContainer(
        name = "example_container_specs_without_data",
        specs = create_example_DTAColumnSpecCollection(1),
      )
    },
    `2` = {
      DTAtools::DTAContainer( 
        name = "demographics",
        specs = create_example_DTAColumnSpecCollection(1),
        data = list("tab1" = table1)
      )
    },
    `3` = {
      DTAtools::DTAContainer(
        name = "vitals", 
        specs = create_example_DTAColumnSpecCollection(2),
        data = list("tab2" = table2)
      )
    },
    cli::cli_abort("No example available for the provided index.")
  )

}

#' @title Print Method for DTAContainer
#' @description Print a summary of a DTAContainer object.
#' @param x A DTAContainer object.
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @importFrom stringr str_c str_glue
#' @examples
#' library(DTAtools)
#' print(create_example_DTAContainer())
#' @name print
#' @export
method(print, DTAContainer) <- function(x) {

  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAContainer}> : {.field {x@name}}")
  if(!is.null(x@specs)) {
    cli_alert_info("Column specs ({length(x@specs@columns)}): {column_preview(x@specs)}")
  } else {
    cli_alert_info("Column specs: none")
  }
  n_tables <- length(x@data)
  
  if (n_tables > 0) {
    table_names <- names(x@data)
    
    if (n_tables > 5) {
      shown_names <- c(table_names[1:4], "...", table_names[n_tables])
    } else {
      shown_names <- table_names
    }
    
    # Build the message with proper cli markup, need paste and paste0
    # instead of stringr functions to work with cli
    alert_message <- paste0("Data tables (", n_tables, "): ", 
                           paste(paste0("{.field ", shown_names, "}"), 
                                collapse = ", "))
    cli_alert_info(alert_message)
  } else {
    cli_alert("Data tables: {.emph none}")
  }

  if (is.null(x@fileinfo) || length(x@fileinfo) == 0) {
    cli_alert("Fileinfo entries: {.emph none}")
  } else {
    cli_alert_info("Fileinfo entries: {length(x@fileinfo)}")
  }

  invisible(x)
}

