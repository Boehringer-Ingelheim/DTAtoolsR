#' @title DTAFileCSV Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileCSV}, which extends \code{DTAFile}
#' to represent metadata and configuration for CSV (Tab-Separated Values)
#'  data files.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#'
#' @name DTAFileCSV-class
#' @return An object of class \code{DTAFileCSV}.
#' @seealso \code{\link{DTAFile}}
#' @include DTAFileTabular-class.R
#' @export
DTAFileCSV <- S7::new_class(
  # nolint
  "DTAFileCSV",
  parent = DTAFileTabular,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      DTAFileTabular(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern,
        sep = ",",
        has_header = has_header,
        quote = quote
      )
    )
  }
)


#' @title Read File for DTAFileCSV Objects
##' @name read_file_execution
#' @description
#' Reads a CSV file using the parameters specified in a
#' \code{DTAFileCSV} object. This method uses \code{arrow::read_csv_arrow}
#' for efficient CSV parsing.
#' @importFrom arrow read_csv_arrow
#' @param x A \code{DTAFileCSV} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileCSV) <- function(x, ...) {
  file <- list(...)[[1]]
  table_obj <- arrow::read_csv_arrow(
    file,
    quote = x@quote,
    # col_names = FALSE makes arrow generate names and keep the first row as
    # data; skipping a row would instead discard the first data row.
    col_names = x@has_header,
    as_data_frame = FALSE
  )

  dta_normalize_column_names(table_obj)
}

#' @title Create Example DTAFileCSV Object
#' @description
#' Creates an example \code{DTAFileCSV} object using example files.
#' @importFrom cli cli_abort
#' @param index example selector.
#' @return An example \code{DTAFileCSV} object.
#' @examples
#' library(DTAtools)
#' create_example_DTAFileCSV()
#' @export
create_example_DTAFileCSV <- function(index = 1) {
  if (index == 1) {
    example_file <- system.file(
      "extdata",
      "clinical_data.csv",
      package = "DTAtools"
    )
    DTAFileCSV(
      filename = basename(example_file) # makes sure this was derived from existing example
    )
  } else {
    cli::cli_abort(
      "Only index = 1 is supported for create_example_DTAFileCSV()."
    )
  }
}


#' @title Print DTAFileCSV Object
#' @description
#' Print method for DTAFileCSV objects.
#' @param x An object of class DTAFileCSV
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_div cli_text
#' @examples
#' library(DTAtools)
#' print(create_example_DTAFileCSV())
#'
#' @name print
#' @export
method(print, DTAFileCSV) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAFileCSV}>")

  print_info(x)

  invisible(x)
}
