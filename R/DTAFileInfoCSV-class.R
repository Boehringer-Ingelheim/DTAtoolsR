#' @title DTAFileInfoCSV Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileInfoCSV}, which extends \code{DTAFileInfo}
#' to represent metadata and configuration for CSV (Tab-Separated Values)
#'  data files.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#'
#' @name DTAFileInfoCSV-class
#' @return An object of class \code{DTAFileInfoCSV}.
#' @examples
#'  \dontrun{
#' }
#' @seealso \code{\link{DTAFileInfo}}
#' @include DTAFileInfoTabular-class.R
#' @export
DTAFileInfoCSV <- S7::new_class( # nolint
  "DTAFileInfoCSV",
  parent = DTAFileInfoTabular,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      DTAFileInfoTabular(
        filename = filename,
        number_of_files = number_of_files,
        pattern = pattern,
        sep = ",",
        has_header = has_header,
        quote = quote
      )
    )
  }
)


#' @title Read File for DTAFileInfoCSV Objects
##' @name read_file_execution-DTAFileInfoCSV
#' @description
#' Reads a CSV file using the parameters specified in a
#' \code{DTAFileInfoCSV} object. This method uses \code{arrow::read_csv_arrow}
#' for efficient CSV parsing.
#' @importFrom arrow read_delim_arrow
#' @param x A \code{DTAFileInfoCSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
method(read_file_execution, DTAFileInfoCSV) <- function(x, file) {
  return(arrow::read_csv_arrow(
    file,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    as_data_frame = FALSE
  ))
}

#' @title Create Example DTAFileInfoCSV Object
#' @description
#' Creates an example \code{DTAFileInfoCSV} object using example files.
#' @param index example selector.
#' @return An example \code{DTAFileInfoCSV} object.
#' @examples
#' library(DTAtools)
#' create_example_DTAFileInfoCSV()
#' @export
create_example_DTAFileInfoCSV <- function(index = 1) {
  if (index != 1) {
    example_file <- system.file("extdata", "data_spec.csv", package = "DTAtoolsR")
    DTAFileInfoCSV(
      filename = basename(example_file) # makes sure this was derived from existing example
    )
  } else {
    cli::cli_abort("Only index = 1 is supported for create_example_DTAFileInfoCSV().")
  }
}


#' @title Print DTAFileInfoCSV Object
#' @description
#' Print method for DTAFileInfoCSV objects.
#' @param x An object of class DTAFileInfoCSV
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @examples
#' library(DTAtools)
#' print(create_example_DTAFileInfoCSV())
#'
#' @name print
#' @export
method(print, DTAFileInfoCSV) <- function(x, ...) {
  cat("<DTAFileInfoCSV>\n")
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert_info("Pattern: {x@pattern}")
  cli::cli_alert_info("Number of files: {x@number_of_files}")
  cli::cli_alert_info("Separator: {x@sep}")
  cli::cli_alert_info("Has header: {x@has_header}")
  cli::cli_alert_info("Quote: {x@quote}")
  invisible(x)
}


