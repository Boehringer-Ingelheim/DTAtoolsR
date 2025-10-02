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
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#'
#' @name DTAFileCSV-class
#' @return An object of class \code{DTAFileCSV}.
#' @examples
#'  \dontrun{
#' }
#' @seealso \code{\link{DTAFile}}
#' @include DTAFileTabular-class.R
#' @export
DTAFileCSV <- S7::new_class( # nolint
  "DTAFileCSV",
  parent = DTAFileTabular,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
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
        min_number_of_files = NULL,
        max_number_of_files = NULL,
        info = NULL,
        pattern = pattern,
        sep = ",",
        has_header = has_header,
        quote = quote
      )
    )
  }
)


#' @title Read File for DTAFileCSV Objects
##' @name read_file_execution-DTAFileCSV
#' @description
#' Reads a CSV file using the parameters specified in a
#' \code{DTAFileCSV} object. This method uses \code{arrow::read_csv_arrow}
#' for efficient CSV parsing.
#' @importFrom arrow read_delim_arrow
#' @param x A \code{DTAFileCSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
method(read_file_execution, DTAFileCSV) <- function(x, file) {
  return(arrow::read_csv_arrow(
    file,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    as_data_frame = FALSE
  ))
}

#' @title Create Example DTAFileCSV Object
#' @description
#' Creates an example \code{DTAFileCSV} object using example files.
#' @param index example selector.
#' @return An example \code{DTAFileCSV} object.
#' @examples
#' library(DTAtools)
#' create_example_DTAFileCSV()
#' @export
create_example_DTAFileCSV <- function(index = 1) {
  if (index == 1) {
    example_file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
    DTAFileCSV(
      filename = basename(example_file) # makes sure this was derived from existing example
    )
  } else {
    cli::cli_abort("Only index = 1 is supported for create_example_DTAFileCSV().")
  }
}


#' @title Print DTAFileCSV Object
#' @description
#' Print method for DTAFileCSV objects.
#' @param x An object of class DTAFileCSV
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @examples
#' library(DTAtools)
#' print(create_example_DTAFileCSV())
#'
#' @name print
#' @export
method(print, DTAFileCSV) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAFileCSV}>")
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  if (!is.null(x@min_number_of_files) && !is.null(x@max_number_of_files)) {
    if (x@min_number_of_files == x@max_number_of_files) {
      cli_alert("Files required: {x@min_number_of_files}")
    } else {
      cli_alert("Files required: {x@min_number_of_files} to {x@max_number_of_files}")
    }
  }
  cli::cli_alert("Separator: {x@sep}")
  cli::cli_alert("Has header: {x@has_header}")
  cli::cli_alert("Quote: {x@quote}")
  invisible(x)
}


