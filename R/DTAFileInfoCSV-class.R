
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
#'
#' @seealso \code{\link{DTAFileInfo}}
#' @include DTAFileInfoDelim-class.R
#' @export
DTAFileInfoCSV <- S7::new_class(
  "DTAFileInfoCSV",
  parent = DTAFileInfoDelim,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
    has_header = TRUE,
    quote = '"',
    col_types = NULL
  ) {
    new_object(
      S7_object(),
      filename = filename,
      number_of_files = number_of_files,
      pattern = pattern,
      sep = ",",
      has_header = has_header,
      quote = quote,
      col_types = col_types
    )
  }
)


#' @title Read File for DTAFileInfoCSV Objects
##' @name read_file_execution-DTAFileInfoCSV
#' @description
#' Reads a CSV file using the parameters specified in a
#' \code{DTAFileInfoCSV} object. This method uses \code{readr::read_CSV}
#' for efficient CSV parsing.
#' @importFrom arrow read_delim_arrow
#' @param x A \code{DTAFileInfoCSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
method(read_file_execution, DTAFileInfoCSV) <- function(x, file) {
  return(arrow::read_csv_arrow(
    file,
    #col_types = x@col_types,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    #col_names = x@has_header,
    as_data_frame = FALSE
  ))
}
