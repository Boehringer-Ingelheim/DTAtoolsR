#' @title DTAFileInfoDelim Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileInfoDelim}, which extends \code{DTAFileInfo}
#' to represent metadata and configuration for TSV (Tab-Separated Values)
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
#' @return An object of class \code{DTAFileInfoDelim}.
#' @name DTAFileInfoDelim-class
#' @seealso \code{\link{DTAFileInfo}}
#'
#' @export
DTAFileInfoDelim <- S7::new_class(
  "DTAFileInfoDelim",
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
        has_header = has_header,
        quote = quote,
        sep = "\t")
    )
  }
)


#' @title Read File for DTAFileInfoDelim Objects
#' @description
#' Reads a TSV file using the parameters specified in a
#' \code{DTAFileInfoDelim} object. This method uses \code{arrow::read_delim_arrow}
#' for efficient TSV parsing.
#'
#' @param x A \code{DTAFileInfoDelim} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
##' @seealso \code{\link{arrow::read_tsv_arrow}}
##' @name read_file_execution-DTAFileInfoDelim
method(read_file_execution, DTAFileInfoDelim) <- function(x, file) {
  return(arrow::read_delim_arrow(
    file,
    #col_types = x@col_types,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    #col_names = x@has_header,
    as_data_frame = FALSE
  ))
}

#' @title Print DTAFileInfoDelim Object
#' @description
#' Print method for DTAFileInfoDelim objects.
#' @param x An object of class DTAFileInfoDelim
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @examples
#' library(DTAtools)
#' print(DTAFileInfoDelim("example.tsv"))
#'
#' @name print
#' @export
method(print, DTAFileInfoDelim) <- function(x, ...) {
  cat("<DTAFileInfoDelim>\n")
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert_info("Pattern: {x@pattern}")
  cli::cli_alert_info("Number of files: {x@number_of_files}")
  cli::cli_alert_info("Separator: {x@sep}")
  cli::cli_alert_info("Has header: {x@has_header}")
  cli::cli_alert_info("Quote: {x@quote}")
  invisible(x)
}



