#' @title DTAFileInfoDelim Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileInfoDelim}, which extends \code{DTAFileInfo}
#' to represent metadata and configuration for Delim (Tab-Separated Values)
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
#' @param col_types Character string specifying the type of each column (e.g.,
#'   \code{"cccidcl"}). Default is \code{NULL}.
#'
#' @name DTAFileInfoDelim-class
#' @return An object of class \code{DTAFileInfoDelim}.
#'
#' @seealso \code{\link{DTAFileInfo}}
#'
#' @export
DTAFileInfoDelim <- S7::new_class(
  "DTAFileInfo",
  parent = DTAFileInfo,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
    sep = "\t",
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      S7_object(),
      filename = filename,
      number_of_files = number_of_files,
      pattern = pattern,
      sep = sep,
      has_header = has_header,
      quote = quote,
      col_types = col_types
    )
  },
  properties = list(
    sep = class_character,
    has_header = class_logical,
    quote = class_character
  )
)


#' @title Read File for DTAFileInfoDelim Objects
##' @name read_file_execution-DTAFileInfoDelim
#' @description
#' Reads a Delim file using the parameters specified in a
#' \code{DTAFileInfoDelim} object. This method uses \code{readr::read_Delim}
#' for efficient Delim parsing.
#' @importFrom arrow read_delim_arrow
#' @param x A \code{DTAFileInfoDelim} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
method(read_file_execution, DTAFileInfoDelim) <- function(x, file) {
  return(arrow::read_Delim_arrow(
    file,
    #col_types = x@col_types,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    #col_names = x@has_header,
    as_data_frame = FALSE
  ))
}
