#' @title DTAFileTabular Class Constructor
#'
#' @description
#' Defines the S7 class \code{C}, which extends \code{DTAFile}
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
#' @name DTAFileTabular-class
#' @return An object of class \code{DTAFileTabular}.
#'
#' @seealso \code{\link{DTAFile}}
#'
#' @export
DTAFileTabular <- S7::new_class(
  "DTAFile",
  parent = DTAFile,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
    missing_values = "",
    sep = "\t",
    has_header = TRUE,
    quote = '"',
    encoding = "UTF-8"
  ) {
    new_object(
      .parent = DTAFile(filename = filename,
                  number_of_files = number_of_files,
                  pattern = pattern),
      sep = sep,
      has_header = has_header,
      quote = quote,
      missing_values = missing_values
    )
  },
  properties = list(
    sep = class_character,
    has_header = class_logical,
    quote = class_character,
    missing_values = class_character
  ),
  validator = function(self) {
    if (!is.character(self@sep) || nchar(self@sep) != 1) {
      "'sep' must be a single character."
    }

    if (!is.logical(self@has_header) || length(self@has_header) != 1) {
      "'has_header' must be a single logical value."
    }

    if (!is.character(self@quote) || nchar(self@quote) != 1) {
      "'quote' must be a single character."
    }
  }
)


#' @title Read File for DTAFileTabular Objects
#' @name read_file_execution-DTAFileTabular
#' @description
#' \code{DTAFileTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileTSV},
#' \code{DTAFileCSV} or \code{DTAFileDelim}.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileTabular} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
method(read_file_execution, DTAFileTabular) <- function(x, file) {
  cli::cli_abort("This method is not implemented. You need to
  use an object of a class which is derived from DTAFileTabular class.")
}
