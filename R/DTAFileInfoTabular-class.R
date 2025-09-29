#' @title DTAFileInfoTabular Class Constructor
#'
#' @description
#' Defines the S7 class \code{C}, which extends \code{DTAFileInfo}
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
#' @name DTAFileInfoTabular-class
#' @return An object of class \code{DTAFileInfoTabular}.
#'
#' @seealso \code{\link{DTAFileInfo}}
#'
#' @export
DTAFileInfoTabular <- S7::new_class(
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
      .parent = DTAFileInfo(filename = filename,
                  number_of_files = number_of_files,
                  pattern = pattern),
      sep = sep,
      has_header = has_header,
      quote = quote
    )
  },
  properties = list(
    sep = class_character,
    has_header = class_logical,
    quote = class_character
  )
)


#' @title Read File for DTAFileInfoTabular Objects
#' @name read_file_execution-DTAFileInfoTabular
#' @description
#' \code{DTAFileInfoTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileInfoTSV},
#' \code{DTAFileInfoCSV} or \code{DTAFileInfoDelim}.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileInfoTabular} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
method(read_file_execution, DTAFileInfoTabular) <- function(x, file) {
  cli::cli_abort("This method is not implemented. You need to
  use an object of a class which is derived from DTAFileInfoTabular class.")
}
