
#' @title DTAFileInfoTSV Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileInfoTSV}, which extends \code{DTAFileInfo}
#' to represent metadata and configuration for TSV (Tab-Separated Values)
#'  data files.
#'
#' @param filename Character. Path to the TSV file.
#' @param number_of_files Integer. Number of files to be processed.
#'  Defaults to 1.
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical. Indicates if the TSV file contains
#' a header row. Defaults to \code{TRUE}.
#' @param quote Character. Quoting character used in the TSV file.
#'  Defaults to double quote ('"').
#' @param col_types Optional. Column types specification. Defaults
#' to \code{NULL}.
#'
#' @return An object of class \code{DTAFileInfoTSV}.
#' @name DTAFileInfoTSV-class
#' @seealso \code{\link{DTAFileInfo}}
#'
#' @export
DTAFileInfoTSV <- S7::new_class(
  "DTAFileInfoTSV",
  parent = DTAFileInfo,
  constructor = function(
    filename,
    number_of_files = 1,
    sep = "\t",
    has_header = TRUE,
    quote = '"',
    col_types = NULL
  ) {
    new_object(
      S7_object(),
      filename = filename,
      number_of_files = number_of_files,
      sep = sep,
      has_header = has_header,
      rownames = row_names,
      quote = quote,
      col_types = col_types
    ) 
  }
)

if (!exists("read_file", mode = "function")) {
  read_file <- new_generic("read_file", "x")
}
#' @title Read File for DTAFileInfoTSV Objects
#' @description
#' Reads a TSV file using the parameters specified in a
#' \code{DTAFileInfoTSV} object. This method uses \code{readr::read_tsv}
#' for efficient TSV parsing.
#'
#' @importFrom arrow read_delim_arrow
#' @param x A \code{DTAFileInfoTSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
##' @seealso \code{\link{readr::read_tsv}}
##' @name read_file-DTAFileInfoTSV
method(read_file, DTAFileInfoTSV) <- function(x, file) {
  if (DTAtools::matches_filename(x, file)) {
    return(arrow::read_delim_arrow(
      file,
      quote = x@quote,
      skip = if (x@has_header) 0 else 1,
      col_names = x@has_header,
      as_data_frame = FALSE
    ))
  } else {
    stop(simpleError(
      paste(
        "The provided file does not match the filename in the",
        "DTAFileInfoTSV object."
      )
    ))
  }
}