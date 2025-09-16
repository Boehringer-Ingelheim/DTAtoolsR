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
#'  Defaults to tab ("\t").
#' @param has_header Logical. Indicates if the TSV file contains
#' a header row. Defaults to \code{TRUE}.
#' @param quote Character. Quoting character used in the TSV file.
#'  Defaults to double quote ('"').
#' @param col_types Optional. Column types specification. Defaults
#' to \code{NULL}.
#'
#' @return An object of class \code{DTAFileInfoTSV}.
#'
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
    self$filename <- filename
    self$number_of_files <- number_of_files
    self$sep <- sep
    self$has_header <- has_header
    self$quote <- quote
    self$col_types <- col_types
    new_object(
      S7_object(),
      fileinfo = self
    )
  }
)

#' @title Read File for DTAFileInfoTSV Objects
#' @description
#' Reads a TSV file using the parameters specified in a
#' \code{DTAFileInfoTSV} object. This method uses \code{readr::read_tsv}
#' for efficient TSV parsing.
#'
#' @importFrom readr read_tsv
#' @param x A \code{DTAFileInfoTSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#'
#' @seealso \code{\link{readr::read_tsv}}
#' @export
method(readFile, DTAFileInfoTSV) <- function(x, file) {
  if (DTAtools::matchesFilename(x, file)) {
    return(readr::read_tsv(
      file,
      col_types = x@col_types,
      quote = x@quote,
      skip = if (x@has_header) 0 else 1,
      col_names = x@has_header
    ))
  } else {
    stop(simpleError(
      "The provided file does not match the filename in the DTAFileInfoTSV object."
    ))
  }
}
