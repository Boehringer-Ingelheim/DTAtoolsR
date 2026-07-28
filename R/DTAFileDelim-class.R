#' @title DTAFileDelim Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileDelim}, which extends \code{DTAFile}
#' to represent metadata and configuration for TSV (Tab-Separated Values)
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
#' @return An object of class \code{DTAFileDelim}.
#' @name DTAFileDelim-class
#' @seealso \code{\link{DTAFile}}
#'
#' @export
DTAFileDelim <- S7::new_class(
  "DTAFileDelim",
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
      .parent = DTAFileTabular(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern,
        has_header = has_header,
        quote = quote,
        sep = "\t"
      )
    )
  }
)


#' @title Read File for DTAFileDelim Objects
#' @description
#' Reads a TSV file using the parameters specified in a
#' \code{DTAFileDelim} object. This method uses \code{arrow::read_delim_arrow}
#' for efficient TSV parsing.
#'
#' @importFrom arrow read_delim_arrow
#'
#' @param x A \code{DTAFileDelim} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be read.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
##' @seealso Uses \code{arrow::read_delim_arrow()} for parsing.
##' @name read_file_execution
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileDelim) <- function(x, ...) {
  file <- list(...)[[1]]
  table_obj <- arrow::read_delim_arrow(
    file,
    #col_types = x@col_types,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    #col_names = x@has_header,
    as_data_frame = FALSE
  )

  dta_normalize_column_names(table_obj)
}

#' @title Print DTAFileDelim Object
#' @description
#' Print method for DTAFileDelim objects.
#' @param x An object of class DTAFileDelim
#' @param ... Additional arguments (not used)
#' @importFrom cli cli_text cli_div
#' @return Invisibly returns the input object
#' @examples
#' library(DTAtools)
#' print(DTAFileDelim("example.tsv"))
#'
#' @name print
#' @export
method(print, DTAFileDelim) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAFileDelim}>")

  print_info(x)

  invisible(x)
}
