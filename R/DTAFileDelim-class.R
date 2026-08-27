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
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#' @param sep Character. Single-character field separator used in the file.
#'   Defaults to tab (\code{"\\t"}).
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#' @param missing_values Character. Values the file writes for a missing cell
#'   (for instance \code{"."} in the SAS convention), honoured in addition to
#'   the empty string. The default \code{""} declares nothing and keeps the
#'   reader's own missing set.
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
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    sep = "\t",
    has_header = TRUE,
    quote = '"',
    missing_values = ""
  ) {
    new_object(
      DTAFileTabular(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern,
        pattern_description = pattern_description,
        has_header = has_header,
        quote = quote,
        sep = sep,
        missing_values = missing_values
      )
    )
  }
)


#' @title Read File for DTAFileDelim Objects
#' @description
#' Reads a delimited file using the parameters specified in a
#' \code{DTAFileDelim} object, via \code{dta_read_delim_normalized()}. Any
#' values declared in \code{missing_values} are honoured as missing, in
#' addition to the empty string, which is always treated as missing.
#'
#' @param x A \code{DTAFileDelim} object containing file reading parameters.
#' @param ... A `file` argument giving the path to the file to be read, and an
#'   optional `specs` argument: a `DTAColumnSpecCollection` whose declared
#'   types decide how the columns are parsed. Without it every column is
#'   inferred, as before.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
##' @name read_file_execution
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileDelim) <- function(x, ...) {
  args <- dta_reader_args(...)
  dta_read_delim_normalized(
    args$file,
    delim = x@sep,
    quote = x@quote,
    has_header = x@has_header,
    specs = args$specs,
    na = dta_reader_na_values(x)
  )
}

#' @title Open File Lazily for DTAFileDelim Objects
##' @name open_file_execution
#' @description
#' Opens a delimited file as a lazy \code{arrow::Dataset}, using the separator
#' declared on the \code{DTAFileDelim} object -- the same one its eager reader
#' uses. Any values declared in \code{missing_values} are honoured as missing,
#' in addition to the empty string, which is always treated as missing.
#' @param x A \code{DTAFileDelim} object containing file reading parameters.
#' @param ... A `file` argument giving the path to the file, and an optional
#'   `specs` argument: a `DTAColumnSpecCollection` whose declared types decide
#'   how the columns are parsed.
#' @return An \code{arrow::Dataset}.
#' @usage open_file_execution(x, ...)
method(open_file_execution, DTAFileDelim) <- function(x, ...) {
  args <- dta_reader_args(..., .caller = "open_file_execution")
  dta_open_normalized_dataset(
    args$file,
    specs = args$specs,
    delim = x@sep,
    quote = x@quote,
    has_header = x@has_header,
    na = dta_reader_na_values(x)
  )
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
