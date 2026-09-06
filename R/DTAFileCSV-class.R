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
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
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
#' @param missing_values Character. Values the file writes for a missing cell
#'   (for instance \code{"."} in the SAS convention), honoured in addition to
#'   the empty string. The default \code{""} declares nothing and keeps the
#'   reader's own missing set.
#' @param newlines_in_values Logical; \code{TRUE} if a quoted field may contain
#'   a line break. Default \code{FALSE}. See \code{\link{DTAFileTabular}}.
#' @param encoding Character; the file's character encoding. Default
#'   \code{"UTF-8"}. See \code{\link{DTAFileTabular}}.
#'
#' @name DTAFileCSV-class
#' @return An object of class \code{DTAFileCSV}.
#' @seealso \code{\link{DTAFile}}
#' @include DTAFileTabular-class.R
#' @export
DTAFileCSV <- S7::new_class(
  # nolint
  "DTAFileCSV",
  parent = DTAFileTabular,
  constructor = function(
    filename,
    pattern = FALSE,
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    has_header = TRUE,
    quote = '"',
    missing_values = "",
    newlines_in_values = FALSE,
    encoding = "UTF-8"
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
        sep = ",",
        has_header = has_header,
        quote = quote,
        missing_values = missing_values,
        newlines_in_values = newlines_in_values,
        encoding = encoding
      )
    )
  }
)


#' @title Read File for DTAFileCSV Objects
##' @name read_file_execution
#' @description
#' Reads a CSV file using the parameters specified in a \code{DTAFileCSV}
#' object, via \code{dta_read_delim_normalized()}. Any values declared in
#' \code{missing_values} are honoured as missing, in addition to the empty
#' string, which is always treated as missing.
#' @param x A \code{DTAFileCSV} object containing file reading parameters.
#' @param ... A `file` argument giving the path to the file to be read, and an
#'   optional `specs` argument: a `DTAColumnSpecCollection` whose declared
#'   types decide how the columns are parsed. Without it every column is
#'   inferred, as before.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileCSV) <- function(x, ...) {
  args <- dta_reader_args(...)
  dta_read_delim_normalized(
    args$file,
    delim = ",",
    quote = x@quote,
    has_header = x@has_header,
    specs = args$specs,
    na = dta_reader_na_values(x),
    handler = x
  )
}

#' @title Open File Lazily for DTAFileCSV Objects
##' @name open_file_execution
#' @description
#' Opens a CSV file as a lazy \code{arrow::Dataset} using the parameters
#' specified in a \code{DTAFileCSV} object. The comma is fixed, matching this
#' handler's eager reader. Any values declared in \code{missing_values} are
#' honoured as missing, in addition to the empty string, which is always
#' treated as missing.
#' @param x A \code{DTAFileCSV} object containing file reading parameters.
#' @param ... A `file` argument giving the path to the file, and an optional
#'   `specs` argument: a `DTAColumnSpecCollection` whose declared types decide
#'   how the columns are parsed.
#' @return An \code{arrow::Dataset}.
#' @usage open_file_execution(x, ...)
method(open_file_execution, DTAFileCSV) <- function(x, ...) {
  args <- dta_reader_args(..., .caller = "open_file_execution")
  dta_open_normalized_dataset(
    args$file,
    specs = args$specs,
    delim = ",",
    quote = x@quote,
    has_header = x@has_header,
    na = dta_reader_na_values(x),
    handler = x
  )
}

#' @title Create Example DTAFileCSV Object
#' @description
#' Creates an example \code{DTAFileCSV} object using example files.
#' @importFrom cli cli_abort
#' @param index example selector.
#' @return An example \code{DTAFileCSV} object.
#' @examples
#' library(DTAtools)
#' create_example_DTAFileCSV()
#' @export
create_example_DTAFileCSV <- function(index = 1) {
  if (index == 1) {
    example_file <- system.file(
      "extdata",
      "clinical_data.csv",
      package = "DTAtools"
    )
    DTAFileCSV(
      filename = basename(example_file) # makes sure this was derived from existing example
    )
  } else {
    cli::cli_abort(
      "Only index = 1 is supported for create_example_DTAFileCSV()."
    )
  }
}


#' @title Print DTAFileCSV Object
#' @description
#' Print method for DTAFileCSV objects.
#' @param x An object of class DTAFileCSV
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_div cli_text
#' @examples
#' library(DTAtools)
#' print(create_example_DTAFileCSV())
#'
#' @name print
#' @export
method(print, DTAFileCSV) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAFileCSV}>")

  print_info(x)

  invisible(x)
}
