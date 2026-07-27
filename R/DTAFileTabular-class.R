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
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#' @param missing_values Character. String representing missing values in the
#'   file. Default is \code{""}.
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
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
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    missing_values = "",
    sep = "\t",
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      .parent = DTAFile(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern
      ),
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
#' @name read_file_execution
#' @description
#' \code{DTAFileTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileTSV},
#' \code{DTAFileCSV} or \code{DTAFileDelim}.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileTabular} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileTabular) <- function(x, ...) {
  cli::cli_abort(
    "This method is not implemented. You need to
  use an object of a class which is derived from DTAFileTabular class."
  )
}

#' @keywords internal
dta_normalize_column_names <- function(table_obj) {
  current_names <- names(table_obj)
  cleaned_names <- trimws(gsub('^\\s*"|"\\s*$', '', current_names))

  if (!identical(current_names, cleaned_names)) {
    names(table_obj) <- cleaned_names
  }

  table_obj
}


#' Print Information About a DTAFile Object
#'
#' This method prints detailed information about a \code{DTAFile} object, including its filename, pattern, and the number of files associated with it. The information is displayed using the \code{cli} package for formatted output.
#'
#' @importFrom cli cli_alert_info cli_alert
#'
#' @param x A \code{DTAFile} object whose information is to be printed.
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @details
#' The function displays the filename and pattern of the \code{DTAFile} object. It also prints the minimum and maximum number of files, or a single value if both are equal.
#'
#' @examples
#' dta_file <- DTAFileCSV(filename = "data.csv")
#' print_info(dta_file)
#'
#' @name print_info
#' @seealso \code{\link{DTAFile}}
#' @export
if (!exists("print_info", mode = "function")) {
  print_info <- new_generic("print_info", "x")
}
method(print_info, DTAFileTabular) <- function(x) {
  # TODO This does not work, currently a workaround
  #super(print_info, x)
  #method(print_info, DTAFile)(x)
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  if (x@min_number_of_files == x@max_number_of_files) {
    cli::cli_alert("Number of files: {x@min_number_of_files}")
  } else {
    cli::cli_alert("Min number of files: {x@min_number_of_files}")
    cli::cli_alert("Max number of files: {x@max_number_of_files}")
  }
  cli::cli_alert("Separator: {x@sep}")
  cli::cli_alert("Has header: {x@has_header}")
  cli::cli_alert("Quote: {x@quote}")

  invisible(x)
}
