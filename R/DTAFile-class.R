#' @title DTAFile Class
#' @description
#' The \code{DTAFile} class stores metadata and parsing instructions for
#' delimited data files. It specifies file names (or patterns), the expected
#' number of files, and how to read them, including separator, header presence,
#' quoting, and column types.
#'
#' @import S7
#' @importFrom cli cli_abort cli_alert_info symbol
#' @importFrom stringr str_detect str_glue
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param number_of_files Numeric or \code{NULL}; number of files
#'   expected. Default is \code{1}. If two numbers are provided,
#'   they represent min and maximum of files expected.
#' @param sep Character; field separator used in the file (e.g., \code{","},
#'   \code{"\\t"}).
#'
#' @return An object of class \code{DTAFile} containing file parsing
#'   information.
#' @name DTAFile-class
#' @details This class is used internally by the DTAtoolsR package to
#' @details This class is used internally by the DTAtoolsR package to
#' manage metadata and properties of DTA files.
#' @keywords internal
#' @examples
#' \dontrun{
#'   DTAFile("file.txt")
#'   DTAFile(c("file1.txt", "file2.txt"))
#'   DTAFile("file\\d+\\.txt", pattern = TRUE)
#' }
#' @export
DTAFile <- new_class(
  "DTAFile",
  constructor = function(
    filename,
    pattern = FALSE,
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL
  ) {

    if (!pattern && number_of_files != 1) {
      cli_abort("if pattern is FALSE, then number_of_files must be 1. Then only one file can exist for this filename.")
    }

    if(length(number_of_files) > 1) {
      cli_abort("'number_of_files' can only be length 1.")
    }

    if (!is.null(number_of_files) && (!is.null(min_number_of_files) || !is.null(max_number_of_files))) {
      cli_abort("You must not set both 'number_of_files' and 'min_number_of_files'/'max_number_of_files'. Choose one approach.")
    }

    if(is.null(number_of_files) && is.null(min_number_of_files) && is.null(max_number_of_files)) {
      min_number_of_files <- 1
      max_number_of_files <- 1
    }

    new_object(
      S7_object(),
      filename = filename,
      pattern = pattern,
      pattern_description = pattern_description,
      min_number_of_files = min_number_of_files,
      max_number_of_files = max_number_of_files,
      info = info
    )
  },
  properties = list(
    filename = class_character,
    pattern = class_logical,
    pattern_description = class_character_or_null,
    min_number_of_files = class_numeric_or_null,
    max_number_of_files = class_numeric_or_null,
    info = class_character_or_list_or_null
  ),
  validator = function(self) {
    if (!is.character(self@filename) || is.null(self@filename) || self@filename == "") {
      cli::cli_abort("The 'filename' property must be a non-empty character vector.")
    }
    if (!is.logical(self@pattern) || length(self@pattern) != 1) {
      cli::cli_abort("The 'pattern' property must be a single logical value.")
    }
    if (!is.numeric(self@min_number_of_files)) {
      cli::cli_abort("The 'min_number_of_files' property must be numeric.")
    }
    if (!is.numeric(self@max_number_of_files)) {
      cli::cli_abort("The 'max_number_of_files' property must be numeric.")
    }
  }
)


if (!exists("min_number_of_files", mode = "function")) {
  min_number_of_files <- new_generic("min_number_of_files", "x")
}
#' @title Get min number of files
#' @description Returns the min number of files specified in a `DTAFile` object.
#'
#' @param x An object of class `DTAFile`.
#' @return The number of files.
#'
#' @examples
#' \dontrun{
#'   file_info <- DTAFile("file.txt", number_of_files = 1)
#'   min_number_of_files(file_info)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Returns the \code{min_number_of_files} property.}
#' }
#' @name min_number_of_files
#' @export
method(min_number_of_files, DTAFile) <- function(x) {
  x@min_number_of_files[1]
}


if (!exists("max_number_of_files", mode = "function")) {
  max_number_of_files <- new_generic("max_number_of_files", "x")
}
#' @title Get max number of files
#' @description Returns the max number of files specified in a `DTAFile` object.
#'
#' @param x An object of class `DTAFile`.
#' @return The number of files.
#'
#' @examples
#' \dontrun{
#'   file_info <- DTAFile("file.txt", number_of_files = 1)
#'   max_number_of_files(file_info)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Returns the \code{max_number_of_files} property.}
#' }
#' @name max_number_of_files
#' @export
method(max_number_of_files, DTAFile) <- function(x) {
  x@max_number_of_files
}


if (!exists("matches_filename", mode = "function")) {
  matches_filename <- new_generic("matches_filename", "x")
}
#' @title Matches Filename
#' @description Checks if a given filename matches the pattern in a `DTAFile` object.
#'
#' @param x A `DTAFile` object.
#' @param file A character string representing the name of the file to check against
#'   the stored filename or pattern
#' @return A logical value indicating whether the filename matches.
#' @importFrom stringr str_detect
#' @examples
#' \dontrun{
#'   file_info <- DTAFile("file.txt")
#'   matches_filename(file_info, "file.txt")
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Returns `TRUE` if the filename matches the pattern.}
#' }
#' @name matches_filename
#' @rdname matches_filename
#' @export
method(matches_filename, DTAFile) <- function(x, file) {
  if (x@pattern) {
    stringr::str_detect(file, x@filename)
  } else {
    return(file %in% x@filename)
  }
}

if (!exists("read_file_execution", mode = "function")) {
  read_file_execution <- new_generic("read_file", "x")
}
#' @title Read a file
#' @description Reads a data file using the parameters specified in a
#'   \code{DTAFile} object or one of its subclasses.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return An Arrow Table containing the file's contents.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{This is a base implementation that throws an error,
#'   as it must be implemented by a subclass.}
#' }
#' @name read_file_execution
#' @rdname read_file_execution
#' @export
method(read_file_execution, DTAFile) <- function(x, file) {
  stop("This method is not implemented. You need to
  use an object of a class which is derived from this class.")
}


if (!exists("read_file", mode = "function")) {
  read_file <- new_generic("read_file", "x")
}
#' @title Read a file based on DTAFile
#' @description Reads a data file using the parameters specified in a
#'   \code{DTAFile} object or one of its subclasses.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return An Arrow Table containing the file's contents.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{This is a base implementation that throws an error,
#'   as it must be implemented by a subclass.}
#' }
#' @importFrom stringr str_glue
#' @importFrom cli cli_abort
#' @name read_file
#' @rdname read_file
#' @export
method(read_file, DTAFile) <- function(x, file) {
  if (DTAtools::matches_filename(x, basename(file))) {
    if(file.exists(file)) {
      read_file_execution(x, file)
    } else {
      cli_abort(simpleError(str_glue("File '{file}' cannot be found.")))
    }
  } else {
    cli_abort(simpleError("The provided file does not match the filename in the DTAFileTabular object."))
  }
}

#' @title Print DTAFile Object
#' @description
#' Print method for DTAFile objects.
#' @param x An object of class DTAFile
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_text cli_div
#' @examples
#' \dontrun{
#'  # do not use this, use derived classes instead, e.g.
#'  # DTAFileCSV or DTAFileTSV
#'  DTAFile("example.tsv")
#' }
#' @name print
#' @export
method(print, DTAFile) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAFile}> : {.field {x@name}}")

  print_file_info(x)

  invisible(x)
}


#' Print Information About a DTAFile Object
#'
#' This method prints detailed information about a \code{DTAFile} object, including its filename, pattern, and the number of files associated with it. The information is displayed using the \code{cli} package for formatted output.
#'
#' @param x A \code{DTAFile} object whose information is to be printed.
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @details
#' The function displays the filename and pattern of the \code{DTAFile} object. It also prints the minimum and maximum number of files, or a single value if both are equal.
#'
#' @examples
#' \dontrun{
#' dta_file <- DTAFile(filename = "data.csv", pattern = "*.csv", min_number_of_files = 1, max_number_of_files = 1)
#' print_file_info(dta_file)
#' }
#'
#' @seealso \code{\link{DTAFile}}
#' @export
print_file_info <- new_generic("print_file_info", "x")

method(print_file_info, DTAFile) <- function(x) {
  cli_alert_info("Filename: {x@filename}")
  cli_alert("Pattern: {x@pattern}")
  if(x@min_number_of_files == x@max_number_of_files) {
    cli_alert("Number of files: {x@min_number_of_files}")
  } else {
    cli_alert("Min number of files: {x@min_number_of_files}")
    cli_alert("Max number of files: {x@max_number_of_files}")
  } 
  invisible(x)
}
