#' @title DTAFileInfo Class
#' @description
#' The \code{DTAFileInfo} class stores metadata and parsing instructions for
#' delimited data files. It specifies file names (or patterns), the expected
#' number of files, and how to read them, including separator, header presence,
#' quoting, and column types.
#'
#' @import S7
#'
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
#' @return An object of class \code{DTAFileInfo} containing file parsing
#'   information.
#' @name DTAFileInfo-class
#' @details This class is used internally by the DTAtoolsR package to
#' @details This class is used internally by the DTAtoolsR package to
#' manage metadata and properties of DTA files.
#' @keywords internal
#' @examples
#' \dontrun{
#'   DTAFileInfo("file.txt")
#'   DTAFileInfo(c("file1.txt", "file2.txt"))
#'   DTAFileInfo("file\\d+\\.txt", pattern = TRUE)
#' }
#' @export
DTAFileInfo <- new_class(
  "DTAFileInfo",
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1
  ) {
    if (!pattern && number_of_files != 1) {
      stop("if pattern is FALSE, then number_of_files must be 1.")
    }

    if (!is.numeric(number_of_files) ||
        any(number_of_files < 0) ||
        length(number_of_files) > 2) {
      stop("'number_of_files' must be NULL, a single non-negative number, or a vector of two non-negative numbers (min and max).")
    }

    min_number_of_files = number_of_files
    max_number_of_files = number_of_files

    if(length(number_of_files) == 2) {
      if (number_of_files[1] >= number_of_files[2] ) {
        stop("If 'number_of_files' has two elements min and max, min must be less than max.")
      }
      min_number_of_files = number_of_files[1]
      max_number_of_files = number_of_files[2]
    }

    new_object(
      S7_object(),
      filename = filename,
      pattern = pattern,
      min_number_of_files = min_number_of_files,
      max_number_of_files = max_number_of_files
    )
  },
  properties = list(
    filename = class_character,
    pattern = class_logical,
    min_number_of_files = class_numeric_or_null,
    max_number_of_files = class_numeric_or_null
  ),
  validator = function(self) {
    if (!is.character(self@filename)) {
      stop("The 'filename' property must be a character vector.")
    }
    if (!is.logical(self@pattern) || length(self@pattern) != 1) {
      stop("The 'pattern' property must be a single logical value.")
    }
    if (!is.numeric(self@min_number_of_files)) {
      stop("The 'min_number_of_files' property must be numeric.")
    }
    if (!is.numeric(self@max_number_of_files)) {
      stop("The 'max_number_of_files' property must be numeric.")
    }
  }
)


if (!exists("min_number_of_files", mode = "function")) {
  min_number_of_files <- new_generic("min_number_of_files", "x")
}
#' @title Get min number of files
#' @description Returns the min number of files specified in a `DTAFileInfo` object.
#'
#' @param x An object of class `DTAFileInfo`.
#' @return The number of files.
#'
#' @examples
#' \dontrun{
#'   file_info <- DTAFileInfo("file.txt", number_of_files = 1)
#'   min_number_of_files(file_info)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFileInfo}}{Returns the \code{min_number_of_files} property.}
#' }
#' @name min_number_of_files
#' @export
method(min_number_of_files, DTAFileInfo) <- function(x) {
  x@min_number_of_files[1]
}


if (!exists("max_number_of_files", mode = "function")) {
  max_number_of_files <- new_generic("max_number_of_files", "x")
}
#' @title Get max number of files
#' @description Returns the max number of files specified in a `DTAFileInfo` object.
#'
#' @param x An object of class `DTAFileInfo`.
#' @return The number of files.
#'
#' @examples
#' \dontrun{
#'   file_info <- DTAFileInfo("file.txt", number_of_files = 1)
#'   max_number_of_files(file_info)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFileInfo}}{Returns the \code{max_number_of_files} property.}
#' }
#' @name max_number_of_files
#' @export
method(max_number_of_files, DTAFileInfo) <- function(x) {
  x@max_number_of_files
}


if (!exists("matches_filename", mode = "function")) {
  matches_filename <- new_generic("matches_filename", "x")
}
#' @title Matches Filename
#' @description Checks if a given filename matches the pattern in a `DTAFileInfo` object.
#'
#' @param x A `DTAFileInfo` object.
#' @param file A character string representing the name of the file to check against
#'   the stored filename or pattern
#' @return A logical value indicating whether the filename matches.
#'
#' @examples
#' \dontrun{
#'   file_info <- DTAFileInfo("file.txt")
#'   matches_filename(file_info, "file.txt")
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFileInfo}}{Returns `TRUE` if the filename matches the pattern.}
#' }
#' @name matches_filename
#' @rdname matches_filename
#' @export
method(matches_filename, DTAFileInfo) <- function(x, file) {
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
#'   \code{DTAFileInfo} object or one of its subclasses.
#'
#' @param x A \code{DTAFileInfo} object (or subclass) containing file reading
#'   parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return An Arrow Table containing the file's contents.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFileInfo}}{This is a base implementation that throws an error,
#'   as it must be implemented by a subclass.}
#' }
#' @name read_file_execution
#' @rdname read_file_execution
#' @export
method(read_file_execution, DTAFileInfo) <- function(x, file) {
  stop("This method is not implemented. You need to
  use an object of a class which is derived from this class.")
}


if (!exists("read_file", mode = "function")) {
  read_file <- new_generic("read_file", "x")
}
#' @title Read a file based on DTAFileInfo
#' @description Reads a data file using the parameters specified in a
#'   \code{DTAFileInfo} object or one of its subclasses.
#'
#' @param x A \code{DTAFileInfo} object (or subclass) containing file reading
#'   parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return An Arrow Table containing the file's contents.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFileInfo}}{This is a base implementation that throws an error,
#'   as it must be implemented by a subclass.}
#' }
#' @importFrom stringr str_glue
#' @name read_file
#' @rdname read_file
#' @export
method(read_file, DTAFileInfo) <- function(x, file) {
  if (DTAtools::matches_filename(x, basename(file))) {
    if(file.exists(file)) {
      read_file_execution(x, file)
    } else {
      stop(simpleError(str_glue("File '{file}' cannot be found.")))
    }
  } else {
    stop(simpleError("The provided file does not match the filename in the DTAFileInfoTabular object."))
  }
}


