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
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param sep Character; field separator used in the file (e.g., \code{","},
#'   \code{"\\t"}).
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#' @param col_types Character string specifying the type of each column (e.g.,
#'   \code{"cccidcl"}). Default is \code{NULL}.
#'
#' @return An object of class \code{DTAFileInfo} containing file parsing
#'   information.
#' @name DTAFileInfo-class
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
    number_of_files = 1,
    sep,
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
  },
  properties = list(
    filename = class_any, # class_list or class_character
    number_of_files = class_numeric_or_null,
    sep = class_character,
    has_header = class_logical,
    rownames = class_logical,
    quote = class_logical_or_null,
    col_types = class_character
  )
)


if (!exists("number_of_files", mode = "function")) {
  number_of_files <- new_generic("number_of_files", "x")
}
#' @title Get number of files
#' @description Returns the number of files specified in a `DTAFileInfo` object.
#'
#' @param x An object of class `DTAFileInfo`.
#' @return The number of files.
#'
#' @examples
#' \dontrun{
#'   file_info <- DTAFileInfo("file.txt", number_of_files = 1)
#'   number_of_files(file_info)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFileInfo}}{Returns the sum of the \code{number_of_files} property.}
#' }
#' @name number_of_files
#' @export
method(number_of_files, DTAFileInfo) <- function(x) {
  sum(x@number_of_files)
}

if (!exists("matches_filename", mode = "function")) {
  matches_filename <- new_generic("matches_filename", "x")
}
#' @title Matches Filename
#' @description Checks if a given filename matches the pattern in a `DTAFileInfo` object.
#'
#' @param x A `DTAFileInfo` object.
#' @param file A character string representing the name of the file to check.
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
    file %in% x@filename
  }
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
#' @name read_file
#' @rdname read_file
#' @export
method(read_file, DTAFileInfo) <- function(x, file) {
  stop("This method is not implemented. Use a derived class.")
}
