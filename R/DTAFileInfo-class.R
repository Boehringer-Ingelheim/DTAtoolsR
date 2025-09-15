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


#' @title get max number of files
#' @description
#' Returns the number of files specified in the DTAFileInfo object.
#' @param x An object of class DTAContainer
#' @return number of files
#' @examples
#' \dontrun{
#' column_format <- number_of_files(dtafileinfo)
#' }
# Define the generic only if it doesn't already exist
#' @name number_of_files-DTAFileInfo
#' @export
if (!exists("number_of_files", mode = "function")) {
  number_of_files <- new_generic("number_of_files", "x")
}
method(number_of_files, DTAFileInfo) <- function(x) {
  return(sum(x@number_of_files))
}


#' @name matches_filename-DTAFileInfo
#' @title Matches Filename
#' @description Check if a filename matches a given pattern
#'
#' Determines whether the provided filename matches the specified pattern.
#'
#' @param file A character string representing the name of the file to check.
#' @return A logical value indicating whether the filename matches the pattern.
#' @examples
#' \dontrun{
#' matches_filename(dtafileinfo, "exact_file_name.tsv")
#' matches_filename(dtafileinfo, "\\.tsv$")
#' }
#' @export
if (!exists("matches_filename", mode = "function")) {
  matches_filename <- new_generic("matches_filename", "x")
}
method(matches_filename, DTAFileInfo) <- function(x, file) {
  if (x@pattern) {
    return(stringr::str_detect(file, x@filename))
  } else {
    return(file %in% x@filename)
  }
}

#' @title Read File for DTAFileInfo Objects
#' @description
#' This is a protype funnction for reading in files.
#'
#' @param x A \code{DTAFileInfo} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return An arrow table containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#' @name read_file-DTAFileInfo
#' @export
if (!exists("read_file", mode = "function")) {
  read_file <- new_generic("read_file", "x")
}
method(read_file, DTAFileInfo) <- function(x, file) {
  stop("This method is not implemented. You need to 
  use an object of a class which is derived from this class.")
}
