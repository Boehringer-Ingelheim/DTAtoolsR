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
#'   \code{"\t"}).
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#' @param col_types Character string specifying the type of each column (e.g.,
#'   \code{"cccidcl"}). Default is \code{NULL}.
#'
#' @return An object of class \code{DTAFileInfo} containing file parsing
#'   information.
#'
#' @examples
#' \dontrun{
#'   DTAFileInfo("file.txt")
#'   DTAFileInfo(c("file1.txt", "file2.txt"))
#'   DTAFileInfo("file\\d+\\.txt", pattern = TRUE)
#' }
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
    self$filename <- filename
    self$number_of_files <- number_of_files
    self$sep <- sep
    self$has_header <- has_header
    self$rownames <- row_names
    self$quote <- quote
    self$col_types <- col_types
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
#' column_format <- numberOfFiles(dtafileinfo)
#' }
# Define the generic only if it doesn't already exist
#' @name numberOfFiles-DTAFileInfo
if (!exists("numberOfFiles", mode = "function")) {
  numberOfFiles <- new_generic("numberOfFiles", "x")
}
#' @export
method(numberOfFiles, DTAFileInfo) <- function(x) {
  return(sum(x@number_of_files))
}

#' Check if a filename matches a given pattern
#'
#' Determines whether the provided filename matches the specified pattern.
#'
#' @param file A character string representing the name of the file to check.
#' @return A logical value indicating whether the filename matches the pattern.
#' @examples
#' \dontrun{
#' matchesFilename(dtafileinfo, "exact_file_name.tsv")
#' matchesFilename(dtafileinfo, "\\.tsv$")
#' }
#' @export
method(matchesFilename, DTAFileInfo) <- function(x, file) {
  if (x@pattern) {
    return(stringr::str_detect(file, x@filename))
  } else {
    return(file %in% x@filename)
  }
}


if (!exists("readFile", mode = "function")) {
  readFile <- new_generic("readFile", "x")
}

#' @title Read File for DTAFileInfo Objects
#' @description
#' Reads a delimited file using the parameters specified in a
#' \code{DTAFileInfo} object. The function checks if the provided file
#' matches the expected filename pattern and, if so, reads the file with
#' the appropriate separator, header, quote, and column types.
#'
#' @importFrom readr read_delim
#' @param x A \code{DTAFileInfo} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return A data frame containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#'
#' @seealso \code{\link{read.delim}}
#' @export
method(readFile, DTAFileInfo) <- function(x, file) {
  if (DTAtools::matchesFilename(x, file)) {
    return(readr::read_delim(file,
                             sep = x@sep,
                             header = x@has_header,
                             quote = x@quote,
                             col_types = x@col_types))
  }

  return()
}
