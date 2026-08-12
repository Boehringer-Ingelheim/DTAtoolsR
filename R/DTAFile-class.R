#' @title DTAFile Class
#' @description
#' The \code{DTAFile} class stores metadata and parsing instructions for
#' delimited data files. It specifies file names (or patterns), the expected
#' number of files, and how to read them, including separator, header presence,
#' quoting, and column types.
#'
#' @import S7
#' @importFrom cli cli_abort
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param number_of_files Numeric or \code{NULL}; number of files
#'   expected. Default is \code{1}. If two numbers are provided,
#'   they represent min and maximum of files expected.
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#'
#' @return An object of class \code{DTAFile} containing file parsing
#'   information.
#' @name DTAFile-class
#' @details This class is used internally by the DTAtoolsR package to
#' manage metadata and properties of DTA files.
#' @keywords internal
#' @examples
#' file_info <- DTAFile("file.txt")
#' file_info_pattern <- DTAFile("file\\d+\\.txt", pattern = TRUE)
#' @export
DTAFile <- S7::new_class(
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
    if (is.null(pattern)) {
      pattern <- FALSE
    }

    if (
      is.null(number_of_files) &&
        is.null(min_number_of_files) &&
        is.null(max_number_of_files)
    ) {
      number_of_files <- 1
    }

    if (!pattern && number_of_files != 1) {
      cli::cli_abort(
        "if pattern is FALSE, then number_of_files must be 1. Then only one file can exist for this filename."
      )
    }

    if (length(number_of_files) > 1) {
      cli::cli_abort("'number_of_files' can only be length 1.")
    }

    if (
      !is.null(number_of_files) &&
        (!is.null(min_number_of_files) || !is.null(max_number_of_files))
    ) {
      cli::cli_abort(
        "You must not set both 'number_of_files' and 'min_number_of_files'/'max_number_of_files'. Choose one approach."
      )
    }

    if (
      !is.null(number_of_files) &&
        is.numeric(number_of_files) &&
        length(number_of_files) != 1
    ) {
      cli::cli_abort("'number_of_files' must be a single number or NULL.")
    }

    if (!is.null(number_of_files)) {
      if (!is.numeric(number_of_files)) {
        cli::cli_abort(
          "'number_of_files' must be a non-negative integer or NULL."
        )
      }
      min_number_of_files <- number_of_files
      max_number_of_files <- number_of_files
    }

    if (
      is.null(number_of_files) &&
        is.null(min_number_of_files) &&
        is.null(max_number_of_files)
    ) {
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
    if (
      !is.character(self@filename) ||
        is.null(self@filename) ||
        self@filename == ""
    ) {
      cli::cli_abort(
        "The 'filename' property must be a non-empty character vector."
      )
    }
    if (!is.logical(self@pattern) || length(self@pattern) != 1) {
      cli::cli_abort("The 'pattern' property must be a single logical value.")
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
#' @param ... Not used by current methods; reserved for future extensions.
#' @return The number of files.
#'
#' @examples
#' file_info <- DTAFile("file.txt", number_of_files = 1)
#' min_number_of_files(file_info)
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Returns the \code{min_number_of_files} property.}
#' }
#' @name min_number_of_files
#' @export
method(min_number_of_files, DTAFile) <- function(x, ...) {
  x@min_number_of_files[1]
}


if (!exists("max_number_of_files", mode = "function")) {
  max_number_of_files <- new_generic("max_number_of_files", "x")
}
#' @title Get max number of files
#' @description Returns the max number of files specified in a `DTAFile` object.
#'
#' @param x An object of class `DTAFile`.
#' @param ... Not used by current methods; reserved for future extensions.
#' @return The number of files.
#'
#' @examples
#' file_info <- DTAFile("file.txt", number_of_files = 1)
#' max_number_of_files(file_info)
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Returns the \code{max_number_of_files} property.}
#' }
#' @name max_number_of_files
#' @export
method(max_number_of_files, DTAFile) <- function(x, ...) {
  x@max_number_of_files
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
#' file_info <- DTAFile("file.txt")
#' matches_filename(file_info, "file.txt")
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Returns `TRUE` if the filename matches the pattern.}
#' }
#' @name matches_filename
#' @rdname matches_filename
#' @export
if (!exists("matches_filename", mode = "function")) {
  matches_filename <- new_generic("matches_filename", "x")
}

method(matches_filename, DTAFile) <- function(x, file) {
  file_name <- basename(file)

  if (x@pattern) {
    stringr::str_detect(file_name, x@filename)
  } else {
    return(file_name %in% x@filename)
  }
}

#' @title Read a file
#' @description Reads a data file using the parameters specified in a
#'   \code{DTAFile} object or one of its subclasses.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param ... Additional arguments; the concrete methods expect a single
#'   unnamed/named `file` argument giving the path to the file to be read.
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
if (!exists("read_file_execution", mode = "function")) {
  read_file_execution <- new_generic("read_file_execution", "x")
}

#' @export
method(read_file_execution, DTAFile) <- function(x, ...) {
  cli::cli_abort(
    "This method is not implemented. You need to
  use an object of a class which is derived from this class."
  )
}


#' @title Read a file based on DTAFile
#' @description Reads a data file using the parameters specified in a
#'   \code{DTAFile} object or one of its subclasses.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param file A character string specifying the path to the file to be read.
#' @param namecheck Logical; when \code{TRUE} (the default) the file name must
#'   match the object's filename or pattern.
#' @param specs A \code{DTAColumnSpecCollection} declaring the columns, or
#'   \code{NULL} (the default).
#'
#'   A reader that knows nothing about the specification has to guess a type per
#'   column, and it guesses before any code in this package sees the data: a
#'   column of quoted subject ids -- \code{"007"}, \code{"008"} -- is inferred as
#'   an integer and arrives in R as \code{7} and \code{8}. Handing the specs to
#'   the reader lets a column the specification declares as text be read as text.
#'
#'   \code{NULL} keeps the reader exactly as dumb as it was: every column is
#'   inferred. This is the behaviour of a standalone \code{read_file()} call on a
#'   bare \code{DTAFile}, which has no specification to consult;
#'   \code{\link{load_file}()} is where a dataset's specs and its file handler
#'   meet, and it is the caller that supplies them.
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
if (!exists("read_file", mode = "function")) {
  read_file <- new_generic("read_file", "x")
}

method(read_file, DTAFile) <- function(x, file, namecheck = TRUE, specs = NULL) {
  continue <- TRUE

  if (namecheck) {
    if (!DTAtools::matches_filename(x, basename(file))) {
      continue <- FALSE
      cli::cli_abort(
        stringr::str_glue("The provided file '{file}' does not match the filename or pattern in the DTAFile object.")
      )
    }
  }

  if (continue) {
    if (file.exists(file)) {
      read_file_execution(x, file, specs = specs)
    } else {
      cli::cli_abort(stringr::str_glue(
        "File '{file}' cannot be found."
      ))
    }
  }
}

#' @title Print DTAFile Object
#' @description
#' Print method for DTAFile objects.
#' @param x An object of class DTAFile
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_text cli_div
#' @examples
#' # do not use this, use derived classes instead, e.g.
#' # DTAFileCSV or DTAFileTSV
#' print(DTAFileCSV("example.csv"))
#' @name print
#' @export
method(print, DTAFile) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAFile}> : {.field {paste(x@filename, collapse = ', ')}}")

  print_info(x)

  invisible(x)
}


#' Print Information About a DTAFile Object
#'
#' This method prints detailed information about a \code{DTAFile} object, including its filename, pattern, and the number of files associated with it. The information is displayed using the \code{cli} package for formatted output.
#'
#' @importFrom cli cli_alert_info cli_alert
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
#' @seealso \code{\link{DTAFile}}
#' @name print_info
#' @export
if (!exists("print_info", mode = "function")) {
  print_info <- new_generic("print_info", "x")
}

# Registered outside the guard above: when `print_info` has already been
# created by an earlier-collated file, the method must still be attached,
# otherwise printing a plain DTAFile has no `print_info` method to dispatch to.
#' @export
method(print_info, DTAFile) <- function(x) {
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  if (x@min_number_of_files == x@max_number_of_files) {
    cli::cli_alert("Number of files: {x@min_number_of_files}")
  } else {
    cli::cli_alert("Min number of files: {x@min_number_of_files}")
    cli::cli_alert("Max number of files: {x@max_number_of_files}")
  }
  invisible(x)
}


#' Print Information About a DTAFile Object
#'
#' This method prints detailed information about a \code{DTAFile} object, including its filename, pattern, and the number of files associated with it. The information is displayed using the \code{cli} package for formatted output.
#'
#' @importFrom cli cli_alert
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
#' print_short_info(dta_file)
#'
#' @seealso \code{\link{DTAFile}}
#' @name print_short_info
#' @export
if (!exists("print_short_info", mode = "function")) {
  print_short_info <- new_generic("print_short_info", "x")
}
method(print_short_info, DTAFile) <- function(x, ...) {
  if (
    !x@pattern || (x@pattern && x@min_number_of_files == x@max_number_of_files)
  ) {
    cli::cli_alert("{x@filename} ({x@min_number_of_files})")
  } else {
    cli::cli_alert(
      "{x@filename} ({x@min_number_of_files}-{x@max_number_of_files})"
    )
  }

  invisible(x)
}
