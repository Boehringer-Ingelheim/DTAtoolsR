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

    # A YAML sequence of file names parses to a LIST of strings, which the
    # character property would reject outright -- even though `filename` is
    # documented as a vector and matches_filename() implements the several-names
    # case. Flatten it here so a `filename:` sequence is the vector it means.
    if (is.list(filename) && all(vapply(filename, is.character, logical(1)))) {
      filename <- unlist(filename, use.names = FALSE)
    }

    if (
      is.null(number_of_files) &&
        is.null(min_number_of_files) &&
        is.null(max_number_of_files)
    ) {
      number_of_files <- 1
    }

    # A non-pattern handler names one exact file, so every count it declares
    # must be 1 -- whichever of the three arguments it used to say so. Testing
    # `number_of_files != 1` alone left two holes: a min/max pair was never
    # checked at all, and with only a min/max set `number_of_files` is NULL, so
    # the comparison was on a zero-length value and errored with a message about
    # the wrong thing.
    if (!pattern) {
      declared <- c(number_of_files, min_number_of_files, max_number_of_files)
      if (length(declared) > 0 && any(declared != 1)) {
        cli::cli_abort(
          "if pattern is FALSE, then number_of_files must be 1. Then only one file can exist for this filename."
        )
      }
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
    # `self@filename == ""` was a length-1 test on a property documented as a
    # VECTOR: a handler carrying two names made the `if` condition length 2,
    # which is an error in R, so the several-names case that
    # matches_filename() implements could never be constructed.
    if (
      !is.character(self@filename) ||
        length(self@filename) == 0 ||
        any(!nzchar(self@filename))
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


#' @title Compression Suffixes a Declared Filename Also Covers
#' @description
#' Compression is a transport detail, not part of the data's identity: a
#' specification that declares `data.csv` is satisfied by `data.csv.gz`. Arrow
#' decompresses these transparently on read, so the declaration never has to
#' mention them.
#'
#' Only gzip is listed. Arrow can read other codecs, but claiming a suffix here
#' that the reader then fails on trades a clear "no file matched" for an opaque
#' Arrow error.
#' @return A character vector of extensions, without the leading dot.
#' @keywords internal
dta_compression_extensions <- function() {
  "gz"
}

#' @title Drop a Compression Suffix From a Filename
#' @param file_name Character. A file's basename.
#' @return The name with a trailing compression extension removed, unchanged
#'   when it has none.
#' @keywords internal
dta_strip_compression_extension <- function(file_name) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext %in% dta_compression_extensions()) {
    tools::file_path_sans_ext(file_name)
  } else {
    file_name
  }
}

#' @title Matches Filename
#' @description Checks if a given filename matches the pattern in a `DTAFile` object.
#'
#' A compressed file matches the uncompressed name: `data.csv.gz` satisfies a
#' handler declared as `data.csv`, and one declared with the pattern
#' `^data\\.csv$`. The suffix is stripped from the candidate rather than
#' appended to the declaration, so existing anchored patterns keep working.
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
#' # A gzipped file matches the name the specification declares.
#' matches_filename(DTAFile("data.csv"), "data.csv.gz")
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
  dta_matches_filename_base(x, file)
}

# The name/pattern half of matches_filename(), shared with the DTAFileAny
# method, which ANDs its file-ending restriction onto this result.
#
# Factored out rather than reached through S7::super(): this codebase calls
# super() nowhere (the only two occurrences are commented out), and a plain
# helper keeps the base behaviour testable on its own.
#
# Returns one logical PER declared name or pattern -- a handler may carry
# several -- and every caller reduces the vector itself.
#' @keywords internal
dta_matches_filename_base <- function(x, file) {
  file_name <- basename(file)
  candidates <- unique(c(file_name, dta_strip_compression_extension(file_name)))

  if (x@pattern) {
    # `x@filename` may hold several patterns, and the result is one logical per
    # pattern. Folding with OR over the candidates preserves that shape.
    Reduce(`|`, lapply(candidates, function(nm) {
      stringr::str_detect(nm, x@filename)
    }))
  } else {
    any(candidates %in% x@filename)
  }
}

#' @keywords internal
dta_assert_single_file_path <- function(file, .caller) {
  if (!is.character(file) || length(file) != 1 || is.na(file) || !nzchar(file)) {
    cli::cli_abort(
      "{.fn {(.caller)}} requires {.arg file} to be a single non-missing, non-empty path."
    )
  }

  file
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
  file <- dta_assert_single_file_path(file, "read_file")
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


#' @title Open a File Lazily for a DTAFile
#' @description
#' The per-subclass half of \code{\link{open_file}()}, mirroring
#' \code{\link{read_file_execution}()}: it does the actual opening, having been
#' handed a file whose name has already been checked.
#'
#' Each concrete handler supplies its own delimiter, exactly as its
#' \code{read_file_execution()} method does, so a handler reads and opens the
#' same file the same way.
#'
#' @param x A \code{DTAFile} object (or subclass).
#' @param ... A \code{file} argument giving the path, and an optional
#'   \code{specs} argument (a \code{DTAColumnSpecCollection}) whose declared
#'   types pin the columns at parse time.
#' @return An \code{arrow::Dataset}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{DTAFile}}{Base implementation; aborts, because a handler that
#'   has not declared how it is delimited cannot be scanned.}
#' }
#' @name open_file_execution
#' @rdname open_file_execution
#' @export
if (!exists("open_file_execution", mode = "function")) {
  open_file_execution <- new_generic("open_file_execution", "x")
}

#' @export
method(open_file_execution, DTAFile) <- function(x, ...) {
  # Reached by a handler subclass that has not implemented lazy opening -- a
  # user-defined non-delimited format, say. The eager path is always available,
  # so the message names it rather than leaving the caller stuck.
  cli::cli_abort(c(
    "Streaming is not supported for {.cls {class(x)[[1]]}} file handlers.",
    "i" = "Load this file with {.code stream = \"never\"} to read it into memory instead."
  ))
}


#' @title Open a file lazily based on DTAFile
#' @description
#' The lazy counterpart of \code{\link{read_file}()}. Where \code{read_file()}
#' returns an Arrow \code{Table} holding the whole file in memory, this returns
#' an Arrow \code{Dataset} that is scanned in batches when it is used --
#' which is what lets a file larger than memory be validated at all.
#'
#' Both apply the same name checks and the same column-type pinning, so the two
#' differ in when the data is read, not in what it is read as.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param file A character string specifying the path to the file to be opened.
#' @param namecheck Logical; when \code{TRUE} (the default) the file name must
#'   match the object's filename or pattern.
#' @param specs A \code{DTAColumnSpecCollection} declaring the columns, or
#'   \code{NULL} (the default). Passed to the reader so that a column the
#'   specification declares as text is parsed as text rather than inferred --
#'   see \code{\link{read_file}()} for why that matters.
#' @return An \code{arrow::Dataset}.
#' @seealso \code{\link{read_file}()} for the materialising counterpart, and
#'   \code{\link{load_file}()}, whose \code{stream} argument chooses between
#'   them.
#' @examples
#' handler <- DTAFileCSV(filename = "clinical_data.csv")
#' file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#' ds <- open_file(handler, file)
#' # Nothing has been read yet; the columns are known from the header alone.
#' names(ds)
#' @importFrom stringr str_glue
#' @importFrom cli cli_abort
#' @name open_file
#' @rdname open_file
#' @export
if (!exists("open_file", mode = "function")) {
  open_file <- new_generic("open_file", "x")
}

#' @export
method(open_file, DTAFile) <- function(x, file, namecheck = TRUE, specs = NULL) {
  file <- dta_assert_single_file_path(file, "open_file")

  if (namecheck && !DTAtools::matches_filename(x, basename(file))) {
    cli::cli_abort(
      stringr::str_glue("The provided file '{file}' does not match the filename or pattern in the DTAFile object.")
    )
  }

  if (!file.exists(file)) {
    cli::cli_abort(stringr::str_glue("File '{file}' cannot be found."))
  }

  open_file_execution(x, file, specs = specs)
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
