#' @title DTAFileTSV Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileTSV}, which extends \code{DTAFile}
#' to represent metadata and configuration for TSV (Tab-Separated Values)
#'  data files.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#'
#' @return An object of class \code{DTAFileTSV}.
#' @name DTAFileTSV-class
#' @seealso \code{\link{DTAFile}}
#'
#' @export
DTAFileTSV <- S7::new_class(
  "DTAFileTSV",
  parent = DTAFileTabular,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      DTAFileTabular(
        filename = filename,
        number_of_files = number_of_files,
        pattern = pattern,
        has_header = has_header,
        quote = quote,
        sep = "\t")
    )
  }
)


#' @title Read File for DTAFileTSV Objects
#' @description
#' Reads a TSV file using the parameters specified in a
#' \code{DTAFileTSV} object. This method uses \code{arrow::read_delim_arrow}
#' for efficient TSV parsing.
#'
#' @param x A \code{DTAFileTSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
##' @seealso \code{\link{arrow::read_tsv_arrow}}
##' @name read_file_execution-DTAFileTSV
method(read_file_execution, DTAFileTSV) <- function(x, file) {
  return(arrow::read_tsv_arrow(
    file,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    #col_names = x@has_header,
    as_data_frame = FALSE
  ))
}

#' @title Create Example DTAFileTSV Object
#' @description
#' Creates an example \code{DTAFileTSV} object using example files.
#' @param index example selector.
#' @return An example \code{DTAFileTSV} object.
#' @examples
#' library(DTAtools)
#' create_example_DTAFileTSV()
#' @export
create_example_DTAFileTSV <- function(index = 1) {
  if (index == 1) {
    example_file <- system.file("extdata", "gf_data_small.tsv", package = "DTAtools")
    DTAFileTSV(
      filename = basename(example_file) # makes sure this was derived from existing example
    )
  } else {
    cli::cli_abort("Only index = 1 is supported for create_example_DTAFileTSV().")
  }
}

#' @title Print DTAFileTSV Object
#' @description
#' Print method for DTAFileTSV objects.
#' @param x An object of class DTAFileTSV
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @examples
#' library(DTAtools)
#' print(create_example_DTAFileTSV())
#'
#' @name print
#' @export
method(print, DTAFileTSV) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAFileTSV}>")

  cli_alert_info("Filename: {x@filename}")
  cli_alert("Pattern: {x@pattern}")
  if (!is.null(x@min_number_of_files) && !is.null(x@max_number_of_files)) {
    if (x@min_number_of_files == x@max_number_of_files) {
      cli_alert("Files required: {x@min_number_of_files}")
    } else {
      cli_alert("Files required: {x@min_number_of_files} to {x@max_number_of_files}")
    }
  }

  cli_alert("Separator: {x@sep}")
  cli_alert("Has header: {x@has_header}")
  cli_alert("Quote: {x@quote}")
  invisible(x)
}


