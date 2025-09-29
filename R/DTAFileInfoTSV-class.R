#' @title DTAFileInfoTSV Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileInfoTSV}, which extends \code{DTAFileInfo}
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
#' @return An object of class \code{DTAFileInfoTSV}.
#' @name DTAFileInfoTSV-class
#' @seealso \code{\link{DTAFileInfo}}
#'
#' @export
DTAFileInfoTSV <- S7::new_class(
  "DTAFileInfoTSV",
  parent = DTAFileInfoTabular,
  constructor = function(
    filename,
    pattern = FALSE,
    number_of_files = 1,
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      DTAFileInfoTabular(
        filename = filename,
        number_of_files = number_of_files,
        pattern = pattern,
        has_header = has_header,
        quote = quote,
        sep = "\t")
    )
  }
)


#' @title Read File for DTAFileInfoTSV Objects
#' @description
#' Reads a TSV file using the parameters specified in a
#' \code{DTAFileInfoTSV} object. This method uses \code{arrow::read_delim_arrow}
#' for efficient TSV parsing.
#'
#' @param x A \code{DTAFileInfoTSV} object containing file reading parameters.
#' @param file A character string specifying the path to the file to be read.
#'
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
##' @seealso \code{\link{arrow::read_tsv_arrow}}
##' @name read_file_execution-DTAFileInfoTSV
method(read_file_execution, DTAFileInfoTSV) <- function(x, file) {
  return(arrow::read_tsv_arrow(
    file,
    quote = x@quote,
    skip = if (x@has_header) 0 else 1,
    #col_names = x@has_header,
    as_data_frame = FALSE
  ))
}

#' @title Create Example DTAFileInfoTSV Object
#' @description
#' Creates an example \code{DTAFileInfoTSV} object using example files.
#' @param index example selector.
#' @return An example \code{DTAFileInfoTSV} object.
#' @examples
#' library(DTAtools)
#' create_example_DTAFileInfoTSV()
#' @export
create_example_DTAFileInfoTSV <- function(index = 1) {
  if (index == 1) {
    example_file <- system.file("extdata", "data_spec.tsv", package = "DTAtoolsR")
    DTAFileInfoTSV(
      filename = basename(example_file) # makes sure this was derived from existing example
    )
  } else {
    cli::cli_abort("Only index = 1 is supported for create_example_DTAFileInfoTSV().")
  }
}

#' @title Print DTAFileInfoTSV Object
#' @description
#' Print method for DTAFileInfoTSV objects.
#' @param x An object of class DTAFileInfoTSV
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @examples
#' library(DTAtools)
#' print(create_example_DTAFileInfoTSV())
#'
#' @name print
#' @export
method(print, DTAFileInfoTSV) <- function(x, ...) {
  cli_text("<DTAFileInfoTSV>\n")
  cli_alert_info("Filename: {x@filename}")
  cli_alert("Pattern: {x@pattern}")
  cli_alert("Number of files: {x@number_of_files}")
  cli_alert("Separator: {x@sep}")
  cli_alert("Has header: {x@has_header}")
  cli_alert("Quote: {x@quote}")
  invisible(x)
}


