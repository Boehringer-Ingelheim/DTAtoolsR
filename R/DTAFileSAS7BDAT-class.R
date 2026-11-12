#' @title DTAFileSAS7BDAT Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileSAS7BDAT}, which extends
#' \code{\link{DTAFile}} to describe a SAS dataset (a \code{.sas7bdat} file) that
#' is read as input data and validated against a specification, the way a CSV
#' file is. It is declared in YAML as \code{type: sas7bdat}.
#'
#' The file is decoded by the \pkg{haven} package (ReadStat). A compressed
#' delivery (\code{ae.sas7bdat.gz}) needs no separate decompression step; see
#' the section \dQuote{Held in memory} for the temporary file this creates.
#'
#' It descends from \code{\link{DTAFile}}, not from \code{\link{DTAFileTabular}}:
#' a SAS dataset carries its own column names and types, so it has no separator,
#' header flag, quote character, missing-value marker, newline setting or
#' encoding to declare. The encoding is the one the file itself records.
#' \code{\link{DTADataSetTabular}} accepts this handler alongside the delimited
#' ones.
#'
#' @section What is read, and how:
#' \itemize{
#'   \item Values are taken as stored. A numeric column stays numeric (a SAS
#'     missing value, including the special \code{.A}-\code{.Z} ones, becomes
#'     \code{NA}) and a character column stays text.
#'   \item An empty character value is missing, because SAS has no other way to
#'     write a missing text value. Trailing blanks are not part of a SAS value
#'     and are dropped; leading blanks, leading zeros, non-ASCII characters and
#'     the literal text \code{"NA"} are kept as they are, so \code{"NA"} is a
#'     value here, unlike in a CSV file.
#'   \item Column names are exactly as stored, including their case. They are not
#'     cleaned, and a file with duplicate names is refused.
#'   \item Labels, display formats and every other SAS attribute are dropped.
#'   \item A column that SAS stores as a date, datetime or time (it carries a
#'     SAS date, datetime or time \emph{format}) is read as text, as the declared
#'     SAS format would show it, e.g. \code{15AUG2025} under \code{DATE9.}, so it
#'     can be checked like any other formatted text. Without a declaration for
#'     the column, or when the declaration names another kind of column or no
#'     format, it is ISO 8601 text (\code{2025-08-15},
#'     \code{2025-08-15T14:30:00}, \code{14:30:00}). When the column is declared
#'     \code{Num}, \code{Int} or \code{Char} it stays the SAS number: days (dates)
#'     or seconds (datetimes, times) counted from 1960-01-01, not 1970-01-01.
#'   \item A SAS date stored without a date format is just a number to the
#'     reader, and stays one; declaring it \code{SAS Date} then fails with a
#'     type error.
#'   \item A value is checked as stored, not as displayed: a number under a
#'     \code{w.d} display format is not rounded first.
#' }
#' A type that does not match the declaration is a column-specification error
#' (the file delivered a number where the specification wants text, a fraction
#' where it wants \code{SAS Int}, or a date where it wants a datetime), where the
#' same mismatch in a CSV file is an import error. The exception is text in a
#' column declared \code{SAS Num} or \code{SAS Int}: it is parsed as a CSV value
#' is, and a value that does not parse is an import error. A column declared
#' \code{SAS Int} is held as an Arrow double, not an integer, so a whole number
#' passes and a fraction, or a value outside the 32-bit integer range, is a
#' \code{type} column-specification error. Do not declare a \code{length} on a
#' SAS date, datetime or time column: it is checked against the text above, not
#' against the 8 bytes SAS stores.
#'
#' @section Held in memory:
#' A SAS dataset is always read into memory; there is no lazy reader.
#' \code{\link{load_file}()} with \code{stream = "auto"} reads it whatever its
#' size, and says so with a message of class \code{dta_sas7bdat_in_memory};
#' \code{stream = "always"} and \code{\link{open_file}()} are refused. Expect a
#' peak of about three times the size of the data, and more for a file SAS wrote
#' compressed, which can be far larger in memory than on disk. A file near the
#' app's upload limit can exhaust a worker. A \code{.sas7bdat.gz} needs no
#' separate decompression step, but \pkg{haven} unpacks it to a temporary file
#' in \code{tempdir()} of the full uncompressed size, which is removed when it
#' is garbage collected. Catalog files (\code{.sas7bcat}) are not read; ship
#' them as a \code{type: file} deliverable.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
#' @param number_of_files Numeric or \code{NULL}; the exact number of files
#'   expected. Must be a single value -- a length-2 vector is an error, not a
#'   range. Default is \code{1}. To express a range, set
#'   \code{min_number_of_files}/\code{max_number_of_files} instead; supplying
#'   \code{number_of_files} alongside either of them is an error.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character, list or \code{NULL}; free-text description of the
#'   file.
#' @param container Character or \code{NULL}; the name of the \code{.zip}
#'   archive or \code{.xpt} SAS transport file this file arrives inside.
#'   \code{*} matches any text and \code{?} one character
#'   (\code{"sdtm_*.zip"}); the name is matched as a whole, independently of
#'   \code{pattern}. \code{NULL} (the default) accepts the file loose or inside
#'   any container. Only \code{\link{load_delivery}()} consults it.
#'
#' @name DTAFileSAS7BDAT-class
#' @return An object of class \code{DTAFileSAS7BDAT}.
#'
#' @seealso \code{\link{DTAFile}}, \code{\link{DTADataSetTabular}},
#'   \code{\link{DTAFileFactory}}
#' @include DTAFile-class.R
#' @examples
#' handler <- DTAFileSAS7BDAT(filename = "ae.sas7bdat")
#' matches_filename(handler, "ae.sas7bdat")
#'
#' # A compressed delivery matches the name it carries underneath.
#' matches_filename(handler, "ae.sas7bdat.gz")
#'
#' # Reading a SAS dataset (the example file ships with haven).
#' sas_file <- system.file("examples", "iris.sas7bdat", package = "haven")
#' iris_handler <- DTAFileSAS7BDAT(filename = "iris.sas7bdat")
#' dim(read_file(iris_handler, sas_file))
#' @export
DTAFileSAS7BDAT <- S7::new_class(
  "DTAFileSAS7BDAT",
  parent = DTAFile,
  constructor = function(
    filename,
    pattern = FALSE,
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    container = NULL
  ) {
    new_object(
      .parent = DTAFile(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern,
        pattern_description = pattern_description,
        container = container
      )
    )
  }
)


#' @title Read File for DTAFileSAS7BDAT Objects
##' @name read_file_execution
#' @description
#' Reads a SAS dataset with \pkg{haven} and returns it as an Arrow Table, after
#' normalising it as described in \code{\link{DTAFileSAS7BDAT}}.
#' @param x A \code{DTAFileSAS7BDAT} object.
#' @param ... A `file` argument giving the path to the file to be read, and an
#'   optional `specs` argument: a `DTAColumnSpecCollection`. When supplied, it
#'   decides how a SAS date, datetime or time column is rendered.
#' @return An Arrow Table with the dataset's contents.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileSAS7BDAT) <- function(x, ...) {
  args <- dta_reader_args(...)
  dta_read_sas7bdat(args$file, specs = args$specs)
}


#' @title Read a SAS Dataset into an Arrow Table
#' @description
#' Reads `path` with [haven::read_sas()] and hands the result to
#' [dta_sas7bdat_normalise()]. haven reads a gzip-compressed file by unpacking
#' it to a temporary file in `tempdir()`.
#'
#' ReadStat prints its own failure text to standard output, which is captured
#' here so a refused file produces one condition and no stray console output.
#' @param path Path to a `.sas7bdat` or `.sas7bdat.gz` file.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @return An Arrow Table.
#' @section Errors:
#' A file that cannot be decoded -- empty, truncated, encrypted, not a SAS
#' dataset at all, or with a duplicate column name -- aborts with class `dta_sas7bdat_read_error` and the offending
#' `path` on the condition; haven's own message is shown as the reason, never
#' matched.
#' @keywords internal
dta_read_sas7bdat <- function(path, specs = NULL) {
  df <- tryCatch(
    {
      utils::capture.output(
        df <- haven::read_sas(path, .name_repair = "check_unique")
      )
      df
    },
    error = function(e) {
      # No `parent`: it would print haven's text a second time, with the full
      # server path. The bullet carries that text, path reduced to the base name.
      shown <- basename(path)
      reason <- dta_zip_reason(e, path) # every spelling of the path, not only the given one
      cli::cli_abort(
        c(
          "Cannot read {.file {shown}} as a SAS dataset.",
          "x" = "{reason}",
          "i" = "A duplicate column name, or an empty, truncated or encrypted file, or one that is not a {.file .sas7bdat}, is the usual cause."
        ),
        class = "dta_sas7bdat_read_error",
        path = path
      )
    }
  )

  # The file names its encoding and haven converts from it, but a header that
  # names the wrong one hands over bytes that are not UTF-8, and those would
  # be reported later as wrong values (or crash a check) far from the cause.
  dta_sas_refuse_invalid_utf8(
    df, path, "Cannot read {.file {shown}} as a SAS dataset.",
    "The file's header names a character encoding its text does not use; ask for the dataset to be written again with its encoding set correctly (UTF-8 is safest).",
    class = "dta_sas7bdat_read_error"
  )

  df <- dta_sas7bdat_normalise(df, specs)
  arrow::as_arrow_table(as.data.frame(df))
}

# Refuses a freshly read SAS data frame whose variable names or text values are
# not valid UTF-8, naming the first column and row but never printing the bad
# bytes. `heading` is a cli template interpolating `shown` (the base name).
dta_sas_refuse_invalid_utf8 <- function(df, path, heading, hint, class) {
  shown <- basename(path)
  bad_names <- which(!validUTF8(names(df)))
  if (length(bad_names)) {
    at <- bad_names[[1]]
    cli::cli_abort(
      c(heading, "x" = "Variable number {at} has a name that is not valid UTF-8 text.", "i" = "{hint}"),
      class = class, path = path
    )
  }
  for (col in names(df)) {
    x <- df[[col]]
    if (!is.character(x)) next
    row <- which(!validUTF8(x) & !is.na(x))
    if (length(row)) {
      row <- row[[1]]
      cli::cli_abort(
        c(heading, "x" = "Column {.field {col}} holds text that is not valid UTF-8, first in row {row}.", "i" = "{hint}"),
        class = class, path = path
      )
    }
  }
  invisible(df)
}


# SAS counts from 1960-01-01, R from 1970-01-01: the two epochs are 3653 days
# apart (3650, plus the leap days of 1960, 1964 and 1968).
`__DTAtools_sas_epoch_days__` <- 3653

#' @title Normalise a Freshly Read SAS Dataset
#' @description
#' Pure (no I/O) clean-up of the data frame haven returns, so the rest of the
#' package sees the same shapes it sees from a CSV file:
#'
#' * every attribute except `class`, `tzone`, `units` and `levels` is dropped
#'   (labels, SAS formats, display widths);
#' * an empty character value becomes `NA`;
#' * a column haven delivers as `Date`, `POSIXct` or `hms` is converted back to
#'   its SAS number and then, by what `specs` declares for the column, rendered
#'   as the declared format's display text, kept as the SAS number, or rendered
#'   as ISO 8601 text -- see [DTAFileSAS7BDAT].
#' @param df A data frame (haven returns a tibble).
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @return A data frame with the same columns.
#' @keywords internal
dta_sas7bdat_normalise <- function(df, specs = NULL) {
  keep <- c("class", "tzone", "units", "levels")
  temporal_types <- c("Date", "DateTime", "Time")

  for (column in names(df)) {
    x <- df[[column]]

    attrs <- attributes(x)
    attributes(x) <- attrs[intersect(names(attrs), keep)]

    kind <- if (inherits(x, "POSIXct")) {
      "DateTime"
    } else if (inherits(x, "Date")) {
      "Date"
    } else if (inherits(x, "difftime")) {
      "Time"
    }

    if (!is.null(kind)) {
      n <- switch(kind,
        Date = as.numeric(x) + `__DTAtools_sas_epoch_days__`,
        DateTime = as.numeric(x) + `__DTAtools_sas_epoch_days__` * 86400,
        Time = as.numeric(
          as.difftime(as.numeric(unclass(x)), units = attr(x, "units")),
          units = "secs"
        )
      )

      structure <- dta_spec_column_structure(specs, column)
      type <- tryCatch(structure@type, error = function(e) NULL)
      format <- tryCatch(structure@format, error = function(e) NULL)
      has_type <- is.character(type) && length(type) == 1 && !is.na(type) && nzchar(type)
      has_format <- is.character(format) && length(format) == 1 && !is.na(format) && nzchar(format)

      x <- if (has_type && identical(type, kind) && has_format) {
        dta_sas_render_temporal(n, kind, format)
      } else if (has_type && !type %in% temporal_types) {
        n
      } else {
        dta_sas_render_temporal(n, kind, NULL)
      }
    } else if (is.character(x)) {
      x[!is.na(x) & x == ""] <- NA_character_
    }

    df[[column]] <- x
  }

  df
}
