#' @title DTAFileXPT Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileXPT}, which extends
#' \code{\link{DTAFileSAS7BDAT}} to describe a single-dataset SAS transport
#' (XPORT, V5 or V8) file (a \code{.xpt} file) that is read as input data and
#' validated against a specification, the way a CSV file is. It is declared in
#' YAML as \code{type: xpt}.
#'
#' The file is read through \pkg{haven} and normalised exactly like a
#' \code{.sas7bdat} file: values as stored, blanks missing, temporal columns per
#' the declared format (see \link{DTAFileSAS7BDAT}). It is always read into
#' memory. A library holding several datasets is refused by this handler and
#' split by \link{load_delivery}(), which offers member AE as \code{ae.xpt}.
#' The option \code{DTAtools.container_max_member_bytes} bounds the size of the
#' dataset this handler reads, and \code{DTAtools.container_max_bytes} bounds
#' what a gzip-compressed file inflates to, whether it is read through
#' \code{read_file()}, \code{load_file()} or \code{load_delivery()}.
#'
#' A transport file records no character encoding, so its text is read as UTF-8
#' and a file holding other bytes is refused (deliver such a dataset as
#' \code{.sas7bdat}, which records its encoding). Rows are counted from the
#' file's own layout, so trailing all-blank text rows are kept; a V5 file whose
#' rows are shorter than 80 bytes does not say how many it holds, and its
#' trailing blank rows are lost.
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
#' @param container Character or \code{NULL}; where this file must arrive: a
#'   container id defined in the document's \code{containers} (\code{"sdtm"}),
#'   the name of a \code{.zip} archive or \code{.xpt} SAS transport file
#'   (\code{"sdtm_*.zip"}; \code{*} matches any text and \code{?} one
#'   character, the name is matched as a whole, independently of
#'   \code{pattern}), or \code{"none"} (no container needed, even when the
#'   document has a default container). \code{NULL} (the default) means the
#'   document's default container if it has one, otherwise the file may
#'   arrive loose or inside any container. An id resolves only inside a
#'   \code{\link{DTA}}.
#'
#' @name DTAFileXPT-class
#' @return An object of class \code{DTAFileXPT}.
#'
#' @seealso \code{\link{DTAFileSAS7BDAT}}, \code{\link{DTAFile}},
#'   \code{\link{DTADataSetTabular}}, \code{\link{DTAFileFactory}}
#' @include DTAFileSAS7BDAT-class.R
#' @examples
#' handler <- DTAFileXPT(filename = "ae.xpt")
#' matches_filename(handler, "ae.xpt")
#' matches_filename(handler, "ae.xpt.gz")
#'
#' # Write a one-dataset transport file and read it back.
#' path <- file.path(tempdir(), "ae.xpt")
#' haven::write_xpt(data.frame(USUBJID = c("S-001", "S-002"), AESEQ = c(1, 2)), path, name = "AE")
#' dim(read_file(handler, path))
#' unlink(path)
#' @export
DTAFileXPT <- S7::new_class(
  "DTAFileXPT",
  parent = DTAFileSAS7BDAT,
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
      .parent = DTAFileSAS7BDAT(
        filename = filename,
        pattern = pattern,
        pattern_description = pattern_description,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        container = container
      )
    )
  }
)


#' @title Read File for DTAFileXPT Objects
##' @name read_file_execution
#' @description
#' Reads a SAS transport file with \pkg{haven} and returns it as an Arrow Table,
#' after normalising it as described in \code{\link{DTAFileSAS7BDAT}}.
#' @return An Arrow Table with the file's contents.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileXPT) <- function(x, ...) {
  args <- dta_reader_args(...)
  dta_refuse_column_names(args)
  dta_read_xpt(args$file, specs = args$specs)
}


# Reads one SAS transport file: a library holding several datasets, an empty
# one, and one whose single member is larger than the container limit are each
# refused with their own class. The member is read through haven with its
# console output captured, and normalised exactly like a .sas7bdat. The file
# name reaches every message as a value, never as a format string.
dta_read_xpt <- function(path, specs = NULL) {
  members <- dta_xpt_members(path)
  shown <- basename(path)
  if (nrow(members) > 1) {
    found <- members$member
    cli::cli_abort(
      c(
        "{.file {shown}} holds {length(found)} SAS datasets ({.val {found}}); a {.code type: xpt} entry reads one.",
        "i" = "Give each dataset its own entry (member AE as {.file ae.xpt}) and deliver the library under a name no entry declares, or name it as their {.code container}; it is then split into one file per dataset."
      ),
      class = "dta_xpt_multiple_members",
      path = path,
      members = found
    )
  }
  if (nrow(members) == 0) {
    cli::cli_abort(
      c(
        "Cannot read {.file {shown}} as a SAS transport file.",
        "x" = "It holds no dataset."
      ),
      class = "dta_xpt_read_error",
      path = path
    )
  }
  # The member is read into memory: judge it by its size in the (inflated)
  # stream, which the scan above has already measured.
  size <- members$end - members$start + 240
  cap <- dta_container_limits()$max_member_bytes
  if (size > cap) {
    dta_container_too_large(path, "DTAtools.container_max_member_bytes", size, cap)
  }
  df <- tryCatch(
    {
      utils::capture.output(df <- haven::read_xpt(path, .name_repair = "check_unique"))
      df
    },
    error = function(e) {
      # No `parent`: the bullet already carries haven's text, path reduced.
      reason <- dta_zip_reason(e, path) # every spelling of the path, not only the given one
      cli::cli_abort(
        c(
          "Cannot read {.file {shown}} as a SAS transport file.",
          "x" = "{reason}"
        ),
        class = "dta_xpt_read_error",
        path = path
      )
    }
  )
  unreadable <- function(...) {
    cli::cli_abort(
      c("Cannot read {.file {shown}} as a SAS transport file.", ...),
      class = "dta_xpt_read_error",
      path = path,
      .envir = parent.frame()
    )
  }

  # ReadStat takes trailing all-blank observations for the padding that fills
  # the last 80-byte record and drops them, so a delivery would lose rows that
  # nullable: false must see. The member's own layout says how many there are;
  # blank text rows are restored so they take the same road as blank values
  # read from the file. Anything else that disagrees is not guessed at.
  expected <- dta_xpt_expected_rows(path, members$start, members$end, members$version)
  if (!is.na(expected) && nrow(df) != expected) {
    missing <- expected - nrow(df)
    if (missing > 0 && length(df) > 0 && all(vapply(df, is.character, logical(1)))) {
      cols <- lapply(df, function(x) {
        x[nrow(df) + seq_len(missing)] <- ""
        x
      })
      df <- structure(cols, class = class(df), row.names = .set_row_names(expected))
    } else {
      got <- nrow(df)
      unreadable(
        "x" = "The file header announces {expected} row{?s}, but {got} could be read."
      )
    }
  }

  # A transport file records no character encoding. haven hands the bytes over
  # as they are, and text that is not UTF-8 would crash the checks later, far
  # from its cause: refuse it here, and never print the bad bytes.
  dta_sas_refuse_invalid_utf8(
    df, path, "Cannot read {.file {shown}} as a SAS transport file.",
    "A transport file records no character encoding and DTAtools reads its text as UTF-8; ask for a file written by a UTF-8 (or ASCII-only) SAS session, or deliver the dataset as .sas7bdat, which records its encoding.",
    class = "dta_xpt_read_error"
  )
  arrow::as_arrow_table(as.data.frame(dta_sas7bdat_normalise(df, specs)))
}
