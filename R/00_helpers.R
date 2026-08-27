#' Null-Coalescing Operator
#'
#' `%||%` is used throughout the package but only became part of base R in
#' 4.4.0, while `DESCRIPTION` declares `R (>= 4.1.0)`. Importing it from
#' `rlang` keeps the package working on the older R versions it claims to
#' support.
#'
#' @importFrom rlang %||%
#' @name null-default
#' @noRd
NULL

class_null <- S7::new_S3_class("NULL")
class_Date_or_null <- S7::class_Date | class_null
class_DTAColumnSpec <- S7::new_class("DTAColumnSpec")
class_DTAColumnSpecCollection <- S7::new_class(
  "DTAColumnSpecCollection"
)
class_DTAColumnSpecStructure <- S7::new_class(
  "DTAColumnSpecStructure"
)
class_DTAColumnSpecStructure_or_null <- class_DTAColumnSpecStructure |
  class_null
class_DTADataSet <- S7::new_class("DTADataSet")
class_DTAMetaData <- S7::new_class("DTAMetaData")
class_list_or_null <- S7::class_list | class_null
class_character_or_null <- S7::class_character | class_null
class_numeric_or_null <- S7::class_numeric | class_null
class_character_or_numeric_or_null <- S7::class_character |
  S7::class_numeric |
  class_null
class_logical_or_null <- S7::class_logical | class_null
class_character_or_list <- S7::class_character |
  S7::class_list
class_character_or_list_or_null <- S7::class_character |
  S7::class_list |
  class_null
class_character_or_numeric_or_null_or_list <- S7::class_character |
  S7::class_numeric |
  class_null |
  S7::class_list

`__extract_prefix_and_rest__` <- function(x) {
  if (is.null(x)) {
    return(list(prefix = NULL, rest = NULL))
  }
  parts <- stringr::str_split(x, "\\s+", n = 2)[[1]]
  prefix <- parts[1]
  rest <- if (length(parts) > 1) parts[2] else NULL
  if (is.null(prefix) || prefix == "") {
    cli_abort("No prefix could be extracted from '{x}'.")
  }
  list(prefix = prefix, rest = rest)
}

# Narrow an error count back to integer, but only when it fits.
#
# Error counts are accumulated as doubles. Both the schema and the import axis
# produce one error per bad cell, and counting -- unlike retention -- is
# deliberately uncapped, so a large dirty file runs the total past
# `.Machine$integer.max`. Integer accumulation there does not raise an error: it
# yields `NA` with a warning, and the `NA` then propagates into the pass/fail
# verdict, so the files too broken to count were the ones that stopped being
# judged. Doubles represent whole numbers exactly to 2^53.
#
# Integer is still what every consumer of `details` has always seen, so counts
# are narrowed back on the way out. A count that genuinely exceeds the integer
# range stays a double rather than becoming `NA`.
#
# The range test is two-sided even though counts are never negative: a one-sided
# `n <= .Machine$integer.max` is true of every negative number, so it would hand
# `as.integer()` exactly the values it cannot represent.
dta_narrow_count <- function(n) {
  if (length(n) == 1 && !is.na(n) && abs(n) <= .Machine$integer.max) {
    return(as.integer(n))
  }
  n
}

#' @title Render a Count (or Row Number) for a Message
#' @description
#' Counts and row numbers are deliberately kept as doubles past
#' `.Machine$integer.max` (see `dta_narrow_count()`), but `sprintf("%d", ...)`
#' does not render such a double -- it errors -- and `as.character()`/`paste()`
#' render it in scientific notation (`3e+09`). Both failure modes surfaced at
#' exactly the scale the doubles exist for: a message assembled after a
#' multi-hour scan either crashed or reported unreadable evidence. This is the
#' one renderer every message builder shares: plain digits at any magnitude,
#' identical to `%d` for values that fit an integer.
#' @param n A numeric vector of whole numbers.
#' @return A character vector of the numbers in plain (non-scientific) digits.
#' @keywords internal
dta_format_count <- function(n) {
  format(n, scientific = FALSE, trim = TRUE, big.mark = "")
}

#' @title Render a Group-Label Value
#' @description
#' Group-condition messages render each grouping column's value with
#' `as.character()`, e.g. `"AGE=1000000"`. For a numeric column that used to
#' show `"1e+06"` where its integer twin showed `"1000000"` -- and which one
#' you got depended on the int-vs-double narrowing decision, which
#' legitimately differs between the streamed (per-batch) and eager
#' (whole-table) paths. `digits = 15` mirrors `as.character()`'s precision,
#' so the rendering matches `as.character()` wherever that form is
#' non-scientific; a value whose `as.character()` form IS scientific (very
#' small or very large magnitudes) expands to plain digits instead, which can
#' be long for extreme values -- identical on both paths either way.
#' @param x A single group-column value.
#' @return A length-1 character string.
#' @keywords internal
dta_group_label_value <- function(x) {
  if (is.numeric(x)) {
    return(format(x, digits = 15L, scientific = FALSE, trim = TRUE, big.mark = ""))
  }
  as.character(x)
}

#' @title Narrow Reported Row Numbers Back to Integer
#' @description
#' The streaming driver turns a batch-local row number into a global one by
#' adding the number of rows already consumed. That offset is a double, for the
#' reason given above `dta_narrow_count()`: an integer offset silently becomes
#' `NA` past `.Machine$integer.max`, and every reported row number goes with it,
#' on exactly the files the streaming path exists to handle.
#'
#' Adding a double to an integer vector widens the whole vector, so without this
#' the reported `row` column would change type for every file, however small.
#' Integer is what every consumer of the error frames has always seen, so the
#' vector is narrowed back whenever it still fits.
#'
#' The range test is two-sided, mirroring `dta_narrow_count()`: a one-sided
#' `v <= .Machine$integer.max` is true of every negative number, so it would
#' hand `as.integer()` precisely the values it cannot represent. `NA` passes the
#' test because `as.integer(NA)` is `NA_integer_` and loses nothing; a row
#' number genuinely beyond the integer range stays a double rather than becoming
#' `NA`.
#'
#' @param v A numeric vector of row numbers.
#' @return `v` as an integer vector when every value fits the integer range, and
#'   `v` unchanged otherwise.
#' @keywords internal
dta_narrow_rows <- function(v) {
  if (all(is.na(v) | abs(v) <= .Machine$integer.max)) {
    return(as.integer(v))
  }
  v
}

`__DTAtools_supported_backends__` <- c("SAS")
`__DTAtools_supported_dataset_types__` <- c("tabular", "file")
`__DTAtools_stream_modes__` <- c("auto", "always", "never")

# The size above which `stream = "auto"` keeps a file lazy rather than reading
# it into memory. 512 MB is chosen to sit well below the point where an R
# session on a typical analyst machine starts to struggle: an Arrow table is
# several times its on-disk size once strings are materialised, so a 512 MB CSV
# is already a multi-gigabyte object.
`__DTAtools_stream_threshold_default__` <- 512 * 1024^2

# What a gzip-compressed delimited file typically expands to. Text tables
# compress well, so comparing the on-disk size of a .gz against the threshold
# made "auto" read multi-gigabyte tables into memory -- the inversion of its
# purpose, on exactly the inputs large enough to be shipped compressed. A
# fixed factor rather than gzip's ISIZE trailer, because ISIZE is the size
# modulo 2^32 and lies for members past 4 GB -- the very files that matter
# here.
`__DTAtools_gz_expansion_ratio__` <- 4

#' @title Decide Whether to Stream a File
#' @description
#' Turns the user-facing `stream` argument into the single yes/no the readers
#' need. `"auto"` is the only mode that looks at the file at all; the other two
#' are the user overriding the guess in either direction.
#'
#' `TRUE` and `FALSE` are accepted as aliases for `"always"` and `"never"`,
#' because a logical is the first thing most callers will try.
#'
#' @param stream One of `"auto"`, `"always"`, `"never"`, or a single logical.
#'   Defaults to `getOption("DTAtools.stream")`, itself defaulting to `"auto"`.
#' @param file Path to the file about to be read. Only consulted for `"auto"`.
#' @return A single `TRUE` (keep it lazy) or `FALSE` (read it into memory).
#' @details
#' The `"auto"` threshold is `getOption("DTAtools.stream_threshold")`, in bytes.
#'
#' For a gzip-compressed file the on-disk size is multiplied by a fixed
#' expansion estimate (4x) before the comparison, so a compressed table large
#' enough to blow up an R session streams by default. The estimate is coarse
#' on purpose -- gzip's own ISIZE trailer is the size modulo 2^32 and lies for
#' members past 4 GB -- and either `stream` override still wins.
#' @keywords internal
dta_resolve_stream_mode <- function(
  stream = getOption("DTAtools.stream", "auto"),
  file = NULL
) {
  # A logical is what most people reach for first, so it is accepted rather than
  # rejected on a technicality. NA is not: it is not a decision.
  if (is.logical(stream)) {
    if (length(stream) != 1 || is.na(stream)) {
      cli_abort(
        "{.arg stream} must be a single non-missing value, not {.val {stream}}."
      )
    }
    return(stream)
  }

  if (!is.character(stream) || length(stream) != 1 || is.na(stream)) {
    cli_abort(
      "{.arg stream} must be one of {.val {`__DTAtools_stream_modes__`}}, or a single logical."
    )
  }

  if (!stream %in% `__DTAtools_stream_modes__`) {
    cli_abort(
      "{.arg stream} must be one of {.val {`__DTAtools_stream_modes__`}}, not {.val {stream}}."
    )
  }

  if (identical(stream, "always")) {
    return(TRUE)
  }
  if (identical(stream, "never")) {
    return(FALSE)
  }

  # "auto" from here. A file that cannot be sized cannot be judged too big, so
  # the safe answer is the historical one: read it into memory.
  if (is.null(file) || length(file) != 1 || is.na(file) || !file.exists(file)) {
    return(FALSE)
  }

  threshold <- getOption(
    "DTAtools.stream_threshold",
    `__DTAtools_stream_threshold_default__`
  )

  size <- file.size(file)
  if (is.na(size)) {
    return(FALSE)
  }

  # Compare an ESTIMATE of the materialised size, not the bytes on disk: a
  # compressed file is several times smaller on disk than in memory, so the
  # raw comparison under-triggered for exactly the large inputs "auto" exists
  # to protect.
  if (tolower(tools::file_ext(file)) %in% dta_compression_extensions()) {
    size <- size * `__DTAtools_gz_expansion_ratio__`
  }

  size > threshold
}
# All file types this package's `DTAFileFactory()` knows how to construct a
# handler for. This list is deliberately just the union -- it does not by
# itself keep `type: any` out of a tabular dataset or `type: csv` out of a
# file dataset; that per-dataset-type enforcement lives in the dataset
# validators, e.g. `DTADataSetTabular`'s, which requires every handler to
# inherit from `DTAtools::DTAFileTabular` rather than checking against a type
# string here.
`__DTAtools_supported_file_types__` <- c("any", "csv", "tsv") # TODO: "sas7bdat", ..

#' @title Check Generic
#' @description
#' Generic function for validating DTA-related objects (e.g. \code{DTA},
#' \code{DTADataSet}, \code{DTADataSetTabular}). Defined here (rather than in
#' individual class files) because R files are loaded alphabetically and
#' several class files need this generic to already exist when they register
#' their methods.
#' @param x An object to check.
#' @param ... Additional arguments passed to methods.
#' @return Depends on the method implementation.
#' @name check
#' @export
if (!exists("check", mode = "function")) {
  check <- S7::new_generic("check", "x")
}

#' Create a DTAColumnSpecStructure Object
#'
#' Constructs a DTAColumnSpecStructure object for a specified backend (e.g., SAS or R),
#' based on the provided type, format, and length. The function validates that the
#' prefixes of type and format are supported and match, then dispatches to the appropriate
#' backend-specific constructor.
#'
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param format Character. The format specification, potentially prefixed with a backend identifier.
#' @param length Integer or NULL. The length specification for the column (optional).
#'
#' @return An object of class \code{DTAColumnSpecStructureSAS}.
#'
#' @details
#' The function checks that the prefixes of \code{type} and \code{format} are among the supported
#' backends and that they match. If both are provided and do not match, an error is thrown.
#' The backend is determined by the prefix of \code{type} or \code{format}.
#'
#' @examples
#' library(DTAtools)
#' DTAColumnSpecStructureFactory(type = "SAS Char", format = "SAS $10.", length = 10)
#'
#' @seealso \code{\link{DTAColumnSpecStructureSAS}}
#' @export
DTAColumnSpecStructureFactory <- function(
  type = NULL,
  format = NULL,
  length = NULL
) {
  type_info <- `__extract_prefix_and_rest__`(type)
  format_info <- `__extract_prefix_and_rest__`(format)

  if (
    !is.null(type_info$prefix) &&
      !type_info$prefix %in% `__DTAtools_supported_backends__`
  ) {
    cli_abort(
      "'type' prefix '{type_info$prefix}'must be one of the supported backends: {str_flatten_comma(`__DTAtools_supported_backends__`)}"
    )
  }

  if (
    !is.null(format_info$prefix) &&
      !format_info$prefix %in% `__DTAtools_supported_backends__`
  ) {
    cli_abort(
      "'format' prefix '{format_info$prefix}' must be one of the supported backends: {str_flatten_comma(`__DTAtools_supported_backends__`)}"
    )
  }

  # If both type and format are provided, check backend support and that prefixes match
  if (
    !is.null(type_info$prefix) &&
      !is.null(format_info$prefix) &&
      type_info$prefix != format_info$prefix
  ) {
    cli_abort(
      "The 'type' and 'format' prefixes must be the same. Got '{type_info$prefix}' and '{format_info$prefix}'."
    )
  }

  backend <- if (!is.null(type_info$prefix)) {
    type_info$prefix
  } else {
    format_info$prefix
  }

  switch(backend,
    SAS = DTAtools::DTAColumnSpecStructureSAS(
      type = type_info[["rest"]],
      format = format_info[["rest"]],
      length = length
    ),
    cli::cli_abort("Backend '{backend}' not implemented.")
  )
}


#' @title Create a DTADataSetFactory Object
#'
#' @description
#' Constructs a DTADataSetFactory object for a specified backend (e.g., SAS or R),
#' based on the provided type.
#'
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param columns A list with column specifications (optional)
#' @param rules A list of rules specifications (optional)
#' @param files Specification of the dataset's file handlers (optional). Either
#'   a single named list -- one handler, the shape a YAML \code{files:} mapping
#'   parses to -- or an unnamed list of such lists, one per handler, the shape a
#'   YAML \code{files:} sequence parses to. \code{NULL} or an empty list yields a
#'   dataset with no file handlers.
#' @param ... Character. Arguments passed on to the specific backend constructor.
#' @importFrom cli cli_abort
#' @return An object derived from class \code{DTADataSet} like \code{DTADataSetTabular},
#' depending on the backend specified.
#'
#' @examples
#' library(DTAtools)
#' DTADataSetFactory(
#'   type = "file",
#'   name = "mydataset",
#'   files = list(type = "csv", filename = "clinical_data.csv")
#' )
#'
#' # Several file handlers: an unnamed list of handler specifications.
#' DTADataSetFactory(
#'   type = "file",
#'   name = "mydataset",
#'   files = list(
#'     list(type = "csv", filename = "clinical_data.csv"),
#'     list(type = "tsv", filename = "gf_data_small_smirna.tsv")
#'   )
#' )
#'
#' @seealso \code{\link{DTADataSet}}, \code{\link{DTADataSetTabular}}
#' @export
DTADataSetFactory <- function(
  type,
  columns = NULL,
  rules = NULL,
  files = NULL,
  ...
) {
  # check that type is not NULL and
  if (!type %in% `__DTAtools_supported_dataset_types__`) {
    cli_abort(
      "'type' prefix '{type}'must be one of the supported: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}"
    )
  }

  file_handlers <- dta_file_handlers_from_list(files)

  switch(type,
    "tabular" = {
      return(DTADataSetTabular(
        specs = specs_from_list(columns = columns, rules = rules),
        files = file_handlers,
        ...
      ))
    },
    "file" = {
      return(DTADataSetFile(
        files = file_handlers,
        ...
      ))
    },
    cli_abort("Dataset type '{type}' not implemented.")
  )
}


#' @title Build the file handlers of a dataset from a list
#' @description
#' Turns the \code{files} element of a dataset specification into the list of
#' \code{DTAFile} objects a \code{DTADataSet} expects.
#'
#' A dataset may declare more than one file handler, so \code{files} is accepted
#' in both shapes YAML can produce: a mapping (one handler, a *named* list) or a
#' sequence of mappings (several handlers, an *unnamed* list). Names are what
#' separates the two -- \code{yaml::read_yaml()} names the elements of a mapping
#' and leaves those of a sequence unnamed -- so a handler whose own value is a
#' sequence (\code{info:}) is still read as the single mapping it is.
#'
#' @param files \code{NULL}, a named list (one handler), or an unnamed list of
#'   named lists (one per handler).
#' @importFrom cli cli_abort
#' @return A list of \code{DTAFile} objects, empty when \code{files} is
#'   \code{NULL} or empty.
#' @examples
#' library(DTAtools)
#' dta_file_handlers_from_list(list(type = "csv", filename = "clinical_data.csv"))
#' dta_file_handlers_from_list(list(
#'   list(type = "csv", filename = "a.csv"),
#'   list(type = "tsv", filename = "b.tsv")
#' ))
#' dta_file_handlers_from_list(NULL)
#' @export
dta_file_handlers_from_list <- function(files) {
  if (is.null(files) || length(files) == 0) {
    return(list())
  }

  if (!is.list(files)) {
    cli_abort(
      "'files' must be a list describing one file handler, or a list of such lists."
    )
  }

  # A mapping (one handler) is FULLY named; a sequence (several) is fully
  # unnamed. Anything in between is neither -- a half-named list would otherwise
  # be forwarded whole to DTAFileFactory as if it were one handler, and fail
  # somewhere further in with a message about the wrong thing.
  nms <- names(files)
  fully_named <- !is.null(nms) && all(nzchar(nms))
  fully_unnamed <- is.null(nms) || !any(nzchar(nms))

  if (fully_named) {
    return(list(do.call(DTAFileFactory, files)))
  }

  if (!fully_unnamed) {
    cli_abort(
      "'files' must be either one named file handler or a list of file handlers, not a mix of named and unnamed entries."
    )
  }

  lapply(seq_along(files), function(i) {
    entry <- files[[i]]
    if (!is.list(entry) || is.null(names(entry))) {
      cli_abort(
        "File handler {i} must be a named list with at least a 'type' and a 'filename'."
      )
    }
    do.call(DTAFileFactory, entry)
  })
}


#' @title Create a DTAFile Object
#'
#' @description
#' Constructs a DTAFile object for a specified backend (e.g., SAS or R),
#' based on the provided type and file path.
#'
#' `type` may name any format in either supported list: the readable formats a
#' tabular dataset needs (`csv`, `tsv`) or `any`, the reader-less handler a
#' \code{\link{DTADataSetFile}} uses for a deliverable that is never parsed.
#' Which of those a given dataset will accept is enforced by the dataset itself
#' -- \code{\link{DTADataSetTabular}} requires every handler to be readable --
#' so this factory builds what it is asked for and the dataset has the final
#' say.
#'
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param ... Additional arguments passed to the specific backend constructor.
#'   For `type = "any"` this includes the optional `extensions` restriction; see
#'   \code{\link{DTAFileAny}}.
#'
#' @return An object derived from class \code{DTAFile}, depending on the backend specified.
#'
#' @examples
#' library(DTAtools)
#' DTAFileFactory(type = "csv", filename = "clinical_data.csv")
#'
#' # A deliverable that is never parsed, restricted to two endings.
#' DTAFileFactory(
#'   type = "any",
#'   filename = ".*",
#'   pattern = TRUE,
#'   extensions = c("pdf", "zip")
#' )
#'
#' @seealso \code{\link{DTAFile}}, \code{\link{DTAFileAny}}
#' @export
DTAFileFactory <- function(
  type,
  ...
) {
  if (is.null(type) || type == "") {
    cli_abort("'type' must be a non-empty string.")
  }

  if (!type %in% `__DTAtools_supported_file_types__`) {
    cli_abort(
      "'type' '{type}' must be one of the supported file types: {str_flatten_comma(`__DTAtools_supported_file_types__`)}"
    )
  }

  switch(type,
    csv = DTAFileCSV(
      ...
    ),
    tsv = DTAFileTSV(
      ...
    ),
    any = DTAFileAny(
      ...
    ),
    cli_abort("Filetype '{type}' not implemented.")
  )
}
