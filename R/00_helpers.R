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

`__DTAtools_supported_backends__` <- c("SAS")
`__DTAtools_supported_dataset_types__` <- c("tabular", "file")
`__DTAtools_stream_modes__` <- c("auto", "always", "never")

# The size above which `stream = "auto"` keeps a file lazy rather than reading
# it into memory. 512 MB is chosen to sit well below the point where an R
# session on a typical analyst machine starts to struggle: an Arrow table is
# several times its on-disk size once strings are materialised, so a 512 MB CSV
# is already a multi-gigabyte object.
`__DTAtools_stream_threshold_default__` <- 512 * 1024^2

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
#' Note that the size compared is the size *on disk*. For a compressed file --
#' `.csv.gz` -- that is the compressed size, which can be several times smaller
#' than what materialising it would cost, so `"auto"` under-triggers there. Pass
#' `stream = "always"` for a large compressed input.
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

  size > threshold
}
`__DTAtools_supported_file_types__` <- c("csv", "tsv") # TODO: "sas7bdat", ..

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
#' @param type Character. The type specification, potentially prefixed with a backend identifier.
#' @param ... Additional arguments passed to the specific backend constructor.
#'
#' @return An object derived from class \code{DTAFile}, depending on the backend specified.
#'
#' @examples
#' library(DTAtools)
#' DTAFileFactory(type = "csv", filename = "clinical_data.csv")
#'
#' @seealso \code{\link{DTAFile}}
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
    cli_abort("Filetype '{type}' not implemented.")
  )
}
