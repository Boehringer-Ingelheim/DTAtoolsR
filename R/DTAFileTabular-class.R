#' @title DTAFileTabular Class Constructor
#'
#' @description
#' Defines the S7 class \code{C}, which extends \code{DTAFile}
#' to represent metadata and configuration for Delim (Tab-Separated Values)
#'  data files.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
#' @param number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected. Default is \code{1}.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#' @param missing_values Character. String representing missing values in the
#'   file. Default is \code{""}.
#' @param sep Character. Field separator used in the TSV file.
#'  Defaults to tab ("\\t").
#' @param has_header Logical; \code{TRUE} if the first row is a header. Default
#'   is \code{TRUE}.
#' @param quote Character or \code{NULL}; quoting character for fields. Default
#'   is \code{'"'}.
#'
#' @name DTAFileTabular-class
#' @return An object of class \code{DTAFileTabular}.
#'
#' @seealso \code{\link{DTAFile}}
#'
#' @export
DTAFileTabular <- S7::new_class(
  "DTAFileTabular",
  parent = DTAFile,
  constructor = function(
    filename,
    pattern = FALSE,
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    missing_values = "",
    sep = "\t",
    has_header = TRUE,
    quote = '"'
  ) {
    new_object(
      .parent = DTAFile(
        filename = filename,
        number_of_files = number_of_files,
        min_number_of_files = min_number_of_files,
        max_number_of_files = max_number_of_files,
        info = info,
        pattern = pattern,
        pattern_description = pattern_description
      ),
      sep = sep,
      has_header = has_header,
      quote = quote,
      missing_values = missing_values
    )
  },
  properties = list(
    sep = class_character,
    has_header = class_logical,
    quote = class_character,
    missing_values = class_character
  ),
  validator = function(self) {
    # Collect every violation: returning the messages from separate `if`
    # blocks would silently discard all but the last one.
    problems <- character()

    # `nchar(NA_character_)` is `NA`, not an error -- but `NA != 1` is also
    # `NA`, and handing that to the `||` chain's `if` aborts with the opaque
    # "missing value where TRUE/FALSE needed" instead of this check's own
    # message. `is.na()` is checked first (after the length-1 guard makes it
    # safe to call), so `nchar()` never runs on a value it cannot answer for.
    if (
      !is.character(self@sep) ||
        length(self@sep) != 1 ||
        is.na(self@sep) ||
        nchar(self@sep) != 1
    ) {
      problems <- c(problems, "'sep' must be a single character.")
    }

    if (!is.logical(self@has_header) || length(self@has_header) != 1) {
      problems <- c(problems, "'has_header' must be a single logical value.")
    }

    if (
      !is.character(self@quote) ||
        length(self@quote) != 1 ||
        is.na(self@quote) ||
        nchar(self@quote) != 1
    ) {
      problems <- c(problems, "'quote' must be a single character.")
    }

    if (length(problems) == 0) {
      NULL
    } else {
      problems
    }
  }
)


#' @title Read File for DTAFileTabular Objects
#' @name read_file_execution
#' @description
#' \code{DTAFileTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileTSV},
#' \code{DTAFileCSV} or \code{DTAFileDelim}.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileTabular} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be read.
#' @return A tibble containing the contents of the file if the filename
#' matches; otherwise, returns \code{NULL}.
#' @usage read_file_execution(x, ...)
method(read_file_execution, DTAFileTabular) <- function(x, ...) {
  cli::cli_abort(
    "This method is not implemented. You need to
  use an object of a class which is derived from DTAFileTabular class."
  )
}

#' @title Open File Lazily for DTAFileTabular Objects
#' @name open_file_execution
#' @description
#' \code{DTAFileTabular} is a virtual class. This method needs to be
#' implemented in derived classes like \code{DTAFileTSV},
#' \code{DTAFileCSV} or \code{DTAFileDelim}, each of which knows its own
#' delimiter.
#' @importFrom cli cli_abort
#' @param x A \code{DTAFileTabular} object containing file reading parameters.
#' @param ... A single `file` argument: character string specifying the path
#'   to the file to be opened.
#' @return An \code{arrow::Dataset}.
#' @usage open_file_execution(x, ...)
method(open_file_execution, DTAFileTabular) <- function(x, ...) {
  cli::cli_abort(
    "This method is not implemented. You need to
  use an object of a class which is derived from DTAFileTabular class."
  )
}

#' @title Reader Arguments Passed Through `...`
#' @description
#' `read_file_execution()` and `open_file_execution()` dispatch on `x` alone and
#' take everything else through `...`, so each concrete method has to pull its
#' arguments back out. Doing that as `list(...)[[1]]` makes the file the *first*
#' argument by position, which silently picks up `specs` if a caller ever names
#' its arguments in the other order. Removing the named entries first leaves the
#' file as the only positional one, so all six methods agree on what they were
#' given however the call was written.
#' @param ... The arguments the calling method was called with.
#' @param .caller Name of the generic to blame in the error message. Both
#'   `read_file_execution()` and `open_file_execution()` are exported, so a
#'   caller can reach this failure through either -- and naming the wrong one
#'   sends them looking in the wrong place.
#' @return A list with `file` (the path) and `specs` (a
#'   `DTAColumnSpecCollection`, or `NULL` when the caller supplied none).
#' @keywords internal
dta_reader_args <- function(..., .caller = "read_file_execution") {
  args <- list(...)
  nms <- names(args)

  if (is.null(nms)) {
    nms <- rep("", length(args))
  }

  # Exact matching: `[[` with a character index partial-matches only when asked
  # to, unlike `$`, which would let a stray `spec = ` argument bind to `specs`.
  pick <- function(name) {
    index <- which(nms == name)
    if (length(index) == 0) NULL else args[[index[[1]]]]
  }

  specs <- pick("specs")
  file <- pick("file")

  if (is.null(file)) {
    positional <- args[!nzchar(nms)]

    if (length(positional) == 0) {
      # `{(.caller)}`, not `{.caller}`: cli >= 3.4 reads a `{}` expression
      # starting with a dot as a style name, not a variable. The parentheses
      # say this is the variable. The dot on the parameter itself stays, so it
      # cannot collide with a `...` argument of the same name.
      cli::cli_abort("{.fn {(.caller)}} requires a {.arg file} argument.")
    }

    file <- positional[[1]]
  }

  list(file = file, specs = specs)
}

#' @keywords internal
dta_normalize_column_names <- function(table_obj) {
  current_names <- names(table_obj)
  cleaned_names <- dta_clean_column_names(current_names)

  if (!identical(current_names, cleaned_names)) {
    names(table_obj) <- cleaned_names
  }

  table_obj
}

#' @title The Column-Name Cleaning Rule
#' @description
#' Strips surrounding quotes and whitespace from header names. Factored out of
#' `dta_normalize_column_names()` so the eager and lazy readers apply the *same*
#' rule -- they cannot apply it the same way, but they must reach the same
#' answer, or the identical file would present different column names depending
#' on how it happened to be loaded and would match the specs on only one path.
#' @param x Character vector of raw column names.
#' @return The cleaned character vector.
#' @keywords internal
dta_clean_column_names <- function(x) {
  trimws(gsub('^\\s*"|"\\s*$', "", x))
}

#' @title Open a Delimited File Lazily, With Clean Column Names
#' @description
#' The lazy counterpart of `dta_read_delim_normalized()`: both pin every
#' column to `utf8` and apply the same column-name cleaning, differing only in
#' when the file is read. An Arrow `Dataset` has no `names<-` method, though,
#' so here the cleaned names have to be supplied when the dataset is *opened*
#' rather than assigned afterwards -- which is why this cannot simply share
#' the eager function's code.
#'
#' Every column -- not only the ones a specification declares -- is pinned to
#' `utf8`. A declared column is pinned so the specification, not Arrow's own
#' inference, decides its type. An undeclared column is pinned for a different
#' reason: a lazy dataset's schema is inferred from only its first block and
#' then locked in for the whole scan, so a column that looks integer-only
#' early on but holds `"0.01"` far down aborts the entire read -- `CSV
#' conversion error to int64: invalid value '0.01'` -- potentially hours into
#' a multi-hundred-million-row scan. Reading as text can never fail that way.
#' The per-batch coercion ([dta_coerce_table_to_specs()]) and the rules' own
#' strict numeric conversion do all the real typing in R, so nothing
#' downstream changes for data that was already well-typed.
#'
#' The file is opened twice, unconditionally: the first open exists only to
#' learn the column names from the header; the second pins every column to
#' `utf8`, whether or not the header needed cleaning. That is cheap --
#' opening a dataset reads the header, not the data -- and it buys the thing
#' the alternative loses: renaming through `dplyr` would return an
#' `arrow_dplyr_query`, which has no `$files`, so
#' `dta_table_change_signal()` could not fingerprint it and `check()` would
#' revalidate the table on every run. Re-opening keeps a true `Dataset`.
#'
#' @param path Character path to the delimited file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. Only consulted for the
#'   first, header-only open -- the final schema pins every column to `utf8`
#'   regardless of what `specs` declares; see Description.
#' @param delim,quote,has_header Delimited-text parse options.
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @return An `arrow::Dataset`.
#' @keywords internal
dta_open_normalized_dataset <- function(
  path,
  specs = NULL,
  delim = ",",
  quote = '"',
  has_header = TRUE,
  na = NULL
) {
  # First open reads only the header, to learn the raw column names -- opening
  # a dataset never scans the data, so discarding this one below is cheap.
  dataset <- dta_open_delimited_dataset(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header
  )

  raw_names <- names(dataset)
  # With no header there is nothing to clean: Arrow's generated names (f0,
  # f1, ...) are used as-is, exactly as before this rework.
  cleaned <- if (isTRUE(has_header)) dta_clean_column_names(raw_names) else raw_names

  full_types <- do.call(
    arrow::schema,
    setNames(rep(list(arrow::utf8()), length(cleaned)), cleaned)
  )

  # `col_names = TRUE` lets Arrow keep reading the file's own header line, and
  # that only works because `full_types` is keyed by `cleaned`, which equals
  # `raw_names` whenever nothing needed cleaning (including the no-header
  # case, where `cleaned` was never touched). Once cleaning changed something,
  # the header text no longer matches the schema's keys, so the clean names
  # must be supplied explicitly and the header row skipped instead of parsed a
  # second time as a data row.
  reopen_args <- list(
    path,
    delim = delim,
    quote = quote,
    col_names = if (identical(raw_names, cleaned) && has_header) TRUE else cleaned,
    skip = if (!identical(raw_names, cleaned) && has_header) 1 else 0,
    col_types = full_types
  )

  if (!is.null(na)) {
    reopen_args$na <- na
  }

  do.call(arrow::open_delim_dataset, reopen_args)
}

#' @title Read a Delimited File Eagerly, With Clean Column Names
#' @description
#' The eager counterpart of `dta_open_normalized_dataset()`: reads the whole
#' file into memory rather than opening it as a lazy `Dataset`, sharing the
#' same declared-type pinning and column-name cleaning so the two paths type a
#' column identically -- the "same column-type pinning" contract documented on
#' `open_file()`.
#'
#' Unlike the lazy opener, only the columns a specification declares are
#' pinned to `utf8` ([dta_reader_col_types()]); an undeclared column is left to
#' Arrow's inference, as it always was for this reader. The lazy path pins
#' every column because a dataset scan can run for hours over hundreds of
#' millions of rows and cannot afford Arrow locking in a wrong type partway
#' through; an eager read is bounded by what fits in memory, so that risk does
#' not arise here in the same way.
#'
#' @param path Character path to the delimited file.
#' @param delim,quote,has_header Delimited-text parse options.
#' @param specs A `DTAColumnSpecCollection` or `NULL`, used to pin declared
#'   column types at parse time.
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @return An Arrow `Table`.
#' @keywords internal
dta_read_delim_normalized <- function(
  path,
  delim,
  quote,
  has_header,
  specs = NULL,
  na = NULL
) {
  read_args <- list(
    path,
    delim = delim,
    quote = quote,
    # col_names = FALSE makes arrow generate names and keep the first row as
    # data; skipping a row would instead discard the first data row.
    col_names = has_header,
    # NULL, the reader's own default, means "infer every column".
    col_types = dta_reader_col_types(specs, has_header),
    as_data_frame = FALSE
  )

  if (!is.null(na)) {
    read_args$na <- na
  }

  table_obj <- do.call(arrow::read_delim_arrow, read_args)
  raw_names <- names(table_obj)
  table_obj <- dta_normalize_column_names(table_obj)

  # `col_types` above is keyed by the *clean* spec ids, but this first read
  # matched it against the still-quoted, padded header -- so it pinned
  # nothing, and a declared column was silently inferred, exactly the failure
  # this whole scheme exists to prevent (a quoted "007" id already arrived as
  # the integer 7 by the time dta_normalize_column_names() ran, above).
  # Re-reading with the header skipped and the clean names supplied explicitly
  # is the only way to get the declared types matched against a header that
  # needed cleaning. That costs a second pass, but only for files in that
  # position -- and it is what makes this eager reader honour the same
  # declared-type pinning as the lazy one.
  needs_reread <- isTRUE(has_header) &&
    !identical(raw_names, names(table_obj)) &&
    !is.null(specs)

  if (!needs_reread) {
    return(table_obj)
  }

  cleaned_names <- names(table_obj)

  reread_args <- list(
    path,
    delim = delim,
    quote = quote,
    col_names = cleaned_names,
    skip = 1,
    col_types = dta_reader_col_types(specs, has_header),
    as_data_frame = FALSE
  )

  if (!is.null(na)) {
    reread_args$na <- na
  }

  # Normalizing again is a no-op -- `cleaned_names` is already clean -- but
  # applying it keeps this branch returning through the same shape as the
  # first, rather than a table that skipped a step the other one did not.
  dta_normalize_column_names(do.call(arrow::read_delim_arrow, reread_args))
}

#' @title Missing-Value Markers for a Tabular Handler
#' @description
#' Maps a `DTAFileTabular` handler's declared `missing_values` onto the `na`
#' argument the readers expect. An empty cell is always treated as missing
#' regardless of what a handler declares; anything in `missing_values` adds to
#' that rather than replacing it.
#' @param x A `DTAFileTabular` object (or subclass).
#' @return A character vector to pass as `na`, or `NULL` when the handler
#'   declares no missing-value markers -- `NULL` lets the reader keep Arrow's
#'   own default (`c("", "NA")`) rather than narrowing it.
#' @keywords internal
dta_reader_na_values <- function(x) {
  declared <- x@missing_values

  # The property DEFAULTS to "" on every handler, so a bare "" (or NA) is
  # "nothing declared", not a declaration that only the empty cell is missing.
  # Without this filter every existing handler would silently narrow Arrow's
  # default missing set (`""` and `"NA"`) down to just `""`, turning literal
  # "NA" text into values -- a global behaviour change no one asked for.
  declared <- declared[!is.na(declared) & nzchar(declared)]

  if (length(declared) == 0) {
    return(NULL)
  }

  unique(c("", declared))
}


#' Print Information About a DTAFile Object
#'
#' This method prints detailed information about a \code{DTAFile} object, including its filename, pattern, and the number of files associated with it. The information is displayed using the \code{cli} package for formatted output.
#'
#' @importFrom cli cli_alert_info cli_alert
#'
#' @param x A \code{DTAFile} object whose information is to be printed.
#'
#' @return The input object \code{x}, returned invisibly.
#'
#' @details
#' The function displays the filename and pattern of the \code{DTAFile} object. It also prints the minimum and maximum number of files, or a single value if both are equal and set; an unset bound prints as "unbounded".
#'
#' @examples
#' dta_file <- DTAFileCSV(filename = "data.csv")
#' print_info(dta_file)
#'
#' @name print_info
#' @seealso \code{\link{DTAFile}}
#' @export
if (!exists("print_info", mode = "function")) {
  print_info <- new_generic("print_info", "x")
}
method(print_info, DTAFileTabular) <- function(x) {
  # S7::super() is unused throughout this codebase (see
  # dta_matches_filename_base()), so the parent's filename/pattern lines are
  # repeated here rather than reached through it. The number-of-files logic
  # is NOT repeated, though -- that one is buggy enough (NULL-unsafe) to be
  # worth sharing instead of copying; see dta_print_file_count().
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  dta_print_file_count(x)
  cli::cli_alert("Separator: {x@sep}")
  cli::cli_alert("Has header: {x@has_header}")
  cli::cli_alert("Quote: {x@quote}")

  invisible(x)
}
