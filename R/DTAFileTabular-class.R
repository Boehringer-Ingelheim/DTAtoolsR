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
#' @param newlines_in_values Logical; \code{TRUE} if a quoted field may contain
#'   a line break. Default is \code{FALSE}, which is Arrow's own default and
#'   what every file written by a well-behaved exporter satisfies.
#'
#'   The setting exists because a quoted line break is only a problem when it
#'   straddles one of the fixed-size blocks the file is read in (see
#'   \code{DTAtools.stream_block_size}): a small file with quoted newlines reads
#'   correctly either way, and the same data past the first block fails with
#'   "CSV parse error: straddling object straddles two block boundaries". Turning
#'   this on makes the reader hold a block open until the quoted value ends, at
#'   some cost in parse speed -- which is why it is not the default.
#' @param encoding Character; the character encoding of the file, as accepted by
#'   \code{\link[base]{iconv}}. Default is \code{"UTF-8"}.
#'
#'   Anything other than UTF-8 is honoured on the in-memory reader only. Arrow's
#'   dataset scanner has no re-encoding step, so a non-UTF-8 file cannot be
#'   validated lazily and \code{\link{open_file}()} says so rather than reading
#'   the bytes as if they were UTF-8 and disagreeing with the in-memory path.
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
    quote = '"',
    newlines_in_values = FALSE,
    encoding = "UTF-8"
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
      missing_values = missing_values,
      # Both defaults are chosen so that every handler built before these
      # properties existed -- and every YAML that does not mention them --
      # keeps reading its file exactly as it did.
      newlines_in_values = newlines_in_values,
      encoding = encoding
    )
  },
  properties = list(
    sep = class_character,
    has_header = class_logical,
    quote = class_character,
    missing_values = class_character,
    newlines_in_values = class_logical,
    encoding = class_character
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

    # Same shape as the `has_header` check, plus the NA guard: this value ends
    # up in `arrow::csv_parse_options()`, which rejects NA with a message about
    # its own argument rather than about the handler.
    if (
      !is.logical(self@newlines_in_values) ||
        length(self@newlines_in_values) != 1 ||
        is.na(self@newlines_in_values)
    ) {
      problems <- c(
        problems,
        "'newlines_in_values' must be a single non-missing logical value."
      )
    }

    if (
      !is.character(self@encoding) ||
        length(self@encoding) != 1 ||
        is.na(self@encoding) ||
        !nzchar(self@encoding)
    ) {
      problems <- c(problems, "'encoding' must be a single non-empty string.")
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

#' @title Apply the Column-Name Cleaning Rule to a Table
#' @description
#' Renames an already-read table's columns with [dta_clean_column_names()].
#' Neither reader needs this any more -- both are told the cleaned names when
#' the file is opened, by [dta_delim_reader_plan()] -- but it remains the
#' shortest way to state the rule on an object that has already been read, and
#' the tests exercise it directly.
#' @param table_obj Any object with `names()` and `names<-`.
#' @return `table_obj` with cleaned names.
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
#' Strips surrounding quotes and whitespace from header names. Applied once, by
#' [dta_delim_reader_plan()], to the names both readers are then given -- so the
#' identical file cannot present different column names depending on how it
#' happened to be loaded, and cannot match the specs on only one path.
#' @param x Character vector of raw column names.
#' @return The cleaned character vector.
#' @keywords internal
dta_clean_column_names <- function(x) {
  trimws(gsub('^\\s*"|"\\s*$', "", x))
}

#' @title Parse Settings Declared by a File Handler
#' @description
#' Pulls the two real-world parse settings off a `DTAFileTabular` handler, with
#' the defaults that apply when there is no handler at all -- which is the case
#' for the entry points that open a path directly
#' (`dta_open_validation_dataset()`, `cache_as_parquet()`).
#' @param handler A `DTAFileTabular` (or subclass), or `NULL`.
#' @return A list with `newlines_in_values` (single logical) and `encoding`
#'   (single string).
#' @keywords internal
dta_reader_parse_settings <- function(handler = NULL) {
  defaults <- list(newlines_in_values = FALSE, encoding = "UTF-8")

  if (is.null(handler)) {
    return(defaults)
  }

  # tryCatch, not `%||%`: a handler predating these properties -- one restored
  # from a serialised object, say -- has no such property at all, and reaching
  # for it is an error rather than a NULL.
  pick <- function(name, default) {
    value <- tryCatch(S7::prop(handler, name), error = function(e) NULL)
    if (length(value) != 1 || is.na(value)) default else value
  }

  list(
    newlines_in_values = isTRUE(pick("newlines_in_values", FALSE)),
    encoding = as.character(pick("encoding", "UTF-8"))
  )
}

#' @title The Header Names of a Delimited File
#' @description
#' Opens the file for its header alone. An `arrow::Dataset` reads its schema
#' from the first block and nothing else, so the cost is one block of I/O
#' whatever the file weighs: a median of 10 ms on a 169 MB CSV, against 5 ms on
#' a two-row one.
#'
#' Arrow reports a header it cannot use -- two columns with the same name, most
#' commonly -- as a schema failure whose message does not mention the header at
#' all ("Could not read schema ... Is this a 'csv' file?"). It is re-raised here
#' naming the file and the likely cause, because this is the first point at
#' which the file is touched and therefore where the user will be looking.
#' @param path Character path to the delimited file.
#' @param parse_options An `arrow::CsvParseOptions`.
#' @param has_header Logical; whether the first line names the columns.
#' @param encoding The declared file encoding.
#' @return A character vector of raw (uncleaned) column names.
#' @keywords internal
dta_delim_header_names <- function(path, parse_options, has_header, encoding = "UTF-8") {
  read_options <- if (isTRUE(has_header)) {
    arrow::csv_read_options(block_size = dta_stream_block_size())
  } else {
    arrow::csv_read_options(
      block_size = dta_stream_block_size(),
      autogenerate_column_names = TRUE
    )
  }

  dataset <- tryCatch(
    arrow::open_delim_dataset(
      path,
      parse_options = parse_options,
      read_options = read_options
    ),
    error = function(e) {
      reason <- conditionMessage(e)
      cli::cli_abort(c(
        "Cannot read the column names of {.path {path}}.",
        "x" = "{reason}",
        "i" = "Repeated or unparseable names in the first line are the usual cause."
      ))
    }
  )

  raw_names <- names(dataset$schema)

  # The dataset reader has no re-encoding step (see dta_open_normalized_dataset())
  # so a non-ASCII header arrives here as raw bytes mislabelled UTF-8. The
  # in-memory reader DOES re-encode, and would otherwise be handed names that
  # disagree with the ones it read for itself.
  if (!identical(toupper(encoding), "UTF-8")) {
    converted <- tryCatch(
      iconv(raw_names, from = encoding, to = "UTF-8"),
      error = function(e) NULL
    )
    if (!is.null(converted)) {
      raw_names <- ifelse(is.na(converted), raw_names, converted)
    }
  }

  raw_names
}

#' @title One Reader Configuration for Both Paths
#' @description
#' Everything the eager and the lazy reader need to read the same file the same
#' way, derived once. Both are then handed the SAME column names, the same row
#' to start at and the same column types; they differ only in when the bytes are
#' read.
#'
#' @section Why the names are supplied rather than parsed:
#' A header can need cleaning -- `" AGE "`, `"\"AGE\""` -- and the cleaned name
#' is what the specification declares. An `arrow::Dataset` has no `names<-`, so
#' the lazy path could never rename after the fact; and renaming through
#' `dplyr` returns an `arrow_dplyr_query`, which has no `$files`, so
#' [dta_table_change_signal()] could not fingerprint it and `check()` would
#' revalidate the table on every run. Supplying the cleaned names at open time
#' and skipping the header line is the only form that works for both, so both
#' use it -- including for a header that needed no cleaning, so that there is
#' one code path rather than two.
#'
#' The header is read once, by [dta_delim_header_names()]. Before this was
#' factored out, the eager reader read a file with a padded header TWICE: once
#' to learn the names, then again with the declared types pinned.
#'
#' @section Why every column is read as text:
#' When `specs` is supplied, EVERY column is pinned to `utf8` -- not only the
#' declared ones. Three reasons, in order of how badly they bite:
#'
#' 1. **The two paths must agree.** A lazy dataset infers its schema from the
#'    first block and locks it in, so this reader has always pinned everything
#'    to text; the eager reader used to leave an undeclared column to Arrow's
#'    inference. The same undeclared column was then a `double` in memory and a
#'    string when streamed, and a uniqueness rule over it counted `1.5` and
#'    `1.50` as one key on one path and two on the other -- the same file, two
#'    verdicts.
#' 2. **A scan cannot afford a wrong guess.** A column that looks integer-only
#'    in the first block but holds `"0.01"` far down aborts the whole read --
#'    `CSV conversion error to int64: invalid value '0.01'` -- potentially hours
#'    into a multi-hundred-million-row scan. Text never fails that way.
#' 3. **Inference destroys data.** A quoted `"007"` id arrives as the integer 7
#'    before any code in this package sees it.
#'
#' Reading as text loses nothing: [dta_coerce_table_to_specs()] and the rules'
#' own strict numeric conversion do all the real typing in R, and report what
#' they could not convert instead of aborting.
#'
#' When `specs` is `NULL` -- a bare `read_file()` on a handler, with no
#' specification to honour -- inference is left alone, exactly as before.
#'
#' @param path Character path to the delimited file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`.
#' @param delim,quote,has_header Delimited-text parse options.
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @param handler A `DTAFileTabular` (or subclass) whose `newlines_in_values`
#'   and `encoding` apply, or `NULL` for the defaults.
#' @return A list with `path`, `column_names`, `skip`, `col_types`,
#'   `parse_options`, `encoding` and `na`.
#' @keywords internal
dta_delim_reader_plan <- function(
  path,
  specs = NULL,
  delim = ",",
  quote = '"',
  has_header = TRUE,
  na = NULL,
  handler = NULL
) {
  settings <- dta_reader_parse_settings(handler)

  # Spelled out rather than left to arrow's readr-flavoured translation, which
  # is what `delim = `/`quote = ` go through: every value below is the one that
  # translation produces, so the only behaviour that changes is the one setting
  # this package now exposes.
  parse_options <- arrow::csv_parse_options(
    delimiter = delim,
    quoting = nzchar(quote),
    quote_char = quote,
    double_quote = TRUE,
    escaping = FALSE,
    escape_char = "\\",
    newlines_in_values = settings$newlines_in_values,
    ignore_empty_lines = TRUE
  )

  raw_names <- dta_delim_header_names(
    path,
    parse_options = parse_options,
    has_header = has_header,
    encoding = settings$encoding
  )

  # With no header there is nothing to clean: Arrow's generated names (f0,
  # f1, ...) are used as-is.
  cleaned <- if (isTRUE(has_header)) dta_clean_column_names(raw_names) else raw_names

  duplicated_names <- unique(cleaned[duplicated(cleaned)])
  if (length(duplicated_names) > 0) {
    # Reachable only by cleaning: `A` and `" A"` are two names to Arrow and one
    # afterwards. Supplying the collided names to the reader would fail deep
    # inside Arrow with a message that never mentions the trimming.
    cli::cli_abort(c(
      "Cleaning the header of {.path {path}} left repeated column names.",
      "x" = "Repeated after trimming quotes and spaces: {.val {duplicated_names}}.",
      "i" = "Give the columns distinct names in the file."
    ))
  }

  col_types <- if (is.null(specs)) {
    NULL
  } else {
    # Built from fields rather than named arguments: a header may legitimately
    # carry an empty name, and an empty argument name is positional.
    arrow::schema(lapply(cleaned, function(nm) arrow::field(nm, arrow::utf8())))
  }

  list(
    path = path,
    column_names = cleaned,
    # The header line was consumed to learn the names; skipping it is what
    # keeps it from being parsed a second time as a data row.
    skip = as.integer(isTRUE(has_header)),
    col_types = col_types,
    parse_options = parse_options,
    encoding = settings$encoding,
    na = na
  )
}

#' @title Open a Delimited File Lazily, With Clean Column Names
#' @description
#' The lazy half of [dta_delim_reader_plan()]: the plan says how the file is to
#' be read, this opens it as an `arrow::Dataset` so nothing is read until a scan
#' asks for it. Its eager twin is [dta_read_delim_normalized()]; the two are
#' given the identical names, skip and column types, so a file cannot present
#' itself differently depending on which one loaded it.
#'
#' The scan's read block -- and with it the size of a batch, and peak memory --
#' comes from [dta_stream_block_size()]; see there for why `batch_rows` cannot
#' do that job on a delimited file.
#'
#' @param path Character path to the delimited file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. When supplied, every
#'   column is read as text; see [dta_delim_reader_plan()].
#' @param delim,quote,has_header Delimited-text parse options.
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @param handler A `DTAFileTabular` (or subclass) whose `newlines_in_values`
#'   and `encoding` apply, or `NULL` for the defaults.
#' @return An `arrow::Dataset`.
#' @keywords internal
dta_open_normalized_dataset <- function(
  path,
  specs = NULL,
  delim = ",",
  quote = '"',
  has_header = TRUE,
  na = NULL,
  handler = NULL
) {
  settings <- dta_reader_parse_settings(handler)

  # Checked before the plan does any I/O. Arrow re-encodes by wrapping the
  # INPUT STREAM, which only the in-memory reader owns; a dataset opens its own
  # files, so `csv_read_options(encoding = )` is accepted and then silently
  # ignored here -- the bytes come back either as `binary` or, with UTF-8
  # checking off, as a string of undecoded bytes. Either way the same file
  # would validate differently depending on how it was loaded, which is the one
  # outcome this reader exists to prevent. Refusing is the honest answer.
  if (!identical(toupper(settings$encoding), "UTF-8")) {
    encoding <- settings$encoding
    cli::cli_abort(c(
      "A file declaring {.val {encoding}} cannot be validated lazily.",
      "x" = "Arrow's dataset scanner has no re-encoding step, so {.path {path}} would be read as if its bytes were UTF-8.",
      "i" = "Load it with {.code stream = \"never\"}, or convert the file to UTF-8 first."
    ))
  }

  plan <- dta_delim_reader_plan(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header,
    na = na,
    handler = handler
  )

  open_args <- list(
    path,
    parse_options = plan$parse_options,
    read_options = arrow::csv_read_options(
      column_names = plan$column_names,
      skip_rows = plan$skip,
      block_size = dta_stream_block_size()
    ),
    col_types = plan$col_types
  )

  if (!is.null(plan$na)) {
    open_args$na <- plan$na
  }

  do.call(arrow::open_delim_dataset, open_args)
}

#' @title Read a Delimited File Eagerly, With Clean Column Names
#' @description
#' The eager half of [dta_delim_reader_plan()]: same names, same skip, same
#' column types as [dta_open_normalized_dataset()] gives the scanner, read into
#' memory as an Arrow `Table` instead of left on disk.
#'
#' Unlike the lazy opener this one CAN honour a non-UTF-8 `encoding`: it reads
#' through an input stream, which Arrow will re-encode.
#'
#' @param path Character path to the delimited file.
#' @param delim,quote,has_header Delimited-text parse options.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. When supplied, every
#'   column is read as text; see [dta_delim_reader_plan()].
#' @param na Character vector of strings to read as missing, or `NULL`
#'   (the default) to keep Arrow's own default (`c("", "NA")`) unchanged.
#' @param handler A `DTAFileTabular` (or subclass) whose `newlines_in_values`
#'   and `encoding` apply, or `NULL` for the defaults.
#' @return An Arrow `Table`.
#' @keywords internal
dta_read_delim_normalized <- function(
  path,
  delim,
  quote,
  has_header,
  specs = NULL,
  na = NULL,
  handler = NULL
) {
  plan <- dta_delim_reader_plan(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header,
    na = na,
    handler = handler
  )

  read_args <- list(
    path,
    parse_options = plan$parse_options,
    # `read_options` wins over the `col_names`/`skip` arguments, which is why
    # they are not passed at all: two sources for one setting is how the two
    # readers drifted apart in the first place.
    read_options = arrow::csv_read_options(
      column_names = plan$column_names,
      skip_rows = plan$skip,
      encoding = plan$encoding
    ),
    col_types = plan$col_types,
    as_data_frame = FALSE
  )

  if (!is.null(plan$na)) {
    read_args$na <- plan$na
  }

  do.call(arrow::read_delim_arrow, read_args)
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
# `inherits = FALSE` scopes this lookup to this package's namespace; without
# it, an attached package exporting a plain function of the same name would
# make this guard skip creating the generic. See `R/00_helpers.R` for the
# full account.
if (!exists("print_info", mode = "function", inherits = FALSE)) {
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
  cli::cli_alert("Newlines in values: {x@newlines_in_values}")
  cli::cli_alert("Encoding: {x@encoding}")

  invisible(x)
}
