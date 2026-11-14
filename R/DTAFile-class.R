#' @title DTAFile Class
#' @description
#' The \code{DTAFile} class is the base of the file-handler hierarchy. It
#' stores the file names (or patterns) a delivery may use, a description of the
#' pattern, the expected (minimum, maximum or exact) number of files, and
#' free-text info. How a file is read -- separator, header presence, quoting,
#' column types -- is held by \code{\link{DTAFileTabular}} and its subclasses,
#' not here.
#'
#' @import S7
#' @importFrom cli cli_abort
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param number_of_files Numeric or \code{NULL}; the exact number of files
#'   expected. Must be a single value -- a length-2 vector is an error, not a
#'   range. Default is \code{1}. To express a range, set
#'   \code{min_number_of_files}/\code{max_number_of_files} instead; supplying
#'   \code{number_of_files} alongside either of them is an error.
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
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
#' @return An object of class \code{DTAFile} containing file parsing
#'   information.
#' @name DTAFile-class
#' @details This class is the base of the file-handler hierarchy and carries
#'   the metadata and parsing properties every handler shares. It is rarely
#'   constructed directly: prefer a concrete subclass --
#'   \code{\link{DTAFileCSV}}, \code{\link{DTAFileTSV}},
#'   \code{\link{DTAFileDelim}} for a delimited file that is read,
#'   \code{\link{DTAFileSAS7BDAT}} for a SAS dataset that is read, or
#'   \code{\link{DTAFileAny}} for a deliverable that is only checked for
#'   arrival.
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
    info = NULL,
    container = NULL
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

    # Every rule on the declared counts lives in dta_file_count_bounds_problem(),
    # which lint_dta() runs on the raw YAML as well, so the lint flags exactly
    # what this constructor refuses.
    problem <- dta_file_count_bounds_problem(
      min_number_of_files, max_number_of_files,
      number = number_of_files, pattern = pattern
    )
    if (!is.null(problem)) {
      cli::cli_abort("{problem}", class = "dta_file_count_bound_invalid")
    }
    if (!is.null(number_of_files)) {
      min_number_of_files <- number_of_files
      max_number_of_files <- number_of_files
    }

    # Checked here, before new_object(): S7 checks a property's TYPE before it
    # runs the validator, so `container = 3`, `TRUE` or a list would otherwise
    # be refused with a plain simpleError and no class. A list is refused, not
    # flattened (the YAML lint refuses `container: [a.zip]` the same way).
    problem <- dta_container_ref_problem(container)
    if (!is.null(problem)) {
      cli::cli_abort(
        "{problem}",
        class = "dta_container_invalid",
        container = container
      )
    }

    new_object(
      S7_object(),
      filename = filename,
      pattern = pattern,
      pattern_description = pattern_description,
      min_number_of_files = min_number_of_files,
      max_number_of_files = max_number_of_files,
      info = info,
      container = container
    )
  },
  properties = list(
    filename = class_character,
    pattern = class_logical,
    pattern_description = class_character_or_null,
    min_number_of_files = class_numeric_or_null,
    max_number_of_files = class_numeric_or_null,
    info = class_character_or_list_or_null,
    container = class_character_or_null
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

    # Bounds are counts of files: NULL (no bound) or one non-negative whole
    # number. NA and Inf are refused -- "no bound" is NULL, and an unbounded
    # maximum is expressed by leaving it unset. Here rather than in the
    # constructor so `h@min_number_of_files <- 5` is checked as well.
    problem <- dta_file_count_bounds_problem(
      self@min_number_of_files, self@max_number_of_files,
      pattern = self@pattern
    )
    if (!is.null(problem)) {
      cli::cli_abort("{problem}", class = "dta_file_count_bound_invalid")
    }

    # The constructor checks the same thing; this is what refuses `h@container
    # <- "x.tar"`, which never passes through the constructor.
    container <- dta_handler_container(self)
    problem <- dta_container_ref_problem(container)
    if (!is.null(problem)) {
      cli::cli_abort(
        "{problem}",
        class = "dta_container_invalid",
        container = container
      )
    }
  }
)


# `inherits = FALSE` scopes this lookup to this package's namespace; without
# it, an attached package exporting a plain function of the same name would
# make this guard skip creating the generic. See `R/00_helpers.R` for the
# full account.
if (!exists("min_number_of_files", mode = "function", inherits = FALSE)) {
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


if (!exists("max_number_of_files", mode = "function", inherits = FALSE)) {
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
#' @param file_name Character. One or more file basenames.
#' @return The names with a trailing compression extension removed, unchanged
#'   where there is none.
#' @details
#' Vectorised, like the `tools::file_*` functions it is built from. It reads as
#' a scalar helper and every caller passes one name, but an `if` on the
#' extension turns a two-element vector into R's own "the condition has
#' length > 1" -- raised from wherever the vector came from, which is never
#' where the reader is looking. `ifelse()` answers per element instead.
#' @keywords internal
dta_strip_compression_extension <- function(file_name) {
  # ifelse() on an empty test returns logical(0), not character(0), and a
  # caller comparing types would be handed the wrong one.
  if (length(file_name) == 0) {
    return(file_name)
  }

  ext <- tolower(tools::file_ext(file_name))
  ifelse(
    ext %in% dta_compression_extensions(),
    tools::file_path_sans_ext(file_name),
    file_name
  )
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
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{file}{A character string naming the file to check against the
#'       handler's stored filename or pattern.}
#'   }
#' @return A logical vector indicating whether the filename matches. With
#'   \code{pattern = FALSE} it is a single value (does the name equal any
#'   declared name); with \code{pattern = TRUE} it holds one value per declared
#'   pattern, so reduce it with \code{any()} before using it in a condition.
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
#' @usage matches_filename(x, ...)
#' @export
if (!exists("matches_filename", mode = "function", inherits = FALSE)) {
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
# With `pattern = TRUE` it returns one logical PER declared pattern -- a
# handler may carry several; with `pattern = FALSE` a single logical. Since it
# can be a vector, no caller may feed the raw result straight into `if`; every
# caller reduces it first, with `isTRUE(any(...))`.
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

# ---- container declarations ---------------------------------------------------
# A handler may say which .zip archive or .xpt SAS transport file its file
# arrives inside (`container`). Only load_delivery() consults it; read_file()
# and open_file() work on the file they are given.

# The regular expression for a container name: `*` is any text, `?` one
# character, every other character is literal, and the whole name must match.
dta_container_regex <- function(glob) {
  esc <- gsub("([.+^$(){}|\\\\\\[\\]])", "\\\\\\1", glob, perl = TRUE)
  paste0("^", gsub("?", ".", gsub("*", ".*", esc, fixed = TRUE), fixed = TRUE), "$")
}

# NULL when `value` is an acceptable container, else the sentence that says
# why not. Shared by the constructor and the validator.
# Why a file entry's count bounds cannot hold, or NULL. Each bound is NULL (no
# bound) or one finite non-negative whole number -- an unbounded maximum is an
# unset one, so NA and Inf are refused -- and the minimum is not above the
# maximum. `number` (number_of_files) is the exact count, set instead of the
# two bounds, and a literal entry (`pattern` FALSE) names one file, so every
# count it declares must be 1. Shared by the DTAFile constructor and validator
# and by lint_dta(), which judges the raw YAML, so they can never disagree.
dta_file_count_bounds_problem <- function(min, max, number = NULL, pattern = TRUE) {
  whole <- function(b) is.null(b) || (is.numeric(b) && length(b) == 1L && is.finite(b) && b >= 0 && b == round(b))
  if (!is.null(number)) {
    if (!is.null(min) || !is.null(max)) {
      return("You must not set both 'number_of_files' and 'min_number_of_files'/'max_number_of_files'. Choose one approach.")
    }
    if (!whole(number)) {
      return("'number_of_files' must be NULL or a single non-negative whole number.")
    }
    min <- number
    max <- number
  }
  if (!isTRUE(pattern)) {
    declared <- c(min, max)
    if (length(declared) > 0 && !isTRUE(all(declared == 1))) {
      return("if pattern is FALSE, then number_of_files must be 1. Then only one file can exist for this filename.")
    }
  }
  if (!whole(min)) {
    return("'min_number_of_files' must be NULL or a single non-negative whole number.")
  }
  if (!whole(max)) {
    return("'max_number_of_files' must be NULL or a single non-negative whole number.")
  }
  if (!is.null(min) && !is.null(max) && min > max) {
    return(sprintf(
      "'min_number_of_files' (%s) exceeds 'max_number_of_files' (%s).",
      format(min, scientific = FALSE), format(max, scientific = FALSE)
    ))
  }
  NULL
}

dta_container_value_problem <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
    return("The container of a file entry must be a single non-empty file name.")
  }
  if (grepl("[[:cntrl:]]", value) || !identical(value, trimws(value))) {
    return(sprintf("The container '%s' must be a file name without control characters or leading or trailing spaces.", value))
  }
  if (grepl("[/\\\\]", value)) {
    return(sprintf("The container '%s' must be a file name, not a path.", value))
  }
  if (is.na(dta_container_kind(value))) {
    return(sprintf("The container '%s' must name a .zip archive or a .xpt SAS transport file.", value))
  }
  NULL
}

# The only reader of the property. A handler restored from an autosave written
# before `container` existed has no such slot, and reads as "no container".
dta_handler_container <- function(h) tryCatch(h@container, error = function(e) NULL)

# ---- container references ------------------------------------------------------
# A file entry's `container` is a REFERENCE, and the three kinds are disjoint by
# syntax, so no lookup order exists to get wrong:
#   "none"        reserved: the entry needs no container, even under a default;
#   has a dot     a container FILE NAME (glob); equal to an explicit container's
#                 `filename` it IS that container, otherwise it declares an
#                 implied container of its own;
#   has no dot    a container ID, which must name an explicit container.

# NULL when `value` is an acceptable reference, else the sentence saying why
# not. Dotted values keep the 0.27.0 judge (and so its classes and messages).
dta_container_ref_problem <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(dta_container_value_problem(value))
  }
  if (identical(value, "none")) {
    return(NULL)
  }
  if (grepl(".", value, fixed = TRUE)) {
    return(dta_container_value_problem(value))
  }
  if (!dta_container_id_ok(value)) {
    return(sprintf(
      "The container of a file entry must be a .zip/.xpt file name or a container id (letters, digits, '_' or '-', starting with a letter); got '%s'.",
      value
    ))
  }
  NULL
}

# The containers a DTA (or an already-extracted list) holds; an object restored
# from before the property existed holds none.
dta_containers <- function(x) {
  if (!inherits(x, "S7_object")) {
    return(if (is.null(x)) list() else x)
  }
  tryCatch(x@containers, error = function(e) list())
}

# "Container info", the one representation every consumer uses:
# list(key, filename, description, default, implied). `key` is the id of an
# explicit container and the file name of an implied one. An implied container
# is a plain list and NEVER a DTAContainer: it has no id, and a file name would
# violate the id rule.
dta_container_info <- function(container) {
  list(
    key = container@id, filename = container@filename,
    description = container@description, default = container@default,
    implied = FALSE
  )
}

# Container info for `ref` against `containers` (a list of DTAContainer), or
# NULL: "none" and an unknown id resolve to nothing, a dotted reference to the
# explicit container with exactly that file name, else to an implied one.
dta_resolve_container <- function(containers, ref) {
  if (is.null(ref) || identical(ref, "none")) {
    return(NULL)
  }
  if (grepl(".", ref, fixed = TRUE)) {
    for (cn in dta_containers(containers)) {
      if (identical(cn@filename, ref)) {
        return(dta_container_info(cn))
      }
    }
    return(list(key = ref, filename = ref, description = NULL, default = FALSE, implied = TRUE))
  }
  cn <- dta_containers(containers)[[ref]]
  if (is.null(cn)) NULL else dta_container_info(cn)
}

# Where entry `h` must arrive: its own reference resolved; no reference means
# the default container, if the document has one; "none" opts out of it.
# `x` is a DTA or its list of containers.
dta_effective_container <- function(x, h) {
  containers <- dta_containers(x)
  ref <- dta_handler_container(h)
  if (is.null(ref)) {
    for (cn in containers) {
      if (isTRUE(cn@default)) {
        return(dta_container_info(cn))
      }
    }
    return(NULL)
  }
  dta_resolve_container(containers, ref)
}

# TRUE when one of `names` (container file names) matches `glob`. Letter case
# must match; a compression suffix is ignored. The glob -> regex step is pure,
# so it is memoised here (dta_container_regex() itself stays unmemoised).
dta_glob_regex_memo <- new.env(parent = emptyenv())
dta_glob_matches <- function(glob, names, ignore.case = FALSE) {
  if (is.null(glob) || is.na(glob) || length(names) == 0) {
    return(FALSE)
  }
  re <- dta_glob_regex_memo[[glob]]
  if (is.null(re)) {
    re <- dta_container_regex(glob)
    assign(glob, re, envir = dta_glob_regex_memo)
  }
  any(grepl(re, unique(c(names, dta_strip_compression_extension(names))), perl = TRUE, ignore.case = ignore.case))
}

# Whether entry `h` can take a candidate of container `kind` ("zip", "xpt" or
# NA for a plain file; vectorised over `kind`): a .zip only as a deliverable
# (DTAFileAny), a .xpt as one SAS transport file (DTAFileXPT) or a deliverable.
dta_entry_takes_kind <- function(h, kind) {
  any_ok <- inherits(h, "DTAtools::DTAFileAny")
  xpt_ok <- any_ok || inherits(h, "DTAtools::DTAFileXPT")
  ok <- rep(TRUE, length(kind))
  ok[!is.na(kind) & kind == "zip"] <- any_ok
  ok[!is.na(kind) & kind == "xpt"] <- xpt_ok
  ok
}

# load_file() reads ONE file. A .zip, or a .xpt the entry does not read as a
# SAS transport file, is a container: say where to take it instead. The message
# carries the base name only (never a server path) and goes to cli as data.
dta_assert_not_container <- function(x, handler, file) {
  kind <- dta_container_kind(file)
  if (!is.na(kind) && !dta_entry_takes_kind(handler, kind)) {
    msg <- sprintf(
      "'%s' is a container (.zip/.xpt); load_file() reads one file. Use load_delivery(x, files, datasets = \"%s\") to load this dataset's files from it.",
      basename(file), x@id
    )
    cli::cli_abort("{msg}", class = "dta_load_file_container")
  }
  invisible(NULL)
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

#' @title The Guard Sequence Shared by `read_file()` and `open_file()`
#' @description
#' Both generics accept the same arguments and must reject the same malformed
#' input the same way before handing off to their respective `*_execution()`
#' method -- factored out here so the two stay in sync rather than drifting,
#' as two independent copies of the same checks eventually do.
#' @param x A `DTAFile` object (or subclass).
#' @param file A character string specifying the path to the file.
#' @param namecheck Logical; when `TRUE` the file name must match the
#'   object's filename or pattern.
#' @param .caller Name of the generic to blame in the single-path error
#'   message (`"read_file"` or `"open_file"`).
#' @return The validated `file` path.
#' @keywords internal
dta_check_readable_file <- function(x, file, namecheck, .caller) {
  file <- dta_assert_single_file_path(file, .caller)

  # any(): matches_filename() yields one logical PER declared name or
  # pattern, so a handler with several of either would otherwise feed a
  # vector into `if` and abort with "the condition has length > 1" instead
  # of the intended "does not match" message.
  if (namecheck && !isTRUE(any(DTAtools::matches_filename(x, basename(file))))) {
    # Interpolated by cli itself, not pre-rendered with str_glue(): glue expands
    # the path FIRST, and cli then reads any brace the path contains as an
    # expression of its own -- so a perfectly ordinary `data{1}.csv` aborted
    # with cli's "unexpected token" instead of this message, and a path
    # containing `{.field x}` would have been rendered as markup. cli escapes
    # braces inside an interpolated value, so `{.path {file}}` is safe for any
    # path. The same applies to the "cannot be found" message below.
    cli::cli_abort(
      "The provided file {.path {file}} does not match the filename or pattern in the DTAFile object."
    )
  }

  if (!file.exists(file)) {
    cli::cli_abort("File {.path {file}} cannot be found.")
  }

  # A missing file is the more fundamental problem, so this check runs after
  # file.exists() above rather than before it. Arrow does not reject a `.zip`
  # outright -- it tries to parse the archive bytes as delimited text and
  # fails with an opaque "Is this a 'csv' file?" schema error, sometimes only
  # after scanning for hours. Catching the extension here trades that for an
  # immediate, actionable message.
  if (tolower(tools::file_ext(file)) == "zip") {
    cli::cli_abort(
      c(
        ".zip archives are not read by a file handler.",
        "i" = "Load the archive as a delivery (drop it in the app, or {.fn load_delivery} in R): each file inside is matched to the specification. A single file can also be read gzip-compressed ({.file .gz}) or uncompressed."
      ),
      class = "dta_zip_not_readable"
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
#' @examples
#' # The reading half of `read_file()`, called on a concrete handler. It
#' # dispatches on `x` alone, so the path travels in `...` and is named.
#' handler <- DTAFileCSV(filename = "clinical_data.csv")
#' path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#'
#' table <- read_file_execution(handler, file = path)
#' dim(table)
#'
#' @name read_file_execution
#' @rdname read_file_execution
#' @export
if (!exists("read_file_execution", mode = "function", inherits = FALSE)) {
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
#'   \code{DTAFile} object or one of its subclasses. A delimited file is parsed
#'   by Arrow; a SAS dataset (\code{\link{DTAFileSAS7BDAT}}) or a SAS transport
#'   file (\code{\link{DTAFileXPT}}) is decoded by \pkg{haven}. A
#'   \code{container} declaration is not consulted here; see
#'   \code{\link{load_delivery}()}.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{file}{A character string giving the path to the file to be read.}
#'     \item{namecheck}{Logical; when \code{TRUE} (the default) the file name
#'       must match the object's filename or pattern.}
#'     \item{specs}{A \code{DTAColumnSpecCollection} declaring the columns, or
#'       \code{NULL} (the default). See the section below for why it matters.}
#'   }
#' @section Why the specs are handed to the reader:
#'   A reader that knows nothing about the specification has to guess a type per
#'   column, and it guesses before any code in this package sees the data: a
#'   column of quoted subject ids -- \code{"007"}, \code{"008"} -- is inferred as
#'   an integer and arrives in R as \code{7} and \code{8}. Handing the specs to
#'   the reader makes it read every column as text, so nothing is guessed: the
#'   declared types are applied afterwards, in R.
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
#' @examples
#' handler <- DTAFileCSV(filename = "clinical_data.csv")
#' path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#'
#' table <- read_file(handler, path)
#' dim(table)
#' names(table)[1:3]
#'
#' # The whole file is in memory now. `open_file()` is the counterpart that
#' # leaves it on disk to be scanned in batches.
#'
#' @importFrom stringr str_glue
#' @importFrom cli cli_abort
#' @name read_file
#' @rdname read_file
#' @usage read_file(x, ...)
#' @export
if (!exists("read_file", mode = "function", inherits = FALSE)) {
  read_file <- new_generic("read_file", "x")
}

method(read_file, DTAFile) <- function(x, file, namecheck = TRUE, specs = NULL) {
  file <- dta_check_readable_file(x, file, namecheck, "read_file")
  read_file_execution(x, file, specs = specs)
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
#' @examples
#' # The opening half of `open_file()`, called on a concrete handler. It
#' # dispatches on `x` alone, so the path travels in `...` and is named.
#' handler <- DTAFileCSV(filename = "clinical_data.csv")
#' path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#'
#' ds <- open_file_execution(handler, file = path)
#' # Nothing has been read yet; the columns come from the header alone.
#' names(ds)
#'
#' @name open_file_execution
#' @rdname open_file_execution
#' @export
if (!exists("open_file_execution", mode = "function", inherits = FALSE)) {
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
#' Only the delimited handlers can be opened lazily. A SAS dataset
#' (\code{\link{DTAFileSAS7BDAT}}) or SAS transport file
#' (\code{\link{DTAFileXPT}}) is decoded whole, so \code{open_file()} on it
#' aborts and points to \code{stream = "never"}. A \code{container} declaration
#' is not consulted here; see \code{\link{load_delivery}()}.
#'
#' @section What a batch of a delimited scan actually is:
#' Arrow splits a delimited file into fixed-size READ BLOCKS -- eight megabytes
#' by default -- and emits one record batch per block. The \code{batch_rows}
#' argument of \code{\link{check}()} reaches
#' \code{arrow::Scanner$create(batch_size = )}, which only ever \emph{slices} a
#' batch that is already larger, so on a delimited file it is a ceiling and not
#' a target: measured on a 4.4 MB, 46,000-row CSV, a 1 MiB block gave 5
#' batches of about 10,485 rows for \code{batch_rows} of 131,072 and of
#' 1,000,000 alike, while the 8 MiB default gave one batch of all 46,000 rows.
#'
#' Peak memory during a scan is therefore governed by the block size times
#' Arrow's read-ahead, not by \code{batch_rows}. Change it with
#' \code{options(DTAtools.stream_block_size = )}, in bytes. Read-ahead is about
#' one block in flight per CPU thread, so the cost of moving it is roughly the
#' difference times the core count -- 48 MB, measured, between a 1 MiB block and
#' the 8 MiB default on eight threads. Lower it on a memory-starved machine;
#' raise it for a file whose rows are much wider than about 420 bytes, where
#' 8 MiB no longer yields a batch large enough for the Arrow numeric parse.
#'
#' A non-UTF-8 \code{encoding} declared on the handler is honoured here too,
#' although Arrow's dataset scanner has no re-encoding step of its own: the
#' file is converted once, streaming and in bounded memory, to a UTF-8 copy
#' under \code{\link[base]{tempdir}()} which the scan then reads. The copy is
#' cached for the session and costs disk rather than memory; see
#' \code{\link{DTAFileTabular}}. The wide encodings (UTF-16, UTF-32, UCS-2,
#' UCS-4) are the exception -- a newline byte is not a character boundary
#' there, so such a file is refused and must be read with
#' \code{stream = "never"}.
#'
#' @param x A \code{DTAFile} object (or subclass) containing file reading
#'   parameters.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{file}{A character string giving the path to the file to be
#'       opened.}
#'     \item{namecheck}{Logical; when \code{TRUE} (the default) the file name
#'       must match the object's filename or pattern.}
#'     \item{specs}{A \code{DTAColumnSpecCollection} declaring the columns, or
#'       \code{NULL} (the default). Passed to the reader so that a column the
#'       specification declares as text is parsed as text rather than inferred
#'       -- see \code{\link{read_file}()} for why that matters.}
#'   }
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
#' @usage open_file(x, ...)
#' @export
if (!exists("open_file", mode = "function", inherits = FALSE)) {
  open_file <- new_generic("open_file", "x")
}

#' @export
method(open_file, DTAFile) <- function(x, file, namecheck = TRUE, specs = NULL) {
  file <- dta_check_readable_file(x, file, namecheck, "open_file")
  open_file_execution(x, file, specs = specs)
}


#' @importFrom cli cli_text cli_div
#' @name print
#' @export
method(print, DTAFile) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAFile}> : {.field {paste(x@filename, collapse = ', ')}}")

  print_info(x)

  invisible(x)
}


#' @title Print a DTAFile's File-Count Bounds
#' @description
#' The number-of-files half of `print_info()`, factored out so `DTAFile` and
#' `DTAFileTabular` -- which cannot reach the parent method through
#' `S7::super()`, as this codebase uses it nowhere (see
#' `dta_matches_filename_base()`) -- apply the identical, NULL-safe rule
#' rather than two copies that can drift.
#'
#' `min_number_of_files` and `max_number_of_files` are usually set together,
#' but a handler may declare only one bound (e.g. `min_number_of_files`
#' alone), leaving the other `NULL`. Comparing a number to `NULL` with `==`
#' yields a zero-length logical, and handing that to `if` aborts with
#' "argument is of length zero" -- this helper renders a `NULL` bound as
#' "unbounded" instead of ever making that comparison unguarded.
#' @param x A `DTAFile` object (or subclass).
#' @return `NULL`, invisibly. Called for its printed side effect.
#' @keywords internal
dta_print_file_count <- function(x) {
  min_n <- x@min_number_of_files
  max_n <- x@max_number_of_files

  if (!is.null(min_n) && !is.null(max_n) && min_n == max_n) {
    cli::cli_alert("Number of files: {min_n}")
    return(invisible(NULL))
  }

  min_label <- if (is.null(min_n)) "unbounded" else min_n
  max_label <- if (is.null(max_n)) "unbounded" else max_n

  cli::cli_alert("Min number of files: {min_label}")
  cli::cli_alert("Max number of files: {max_label}")

  invisible(NULL)
}


#' @title Print Detailed Information About a DTAtools Object
#' @description
#' Prints the fuller, multi-line form of an object, where
#' \code{\link{print}()} gives the summary and
#' \code{\link{print_short_info}()} a single line. Methods exist for
#' \code{\link{DTAFile}} and its subclasses, \code{\link{DTADataSet}},
#' \code{\link{DTAMetaData}} and \code{\link{DTAColumnSpecStructure}}.
#'
#' For a file handler it shows the filename or pattern, then the minimum and
#' maximum number of files expected -- as a single value when both bounds are
#' equal and set, and as "unbounded" for a bound that is unset. A
#' \code{\link{DTAFileAny}} additionally lists the file endings it accepts. For
#' a dataset it shows the template provenance and a summary of the file
#' handlers, and for metadata the agreement's title, version and parties.
#' @importFrom cli cli_alert_info cli_alert
#' @param x A DTAtools object.
#' @param ... Additional arguments (not used).
#' @return Invisibly, \code{x}.
#' @examples
#' # A file handler, and one restricted to particular endings.
#' print_info(DTAFileCSV(filename = "data.csv"))
#' print_info(DTAFileAny(filename = "study_report.pdf", extensions = "pdf"))
#'
#' # A dataset and the agreement metadata.
#' print_info(create_example_DTADataSetTabular(2))
#' print_info(create_example_DTAMetaData())
#' @seealso \code{\link{print}()}, \code{\link{print_short_info}()}
#' @usage print_info(x, ...)
#' @name print_info
#' @export
if (!exists("print_info", mode = "function", inherits = FALSE)) {
  print_info <- new_generic("print_info", "x")
}

# Registered outside the guard above: when `print_info` has already been
# created by an earlier-collated file, the method must still be attached,
# otherwise printing a plain DTAFile has no `print_info` method to dispatch to.
#' @export
method(print_info, DTAFile) <- function(x) {
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  dta_print_file_count(x)
  invisible(x)
}


#' @title Print a Short Summary of a DTAtools Object
#' @description
#' The compact form, used when listing many objects at once, where
#' \code{\link{print}()} gives a summary and \code{\link{print_info}()} the
#' full detail. Methods exist for \code{\link{DTAFile}} and its subclasses,
#' \code{\link{DTADataSet}}, \code{\link{DTADataSetTabular}} and
#' \code{\link{DTAMetaData}}.
#'
#' A file handler prints on one line: the filename or pattern, then the
#' expected file count in parentheses -- a single value when both bounds are
#' equal and set, and an open range such as \code{"(2-)"} when one bound is
#' unset. A \code{\link{DTADataSetTabular}} adds two further lines to the
#' dataset's own, counting its column specs and rules and then its tables.
#' @importFrom cli cli_alert
#' @param x A DTAtools object.
#' @param ... Additional arguments (not used).
#' @return Invisibly, \code{x}.
#' @examples
#' print_short_info(DTAFileCSV(filename = "data.csv"))
#'
#' # An open-ended file count.
#' print_short_info(DTAFileCSV(
#'   filename = "part.*", pattern = TRUE,
#'   min_number_of_files = 2
#' ))
#'
#' print_short_info(create_example_DTADataSetTabular())
#' print_short_info(create_example_DTAMetaData())
#' @seealso \code{\link{print}()}, \code{\link{print_info}()}
#' @usage print_short_info(x, ...)
#' @name print_short_info
#' @export
if (!exists("print_short_info", mode = "function", inherits = FALSE)) {
  print_short_info <- new_generic("print_short_info", "x")
}
method(print_short_info, DTAFile) <- function(x, ...) {
  min_n <- x@min_number_of_files
  max_n <- x@max_number_of_files

  # Comparing a number to NULL with `==` yields a zero-length logical, which
  # `if` refuses outright ("argument is of length zero") -- guard the
  # equality check itself rather than the branch that follows it.
  if (!is.null(min_n) && !is.null(max_n) && min_n == max_n) {
    cli::cli_alert("{x@filename} ({min_n})")
  } else {
    min_label <- if (is.null(min_n)) "" else min_n
    max_label <- if (is.null(max_n)) "" else max_n
    cli::cli_alert("{x@filename} ({min_label}-{max_label})")
  }

  invisible(x)
}
