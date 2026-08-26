#' @title DTAFileAny Class Constructor
#'
#' @description
#' Defines the S7 class \code{DTAFileAny}, which extends \code{DTAFile} to
#' describe a deliverable that is **never parsed** -- a PDF, an archive, a
#' report, an audit log, raw instrument output.
#'
#' Its siblings (\code{\link{DTAFileCSV}}, \code{\link{DTAFileTSV}}) all descend
#' from \code{\link{DTAFileTabular}} and exist to *read* a file. This one
#' deliberately does not: it carries no separator, no header flag and no
#' \code{read_file_execution()} method, because a \code{\link{DTADataSetFile}}
#' only ever asks whether the file arrived, is non-empty and can be opened.
#'
#' @section Restricting the file ending:
#' \code{extensions} is an optional allow-list of endings. It is an **open**
#' list rather than a fixed set of known formats, so a delivery of `.xpt`,
#' `.sas7bdat` or `.e2b` files needs no change to this package.
#'
#' \describe{
#'   \item{\code{NULL} (the default)}{Any ending is accepted. This is what
#'     `type: any` on its own means, and it is what makes a file dataset able
#'     to take whatever it is given.}
#'   \item{e.g. \code{c("pdf", "zip")}}{Only those endings are accepted;
#'     \code{\link{matches_filename}()} rejects anything else, so the file is
#'     turned away as it is offered rather than at validation time.}
#' }
#'
#' Entries are normalised on construction -- lower-cased, with any leading dot
#' removed -- so \code{".PDF"} and \code{"pdf"} declare the same restriction and
#' a document cannot depend on how its author happened to write it. A
#' compressed delivery satisfies the ending it carries underneath, so
#' \code{report.pdf.gz} passes \code{extensions = "pdf"}, exactly as
#' \code{data.csv.gz} already matches a handler declared as \code{data.csv}.
#'
#' \code{\link{matches_filename}()} keeps its usual shape here: one logical per
#' declared name or pattern, so a handler carrying several is not collapsed to a
#' single verdict by the extension test.
#'
#' @param filename Character vector of file names or regular expression patterns
#'   to match files.
#' @param pattern Logical; if \code{TRUE}, \code{filename} is treated as a regex
#'   pattern. Default is \code{FALSE}.
#' @param pattern_description Character or \code{NULL}; human-readable
#'   description of the \code{filename} pattern.
#' @param number_of_files Numeric or \code{NULL}; number of files expected.
#'   Default is \code{1}.
#' @param min_number_of_files Numeric or \code{NULL}; minimum number of files
#'   expected.
#' @param max_number_of_files Numeric or \code{NULL}; maximum number of files
#'   expected.
#' @param info Character or \code{NULL}; free-text description of the file.
#' @param extensions Character vector or \code{NULL}; allowed file endings. See
#'   the section above.
#'
#' @name DTAFileAny-class
#' @return An object of class \code{DTAFileAny}.
#'
#' @seealso \code{\link{DTAFile}}, \code{\link{DTADataSetFile}}
#' @include DTAFile-class.R
#' @examples
#' # Any ending at all.
#' DTAFileAny(filename = "study_report.pdf")
#'
#' # A pattern restricted to two endings.
#' handler <- DTAFileAny(
#'   filename = "^report_.*",
#'   pattern = TRUE,
#'   number_of_files = 3,
#'   extensions = c("pdf", "zip")
#' )
#' matches_filename(handler, "report_2024.pdf")
#' matches_filename(handler, "report_2024.csv")
#'
#' # A compressed delivery satisfies the ending underneath it.
#' matches_filename(handler, "report_2024.pdf.gz")
#' @export
DTAFileAny <- S7::new_class(
  "DTAFileAny",
  parent = DTAFile,
  constructor = function(
    filename,
    pattern = FALSE,
    pattern_description = NULL,
    number_of_files = NULL,
    min_number_of_files = NULL,
    max_number_of_files = NULL,
    info = NULL,
    extensions = NULL
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
      extensions = dta_normalise_extensions(extensions)
    )
  },
  properties = list(
    extensions = class_character_or_null
  ),
  validator = function(self) {
    if (!is.null(self@extensions) && !is.character(self@extensions)) {
      cli::cli_abort("'extensions' must be a character vector or NULL.")
    }

    # dta_normalise_extensions() only runs in the constructor, so a direct
    # property assignment (`h@extensions <- ".PDF"`) would otherwise sail past
    # the validator with a value matches_filename() can never match: the
    # comparison in dta_file_extension_allowed() is always against a bare,
    # lower-cased ending. Every entry has to already be in that normalised
    # shape -- non-empty, lower-case, no leading dot.
    if (!is.null(self@extensions)) {
      entries <- self@extensions
      offending <- vapply(
        entries,
        function(entry) {
          is.na(entry) ||
            !nzchar(entry) ||
            entry != tolower(entry) ||
            startsWith(entry, ".")
        },
        logical(1)
      )

      if (any(offending)) {
        bad <- entries[offending]
        # dta_normalise_extensions() can itself collapse to NULL (e.g. every
        # offending entry was blank), so the suggestion needs a fallback --
        # `{.val {NULL}}` in the message below would otherwise render nothing.
        suggestion <- dta_normalise_extensions(bad)
        if (is.null(suggestion)) {
          suggestion <- "a non-empty, lower-case ending with no leading dot"
        }
        # cli::qty() names the count explicitly: `bad` and `suggestion` can
        # differ in length (normalisation may drop an entry), and leaving
        # `{?y/ies}` to infer the count itself errors with "Multiple
        # quantities for pluralization" whenever they do.
        cli::cli_abort(c(
          "!" = "{cli::qty(length(bad))}'extensions' entr{?y/ies} {.val {bad}} must already be normalised (non-empty, lower-case, no leading dot).",
          "i" = "Use {.val {suggestion}} instead."
        ))
      }
    }
  }
)

# Normalise an `extensions` declaration to the one shape matches_filename() can
# compare against.
#
# A YAML sequence parses to a LIST of strings, which the character property
# would reject outright -- the same trap `filename` falls into in DTAFile's
# constructor, and it is flattened here for the same reason.
#
# The comparison is on a bare, lower-cased ending, so ".PDF", "PDF" and "pdf"
# all have to arrive as "pdf". Doing it here rather than at match time means a
# document cannot depend on how its author happened to write the ending, and
# the stored object shows the reader exactly what will be compared.
#
# Everything empty collapses to NULL -- the "no restriction" value -- so a
# blank field from the app's editor unsets the restriction instead of storing
# an empty vector that would match nothing at all.
#
# `no`, `off`, `y`, `n`, `yes`, `on` are ordinary YAML booleans, and a bare
# number is numeric -- neither arrives as a string, so `as.character()` used
# to coerce them silently into the unmatchable "false"/"true"/"2". No real
# extension is legitimately numeric-looking (a genuine one like `e2b` is
# already text), so there is nothing worth coercing: anything that did not
# already arrive as character aborts instead, naming the value and telling
# the author to quote it.
#' @keywords internal
dta_normalise_extensions <- function(extensions) {
  if (is.null(extensions) || length(extensions) == 0) {
    return(NULL)
  }

  if (is.list(extensions)) {
    extensions <- unlist(extensions, use.names = FALSE)
  }

  if (!is.character(extensions)) {
    cli::cli_abort(c(
      "'extensions' must be given as text, not {.cls {class(extensions)[[1]]}} ({.val {extensions}}).",
      i = "YAML parses an unquoted {.val no}/{.val off}/{.val yes} or a bare number as a boolean or numeric value -- quote it instead, e.g. {.code - \"no\"}."
    ))
  }

  extensions <- trimws(extensions[!is.na(extensions)])
  extensions <- tolower(sub("^\\.+", "", extensions))
  extensions <- unique(extensions[nzchar(extensions)])

  if (length(extensions) == 0) {
    return(NULL)
  }

  extensions
}

# Adds the optional file-ending restriction to the name/pattern test every
# DTAFile performs.
#
# Deliberately carries no roxygen block, exactly like method(matches_filename,
# DTAFile): the generic's own block documents `x` and `file` and produces no
# \usage section, and a documented method here would generate one that
# contradicts the generic's (x, ...) signature. What this method does that the
# base does not is described in the DTAFileAny class documentation, where the
# `extensions` property is introduced.
method(matches_filename, DTAFileAny) <- function(x, file) {
  matched <- dta_matches_filename_base(x, file)

  if (is.null(x@extensions)) {
    return(matched)
  }

  # `&` rather than a guard clause that returns a scalar: `matched` is one
  # logical PER declared name/pattern, and recycling a length-1 FALSE across it
  # preserves that shape. Returning a bare FALSE would collapse a multi-name
  # handler to a single verdict.
  matched & dta_file_extension_allowed(file, x@extensions)
}

# Does this file's ending appear in the allow-list?
#
# Tested against the same candidate set the name match uses -- the basename, and
# the basename with a compression suffix removed -- so `report.pdf.gz` satisfies
# `extensions = "pdf"`, consistent with `data.csv.gz` matching a handler
# declared as `data.csv`. Without that, restricting the endings would quietly
# forbid the compressed deliveries the rest of the package goes out of its way
# to accept.
#
# The test is a case-insensitive SUFFIX match (candidate ends with "." + ext),
# not `tools::file_ext() %in% extensions`. `file_ext()` returns only the last
# dot-separated segment, so a multi-part ending like `extensions = "tar.gz"`
# could never match `arch.tar.gz` -- `file_ext()` sees only `"gz"`. A suffix
# test has no such limit.
#' @keywords internal
dta_file_extension_allowed <- function(file, extensions) {
  if (is.null(extensions) || length(extensions) == 0) {
    return(TRUE)
  }

  file_name <- basename(file)
  candidates <- unique(c(file_name, dta_strip_compression_extension(file_name)))
  suffixes <- paste0(".", tolower(extensions))

  any(vapply(
    candidates,
    function(candidate) any(endsWith(tolower(candidate), suffixes)),
    logical(1)
  ))
}

#' @title Print DTAFileAny Object
#' @description
#' Print method for DTAFileAny objects.
#' @param x An object of class DTAFileAny
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_div cli_text
#' @examples
#' library(DTAtools)
#' print(DTAFileAny(filename = "study_report.pdf", extensions = "pdf"))
#'
#' @name print
#' @export
method(print, DTAFileAny) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAFileAny}>")

  print_info(x)

  invisible(x)
}

#' @title Print Information About a DTAFileAny Object
#' @description
#' Print method for detailed information about a \code{DTAFileAny} object,
#' including its filename, pattern, number of files, and the endings it
#' accepts.
#' @param x A \code{DTAFileAny} object whose information is to be printed.
#' @return The input object \code{x}, returned invisibly.
#' @importFrom cli cli_alert_info cli_alert
#' @examples
#' library(DTAtools)
#' print_info(DTAFileAny(filename = "study_report.pdf", extensions = "pdf"))
#'
#' @name print_info
#' @export
method(print_info, DTAFileAny) <- function(x) {
  # Duplicated from method(print_info, DTAFile) rather than calling super():
  # per the TODO on DTAFileTabular's own override, super() does not work here.
  # Duplicating is what lets this method add the "Allowed endings" line no
  # other DTAFile sibling has -- without it, a handler that refuses every
  # `.csv` prints identically to one that accepts anything.
  cli::cli_alert_info("Filename: {x@filename}")
  cli::cli_alert("Pattern: {x@pattern}")
  if (x@min_number_of_files == x@max_number_of_files) {
    cli::cli_alert("Number of files: {x@min_number_of_files}")
  } else {
    cli::cli_alert("Min number of files: {x@min_number_of_files}")
    cli::cli_alert("Max number of files: {x@max_number_of_files}")
  }
  endings <- if (is.null(x@extensions)) "any" else paste(x@extensions, collapse = ", ")
  cli::cli_alert("Allowed endings: {endings}")
  invisible(x)
}
