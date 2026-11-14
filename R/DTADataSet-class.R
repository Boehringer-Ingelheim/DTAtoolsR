#' @title DTADataSet Class
#' @description Class to handle data from files
#' @include importConversion.R
#' @import S7
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom stringr str_flatten_comma
#' @param id Character. Identifier of the dataset: its key in a DTA's
#'   \code{datasets}, in every validation message and in \code{load_file()}.
#' @param type Character. Dataset type: either \code{"tabular"}, whose files
#'   are read and validated against column specifications, or \code{"file"},
#'   whose files are only checked for presence, readability and non-emptiness.
#' @param files a list of DTAFile objects specifying input file information.
#' @param name Character or NULL. Human-readable name of the dataset, e.g.
#'   \code{"Demographics"}; shown in place of the \code{id} where one is set.
#' @param description Character or NULL. Long free-text description of the
#'   dataset.
#' @param template_source Character or NULL. Source of the template used to
#'   generate the dataset specification.
#' @param template_version Character or NULL. Version of the template used.
#' @param template_date Character or NULL. Date of the template used.
#' @return An object of class DTADataSet.
#'
#' @section Provenance:
#' The property \code{provenance} records, for every bound item (a table of a
#' tabular dataset, a path of a file dataset), the delivered file name, the
#' chain of containers it arrived in (outermost first; empty when it arrived
#' loose) and \code{via}: \code{"file"} when \code{load_file()} bound it,
#' \code{"delivery"} when \code{load_delivery()} did. It travels with the
#' dataset. It is maintained by \code{load_file()} and \code{load_delivery()}
#' only: a table assigned to \code{@tables} directly has no record, or, if it
#' replaced a recorded one, keeps that record.
#'
#' @examples
#' ds <- DTADataSet(
#'   id = "example_dataset",
#'   type = "file",
#'   files = list(create_example_DTAFileCSV()),
#'   name = "Example dataset"
#' )
#' ds
#' @export
DTADataSet <- S7::new_class(
  "DTADataSet",
  constructor = function(
    id,
    type,
    files = list(),
    name = NULL,
    description = NULL,
    template_source = NULL,
    template_version = NULL,
    template_date = NULL
  ) {
    if (missing(id)) dta_abort_missing_dataset_id()
    if (inherits(files, "DTAtools::DTAFile")) {
      files <- list(files)
    }

    new_object(
      S7_object(),
      id = id,
      type = type,
      files = files,
      name = name,
      description = description,
      template_source = template_source,
      template_version = template_version,
      template_date = template_date,
      provenance = list()
    )
  },
  properties = list(
    id = class_character,
    type = class_character,
    files = class_list, # list of DTAFile
    name = class_character_or_null,
    template_source = class_character_or_null,
    template_version = class_character_or_null,
    template_date = class_character_or_null,
    description = class_character_or_null,
    # One record per bound item; see the Provenance section above. No
    # validator rule: a record is bookkeeping, never a reason to refuse an edit.
    provenance = S7::new_property(class_list, default = list())
  ),
  validator = function(self) {
    if (!is.character(self@id) || length(self@id) != 1 || is.na(self@id) || self@id == "") {
      cli_abort("Property 'id' must be a single non-empty string.")
    }
    for (p in c("name", "description")) {
      v <- S7::prop(self, p)
      if (!is.null(v) && (length(v) != 1 || is.na(v))) {
        cli_abort("Property {.field {p}} must be a single string or NULL.")
      }
    }
    if (!all(sapply(self@files, inherits, "DTAtools::DTAFile"))) {
      cli_abort("All elements in 'files' must be of class 'DTAFile'")
    }
    if (!self@type %in% `__DTAtools_supported_dataset_types__`) {
      cli_abort(
        "Property 'type' is '{self@type})', must be one of: {str_flatten_comma(`__DTAtools_supported_dataset_types__`)}"
      )
    }
  }
)

# The records of a dataset (`list()` for an object restored from before the
# property existed) and the write-safe way to add or drop one. `key` is the
# table name (tabular) or the bound path (file dataset); `rec = NULL` drops it.
# An object that has no such slot is returned unchanged: it still loads, its
# items then read as "unrecorded".
dta_provenance <- function(ds) {
  tryCatch(ds@provenance, error = function(e) list())
}
dta_set_provenance <- function(ds, key, rec) {
  # An object restored from an older version has no slot: unchanged. Any other
  # error is a real one and propagates.
  if (!"provenance" %in% S7::prop_names(ds)) {
    return(ds)
  }
  prov <- ds@provenance
  prov[[key]] <- rec
  if (length(prov) == 0L) prov <- list()
  ds@provenance <- prov
  ds
}

#' @title Get max number of files
#' @description
#' S7 generic. Returns the maximum number of files expected — either a
#' single \code{DTAFile} handler's own maximum, or the sum across all file
#' handlers in a \code{DTADataSet}.
#' @param x An object of class \code{DTAFile} or \code{DTADataSet}.
#' @param ... Not used by current methods; reserved for future extensions.
#' @return numeric: number of files
#' @name max_number_of_files
#' @export
max_number_of_files <- new_generic("max_number_of_files", "x")

#' @title Get max number of files for a DTADataSet
#' @description
#' Returns the sum of max number of files specified all associated DTAFile
#' objects.
#' @param x An object of class DTADataSet
#' @return numeric: number of files
#' @examples
#' ds <- DTADataSet(
#'   id = "example_dataset",
#'   type = "file",
#'   files = list(create_example_DTAFileCSV())
#' )
#' max_number_of_files(ds)
#' @name max_number_of_files
#' @export
method(max_number_of_files, DTADataSet) <- function(x, ...) {
  bounds <- lapply(x@files, max_number_of_files)
  # A handler without a maximum is unbounded; dropping its NULL from the sum
  # made a dataset that accepts any number of files report a maximum of 0.
  if (any(vapply(bounds, is.null, logical(1)))) {
    return(Inf)
  }
  sum(unlist(bounds))
}


#' @title Get min number of files
#' @description
#' S7 generic. Returns the minimum number of files expected — either a
#' single \code{DTAFile} handler's own minimum, or the sum across all file
#' handlers in a \code{DTADataSet}.
#' @param x An object of class \code{DTAFile} or \code{DTADataSet}.
#' @param ... Not used by current methods; reserved for future extensions.
#' @return numeric: number of files
#' @name min_number_of_files
#' @export
min_number_of_files <- new_generic("min_number_of_files", "x")

#' @title Get min number of files for a DTADataSet
#' @description
#' Returns the sum of min number of files specified all associated DTAFile
#' objects.
#' @param x An object of class DTADataSet
#' @return numeric: number of files
#' @examples
#' ds <- DTADataSet(
#'   id = "example_dataset",
#'   type = "file",
#'   files = list(create_example_DTAFileCSV())
#' )
#' min_number_of_files(ds)
#' @name min_number_of_files
#' @export
method(min_number_of_files, DTADataSet) <- function(x, ...) {
  sum(unlist(sapply(x@files, min_number_of_files)))
}

#' @importFrom cli cli_alert_info cli_alert cli_text
#' @importFrom stringr str_c str_glue
#' @name print
#' @export
method(print, DTADataSet) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTADataSet}> : {.field {x@id}}")

  print_info(x)
  invisible(x)
}

#' @name print_info
#' @usage print_info(x, ...)
#' @export
# `inherits = FALSE` scopes this lookup to this package's namespace; without
# it, an attached package exporting a plain function of the same name would
# make this guard skip creating the generic. See `R/00_helpers.R` for the
# full account.
if (!exists("print_info", mode = "function", inherits = FALSE)) {
  print_info <- new_generic("print_info", "x")
}
method(print_info, DTADataSet) <- function(x) {
  if (!is.null(x@name)) {
    cli_text("- Name: {x@name}")
  }
  if (!is.null(x@description)) {
    cli_text("- Description: {x@description}")
  }

  if (!is.null(x@template_source)) {
    cli_text("- Template source: {.emph {x@template_source}}")
  }
  if (!is.null(x@template_version)) {
    cli_text("- Template version: {.emph {x@template_version}}")
  }
  if (!is.null(x@template_date)) {
    cli_text("- Template date: {.emph {x@template_date}}")
  }
  if (is.null(x@files) || length(x@files) == 0) {
    cli_alert_info("Files: {.emph none}")
  } else {
    min_number_of_files <- min_number_of_files(x)
    max_number_of_files <- max_number_of_files(x)
    entry_label <- if (length(x@files) == 1) "entry" else "entries"
    file_label_min <- if (min_number_of_files == 1) "file" else "files"
    file_label_max <- if (max_number_of_files == 1) "file" else "files"
    if (is.infinite(max_number_of_files)) {
      alert_message <- str_glue(
        "Files: {length(x@files)} {entry_label} with a total of {min_number_of_files} or more files"
      )
    } else if (min_number_of_files == max_number_of_files) {
      alert_message <- str_glue(
        "Files: {length(x@files)} {entry_label} with a total of {min_number_of_files} {file_label_min}"
      )
    } else {
      alert_message <- str_glue(
        "Files: {length(x@files)} {entry_label} with a total of {min_number_of_files} to {max_number_of_files} {file_label_max}"
      )
    }
    cli_alert_info(alert_message)
    for (f in x@files) {
      print_short_info(f)
    }
  }
}


#' @importFrom cli cli_alert_info cli_alert
#' @importFrom stringr str_c str_glue
#' @name print_short_info
#' @export
if (!exists("print_short_info", mode = "function", inherits = FALSE)) {
  print_short_info <- new_generic("print_short_info", "x")
}
method(print_short_info, DTADataSet) <- function(x, ...) {
  min_n <- min_number_of_files(x)
  max_n <- max_number_of_files(x)
  if (max_n == 0) {
    file_info <- "0 files"
  } else if (is.infinite(max_n)) {
    file_info <- str_glue("{min_n} or more files")
  } else if (min_n == max_n) {
    if (min_n == 1) {
      file_info <- "1 file"
    } else {
      file_info <- str_glue("{min_n} files")
    }
  } else {
    file_info <- str_glue("{min_n} to {max_n} files")
  }

  # `nm` and `ty` are INTERPOLATED as variables, never pasted into the
  # markup: cli parses `{...}` in the literal string it is handed, so building
  # the message with str_c()/paste0() first -- as this used to -- let a
  # dataset named e.g. "d{x}" take print_short_info() down with "cannot
  # coerce type 'object' to vector of type 'character'" (cli tried to
  # evaluate the stray `{x}` as the function argument `x` itself). Braces
  # inside an interpolated value are escaped by cli itself.
  nm <- x@id
  ty <- x@type

  if (max_n == 0) {
    cli_alert("Files: none associated, type: {ty}")
  } else {
    cli_alert("Files: {.field {nm}} ({file_info}, {ty})")
  }

  return(invisible(x))
}


#' @title Read DTADataSet from YAML
#' @description
#' Constructs a DTADataSet object from a YAML file specification.
#' @param file Path to the YAML file containing DTADataSet specification
#' @importFrom yaml read_yaml
#' @importFrom cli cli_abort
#' @return An object of class DTADataSet
#' @examples
#' file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#' dta <- read_dataset_from_yaml(file)
#' @export
read_dataset_from_yaml <- function(file) {
  if (!file.exists(file)) {
    cli_abort("YAML file does not exist: {.file {file}}")
  }

  # Lint first, as read_dta_from_yaml() does: every problem at once.
  dta_lint_enforce(lint_dta(file))

  yaml_data <- yaml::read_yaml(file)

  dta_dataset_from_list(yaml_data)
}


#' @title DTADataSet from list
#' @description
#' Constructs a DTADataSet object from a list
#' @param x List
#' @param recursive Unused. It is only set to \code{FALSE} by the function's
#'   own recursive call for a list of datasets and is never read; leave it at
#'   its default.
#' @importFrom cli cli_abort
#' @return A \code{DTADataSet} object, or, when \code{x} is a list of dataset
#'   specifications, a list of \code{DTADataSet} objects.
#' @examples
#' file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#' yaml_dataset <- yaml::read_yaml(file)
#' dataset <- dta_dataset_from_list(yaml_dataset)
#' @export
dta_dataset_from_list <- function(x, recursive = TRUE) {
  # `[[`/names, not `$`: `$` partial-matches, so a list holding only
  # `identifier:` would pass for one holding `id:`.
  is_entry <- function(e) {
    is.list(e) && any(c("id", "name", "type") %in% names(e))
  }
  if (!is_entry(x)) {
    if (length(x) > 0 && is_entry(x[[1]])) {
      # there are multiple datasets which need to be processed separately
      return(lapply(x, dta_dataset_from_list, recursive = FALSE))
    } else {
      cli_abort("List must contain an {.field id} field or be a list of datasets.")
    }
  }

  id <- x[["id"]]
  if (is.null(id)) dta_abort_missing_dataset_id()
  if (is.null(x[["type"]])) {
    cli_abort("Dataset {.val {id}} must contain a {.field type}.")
  }

  do.call(DTADataSetFactory, x)
}


# The constructors' `id` has no default, so a call that omits it would
# otherwise fail with base R's bare "argument "id" is missing".
#' @keywords internal
dta_abort_missing_dataset_id <- function() {
  cli::cli_abort(
    "A dataset needs an {.arg id}, its identifier; {.arg name} is only the human-readable label."
  )
}

# What to call a dataset where a person reads it: its name, else its id.
#' @keywords internal
dta_dataset_label <- function(ds) {
  nm <- ds@name
  if (is.null(nm) || !nzchar(nm)) ds@id else nm
}

# A heading that names the dataset AND keeps its id visible, since the id is
# what every validation message and `{COLUMN_SPECS:<id>}` refers to.
#' @keywords internal
dta_dataset_heading <- function(ds) {
  label <- dta_dataset_label(ds)
  if (identical(label, ds@id)) label else paste0(label, " (", ds@id, ")")
}


#' @title Get files from DTADataSet Object
#' @description
#' Method to get files from DTADataSet object.
#' @param x An object of class DTADataSet.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{name}{Optional single character or single integer. if NULL, returns
#'       a list of all file handlers. If character, returns the file handler
#'       with the specified name. If integer, returns the file handler at the
#'       specified index.}
#'   }
#' @return A list of DTAFile objects, or a single DTAFile object when a name
#'   or index is provided.
#' @examples
#' ds <- create_example_DTADataSetTabular()
#' files(ds)
#' @name files
#' @export
files <- new_generic("files", "x")

#' @export
method(files, DTADataSet) <- function(x, name = NULL) {
  if (
    !is.null(name) &&
      (!(is.character(name) || is.numeric(name)) ||
        (length(name) != 1 && !is.character(name)))
  ) {
    cli_abort(
      "'name' must be a single character vector, single numeric index or NULL."
    )
  }
  all_files <- x@files

  if (is.null(name)) {
    return(all_files)
  }

  if (is.numeric(name)) {
    if (any(name < 1) || any(name > length(all_files))) {
      cli_abort("Numeric 'name' index out of bounds.")
    }
    return(all_files[[name]])
  }

  missing <- setdiff(name, names(all_files))
  if (length(missing) > 0) {
    cli_abort("The following file{?s} not found: {.field {missing}}")
  }

  # Several names, all present: `[[` would index recursively into the first.
  if (length(name) != 1) {
    cli_abort(
      "'name' must be a single character vector, single numeric index or NULL."
    )
  }

  return(all_files[[name]])
}


#' @title Get tables from DTADataSet Object
#' @description
#' Method to get tables from DTADataSet object.
#' @param x An object of class DTADataSet.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{i}{index: optional single character or integer or vector of characters
#'       or integers to select specific tables. if NULL (default), returns all tables.}
#'   }
#' @return A list of tables, or a single table when one index/name is provided.
#' @examples
#' ds <- create_example_DTADataSetTabular()
#' tables(ds)
#' @name tables
#' @export
tables <- new_generic("tables", "x")

#' @export
method(tables, DTADataSet) <- function(x, i = NULL) {
  if (is.null(i)) {
    return(x@tables)
  } else if (length(i) == 1) {
    return(x@tables[[i]])
  } else {
    return(x@tables[i])
  }
}


if (!exists("load_file", mode = "function", inherits = FALSE)) {
  load_file <- new_generic("load_file", "x")
}

#' @title Load file into DTADataSet
#' @description
#' The \code{load_file()} method for the abstract \code{DTADataSet} base class.
#' It is not implemented: it always aborts, directing the caller to
#' \code{DTADataSetTabular} or \code{DTADataSetFile}, which have their own
#' methods.
#' @param x An object of class \code{DTADataSet}.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{handler_index}{Single character or numeric index selecting the
#'       file handler within the dataset. Defaults to \code{1}.}
#'     \item{file}{Path to the input file to be read.}
#'     \item{name}{Optional name under which the loaded table should be stored.
#'       Defaults to \code{tools::file_path_sans_ext(basename(file))}.}
#'   }
#' @return Never returns; always signals an error on this base class.
#' @usage load_file(x, ...)
#' @name load_file
#' @export
method(load_file, DTADataSet) <- function(
  x,
  handler_index = 1,
  file,
  name = tools::file_path_sans_ext(basename(file)),
  ...
) {
  cli::cli_abort(c(
    "{.fn load_file} is not implemented for {.cls {class(x)[[1]]}}.",
    i = "Use a {.cls DTADataSetTabular} or {.cls DTADataSetFile}, or add a method for this subclass."
  ))
}


# An in-memory digest of an arbitrary R object.
#
# rlang::hash() serialises in memory. The implementation it replaces wrote the
# object to a temporary .rds -- gzip-compressed, because that is saveRDS()'s
# default -- and then md5sum()'d the file, so identifying one in-memory table
# cost a full compression pass plus two trips through the filesystem. On a
# large table that is more expensive than validating it, which is the opposite
# of what a skip-if-unchanged signal is for.
#
# The digest is a signal, never a persisted contract: an index entry carrying a
# hash from an older session simply fails to match and the table revalidates.
#' @keywords internal
dta_hash_object <- function(x) {
  rlang::hash(x)
}

# Resolves a `tables` selection against a dataset's table names.
#
# A dataset with no tables is not an error. It is the ordinary state of a
# specification whose data has not been delivered yet, and reporting on it --
# validation_status(), results(), messages(), check() -- must say "nothing
# loaded", not abort. Aborting here made a single undelivered dataset take the
# whole DTA's report down with it.
#
# An EXPLICIT selection still aborts: asking for table "x" of a dataset that
# holds none is a mistake about the dataset, and answering it with an empty
# selection would silently drop the caller's request.
#' @keywords internal
dta_table_id_to_names <- function(x, tables = NULL) {
  all_names <- names(x@tables)

  if (is.null(all_names)) {
    all_names <- character(0)
  }

  # "No names" and "no tables" are not the same thing, and only the second is
  # benign. A dataset holding tables that cannot be addressed is a broken
  # document, and answering it with an empty selection would quietly leave its
  # data out of every report -- so this case keeps failing loudly, as the whole
  # function used to.
  if (length(all_names) == 0 && length(x@tables) > 0) {
    cli::cli_abort(c(
      "The tables of dataset {.field {x@id}} have no names.",
      i = "Every table is addressed by name; supply a named list or use {.fn load_file}."
    ))
  }

  if (is.null(tables)) {
    return(all_names)
  }

  if (is.numeric(tables)) {
    if (any(tables < 1) || any(tables > length(all_names))) {
      cli::cli_abort("Table index out of bounds.")
    }
    return(all_names[tables])
  }

  if (is.character(tables)) {
    missing <- setdiff(tables, all_names)
    if (length(missing) > 0) {
      cli::cli_abort("Table{?s} not found: {.field {missing}}")
    }
    return(tables)
  }

  cli::cli_abort("'tables' must be NULL, numeric, or character.")
}

#' @keywords internal
dta_default_validation_artifact_dir <- function(x) {
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", x@id)
  file.path(tempdir(), "DTAtools_validation", safe_name)
}

#' @keywords internal
dta_new_validation_run_id <- function() {
  format(Sys.time(), "%Y%m%dT%H%M%OS3")
}

#' @keywords internal
dta_validation_result_to_row <- function(table_name, status, index_entry, target_type = "table",
                                         ok = NULL) {
  # `ok` overrides the entry's own verdict, and exists for exactly one caller:
  # the "unspecified" status, whose verdict is NA rather than TRUE or FALSE.
  # NA is load-bearing -- it makes both n_valid (sum(ok == TRUE)) and n_invalid
  # (sum(ok == FALSE)) skip the row, so a dataset with no column specification
  # reports as incomplete rather than as either a pass or a data failure. The
  # isTRUE() below would silently flatten that NA to FALSE, which is why the
  # override is a parameter here rather than a second copy of this data.frame:
  # the COLUMN SET stays defined in one place.
  data.frame(
    table = table_name,
    target_type = target_type,
    status = status,
    ok = if (is.null(ok)) isTRUE(index_entry$ok) else ok,
    validated_at = as.character(index_entry$validated_at),
    run_id = index_entry$run_id,
    validation_run = if (!is.null(index_entry$validation_run)) index_entry$validation_run else index_entry$run_id,
    # All three counts are narrowed via dta_narrow_count(), never as.integer()'d,
    # so a count past .Machine$integer.max stays a double instead of becoming NA
    # -- an as.integer()'d NA is exactly what the DTA-level sum(..., na.rm = TRUE)
    # rollup silently drops, under-reporting the affected axis as 0.
    n_columnspec_errors = dta_narrow_count(index_entry$n_columnspec_errors),
    n_rule_errors = dta_narrow_count(index_entry$n_rule_errors),
    # An index entry recorded before the import axis existed knows nothing
    # about it. NA ("unknown") is the honest value; 0 would claim a clean
    # import axis that was never checked.
    n_import_errors = if (is.null(index_entry$n_import_errors)) {
      NA_integer_
    } else {
      dta_narrow_count(index_entry$n_import_errors)
    },
    stringsAsFactors = FALSE
  )
}


#' @title Check DTADataSet
#' @description
#' Validates a \code{DTADataSet} object's properties and structure.
#' This is the base method; subclasses like \code{DTADataSetTabular}
#' override it to add table-specific validation.
#' @param x A \code{DTADataSet} object.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{force}{Logical. If \code{FALSE}, validation for unchanged data is skipped.}
#'     \item{persist}{Logical. If \code{TRUE}, validation artifacts are persisted.}
#'     \item{artifact_dir}{Character or NULL. Optional output directory for artifacts.}
#'     \item{quiet}{Logical. If TRUE, suppresses console output. Default is FALSE.}
#'   }
#' @return Invisibly returns \code{x}.
#' @usage check(x, ...)
#' @name check
#' @export
S7::method(check, DTADataSet) <- function(
  x,
  force = FALSE,
  persist = TRUE,
  artifact_dir = NULL,
  quiet = FALSE,
  validation_run = NULL
) {
  # Base class method: just validate that required properties exist.
  # Subclasses (e.g., DTADataSetTabular) override this to add table validation.

  has_specs <- !is.null(tryCatch(x@specs, error = function(e) NULL))
  has_tables <- !is.null(tryCatch(x@tables, error = function(e) NULL))
  has_validation_index <- !is.null(tryCatch(x@validation_index, error = function(e) NULL))
  has_validation_store <- !is.null(tryCatch(x@validation_store, error = function(e) NULL))

  if (!has_specs || !has_tables || !has_validation_index || !has_validation_store) {
    cli::cli_abort(
      "check() requires a DTADataSet subclass with properties: specs, tables, validation_index, and validation_store."
    )
  }

  # Base class just validates structure; subclasses do table-specific work.
  invisible(x)
}
