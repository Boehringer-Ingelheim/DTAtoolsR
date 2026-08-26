#' @title DTADataSetFile Class
#' @description Handles file-backed datasets that only need to verify that one
#'   or more referenced files exist, are readable, and are not empty.
#' @import S7
#' @importFrom cli cli_abort
#' @include validationReporting.R
#' @include DTAFileAny-class.R
#' @param name Character. Name of the container.
#' @param paths Character vector of file paths to validate.
#' @param files A list of DTAFile objects specifying input file information.
#' @param description Character or NULL. Optional description.
#' @param template_source Character or NA. Source of the template used to
#'   generate the dataset specification.
#' @param template_version Character or NA. Version of the template used.
#' @param template_date Character or NA. Date of the template used.
#' @return An object of class DTADataSetFile.
#' @export
DTADataSetFile <- S7::new_class(
  "DTADataSetFile",
  parent = DTADataSet,
  constructor = function(
    name,
    paths = character(),
    files = list(),
    description = NULL,
    template_source = NULL,
    template_version = NULL,
    template_date = NULL
  ) {
    if (inherits(files, "DTAtools::DTAFile")) {
      files <- list(files)
    }

    if (length(paths) > 0 && length(files) == 0) {
      # DTAFileAny, not a bare DTAFile: this handler is only ever asked whether
      # the delivered name matches, never to read anything, and `any` is the one
      # type dta_handler_type()/DTAFileFactory() can name in both directions. A
      # bare DTAFile would serialise to a `type:` nothing could read back.
      files <- lapply(paths, function(path) {
        DTAFileAny(filename = basename(path), number_of_files = 1)
      })
    }

    # A file dataset never parses anything -- check() only ever asks whether a
    # target arrived, is non-empty and can be opened -- so a reader handler
    # (DTAFileCSV, DTAFileTSV, a bare DTAFile, ...) is meaningless here and
    # would let the rest of the package treat this dataset as readable when it
    # is not. The fix is COERCION, not rejection: an existing document that
    # named a reader type before this constraint existed must keep loading, so
    # every non-DTAFileAny handler is rebuilt as a DTAFileAny carrying every
    # property the source handler has. Properties are read defensively because
    # a bare DTAFile lacks some of what a reader subclass would carry.
    files <- lapply(files, function(h) {
      if (inherits(h, "DTAtools::DTAFileAny")) {
        return(h)
      }

      DTAFileAny(
        filename = tryCatch(h@filename, error = function(e) NULL),
        pattern = tryCatch(h@pattern, error = function(e) NULL),
        pattern_description = tryCatch(h@pattern_description, error = function(e) NULL),
        number_of_files = NULL,
        min_number_of_files = tryCatch(h@min_number_of_files, error = function(e) NULL),
        max_number_of_files = tryCatch(h@max_number_of_files, error = function(e) NULL),
        info = tryCatch(h@info, error = function(e) NULL)
      )
    })

    new_object(
      .parent = DTADataSet(
        name = name,
        type = "file",
        files = files,
        template_source = template_source,
        template_version = template_version,
        template_date = template_date,
        description = description
      ),
      file_paths = paths,
      validation_index = list(),
      validation_store = list(),
      validation_artifact_dir = NULL
    )
  },
  properties = list(
    file_paths = S7::new_property(S7::class_character, default = character()),
    validation_index = S7::new_property(S7::class_list, default = list()),
    validation_store = S7::new_property(S7::class_list, default = list()),
    # Same contract as DTADataSetTabular: remembers where check(persist = TRUE)
    # wrote its artifacts so a caller-supplied directory survives to the next
    # check() call.
    validation_artifact_dir = class_character_or_null
  ),
  validator = function(self) {
    if (!is.character(self@file_paths) && !is.null(self@file_paths)) {
      cli_abort("'file_paths' must be a character vector or NULL.")
    }

    if (!is.null(self@validation_artifact_dir) && !dir.exists(self@validation_artifact_dir)) {
      cli_abort("Property 'validation_artifact_dir' must be a valid directory path or NULL.")
    }
  }
)

# Builds one stable, unique key per target file. Plain basename() is not
# unique: two paths in different directories sharing a basename would collapse
# into a single validation entry and one file would silently disappear from the
# report. Basenames are kept where they are unambiguous, so the common case
# stays human readable, and colliding entries fall back to their full path.
#' @keywords internal
dta_file_target_keys <- function(paths) {
  if (length(paths) == 0) {
    return(character())
  }

  keys <- basename(paths)
  colliding <- keys %in% keys[duplicated(keys)]
  keys[colliding] <- paths[colliding]

  # Guards against the same path being listed twice.
  make.unique(keys, sep = "_")
}

# The file-dataset counterpart of dta_table_id_to_names(). It resolves against
# an explicit vector of names rather than x@tables, because check() has to
# select among the *targets* (which exist before any validation) while
# validation_status()/messages() select among the *validated* entries.
#' @keywords internal
dta_file_id_to_names <- function(all_names, tables = NULL) {
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

# The targets this dataset validates: the UNION of what was actually
# delivered (@file_paths) and what the specification declares (@files),
# returned as a NAMED character vector (key -> path). A delivered path is
# never dropped just because it also satisfies a declared handler, and a
# declared handler is never dropped just because nothing arrived for it --
# doing the latter used to make a 1-of-3 delivery report as a clean PASS.
#
# For every handler with no delivered path satisfying it (tested with
# matches_filename(), never by treating the declared name as a path to stat),
# every name it declares contributes one entry with path = NA_character_ --
# the marker for "declared but not delivered". `filename` is a VECTOR, so this
# flattens with unlist()/lapply(), never vapply(..., character(1)), which
# would crash on a handler that declares more than one name.
#
# Names are deduplicated; where a declared name collides with a delivered
# key, the delivered entry wins (setdiff() drops it from the missing side).
#' @keywords internal
dta_file_dataset_targets <- function(x) {
  paths <- x@file_paths
  delivered <- stats::setNames(paths, dta_file_target_keys(paths))

  declared_names <- unlist(lapply(x@files, function(handler) {
    satisfied <- length(paths) > 0 && any(vapply(
      paths,
      function(p) any(matches_filename(handler, basename(p))),
      logical(1)
    ))

    if (satisfied) character(0) else handler@filename
  }))

  missing_names <- setdiff(declared_names, names(delivered))
  if (length(missing_names) > 0) {
    missing <- stats::setNames(
      rep(NA_character_, length(missing_names)),
      missing_names
    )
    delivered <- c(delivered, missing)
  }

  delivered
}

# Staleness signal for one target, the file-dataset analogue of the table hash
# DTADataSetTabular compares. Identity (path), existence, size and mtime are
# what this dataset's checks actually depend on, and all four are cheap to read
# -- unlike a content digest, which would re-read every transfer file on every
# check(). A touched-but-identical file merely re-validates; the fingerprint
# never claims "unchanged" for a file that changed.
#' @keywords internal
dta_file_fingerprint <- function(path) {
  info <- file.info(path)

  dta_hash_object(list(
    path = path,
    exists = file.exists(path),
    size = unname(info$size),
    mtime = unname(info$mtime),
    isdir = unname(info$isdir)
  ))
}

#' @keywords internal
validate_file_dataset_entry <- function(path) {
  if (!file.exists(path)) {
    return(list(
      ok = FALSE,
      message = sprintf("File '%s' not found.", path)
    ))
  }

  if (!file.info(path)$isdir && file.size(path) <= 0) {
    return(list(
      ok = FALSE,
      message = sprintf("File '%s' is empty.", path)
    ))
  }

  if (file.access(path, 4) != 0) {
    return(list(
      ok = FALSE,
      message = sprintf("File '%s' is not readable.", path)
    ))
  }

  readable <- tryCatch(
    {
      con <- file(path, open = "rb")
      on.exit(close(con), add = TRUE)
      readBin(con, what = "raw", n = 1)
      TRUE
    },
    error = function(e) FALSE
  )

  if (!isTRUE(readable)) {
    return(list(
      ok = FALSE,
      message = sprintf("File '%s' could not be read.", path)
    ))
  }

  list(ok = TRUE, message = NULL)
}

# The detailed result for one target, in the same shape validate_table_detailed()
# produces for a tabular target, so that everything downstream (messages(),
# inspect(), the persisted artifact) reads one format.
#' @keywords internal
dta_file_validation_details <- function(validation_result) {
  ok <- isTRUE(validation_result$ok)
  failure <- list(list(
    id = "file_presence",
    valid = FALSE,
    message = validation_result$message
  ))

  list(
    ok = ok,
    columnspec_valid = TRUE,
    rules_valid = ok,
    import_valid = TRUE,
    n_columnspec_errors = 0L,
    n_rule_errors = if (ok) 0L else 1L,
    n_import_errors = 0L,
    columnspec_errors = list(summarised_error = NULL, full_error = NULL),
    rule_results = if (ok) {
      list(list(id = "file_presence", valid = TRUE, message = NULL))
    } else {
      failure
    },
    rule_errors = if (ok) list() else failure,
    import_errors = NULL,
    result_version = 2L
  )
}

# The status row of a target that has not been validated. Also, called with
# character(0), the empty frame validation_status() must return when there is
# nothing to report -- one definition, so the "no entry" and "no entries at all"
# shapes cannot drift apart.
#' @keywords internal
dta_file_empty_status_row <- function(table_name) {
  data.frame(
    table = table_name,
    target_type = rep("file", length(table_name)),
    status = rep("not_validated", length(table_name)),
    ok = rep(NA, length(table_name)),
    validated_at = rep(NA_character_, length(table_name)),
    run_id = rep(NA_character_, length(table_name)),
    validation_run = rep(NA_character_, length(table_name)),
    n_columnspec_errors = rep(NA_integer_, length(table_name)),
    n_rule_errors = rep(NA_integer_, length(table_name)),
    n_import_errors = rep(NA_integer_, length(table_name)),
    stringsAsFactors = FALSE
  )
}

# Validates and coerces `handler_index` before it is used to subscript
# `files_list`. The guard it replaces, `handler_index < 1 || handler_index >
# length(...)`, ran an unguarded `||` on a value that could be NULL, NA,
# length > 1, or character -- and a character index does STRING comparison,
# so `"2" > 12` is TRUE and a perfectly valid index was rejected, even though
# load_file()'s own documentation promises a character index is accepted.
#
# A character index is first tried against the handler list's own names (were
# it ever a named list); failing that it is coerced with as.integer() the same
# way a numeric index is, so "2" behaves exactly like 2.
#' @keywords internal
dta_resolve_file_handler_index <- function(handler_index, files_list) {
  if (
    is.null(handler_index) ||
      length(handler_index) != 1 ||
      is.na(handler_index)
  ) {
    cli::cli_abort(
      "'handler_index' must be a single, non-missing character or numeric value."
    )
  }

  if (is.character(handler_index)) {
    handler_names <- names(files_list)
    if (!is.null(handler_names) && handler_index %in% handler_names) {
      return(handler_index)
    }

    coerced <- suppressWarnings(as.integer(handler_index))
    if (is.na(coerced)) {
      cli::cli_abort(
        "Invalid handler_index: '{handler_index}'. Must name a file handler or be a numeric index between 1 and {length(files_list)}."
      )
    }
    handler_index <- coerced
  } else if (!is.numeric(handler_index)) {
    cli::cli_abort(
      "'handler_index' must be a single character or numeric value, not {.cls {class(handler_index)[[1]]}}."
    )
  }

  handler_index <- as.integer(handler_index)
  if (
    is.na(handler_index) ||
      handler_index < 1 ||
      handler_index > length(files_list)
  ) {
    cli::cli_abort(
      "Invalid handler_index: {handler_index}. Must be between 1 and {length(files_list)}."
    )
  }

  handler_index
}

#' @title Load a file into a DTADataSetFile
#' @description
#' Binds a delivered file to a file-backed dataset, the counterpart of the
#' \code{\link{DTADataSetTabular}} method that reads a table.
#'
#' Nothing is read, opened or stat-ed here. Whether the file exists, is
#' non-empty and can be opened is the entire contract of \code{\link{check}()}
#' for this class, and deferring to it is what lets a specification bind a file
#' that has not arrived yet and report it as missing rather than refusing to
#' record it at all.
#'
#' The delivered name is still checked against the handler, exactly as
#' \code{\link{read_file}()} does for a tabular dataset, so a file dropped into
#' the wrong slot is refused the same way in both.
#'
#' @param x A \code{DTA}, \code{DTADataSetTabular} or \code{DTADataSetFile}
#'   object -- this page documents every \code{load_file()} method.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{file}{Path to the delivered file.}
#'     \item{handler_index}{Single character or numeric index selecting the
#'       file handler within the dataset. Defaults to \code{1}.}
#'     \item{name}{Optional name under which the file is recorded. Must equal
#'       \code{basename(file)} -- with the extension, unlike the tabular
#'       method, because that is the key every report for this class uses. A
#'       file dataset is keyed by the delivered file's own name, so \code{name}
#'       cannot rename it; passing anything else aborts. Defaults to
#'       \code{basename(file)}.}
#'     \item{stream}{Accepted and ignored; a file dataset reads nothing.}
#'   }
#' @return The updated \code{DTADataSetFile} object.
#' @examples
#' handler <- DTAFileAny(filename = "clinical_data.csv")
#' ds <- DTADataSetFile(name = "delivery", files = list(handler))
#'
#' file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#' ds <- load_file(ds, file = file, handler_index = 1)
#' validation_status(check(ds, quiet = TRUE))$ok
#' @usage load_file(x, ...)
#' @rdname load_file
#' @export
method(load_file, DTADataSetFile) <- function(
  x,
  file,
  handler_index = 1,
  name = basename(file),
  # Accepted and ignored, like check()'s batch_rows/max_errors. A file dataset
  # reads nothing, so there is no in-memory-versus-lazy decision to make -- but
  # method(load_file, DTA) forwards `stream` to every dataset it dispatches to,
  # and without the formal that call dies on "unused argument" before this body
  # is ever reached.
  stream = getOption("DTAtools.stream", "auto"),
  ...
) {
  handler_index <- dta_resolve_file_handler_index(handler_index, x@files)

  file <- dta_assert_single_file_path(file, "load_file")

  # `name` is not a rename hook: this class keys every report by the
  # delivered file's own name, so a caller-supplied `name` that diverges from
  # it would silently unbind whatever was previously recorded under the real
  # name (or the name it was told to use) instead of replacing it. The default
  # is always identical to this, so only an explicit, divergent `name` aborts.
  if (!identical(name, basename(file))) {
    cli::cli_abort(
      "'name' ({.val {name}}) must equal the delivered file's own name ({.val {basename(file)}}). A file dataset is keyed by the delivered file's name and 'name' cannot rename it."
    )
  }

  handler <- files(x, handler_index)
  # any(): matches_filename() yields one logical PER declared name or pattern,
  # and a handler may carry several. isTRUE() alone would collapse a length > 1
  # result to FALSE and reject a file that matched.
  if (!isTRUE(any(matches_filename(handler, basename(file))))) {
    cli::cli_abort(
      "The provided file '{file}' does not match the filename or pattern in the DTAFile object."
    )
  }

  # Replace in place when this file is already bound, append otherwise -- the
  # same contract as the tabular method's `x@tables[[name]] <- ...`. The PATH
  # is matched first, so redelivering the same file from the same location
  # always replaces; only when that fails does a KEY match apply, so a
  # redelivery of a colliding basename from a different directory also
  # replaces, rather than silently accumulating under ever-lengthening
  # full-path keys.
  slot <- match(file, x@file_paths)
  if (is.na(slot)) {
    slot <- match(name, dta_file_target_keys(x@file_paths))
  }

  if (is.na(slot)) {
    x@file_paths <- c(x@file_paths, file)
  } else {
    x@file_paths[[slot]] <- file
    # The previous file's verdict describes a file that is no longer there.
    # Left behind, check() would compare the new file against the old
    # fingerprint, find them different, and revalidate -- but until it ran, every
    # report would show the replaced file's result under the new file's name.
    # The key is re-derived from the file_paths AFTER the replacement, and
    # picked out by `slot` -- not `name` -- because a basename collision can
    # make the key at this position a full path rather than the basename.
    replaced_key <- dta_file_target_keys(x@file_paths)[[slot]]
    x@validation_index[[replaced_key]] <- NULL
    x@validation_store[[replaced_key]] <- NULL
  }

  x
}


#' @title Clear Validation State of a DTADataSetFile
#' @description Clears in-memory validation state for one or all file targets.
#' @param x A \code{DTADataSetTabular} or \code{DTADataSetFile} object --
#'   this page documents both \code{clear_validation()} methods.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{tables}{NULL (default), character target names, or numeric target
#'       indices.}
#'     \item{remove_artifacts}{Logical. If TRUE, delete artifact files for the
#'       selected targets.}
#'   }
#' @return Invisibly returns \code{x}.
#' @examples
#' path <- tempfile(fileext = ".txt")
#' writeLines("content", path)
#' ds <- check(DTADataSetFile(name = "delivery", paths = path), quiet = TRUE)
#' nrow(validation_status(clear_validation(ds)))
#' @usage clear_validation(x, ...)
#' @rdname clear_validation
#' @export
S7::method(clear_validation, DTADataSetFile) <- function(
  x,
  tables = NULL,
  remove_artifacts = FALSE
) {
  # Resolved against the VALIDATED entries rather than the dataset's targets:
  # there is nothing to clear for a target that was never checked, and asking
  # for one by index should mean the same thing here as it does in
  # validation_status() and messages().
  target_tables <- dta_file_id_to_names(names(x@validation_index), tables)

  for (table_name in target_tables) {
    entry <- x@validation_index[[table_name]]

    if (remove_artifacts && !is.null(entry) && !is.null(entry$artifact_path)) {
      if (file.exists(entry$artifact_path)) {
        unlink(entry$artifact_path)
      }
    }

    x@validation_index[[table_name]] <- NULL
    x@validation_store[[table_name]] <- NULL
    # No @import_issues here, unlike the tabular method: nothing is typed on the
    # way in, so a file dataset has no import axis to forget.
  }

  invisible(x)
}


#' @title Check DTADataSetFile
#' @description
#' Validates a \code{DTADataSetFile} object's underlying file(s) and structure.
#' @param x A \code{DTA}, \code{DTADataSet}, \code{DTADataSetTabular} or
#'   \code{DTADataSetFile} object -- this page documents every \code{check()} method.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{tables}{Optional. Character target names or numeric target indices
#'       to validate. If NULL (default), validates all targets.}
#'     \item{force}{Logical. If TRUE, forces re-validation even if the file is
#'       unchanged since the last check. Default is FALSE.}
#'     \item{persist}{Logical. If TRUE (default), persists validation
#'       artifacts to disk.}
#'     \item{artifact_dir}{Character or NULL. Optional output directory for
#'       persisted validation artifacts.}
#'     \item{quiet}{Logical. If TRUE, suppresses console output. Default is FALSE.}
#'   }
#' @return Invisibly returns the updated \code{DTADataSetFile} object \code{x}.
#' @usage check(x, ...)
#' @name check
#' @export
S7::method(check, DTADataSetFile) <- function(
  x,
  tables = NULL,
  force = FALSE,
  persist = TRUE,
  artifact_dir = NULL,
  quiet = FALSE,
  validation_run = NULL,
  # Accepted and ignored. A file dataset checks files, not tables, so there is
  # nothing to scan in batches -- but `check()` on a DTA calls every dataset the
  # same way, and it should not have to branch on the dataset's class to do it.
  batch_rows = NULL,
  max_errors = NULL
) {
  if (is.null(validation_run)) {
    validation_run <- dta_new_validation_run_id()
  }

  # target_files is now a NAMED vector (key -> path, NA for an undelivered
  # target); its own names ARE the keys, and re-deriving them with
  # dta_file_target_keys() would try to treat every NA entry as a path.
  target_files <- dta_file_dataset_targets(x)
  target_keys <- names(target_files)
  selected_keys <- dta_file_id_to_names(target_keys, tables)

  # Entries for targets outside `tables` are carried over untouched, exactly as
  # DTADataSetTabular does; rebuilding the index from scratch would delete the
  # validation state of every target the caller did not ask for.
  validation_index <- x@validation_index
  validation_store <- x@validation_store
  output_rows <- list()

  if (isTRUE(persist)) {
    if (is.null(artifact_dir)) {
      artifact_dir <- if (!is.null(x@validation_artifact_dir)) {
        x@validation_artifact_dir
      } else {
        dta_default_validation_artifact_dir(x)
      }
    }
    dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
    x@validation_artifact_dir <- artifact_dir
  }

  for (idx in seq_along(selected_keys)) {
    table_name <- selected_keys[idx]
    path <- target_files[[match(table_name, target_keys)]]
    # NA marks a target that dta_file_dataset_targets() declared but that no
    # delivered path satisfies. The hash is stable for as long as the target
    # stays undelivered, so the unchanged/force skip logic below still applies
    # instead of re-reporting "missing" as a fresh finding on every check().
    missing <- is.na(path)
    file_hash <- if (missing) {
      dta_hash_object(list(missing = table_name))
    } else {
      dta_file_fingerprint(path)
    }

    previous <- validation_index[[table_name]]
    unchanged <- !is.null(previous) && identical(previous$file_hash, file_hash)

    if (!isTRUE(force) && unchanged) {
      previous$validation_run <- validation_run
      validation_index[[table_name]] <- previous

      output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
        table_name = table_name,
        status = "skipped",
        index_entry = previous,
        target_type = "file"
      )
      next
    }

    if (!isTRUE(quiet)) {
      cli::cli_text()
      cli::cli_rule(paste0("File ", idx, " of ", length(selected_keys), ": ", table_name))
    }

    if (missing) {
      # Never dta_file_fingerprint()/stat this target -- there is no delivered
      # path to look at -- and never persist an artifact for it. The details
      # still go through dta_file_validation_details() so the shape matches
      # every other target exactly; only the message differs.
      validation_result <- list(
        ok = FALSE,
        message = paste0("Expected file '", table_name, "' was not delivered.")
      )
      details <- dta_file_validation_details(validation_result)
      validated_at <- Sys.time()
      run_id <- format(validated_at, "%Y%m%dT%H%M%OS3")

      if (!isTRUE(quiet)) {
        cli::cli_alert_danger(validation_result$message)
      }

      entry <- list(
        ok = FALSE,
        validated_at = validated_at,
        run_id = run_id,
        validation_run = validation_run,
        file_hash = file_hash,
        n_columnspec_errors = 0L,
        n_rule_errors = details$n_rule_errors,
        n_import_errors = 0L,
        artifact_path = NULL,
        path = NA_character_,
        label = table_name
      )

      validation_index[[table_name]] <- entry
      validation_store[[table_name]] <- details

      output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
        table_name = table_name,
        status = "validated",
        index_entry = entry,
        target_type = "file"
      )
      next
    }

    validation_result <- validate_file_dataset_entry(path)
    details <- dta_file_validation_details(validation_result)
    validated_at <- Sys.time()
    run_id <- format(validated_at, "%Y%m%dT%H%M%OS3")
    artifact_path <- NULL

    if (isTRUE(persist)) {
      safe_target <- gsub("[^A-Za-z0-9_-]", "_", table_name)
      target_dir <- file.path(artifact_dir, safe_target)
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      artifact_path <- file.path(target_dir, paste0(run_id, ".rds"))
      saveRDS(details, artifact_path)
    }

    if (!isTRUE(quiet)) {
      if (isTRUE(details$ok)) {
        cli::cli_alert_success(paste0("File '", path, "' is readable and not empty."))
      } else {
        cli::cli_alert_danger(validation_result$message)
      }
    }

    entry <- list(
      ok = isTRUE(details$ok),
      validated_at = validated_at,
      run_id = run_id,
      validation_run = validation_run,
      file_hash = file_hash,
      n_columnspec_errors = 0L,
      n_rule_errors = details$n_rule_errors,
      n_import_errors = 0L,
      artifact_path = artifact_path,
      path = path,
      label = basename(path)
    )

    validation_index[[table_name]] <- entry
    validation_store[[table_name]] <- details

    output_rows[[length(output_rows) + 1]] <- dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = entry,
      target_type = "file"
    )
  }

  x@validation_index <- validation_index
  x@validation_store <- validation_store

  summary_df <- do.call(rbind, output_rows)
  attr(x, "last_validation_summary") <- summary_df

  if (!isTRUE(quiet) && !is.null(summary_df) && nrow(summary_df) > 0) {
    n_total <- nrow(summary_df)
    n_valid <- sum(summary_df$ok == TRUE, na.rm = TRUE)
    file_word <- if (n_total == 1) "file" else "files"

    cli::cli_text()
    if (n_valid < n_total) {
      cli::cli_alert_danger(paste0(n_valid, " of ", n_total, " ", file_word, " valid"))
    } else {
      cli::cli_alert_success(paste0(n_total, " ", file_word, " passed validation"))
    }
  }

  invisible(x)
}

#' @export
S7::method(validation_errors, DTADataSetFile) <- function(
  x,
  table,
  source = c("auto", "memory", "artifact")
) {
  source <- match.arg(source)
  table_name <- dta_file_id_to_names(names(x@validation_index), table)
  table_name <- table_name[[1]]

  if (source %in% c("auto", "memory")) {
    in_memory <- x@validation_store[[table_name]]
    if (!is.null(in_memory)) {
      return(dta_as_validation_details(dta_migrate_validation_details(in_memory)))
    }
  }

  entry <- x@validation_index[[table_name]]
  if (is.null(entry) || is.null(entry$artifact_path)) {
    cli::cli_abort(
      "No validation artifact available for target '{table_name}'. Run check() first with persist = TRUE."
    )
  }

  if (!file.exists(entry$artifact_path)) {
    cli::cli_abort(
      "Validation artifact for target '{table_name}' does not exist at '{entry$artifact_path}'."
    )
  }

  # Migrated on read like the tabular artifacts: an artifact written before the
  # import axis existed reports import_valid = NA ("unknown"), never a clean
  # axis it never checked.
  dta_as_validation_details(
    dta_migrate_validation_details(readRDS(entry$artifact_path))
  )
}

#' @export
S7::method(results, DTADataSetFile) <- function(x, tables = NULL) {
  status_df <- validation_status(x, tables = tables)
  dataset_name <- if (!is.null(x@name) && nzchar(x@name)) x@name else NA_character_
  dta_results_from_status(status_df, dataset_name = dataset_name)
}

#' @export
S7::method(messages, DTADataSetFile) <- function(
  x,
  tables = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  source <- match.arg(source)
  target_tables <- if (is.null(tables)) {
    names(x@validation_index)
  } else {
    if (is.numeric(tables)) {
      table_names <- names(x@validation_index)
      if (any(tables < 1) || any(tables > length(table_names))) {
        cli::cli_abort("Table index out of bounds.")
      }
      table_names[tables]
    } else if (is.character(tables)) {
      missing <- setdiff(tables, names(x@validation_index))
      if (length(missing) > 0) {
        cli::cli_abort("Table{?s} not found: {.field {missing}}")
      }
      tables
    } else {
      cli::cli_abort("'tables' must be NULL, numeric, or character.")
    }
  }

  out <- lapply(target_tables, function(table_name) {
    details <- x@validation_store[[table_name]]
    if (is.null(details)) {
      return(dta_empty_messages())
    }

    rule_errors <- details$rule_errors
    if (is.null(rule_errors) || length(rule_errors) == 0) {
      return(dta_empty_messages())
    }

    do.call(rbind, lapply(rule_errors, function(err) {
      data.frame(
        dataset = x@name,
        target = table_name,
        severity = "error",
        source = "rule",
        rule_id = if (!is.null(err$id)) as.character(err$id) else NA_character_,
        row = NA_real_,
        column = NA_character_,
        keyword = NA_character_,
        message = if (!is.null(err$message)) as.character(err$message) else "file validation error",
        stringsAsFactors = FALSE
      )
    }))
  })

  if (length(out) == 0) {
    return(dta_to_tibble_if_available(dta_empty_messages(), as_tibble = as_tibble))
  }

  msgs <- do.call(rbind, out)
  if (is.null(msgs) || nrow(msgs) == 0) {
    return(dta_to_tibble_if_available(dta_empty_messages(), as_tibble = as_tibble))
  }

  msgs <- dta_attach_message_ids(msgs)
  rownames(msgs) <- NULL
  dta_to_tibble_if_available(msgs, as_tibble = as_tibble)
}
#
#' @export
S7::method(validation_status, DTADataSetFile) <- function(x, tables = NULL) {
  target_tables <- if (is.null(tables)) {
    names(x@validation_index)
  } else {
    if (is.numeric(tables)) {
      table_names <- names(x@validation_index)
      if (any(tables < 1) || any(tables > length(table_names))) {
        cli::cli_abort("Table index out of bounds.")
      }
      table_names[tables]
    } else if (is.character(tables)) {
      missing <- setdiff(tables, names(x@validation_index))
      if (length(missing) > 0) {
        cli::cli_abort("Table{?s} not found: {.field {missing}}")
      }
      tables
    } else {
      cli::cli_abort("'tables' must be NULL, numeric, or character.")
    }
  }

  rows <- lapply(target_tables, function(table_name) {
    entry <- x@validation_index[[table_name]]
    if (is.null(entry)) {
      return(dta_file_empty_status_row(table_name))
    }

    dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = entry,
      target_type = "file"
    )
  })

  # rbind() of an empty list is NULL, and check(DTA) calls nrow() on whatever
  # this returns -- so a dataset with nothing validated yet has to answer with
  # an empty FRAME carrying the real columns, not with nothing at all.
  if (length(rows) == 0) {
    return(dta_file_empty_status_row(character(0)))
  }

  do.call(rbind, rows)
}
#
#' @export
S7::method(inspect, DTADataSetFile) <- function(
  x,
  id = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  source <- match.arg(source)
  msgs <- messages(x, source = source, as_tibble = FALSE)
  msg_rows <- dta_get_message_rows_by_id(msgs, id)

  records <- lapply(seq_len(nrow(msg_rows)), function(i) {
    msg_row <- msg_rows[i, , drop = FALSE]
    target_name <- as.character(msg_row$target)
    details <- x@validation_store[[target_name]]

    out <- list(
      id = as.integer(msg_row$id),
      dataset = as.character(msg_row$dataset),
      target = target_name,
      source = as.character(msg_row$source),
      severity = as.character(msg_row$severity),
      type = "rule",
      headline = sprintf("[%s/%s] %s", msg_row$dataset, target_name, msg_row$message),
      why = "File-level rule checks file presence/readability/non-empty constraints.",
      message = as.character(msg_row$message),
      rule_id = as.character(msg_row$rule_id),
      file_path = NA_character_,
      details = details
    )

    entry <- x@validation_index[[target_name]]
    if (!is.null(entry$path)) {
      out$file_path <- entry$path
    } else if (length(x@file_paths) > 0) {
      match_idx <- which(basename(x@file_paths) == target_name)
      if (length(match_idx) > 0) {
        out$file_path <- x@file_paths[[match_idx[[1]]]]
      }
    }

    out
  })

  out_df <- dta_inspect_records_to_df(records)
  dta_to_tibble_if_available(out_df, as_tibble = as_tibble)
}
