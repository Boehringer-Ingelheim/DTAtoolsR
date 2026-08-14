#' @title DTADataSetFile Class
#' @description Handles file-backed datasets that only need to verify that one
#'   or more referenced files exist, are readable, and are not empty.
#' @import S7
#' @importFrom cli cli_abort
#' @include validationReporting.R
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
      files <- lapply(paths, function(path) {
        DTAFile(filename = basename(path), number_of_files = 1)
      })
    }

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

# The paths this dataset validates: explicit paths when given, otherwise the
# filenames declared by the file handlers.
#' @keywords internal
dta_file_dataset_targets <- function(x) {
  if (length(x@file_paths) > 0) {
    return(x@file_paths)
  }

  vapply(x@files, function(file_info) file_info@filename, character(1))
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
    schema_valid = TRUE,
    rules_valid = ok,
    import_valid = TRUE,
    n_schema_errors = 0L,
    n_rule_errors = if (ok) 0L else 1L,
    n_import_errors = 0L,
    schema_errors = list(summarised_error = NULL, full_error = NULL),
    rule_results = if (ok) {
      list(list(id = "file_presence", valid = TRUE, message = NULL))
    } else {
      failure
    },
    rule_errors = if (ok) list() else failure,
    import_errors = NULL,
    schema_version = 2L
  )
}

#' @title Check DTADataSetFile
#' @description
#' Validates a \code{DTADataSetFile} object's underlying file(s) and structure.
#' @param x A \code{DTADataSetFile} object.
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
  validation_run = NULL
) {
  if (is.null(validation_run)) {
    validation_run <- dta_new_validation_run_id()
  }

  target_files <- dta_file_dataset_targets(x)
  target_keys <- dta_file_target_keys(target_files)
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
    file_hash <- dta_file_fingerprint(path)

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
      n_schema_errors = 0L,
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
      return(data.frame(
        table = table_name,
        target_type = "file",
        status = "not_validated",
        ok = NA,
        validated_at = NA_character_,
        run_id = NA_character_,
        validation_run = NA_character_,
        n_schema_errors = NA_integer_,
        n_rule_errors = NA_integer_,
        n_import_errors = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }

    dta_validation_result_to_row(
      table_name = table_name,
      status = "validated",
      index_entry = entry,
      target_type = "file"
    )
  })

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
