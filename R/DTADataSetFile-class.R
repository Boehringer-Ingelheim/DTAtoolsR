#' @title DTADataSetFile Class
#' @description Handles file-backed datasets that only need to verify that one
#'   or more referenced files exist, are readable, and are not empty.
#' @import S7
#' @importFrom cli cli_abort
#' @param name Character. Name of the container.
#' @param paths Character vector of file paths to validate.
#' @param files A list of DTAFile objects specifying input file information.
#' @param description Character or NULL. Optional description.
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
      validation_store = list()
    )
  },
  properties = list(
    file_paths = S7::new_property(S7::class_character, default = character()),
    validation_index = S7::new_property(S7::class_list, default = list()),
    validation_store = S7::new_property(S7::class_list, default = list())
  ),
  validator = function(self) {
    if (!is.character(self@file_paths) && !is.null(self@file_paths)) {
      cli_abort("'file_paths' must be a character vector or NULL.")
    }
  }
)

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

  readable <- tryCatch({
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, what = "raw", n = 1)
    TRUE
  }, error = function(e) FALSE)

  if (!isTRUE(readable)) {
    return(list(
      ok = FALSE,
      message = sprintf("File '%s' could not be read.", path)
    ))
  }

  list(ok = TRUE, message = NULL)
}

#' @title Check DTADataSetFile
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

  target_files <- if (length(x@file_paths) > 0) {
    x@file_paths
  } else {
    vapply(x@files, function(file_info) file_info@filename, character(1))
  }

  output_rows <- list()
  validation_details <- list()
  validation_index <- list()
  validation_store <- list()

  for (idx in seq_along(target_files)) {
    path <- target_files[idx]
    table_name <- basename(path)
    validation_result <- validate_file_dataset_entry(path)
    validated_at <- Sys.time()

    entry <- list(
      ok = isTRUE(validation_result$ok),
      validated_at = validated_at,
      run_id = format(validated_at, "%Y%m%dT%H%M%OS3"),
      validation_run = validation_run,
      n_schema_errors = 0L,
      n_rule_errors = if (isTRUE(validation_result$ok)) 0L else 1L,
      artifact_path = NULL
    )

    if (!isTRUE(validation_result$ok)) {
      validation_details[[length(validation_details) + 1]] <- list(
        ok = FALSE,
        schema_valid = TRUE,
        rules_valid = FALSE,
        n_schema_errors = 0L,
        n_rule_errors = 1L,
        schema_errors = list(summarised_error = NULL, full_error = NULL),
        rule_results = list(list(id = "file_presence", valid = FALSE, message = validation_result$message)),
        rule_errors = list(list(id = "file_presence", valid = FALSE, message = validation_result$message))
      )
    } else {
      validation_details[[length(validation_details) + 1]] <- list(
        ok = TRUE,
        schema_valid = TRUE,
        rules_valid = TRUE,
        n_schema_errors = 0L,
        n_rule_errors = 0L,
        schema_errors = list(summarised_error = NULL, full_error = NULL),
        rule_results = list(list(id = "file_presence", valid = TRUE, message = NULL)),
        rule_errors = list()
      )
    }

    validation_index[[table_name]] <- entry
    validation_store[[table_name]] <- validation_details[[length(validation_details)]]

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
  invisible(x)
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

    if (length(x@file_paths) > 0) {
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
