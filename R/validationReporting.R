#' @title Retrieve Check Results
#' @description
#' Returns a programmatic summary of the latest validation/check state.
#' Unlike `check()`, this function is read-only and does not perform validation.
#'
#' Methods are available for `DTA`, `DTADataSet`, and `DTADataSetTabular`.
#' @param x A `DTA`, `DTADataSet`, or `DTADataSetTabular` object.
#' @param ... Additional method arguments.
#' @return
#' A data.frame summarizing validation state.
#'
#' For `DTADataSetTabular`, columns are:
#' `target`, `status`, `validated_at`, `run_id`, `validation_run`,
#' `n_columnspec_errors`, `n_rule_errors`, `n_import_errors`.
#'
#' For `DTA`, the summary is aggregated per dataset and includes:
#' `dataset`, `n_targets`, `n_validated`, `n_valid`, `n_invalid`,
#' `n_skipped`, `n_not_validated`.
#' @examples
#' dta <- create_example_DTA()
#' dta <- check(dta, quiet = TRUE)
#' results(dta)
#'
#' ds <- dta[["demographics"]]
#' results(ds)
#' @name results
#' @include DTA-class.R DTADataSet-class.R DTADataSetTabular-class.R
#' @export
results <- S7::new_generic("results", "x")

#' @keywords internal
dta_results_from_status <- function(status_df, dataset_name = NA_character_) {
  if (is.null(status_df) || nrow(status_df) == 0) {
    # `dataset` must be zero-length here like every other column. Recycling the
    # length-1 `dataset_name` against zero-length columns is not a recycle at
    # all: data.frame() aborts with "arguments imply differing number of rows",
    # so this branch used to work only when the caller passed a zero-length
    # name -- which no real caller does.
    return(data.frame(
      dataset = character(0),
      target = character(0),
      target_type = character(0),
      status = character(0),
      validated_at = character(0),
      run_id = character(0),
      validation_run = character(0),
      n_columnspec_errors = integer(0),
      n_rule_errors = integer(0),
      n_import_errors = integer(0),
      n_targets = integer(0),
      n_validated = integer(0),
      n_valid = integer(0),
      n_invalid = integer(0),
      n_skipped = integer(0),
      n_not_validated = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  result_status <- ifelse(
    status_df$status == "validated" & !is.na(status_df$ok) & !status_df$ok,
    "failed",
    status_df$status
  )

  data.frame(
    dataset = dataset_name,
    target = status_df$table,
    target_type = status_df$target_type,
    status = result_status,
    validated_at = status_df$validated_at,
    run_id = status_df$run_id,
    validation_run = status_df$validation_run,
    n_columnspec_errors = status_df$n_columnspec_errors,
    n_rule_errors = status_df$n_rule_errors,
    n_import_errors = if (is.null(status_df$n_import_errors)) {
      NA_integer_
    } else {
      status_df$n_import_errors
    },
    n_targets = nrow(status_df),
    n_validated = sum(status_df$status == "validated", na.rm = TRUE),
    n_valid = sum(status_df$ok == TRUE, na.rm = TRUE),
    n_invalid = sum(status_df$ok == FALSE, na.rm = TRUE),
    n_skipped = sum(status_df$status == "skipped", na.rm = TRUE),
    n_not_validated = sum(status_df$status == "not_validated", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

#' @export
S7::method(results, DTADataSetTabular) <- function(x, tables = NULL) {
  status_df <- validation_status(x, tables = tables)
  dataset_name <- if (!is.null(x@name) && nzchar(x@name)) x@name else NA_character_
  dta_results_from_status(status_df, dataset_name = dataset_name)
}

#' @export
S7::method(results, DTADataSet) <- function(x, tables = NULL) {
  has_tables <- !is.null(tryCatch(x@tables, error = function(e) NULL))
  has_validation_index <- !is.null(tryCatch(x@validation_index, error = function(e) NULL))

  if (!has_tables || !has_validation_index) {
    cli::cli_abort(
      "results() requires a DTADataSet subclass with table validation state."
    )
  }

  status_df <- validation_status(x, tables = tables)
  dataset_name <- if (!is.null(x@name) && nzchar(x@name)) x@name else NA_character_
  dta_results_from_status(status_df, dataset_name = dataset_name)
}

#' @export
S7::method(results, DTA) <- function(x, datasets = NULL) {
  if (is.null(x@datasets) || length(x@datasets) == 0) {
    cli::cli_abort("DTA object has no datasets.")
  }

  target_datasets <- if (is.null(datasets)) {
    names(x@datasets)
  } else if (is.numeric(datasets)) {
    if (any(datasets < 1) || any(datasets > length(x@datasets))) {
      cli::cli_abort("Dataset index out of bounds.")
    }
    names(x@datasets)[datasets]
  } else if (is.character(datasets)) {
    missing <- setdiff(datasets, names(x@datasets))
    if (length(missing) > 0) {
      cli::cli_abort("The following dataset{?s} not found: {.field {missing}}")
    }
    datasets
  } else {
    cli::cli_abort("'datasets' must be NULL, a character vector, or a numeric vector.")
  }

  rows <- lapply(target_datasets, function(ds_name) {
    ds <- x@datasets[[ds_name]]

    if (!inherits(ds, "DTAtools::DTADataSet")) {
      return(data.frame(
        dataset = ds_name,
        target = NA_character_,
        target_type = NA_character_,
        status = "not_validated",
        validated_at = NA_character_,
        run_id = NA_character_,
        validation_run = NA_character_,
        n_columnspec_errors = NA_integer_,
        n_rule_errors = NA_integer_,
        n_import_errors = NA_integer_,
        n_targets = NA_integer_,
        n_validated = NA_integer_,
        n_valid = NA_integer_,
        n_invalid = NA_integer_,
        n_skipped = NA_integer_,
        n_not_validated = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }

    results(ds)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' @title Retrieve Check Messages
#' @description
#' Returns one row per validation message to support debugging workflows.
#' The output combines column spec and rule failures into a single flat table.
#'
#' Methods are available for `DTA`, `DTADataSet`, and `DTADataSetTabular`.
#' @param x A `DTA`, `DTADataSet`, or `DTADataSetTabular` object.
#' @param ... Additional method arguments.
#' @return
#' A data.frame (or tibble when `as_tibble = TRUE` and package `tibble`
#' is installed) with columns:
#' `dataset`, `target`, `severity`, `source`, `rule_id`,
#' `row`, `column`, `keyword`, `message`.
#' @examples
#' ds <- create_example_DTADataSetTabular(2)
#' ds <- check(ds, quiet = TRUE)
#' messages(ds)
#' @name messages
#' @export
messages <- S7::new_generic("messages", "x")

#' @keywords internal
dta_empty_messages <- function() {
  data.frame(
    id = integer(0),
    dataset = character(0),
    target = character(0),
    severity = character(0),
    source = character(0),
    rule_id = character(0),
    row = numeric(0),
    column = character(0),
    keyword = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
dta_attach_message_ids <- function(msgs) {
  if (is.null(msgs) || nrow(msgs) == 0) {
    return(dta_empty_messages())
  }

  msgs$id <- seq_len(nrow(msgs))
  msgs$id <- as.integer(msgs$id)
  msgs[, c("id", setdiff(names(msgs), "id")), drop = FALSE]
}

#' @keywords internal
dta_to_tibble_if_available <- function(df, as_tibble = TRUE) {
  if (!isTRUE(as_tibble)) {
    return(df)
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  }

  df
}

#' @keywords internal
dta_columnspec_messages_to_df <- function(dataset_name, table_name, details) {
  full_error <- details$columnspec_errors$full_error

  if (is.null(full_error) || nrow(full_error) == 0) {
    return(dta_empty_messages())
  }

  row_values <- if ("row" %in% names(full_error)) full_error$row else rep(NA_real_, nrow(full_error))
  col_values <- if ("column" %in% names(full_error)) full_error$column else rep(NA_character_, nrow(full_error))
  key_values <- if ("keyword" %in% names(full_error)) full_error$keyword else rep(NA_character_, nrow(full_error))
  msg_values <- if ("message" %in% names(full_error)) full_error$message else rep("column spec validation error", nrow(full_error))

  data.frame(
    dataset = rep(dataset_name, nrow(full_error)),
    target = rep(table_name, nrow(full_error)),
    severity = rep("error", nrow(full_error)),
    source = rep("columnspec", nrow(full_error)),
    rule_id = rep(NA_character_, nrow(full_error)),
    row = suppressWarnings(as.numeric(row_values)),
    column = as.character(col_values),
    keyword = as.character(key_values),
    message = as.character(msg_values),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
dta_rule_messages_to_df <- function(dataset_name, table_name, details) {
  rule_errors <- details$rule_errors

  if (is.null(rule_errors) || length(rule_errors) == 0) {
    return(dta_empty_messages())
  }

  do.call(rbind, lapply(rule_errors, function(err) {
    data.frame(
      dataset = dataset_name,
      target = table_name,
      severity = "error",
      source = "rule",
      rule_id = if (!is.null(err$id)) as.character(err$id) else NA_character_,
      row = NA_real_,
      column = NA_character_,
      keyword = NA_character_,
      message = if (!is.null(err$message)) as.character(err$message) else "rule validation error",
      stringsAsFactors = FALSE
    )
  }))
}

#' @keywords internal
dta_import_error_messages <- function(import_errors) {
  if (!is.data.frame(import_errors) || nrow(import_errors) == 0) {
    return(character(0))
  }

  sprintf(
    "value '%s' in column '%s' cannot be represented as declared type '%s' (%s); imported as NA",
    as.character(import_errors$raw),
    as.character(import_errors$column),
    as.character(import_errors$declared_type),
    as.character(import_errors$reason)
  )
}

#' @title Import Messages for One Table
#' @description
#' Converts the import axis of one `validate_table_detailed()` result into
#' message rows.
#'
#' The returned frame carries exactly the same nine columns, in the same order,
#' as `dta_columnspec_messages_to_df()` and `dta_rule_messages_to_df()`. Two
#' populated frames with differing columns make the `rbind()` in
#' `dta_collect_messages_for_dataset()` error, so the raw offending value is
#' embedded in `message` rather than added as a column; the structured value
#' stays in `details$import_errors` and in `inspect()`.
#' @param dataset_name Character. Name of the dataset.
#' @param table_name Character. Name of the table.
#' @param details A list as returned by `validate_table_detailed()`.
#' @return A data.frame of messages, or the empty message frame.
#' @keywords internal
dta_import_messages_to_df <- function(dataset_name, table_name, details) {
  import_valid <- details$import_valid

  # Unknown (an artifact written before the import axis existed) is reported,
  # not assumed clean.
  if (is.null(import_valid) || is.na(import_valid)) {
    return(data.frame(
      dataset = dataset_name,
      target = table_name,
      severity = "warning",
      source = "import",
      rule_id = NA_character_,
      row = NA_real_,
      column = NA_character_,
      keyword = NA_character_,
      message = paste0(
        "validation artifact predates import checking (result_version 1); ",
        "re-run check(force = TRUE)"
      ),
      stringsAsFactors = FALSE
    ))
  }

  import_errors <- details$import_errors

  if (!is.data.frame(import_errors) || nrow(import_errors) == 0) {
    return(dta_empty_messages())
  }

  data.frame(
    dataset = rep(dataset_name, nrow(import_errors)),
    target = rep(table_name, nrow(import_errors)),
    severity = rep("error", nrow(import_errors)),
    source = rep("import", nrow(import_errors)),
    rule_id = rep(NA_character_, nrow(import_errors)),
    row = suppressWarnings(as.numeric(import_errors$row)),
    column = as.character(import_errors$column),
    keyword = as.character(import_errors$reason),
    message = dta_import_error_messages(import_errors),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
dta_collect_messages_for_dataset <- function(x, tables = NULL, source = c("auto", "memory", "artifact")) {
  source <- match.arg(source)
  target_tables <- dta_table_id_to_names(x, tables)

  out <- lapply(target_tables, function(table_name) {
    entry <- x@validation_index[[table_name]]
    if (is.null(entry)) {
      return(dta_empty_messages())
    }

    details <- validation_errors(x, table = table_name, source = source)

    dataset_name <- if (!is.null(x@name) && nzchar(x@name)) x@name else NA_character_

    schema_df <- dta_columnspec_messages_to_df(dataset_name, table_name, details)
    rule_df <- dta_rule_messages_to_df(dataset_name, table_name, details)
    import_df <- dta_import_messages_to_df(dataset_name, table_name, details)

    rbind(schema_df, rule_df, import_df)
  })

  if (length(out) == 0) {
    return(dta_empty_messages())
  }

  msgs <- do.call(rbind, out)
  if (is.null(msgs) || nrow(msgs) == 0) {
    return(dta_empty_messages())
  }

  msgs$row_order <- ifelse(is.na(msgs$row), Inf, msgs$row)
  msgs <- msgs[order(msgs$dataset, msgs$target, msgs$source, msgs$row_order), ]
  msgs$row_order <- NULL
  rownames(msgs) <- NULL
  dta_attach_message_ids(msgs)
}

#' @export
S7::method(messages, DTADataSetTabular) <- function(
  x,
  tables = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  out <- dta_collect_messages_for_dataset(x, tables = tables, source = source)
  dta_to_tibble_if_available(out, as_tibble = as_tibble)
}

#' @export
S7::method(messages, DTADataSet) <- function(
  x,
  tables = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  has_tables <- !is.null(tryCatch(x@tables, error = function(e) NULL))
  has_validation_index <- !is.null(tryCatch(x@validation_index, error = function(e) NULL))

  if (!has_tables || !has_validation_index) {
    cli::cli_abort(
      "messages() requires a DTADataSet subclass with table validation state."
    )
  }

  out <- dta_collect_messages_for_dataset(x, tables = tables, source = source)
  dta_to_tibble_if_available(out, as_tibble = as_tibble)
}

#' @export
S7::method(messages, DTA) <- function(
  x,
  datasets = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  source <- match.arg(source)

  if (is.null(x@datasets) || length(x@datasets) == 0) {
    cli::cli_abort("DTA object has no datasets.")
  }

  target_datasets <- if (is.null(datasets)) {
    names(x@datasets)
  } else if (is.numeric(datasets)) {
    if (any(datasets < 1) || any(datasets > length(x@datasets))) {
      cli::cli_abort("Dataset index out of bounds.")
    }
    names(x@datasets)[datasets]
  } else if (is.character(datasets)) {
    missing <- setdiff(datasets, names(x@datasets))
    if (length(missing) > 0) {
      cli::cli_abort("The following dataset{?s} not found: {.field {missing}}")
    }
    datasets
  } else {
    cli::cli_abort("'datasets' must be NULL, a character vector, or a numeric vector.")
  }

  out <- lapply(target_datasets, function(ds_name) {
    ds <- x@datasets[[ds_name]]
    if (!inherits(ds, "DTAtools::DTADataSet")) {
      return(dta_empty_messages())
    }

    ds_messages <- messages(
      ds,
      source = source,
      as_tibble = FALSE
    )

    if (nrow(ds_messages) == 0) {
      return(ds_messages)
    }

    ds_messages$dataset <- ds_name
    ds_messages
  })

  # Metadata import errors are DTA-level, not per-dataset, so they are folded
  # in here rather than by dta_collect_messages_for_dataset(). The frame has
  # the same columns in the same order, so the rbind below is unaffected.
  if (!is.null(x@metadata)) {
    out <- c(list(dta_metadata_messages_to_df(x@metadata)), out)
  }

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


#' @title Inspect Validation Messages
#' @description
#' Displays detailed, human-friendly diagnostics for one or more validation
#' messages identified by numeric \code{id} values from \code{messages()}.
#'
#' Methods are available for \code{DTA}, \code{DTADataSet},
#' \code{DTADataSetTabular}, and \code{DTADataSetFile}.
#' @param x A \code{DTA}, \code{DTADataSet}, \code{DTADataSetTabular}, or
#'   \code{DTADataSetFile} object.
#' @param ... Additional arguments:
#'   \describe{
#'     \item{id}{Optional integer message id(s) as shown by \code{messages()}.
#'       When omitted, all messages are inspected.}
#'     \item{source}{Character. One of \code{"auto"}, \code{"memory"}, or
#'       \code{"artifact"}.}
#'     \item{as_tibble}{Logical. When \code{TRUE} and package \code{tibble} is
#'       installed, returns a tibble. Otherwise returns a base data.frame.}
#'   }
#' @return A tibble/data.frame with one or more rows per requested id.
#'   Detail fields are appended as flat columns prefixed with
#'   \code{schema_}, \code{context_}, and \code{failing_}.
#' @usage inspect(x, ...)
#' @name inspect
#' @export
inspect <- S7::new_generic("inspect", "x")

#' @keywords internal
dta_validate_inspect_ids <- function(id) {
  if (!is.numeric(id) || length(id) < 1 || any(is.na(id)) || any(id < 1)) {
    cli::cli_abort("'id' must be one or more positive numeric values.")
  }

  as.integer(id)
}

#' @keywords internal
dta_get_message_rows_by_id <- function(msgs, id) {
  ids <- if (missing(id) || is.null(id) || length(id) == 0) {
    as.integer(msgs$id)
  } else {
    dta_validate_inspect_ids(id)
  }

  if (length(ids) == 0) {
    return(msgs[0, , drop = FALSE])
  }

  missing_ids <- setdiff(unique(ids), msgs$id)
  if (length(missing_ids) > 0) {
    cli::cli_abort("Message id{?s} not found: {.field {missing_ids}}")
  }

  rows <- lapply(ids, function(one_id) {
    hit <- msgs[msgs$id == one_id, , drop = FALSE]
    hit[1, , drop = FALSE]
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @keywords internal
dta_recycle_df_rows <- function(df, n) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(row.names = seq_len(n)))
  }

  idx <- rep(seq_len(nrow(df)), length.out = n)
  out <- df[idx, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @keywords internal
dta_rbind_fill <- function(dfs) {
  if (length(dfs) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  all_names <- unique(unlist(lapply(dfs, names), use.names = FALSE))
  filled <- lapply(dfs, function(df) {
    missing <- setdiff(all_names, names(df))
    for (nm in missing) {
      df[[nm]] <- NA
    }
    df <- df[, all_names, drop = FALSE]
    rownames(df) <- NULL
    df
  })

  out <- do.call(rbind, filled)
  rownames(out) <- NULL
  out
}

#' @keywords internal
dta_value_to_text <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  if (length(x) == 0) {
    return(NA_character_)
  }

  if (is.atomic(x) && length(x) == 1) {
    return(as.character(x))
  }

  if (is.atomic(x)) {
    return(paste(as.character(x), collapse = ","))
  }

  if (is.data.frame(x)) {
    return(paste(utils::capture.output(utils::str(x, give.attr = FALSE)), collapse = " "))
  }

  paste(utils::capture.output(utils::str(x, give.attr = FALSE)), collapse = " ")
}

#' @keywords internal
dta_group_violations_to_df <- function(violations) {
  if (is.null(violations) || length(violations) == 0) {
    return(NULL)
  }
  data.frame(
    group = vapply(violations, function(v) as.character(v$group %||% ""), character(1)),
    constraint = vapply(violations, function(v) as.character(v$constraint_id %||% ""), character(1)),
    message = vapply(violations, function(v) as.character(v$message %||% ""), character(1)),
    rows = vapply(violations, function(v) {
      r <- v$rows
      if (length(r) == 0) {
        return("")
      }
      formatted <- dta_format_group_rows(sort(unique(as.integer(r))), length(r), 30L)
      # For streaming results, v$rows holds only a head; flag truncated output.
      if (isTRUE(v$rows_truncated)) {
        paste0(formatted, " (+more)")
      } else {
        formatted
      }
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
dta_simplify_df_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }

  out <- lapply(df, function(col) {
    is_simple_atomic <- is.atomic(col) && is.null(dim(col))
    if (is_simple_atomic) {
      return(col)
    }

    vapply(as.list(col), dta_value_to_text, character(1))
  })

  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  names(out) <- names(df)
  rownames(out) <- NULL
  out
}

#' @keywords internal
dta_flatten_inspect_record <- function(record) {
  detail_sources <- list(
    columnspec = record$columnspec_matches,
    context = record$row_context,
    failing = record$failing_rows_preview,
    import = record$import_matches,
    group_violation = dta_group_violations_to_df(record$group_violation_details)
  )

  detail_rows <- vapply(detail_sources, function(x) {
    if (is.data.frame(x)) nrow(x) else 0L
  }, integer(1))

  out_n <- max(1L, detail_rows)

  base_names <- setdiff(names(record), c("columnspec_matches", "row_context", "failing_rows_preview", "import_matches", "rule_definition", "details", "group_violation_details"))
  base <- as.data.frame(lapply(record[base_names], function(x) {
    if (is.null(x)) NA else x
  }), stringsAsFactors = FALSE, optional = TRUE)
  base <- base[rep(1L, out_n), , drop = FALSE]
  rownames(base) <- NULL

  detail_df <- lapply(names(detail_sources), function(prefix) {
    df <- detail_sources[[prefix]]
    if (!is.data.frame(df) || ncol(df) == 0) {
      return(data.frame(row.names = seq_len(out_n)))
    }

    df <- dta_simplify_df_columns(df)
    df <- dta_recycle_df_rows(df, out_n)
    names(df) <- paste0(prefix, "_", names(df))
    df
  })

  extra <- data.frame(row.names = seq_len(out_n))
  if (!is.null(record$rule_definition)) {
    extra$rule_definition_class <- paste(class(record$rule_definition), collapse = ",")
  }
  if (is.list(record$details)) {
    details <- record$details
    if (!is.null(details$ok)) extra$details_ok <- isTRUE(details$ok)
    if (!is.null(details$columnspec_valid)) extra$details_columnspec_valid <- isTRUE(details$columnspec_valid)
    if (!is.null(details$rules_valid)) extra$details_rules_valid <- isTRUE(details$rules_valid)
    # NA ("unknown", a pre-import-axis artifact) must stay NA here: isTRUE()
    # would report it as FALSE and invent a failure that was never observed.
    if (!is.null(details$import_valid)) {
      extra$details_import_valid <- if (is.na(details$import_valid)) NA else isTRUE(details$import_valid)
    }
    if (!is.null(details$n_columnspec_errors)) extra$details_n_columnspec_errors <- as.integer(details$n_columnspec_errors)
    if (!is.null(details$n_rule_errors)) extra$details_n_rule_errors <- as.integer(details$n_rule_errors)
    if (!is.null(details$n_import_errors)) extra$details_n_import_errors <- as.integer(details$n_import_errors)
  }
  if (ncol(extra) > 0) {
    extra <- extra[rep(1L, out_n), , drop = FALSE]
  }

  out <- cbind(base, do.call(cbind, detail_df), extra)
  rownames(out) <- NULL
  out
}

#' @keywords internal
dta_inspect_records_to_df <- function(records) {
  if (length(records) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  flattened <- lapply(records, dta_flatten_inspect_record)
  dta_rbind_fill(flattened)
}

#' @keywords internal
dta_rule_failure_row_indices <- function(rule, df) {
  if (inherits(rule, "DTAtools::DTARuleColRange")) {
    col <- rule@columns[[1]]
    if (!col %in% names(df)) {
      return(integer(0))
    }

    lower <- if (length(rule@min) > 0) rule@min[1] else -Inf
    upper <- if (length(rule@max) > 0) rule@max[1] else Inf
    # This must reproduce rule_check_range() exactly. The two carried the same
    # defect independently -- `as.numeric()` on a factor read its level codes,
    # and `!is.na(values)` waved an unconvertible value through -- and any
    # divergence makes messages() report N violated rows while inspect() shows
    # failing_row_count = 0.
    converted <- dta_as_numeric_strict(df[[col]])
    mask <- ((converted$values < lower | converted$values > upper) %in% TRUE) |
      converted$unconvertible
    return(which(mask))
  }

  if (inherits(rule, "DTAtools::DTARuleColUnique")) {
    cols <- rule@columns
    if (!all(cols %in% names(df))) {
      return(integer(0))
    }

    dup_mask <- duplicated(df[, cols, drop = FALSE]) | duplicated(df[, cols, drop = FALSE], fromLast = TRUE)
    return(which(dup_mask))
  }

  if (inherits(rule, "DTAtools::DTARuleColCondition")) {
    # Deliberately the same call rule_check_col_condition() makes, so the row
    # preview and the reported violation count can never disagree.
    return(which(dta_condition_violated(rule, df) %in% TRUE))
  }

  if (inherits(rule, "DTAtools::DTARuleGroupCondition")) {
    # A grouped constraint fails for a *group*, not for a row, so the rows come
    # from the violation records the check itself built. Re-running the check is
    # what keeps the two in step; there is no cheaper mask to reproduce.
    result <- tryCatch(
      rule_check_group_condition(rule, df),
      dta_rule_not_applicable = function(cnd) NULL
    )
    if (is.null(result) || isTRUE(result$valid)) {
      return(integer(0))
    }

    rows <- unlist(lapply(result$details, function(v) v$rows), use.names = FALSE)
    return(sort(unique(as.integer(rows))))
  }

  integer(0)
}

#' @keywords internal
dta_build_inspect_row_context <- function(table_df, msg_row) {
  if (is.na(msg_row$row) || msg_row$row < 1 || msg_row$row > nrow(table_df)) {
    return(NULL)
  }

  useful_cols <- c("SUBJECT_ID", "VISIT", msg_row$column)
  useful_cols <- useful_cols[useful_cols %in% names(table_df)]
  if (length(useful_cols) == 0) {
    useful_cols <- names(table_df)[seq_len(min(6, ncol(table_df)))]
  }

  out <- table_df[msg_row$row, useful_cols, drop = FALSE]
  out$.row <- msg_row$row
  out[, c(".row", setdiff(names(out), ".row")), drop = FALSE]
}

#' @keywords internal
dta_filter_columnspec_matches <- function(columnspec_full, msg_row) {
  if (is.null(columnspec_full) || nrow(columnspec_full) == 0) {
    return(columnspec_full)
  }

  columnspec_full <- as.data.frame(columnspec_full)
  work <- columnspec_full

  apply_filter <- function(df, field, value) {
    if (is.null(df) || nrow(df) == 0 || !field %in% names(df)) {
      return(df)
    }

    if (is.numeric(value)) {
      return(df[!is.na(df[[field]]) & df[[field]] == value, , drop = FALSE])
    }

    value_chr <- as.character(value)
    if (length(value_chr) == 0 || is.na(value_chr) || !nzchar(value_chr)) {
      return(df)
    }

    field_chr <- as.character(df[[field]])
    df[!is.na(field_chr) & field_chr == value_chr, , drop = FALSE]
  }

  candidates <- list(
    list(field = "row", value = suppressWarnings(as.numeric(msg_row$row))),
    list(field = "column", value = msg_row$column),
    list(field = "keyword", value = msg_row$keyword),
    list(field = "message", value = msg_row$message)
  )

  for (cand in candidates) {
    next_work <- apply_filter(work, cand$field, cand$value)
    if (nrow(next_work) > 0) {
      work <- next_work
    }
  }

  if (nrow(work) > 0) {
    return(work)
  }

  key_work <- columnspec_full
  key_candidates <- list(
    list(field = "keyword", value = msg_row$keyword),
    list(field = "message", value = msg_row$message)
  )
  for (cand in key_candidates) {
    next_work <- apply_filter(key_work, cand$field, cand$value)
    if (nrow(next_work) > 0) {
      key_work <- next_work
    }
  }

  if (nrow(key_work) > 0) {
    return(key_work)
  }

  row_work <- apply_filter(columnspec_full, "row", suppressWarnings(as.numeric(msg_row$row)))
  if (nrow(row_work) > 0) {
    return(row_work)
  }

  columnspec_full
}

#' @keywords internal
dta_filter_import_matches <- function(import_errors, msg_row) {
  if (!is.data.frame(import_errors) || nrow(import_errors) == 0) {
    return(NULL)
  }

  keep <- rep(TRUE, nrow(import_errors))

  msg_row_no <- suppressWarnings(as.numeric(msg_row$row))
  if (length(msg_row_no) == 1 && !is.na(msg_row_no) && "row" %in% names(import_errors)) {
    keep <- keep & suppressWarnings(as.numeric(import_errors$row)) == msg_row_no
  }

  msg_col <- as.character(msg_row$column)
  if (length(msg_col) == 1 && !is.na(msg_col) && "column" %in% names(import_errors)) {
    keep <- keep & as.character(import_errors$column) == msg_col
  }

  hits <- import_errors[keep %in% TRUE, , drop = FALSE]
  rownames(hits) <- NULL

  if (nrow(hits) == 0) {
    return(import_errors)
  }

  hits
}

#' @keywords internal
dta_inspect_tabular_message <- function(x, msg_row, source = c("auto", "memory", "artifact")) {
  source <- match.arg(source)
  table_name <- as.character(msg_row$target)
  details <- validation_errors(x, table = table_name, source = source)
  table_df <- as.data.frame(x@tables[[table_name]])

  out <- list(
    id = as.integer(msg_row$id),
    dataset = as.character(msg_row$dataset),
    target = table_name,
    source = as.character(msg_row$source),
    severity = as.character(msg_row$severity),
    headline = sprintf("[%s/%s] %s", msg_row$dataset, table_name, msg_row$message),
    message = as.character(msg_row$message)
  )

  if (identical(as.character(msg_row$source), "columnspec")) {
    columnspec_full <- details$columnspec_errors$full_error
    schema_match <- dta_filter_columnspec_matches(columnspec_full, msg_row)

    out$type <- "columnspec"
    out$why <- "Column values violate JSON schema constraints (type/value/length/required)."
    out$row_context <- dta_build_inspect_row_context(table_df, msg_row)
    out$columnspec_matches <- utils::head(schema_match, 20)
    return(out)
  }

  # Without an explicit branch an import message would fall through to the rule
  # branch below, look up rule_id = NA, and return a nonsense record.
  if (identical(as.character(msg_row$source), "import")) {
    out$type <- "import"
    out$why <- paste(
      "A value could not be represented in the column's declared type,",
      "so the typed column holds NA and the raw value was kept."
    )
    out$row_context <- dta_build_inspect_row_context(table_df, msg_row)
    out$import_matches <- utils::head(
      dta_filter_import_matches(details$import_errors, msg_row),
      20
    )
    return(out)
  }

  rule_id <- as.character(msg_row$rule_id)
  rules_list <- tryCatch(x@specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }
  rule_idx <- which(vapply(rules_list, function(r) identical(r@id, rule_id), logical(1)))
  rule_def <- if (length(rule_idx) > 0) rules_list[[rule_idx[[1]]]] else NULL
  failing_rows <- if (!is.null(rule_def)) dta_rule_failure_row_indices(rule_def, table_df) else integer(0)

  row_preview <- NULL
  group_violation_details <- NULL

  is_group_condition <- !is.null(rule_def) && inherits(rule_def, "DTAtools::DTARuleGroupCondition")

  if (length(failing_rows) > 0) {
    # Group condition rules show ALL failing rows and include all involved columns.
    preview_rows <- if (is_group_condition) failing_rows else utils::head(failing_rows, 10)

    if (is_group_condition) {
      preview_cols <- rule_def@group_by %||% c("SUBJECT_ID", "VISIT")
      # Add all columns referenced in any condition.
      cond_cols <- unique(unlist(lapply(rule_def@conditions, names), use.names = FALSE))
      preview_cols <- c(preview_cols, cond_cols)
    } else {
      preview_cols <- c("SUBJECT_ID", "VISIT")
      if (!is.null(rule_def) && inherits(rule_def, "DTAtools::DTARuleColRange")) {
        preview_cols <- c(preview_cols, rule_def@columns[[1]])
      } else if (!is.null(rule_def) && inherits(rule_def, "DTAtools::DTARuleColUnique")) {
        preview_cols <- c(preview_cols, rule_def@columns)
      } else if (!is.null(rule_def) && inherits(rule_def, "DTAtools::DTARuleColCondition")) {
        preview_cols <- c(preview_cols, names(rule_def@condition), names(rule_def@then))
      }
    }

    preview_cols <- unique(preview_cols[preview_cols %in% names(table_df)])
    if (length(preview_cols) == 0) {
      preview_cols <- names(table_df)[seq_len(min(6, ncol(table_df)))]
    }

    row_preview <- table_df[preview_rows, preview_cols, drop = FALSE]
    row_preview$.row <- preview_rows
    row_preview <- row_preview[, c(".row", setdiff(names(row_preview), ".row")), drop = FALSE]
  }

  # For group condition rules, also store per-violation structured detail so the
  # Shiny inspect view can render a richer breakdown by group/constraint.
  if (is_group_condition) {
    gc_result <- tryCatch(
      rule_check_group_condition(rule_def, table_df),
      error = function(e) NULL
    )
    if (!is.null(gc_result) && !isTRUE(gc_result$valid)) {
      group_violation_details <- gc_result$details
    }
  }

  out$type <- "rule"
  out$why <- if (is_group_condition) {
    "A group-level constraint was violated: within one or more groups of rows, conditions that must be mutually exclusive co-occur, or a required follow-on condition is absent."
  } else {
    "Rule logic found rows that violate IF/THEN, range, or uniqueness constraints."
  }
  out$rule_id <- rule_id
  out$rule_definition <- rule_def
  out$failing_row_count <- length(failing_rows)
  out$failing_rows_preview <- row_preview
  out$group_violation_details <- group_violation_details
  out
}

#' @export
S7::method(inspect, DTADataSetTabular) <- function(
  x,
  id = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  source <- match.arg(source)
  msgs <- messages(x, source = source, as_tibble = FALSE)
  msg_rows <- dta_get_message_rows_by_id(msgs, id)

  records <- lapply(seq_len(nrow(msg_rows)), function(i) {
    dta_inspect_tabular_message(x, msg_rows[i, , drop = FALSE], source = source)
  })

  out <- dta_inspect_records_to_df(records)
  dta_to_tibble_if_available(out, as_tibble = as_tibble)
}

#' @export
S7::method(inspect, DTADataSet) <- function(
  x,
  id = NULL,
  source = c("auto", "memory", "artifact"),
  as_tibble = TRUE
) {
  if (inherits(x, "DTAtools::DTADataSetTabular")) {
    return(S7::method(inspect, DTADataSetTabular)(x, id = id, source = source, as_tibble = as_tibble))
  }

  if (inherits(x, "DTAtools::DTADataSetFile")) {
    return(S7::method(inspect, DTADataSetFile)(x, id = id, source = source, as_tibble = as_tibble))
  }

  cli::cli_abort("inspect() is currently implemented for DTADataSetTabular and DTADataSetFile subclasses.")
}

#' @export
S7::method(inspect, DTA) <- function(
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
    dataset_name <- as.character(msg_row$dataset)
    ds <- x@datasets[[dataset_name]]
    if (is.null(ds) || !inherits(ds, "DTAtools::DTADataSet")) {
      cli::cli_abort("Dataset '{dataset_name}' not found in DTA object.")
    }

    if (inherits(ds, "DTAtools::DTADataSetTabular")) {
      return(dta_inspect_tabular_message(ds, msg_row, source = source))
    }

    if (inherits(ds, "DTAtools::DTADataSetFile")) {
      target_name <- as.character(msg_row$target)
      details <- ds@validation_store[[target_name]]

      return(list(
        id = as.integer(msg_row$id),
        dataset = dataset_name,
        target = target_name,
        source = as.character(msg_row$source),
        severity = as.character(msg_row$severity),
        type = "rule",
        headline = sprintf("[%s/%s] %s", dataset_name, target_name, msg_row$message),
        why = "File-level rule checks file presence/readability/non-empty constraints.",
        message = as.character(msg_row$message),
        rule_id = as.character(msg_row$rule_id),
        details = details
      ))
    }

    cli::cli_abort("inspect() is currently implemented for DTADataSetTabular and DTADataSetFile datasets.")
  })

  out <- dta_inspect_records_to_df(records)
  dta_to_tibble_if_available(out, as_tibble = as_tibble)
}
