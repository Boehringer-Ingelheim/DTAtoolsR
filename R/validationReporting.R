#' @title Retrieve Check Results
#' @description
#' Returns a programmatic summary of the latest validation/check state.
#' Unlike `check()`, this function is read-only and does not perform validation.
#'
#' Methods are available for `DTA`, `DTADataSet`, and `DTADataSetTabular`.
#' @param x A `DTA`, `DTADataSet`, or `DTADataSetTabular` object.
#' @param ... Method arguments. The `DTADataSet` and `DTADataSetTabular`
#'   methods take `tables` (names or indices of the tables to report on;
#'   `NULL` for all). The `DTA` method takes `datasets` (`NULL`, dataset
#'   names, or indices).
#' @return
#' A data.frame with one row per validation target (table), with columns:
#' `dataset`, `target`, `target_type`, `status`, `validated_at`, `run_id`,
#' `validation_run`, `n_columnspec_errors`, `n_rule_errors`,
#' `n_import_errors`, `n_targets`, `n_validated`, `n_valid`, `n_invalid`,
#' `n_skipped`, `n_not_validated`.
#'
#' The `n_targets` ... `n_not_validated` columns are counts over all
#' targets of the dataset and repeat on every row of that dataset; they are
#' not aggregated across datasets. For a `DTA`, the per-dataset results are
#' row-bound. A dataset that is not a `DTADataSet` contributes a single row
#' with status `"not_validated"` and `NA` in the other columns.
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
  dataset_name <- if (!is.null(x@id) && nzchar(x@id)) x@id else NA_character_
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
  dataset_name <- if (!is.null(x@id) && nzchar(x@id)) x@id else NA_character_
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
#' The output combines column spec, rule and import failures into a single
#' flat table.
#'
#' Methods are available for `DTA`, `DTADataSet`, and `DTADataSetTabular`.
#' @param x A `DTA`, `DTADataSet`, or `DTADataSetTabular` object.
#' @param ... Method arguments:
#'   * `tables` (`DTADataSet` and `DTADataSetTabular` methods): names or
#'     indices of the tables to report on; `NULL` for all.
#'   * `datasets` (`DTA` method): `NULL`, dataset names, or indices.
#'   * `source`: where validation details are read from, one of `"auto"`
#'     (the default), `"memory"` or `"artifact"`.
#'   * `as_tibble`: logical, default `TRUE`. Return a tibble when the
#'     `tibble` package is installed.
#' @return
#' A data.frame (or tibble when `as_tibble = TRUE` and package `tibble`
#' is installed) with columns:
#' `id` (sequential integer), `dataset`, `target`, `severity`, `source`
#' (`"columnspec"`, `"rule"` or `"import"`), `rule_id`,
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

    dataset_name <- if (!is.null(x@id) && nzchar(x@id)) x@id else NA_character_

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
#'   \code{columnspec_}, \code{context_}, \code{failing_}, \code{import_}
#'   and \code{group_violation_}. Summary fields of the validation result are
#'   added as \code{details_*} columns, and the class of the rule behind a
#'   rule message as \code{rule_definition_class}.
#' @seealso \code{\link{explain_messages}} for the same findings in plain language.
#' @usage inspect(x, ...)
#' @examples
#' ds <- check(create_example_DTADataSetTabular(2), quiet = TRUE)
#'
#' # Every message, numbered, then one of them in full.
#' messages(ds)
#' inspect(ds, id = 1)
#' @name inspect
#' @export
inspect <- S7::new_generic("inspect", "x")

# Set while check(<DTA>) runs its datasets, so they do not each repeat the
# explain_messages() hint that the DTA summary prints once.
dta_check_state <- new.env(parent = emptyenv())

#' @keywords internal
#' @title Find the Rule Definition Behind a Rule Message
#' @param x A `DTA` (the rule is looked up in `x@datasets[[dataset_name]]`) or a
#'   `DTADataSet` (used as is).
#' @param dataset_name Dataset name as reported by `messages()`.
#' @param rule_id The rule's `id`.
#' @return The rule object, or `NULL` when it cannot be resolved.
dta_find_rule_def <- function(x, dataset_name, rule_id) {
  tryCatch(
    {
      ds <- if (inherits(x, "DTAtools::DTA")) x@datasets[[dataset_name]] else x
      if (!is.null(ds) && !is.null(ds@specs) && !is.null(ds@specs@rules)) {
        for (r in ds@specs@rules) {
          if (!is.null(r@id) && identical(r@id, rule_id)) {
            return(r)
          }
        }
      }
      NULL
    },
    error = function(e) NULL
  )
}

#' @title Explain Validation Messages in Plain Language
#' @description
#' Prints, for every validation message, what was checked, what the value
#' should have been and what it actually was, in the same words the HTML report
#' from \code{\link{write_validation_report}} uses. Where \code{\link{messages}}
#' gives the validator's generic wording ("must be equal to one of the allowed
#' values") and \code{\link{inspect}} a wide technical table, this names the
#' allowed values and shows the offending one.
#' @param x A checked \code{DTA} or \code{DTADataSet} (anything
#'   \code{messages()} and \code{inspect()} accept).
#' @param id Optional integer message id(s) as shown by \code{messages()}.
#'   Default: all messages.
#' @param n Maximum number of messages to print. Use \code{Inf} for all.
#' @return Invisibly, a data.frame with one row per explained message and the
#'   columns \code{id}, \code{dataset}, \code{target}, \code{source},
#'   \code{row}, \code{column}, \code{problem}, \code{should_be} and
#'   \code{actual}.
#' @seealso \code{\link{messages}}, \code{\link{inspect}},
#'   \code{\link{write_validation_report}}
#' @examples
#' ds <- check(create_example_DTADataSetTabular(2), quiet = TRUE)
#' explain_messages(ds, n = 3)
#' @export
explain_messages <- function(x, id = NULL, n = 20) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    cli::cli_abort("'n' must be a single number >= 1 (use Inf for all messages).")
  }

  msgs <- messages(x, as_tibble = FALSE)
  if (!is.null(id)) {
    msgs <- dta_get_message_rows_by_id(msgs, id)
  }
  out <- dta_explain_empty()
  if (nrow(msgs) == 0) {
    cli::cli_alert_success("No validation messages.")
    return(invisible(out))
  }

  n_more <- max(nrow(msgs) - n, 0)
  msgs <- msgs[seq_len(min(nrow(msgs), n)), , drop = FALSE]

  # One batched call, as the report does: inspect() re-collects every message
  # on each invocation. Failure falls back to the plain messages() row.
  inspected <- tryCatch(
    inspect(x, id = msgs$id, as_tibble = FALSE),
    error = function(e) NULL
  )
  by_id <- if (!is.null(inspected) && nrow(inspected) > 0) {
    split(seq_len(nrow(inspected)), inspected$id)
  }

  rows <- lapply(seq_len(nrow(msgs)), function(i) {
    msg_row <- msgs[i, , drop = FALSE]
    idx <- by_id[[as.character(msg_row$id)]]
    detail <- if (is.null(idx)) msg_row else inspected[idx, , drop = FALSE]
    rule_def <- NULL
    if (identical(as.character(msg_row$source), "rule") && !is.na(msg_row$rule_id)) {
      rule_def <- dta_find_rule_def(x, as.character(msg_row$dataset), as.character(msg_row$rule_id))
    }
    dta_explain_one(msg_row, detail, rule_def)
  })

  if (n_more > 0) {
    cli::cli_alert_info("... and {n_more} more message{?s}; use {.code n = Inf} to show all.")
  }

  invisible(do.call(rbind, rows))
}

#' @keywords internal
dta_explain_empty <- function() {
  data.frame(
    id = integer(0), dataset = character(0), target = character(0),
    source = character(0), row = numeric(0), column = character(0),
    problem = character(0), should_be = character(0), actual = character(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @title Offending Rows as Printed Lines
#' @param detail `inspect()` rows of one message.
#' @param prefix_pattern Regex selecting the columns to show (`^failing_`,
#'   `^context_` or `^group_violation_`).
#' @param max_rows Maximum number of rows to print.
#' @return Character vector of printed lines, or `NULL` when nothing was captured.
dta_explain_rows_lines <- function(detail, prefix_pattern, max_rows = 5) {
  cols <- grep(prefix_pattern, names(detail), value = TRUE)
  if (length(cols) == 0) {
    return(NULL)
  }
  sub <- unique(detail[, cols, drop = FALSE])
  names(sub) <- sub(prefix_pattern, "", names(sub))
  names(sub)[names(sub) == ".row"] <- "Row"
  sub <- sub[rowSums(!is.na(sub)) > 0, , drop = FALSE]
  if (nrow(sub) == 0) {
    return(NULL)
  }
  if ("Row" %in% names(sub)) {
    sub <- sub[, c("Row", setdiff(names(sub), "Row")), drop = FALSE]
  }
  shown <- utils::head(sub, max_rows)
  lines <- utils::capture.output(print(shown, row.names = FALSE))
  if (nrow(sub) > nrow(shown)) {
    lines <- c(lines, paste0("... ", nrow(sub) - nrow(shown), " more row(s)"))
  }
  lines
}

#' @keywords internal
#' @title Print and Return the Explanation of One Message
#' @description
#' Mirrors the type branching of `.report_inspect_panel_html()`, with
#' `html = FALSE`. Message text is only ever interpolated through local
#' variables: cli parses `{}` in the format string it is handed, so pasting a
#' data or spec value into it would evaluate that value as code.
#' @param msg_row One row of `messages()`.
#' @param detail `inspect()` rows for the same id (or `msg_row` as fallback).
#' @param rule_def Rule object or `NULL`.
#' @return A one-row data.frame as described in `explain_messages()`.
dta_explain_one <- function(msg_row, detail, rule_def) {
  row1 <- detail[1, , drop = FALSE]
  type <- if ("type" %in% names(row1) && !is.na(row1[["type"]])) {
    as.character(row1[["type"]])
  } else {
    as.character(msg_row$source)
  }
  label <- switch(type,
    rule = "Rule failure",
    columnspec = "Column spec violation",
    import = "Import error",
    "Message"
  )
  if (identical(type, "rule") && !is.na(msg_row$rule_id)) {
    label <- paste0(label, " (", msg_row$rule_id, ")")
  }

  row_no <- suppressWarnings(as.numeric(msg_row$row))
  col_nm <- as.character(msg_row$column)
  where <- c(
    if (!is.na(row_no)) paste0("Row ", format(row_no, scientific = FALSE)),
    if (!is.na(col_nm)) paste0("column ", col_nm)
  )
  problem <- as.character(msg_row$message)
  problem_line <- if (length(where) > 0) {
    paste0(paste(where, collapse = ", "), ": ", problem)
  } else {
    problem
  }

  info <- character(0)
  table_lines <- NULL
  if (identical(type, "columnspec")) {
    # inspect() carries no columnspec_* detail for an undeclared column; the
    # messages() row still knows the keyword, and the sentences key off it.
    if (!"columnspec_keyword" %in% names(row1) || is.na(row1[["columnspec_keyword"]])) {
      row1[["columnspec_keyword"]] <- as.character(msg_row$keyword)
    }
    should_be <- .report_columnspec_expected_text(row1, html = FALSE)
    actual <- .report_columnspec_actual_text(row1, html = FALSE)
  } else if (identical(type, "import")) {
    should_be <- .report_import_expected_text(row1, html = FALSE)
    actual <- .report_import_actual_text(row1, html = FALSE)
  } else if (identical(type, "rule")) {
    desc <- tryCatch(rule_def@description, error = function(e) NULL)
    if (!is.null(desc) && length(desc) == 1 && !is.na(desc) && nzchar(desc)) {
      info <- paste0("What this rule checks: ", desc)
    }
    if (inherits(rule_def, "DTAtools::DTARuleGroupCondition")) {
      should_be <- .report_group_condition_expected_text(rule_def, html = FALSE)
      info <- c(info, vapply(
        rule_def@constraints,
        function(cn) .report_group_constraint_desc(cn, html = FALSE),
        character(1)
      ))
      table_lines <- c(
        dta_explain_rows_lines(detail, "^group_violation_"),
        dta_explain_rows_lines(detail, "^failing_")
      )
    } else {
      should_be <- .report_rule_expected_text(rule_def, html = FALSE)
      pattern <- if (any(grepl("^failing_", names(detail)))) "^failing_" else "^context_"
      table_lines <- dta_explain_rows_lines(detail, pattern)
    }
    actual <- if (is.null(table_lines)) {
      "No offending rows were captured."
    } else {
      paste(table_lines, collapse = "\n")
    }
  } else {
    should_be <- problem
    actual <- "(see technical detail via inspect())"
  }

  header <- paste0(
    "#", msg_row$id, " | ", msg_row$dataset, " / ", msg_row$target, " | ", label
  )
  cli::cli_rule(left = "{header}")
  cli::cli_text("{problem_line}")
  for (line in info) {
    cli::cli_alert_info("{line}")
  }
  cli::cli_alert_success("Should be: {should_be}")
  if (is.null(table_lines)) {
    cli::cli_alert_danger("Actual: {actual}")
  } else {
    cli::cli_alert_danger("Actual:")
    cli::cli_verbatim(table_lines)
  }

  data.frame(
    id = as.integer(msg_row$id),
    dataset = as.character(msg_row$dataset),
    target = as.character(msg_row$target),
    source = as.character(msg_row$source),
    row = row_no,
    column = col_nm,
    problem = problem,
    should_be = should_be,
    actual = actual,
    stringsAsFactors = FALSE
  )
}

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
    # dta_narrow_count(), not as.integer(): these two counts are uncapped, so a
    # dirty enough file pushes them past the integer range, and coercing there
    # would put back the `NA` the counters were widened to avoid -- losing the
    # count in `inspect()` output rather than in the verdict. `n_rule_errors` is
    # a count of failing rules, bounded by the spec, so it stays an integer.
    if (!is.null(details$n_columnspec_errors)) extra$details_n_columnspec_errors <- dta_narrow_count(details$n_columnspec_errors)
    if (!is.null(details$n_rule_errors)) extra$details_n_rule_errors <- as.integer(details$n_rule_errors)
    if (!is.null(details$n_import_errors)) extra$details_n_import_errors <- dta_narrow_count(details$n_import_errors)
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

#' @title Table Frame for Inspection, Bounded and Column-Limited
#' @description
#' `dta_inspect_tabular_message()` used to call
#' `as.data.frame(x@tables[[table_name]])` unconditionally, which for a
#' streamed table materialises the whole delivery -- everything `check()` was
#' streaming specifically to avoid -- just to show one row of context. This
#' collects only what a preview actually needs: a column subset and,
#' when supplied, the first `max_row` rows.
#'
#' A table already in memory is untouched: `as.data.frame()`, exactly as
#' before. A `RecordBatchReader` cannot be safely re-read -- it is one-shot,
#' and the scan `check()` ran may already have consumed it -- so this returns
#' `NULL` rather than reading (and thereby destroying) whatever is left of it.
#' Any other lazy source (`Dataset`, `arrow_dplyr_query`) is scanned fresh,
#' which is safe to repeat, through the same ordered record-batch reader the
#' validation scan numbers its rows from.
#' @param tbl A table representation, as held in `x@tables[[table_name]]`.
#' @param columns Character, or `NULL` for every column.
#' @param max_row Numeric, or `NULL`/non-finite for no row bound. When finite
#'   and positive, only the first `max_row` rows are read, so row `k` of the
#'   returned frame stays row `k` of the underlying table.
#' @return A data.frame, or `NULL` when `tbl` cannot be safely re-read.
#' @keywords internal
dta_inspect_table_frame <- function(tbl, columns = NULL, max_row = NULL) {
  if (!dta_table_is_lazy(tbl)) {
    return(as.data.frame(tbl))
  }

  if (inherits(tbl, "RecordBatchReader")) {
    return(NULL)
  }

  query <- tbl
  if (!is.null(columns) && length(columns) > 0) {
    # dta_build_inspect_row_context() falls back to the first six columns when
    # none of the ones it wants exist; select those here so a lazy table gets
    # the same fallback rather than an empty frame.
    available <- names(tbl)
    if (!any(columns %in% available)) {
      columns <- available[seq_len(min(6, length(available)))]
    }
    query <- dplyr::select(query, dplyr::any_of(columns))
  }
  # Read through the same ordered Scanner -> RecordBatchReader the validation
  # engine numbers its rows from (dta_validate_table_stream()), so row `k`
  # here is the row `k` a message reports. `head()` on a Dataset promises no
  # order across batches, so it was a different reader that merely agreed
  # on small inputs. The bounded case stops at the first `max_row` rows.
  reader <- arrow::Scanner$create(query)$ToRecordBatchReader()
  if (is.null(max_row) || !is.finite(max_row) || max_row <= 0) {
    return(as.data.frame(as.data.frame(reader$read_table())))
  }

  batches <- list()
  n <- 0
  while (n < max_row) {
    batch <- reader$read_next_batch()
    if (is.null(batch)) {
      break
    }
    batches[[length(batches) + 1L]] <- as.data.frame(batch)
    n <- n + nrow(batches[[length(batches)]])
  }
  if (length(batches) == 0) {
    # Nothing to read: the empty table still carries the columns.
    return(as.data.frame(as.data.frame(reader$read_table())))
  }
  out <- as.data.frame(do.call(rbind, batches))
  out <- out[seq_len(min(nrow(out), max_row)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @title Columns a Rule's Inspection Needs
#' @description
#' The columns `dta_inspect_tabular_message()` must load to recompute a rule's
#' failing rows and build its preview: the rule's own target column(s) (a
#' group condition's `group_by` and condition columns for that rule type),
#' plus `SUBJECT_ID`/`VISIT`, which the preview always tries to show for
#' context. Requesting only these keeps a lazily-held table from being pulled
#' into memory in full just to inspect one message.
#' @param rule_def A rule object, or `NULL` when the rule could not be
#'   resolved.
#' @return Character. Deduplicated column names.
#' @keywords internal
dta_inspect_rule_needed_columns <- function(rule_def) {
  rule_cols <- if (is.null(rule_def)) {
    character(0)
  } else if (inherits(rule_def, "DTAtools::DTARuleGroupCondition")) {
    cond_cols <- unique(unlist(lapply(rule_def@conditions, names), use.names = FALSE))
    c(rule_def@group_by %||% character(0), cond_cols)
  } else if (inherits(rule_def, "DTAtools::DTARuleColRange")) {
    rule_def@columns
  } else if (inherits(rule_def, "DTAtools::DTARuleColUnique")) {
    dta_unique_columns(rule_def)
  } else if (inherits(rule_def, "DTAtools::DTARuleColCondition")) {
    c(names(rule_def@condition), names(rule_def@then))
  } else {
    character(0)
  }

  unique(c(rule_cols, "SUBJECT_ID", "VISIT"))
}

#' @keywords internal
dta_inspect_tabular_message <- function(x, msg_row, source = c("auto", "memory", "artifact")) {
  source <- match.arg(source)
  table_name <- as.character(msg_row$target)
  details <- validation_errors(x, table = table_name, source = source)
  raw_table <- x@tables[[table_name]]

  out <- list(
    id = as.integer(msg_row$id),
    dataset = as.character(msg_row$dataset),
    target = table_name,
    source = as.character(msg_row$source),
    severity = as.character(msg_row$severity),
    headline = sprintf("[%s/%s] %s", msg_row$dataset, table_name, msg_row$message),
    message = as.character(msg_row$message)
  )

  # Both branches below build a row context for exactly one row (msg_row$row),
  # so only that row -- and only the columns dta_build_inspect_row_context()
  # can use -- is ever requested from a lazily-held table. Without a row to
  # show, there is nothing worth reading: dta_build_inspect_row_context()
  # would return NULL anyway, so the read is skipped rather than materialising
  # (or scanning) the table for no reason.
  msg_row_no <- suppressWarnings(as.numeric(msg_row$row))
  has_row <- length(msg_row_no) == 1 && !is.na(msg_row_no) && msg_row_no >= 1

  if (identical(as.character(msg_row$source), "columnspec")) {
    columnspec_full <- details$columnspec_errors$full_error
    schema_match <- dta_filter_columnspec_matches(columnspec_full, msg_row)

    row_cols <- unique(c("SUBJECT_ID", "VISIT", as.character(msg_row$column)))
    row_cols <- row_cols[!is.na(row_cols) & nzchar(row_cols)]
    table_df <- if (has_row) dta_inspect_table_frame(raw_table, columns = row_cols, max_row = msg_row_no) else NULL

    out$type <- "columnspec"
    out$why <- "Column values violate JSON schema constraints (type/value/length/required)."
    if (has_row && is.null(table_df)) {
      out$why <- paste(
        out$why,
        "Row context is unavailable: the table is a one-shot stream already consumed by validation."
      )
    }
    out$row_context <- if (is.null(table_df)) NULL else dta_build_inspect_row_context(table_df, msg_row)
    out$columnspec_matches <- utils::head(schema_match, 20)
    return(out)
  }

  # Without an explicit branch an import message would fall through to the rule
  # branch below, look up rule_id = NA, and return a nonsense record.
  if (identical(as.character(msg_row$source), "import")) {
    row_cols <- unique(c("SUBJECT_ID", "VISIT", as.character(msg_row$column)))
    row_cols <- row_cols[!is.na(row_cols) & nzchar(row_cols)]
    table_df <- if (has_row) dta_inspect_table_frame(raw_table, columns = row_cols, max_row = msg_row_no) else NULL

    out$type <- "import"
    out$why <- paste(
      "A value could not be represented in the column's declared type,",
      "so the typed column holds NA and the raw value was kept."
    )
    if (has_row && is.null(table_df)) {
      out$why <- paste(
        out$why,
        "Row context is unavailable: the table is a one-shot stream already consumed by validation."
      )
    }
    out$row_context <- if (is.null(table_df)) NULL else dta_build_inspect_row_context(table_df, msg_row)
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

  is_group_condition <- !is.null(rule_def) && inherits(rule_def, "DTAtools::DTARuleGroupCondition")

  # Failing rows are recomputed from the table, not read off `details`, so
  # there is no row bound to apply here (unlike the columnspec/import
  # branches above): the whole table -- narrowed to the columns this rule
  # actually reads -- has to be scanned regardless.
  table_df <- if (!is.null(rule_def)) {
    dta_inspect_table_frame(raw_table, columns = dta_inspect_rule_needed_columns(rule_def))
  } else {
    NULL
  }

  if (!is.null(rule_def) && is.null(table_df)) {
    out$type <- "rule"
    out$why <- paste(
      if (is_group_condition) {
        "A group-level constraint was violated: within one or more groups of rows, conditions that must be mutually exclusive co-occur, or a required follow-on condition is absent."
      } else {
        "Rule logic found rows that violate IF/THEN, range, or uniqueness constraints."
      },
      "Failing rows are unavailable: the table is a one-shot stream already consumed by validation."
    )
    out$rule_id <- rule_id
    out$rule_definition <- rule_def
    out$failing_row_count <- NA_integer_
    out$failing_rows_preview <- NULL
    out$group_violation_details <- NULL
    return(out)
  }

  failing_rows <- if (!is.null(rule_def)) dta_rule_failure_row_indices(rule_def, table_df) else integer(0)

  row_preview <- NULL
  group_violation_details <- NULL

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
