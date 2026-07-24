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
#' `n_schema_errors`, `n_rule_errors`.
#'
#' For `DTA`, the summary is aggregated per dataset and includes:
#' `dataset`, `n_targets`, `n_validated`, `n_valid`, `n_invalid`,
#' `n_skipped`, `n_not_validated`.
#' @examples
#' \dontrun{
#' dta <- create_example_DTA()
#' dta <- check(dta, quiet = TRUE)
#' results(dta)
#'
#' ds <- dta[["demographics"]]
#' results(ds)
#' }
#' @name results
#' @export
results <- S7::new_generic("results", "x")

#' @keywords internal
dta_results_from_status <- function(status_df, dataset_name = NA_character_) {
  if (is.null(status_df) || nrow(status_df) == 0) {
    return(data.frame(
      dataset = dataset_name,
      target = character(0),
      target_type = character(0),
      status = character(0),
      validated_at = character(0),
      run_id = character(0),
      validation_run = character(0),
      n_schema_errors = integer(0),
      n_rule_errors = integer(0),
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
    n_schema_errors = status_df$n_schema_errors,
    n_rule_errors = status_df$n_rule_errors,
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
        n_schema_errors = NA_integer_,
        n_rule_errors = NA_integer_,
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
#' The output combines schema and rule failures into a single flat table.
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
#' \dontrun{
#' ds <- create_example_DTADataSetTabular(2)
#' ds <- check(ds, quiet = TRUE)
#' messages(ds)
#' }
#' @name messages
#' @export
messages <- S7::new_generic("messages", "x")

#' @keywords internal
dta_empty_messages <- function() {
  data.frame(
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
dta_schema_messages_to_df <- function(dataset_name, table_name, details) {
  full_error <- details$schema_errors$full_error

  if (is.null(full_error) || nrow(full_error) == 0) {
    return(dta_empty_messages())
  }

  row_values <- if ("row" %in% names(full_error)) full_error$row else rep(NA_real_, nrow(full_error))
  col_values <- if ("column" %in% names(full_error)) full_error$column else rep(NA_character_, nrow(full_error))
  key_values <- if ("keyword" %in% names(full_error)) full_error$keyword else rep(NA_character_, nrow(full_error))
  msg_values <- if ("message" %in% names(full_error)) full_error$message else rep("schema validation error", nrow(full_error))

  data.frame(
    dataset = rep(dataset_name, nrow(full_error)),
    target = rep(table_name, nrow(full_error)),
    severity = rep("error", nrow(full_error)),
    source = rep("schema", nrow(full_error)),
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

    schema_df <- dta_schema_messages_to_df(dataset_name, table_name, details)
    rule_df <- dta_rule_messages_to_df(dataset_name, table_name, details)

    rbind(schema_df, rule_df)
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
  msgs
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

  if (length(out) == 0) {
    return(dta_to_tibble_if_available(dta_empty_messages(), as_tibble = as_tibble))
  }

  msgs <- do.call(rbind, out)
  if (is.null(msgs) || nrow(msgs) == 0) {
    return(dta_to_tibble_if_available(dta_empty_messages(), as_tibble = as_tibble))
  }

  rownames(msgs) <- NULL
  dta_to_tibble_if_available(msgs, as_tibble = as_tibble)
}
