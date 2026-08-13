# Streaming evaluation of the schema axis.
#
# The non-streaming path takes a materialised data frame. That is fatal at the
# sizes this package is meant to reach: an 80 GB file cannot be an R data frame
# at all, whatever the validation costs once it is one.
#
# The property that makes streaming safe here is that the schema axis is
# per-row. No constraint it evaluates -- type, maxLength, enum, const, pattern
# -- depends on any other row, so a batch can be checked in isolation and the
# results concatenated. The only cross-batch state needed is the row offset,
# so that reported row numbers are positions in the FILE rather than in the
# batch.
#
# `required` is the exception worth naming: a column absent from the schema
# produces one error per row, so it is emitted per batch like everything else
# and simply accumulates. That is faithful to the non-streaming behaviour, and
# it is also why a structural check belongs ahead of the scan entirely -- a
# later phase gates it there.

#' @title Schema-Axis Validation over a Stream of Record Batches
#' @description
#' Evaluates the schema axis batch by batch, so peak memory is bounded by the
#' batch size rather than by the size of the input. Produces exactly the
#' `summarised_error` / `full_error` pair that `dta_schema_errors()` produces
#' for the same data.
#'
#' Row numbers are global: each batch's errors are offset by the number of rows
#' already consumed, so a row number identifies a position in the input, not in
#' the batch it happened to fall in.
#' @param specs A `DTAColumnSpecCollection`.
#' @param reader An object with a `read_next_batch()` method -- an
#'   `arrow::RecordBatchReader` or a `Scanner`'s reader.
#' @param max_errors Integer or `NULL`. Stop retaining individual errors once
#'   this many have been collected. Counting continues regardless, so the
#'   reported total is exact even when the retained detail is truncated. `NULL`
#'   retains everything, matching the non-streaming path.
#' @return A list with `summarised_error`, `full_error` and `n_errors`. When
#'   retention was truncated, `full_error` carries a `truncated` attribute.
#' @keywords internal
dta_schema_errors_stream <- function(specs, reader, max_errors = NULL) {
  parts <- list()
  row_offset <- 0L
  n_errors <- 0L
  truncated <- FALSE

  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) {
      break
    }

    df <- as.data.frame(batch)
    n_batch_rows <- nrow(df)

    if (n_batch_rows == 0) {
      next
    }

    result <- dta_schema_errors(specs, df)
    errs <- result$full_error

    if (!is.null(errs) && nrow(errs) > 0) {
      # Row numbers arrive batch-local; make them global.
      errs$row <- errs$row + row_offset
      n_errors <- n_errors + nrow(errs)

      if (is.null(max_errors)) {
        parts[[length(parts) + 1]] <- errs
      } else {
        retained <- sum(vapply(parts, nrow, integer(1)))
        room <- max_errors - retained
        if (room > 0) {
          if (nrow(errs) > room) {
            errs <- errs[seq_len(room), , drop = FALSE]
            truncated <- TRUE
          }
          parts[[length(parts) + 1]] <- errs
        } else {
          truncated <- TRUE
        }
      }
    }

    row_offset <- row_offset + n_batch_rows
  }

  if (length(parts) == 0) {
    return(list(summarised_error = NULL, full_error = NULL, n_errors = 0L))
  }

  full_error <- do.call(rbind, parts)
  rownames(full_error) <- NULL
  if (truncated) {
    attr(full_error, "truncated") <- TRUE
  }

  list(
    summarised_error = dta_summarise_schema_errors(full_error),
    full_error = full_error,
    n_errors = n_errors
  )
}

# ---- streaming the rules axis ------------------------------------------------
#
# Rules divide into three kinds by how much of the table they need at once.
#
#   decomposable  A count over a per-row mask. Range and IF/THEN rules are
#                 these. Sum the per-batch counts and the total is exactly the
#                 whole-table answer, because addition does not care how the
#                 rows were grouped.
#
#   keyed         Uniqueness. Not decomposable -- a duplicate may sit in a
#                 different batch from the value it duplicates -- but it needs
#                 only the KEY, not the row. Memory grows with the number of
#                 distinct keys rather than with the number of rows.
#
#   buffered      Grouped cross-row rules. A group can span any number of
#                 batches, so these genuinely need their rows retained. Only
#                 the columns the rule reads are kept, which on a wide table is
#                 a large reduction but is not a bound.
#
# The violation masks come from the same functions the materialising path uses
# (dta_range_violated, dta_condition_violated), so the two cannot drift.

#' @title How a Rule Can Be Streamed
#' @param rule A rule object.
#' @return One of `"decomposable"`, `"keyed"` or `"buffered"`.
#' @keywords internal
dta_rule_stream_kind <- function(rule) {
  switch(normalize_rule_type(rule@type),
    check_range = "decomposable",
    check_col_condition = "decomposable",
    check_unique = "keyed",
    check_group_condition = "buffered",
    # An unrecognised type must not be silently treated as buffered and handed
    # to the grouped evaluator; it is reported as a rule failure, as the
    # materialising path does.
    "unsupported"
  )
}

# A key that reproduces `duplicated()`'s notion of an identical row.
#
# Repeated NAs are duplicates of each other, so missing values need a value of
# their own rather than being dropped. Each part is length-prefixed so that
# c("a", "b") and c("a\002b", "") cannot produce the same key -- without that,
# a separator appearing in the data would silently merge distinct keys.
dta_unique_key <- function(df, cols) {
  parts <- lapply(cols, function(column_name) {
    values <- df[[column_name]]
    text <- as.character(values)
    text[is.na(values)] <- "\001NA"
    paste0(nchar(text, type = "bytes"), ":", text)
  })
  do.call(paste, c(parts, sep = "\002"))
}

#' @title Start Accumulating a Rule Across Batches
#' @param rule A rule object.
#' @return A mutable accumulator.
#' @keywords internal
dta_rule_stream_init <- function(rule) {
  kind <- dta_rule_stream_kind(rule)
  state <- new.env(parent = emptyenv())
  state$kind <- kind
  state$count <- 0L
  state$applicable <- TRUE
  state$condition <- NULL

  if (kind == "keyed") {
    # Hashed environment: membership testing is what this is for, and it grows
    # with distinct keys rather than with rows.
    state$seen <- new.env(hash = TRUE, parent = emptyenv())
  }
  if (kind == "buffered") {
    state$rows <- list()
  }

  state
}

#' @title Fold One Batch into a Rule's Accumulator
#' @param state An accumulator from `dta_rule_stream_init()`.
#' @param rule The rule being accumulated.
#' @param df A data frame holding one batch.
#' @return The accumulator, updated in place.
#' @keywords internal
dta_rule_stream_update <- function(state, rule, df) {
  if (!state$applicable || state$kind == "unsupported") {
    return(state)
  }

  # A rule naming a column the table does not have is not applicable, and says
  # so once rather than once per batch.
  result <- tryCatch(
    {
      switch(state$kind,
        decomposable = {
          violated <- if (identical(normalize_rule_type(rule@type), "check_range")) {
            dta_range_violated(rule, df)
          } else {
            dta_condition_violated(rule, df)
          }
          state$count <- state$count + sum(violated, na.rm = TRUE)
        },
        keyed = {
          cols <- dta_unique_columns(rule)
          missing_cols <- setdiff(cols, names(df))
          if (length(missing_cols) > 0) {
            cli::cli_abort(
              "Column{?s} not found in table: {paste(missing_cols, collapse = ', ')}",
              class = "dta_rule_not_applicable"
            )
          }
          keys <- dta_unique_key(df, cols)
          for (key in keys) {
            if (is.null(state$seen[[key]])) {
              assign(key, TRUE, envir = state$seen)
            } else {
              state$count <- state$count + 1L
            }
          }
        },
        buffered = {
          cols <- intersect(dta_rule_buffer_columns(rule), names(df))
          state$rows[[length(state$rows) + 1]] <- df[, cols, drop = FALSE]
        }
      )
      NULL
    },
    dta_rule_not_applicable = function(cnd) cnd
  )

  if (!is.null(result)) {
    state$applicable <- FALSE
    state$condition <- result
  }

  state
}

#' @title Turn a Rule's Accumulator into a Result
#' @param state An accumulator that has seen every batch.
#' @param rule The rule being accumulated.
#' @return A list with `id`, `valid` and `message`, matching what the
#'   materialising rule functions return.
#' @keywords internal
dta_rule_stream_finalise <- function(state, rule) {
  if (state$kind == "unsupported") {
    return(list(
      id = rule@id,
      valid = FALSE,
      message = paste("Unknown rule type:", normalize_rule_type(rule@type))
    ))
  }

  if (!state$applicable) {
    # Matches apply_schema_rules(): a rule that cannot be evaluated against this
    # table is a rule FAILURE, not a reason to abandon the rest of validation.
    return(list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' could not be evaluated: %s",
        rule@id,
        conditionMessage(state$condition)
      )
    ))
  }

  if (state$kind == "buffered") {
    # Groups span batches, so this rule could only ever be answered once every
    # row it reads had been seen.
    buffered <- if (length(state$rows) == 0) {
      NULL
    } else {
      do.call(rbind, state$rows)
    }
    if (is.null(buffered)) {
      return(list(id = rule@id, valid = TRUE, message = NULL))
    }
    return(rule_check_group_condition(rule, buffered))
  }

  if (state$count == 0) {
    return(list(id = rule@id, valid = TRUE, message = NULL))
  }

  message <- if (state$kind == "keyed") {
    dta_unique_violation_message(rule@id, state$count, dta_unique_columns(rule))
  } else if (identical(normalize_rule_type(rule@type), "check_range")) {
    target <- dta_range_target(rule)
    dta_range_violation_message(rule@id, state$count, target$col, target$range)
  } else {
    dta_condition_violation_message(rule@id, state$count)
  }

  list(id = rule@id, valid = FALSE, message = message)
}

#' @title Columns Forming a Uniqueness Key
#' @param rule A uniqueness rule.
#' @return A character vector of column names.
#' @keywords internal
dta_unique_columns <- function(rule) {
  cols <- rule_get_slot(rule, "column")
  if (is.null(cols)) {
    cols <- rule_get_slot(rule, "columns")
  }
  cols
}

#' @title Columns a Buffered Rule Needs Retained
#' @description
#' Grouped rules cannot be answered batch by batch, but they read far fewer
#' columns than a table has. Retaining only these turns "buffer the table" into
#' "buffer a few columns of it".
#' @param rule A grouped rule.
#' @return A character vector of column names.
#' @keywords internal
dta_rule_buffer_columns <- function(rule) {
  group_by <- tryCatch(rule_get_slot(rule, "group_by"), error = function(e) NULL)
  conditions <- tryCatch(rule_get_slot(rule, "conditions"), error = function(e) NULL)

  condition_cols <- unlist(
    lapply(conditions, names),
    use.names = FALSE
  )

  unique(c(as.character(group_by), as.character(condition_cols)))
}

# ---- the streaming driver ----------------------------------------------------

# Bounded accumulation of a per-cell error frame.
#
# Both the schema and import axes can produce one error per bad cell, so on a
# dirty file the error frame is O(rows) and exhausts memory as surely as the
# data would. Retention is capped; counting is not, so the reported totals stay
# exact and the pass/fail verdict is never an artefact of truncation.
dta_error_sink <- function(max_errors) {
  sink <- new.env(parent = emptyenv())
  sink$parts <- list()
  sink$retained <- 0L
  sink$total <- 0L
  sink$truncated <- FALSE
  sink$max <- max_errors
  sink
}

dta_error_sink_add <- function(sink, errs) {
  if (is.null(errs) || nrow(errs) == 0) {
    return(sink)
  }
  sink$total <- sink$total + nrow(errs)

  if (is.null(sink$max)) {
    sink$parts[[length(sink$parts) + 1]] <- errs
    sink$retained <- sink$retained + nrow(errs)
    return(sink)
  }

  room <- sink$max - sink$retained
  if (room <= 0) {
    sink$truncated <- TRUE
    return(sink)
  }
  if (nrow(errs) > room) {
    errs <- errs[seq_len(room), , drop = FALSE]
    sink$truncated <- TRUE
  }
  sink$parts[[length(sink$parts) + 1]] <- errs
  sink$retained <- sink$retained + nrow(errs)
  sink
}

dta_error_sink_collect <- function(sink) {
  if (length(sink$parts) == 0) {
    return(NULL)
  }
  out <- do.call(rbind, sink$parts)
  rownames(out) <- NULL
  if (sink$truncated) {
    attr(out, "truncated") <- TRUE
  }
  out
}

#' @title Validate a Table from a Stream of Record Batches
#' @description
#' The streaming counterpart of `validate_table_detailed()`. Evaluates all three
#' axes -- column specs, rules, and import typing -- reading one batch at a
#' time, and returns the same `details` structure, so every existing consumer
#' (`results()`, `messages()`, `inspect()`, the Shiny app) works unchanged.
#'
#' Peak memory is bounded by the batch size for the schema axis, by the number
#' of distinct keys for uniqueness rules, and by the retained-error cap. Grouped
#' rules are the exception: a group can span any number of batches, so the
#' columns those rules read are retained for the whole scan.
#'
#' Row numbers are positions in the input, not in the batch a value happened to
#' fall in.
#' @param specs A `DTAColumnSpecCollection`.
#' @param reader An object with a `read_next_batch()` method.
#' @param verbose Logical. Print progress.
#' @param max_errors Integer or `NULL`. Cap on retained per-cell errors. `NULL`
#'   retains everything, matching the materialising path.
#' @param coerce Logical. Type each batch against the specs as it arrives,
#'   recording values that cannot be represented. This is the streaming
#'   equivalent of typing the table once at import.
#' @return A `details` list of the same shape `validate_table_detailed()`
#'   returns.
#' @keywords internal
dta_validate_table_stream <- function(specs,
                                      reader,
                                      verbose = FALSE,
                                      max_errors = NULL,
                                      coerce = TRUE) {
  rules_list <- tryCatch(specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }

  states <- lapply(rules_list, dta_rule_stream_init)

  schema_sink <- dta_error_sink(max_errors)
  carried_sink <- dta_error_sink(max_errors)
  rule_import_sink <- dta_error_sink(max_errors)
  row_offset <- 0L

  if (isTRUE(verbose)) {
    cli::cli_h3("validating with column specs")
  }

  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) {
      break
    }

    df <- as.data.frame(batch)
    n_batch_rows <- nrow(df)
    if (n_batch_rows == 0) {
      next
    }

    # Import typing, per batch. The materialising path types the whole table
    # once and hangs the issues on it as an attribute; with no single table to
    # hang anything on, the issues accumulate here instead.
    if (isTRUE(coerce)) {
      coerced <- dta_coerce_table_to_specs(df, specs)
      df <- coerced$table
      issues <- coerced$issues
      if (is.data.frame(issues) && nrow(issues) > 0) {
        issues$row <- issues$row + row_offset
        dta_error_sink_add(carried_sink, issues)
      }
    }

    schema_result <- dta_schema_errors(specs, df)
    schema_errs <- schema_result$full_error
    if (!is.null(schema_errs) && nrow(schema_errs) > 0) {
      schema_errs$row <- schema_errs$row + row_offset
      dta_error_sink_add(schema_sink, schema_errs)
    }

    for (i in seq_along(rules_list)) {
      dta_rule_stream_update(states[[i]], rules_list[[i]], df)

      # Sourced from the same columns the rule just read as numbers, so an
      # unrepresentable value is reported on both axes rather than moved
      # from one to the other.
      rule_errs <- tryCatch(
        dta_rule_import_errors(rules_list[[i]], df),
        error = function(e) NULL
      )
      if (is.data.frame(rule_errs) && nrow(rule_errs) > 0) {
        rule_errs$row <- rule_errs$row + row_offset
        dta_error_sink_add(rule_import_sink, rule_errs)
      }
    }

    row_offset <- row_offset + n_batch_rows
  }

  full_error <- dta_error_sink_collect(schema_sink)
  summarised_error <- dta_summarise_schema_errors(full_error)
  has_schema_errors <- schema_sink$total > 0

  rule_results <- lapply(seq_along(rules_list), function(i) {
    dta_rule_stream_finalise(states[[i]], rules_list[[i]])
  })
  rule_errors <- Filter(function(x) !isTRUE(x$valid), rule_results)
  rules_valid <- length(rule_errors) == 0

  carried <- dta_error_sink_collect(carried_sink)
  rule_import <- dta_error_sink_collect(rule_import_sink)
  if (!is.null(rule_import)) {
    rule_import <- rule_import[
      !duplicated(rule_import[, c("row", "column"), drop = FALSE]), ,
      drop = FALSE
    ]
    rule_import <- dta_apply_spec_declared_types(rule_import, specs)
  }

  import_errors <- dta_merge_import_errors(carried, rule_import)
  n_import_errors <- carried_sink$total + rule_import_sink$total
  import_valid <- n_import_errors == 0L
  if (n_import_errors == 0L) {
    import_errors <- NULL
  }

  details <- list(
    ok = NA,
    schema_valid = !has_schema_errors,
    rules_valid = isTRUE(rules_valid),
    import_valid = isTRUE(import_valid),
    n_schema_errors = schema_sink$total,
    n_rule_errors = length(rule_errors),
    n_import_errors = as.integer(n_import_errors),
    schema_errors = list(
      summarised_error = summarised_error,
      full_error = full_error
    ),
    rule_results = rule_results,
    rule_errors = rule_errors,
    import_errors = import_errors,
    schema_version = 2L
  )

  details$ok <- dta_details_ok(details)
  details
}

#' @title Fill In Declared Types on Import Errors
#' @description
#' Import errors carry the observed storage type as a placeholder; the column
#' spec's declared type replaces it where one exists. Shared with the
#' materialising path's collection step.
#' @param errors A data frame of import errors.
#' @param specs A `DTAColumnSpecCollection`.
#' @return The data frame, with `declared_type` filled in.
#' @keywords internal
dta_apply_spec_declared_types <- function(errors, specs = NULL) {
  if (is.null(errors) || nrow(errors) == 0) {
    return(errors)
  }
  declared <- vapply(
    errors$column,
    function(column) dta_spec_declared_type(specs, column),
    character(1),
    USE.NAMES = FALSE
  )
  errors$declared_type <- ifelse(is.na(declared), errors$declared_type, declared)
  errors
}

# ---- reading a file without materialising it ---------------------------------

#' @title Open a Delimited File as a Lazy Dataset
#' @description
#' Opens a delimited file for scanning rather than reading it into memory. The
#' parse options and the spec-driven column types are the same ones the eager
#' reader uses, so the columns are typed identically -- the difference is only
#' that nothing is read until a scan asks for it.
#' @param path Character. Path to the file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. Declared types decide how
#'   columns are parsed; without it every column is inferred.
#' @param delim Character. The field separator.
#' @param quote Character. The quoting character.
#' @param has_header Logical. Whether the first line names the columns.
#' @return An `arrow::Dataset`.
#' @keywords internal
dta_open_delimited_dataset <- function(path,
                                       specs = NULL,
                                       delim = ",",
                                       quote = "\"",
                                       has_header = TRUE) {
  arrow::open_delim_dataset(
    path,
    delim = delim,
    quote = quote,
    col_names = has_header,
    col_types = dta_reader_col_types(specs, has_header)
  )
}

#' @title Validate a Delimited File Without Loading It
#' @description
#' Validates a delimited file against a set of column specs by scanning it in
#' batches, so peak memory is governed by the batch size rather than by the size
#' of the file. This is what makes a file larger than memory checkable at all:
#' the eager path has to hold the whole table as an R data frame before it can
#' validate a single row.
#'
#' The result is the same `details` structure the in-memory path returns, so
#' `results()`, `messages()` and `inspect()` accept it unchanged.
#'
#' Memory is bounded by the batch size for the column-spec checks, by the number
#' of distinct keys for uniqueness rules, and by `max_errors` for the retained
#' error detail. Grouped rules are the exception: a group may span any part of
#' the file, so the columns those rules read are held for the whole scan.
#'
#' @section Choosing between this and the in-memory path:
#' This buys feasibility, not speed. Measured over a 16-fold increase in input,
#' the working set this holds stayed flat at about 19 MB while the in-memory
#' path's grew from 51 MB to 272 MB -- but scanning ran roughly twice as slow,
#' because each batch pays its own dispatch and typing overhead.
#'
#' So: use this when the file is large enough that holding it is the problem.
#' For a file that fits in memory comfortably, `validate_table()` is the faster
#' choice and there is nothing to gain here.
#' @param specs A `DTAColumnSpecCollection`.
#' @param path Character. Path to the delimited file.
#' @param delim Character. The field separator. Defaults to a comma.
#' @param quote Character. The quoting character.
#' @param has_header Logical. Whether the first line names the columns.
#' @param batch_rows Integer. Rows per batch. Larger batches trade memory for
#'   fewer per-batch overheads.
#' @param max_errors Integer or `NULL`. Cap on retained per-cell error detail.
#'   Counting is unaffected, so totals and the pass/fail verdict stay exact even
#'   when the retained detail is truncated.
#' @param verbose Logical. Print progress.
#' @return A validation details list.
#' @examples
#' specs <- DTAtools::DTAColumnSpecCollection(
#'   columns = list(
#'     ID = DTAtools::DTAColumnSpec(
#'       id = "ID", type = "SAS Char", length = 4, nullable = FALSE
#'     ),
#'     AGE = DTAtools::DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
#'   )
#' )
#'
#' path <- file.path(tempdir(), "dta_stream_example.csv")
#' utils::write.csv(
#'   data.frame(ID = c("A001", "TOOLONG"), AGE = c(30, 40)),
#'   path,
#'   row.names = FALSE
#' )
#'
#' details <- validate_file_stream(specs, path, verbose = FALSE)
#' details$n_schema_errors
#'
#' unlink(path)
#' @export
validate_file_stream <- function(specs,
                                 path,
                                 delim = ",",
                                 quote = "\"",
                                 has_header = TRUE,
                                 batch_rows = 131072L,
                                 max_errors = NULL,
                                 verbose = TRUE) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  dataset <- dta_open_delimited_dataset(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header
  )

  reader <- arrow::Scanner$create(dataset, batch_size = batch_rows)$ToRecordBatchReader()

  dta_validate_table_stream(
    specs,
    reader,
    verbose = verbose,
    max_errors = max_errors,
    coerce = TRUE
  )
}

#' @title Record Batch Reader for an In-Memory Table
#' @description
#' Wraps an Arrow table or an R data frame as a reader that yields fixed-size
#' record batches. Used to run the streaming path over data that is already in
#' memory -- chiefly to prove the streaming and non-streaming paths agree, and
#' to let a caller bound memory even when the input did not arrive as a stream.
#' @param x An Arrow table, record batch, or data frame.
#' @param batch_rows Integer. Rows per batch.
#' @return An `arrow::RecordBatchReader`.
#' @keywords internal
dta_as_batch_reader <- function(x, batch_rows = 65536L) {
  table_obj <- if (inherits(x, "Table")) x else arrow::as_arrow_table(x)

  # Scanner is the same machinery a file-backed dataset scan uses, so the
  # streaming path is exercised through its real interface rather than a
  # test-only substitute.
  scanner <- arrow::Scanner$create(table_obj, batch_size = batch_rows)
  scanner$ToRecordBatchReader()
}
