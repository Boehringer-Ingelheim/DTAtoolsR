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
    "buffered"
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
  if (!state$applicable) {
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
  if (!state$applicable) {
    return(list(
      id = rule@id,
      valid = NA,
      message = conditionMessage(state$condition),
      not_applicable = TRUE
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
