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
#' `summarised_error` / `full_error` pair that [dta_schema_errors()] produces
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
