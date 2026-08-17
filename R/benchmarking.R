# Package-local flag guarding against nested benchmark measurement. `check()`
# on a `DTA` calls `check()` on each `DTADataSet`, which calls `check()` on
# each `DTADataSetTabular` -- if every level reset the R heap peak counter via
# `gc(reset = TRUE)`, the outermost call's figure would be silently corrupted
# by whatever the innermost call last reset it to. Kept in its own environment
# (parent = emptyenv()) rather than an option, because it is process state
# tracking whether a measurement is already in flight, not user-facing
# configuration.
dta_benchmark_env <- new.env(parent = emptyenv())
dta_benchmark_env$active <- FALSE

#' @title Process RSS in Bytes
#' @description
#' Reads the current process's resident set size via the optional `ps`
#' package. `ps` is listed under `Suggests:`, not `Imports:`, so this must
#' degrade to `NA_real_` -- never error -- when it is not installed.
#' @return A single numeric, or `NA_real_` if `ps` is unavailable or the read
#'   fails for any reason.
#' @keywords internal
dta_process_rss_bytes <- function() {
  tryCatch(
    {
      if (!requireNamespace("ps", quietly = TRUE)) {
        return(NA_real_)
      }
      as.numeric(ps::ps_memory_info()[["rss"]])
    },
    error = function(e) NA_real_
  )
}

#' @title Arrow's Process-Wide Peak Pool Allocation, in Bytes
#' @description
#' Reads `arrow::default_memory_pool()$max_memory`, the high-water mark of
#' Arrow's C++ allocator for the whole process. `arrow` is a hard `Imports:`
#' dependency, but the read is still guarded because the pool object's shape
#' is not part of Arrow's contract with this package.
#' @return A single numeric, or `NA_real_` on any failure.
#' @keywords internal
dta_arrow_pool_max_bytes <- function() {
  tryCatch(
    as.numeric(arrow::default_memory_pool()$max_memory),
    error = function(e) NA_real_
  )
}

#' @title Begin a Benchmark Measurement
#' @description
#' Opens a benchmark measurement window, or declines to. Returns `NULL` --
#' rather than a state object -- whenever no measurement should be taken,
#' which lets a call site do `state <- dta_benchmark_begin(benchmark)` and
#' later `dta_benchmark_end(state)` unconditionally: the `NULL` propagates and
#' `dta_benchmark_end()` becomes a no-op.
#'
#' `gc(reset = TRUE, full = TRUE)` runs *before* the clock is read, so the
#' collection this call triggers is never charged to the measured region. See
#' [dta_benchmark_end()] for the matching rule at the other end.
#' @param enabled Logical. Whether to begin a measurement.
#' @return An opaque state list, or `NULL` when `enabled` is not `TRUE`, or
#'   when a measurement is already in flight (see the nesting guard above).
#' @keywords internal
dta_benchmark_begin <- function(enabled) {
  if (!isTRUE(enabled)) {
    return(NULL)
  }
  if (isTRUE(dta_benchmark_env$active)) {
    return(NULL)
  }
  dta_benchmark_env$active <- TRUE

  # Reset R's heap-peak counter and collect BEFORE the clock starts, so the
  # collection itself is excluded from the timed region.
  gc(reset = TRUE, full = TRUE)

  list(
    proc_time_start = proc.time(),
    arrow_pool_start_bytes = dta_arrow_pool_max_bytes(),
    rss_start_bytes = dta_process_rss_bytes()
  )
}

#' @title End a Benchmark Measurement
#' @description
#' Closes a measurement window opened by [dta_benchmark_begin()] and returns
#' one row of metrics, or `NULL` when `state` is `NULL` (benchmarking was off,
#' or the nesting guard declined this call).
#'
#' `proc.time()` is read *first*, before anything else, so that none of the
#' bookkeeping below -- including the closing `gc()` -- is charged to the
#' measured region. The active flag is reset via `on.exit(add = TRUE)` so a
#' measurement that errors still leaves the nesting guard usable for the next
#' call.
#' @param state The state list returned by [dta_benchmark_begin()], or `NULL`.
#' @param rows Numeric. Row count to attach to the metrics (for
#'   `rows_per_sec`), or `NA_real_` when no trustworthy count is available at
#'   the call site.
#' @return A one-row `data.frame` of metrics, or `NULL` when `state` is
#'   `NULL`.
#' @keywords internal
dta_benchmark_end <- function(state, rows = NA_real_) {
  if (is.null(state)) {
    return(NULL)
  }

  proc_time_end <- proc.time()
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  arrow_pool_end_bytes <- dta_arrow_pool_max_bytes()
  rss_end_bytes <- dta_process_rss_bytes()

  # The closing collection runs AFTER the clock and the pool/RSS reads above,
  # for the same reason `dta_benchmark_begin()` runs its collection before
  # starting the clock: charging collection cost to the measured region would
  # inflate every figure.
  g <- gc(full = TRUE)

  # gc()'s matrix has duplicated "(Mb)" column names, so a fixed column index
  # is fragile across R versions. Find "max used" by name instead, and read
  # the column immediately to its right (its "(Mb)" pair).
  idx <- which(colnames(g) == "max used")
  r_peak_mb <- if (length(idx) == 1L) sum(g[, idx + 1L]) else NA_real_

  elapsed <- proc_time_end - state$proc_time_start
  elapsed_sec <- as.numeric(elapsed[["elapsed"]])
  # On Windows, proc.time() does not measure child-process times and reports
  # NA for user.child/sys.child. na.rm = TRUE treats an unmeasured component
  # as a 0 contribution to the total -- which it is, since it wasn't tracked
  # -- rather than letting a single NA poison the whole sum to NA.
  cpu_user_sec <- sum(
    as.numeric(elapsed[["user.self"]]), as.numeric(elapsed[["user.child"]]),
    na.rm = TRUE
  )
  cpu_sys_sec <- sum(
    as.numeric(elapsed[["sys.self"]]), as.numeric(elapsed[["sys.child"]]),
    na.rm = TRUE
  )

  rss_start_mb <- state$rss_start_bytes / 1024^2
  rss_end_mb <- rss_end_bytes / 1024^2

  arrow_pool_peak_mb <- arrow_pool_end_bytes / 1024^2
  # Arrow's pool has no reset (verified against arrow 25.0.0:
  # `default_memory_pool()` exposes only backend_name, bytes_allocated,
  # class_title, clone, initialize, max_memory, pointer, print, set_pointer).
  # `max_memory` is a per-PROCESS high-water mark, so the difference across
  # this call is only the true cost of THIS call when this call actually set a
  # new high-water mark. When an earlier call in the session peaked higher,
  # the difference is 0 -- a lower bound, not the truth -- and
  # `arrow_call_exact` says so rather than letting a reader assume otherwise.
  arrow_call_exact <- isTRUE(arrow_pool_end_bytes > state$arrow_pool_start_bytes)
  arrow_call_mb <- if (arrow_call_exact) {
    (arrow_pool_end_bytes - state$arrow_pool_start_bytes) / 1024^2
  } else {
    0
  }

  rows_per_sec <- if (is.na(rows) || elapsed_sec == 0) {
    NA_real_
  } else {
    rows / elapsed_sec
  }

  data.frame(
    elapsed_sec = elapsed_sec,
    cpu_user_sec = cpu_user_sec,
    cpu_sys_sec = cpu_sys_sec,
    r_peak_mb = r_peak_mb,
    rss_start_mb = rss_start_mb,
    rss_end_mb = rss_end_mb,
    arrow_pool_peak_mb = arrow_pool_peak_mb,
    arrow_call_mb = arrow_call_mb,
    arrow_call_exact = arrow_call_exact,
    rows = rows,
    rows_per_sec = rows_per_sec,
    benchmarked_at = Sys.time(),
    metrics_version = 1L,
    stringsAsFactors = FALSE
  )
}

#' @title Print a Benchmark Console Summary
#' @description
#' Prints the one-line "Benchmark: ..." summary shared by `check(DTA)` and
#' `validate_file_stream()`, or does nothing when `metrics` is `NULL` (no
#' measurement was taken). Callers are still responsible for checking their
#' own `verbose`/`quiet` flag before calling this -- it has no opinion on
#' that, only on `metrics`.
#' @param metrics The one-row `data.frame` returned by [dta_benchmark_end()],
#'   or `NULL`.
#' @return `NULL`, invisibly. Called for its console side effect.
#' @keywords internal
dta_benchmark_report <- function(metrics) {
  if (is.null(metrics)) {
    return(invisible(NULL))
  }

  cli::cli_alert_info(
    paste0(
      "Benchmark: ", round(metrics$elapsed_sec, 3), "s elapsed, ",
      round(metrics$r_peak_mb, 1), " MB R heap peak, ",
      round(metrics$cpu_user_sec + metrics$cpu_sys_sec, 3), "s CPU"
    )
  )

  invisible(NULL)
}

#' @title Retrieve Benchmark Metrics from a Validation Result
#' @description
#' Returns the one-row metrics `data.frame` attached by `check()` or
#' `validate_file_stream()` when called with `benchmark = TRUE`, or `NULL`
#' when the result was not benchmarked.
#' @param x The return value of `check()` or `validate_file_stream()`.
#' @return A one-row `data.frame`, or `NULL`.
#' @section Columns:
#' `elapsed_sec`, `cpu_user_sec`, `cpu_sys_sec`, `r_peak_mb`, `rss_start_mb`,
#' `rss_end_mb`, `arrow_pool_peak_mb`, `arrow_call_mb`, `arrow_call_exact`,
#' `rows`, `rows_per_sec`, `benchmarked_at`, `metrics_version`.
#'
#' `cpu_user_sec` and `cpu_sys_sec` sum the `.self` and `.child` components of
#' `proc.time()`'s difference. With `use_threads = TRUE` (the
#' `validate_file_stream()` default), CPU time legitimately exceeds elapsed
#' time on a multi-core machine -- that is Arrow doing parallel work, not a
#' measurement bug. On Windows, `proc.time()` never measures child-process
#' time (`user.child`/`sys.child` are always `NA`), so these two columns
#' undercount relative to a platform that does track children -- a reader
#' comparing figures across platforms should know the Windows numbers are
#' self-process CPU only.
#' @section The Arrow columns:
#' `arrow`'s memory pool (`arrow::default_memory_pool()`) has no reset, so its
#' `max_memory` is a high-water mark for the whole R session, not for one
#' call. `arrow_pool_peak_mb` reports that absolute, honest, per-process peak.
#' `arrow_call_mb` reports the difference observed across this call, and
#' `arrow_call_exact` is `TRUE` only when this call actually pushed the
#' process to a new high-water mark; when it is `FALSE`, `arrow_call_mb` is a
#' lower bound on what this call allocated, not a measurement of it -- an
#' earlier, larger call in the same session may be why nothing "new" showed up
#' here.
#' @section Side effect of `benchmark = TRUE`:
#' Measuring the R heap peak accurately requires resetting R's `gc()` peak
#' counters (`gc(reset = TRUE)`) at the start of the call. This is a
#' session-wide side effect: any peak-since-reset figure another part of the
#' session was tracking via `gc()` is reset too. Calls that are already
#' running a benchmark are unaffected -- nested calls no-op instead of
#' resetting again -- but a benchmark started by one call does reset counters
#' another, concurrent piece of code may have been relying on.
#' @examples
#' path <- file.path(tempdir(), "dta_benchmark_example.csv")
#' utils::write.csv(data.frame(ID = c("A001", "A002")), path, row.names = FALSE)
#'
#' specs <- DTAtools::DTAColumnSpecCollection(
#'   columns = list(
#'     ID = DTAtools::DTAColumnSpec(id = "ID", type = "SAS Char", length = 4)
#'   )
#' )
#'
#' details <- validate_file_stream(specs, path, benchmark = TRUE, verbose = FALSE)
#' validation_benchmark(details)
#'
#' unlink(path)
#' @export
validation_benchmark <- function(x) {
  attr(x, "benchmark")
}
