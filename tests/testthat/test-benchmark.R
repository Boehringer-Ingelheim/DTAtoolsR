# Opt-in benchmark mode on check() and validate_file_stream().
#
# These tests assert behaviour, not existence: the shape of the metrics, that
# the instrument never moves the verdict it measures, and that the nesting
# guard and the "ps missing" degradation actually work -- not merely that a
# "benchmark" attribute is present.

bm_metrics_columns <- c(
  "elapsed_sec", "cpu_user_sec", "cpu_sys_sec", "r_peak_mb", "rss_start_mb",
  "rss_end_mb", "arrow_pool_peak_mb", "arrow_call_mb", "arrow_call_exact",
  "rows", "rows_per_sec", "benchmarked_at", "metrics_version"
)

bm_write_csv <- function(df) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  path
}

bm_expect_valid_metrics <- function(metrics) {
  expect_true(is.data.frame(metrics))
  expect_equal(nrow(metrics), 1)
  expect_identical(names(metrics), bm_metrics_columns)
  expect_true(is.finite(metrics$elapsed_sec))
  expect_gte(metrics$elapsed_sec, 0)
  expect_gte(metrics$cpu_user_sec, 0)
}

# ---- check(DTA) --------------------------------------------------------

test_that("check() attaches no benchmark attribute by default", {
  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE)
  expect_null(attr(dta, "benchmark"))
})

test_that("check(benchmark = TRUE) attaches metrics with the documented shape", {
  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE, benchmark = TRUE)
  bm_expect_valid_metrics(attr(dta, "benchmark"))
})

test_that("benchmarking check() does not move the verdict", {
  off <- check(create_example_DTA(), persist = FALSE, quiet = TRUE, benchmark = FALSE)
  on <- check(create_example_DTA(), persist = FALSE, quiet = TRUE, benchmark = TRUE)

  expect_identical(attr(off, "last_validation_ok"), attr(on, "last_validation_ok"))
})

test_that("check() honours options(DTAtools.benchmark = TRUE) without the argument", {
  old <- options(DTAtools.benchmark = TRUE)
  on.exit(options(old), add = TRUE)

  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE)
  expect_false(is.null(attr(dta, "benchmark")))
})

# ---- validate_file_stream() --------------------------------------------

test_that("validate_file_stream() attaches no benchmark attribute by default", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- bm_write_csv(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE)
  expect_null(attr(details, "benchmark"))
})

test_that("validate_file_stream(benchmark = TRUE) attaches metrics with the documented shape", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- bm_write_csv(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE, benchmark = TRUE)
  bm_expect_valid_metrics(attr(details, "benchmark"))
})

test_that("benchmarking validate_file_stream() does not move the verdict", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- bm_write_csv(data.frame(ID = c("A001", "TOOLONG"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  off <- validate_file_stream(specs, path, verbose = FALSE, benchmark = FALSE)
  on <- validate_file_stream(specs, path, verbose = FALSE, benchmark = TRUE)

  expect_identical(off$ok, on$ok)
  expect_identical(off$n_columnspec_errors, on$n_columnspec_errors)
})

test_that("validate_file_stream() honours options(DTAtools.benchmark = TRUE) without the argument", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- bm_write_csv(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  old <- options(DTAtools.benchmark = TRUE)
  on.exit(options(old), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE)
  expect_false(is.null(attr(details, "benchmark")))
})

test_that("a known row count is reported exactly", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- bm_write_csv(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  details <- validate_file_stream(specs, path, verbose = FALSE, benchmark = TRUE)
  expect_equal(attr(details, "benchmark")$rows, 2)
})

test_that("a structural early return still carries metrics", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "GONE", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  path <- bm_write_csv(data.frame(ID = "A001", stringsAsFactors = FALSE))
  on.exit(unlink(path), add = TRUE)

  stopped <- validate_file_stream(
    specs, path,
    on_missing_column = "stop", verbose = FALSE, benchmark = TRUE
  )

  expect_true(isTRUE(attr(stopped, "structural_only")))
  metrics <- attr(stopped, "benchmark")
  bm_expect_valid_metrics(metrics)
  expect_equal(metrics$rows, 0)
})

# ---- validation_benchmark() accessor -----------------------------------

test_that("validation_benchmark() returns NULL when nothing was benchmarked", {
  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE)
  expect_null(validation_benchmark(dta))
})

test_that("validation_benchmark() returns the attached metrics", {
  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE, benchmark = TRUE)
  expect_identical(validation_benchmark(dta), attr(dta, "benchmark"))
})

# ---- internals: nesting guard and ps-absent tolerance -------------------

test_that("dta_benchmark_begin() refuses to nest, and dta_benchmark_end() clears the flag", {
  # Guard the shared flag so a failure mid-test cannot leave it TRUE and break
  # every later test in this file.
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  outer <- dta_benchmark_begin(TRUE)
  expect_false(is.null(outer))
  expect_true(dta_benchmark_env$active)

  inner <- dta_benchmark_begin(TRUE)
  expect_null(inner)

  metrics <- dta_benchmark_end(outer, rows = NA_real_)
  bm_expect_valid_metrics(metrics)
  expect_false(dta_benchmark_env$active)
})

test_that("rss_end_mb is NA or a positive number, never asserted non-NA", {
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  state <- dta_benchmark_begin(TRUE)
  metrics <- dta_benchmark_end(state, rows = NA_real_)

  expect_true(is.na(metrics$rss_end_mb) || metrics$rss_end_mb > 0)
})

test_that("an absent row count is recorded as NA rather than aborting the call", {
  # Call sites read `rows` off an attribute, and an absent attribute is NULL.
  # A NULL reaching `is.na()` would make the `if` fail with a zero-length
  # condition -- the instrument killing the call it was only meant to measure.
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  state <- dta_benchmark_begin(TRUE)
  metrics <- dta_benchmark_end(state, rows = NULL)

  bm_expect_valid_metrics(metrics)
  expect_true(is.na(metrics$rows))
  expect_true(is.na(metrics$rows_per_sec))
})

test_that("an unreadable Arrow pool reports NA, not a zero that reads as measured", {
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  local_mocked_bindings(dta_arrow_pool_max_bytes = function() NA_real_)

  state <- dta_benchmark_begin(TRUE)
  metrics <- dta_benchmark_end(state, rows = NA_real_)

  bm_expect_valid_metrics(metrics)
  expect_true(is.na(metrics$arrow_pool_peak_mb))
  expect_true(is.na(metrics$arrow_call_mb))
  expect_false(metrics$arrow_call_exact)
})

# ---- regression: the nesting guard must not leak on the error path ------
#
# dta_benchmark_env$active is set TRUE by dta_benchmark_begin() and was only
# ever reset by dta_benchmark_end(). If the call body between begin() and
# end() throws, end() is never reached, the flag stays stuck TRUE, and every
# later benchmark = TRUE call silently returns no metrics -- no error, just
# dead instrumentation for the rest of the R session. The fix registers the
# reset via on.exit() in the caller's own frame, so it fires on any exit.

test_that("check(benchmark = TRUE) resets the nesting guard when the call body errors", {
  # Guard the shared flag so a failure mid-test cannot leave it TRUE and break
  # every later test in this file.
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  dta <- create_example_DTA()
  expect_error(
    check(dta, datasets = 999, benchmark = TRUE),
    regexp = "Dataset index out of bounds"
  )
  expect_false(dta_benchmark_env$active)

  # The behaviour that actually matters: benchmarking must still work
  # afterwards, not merely report the flag as FALSE.
  after <- check(create_example_DTA(), persist = FALSE, quiet = TRUE, benchmark = TRUE)
  bm_expect_valid_metrics(attr(after, "benchmark"))
})

test_that("validate_file_stream(benchmark = TRUE) resets the nesting guard when the call body errors", {
  on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)

  # dta_open_validation_dataset()/dta_validate_table_stream() both run after
  # dta_benchmark_begin(), so a rule that blows its resource budget aborts
  # from inside that window -- unlike the file.exists() check at the top of
  # validate_file_stream(), which aborts BEFORE begin() and so cannot
  # exercise this guard.
  rule <- DTARuleColUnique(id = "k_budget", columns = "K")
  specs <- vc_specs(
    list(DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = FALSE)),
    list(rule)
  )
  path <- bm_write_csv(data.frame(
    K = c("key1", "key2", "key3", "key4"), stringsAsFactors = FALSE
  ))
  on.exit(unlink(path), add = TRUE)

  old <- getOption("DTAtools.max_unique_keys")
  on.exit(options(DTAtools.max_unique_keys = old), add = TRUE)
  options(DTAtools.max_unique_keys = 2L)

  # A budget this small also crosses the (unrelated) warn-before-abort
  # threshold on the way, which is expected and not the point of this test.
  expect_error(
    suppressWarnings(
      validate_file_stream(specs, path, verbose = FALSE, benchmark = TRUE, batch_rows = 1L)
    ),
    class = "dta_stream_budget_exceeded"
  )
  expect_false(dta_benchmark_env$active)

  options(DTAtools.max_unique_keys = old)

  # The behaviour that actually matters: benchmarking must still work
  # afterwards, not merely report the flag as FALSE.
  no_rules_specs <- vc_specs(
    list(DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = FALSE))
  )
  after <- validate_file_stream(no_rules_specs, path, verbose = FALSE, benchmark = TRUE)
  bm_expect_valid_metrics(attr(after, "benchmark"))
})
