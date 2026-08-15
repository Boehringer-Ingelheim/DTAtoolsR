test_that("dta_arrow_compute_available() returns a single non-NA logical", {
  dta_arrow_reset_cache()
  on.exit(dta_arrow_reset_cache())

  result <- dta_arrow_compute_available()
  expect_type(result, "logical")
  expect_length(result, 1L)
  expect_false(is.na(result))
})

test_that("dta_use_arrow_compute() is FALSE by default", {
  old <- getOption("DTAtools.use_arrow_compute")
  on.exit(options(DTAtools.use_arrow_compute = old))
  options(DTAtools.use_arrow_compute = NULL)

  expect_false(dta_use_arrow_compute())
})

test_that("dta_use_arrow_compute() only enables when capability check also passes", {
  old_opt <- getOption("DTAtools.use_arrow_compute")
  on.exit({
    options(DTAtools.use_arrow_compute = old_opt)
    dta_arrow_reset_cache()
  })

  options(DTAtools.use_arrow_compute = TRUE)
  expect_identical(dta_use_arrow_compute(), dta_arrow_compute_available())
})

test_that("dta_count_duplicates() matches sum(duplicated()) for character keys", {
  df <- data.frame(
    ID = c("a", "b", "a", "c", "b", "b"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for integer keys", {
  df <- data.frame(ID = c(1L, 2L, 1L, 3L, 3L, 3L))
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for repeated NAs", {
  df <- data.frame(ID = c(1L, NA, NA, 2L, NA))
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for multi-column keys", {
  df <- data.frame(
    A = c("x", "x", "y", "x", "y"),
    B = c(1L, 1L, 2L, 1L, 3L),
    stringsAsFactors = FALSE
  )
  expect_identical(
    dta_count_duplicates(df, c("A", "B")),
    sum(duplicated(df[, c("A", "B"), drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for a double key column", {
  df <- data.frame(ID = c(0.1 + 0.2, 0.3, 0.3, 1.5, 1.5, 1.5))
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for a factor key column", {
  df <- data.frame(
    ID = factor(c("a", "b", "a", "c", "b", "b"), levels = c("a", "b", "c"))
  )
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for a logical key column", {
  df <- data.frame(ID = c(TRUE, FALSE, TRUE, NA, FALSE, TRUE))
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for a Date key column", {
  df <- data.frame(
    ID = as.Date(c(
      "2020-01-01", "2020-01-02", "2020-01-01",
      "2020-01-03", "2020-01-02", "2020-01-02"
    ))
  )
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for a POSIXct key column", {
  df <- data.frame(
    ID = as.POSIXct(c(
      "2020-01-01 10:00:00", "2020-01-01 11:00:00", "2020-01-01 10:00:00",
      "2020-01-01 12:00:00", "2020-01-01 11:00:00", "2020-01-01 11:00:00"
    ), tz = "UTC")
  )
  expect_identical(
    dta_count_duplicates(df, "ID"),
    sum(duplicated(df[, "ID", drop = FALSE]))
  )
})

test_that("dta_count_duplicates() matches sum(duplicated()) for a mixed character+integer key", {
  df <- data.frame(
    A = c("x", "x", "y", "x", "y", "x"),
    B = c(1L, 1L, 2L, 1L, 3L, 2L),
    stringsAsFactors = FALSE
  )
  expect_identical(
    dta_count_duplicates(df, c("A", "B")),
    sum(duplicated(df[, c("A", "B"), drop = FALSE]))
  )
})

test_that("Arrow branch produces the same count as the R path when forced on", {
  testthat::skip_if_not(dta_arrow_compute_available())

  old_use <- getOption("DTAtools.use_arrow_compute")
  old_min_rows <- getOption("DTAtools.arrow_min_rows")
  on.exit(options(
    DTAtools.use_arrow_compute = old_use,
    DTAtools.arrow_min_rows = old_min_rows
  ))

  set.seed(42)
  n <- 2000L
  df <- data.frame(
    ID = sample.int(n / 2, n, replace = TRUE),
    GROUP = sample(letters[1:5], n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  expected <- sum(duplicated(df[, c("ID", "GROUP"), drop = FALSE]))

  options(DTAtools.use_arrow_compute = TRUE, DTAtools.arrow_min_rows = 10L)
  arrow_result <- dta_count_duplicates(df, c("ID", "GROUP"))

  expect_identical(arrow_result, expected)
})

test_that("set_dta_compute_threads(NULL) does not change arrow::cpu_count()", {
  testthat::skip_if_not(dta_arrow_compute_available())

  before <- arrow::cpu_count()
  result <- set_dta_compute_threads()
  after <- arrow::cpu_count()

  expect_identical(after, before)
  expect_identical(result, before)
})

test_that("set_dta_compute_threads() sets and restores arrow::cpu_count()", {
  testthat::skip_if_not(dta_arrow_compute_available())

  original <- arrow::cpu_count()
  on.exit(arrow::set_cpu_count(original))

  target <- if (original == 1L) 2L else 1L
  previous <- set_dta_compute_threads(target)

  expect_identical(previous, original)
  expect_identical(arrow::cpu_count(), as.integer(target))
})

test_that("set_dta_compute_threads() aborts on invalid input", {
  expect_error(
    set_dta_compute_threads(0),
    class = "dta_invalid_thread_count"
  )
  expect_error(
    set_dta_compute_threads(-1),
    class = "dta_invalid_thread_count"
  )
  expect_error(
    set_dta_compute_threads("two"),
    class = "dta_invalid_thread_count"
  )
  expect_error(
    set_dta_compute_threads(c(1, 2)),
    class = "dta_invalid_thread_count"
  )
})
