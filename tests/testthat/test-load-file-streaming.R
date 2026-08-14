# Tests for the `stream` argument on load_file(): the choice between reading a
# file into memory and keeping it lazy for check() to scan in batches.
#
# The load-bearing claim is EQUIVALENCE. A user picking streaming is choosing a
# memory profile, not a different answer, so most of what follows compares the
# two paths on the same file rather than asserting fixed numbers -- a fixture
# whose error counts change later should not make these tests lie.

extdata_file <- function(filename) {
  path <- system.file("extdata", filename, package = "DTAtools")
  expect_true(nzchar(path), info = paste(filename, "missing from extdata"))
  path
}

# A dataset wired to one CSV handler, built fresh so each load starts clean.
tabular_fixture <- function(filename = "clinical_data.csv") {
  DTADataSetTabular(
    name = "demo",
    specs = create_example_DTAColumnSpecCollection(1),
    files = list(DTAFileCSV(filename = filename))
  )
}

details_after_check <- function(ds, table_name, ...) {
  checked <- check(ds, quiet = TRUE, persist = FALSE, ...)
  checked@validation_store[[table_name]]
}

# ---- mode resolution --------------------------------------------------------

test_that("dta_resolve_stream_mode maps every accepted spelling to a decision", {
  file <- extdata_file("clinical_data.csv")

  expect_true(dta_resolve_stream_mode("always", file))
  expect_false(dta_resolve_stream_mode("never", file))
  # Logical aliases, because a logical is what most callers reach for first.
  expect_true(dta_resolve_stream_mode(TRUE, file))
  expect_false(dta_resolve_stream_mode(FALSE, file))
})

test_that("auto decides on file size against the threshold option", {
  file <- extdata_file("clinical_data.csv")

  # The bundled fixture is a few hundred kB, far under the 512 MB default.
  expect_false(dta_resolve_stream_mode("auto", file))

  withr_threshold <- getOption("DTAtools.stream_threshold")
  on.exit(options(DTAtools.stream_threshold = withr_threshold), add = TRUE)

  # Drop the threshold below the fixture's size and the same file now streams,
  # which pins that "auto" reads the option rather than a compiled-in constant.
  options(DTAtools.stream_threshold = 10)
  expect_true(dta_resolve_stream_mode("auto", file))

  options(DTAtools.stream_threshold = file.size(file) * 10)
  expect_false(dta_resolve_stream_mode("auto", file))
})

test_that("auto falls back to reading into memory when the file cannot be sized", {
  # A file that is not there cannot be judged too big, and the safe answer is
  # the historical one. (The missing file is reported later, by the reader.)
  expect_false(dta_resolve_stream_mode("auto", file.path(tempdir(), "no-such-file.csv")))
  expect_false(dta_resolve_stream_mode("auto", NULL))
})

test_that("an unrecognised stream value aborts naming the valid ones", {
  file <- extdata_file("clinical_data.csv")

  # Package-authored cli strings, not translated base R messages.
  expect_error(dta_resolve_stream_mode("sometimes", file), "must be one of")
  expect_error(dta_resolve_stream_mode("sometimes", file), "always")
  expect_error(dta_resolve_stream_mode(NA, file), "single non-missing")
  expect_error(dta_resolve_stream_mode(c(TRUE, FALSE), file), "single non-missing")
  expect_error(dta_resolve_stream_mode(1L, file), "must be one of")
})

test_that("load_file honours the session-wide stream option", {
  file <- extdata_file("clinical_data.csv")
  previous <- getOption("DTAtools.stream")
  on.exit(options(DTAtools.stream = previous), add = TRUE)

  options(DTAtools.stream = "always")
  ds <- load_file(tabular_fixture(), file = file, handler_index = 1)
  expect_true(dta_table_is_lazy(tables(ds)[["clinical_data"]]))

  options(DTAtools.stream = "never")
  ds <- load_file(tabular_fixture(), file = file, handler_index = 1)
  expect_false(dta_table_is_lazy(tables(ds)[["clinical_data"]]))
})

# ---- what load_file actually stores -----------------------------------------

test_that("stream chooses between a materialised Table and a lazy Dataset", {
  file <- extdata_file("clinical_data.csv")

  eager <- load_file(tabular_fixture(), file = file, handler_index = 1, stream = "never")
  lazy <- load_file(tabular_fixture(), file = file, handler_index = 1, stream = "always")

  eager_table <- tables(eager)[["clinical_data"]]
  lazy_table <- tables(lazy)[["clinical_data"]]

  expect_false(dta_table_is_lazy(eager_table))
  expect_true(dta_table_is_lazy(lazy_table))

  # The lazy form must stay a Dataset rather than becoming an
  # `arrow_dplyr_query`: only a Dataset has `$files`, which is what
  # dta_table_change_signal() fingerprints so check() can skip an unchanged
  # table. A query would silently revalidate on every run.
  expect_true(inherits(lazy_table, "Dataset"))
  expect_false(is.null(dta_table_change_signal(lazy_table)))

  # Lazy means "not read yet", not "cannot be read".
  expect_equal(nrow(as.data.frame(lazy_table)), nrow(as.data.frame(eager_table)))
})

test_that("both paths present the same columns in the same order", {
  file <- extdata_file("clinical_data.csv")

  eager <- load_file(tabular_fixture(), file = file, handler_index = 1, stream = "never")
  lazy <- load_file(tabular_fixture(), file = file, handler_index = 1, stream = "always")

  expect_identical(
    names(tables(lazy)[["clinical_data"]]),
    names(tables(eager)[["clinical_data"]])
  )
})

test_that("a quoted or padded header is trimmed on the lazy path too", {
  # The eager reader trims via `names<-`; an Arrow Dataset has no `names<-`, so
  # the lazy opener has to supply cleaned names at open time. If it does not,
  # the same file matches the column specification on one path only.
  path <- file.path(tempdir(), "padded_header.csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c('" AGE ","GENDER "', "42,M", "51,F"), path)

  dataset <- dta_open_normalized_dataset(path, delim = ",", quote = '"', has_header = TRUE)

  expect_identical(names(dataset), c("AGE", "GENDER"))
  # `skip = 1` must drop the header rather than leaving it as a data row.
  expect_equal(nrow(as.data.frame(dataset)), 2L)
  expect_identical(as.data.frame(dataset)$GENDER, c("M", "F"))
  # Re-opening must not have cost the change signal.
  expect_true(inherits(dataset, "Dataset"))
  expect_false(is.null(dta_table_change_signal(dataset)))
})

test_that("a header needing no cleaning is opened once and left alone", {
  path <- extdata_file("clinical_data.csv")
  dataset <- dta_open_normalized_dataset(path, delim = ",", quote = '"', has_header = TRUE)

  expect_true(inherits(dataset, "Dataset"))
  expect_identical(names(dataset), dta_clean_column_names(names(dataset)))
})

# ---- equivalence of the verdict ---------------------------------------------

test_that("streaming and in-memory loads agree on every validation axis", {
  # The all-errors fixture exercises all three axes at once; a fixture that
  # passed everything would prove nothing about the error paths.
  file <- extdata_file("clinical_data_error_all.csv")
  table_name <- "clinical_data_error_all"

  make <- function(stream) {
    load_file(
      tabular_fixture("clinical_data_error_all.csv"),
      file = file, handler_index = 1, stream = stream
    )
  }

  eager <- details_after_check(make("never"), table_name)
  lazy <- details_after_check(make("always"), table_name)

  for (field in c(
    "ok", "columnspec_valid", "rules_valid", "import_valid",
    "n_columnspec_errors", "n_rule_errors", "n_import_errors"
  )) {
    expect_identical(
      lazy[[field]], eager[[field]],
      info = paste("streaming disagreed with in-memory on", field)
    )
  }

  # A fixture that found nothing would make the comparison above vacuous.
  expect_false(eager$ok)
  expect_gt(eager$n_columnspec_errors, 0)
})

test_that("the error counts are integers whether or not anything failed", {
  # These are compared with identical() across the two paths above, so the
  # storage type has to be stable -- including in the zero case, where the
  # in-memory path used to return a double while the streaming path returned an
  # integer.
  #
  # The clean case needs the YAML specs: clinical_data.csv does NOT satisfy
  # create_example_DTAColumnSpecCollection(1), which is what makes it useful as
  # a failure fixture elsewhere in this file.
  clean_dta <- load_file(
    read_dta_from_yaml(extdata_file("clinical_dta.yaml")), 1,
    file = extdata_file("clinical_data.csv")
  )
  clean <- check(clean_dta, quiet = TRUE, persist = FALSE)@datasets[[1]]@validation_store[["clinical_data"]]

  failing <- details_after_check(
    load_file(
      tabular_fixture("clinical_data_error_all.csv"),
      file = extdata_file("clinical_data_error_all.csv"), handler_index = 1
    ),
    "clinical_data_error_all"
  )

  # The zero case is the one that regressed, so establish it really is zero
  # before pinning the type of a zero.
  expect_identical(clean$n_columnspec_errors, 0L)
  expect_gt(failing$n_columnspec_errors, 0)

  for (field in c("n_columnspec_errors", "n_rule_errors", "n_import_errors")) {
    expect_type(clean[[field]], "integer")
    expect_type(failing[[field]], "integer")
  }
})

test_that("streaming finds the same import errors, only later", {
  file <- extdata_file("clinical_data_error_import.csv")
  table_name <- "clinical_data_error_import"

  make <- function(stream) {
    load_file(
      tabular_fixture("clinical_data_error_import.csv"),
      file = file, handler_index = 1, stream = stream
    )
  }

  eager <- make("never")
  lazy <- make("always")

  # The documented difference in WHEN: the in-memory path coerces during
  # load_file() and records the issues on the dataset; the lazy path has no rows
  # yet, so there is nothing to record until check() pulls batches through.
  expect_gt(nrow(eager@import_issues[[table_name]]), 0)
  expect_null(lazy@import_issues[[table_name]])

  # ... and the sameness of WHAT, once check() has run.
  expect_identical(
    details_after_check(lazy, table_name)$n_import_errors,
    details_after_check(eager, table_name)$n_import_errors
  )
  expect_gt(details_after_check(eager, table_name)$n_import_errors, 0)
})

test_that("batch size changes how the file is scanned, not what is found", {
  file <- extdata_file("clinical_data_error_all.csv")
  table_name <- "clinical_data_error_all"

  make <- function() {
    load_file(
      tabular_fixture("clinical_data_error_all.csv"),
      file = file, handler_index = 1, stream = "always"
    )
  }

  one_batch <- details_after_check(make(), table_name, batch_rows = 131072L)
  # 7 forces many batches over a 500-row fixture, so any error whose detection
  # depends on batch boundaries -- or any row number computed per batch rather
  # than per file -- shows up as a disagreement here.
  many_batches <- details_after_check(make(), table_name, batch_rows = 7L)

  for (field in c(
    "ok", "columnspec_valid", "rules_valid", "import_valid",
    "n_columnspec_errors", "n_rule_errors", "n_import_errors"
  )) {
    expect_identical(
      many_batches[[field]], one_batch[[field]],
      info = paste("batch size changed", field)
    )
  }
})

test_that("max_errors caps retained detail without changing the verdict", {
  file <- extdata_file("clinical_data_error_all.csv")
  table_name <- "clinical_data_error_all"

  make <- function() {
    load_file(
      tabular_fixture("clinical_data_error_all.csv"),
      file = file, handler_index = 1, stream = "always"
    )
  }

  uncapped <- details_after_check(make(), table_name)
  capped <- details_after_check(make(), table_name, max_errors = 3L)

  expect_identical(capped$ok, uncapped$ok)
  expect_identical(capped$n_columnspec_errors, uncapped$n_columnspec_errors)
  expect_identical(capped$n_rule_errors, uncapped$n_rule_errors)

  # The cap bounds retained rows, so it must not exceed the uncapped detail.
  capped_detail <- as.data.frame(capped$columnspec_errors$full_error)
  uncapped_detail <- as.data.frame(uncapped$columnspec_errors$full_error)
  expect_lte(nrow(capped_detail), nrow(uncapped_detail))
})

# ---- the DTA-level entry point ----------------------------------------------

test_that("stream reaches the dataset through a DTA object", {
  spec_path <- extdata_file("clinical_dta.yaml")
  fixture <- extdata_file("clinical_data.csv")

  load_dta <- function(stream) {
    load_file(read_dta_from_yaml(spec_path), 1, file = fixture, stream = stream)
  }

  lazy <- load_dta("always")
  eager <- load_dta("never")

  expect_true(dta_table_is_lazy(tables(lazy@datasets[[1]])[["clinical_data"]]))
  expect_false(dta_table_is_lazy(tables(eager@datasets[[1]])[["clinical_data"]]))

  # check() on the DTA must drive the scan and reach the same verdict the
  # in-memory path reaches. Getting there through the object model, rather than
  # through validate_file_stream(), is the whole point of the feature.
  reported <- function(dta) {
    res <- results(check(dta, quiet = TRUE, persist = FALSE))
    res[, c(
      "dataset", "target", "target_type", "status",
      "n_columnspec_errors", "n_rule_errors", "n_import_errors"
    )]
  }

  lazy_res <- reported(lazy)
  expect_equal(nrow(lazy_res), 1)
  expect_identical(lazy_res, reported(eager))
})

# ---- open_file itself --------------------------------------------------------

test_that("open_file rejects a file whose name does not match the handler", {
  file <- extdata_file("clinical_data.csv")
  handler <- DTAFileCSV(filename = "something_else.csv")

  expect_error(open_file(handler, file), "does not match the filename")
  # namecheck = FALSE is the documented way past it.
  expect_true(inherits(open_file(handler, file, namecheck = FALSE), "Dataset"))
})

test_that("open_file rejects a path that does not exist", {
  handler <- DTAFileCSV(filename = "clinical_data.csv")
  missing <- file.path(tempdir(), "clinical_data.csv")
  expect_false(file.exists(missing))

  expect_error(open_file(handler, missing), "cannot be found")
})

test_that("a handler with no lazy opener says so and names the way out", {
  file <- extdata_file("clinical_data.csv")

  # A bare DTAFile has not declared how it is delimited, so it cannot be
  # scanned. The message has to point at the path that does work.
  expect_error(open_file(DTAFile("clinical_data.csv"), file), "not supported")
  expect_error(open_file(DTAFile("clinical_data.csv"), file), "never")
})

test_that("a missing file argument blames the generic that was actually called", {
  # dta_reader_args() is shared by read_file_execution() and
  # open_file_execution(), both exported. Naming the wrong one sends whoever hit
  # it looking in the wrong place.
  handler <- DTAFileCSV(filename = "clinical_data.csv")

  expect_error(open_file_execution(handler), "open_file_execution")
  expect_error(read_file_execution(handler), "read_file_execution")
})

test_that("the TSV handler streams with its own delimiter", {
  file <- extdata_file("gf_data_small_smirna.tsv")
  handler <- DTAFileTSV(filename = "gf_data_small_smirna.tsv")

  lazy <- open_file(handler, file)
  eager <- read_file(handler, file)

  expect_true(inherits(lazy, "Dataset"))
  # A tab-delimited file opened with the wrong delimiter yields a single
  # column, so matching the eager reader's shape is the real check here.
  expect_identical(names(lazy), names(eager))
  expect_gt(length(names(lazy)), 1)
})

test_that("a gzipped file can be opened lazily", {
  file <- extdata_file("clinical_data2.csv.gz")
  handler <- DTAFileCSV(filename = "clinical_data2.csv.gz")

  lazy <- open_file(handler, file)
  expect_true(inherits(lazy, "Dataset"))
  expect_identical(names(lazy), names(read_file(handler, file)))
})
