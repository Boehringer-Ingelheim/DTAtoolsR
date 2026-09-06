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

test_that("a header needing no cleaning keeps its names through the pinned re-open", {
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

test_that("streaming load_file rejects non-scalar file paths early", {
  file <- extdata_file("clinical_data.csv")

  expect_error(
    load_file(
      tabular_fixture(),
      file = c(file, file),
      handler_index = 1,
      stream = "always"
    ),
    "single non-missing, non-empty path"
  )

  expect_error(
    load_file(
      tabular_fixture(),
      file = NA_character_,
      handler_index = 1,
      stream = "always"
    ),
    "single non-missing, non-empty path"
  )
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

# ---- the read block a delimited scan is batched in ---------------------------
# `batch_rows` reaches Scanner$create(batch_size = ), which only SLICES a batch
# that is already larger. On a delimited file the batch is one read block, so
# `batch_rows` never enlarged anything and memory did not respond to it at all.
# DTAtools.stream_block_size is the knob that does.

# Rows per batch, at a batch_size high enough that only the block size can
# decide. Consuming the reader is the point: a Dataset reports nothing about
# how it will be batched until it is scanned.
stream_batch_rows <- function(dataset, batch_size = 1e6L) {
  reader <- arrow::Scanner$create(dataset, batch_size = batch_size)$ToRecordBatchReader()
  rows <- integer(0)
  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) break
    rows <- c(rows, batch$num_rows)
  }
  rows
}

# ~4 MiB of text: several default blocks, one 4 MiB block.
stream_block_fixture <- function() {
  path <- file.path(tempdir(), "stream_block_size.csv")
  if (!file.exists(path) || file.size(path) < 4 * 1024^2) {
    line <- paste(rep("x", 90), collapse = "")
    writeLines(
      c("ID,PAD", sprintf("%07d,%s", seq_len(46000), line)),
      path
    )
  }
  path
}

test_that("the block size option decides how a delimited scan is batched", {
  path <- stream_block_fixture()
  on.exit(unlink(path), add = TRUE)
  expect_gt(file.size(path), 4 * 1024^2)

  handler <- DTAFileCSV(filename = basename(path))

  default_rows <- stream_batch_rows(open_file(handler, path))
  # Several batches, none of them the whole file: this is what `batch_rows`
  # could never change.
  expect_gt(length(default_rows), 1)

  withr::local_options(DTAtools.stream_block_size = 8L * 1024L^2)
  one_block_rows <- stream_batch_rows(open_file(handler, path))

  expect_length(one_block_rows, 1)
  expect_identical(sum(one_block_rows), sum(default_rows))
})

test_that("an unusable block size is rejected rather than silently ignored", {
  withr::local_options(DTAtools.stream_block_size = 0)
  expect_error(dta_stream_block_size(), "stream_block_size")

  withr::local_options(DTAtools.stream_block_size = "1MB")
  expect_error(dta_stream_block_size(), "stream_block_size")

  withr::local_options(DTAtools.stream_block_size = NA)
  expect_error(dta_stream_block_size(), "stream_block_size")
})

test_that("the default block size is arrow's own", {
  expect_identical(dta_stream_block_size(), 1048576L)
})

# ---- quoted line breaks ------------------------------------------------------
# A quoted newline only breaks a read when it straddles a block boundary, so a
# small fixture proves nothing: the file below is deliberately larger than one
# default block, with the offending value placed past it.

# Every row's second field is quoted around a line break, so wherever a block
# boundary falls it falls inside one -- placing a single such value would leave
# the test dependent on where 1 MiB happens to land.
newline_fixture <- function() {
  path <- file.path(tempdir(), "stream_newlines.csv")
  if (!file.exists(path) || file.size(path) < 1.5 * 1024^2) {
    half <- paste(rep("a", 45), collapse = "")
    writeLines(
      c("ID,PAD", sprintf('%07d,"%s\n%s"', seq_len(18000), half, half)),
      path
    )
  }
  path
}

test_that("a quoted line break past the first block needs the handler to declare it", {
  path <- newline_fixture()
  on.exit(unlink(path), add = TRUE)
  expect_gt(file.size(path), 1.5 * 1024^2)

  plain <- DTAFileCSV(filename = basename(path))
  declaring <- DTAFileCSV(filename = basename(path), newlines_in_values = TRUE)

  # Undeclared, a quoted value crosses a block boundary and both paths refuse
  # the file -- the same file, the same failure, which is the point. Arrow's
  # own English, not a translated base-R message.
  eager_error <- expect_error(
    as.data.frame(read_file(plain, path)),
    regexp = "CSV parse error"
  )
  lazy_error <- expect_error(
    as.data.frame(open_file(plain, path)),
    regexp = "CSV parse error"
  )
  # The same diagnosis on both paths -- not the same bytes: Arrow's in-memory
  # reader reports the row number only when the failing block was parsed on
  # the thread that counts rows, which differs between platforms.
  expect_match(conditionMessage(eager_error), "Expected 2 columns, got 1", fixed = TRUE)
  expect_match(conditionMessage(lazy_error), "Expected 2 columns, got 1", fixed = TRUE)

  eager <- as.data.frame(read_file(declaring, path))
  lazy <- as.data.frame(open_file(declaring, path))

  expect_equal(nrow(eager), 18000)
  expect_equal(nrow(lazy), 18000)
  # The line break is inside the value, not between rows.
  expect_identical(as.character(eager$PAD[[15000]]), as.character(lazy$PAD[[15000]]))
  expect_match(as.character(eager$PAD[[15000]]), "\n", fixed = TRUE)
})

test_that("a declared quoted line break survives a check on both paths", {
  path <- newline_fixture()
  on.exit(unlink(path), add = TRUE)

  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    PAD = DTAColumnSpec(id = "PAD", type = "SAS Char", length = 200, nullable = FALSE)
  ))

  verdict <- function(stream) {
    ds <- DTADataSetTabular(
      name = "nl",
      specs = specs,
      files = list(DTAFileCSV(filename = basename(path), newlines_in_values = TRUE))
    )
    ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
    checked <- check(ds, quiet = TRUE, persist = FALSE)
    checked@validation_store[[tools::file_path_sans_ext(basename(path))]]
  }

  eager <- verdict("never")
  lazy <- verdict("always")

  for (field in c("ok", "n_columnspec_errors", "n_rule_errors", "n_import_errors")) {
    expect_identical(lazy[[field]], eager[[field]], info = field)
  }
  expect_true(eager$ok)
})

# ---- character encoding ------------------------------------------------------

# Each caller gets its OWN file. The transcoding cache is keyed on the source's
# path, size and modification time, so two tests sharing one path could hand the
# second a copy the first made -- which would make the cache tests pass for the
# wrong reason and the "changed file" test unreproducible.
latin1_fixture <- function(name = "stream_latin1.csv") {
  path <- file.path(tempdir(), name)
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(
    c(
      charToRaw("NAME,V\n"),
      as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(",1\n"),
      charToRaw("Ann,2\n")
    ),
    con
  )
  path
}

test_that("a declared latin1 encoding is decoded by the in-memory reader", {
  path <- latin1_fixture("stream_latin1_eager.csv")
  on.exit(unlink(path), add = TRUE)

  declaring <- DTAFileCSV(filename = basename(path), encoding = "latin1")
  got <- as.data.frame(read_file(declaring, path))

  expect_identical(got$NAME, c("Jürgen", "Ann"))

  # Without the declaration arrow sees invalid UTF-8 and hands back the raw
  # bytes as a binary column -- a file that "loads" and is unusable.
  plain <- as.data.frame(read_file(DTAFileCSV(filename = basename(path)), path))
  expect_false(is.character(plain$NAME))
})

test_that("a declared latin1 encoding is scanned through a UTF-8 copy", {
  path <- latin1_fixture("stream_latin1_lazy.csv")
  on.exit(unlink(path), add = TRUE)

  declaring <- DTAFileCSV(filename = basename(path), encoding = "latin1")
  dataset <- open_file(declaring, path)

  # Arrow's dataset scanner has no re-encoding step -- it opens its own files --
  # so the bytes are converted first and the scan reads the conversion. This
  # used to be a refusal, which made every non-UTF-8 delivery unstreamable.
  expect_identical(as.data.frame(dataset)$NAME, c("Jürgen", "Ann"))

  # Still a Dataset, not an `arrow_dplyr_query`: only a Dataset has `$files`,
  # which is what the change signal fingerprints.
  expect_true(inherits(dataset, "Dataset"))

  scanned <- normalizePath(dataset$files[[1]], winslash = "/")
  expect_false(identical(scanned, normalizePath(path, winslash = "/")))
  # The copy is a temporary, so it dies with the session rather than settling
  # next to the delivery.
  expect_true(startsWith(scanned, normalizePath(tempdir(), winslash = "/")))
})

test_that("the copy's own name does not decide how it is parsed", {
  # The copy is always written as `.csv`, whatever the delivery was called, and
  # Arrow reads a dataset's compression from the file extension. Two things
  # have to hold: a tab- or semicolon-separated source must still be split on
  # its own delimiter, which travels in `parse_options` and not in the name;
  # and a `.gz` delivery must not leave a copy Arrow then tries to decompress
  # a second time.
  fixture <- function(name, sep) {
    path <- file.path(tempdir(), name)
    con <- file(path, "wb")
    on.exit(close(con), add = TRUE)
    writeBin(
      c(
        charToRaw(paste0("NAME", sep, "V\n")),
        as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(paste0(sep, "1\n")),
        charToRaw(paste0("Ann", sep, "2\n"))
      ),
      con
    )
    path
  }

  tsv <- fixture("stream_latin1_tabs.tsv", "\t")
  delim <- fixture("stream_latin1_semis.txt", ";")
  gz <- paste0(tsv, ".gz")
  on.exit(unlink(c(tsv, delim, gz)), add = TRUE)

  bytes <- readBin(tsv, "raw", n = file.size(tsv))
  con <- gzfile(gz, "wb")
  writeBin(bytes, con)
  close(con)

  handlers <- list(
    tsv = list(DTAFileTSV(filename = basename(tsv), encoding = "latin1"), tsv),
    delim = list(
      DTAFileDelim(filename = basename(delim), sep = ";", encoding = "latin1"),
      delim
    ),
    gz = list(DTAFileTSV(filename = basename(tsv), encoding = "latin1"), gz)
  )

  for (nm in names(handlers)) {
    handler <- handlers[[nm]][[1]]
    file <- handlers[[nm]][[2]]

    lazy <- as.data.frame(open_file(handler, file))
    # Two columns, not one: a copy parsed as comma-separated would have made
    # the whole line a single column named "NAME<sep>V".
    expect_identical(names(lazy), c("NAME", "V"), info = nm)
    expect_identical(lazy$NAME, c("Jürgen", "Ann"), info = nm)
    expect_identical(lazy$NAME, as.data.frame(read_file(handler, file))$NAME, info = nm)

    copy <- open_file(handler, file)$files[[1]]
    expect_true(endsWith(copy, ".csv"), info = nm)
  }
})

test_that("a latin1 file gets the same verdict streamed as in memory", {
  path <- file.path(tempdir(), "stream_latin1_verdict.csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(
    c(
      charToRaw("ID,NAME,SCORE\n"),
      charToRaw("001,"), as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(",10\n"),
      charToRaw("002,"), as.raw(c(0x4d, 0xf6, 0x6c, 0x6c, 0x65, 0x72)), charToRaw(",20\n"),
      # An unconvertible number and a duplicated key, so this compares real
      # error detail on all three axes rather than two clean verdicts.
      charToRaw("003,Ann,notanumber\n"),
      charToRaw("003,"), as.raw(c(0x47, 0x72, 0xf6, 0xdf, 0x65, 0x72, 0x65)), charToRaw(",40\n")
    ),
    con
  )
  close(con)

  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 3, nullable = FALSE),
      NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 6, nullable = FALSE),
      SCORE = DTAColumnSpec(id = "SCORE", type = "SAS Num", nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "id_unique", columns = "ID"))
  )

  # Sorted on every column as text: two paths may legitimately report the same
  # errors in a different order, and an order difference is not a disagreement.
  sorted <- function(errors) {
    if (nrow(errors) == 0) {
      return(errors)
    }
    out <- errors[do.call(order, lapply(errors, as.character)), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  run <- function(stream) {
    ds <- DTADataSetTabular(
      name = "enc",
      specs = specs,
      files = list(DTAFileCSV(filename = basename(path), encoding = "latin1"))
    )
    ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
    checked <- check(ds, quiet = TRUE, persist = FALSE)

    status <- validation_status(checked)
    status <- status[
      , setdiff(names(status), c("validated_at", "run_id", "validation_run")),
      drop = FALSE
    ]

    list(
      status = status,
      errors = sorted(as.data.frame(validation_errors(checked, "stream_latin1_verdict"))),
      n_import_errors = checked@validation_store[["stream_latin1_verdict"]]$n_import_errors,
      names = names(tables(checked)[["stream_latin1_verdict"]])
    )
  }

  eager <- run("never")
  lazy <- run("always")

  # A fixture that passed everything would prove nothing about the error paths.
  expect_false(eager$status$ok)
  expect_gt(nrow(eager$errors), 0)
  expect_gt(eager$n_import_errors, 0)

  expect_identical(lazy$status, eager$status)
  expect_identical(lazy$errors, eager$errors)
  expect_identical(lazy$n_import_errors, eager$n_import_errors)
  expect_identical(lazy$names, eager$names)
})

test_that("a second load reuses the transcoded copy, and a changed file does not", {
  path <- latin1_fixture("stream_latin1_cache.csv")
  on.exit(unlink(path), add = TRUE)

  declaring <- DTAFileCSV(filename = basename(path), encoding = "latin1")

  first <- open_file(declaring, path)
  second <- open_file(declaring, path)
  # The conversion is linear in the file, so repeating it for every load_file()
  # on an unchanged delivery is the whole cost of the feature paid twice.
  expect_identical(second$files, first$files)

  # The cache key carries the source's size and modification time, so editing
  # the file must produce a new copy rather than serve the stale one.
  con <- file(path, "ab")
  writeBin(charToRaw("Zoe,3\n"), con)
  close(con)

  third <- open_file(declaring, path)
  expect_false(identical(third$files, first$files))
  expect_equal(nrow(as.data.frame(third)), 3L)

  # The same thing through the entry point a user actually calls, because that
  # is where the cost would be paid: `load_file()` opens the file afresh every
  # time it is called, so without the cache each call would reconvert.
  loaded <- function() {
    ds <- DTADataSetTabular(
      name = "cache",
      specs = DTAColumnSpecCollection(
        columns = list(
          NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 6),
          V = DTAColumnSpec(id = "V", type = "SAS Num")
        )
      ),
      files = list(DTAFileCSV(filename = basename(path), encoding = "latin1"))
    )
    tables(load_file(ds, file = path, handler_index = 1, stream = "always"))[[
      "stream_latin1_cache"
    ]]
  }

  expect_identical(loaded()$files, third$files)
  expect_identical(loaded()$files, loaded()$files)
})

test_that("a wide encoding is refused rather than converted", {
  path <- latin1_fixture("stream_wide.csv")
  on.exit(unlink(path), add = TRUE)

  # UTF-16 puts an ordinary character's bytes either side of what the converter
  # reads as a line break, so it cannot be transcoded a line at a time. A clear
  # refusal beats a plausible-looking wrong answer about someone's data.
  wide <- DTAFileCSV(filename = basename(path), encoding = "UTF-16LE")

  expect_error(open_file(wide, path), "cannot be converted block by block")
  expect_error(open_file(wide, path), "stream = \"never\"", fixed = TRUE)
})

test_that("an encoding name iconv does not know is refused by name", {
  path <- latin1_fixture("stream_bad_encoding_name.csv")
  on.exit(unlink(path), add = TRUE)

  # A misspelling used to reach the user as iconv()'s own error, rendered in
  # the system language and naming neither the file nor the handler that
  # declared it.
  misspelled <- DTAFileCSV(filename = basename(path), encoding = "latin-1")

  expect_error(open_file(misspelled, path), class = "rlang_error")
  expect_error(open_file(misspelled, path), "latin-1", fixed = TRUE)
  expect_error(open_file(misspelled, path), "iconvlist")

  # The in-memory reader hands the name to Arrow, which has its own opinion of
  # it; only the lazy path converts the bytes itself, so only it is this
  # function's to answer for.
  expect_error(read_file(misspelled, path))
})

test_that("bytes that are not the declared encoding are refused, not mangled", {
  path <- file.path(tempdir(), "stream_bad_bytes.csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(c(charToRaw("NAME,V\n"), as.raw(c(0x41, 0x81, 0xff, 0xfe)), charToRaw(",1\n")), con)
  close(con)

  # Not every platform's iconv rejects the same bytes -- Windows' is lenient
  # about several single-byte code pages -- so the assertion runs only where
  # the conversion really does fail.
  probe <- iconv(rawToChar(as.raw(c(0x41, 0x81, 0xff, 0xfe))), from = "SHIFT-JIS", to = "UTF-8")
  skip_if_not(is.na(probe), "this platform's iconv accepts the bytes this test needs rejected")

  declaring <- DTAFileCSV(filename = basename(path), encoding = "SHIFT-JIS")

  expect_error(open_file(declaring, path), "cannot be decoded as")
  # The place is named, because "somewhere in a 60 GB file" is not actionable.
  # A byte offset rather than a line number: the file is cut into blocks.
  expect_error(open_file(declaring, path), "offset 8")
})

test_that("the change signal of a transcoded dataset identifies the delivered file", {
  path <- latin1_fixture("stream_latin1_signal.csv")
  on.exit(unlink(path), add = TRUE)

  declaring <- DTAFileCSV(filename = basename(path), encoding = "latin1")
  dataset <- open_file(declaring, path)

  # The reader's half of the contract, which holds today: the dataset carries
  # the delivered path, not the temporary copy it is actually scanning.
  expect_identical(
    dta_dataset_source_files(dataset),
    normalizePath(path, winslash = "/")
  )

  # The engine's half: `dta_table_change_signal()` reads the stamp rather than
  # `$files`. Read from `$files` the signal describes the copy, whose
  # modification time is merely when the conversion ran -- so the same unchanged
  # delivery looks like a different table in every session and check()
  # revalidates it every time.
  info <- file.info(normalizePath(path, winslash = "/"))
  expect_identical(
    dta_table_change_signal(dataset),
    dta_hash_object(list(
      files = normalizePath(path, winslash = "/"),
      size = info$size,
      mtime = info$mtime,
      columns = names(dataset$schema)
    ))
  )
})

test_that("a quoted line break inside a latin1 value is the same on both paths", {
  # The transcoder is the only thing between the delivery and the lazy scan, so
  # anything it normalises is a difference between the two readers. `readLines()`
  # folds CRLF and a lone CR to LF, which made a quoted "a\r\nb" one character
  # shorter streamed than in memory -- and a `length` check pass on one path and
  # fail on the other.
  path <- file.path(tempdir(), "stream_latin1_crlf.csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(
    c(
      charToRaw("ID,NOTE\r\n"),
      charToRaw("1,\"a\r\nb\"\r\n"),
      charToRaw("2,\"c\rd\"\r\n"),
      charToRaw("3,"), as.raw(c(0x4a, 0xfc)), charToRaw("\r\n")
    ),
    con
  )
  close(con)

  handler <- DTAFileCSV(
    filename = basename(path),
    encoding = "latin1",
    newlines_in_values = TRUE
  )

  eager <- as.data.frame(read_file(handler, path))
  lazy <- as.data.frame(open_file(handler, path))

  expect_identical(lazy$NOTE, eager$NOTE)
  expect_identical(eager$NOTE, c("a\r\nb", "c\rd", "Jü"))
  expect_identical(nchar(eager$NOTE), c(4L, 3L, 2L))
})

test_that("a lazily held latin1 table follows a delivery that changes under it", {
  # The copy is made at load_file() time and the change signal is keyed on the
  # DELIVERY, so an edited file opened the skip gate and the scan then reread
  # the stale copy: a clean verdict, and the old row count, reported as fresh
  # for data that now fails.
  specs <- DTAColumnSpecCollection(columns = list(
    NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 6, nullable = FALSE),
    V = DTAColumnSpec(id = "V", type = "SAS Num", nullable = FALSE)
  ))

  # One row too long for `length = 6`, so the second check has something to
  # fail on that the first could not have seen.
  violating <- c(
    as.raw(c(0x53, 0x63, 0x68, 0x6d, 0xfc, 0x63, 0x6b, 0x65, 0x72)),
    charToRaw(",3\n")
  )

  name_of <- function(path) tools::file_path_sans_ext(basename(path))

  run <- function(name, encoding, first, appended) {
    path <- file.path(tempdir(), name)
    on.exit(unlink(path), add = TRUE)
    con <- file(path, "wb")
    writeBin(c(charToRaw("NAME,V\n"), first, charToRaw("Ann,2\n")), con)
    close(con)

    ds <- DTADataSetTabular(
      name = "follow",
      specs = specs,
      files = list(DTAFileCSV(filename = basename(path), encoding = encoding))
    )
    ds <- load_file(ds, file = path, handler_index = 1, stream = "always")

    before <- check(ds, quiet = TRUE, persist = FALSE)
    rows_before <- nrow(as.data.frame(tables(before)[[name_of(path)]]))

    # A different size and modification time, so the change signal differs.
    Sys.sleep(0.01)
    con <- file(path, "ab")
    writeBin(appended, con)
    close(con)

    after <- check(before, quiet = TRUE, persist = FALSE)
    rows_after <- nrow(as.data.frame(tables(after)[[name_of(path)]]))

    list(
      before = validation_status(before),
      after = validation_status(after),
      rows_before = rows_before,
      rows_after = rows_after
    )
  }

  latin1 <- run(
    "stream_follow_latin1.csv", "latin1",
    first = c(as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(",1\n")),
    appended = violating
  )
  # The control: a UTF-8 delivery is scanned directly, has never had this
  # defect, and is what the transcoded one must now behave like. Its bytes are
  # ASCII, because a latin1 "ü" is not valid UTF-8 and Arrow would refuse the
  # file rather than reach the behaviour under test.
  utf8 <- run(
    "stream_follow_utf8.csv", "UTF-8",
    first = charToRaw("Jurgen,1\n"),
    appended = charToRaw("Schmuecker,3\n")
  )

  for (label in c("latin1", "utf8")) {
    got <- if (label == "latin1") latin1 else utf8

    expect_true(got$before$ok, info = label)
    expect_identical(got$rows_before, 2L, info = label)

    expect_identical(got$after$status, "validated", info = label)
    expect_false(got$after$ok, info = label)
    expect_identical(got$rows_after, 3L, info = label)
    expect_gt(got$after$n_columnspec_errors, 0)
  }
})

test_that("a latin1 header name is decoded for the in-memory reader", {
  path <- file.path(tempdir(), "stream_latin1_header.csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(c(as.raw(c(0x4e, 0x41, 0x4d, 0xc9)), charToRaw(",V\nAnn,1\n")), con)
  close(con)

  # The names come from a dataset open, which does NOT re-encode; without the
  # explicit conversion the eager reader would be handed names that disagree
  # with the ones it decodes for itself.
  got <- read_file(DTAFileCSV(filename = basename(path), encoding = "latin1"), path)
  expect_identical(names(got), c("NAMÉ", "V"))
})

test_that("a latin1 header name is decoded exactly once on the lazy path", {
  path <- file.path(tempdir(), "stream_latin1_header_lazy.csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(c(as.raw(c(0x4e, 0x41, 0x4d, 0xc9)), charToRaw(",V\nAnn,1\n")), con)
  close(con)

  handler <- DTAFileCSV(filename = basename(path), encoding = "latin1")

  # The scan reads a copy that is ALREADY UTF-8, so the header must not be run
  # through iconv a second time: decoding the UTF-8 bytes of "É" as latin1
  # again would give "Ã‰", and the two paths would name the same column
  # differently.
  expect_identical(names(open_file(handler, path)), c("NAMÉ", "V"))
  expect_identical(names(open_file(handler, path)), names(read_file(handler, path)))
})
