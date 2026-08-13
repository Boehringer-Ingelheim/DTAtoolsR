test_that("DTADataSet object is created from examples", {
  ds1 <- create_example_DTADataSetTabular(1)
  expect_s3_class(ds1, "DTAtools::DTADataSet")

  ds2 <- create_example_DTADataSetTabular(2)
  expect_s3_class(ds2, "DTAtools::DTADataSet")

  ds3 <- create_example_DTADataSetTabular(3)
  expect_s3_class(ds3, "DTAtools::DTADataSet")
})

test_that("DTADataSet object is loaded from yaml", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)
  expect_s3_class(ds, "DTAtools::DTADataSet")
  expect_s3_class(ds@files[[1]], "DTAtools::DTAFileTSV")
  expect_true(is.list(tables(ds)))
  expect_length(tables(ds), 0)
})


test_that("DTADataSet object is created and table can be loaded", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)

  table_path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  expect_equal(max_number_of_files(ds), 1)
  expect_equal(min_number_of_files(ds), 1)

  expect_s3_class(ds, "DTAtools::DTADataSet")

  tab <- read_file(ds@files[[1]], table_path)

  expect_error(read_file(ds@files[[1]], "blala.tsv"), "does not match the filename")

  expect_s3_class(tab, c("R6", "Table", "ArrowTabular", "ArrowObject"))
  expect_equal(nrow(tab), 490)
  expect_equal(ncol(tab), 33)

  expect_true(is.list(files(ds)))
  expect_s3_class(files(ds)[[1]], "DTAtools::DTAFileTSV")

  expect_s3_class(specs(ds), "DTAtools::DTAColumnSpecCollection")

  expect_true(is.list(tables(ds)))

  expect_s3_class(colspec(ds, 1), "DTAtools::DTAColumnSpec")
  expect_s3_class(colspec(ds, "STUDYID"), "DTAtools::DTAColumnSpec")
})

test_that("read_dataset_from_yaml aborts for non-existent yaml file", {
  expect_error(
    read_dataset_from_yaml(file.path(tempdir(), "does-not-exist-dataset.yaml")),
    "does not exist"
  )
})

test_that("DTADataSetTabular can be created with empty tables", {
  ds <- DTADataSetTabular(
    name = "empty_tables",
    specs = create_example_DTAColumnSpecCollection(1),
    tables = list()
  )

  expect_s3_class(ds, "DTAtools::DTADataSetTabular")
  expect_true(is.list(tables(ds)))
  expect_length(tables(ds), 0)
})

test_that("DTADataSet keeps description and template metadata", {
  # The constructor accepted these four arguments and then dropped them on the
  # floor: new_object() forwarded only name, type and files.
  ds <- DTADataSet(
    name = "metadata_dataset",
    type = "file",
    files = list(create_example_DTAFileCSV()),
    description = "a described dataset",
    template_source = "unit test template",
    template_version = "9.9",
    template_date = "2024-12-17"
  )

  expect_equal(ds@description, "a described dataset")
  expect_equal(ds@template_source, "unit test template")
  expect_equal(ds@template_version, "9.9")
  expect_equal(ds@template_date, "2024-12-17")
})

test_that("template metadata from YAML reaches the DTADataSet", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)

  expect_equal(ds@template_version, "3.0")
  expect_equal(ds@template_source, "GF domain smrnaseq")
  expect_equal(ds@template_date, "2024-12-17")
  expect_match(ds@description, "Genomic Findings")
})

test_that("print_info renders the dataset template metadata", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)

  out <- paste(cli::cli_fmt(print_info(ds)), collapse = "\n")

  expect_match(out, "Genomic Findings")
  expect_match(out, "GF domain smrnaseq")
  expect_match(out, "3.0", fixed = TRUE)
  expect_match(out, "2024-12-17", fixed = TRUE)
})

test_that("print_short_info includes the dataset name", {
  ds <- DTADataSet(
    name = "short_info_dataset",
    type = "file",
    files = list(create_example_DTAFileCSV())
  )

  # Interpolated names(x@name) -- always NULL -- so the name was missing and
  # the line read "Files:  (1 file, file)".
  out <- paste(cli::cli_fmt(print_short_info(ds)), collapse = "\n")

  expect_match(out, "short_info_dataset", fixed = TRUE)
  expect_match(out, "1 file", fixed = TRUE)
})

test_that("colspec() errors for out-of-bounds numeric index", {
  ds <- create_example_DTADataSetTabular(2)

  # A bare expect_error() accepts ANY error, including a typo in the call
  # itself. Assert the *kind* of error rather than its text: base R messages
  # are translated (this machine reports them in German), so matching the
  # English wording would make the test locale-dependent.
  #
  # DEFECT, pinned rather than endorsed: unlike get_table(), which aborts with
  # a cli message ("Index {id} is out of bounds."), colspec() has no bounds
  # check and leaks a raw base R subscript error. When a cli_abort is added
  # this expectation SHOULD fail -- change it to
  # expect_error(colspec(ds, 999), class = "rlang_error").
  expect_error(colspec(ds, 999), class = "subscriptOutOfBoundsError")
})

test_that("labels() is exported, not left to base::labels' silent fallback", {
  # The defect: `labels <- new_generic("labels", "x")` was defined and had a
  # method registered for DTADataSetTabular, but no `@export` ever reached the
  # generic itself, so it never made it into NAMESPACE. In an INSTALLED
  # package (export_all = FALSE is the real, non-dev behaviour that
  # pkgload::load_all()'s default of TRUE merely papers over) an unexported
  # `labels()` is invisible to a caller; the call falls through to
  # base::labels.default, which does not error -- it silently returns a
  # plausible-looking wrong value. getNamespaceExports() reports the real
  # NAMESPACE-declared exports regardless of load_all()'s export_all default,
  # so this check catches the silent fallback without needing to flip global
  # session state.
  exported <- getNamespaceExports(asNamespace("DTAtools"))
  expect_true("labels" %in% exported)

  ds <- create_example_DTADataSetTabular(2)
  expect_identical(labels(ds), names(tables(ds)))

  # The fix must not achieve export by replacing base::labels with an
  # independent S7 generic that has no method for anything else: that would
  # newly break `labels()` on every other object type once the package is
  # attached (library(DTAtools) masks base::labels only for the classes it
  # actually handles). The correct fix extends the existing base generic, so
  # `labels` in this package IS base::labels, and base's own dispatch for
  # unrelated classes keeps working.
  expect_identical(labels, base::labels)
  m <- lm(mpg ~ wt, data = mtcars)
  expect_equal(labels(m), "wt")
})


# ---- Multiple file handlers per dataset -------------------------------------
# A DTADataSet has always been able to hold several DTAFile handlers in memory,
# but the reader could only ever build ONE: DTADataSetFactory() called
# do.call(DTAFileFactory, files), which requires `files` to be a single mapping.
# A `files:` SEQUENCE -- the shape a multi-handler document has, and the shape
# the Shiny app already serialises to -- passed the whole sequence as `type` and
# died in base R's coercion, so a two-handler specification could be written but
# never read back.

test_that("a files: sequence builds one handler per entry, in order", {
  ds <- dta_dataset_from_list(list(
    name = "two_handlers",
    type = "tabular",
    files = list(
      list(type = "csv", filename = "a.csv"),
      list(type = "tsv", filename = "b.tsv")
    ),
    columns = list(list(id = "STUDYID", type = "SAS Char"))
  ))

  expect_length(ds@files, 2)
  expect_s3_class(ds@files[[1]], "DTAtools::DTAFileCSV")
  expect_s3_class(ds@files[[2]], "DTAtools::DTAFileTSV")
  expect_identical(ds@files[[1]]@filename, "a.csv")
  expect_identical(ds@files[[2]]@filename, "b.tsv")
})

test_that("a one-element files: sequence is a sequence, not a mapping", {
  # The regression that hid the defect: even a single-entry sequence failed,
  # because the failure was in how `files` was passed on, not in how many
  # handlers were asked for.
  ds <- dta_dataset_from_list(list(
    name = "one_in_a_sequence",
    type = "tabular",
    files = list(list(type = "csv", filename = "a.csv")),
    columns = list(list(id = "STUDYID", type = "SAS Char"))
  ))

  expect_length(ds@files, 1)
  expect_identical(ds@files[[1]]@filename, "a.csv")
})

test_that("a files: mapping still builds exactly one handler", {
  # gf_dataset.yaml is the guard for the discriminator: its `files:` mapping
  # CONTAINS a sequence (info:), so anything that decided "sequence" by looking
  # for a nested list rather than for names would split it into bogus handlers.
  ds <- read_dataset_from_yaml(
    system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  )

  expect_length(ds@files, 1)
  expect_s3_class(ds@files[[1]], "DTAtools::DTAFileTSV")
  expect_identical(ds@files[[1]]@filename, "gf_data_small_smirna.tsv")
})

test_that("a dataset without a files: block has no handlers", {
  # Removing the last file handler is reachable from the app's file editor, and
  # the resulting document must round-trip rather than abort in do.call().
  ds <- dta_dataset_from_list(list(
    name = "no_handlers",
    type = "tabular",
    columns = list(list(id = "STUDYID", type = "SAS Char"))
  ))

  expect_length(ds@files, 0)
  expect_equal(min_number_of_files(ds), 0)
  expect_equal(max_number_of_files(ds), 0)
})

test_that("dta_file_handlers_from_list() rejects a malformed sequence entry", {
  expect_error(
    dta_file_handlers_from_list(list(list(type = "csv", filename = "a.csv"), "b.csv")),
    "must be a named list"
  )
  expect_error(
    dta_file_handlers_from_list("clinical_data.csv"),
    "must be a list describing one file handler"
  )
})

test_that("min/max number of files sum across several handlers", {
  ds <- dta_dataset_from_list(list(
    name = "counts",
    type = "tabular",
    files = list(
      list(type = "csv", filename = "a.csv"),
      list(
        type = "csv", filename = "extra.*[.]csv$", pattern = TRUE,
        min_number_of_files = 1, max_number_of_files = 3
      )
    ),
    columns = list(list(id = "STUDYID", type = "SAS Char"))
  ))

  expect_equal(min_number_of_files(ds), 2)
  expect_equal(max_number_of_files(ds), 4)
})


test_that("dta_file_handlers_from_list rejects a half-named files: block", {
  # A fully named list is one handler, a fully unnamed one is a list of
  # handlers. A mix is neither, and forwarding it whole to DTAFileFactory would
  # fail further in with a message about the wrong thing.
  expect_error(
    dta_file_handlers_from_list(list(type = "csv", "clinical_data.csv")),
    "not a mix of named and unnamed entries"
  )
})
