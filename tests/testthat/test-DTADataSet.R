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
