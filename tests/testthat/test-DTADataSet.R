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
    id = "empty_tables",
    specs = create_example_DTAColumnSpecCollection(1),
    tables = list()
  )

  expect_s3_class(ds, "DTAtools::DTADataSetTabular")
  expect_true(is.list(tables(ds)))
  expect_length(tables(ds), 0)
})

test_that("DTADataSet keeps name, description and template metadata", {
  # The constructor accepted these arguments and then dropped them on the
  # floor: new_object() forwarded only id, type and files.
  ds <- DTADataSet(
    id = "metadata_dataset",
    type = "file",
    files = list(create_example_DTAFileCSV()),
    name = "Metadata dataset",
    description = "a described dataset",
    template_source = "unit test template",
    template_version = "9.9",
    template_date = "2024-12-17"
  )

  expect_equal(ds@id, "metadata_dataset")
  expect_equal(ds@name, "Metadata dataset")
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
  expect_equal(ds@id, "gf_data_specs_pattern")
  expect_equal(ds@name, "Genomic Findings data table")
  expect_equal(
    ds@description,
    "Genomic Findings data, delivered as tab-separated files matched by a filename pattern."
  )
})

test_that("print_info renders the dataset template metadata", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)

  out <- paste(cli::cli_fmt(print_info(ds)), collapse = "\n")
  # cli wraps to the terminal width; match on whitespace-normalised text.
  flat <- gsub("[[:space:]]+", " ", out)

  expect_match(flat, "- Name: Genomic Findings data table", fixed = TRUE)
  expect_match(flat, "- Description: Genomic Findings data, delivered as", fixed = TRUE)
  expect_match(out, "GF domain smrnaseq")
  expect_match(out, "3.0", fixed = TRUE)
  expect_match(out, "2024-12-17", fixed = TRUE)
})

test_that("print_info lists a name and description only when they are set", {
  ds <- DTADataSet(
    id = "bare_dataset",
    type = "file",
    files = list(create_example_DTAFileCSV())
  )
  out <- paste(cli::cli_fmt(print_info(ds)), collapse = "\n")
  expect_false(grepl("Name:", out, fixed = TRUE))
  expect_false(grepl("Description:", out, fixed = TRUE))

  # The id is the heading of print(), not a line of print_info().
  header <- paste(cli::cli_fmt(print(ds)), collapse = "\n")
  expect_match(header, "bare_dataset", fixed = TRUE)

  ds@name <- "Labelled"
  out <- paste(cli::cli_fmt(print_info(ds)), collapse = "\n")
  expect_match(out, "- Name: Labelled", fixed = TRUE)
  expect_false(grepl("Description:", out, fixed = TRUE))
})

test_that("print_short_info includes the dataset id", {
  ds <- DTADataSet(
    id = "short_info_dataset",
    type = "file",
    files = list(create_example_DTAFileCSV())
  )

  # Interpolated names(x@id) -- always NULL -- so the id was missing and
  # the line read "Files:  (1 file, file)".
  out <- paste(cli::cli_fmt(print_short_info(ds)), collapse = "\n")

  expect_match(out, "short_info_dataset", fixed = TRUE)
  expect_match(out, "1 file", fixed = TRUE)
})

test_that("colspec() rejects an id or position that is not in the specification", {
  ds <- create_example_DTADataSetTabular(2)
  n <- length(columns(ds))

  # Each of these used to leak a raw base subscript error (999), silently
  # truncate (1.9 returned column 1), or return NULL (an unknown name).
  for (bad in list(999, 0, -1, n + 1, 1.9, NA_real_, c(1, 2), "NOPE", NA_character_, c("STUDYID", "AGE"), TRUE)) {
    expect_error(colspec(ds, bad), class = "dta_colspec_not_found", info = deparse(bad))
  }

  # The message names the problem in the package's own words.
  expect_error(colspec(ds, 999), "out of bounds")
  expect_error(colspec(ds, "NOPE"), "NOPE", fixed = TRUE)
})

test_that("colspec() returns the same column by name and by position", {
  ds <- create_example_DTADataSetTabular(2)
  ids <- names(columns(ds))
  for (i in seq_along(ids)) {
    expect_identical(colspec(ds, i)@id, ids[[i]])
    expect_identical(colspec(ds, ids[[i]])@id, ids[[i]])
    expect_identical(colspec(specs(ds), i), colspec(ds, ids[[i]]))
  }
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
    id = "two_handlers",
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
    id = "one_in_a_sequence",
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
    id = "no_handlers",
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
    id = "counts",
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


# ---------------------------------------------------------------------------
# DTADataSetTabular validator: only tabular file handlers allowed
# ---------------------------------------------------------------------------

test_that("DTADataSetTabular rejects a DTAFileAny handler", {
  expect_error(
    DTADataSetTabular(
      id = "bad_tabular",
      specs = create_example_DTAColumnSpecCollection(1),
      files = list(DTAFileAny(filename = "audit.log"))
    ),
    "not tabular"
  )
})

test_that("DTADataSetTabular rejects a bare DTAFile handler", {
  expect_error(
    DTADataSetTabular(
      id = "bad_tabular2",
      specs = create_example_DTAColumnSpecCollection(1),
      files = list(DTAFile(filename = "data.txt"))
    ),
    "not tabular"
  )
})

test_that("DTADataSetTabular accepts DTAFileCSV and DTAFileTSV handlers", {
  expect_s3_class(
    DTADataSetTabular(
      id = "ok_csv",
      specs = create_example_DTAColumnSpecCollection(1),
      files = list(DTAFileCSV(filename = "data.csv"))
    ),
    "DTAtools::DTADataSetTabular"
  )
  expect_s3_class(
    DTADataSetTabular(
      id = "ok_tsv",
      specs = create_example_DTAColumnSpecCollection(1),
      files = list(DTAFileTSV(filename = "data.tsv"))
    ),
    "DTAtools::DTADataSetTabular"
  )
})

test_that("DTADataSetFile may hold a DTAFileAny handler", {
  h <- DTAFileAny(filename = "audit.log")
  ds <- DTADataSetFile(id = "file_ds", files = list(h))

  expect_s3_class(ds, "DTAtools::DTADataSetFile")
  expect_s3_class(ds@files[[1]], "DTAtools::DTAFileAny")
})

# ---------------------------------------------------------------------------
# load_file() on a tabular dataset: handler_index and the default table name
# ---------------------------------------------------------------------------

ds_tabular_with_handler <- function(filename) {
  DTADataSetTabular(
    id = "d",
    specs = create_example_DTAColumnSpecCollection(1),
    files = list(DTAFileCSV(filename = filename))
  )
}

test_that("load_file() accepts a character handler_index and defaults it to 1", {
  # The guard this replaces ran `handler_index < 1 || handler_index > length()`
  # on a value that could be NULL, NA, length > 1 or character -- so "1" was
  # passed on to files() as a NAME and aborted with "The following datasets not
  # found: 1", even though a character index is documented as accepted. The
  # documented default of 1 was missing too, so omitting it hit R's own
  # "argument is missing".
  file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  ds <- ds_tabular_with_handler("clinical_data.csv")

  expect_named(tables(load_file(ds, file = file, handler_index = "1")), "clinical_data")
  expect_named(tables(load_file(ds, file = file, handler_index = 1)), "clinical_data")
  expect_named(tables(load_file(ds, file = file)), "clinical_data")
})

test_that("load_file() rejects a handler_index that is not a single usable value", {
  file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  ds <- ds_tabular_with_handler("clinical_data.csv")

  expect_error(load_file(ds, file = file, handler_index = NULL), "single, non-missing")
  expect_error(load_file(ds, file = file, handler_index = NA), "single, non-missing")
  expect_error(load_file(ds, file = file, handler_index = c(1, 1)), "single, non-missing")
  expect_error(load_file(ds, file = file, handler_index = 2), "Invalid handler_index")
  expect_error(load_file(ds, file = file, handler_index = 0), "Invalid handler_index")
})

test_that("a gzipped redelivery replaces the table rather than adding a second", {
  # The default table name stripped ONE extension, so `clinical_data2.csv.gz`
  # became the table `clinical_data2.csv` -- a second table beside
  # `clinical_data2`, holding the same data twice and leaving the first one's
  # verdict standing next to it. Compression is a transport detail; it is not
  # part of the table's identity.
  plain <- system.file("extdata", "clinical_data2.csv", package = "DTAtools")
  gzipped <- system.file("extdata", "clinical_data2.csv.gz", package = "DTAtools")
  expect_true(nzchar(plain) && nzchar(gzipped))

  ds <- ds_tabular_with_handler("clinical_data2.csv")

  ds <- load_file(ds, file = plain, handler_index = 1)
  expect_named(tables(ds), "clinical_data2")

  ds <- load_file(ds, file = gzipped, handler_index = 1)
  expect_named(tables(ds), "clinical_data2")
  expect_length(tables(ds), 1)

  # ... and in the other direction, too.
  ds <- load_file(ds, file = plain, handler_index = 1)
  expect_named(tables(ds), "clinical_data2")
  expect_length(tables(ds), 1)
})

test_that("the app's bound-item key agrees with the table load_file() creates", {
  # dta_bound_item_name() in inst/shiny/dta_app is what the app lists, unloads
  # and looks up validation state by. It restates the package's naming rule, so
  # the two have to be pinned against each other: a divergence means the UI
  # offers a table name that no report of the loaded document knows.
  skip_if_not_installed("shiny")
  bound_name <- app_fn("dta_bound_item_name")

  plain <- system.file("extdata", "clinical_data2.csv", package = "DTAtools")
  gzipped <- system.file("extdata", "clinical_data2.csv.gz", package = "DTAtools")

  ds <- ds_tabular_with_handler("clinical_data2.csv")

  expect_equal(
    names(tables(load_file(ds, file = plain, handler_index = 1))),
    bound_name("tabular", plain)
  )
  expect_equal(
    names(tables(load_file(ds, file = gzipped, handler_index = 1))),
    bound_name("tabular", gzipped)
  )

  # A file dataset still keys by the delivered name, extension and all.
  expect_equal(bound_name("file", gzipped), "clinical_data2.csv.gz")
})


# ---------------------------------------------------------------------------
# A dataset id containing braces is data, not cli syntax
# ---------------------------------------------------------------------------

test_that("print_short_info() survives braces in the dataset id", {
  # print_short_info(DTADataSet) built its message with str_c()/paste0(),
  # splicing x@id's raw text next to literal "{.field " / "}" markup before
  # handing the whole assembled string to cli_alert(). An id such as "d{x}"
  # made cli try to evaluate the stray `{x}` as the function argument `x`
  # itself, aborting with "cannot coerce type 'object' to vector of type
  # 'character'" instead of printing.
  ds <- DTADataSet(
    id = "d{x}",
    type = "file",
    files = list(create_example_DTAFileCSV())
  )

  short <- capture.output(print_short_info(ds), type = "message")
  expect_true(any(grepl("d{x}", short, fixed = TRUE)))

  # The zero-files branch never mentions the id, but must still not abort.
  ds_empty <- DTADataSet(id = "e{y}", type = "file", files = list())
  expect_no_error(capture.output(print_short_info(ds_empty), type = "message"))

  # print()/print_info() route the id through direct interpolation already
  # and must keep doing so.
  full <- capture.output(print(ds), type = "message")
  expect_true(any(grepl("d{x}", full, fixed = TRUE)))
})


# ---------------------------------------------------------------------------
# id / name / description
# ---------------------------------------------------------------------------
# A dataset has `id` (the identifier), `name` (the label) and
# `description` (long free text). An entry without `id` is refused.

# Written to disk so the whole read path is exercised, not only the helper.
write_yaml_lines <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(lines, path)
  path
}

test_that("read_dta_from_yaml() refuses a dataset entry without an id", {
  path <- write_yaml_lines(c(
    "metadata:",
    "  title: Specification without ids",
    "  version: '1.0'",
    "datasets:",
    "  - name: dm",
    "    type: tabular",
    "    columns:",
    "      - id: STUDYID",
    "        type: SAS Char"
  ))
  expect_error(read_dta_from_yaml(path), regexp = "needs an .*id")
})

test_that("read_dta_from_yaml() reads id, name and description as written", {
  path <- write_yaml_lines(c(
    "metadata:",
    "  title: Current specification",
    "  version: '1.0'",
    "datasets:",
    "  - id: dm",
    "    type: tabular",
    "    name: Demographics",
    "    description: One row per subject, as delivered by the CRO.",
    "    files:",
    "      filename: dm.csv",
    "      type: csv",
    "    columns:",
    "      - id: STUDYID",
    "        type: SAS Char"
  ))
  dta <- read_dta_from_yaml(path)

  expect_named(dta@datasets, "dm")
  dm <- dta@datasets[["dm"]]
  expect_identical(dm@id, "dm")
  expect_identical(dm@name, "Demographics")
  expect_identical(dm@description, "One row per subject, as delivered by the CRO.")
})

test_that("read_dataset_from_yaml() reads id, name and description as written, and refuses a missing id", {
  noid <- write_yaml_lines(c(
    "name: dm",
    "type: tabular",
    "columns:",
    "  - id: STUDYID",
    "    type: SAS Char"
  ))
  expect_error(read_dataset_from_yaml(noid), regexp = "needs an .*id")

  current <- write_yaml_lines(c(
    "id: dm",
    "type: tabular",
    "name: Demographics",
    "description: One row per subject.",
    "files:",
    "  filename: dm.csv",
    "  type: csv",
    "columns:",
    "  - id: STUDYID",
    "    type: SAS Char"
  ))
  ds <- read_dataset_from_yaml(current)
  expect_identical(ds@id, "dm")
  expect_identical(ds@name, "Demographics")
  expect_identical(ds@description, "One row per subject.")
})

test_that("dta_dataset_from_list() names what is wrong with an entry", {
  expect_error(
    dta_dataset_from_list(list(description = "no identifying key at all")),
    regexp = "List must contain an .*id.* field or be a list of datasets"
  )
  expect_error(
    dta_dataset_from_list(list(id = "dm")),
    regexp = "Dataset .*dm.* must contain a .*type"
  )
})

test_that("dta_dataset_from_list() refuses an entry without an id", {
  # `name` is the label now; it is never promoted to the identifier.
  expect_error(
    dta_dataset_from_list(list(name = "dm", type = "tabular")),
    regexp = "needs an .*id"
  )
  expect_error(
    dta_dataset_from_list(list(type = "tabular")),
    regexp = "needs an .*id"
  )
  # The first of several entries decides that they are datasets; each one is
  # then checked on its own.
  expect_error(
    dta_dataset_from_list(list(
      list(id = "ae", type = "file"),
      list(name = "dm", type = "tabular")
    )),
    regexp = "needs an .*id"
  )
})

test_that("DTADataSetFactory() forwards id, name and description", {
  tabular <- DTADataSetFactory(
    type = "tabular",
    columns = list(list(id = "STUDYID", type = "SAS Char")),
    id = "dm", name = "Demographics", description = "One row per subject."
  )
  expect_s3_class(tabular, "DTAtools::DTADataSetTabular")
  expect_identical(tabular@id, "dm")
  expect_identical(tabular@name, "Demographics")
  expect_identical(tabular@description, "One row per subject.")

  file <- DTADataSetFactory(
    type = "file",
    id = "reports", name = "Delivery reports", description = "PDF reports."
  )
  expect_s3_class(file, "DTAtools::DTADataSetFile")
  expect_identical(file@id, "reports")
  expect_identical(file@name, "Delivery reports")
  expect_identical(file@description, "PDF reports.")
})

test_that("a dataset constructor called without `id` aborts, name or no name", {
  specs <- create_example_DTAColumnSpecCollection(1)

  # `name` is the label; it is never read as the identifier.
  for (msg in list(
    tryCatch(DTADataSetTabular(name = "dm", specs = specs), error = conditionMessage),
    tryCatch(DTADataSetFile(name = "dm"), error = conditionMessage),
    tryCatch(DTADataSet(name = "dm", type = "file"), error = conditionMessage),
    tryCatch(DTADataSetTabular(specs = specs), error = conditionMessage),
    tryCatch(DTADataSetFile(), error = conditionMessage),
    tryCatch(DTADataSet(type = "file"), error = conditionMessage)
  )) {
    expect_match(cli::ansi_strip(msg), "needs an .*id")
  }
})

test_that("the dataset validator rejects a malformed id, name or description", {
  new_ds <- function(...) {
    DTADataSet(id = "dm", type = "file", files = list(create_example_DTAFileCSV()), ...)
  }
  expect_s3_class(new_ds(), "DTAtools::DTADataSet")

  # name and description are each one string or NULL ...
  expect_error(new_ds(name = c("a", "b")), regexp = "single string or NULL")
  expect_error(new_ds(name = NA_character_), regexp = "single string or NULL")
  expect_error(new_ds(description = c("a", "b")), regexp = "single string or NULL")
  expect_error(new_ds(description = NA_character_), regexp = "single string or NULL")

  # ... and the id is one non-empty string.
  expect_error(
    DTADataSet(id = "", type = "file"),
    regexp = "single non-empty string"
  )
  expect_error(
    DTADataSet(id = NA_character_, type = "file"),
    regexp = "single non-empty string"
  )
  expect_error(
    DTADataSet(id = c("a", "b"), type = "file"),
    regexp = "single non-empty string"
  )
  expect_error(
    DTADataSetTabular(id = "", specs = create_example_DTAColumnSpecCollection(1)),
    regexp = "single non-empty string"
  )

  # The same validator runs on assignment.
  ds <- new_ds()
  expect_error(
    {
      ds@id <- ""
    },
    regexp = "single non-empty string"
  )
  expect_error(
    {
      ds@description <- NA_character_
    },
    regexp = "single string or NULL"
  )
})

test_that("the bundled examples carry an id and, where meaningful, a name", {
  expect_identical(create_example_DTADataSetTabular(2)@id, "demographics")
  expect_identical(create_example_DTADataSetTabular(2)@name, "Demographics")
  expect_identical(create_example_DTADataSetTabular(3)@id, "vitals")
  expect_identical(create_example_DTADataSetTabular(3)@name, "Vital signs")

  bare <- create_example_DTADataSetTabular(1)
  expect_identical(bare@id, "example_container_specs_without_data")
  expect_null(bare@name)
})

test_that("files() names the missing file with agreeing grammar and no stray plural", {
  # The message read "The following datasets{?s} not found", which is a
  # dataset message in a files() function, and with two names missing rendered
  # as "datasetss".
  ds <- create_example_DTADataSetTabular(2)

  one <- expect_error(files(ds, "nope.csv"), class = "rlang_error")
  expect_match(conditionMessage(one), "following file not found", fixed = TRUE)
  expect_match(conditionMessage(one), "nope.csv", fixed = TRUE)

  two <- expect_error(files(ds, c("nope.csv", "nada.csv")), class = "rlang_error")
  expect_match(conditionMessage(two), "following files not found", fixed = TRUE)
  expect_no_match(conditionMessage(two), "datasets", fixed = TRUE)
})

test_that("datasets() on a DTA agrees its plural with the number of names missing", {
  dta <- create_example_DTA()

  two <- expect_error(datasets(dta, c("nope", "nada")), class = "rlang_error")
  expect_match(conditionMessage(two), "following datasets not found", fixed = TRUE)
  expect_no_match(conditionMessage(two), "datasetss", fixed = TRUE)
})
