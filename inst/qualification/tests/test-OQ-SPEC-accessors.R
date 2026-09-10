# Reaching into a specification. REQ-SPEC-018 .. REQ-SPEC-022.
#
# These are how a caller asks a transfer object what it contains, and how the
# Shiny application and the document exporters read it. Each answers on one
# kind of object, and the qualification records which -- a caller reaching for
# the wrong one gets an error rather than a wrong answer, which is the right
# way round but only if it is written down.

sa_transfer <- function() {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  if (!nzchar(path)) {
    testthat::skip("the bundled example specification is not installed")
  }
  read_dta_from_yaml(path)
}

sa_loaded <- function() {
  dta <- sa_transfer()
  csv <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  ds <- load_file(dta[["clinical_data"]], file = csv, handler_index = 1, stream = "never")
  dta@datasets[["clinical_data"]] <- ds
  dta
}

test_that("OQ-SPEC-040 | a transfer object answers for each of its parts | REQ-SPEC-018", {
  dta <- sa_transfer()
  ds <- dta[["clinical_data"]]

  qa_step("its datasets are named", "clinical_data", names(datasets(dta)))
  qa_check("its metadata is a metadata object", inherits(metadata(dta), "DTAtools::DTAMetaData"))
  qa_check("a dataset reports its file handlers", length(files(ds)) >= 1)
  qa_check(
    "a dataset reports its column specification",
    inherits(specs(ds), "DTAtools::DTAColumnSpecCollection")
  )
  qa_check("and its rules", length(rules(specs(ds))) > 0)

  # Tables are what a delivery puts into a dataset, so an unbound dataset has
  # none. Reporting an empty set rather than failing is what lets a caller ask
  # before knowing.
  qa_step("an unbound dataset holds no tables", 0L, length(tables(ds)))
  qa_step(
    "and holds one once a delivery is bound",
    1L, length(tables(sa_loaded()[["clinical_data"]]))
  )
})

test_that("OQ-SPEC-041 | the column accessors report a specification's columns | REQ-SPEC-019", {
  dta <- sa_transfer()
  ds <- dta[["clinical_data"]]
  collection <- specs(ds)

  from_dataset <- names(columns(ds))
  qa_check("a dataset reports its column identifiers", length(from_dataset) > 0)

  # The same question asked of the collection rather than of the dataset. The
  # two must agree, or a caller reading a specification through one route would
  # see a different agreement than one reading it through the other.
  qa_step(
    "the collection reports the same identifiers as the dataset",
    from_dataset, names(collection)
  )
  qa_step(
    "and its labels cover the same columns",
    length(from_dataset), length(labels(collection))
  )

  first <- from_dataset[[1]]
  qa_step(
    "a named column is retrievable from the collection",
    first, colspec(collection, first)@id
  )
  qa_step(
    "and from the dataset directly",
    first, colspec(ds, first)@id
  )
})

test_that("OQ-SPEC-042 | a loaded table is retrievable by name and by position | REQ-SPEC-020", {
  ds <- sa_loaded()[["clinical_data"]]
  name <- names(tables(ds))[[1]]

  by_name <- get_table(ds, name)
  by_position <- get_table(ds, 1)
  qa_check("a table comes back by name", !is.null(by_name))
  qa_step(
    "and by position, giving the same table",
    dim(as.data.frame(by_name)), dim(as.data.frame(by_position))
  )

  # Asking for something that is not there must be an error. Returning an empty
  # table would let a caller validate nothing and report success.
  qa_check(
    "a name that does not exist raises a condition",
    inherits(tryCatch(get_table(ds, "no_such_table"), error = function(e) e), "condition")
  )
  qa_check(
    "and so does a position out of range",
    inherits(tryCatch(get_table(ds, 99), error = function(e) e), "condition")
  )
})

test_that("OQ-SPEC-043 | a specification can be summarised without any data | REQ-SPEC-021", {
  collection <- specs(sa_transfer()[["clinical_data"]])
  preview <- column_preview(collection)

  qa_check("a preview is produced", is.character(preview) && nzchar(preview))
  qa_step(
    "naming the first columns of the specification",
    TRUE,
    all(vapply(
      utils::head(names(collection), 3),
      function(id) grepl(id, preview, fixed = TRUE),
      logical(1)
    ))
  )

  # A long specification is abbreviated rather than printed whole, so a preview
  # stays a preview.
  many <- DTAColumnSpecCollection(
    columns = stats::setNames(
      lapply(sprintf("C%02d", 1:20), function(id) DTAColumnSpec(id = id, type = "SAS Char")),
      sprintf("C%02d", 1:20)
    )
  )
  qa_step(
    "a specification of twenty columns is abbreviated",
    TRUE, grepl("...", column_preview(many), fixed = TRUE)
  )
})

test_that("OQ-SPEC-044 | describing an object does not change it | REQ-SPEC-022", {
  dta <- sa_transfer()
  ds <- dta[["clinical_data"]]

  before <- list(
    datasets = names(datasets(dta)),
    columns = names(columns(ds)),
    rules = vapply(rules(specs(ds)), function(r) r@id, character(1))
  )

  # Printing is what a user does first and most often. A print method that
  # mutated what it described would corrupt an object simply by being looked
  # at, and the corruption would be invisible until much later.
  # The print methods report through cli, which writes to the message stream
  # rather than to standard output, so both have to be captured -- and both
  # have to be captured rather than merely suppressed, or the qualification
  # run's own output would be interleaved with a fixture's.
  describe <- function() {
    print(dta)
    print(ds)
    print(specs(ds))
    print(colspec(ds, names(columns(ds))[[1]]))
    print_info(metadata(dta))
    print_short_info(ds)
  }
  messages <- utils::capture.output(
    on_stdout <- utils::capture.output(describe()),
    type = "message"
  )
  output <- c(on_stdout, messages)

  qa_check("printing produces output", length(output) > 0)
  qa_step(
    "which names the transfer it describes",
    TRUE,
    any(grepl("DTA", output, fixed = TRUE))
  )
  qa_step(
    "and leaves the object exactly as it was",
    before,
    list(
      datasets = names(datasets(dta)),
      columns = names(columns(ds)),
      rules = vapply(rules(specs(ds)), function(r) r@id, character(1))
    )
  )
})
