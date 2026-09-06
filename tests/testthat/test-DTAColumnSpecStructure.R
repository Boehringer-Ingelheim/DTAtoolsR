test_that("DTAColumnSpecStructure stores the properties it is constructed with", {
  x <- DTAColumnSpecStructure(
    type = "Char",
    format = "$12.",
    length = 12,
    backend = "SAS"
  )

  expect_s3_class(x, "DTAtools::DTAColumnSpecStructure")
  expect_equal(x@type, "Char")
  expect_equal(x@format, "$12.")
  expect_equal(x@length, 12)
  expect_equal(x@backend, "SAS")
})

test_that("DTAColumnSpecStructure treats type, format and length as optional", {
  # Only `backend` is required; the other three properties are unions with NULL
  # so a structure may carry a backend and nothing else.
  x <- DTAColumnSpecStructure(backend = "R")

  expect_null(x@type)
  expect_null(x@format)
  expect_null(x@length)
  expect_equal(x@backend, "R")

  # `format` also accepts a bare numeric (e.g. an unquoted YAML width).
  expect_equal(DTAColumnSpecStructure(format = 8.2, backend = "R")@format, 8.2)
})

test_that("DTAColumnSpecStructure validator requires a non-empty backend", {
  expect_error(
    DTAColumnSpecStructure(type = "Char", backend = ""),
    "backend"
  )

  # A missing backend is caught one level earlier, by the S7 property contract:
  # @backend is class_character, so NULL never reaches the validator.
  expect_error(
    DTAColumnSpecStructure(type = "Char", backend = NULL),
    "@backend must be <character>"
  )
})

test_that("DTAColumnSpecStructure enforces its property types", {
  expect_error(
    DTAColumnSpecStructure(type = "Char", backend = 1),
    "@backend must be <character>"
  )
  expect_error(
    DTAColumnSpecStructure(length = "twelve", backend = "SAS"),
    "@length must be"
  )
})

test_that("as_json_schema_type is not implemented on the base class", {
  # The base class deliberately refuses to guess a JSON type; only backend
  # subclasses know the mapping. as_json_schema() calls as_json_schema_type()
  # whenever @type is set, so it propagates the same abort.
  x <- DTAColumnSpecStructure(type = "Char", format = "$12.", length = 12, backend = "SAS")

  expect_error(as_json_schema_type(x), "not implemented at this level")
  expect_error(as_json_schema(x), "not implemented at this level")
})

test_that("print and print_info render the base structure properties", {
  # cli writes to the message connection, hence type = "message".
  x <- DTAColumnSpecStructure(type = "Char", format = "$12.", length = 12, backend = "SAS")

  info <- capture.output(print_info(x), type = "message")
  expect_true(any(grepl("type", info, fixed = TRUE)))
  expect_true(any(grepl("Char", info, fixed = TRUE)))
  expect_true(any(grepl("$12.", info, fixed = TRUE)))
  expect_true(any(grepl("12", info, fixed = TRUE)))
  expect_true(any(grepl("SAS", info, fixed = TRUE)))

  out <- capture.output(print(x), type = "message")
  expect_true(any(grepl("DTAColumnSpecStructure", out, fixed = TRUE)))

  # Unset properties are skipped rather than printed as NULL.
  sparse <- capture.output(
    print_info(DTAColumnSpecStructure(type = "Char", backend = "SAS")),
    type = "message"
  )
  expect_false(any(grepl("format", sparse, fixed = TRUE)))
  expect_false(any(grepl("length", sparse, fixed = TRUE)))

  # Both return their object (invisibly) so they can be chained. Capture the
  # rendering so it does not leak into the test log.
  invisible(capture.output(
    {
      expect_identical(print(x), x)
      expect_identical(print_info(x), x)
    },
    type = "message"
  ))
})

test_that("DTAColumnSpecStructureSAS is a DTAColumnSpecStructure", {
  # The backend subclass must satisfy the base contract, so anything typed
  # against the base class also accepts it.
  sas <- DTAColumnSpecStructureSAS(type = "Char", format = "$12.", length = 12)

  expect_s3_class(sas, "DTAtools::DTAColumnSpecStructure")
  expect_equal(sas@backend, "SAS")

  # Unlike the base class, the subclass implements the JSON type mapping.
  expect_equal(as_json_schema_type(sas), "string")
})

test_that("as.list on the base class prefixes type and format with the backend", {
  # This used to abort for every input: the method built
  # `list(type = ..., format = ..., length = x@length,)` -- note the trailing
  # comma, which makes list() see an empty fourth argument.
  x <- DTAColumnSpecStructure(type = "Char", format = "$12.", length = 12, backend = "SAS")

  expect_equal(
    as.list(x),
    list(type = "SAS Char", format = "SAS $12.", length = 12)
  )

  # The backend prefix comes FIRST in both keys, matching the SAS subclass and
  # the "<BACKEND> <value>" grammar DTAColumnSpecStructureFactory() parses.
  # It used to compose format the other way round ("$12. SAS").
  expect_equal(as.list(x)$format, "SAS $12.")
  expect_equal(
    as.list(x),
    as.list(DTAColumnSpecStructureSAS(type = "Char", format = "$12.", length = 12))
  )
})

test_that("as.list omits unset structure properties instead of emitting 'SAS '", {
  # An unset format must not serialize as the bare backend prefix: "SAS "
  # re-parses to format = "" and is rejected by the SAS validator.
  bare <- DTAColumnSpecStructure(type = "Char", backend = "SAS")
  expect_equal(as.list(bare), list(type = "SAS Char"))
  expect_false("format" %in% names(as.list(bare)))
  expect_false("length" %in% names(as.list(bare)))

  sas <- DTAColumnSpecStructureSAS(type = "Char")
  expect_equal(as.list(sas), list(type = "SAS Char"))
  expect_false("format" %in% names(as.list(sas)))

  # ... and the same for an unset type.
  fmt_only <- DTAColumnSpecStructureSAS(format = "$12.")
  as_list_fmt <- as.list(fmt_only)
  expect_equal(as_list_fmt$format, "SAS $12.")
  # The SAS constructor infers Char from "$12.", so type is present here.
  expect_equal(as_list_fmt$type, "SAS Char")

  base_fmt_only <- DTAColumnSpecStructure(format = "$12.", backend = "SAS")
  expect_equal(as.list(base_fmt_only), list(format = "SAS $12."))

  # Nothing set at all: an empty list, never a list of blank prefixes.
  expect_equal(as.list(DTAColumnSpecStructure(backend = "SAS")), list())
})

test_that("a spec collection without formats survives a YAML round trip", {
  # The write -> read cycle documented on write_columns_to_yaml() used to be
  # broken for every column without a format: as.list() wrote `format: 'SAS '`,
  # which import_specs_from_yaml() rejected with
  # "Unsupported SAS format: ''".
  original <- DTAColumnSpecCollection(
    columns = list(
      STUDYID = DTAColumnSpec(id = "STUDYID", label = "Study", type = "SAS Char"),
      AGE = DTAColumnSpec(id = "AGE", label = "Age", type = "SAS Int", nullable = TRUE)
    ),
    rules = list()
  )
  # Precondition: no column carries a format.
  for (col in original@columns) {
    expect_null(col@structure@format)
  }

  f <- tempfile(fileext = ".yaml")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  write_columns_to_yaml(original, f)

  # The blank backend prefix must not appear in the YAML at all.
  yaml_lines <- readLines(f, warn = FALSE)
  expect_false(any(grepl("SAS '", yaml_lines, fixed = TRUE)))
  expect_false(any(grepl("format:", yaml_lines, fixed = TRUE)))

  back <- suppressMessages(import_specs_from_yaml(f))
  expect_equal(back, original)
})

test_that("the bundled example collection survives its own write -> read cycle", {
  # write_columns_to_yaml() -> import_specs_from_yaml() used to abort with
  # "Unsupported SAS format: ''" on the package's own example collection,
  # because every column serialized `format: 'SAS '`.
  original <- create_example_DTAColumnSpecCollection()

  f <- tempfile(fileext = ".yaml")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  write_columns_to_yaml(original, f)

  back <- NULL
  expect_no_error(back <- suppressMessages(import_specs_from_yaml(f)))

  expect_equal(names(back@columns), names(original@columns))
  for (id in names(original@columns)) {
    expect_equal(back@columns[[id]]@id, original@columns[[id]]@id)
    expect_equal(back@columns[[id]]@label, original@columns[[id]]@label)
    expect_equal(back@columns[[id]]@nullable, original@columns[[id]]@nullable)
    expect_equal(back@columns[[id]]@structure, original@columns[[id]]@structure)
  }

  # Unrelated to the structure serialization and out of scope here: @values is
  # stored as a list but read back from YAML as a character vector, so the two
  # collections are not `identical()` as whole objects.
  expect_equal(
    lapply(back@columns, function(c) as.character(unlist(c@values))),
    lapply(original@columns, function(c) as.character(unlist(c@values)))
  )
})

test_that("print and print_info survive braces in type, format and backend", {
  # Every value here is already interpolated directly (`{x@type}` etc.), so
  # this pins the already-correct behaviour against a future regression. The
  # base class -- unlike DTAColumnSpecStructureSAS -- has no fixed vocabulary
  # for type/format/backend, so a brace is actually reachable here.
  x <- DTAColumnSpecStructure(type = "Char{X}", format = "$1{2}.", length = 5, backend = "SAS{Y}")

  info <- capture.output(print_info(x), type = "message")
  expect_true(any(grepl("Char{X}", info, fixed = TRUE)))
  expect_true(any(grepl("$1{2}.", info, fixed = TRUE)))
  expect_true(any(grepl("SAS{Y}", info, fixed = TRUE)))

  out <- capture.output(print(x), type = "message")
  expect_true(any(grepl("DTAColumnSpecStructure", out, fixed = TRUE)))
})
