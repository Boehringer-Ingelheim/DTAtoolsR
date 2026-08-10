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

# DEFERRED (blocked by implementation bug): as.list() on the base class.
#
# method(as.list, DTAColumnSpecStructure) in R/DTAColumnSpecStructure-class.R
# builds `list(type = ..., format = ..., length = x@length,)` -- note the
# trailing comma, which makes list() see an empty fourth argument. Every call
# therefore fails with R's base error "argument 4 is empty", for any input:
#
#   as.list(DTAColumnSpecStructure(type = "Char", format = "$12.",
#                                  length = 12, backend = "SAS"))
#   #> Error in list(...) : argument 4 is empty
#
# The intended test (round-tripping the base structure through as.list()) is
# left out until the trailing comma is removed; asserting the error instead
# would only lock the bug in place.
