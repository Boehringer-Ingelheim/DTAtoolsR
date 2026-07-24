test_that("DTAColumnSpecStructureSAS accepts all documented SAS format families", {
  cases <- list(
    list(type = "Char", format = "$12.", expected_type = "Char"),
    list(type = "Num", format = "8.2", expected_type = "Num"),
    list(type = "Num", format = "BEST12.", expected_type = "Num"),
    list(type = "Int", format = "8.", expected_type = "Int"),
    list(type = "Int", format = "BEST8.", expected_type = "Int"),
    list(type = "Date", format = "DATE9.", expected_type = "Date"),
    list(type = "Time", format = "TIME8.", expected_type = "Time"),
    list(type = "Time", format = "TIME8.2", expected_type = "Time"),
    list(type = "DateTime", format = "DATETIME20.", expected_type = "DateTime")
  )

  for (case in cases) {
    x <- DTAColumnSpecStructureSAS(type = case$type, format = case$format, length = 20)
    expect_s3_class(x, "DTAtools::DTAColumnSpecStructureSAS")
    expect_equal(x@type, case$expected_type)
    expect_equal(x@format, case$format)
    expect_equal(x@length, 20)
  }
})

test_that("DTAColumnSpecStructureSAS infers type from format when type is missing", {
  cases <- list(
    list(format = "$10.", expected_type = "Char"),
    list(format = "8.2", expected_type = "Num"),
    list(format = "BEST12.", expected_type = "Int"),
    list(format = "8.", expected_type = "Int"),
    list(format = "DATE9.", expected_type = "Date"),
    list(format = "TIME8.2", expected_type = "Time"),
    list(format = "DATETIME20.", expected_type = "DateTime")
  )

  for (case in cases) {
    x <- DTAColumnSpecStructureSAS(format = case$format, length = 20)
    expect_equal(x@type, case$expected_type)
    expect_equal(x@format, case$format)
  }
})

test_that("DTAColumnSpecStructureSAS rejects unsupported or incompatible type/format combinations", {
  expect_error(
    DTAColumnSpecStructureSAS(type = "Currency", format = "$8."),
    "must be one of"
  )

  expect_error(
    DTAColumnSpecStructureSAS(type = "Date", format = "8.2"),
    "not compatible"
  )

  expect_error(
    DTAColumnSpecStructureSAS(type = "Num", format = "A12."),
    "Unsupported SAS format"
  )
})

test_that("DTAColumnSpecStructureSAS JSON type mapping covers extended SAS types", {
  expect_equal(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Char", format = "$12.")), "string")
  expect_equal(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Num", format = "8.2")), "number")
  expect_equal(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Int", format = "8.")), "integer")
  expect_equal(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Date", format = "DATE9.")), "string")
  expect_equal(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Time", format = "TIME8.")), "string")
  expect_equal(as_json_schema_type(DTAColumnSpecStructureSAS(type = "DateTime", format = "DATETIME20.")), "string")
})

test_that("DTAColumnSpec I/O preserves SAS type, format, and length", {
  spec <- DTAColumnSpec(
    id = "VISITDT",
    type = "SAS Date",
    format = "SAS DATE9.",
    length = 9,
    nullable = TRUE
  )

  expect_equal(spec@structure@type, "Date")
  expect_equal(spec@structure@format, "DATE9.")
  expect_equal(spec@structure@length, 9)

  out <- as.list(spec)
  expect_equal(out$type, "SAS Date")
  expect_equal(out$format, "SAS DATE9.")
  expect_equal(out$length, 9)
})

test_that("SAS character shorthand '$w' is normalized to '$w.'", {
  x <- DTAColumnSpecStructureSAS(type = "Char", format = "$50")
  expect_equal(x@type, "Char")
  expect_equal(x@format, "$50.")

  spec <- DTAColumnSpec(id = "TXT", type = "SAS Char", format = "SAS $50")
  expect_equal(spec@structure@format, "$50.")
  expect_equal(as.list(spec)$format, "SAS $50.")
})

test_that("non-documented SAS type aliases are rejected", {
  expect_error(
    DTAColumnSpec(id = "AVAL", type = "SAS Float", format = "SAS 10.4"),
    "must be one of"
  )
})

test_that("enum/const values keep correct types across conversion", {
  spec_int <- DTAColumnSpec(
    id = "INTV",
    type = "SAS Int",
    format = "SAS 8.",
    values = list(1L, 2L),
    nullable = FALSE
  )
  schema_int <- as_json_schema(spec_int)
  expect_type(schema_int$enum, "integer")
  expect_identical(schema_int$enum, c(1L, 2L))

  spec_num <- DTAColumnSpec(
    id = "NUMV",
    type = "SAS Num",
    format = "SAS 8.2",
    values = c(1.25, 2.50),
    nullable = FALSE
  )
  schema_num <- as_json_schema(spec_num)
  expect_type(schema_num$enum, "double")
  expect_equal(schema_num$enum, c(1.25, 2.50))

  spec_char <- DTAColumnSpec(
    id = "CHARV",
    type = "SAS Char",
    format = "SAS $10.",
    length = 10,
    values = list("A", "B"),
    nullable = FALSE
  )
  schema_char <- as_json_schema(spec_char)
  expect_type(schema_char$enum, "character")
  expect_identical(schema_char$enum, c("A", "B"))

  spec_date <- DTAColumnSpec(
    id = "DATEV",
    type = "SAS Date",
    format = "SAS DATE9.",
    values = list(as.Date("2026-01-01")),
    nullable = FALSE
  )
  schema_date <- as_json_schema(spec_date)
  expect_type(schema_date$const, "character")
  expect_identical(schema_date$const, "2026-01-01")

  spec_time <- DTAColumnSpec(
    id = "TIMEV",
    type = "SAS Time",
    format = "SAS TIME8.2",
    values = list("01:02:03.50"),
    nullable = FALSE
  )
  schema_time <- as_json_schema(spec_time)
  expect_type(schema_time$const, "character")
  expect_identical(schema_time$const, "01:02:03.50")

  spec_datetime <- DTAColumnSpec(
    id = "DTV",
    type = "SAS DateTime",
    format = "SAS DATETIME20.",
    values = list(as.POSIXct("2026-01-01 10:11:12", tz = "UTC")),
    nullable = FALSE
  )
  schema_datetime <- as_json_schema(spec_datetime)
  expect_type(schema_datetime$const, "character")
  expect_identical(schema_datetime$const, "2026-01-01 10:11:12")
})

test_that("validate_table preserves mixed typed values during conversion", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      CHARV = DTAColumnSpec(id = "CHARV", type = "SAS Char", format = "SAS $10.", length = 10, nullable = TRUE),
      INTV = DTAColumnSpec(id = "INTV", type = "SAS Int", format = "SAS 8.", nullable = TRUE),
      NUMV = DTAColumnSpec(id = "NUMV", type = "SAS Num", format = "SAS 8.2", nullable = TRUE),
      DATEV = DTAColumnSpec(id = "DATEV", type = "SAS Date", format = "SAS DATE9.", nullable = TRUE),
      TIMEV = DTAColumnSpec(id = "TIMEV", type = "SAS Time", format = "SAS TIME8.2", nullable = TRUE),
      DTV = DTAColumnSpec(id = "DTV", type = "SAS DateTime", format = "SAS DATETIME20.", nullable = TRUE)
    )
  )

  table <- data.frame(
    CHARV = c("A", "B", NA),
    INTV = c(1L, 2L, NA_integer_),
    NUMV = c(1.25, 2.50, NA_real_),
    DATEV = as.Date(c("2026-01-01", "2026-01-02", NA)),
    TIMEV = c("01:02:03.50", "23:59:59.99", NA),
    DTV = as.POSIXct(c("2026-01-01 10:11:12", "2026-01-02 21:22:23", NA), tz = "UTC")
  )

  original <- table
  validated <- validate_table(specs = specs, table = table, verbose = FALSE)

  expect_identical(validated, original)
  expect_identical(table, original)
})

test_that("only documented SAS format families are accepted", {
  expect_no_error(DTAColumnSpecStructureSAS(type = "Char", format = "$10."))
  expect_no_error(DTAColumnSpecStructureSAS(type = "Int", format = "8."))
  expect_no_error(DTAColumnSpecStructureSAS(type = "Num", format = "8.2"))
  expect_no_error(DTAColumnSpecStructureSAS(type = "Num", format = "BEST12."))
  expect_no_error(DTAColumnSpecStructureSAS(type = "Date", format = "DATE9."))
  expect_no_error(DTAColumnSpecStructureSAS(type = "Time", format = "TIME8."))
  expect_no_error(DTAColumnSpecStructureSAS(type = "Time", format = "TIME8.2"))
  expect_no_error(DTAColumnSpecStructureSAS(type = "DateTime", format = "DATETIME20."))

  expect_error(DTAColumnSpecStructureSAS(type = "Char", format = "$10.2"), "Unsupported SAS format")
  expect_error(DTAColumnSpecStructureSAS(type = "Date", format = "DATE9.2"), "Unsupported SAS format")
  expect_error(DTAColumnSpecStructureSAS(type = "DateTime", format = "DATETIME20.3"), "Unsupported SAS format")
  expect_error(DTAColumnSpecStructureSAS(type = "Time", format = "TIME8"), "Unsupported SAS format")
  expect_error(DTAColumnSpecStructureSAS(type = "Num", format = "BEST12.2"), "Unsupported SAS format")
  expect_error(DTAColumnSpecStructureSAS(type = "Num", format = "E12."), "Unsupported SAS format")
})

test_that("validate_table chunked path preserves values without mutation", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      CHARV = DTAColumnSpec(id = "CHARV", type = "SAS Char", format = "SAS $8.", length = 8, nullable = FALSE),
      NUMV = DTAColumnSpec(id = "NUMV", type = "SAS Num", format = "SAS 8.2", nullable = FALSE)
    )
  )

  n <- 6001
  table <- data.frame(
    CHARV = rep(c("ALPHA", "BETA"), length.out = n),
    NUMV = rep(c(1.25, 9.75), length.out = n)
  )

  original <- table
  validated <- validate_table(specs = specs, table = table, verbose = FALSE)

  expect_identical(validated, original)
  expect_identical(table, original)
})
