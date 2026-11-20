# The R backend of a column type: `type: R numeric`, `R Date`, and so on. The
# expected values are written out from the specification of the type
# vocabulary, never copied from what the constructor returned.

r_types <- c("character", "numeric", "integer", "logical", "Date", "POSIXct", "hms")
r_temporal <- c("Date", "POSIXct", "hms")

# ---- vocabulary ---------------------------------------------------------------

test_that("DTAColumnSpecStructureR accepts each of the seven types in its canonical spelling", {
  for (type in r_types) {
    x <- DTAColumnSpecStructureR(type = type)
    expect_s3_class(x, "DTAtools::DTAColumnSpecStructureR")
    expect_identical(x@type, type)
    expect_identical(x@backend, "R")
  }
})

test_that("the type is matched case-insensitively and stored canonically", {
  spelled <- c("CHARACTER", "Numeric", "iNTEGER", "logical", "date", "POSIXCT", "Hms")
  for (k in seq_along(spelled)) {
    expect_identical(
      DTAColumnSpecStructureR(type = spelled[k])@type,
      r_types[k],
      info = spelled[k]
    )
  }
  expect_identical(DTAColumnSpecStructureR(type = "  integer  ")@type, "integer")
})

test_that("double is accepted as a synonym of numeric and stored as numeric", {
  expect_identical(DTAColumnSpecStructureR(type = "double")@type, "numeric")
  expect_identical(DTAColumnSpecStructureR(type = "DOUBLE")@type, "numeric")
  expect_identical(DTAColumnSpecStructureFactory(type = "R double")@type, "numeric")
})

test_that("a type outside the vocabulary is rejected, including every SAS spelling", {
  for (type in c("factor", "string", "Char", "Num", "Int", "Time", "DateTime", "Bool", "float", "difftime")) {
    expect_error(
      DTAColumnSpecStructureR(type = type),
      "'type' must be one of: character, numeric, integer, logical, Date, POSIXct, hms",
      info = type
    )
  }
  expect_error(
    DTAColumnSpec(id = "X", type = "R Char"),
    "'type' must be one of"
  )
})

test_that("an empty type is refused rather than dropped", {
  expect_error(DTAColumnSpecStructureR(type = ""), "'type' cannot be an empty string")
  expect_error(DTAColumnSpecStructureFactory(type = "R "), "'type' cannot be an empty string")
  expect_error(DTAColumnSpecStructureFactory(type = "R    "), "'type' cannot be an empty string")
})

test_that("a structure with no type and no format is allowed", {
  x <- DTAColumnSpecStructureR()
  expect_null(x@type)
  expect_null(x@format)
  expect_identical(x@backend, "R")
})

# ---- format and length --------------------------------------------------------

test_that("a format is refused on every type that is not temporal, and the message names the type", {
  for (type in c("character", "numeric", "integer", "logical")) {
    expect_error(
      DTAColumnSpecStructureR(type = type, format = "%Y-%m-%d"),
      paste0("only allowed for an R Date, POSIXct or hms column, not for an R ", type, " column"),
      info = type
    )
  }
})

test_that("a length is allowed on character only", {
  expect_identical(DTAColumnSpecStructureR(type = "character", length = 12)@length, 12)
  expect_identical(DTAColumnSpecStructureR(length = 5)@length, 5)
  for (type in setdiff(r_types, "character")) {
    expect_error(
      DTAColumnSpecStructureR(type = type, length = 12),
      paste0("only allowed for an R character column, not for an R ", type, " column"),
      info = type
    )
  }
})

test_that("a format that is empty after the prefix is refused, not dropped", {
  expect_error(
    DTAColumnSpecStructureFactory(type = "R Date", format = "R "),
    "'format' must be one non-empty piece of text"
  )
  expect_error(
    DTAColumnSpecStructureFactory(type = "R Date", format = "R     "),
    "'format' must be one non-empty piece of text"
  )
  expect_error(
    DTAColumnSpecStructureR(type = "Date", format = ""),
    "'format' must be one non-empty piece of text"
  )
  expect_error(
    DTAColumnSpecStructureR(type = "Date", format = NA_character_),
    "'format' must be one non-empty piece of text"
  )
})

test_that("a trailing or leading space around the format is trimmed", {
  x <- DTAColumnSpecStructureFactory(type = "R Date", format = "R %Y-%m-%d  ")
  expect_identical(x@format, "%Y-%m-%d")
  expect_identical(DTAColumnSpecStructureR(type = "Date", format = "  %d/%m/%Y")@format, "%d/%m/%Y")
})

test_that("a literal outside the whitelist is refused, whatever the type", {
  literals <- c(
    "%Y-%m-%d5", "%Y-%m-%d{x}", "{x}", "%Y-%m-%d'", "%Y-%m-%d\"", "%Y\\%m-%d",
    "%Y-%m-%d a", "%Y-%m-%d t", "%Y-%m-%dé", "%Y-%m-%d(", "%Y-%m-%d*", "%Y-%m-%d|"
  )
  for (format in literals) {
    expect_error(
      DTAColumnSpecStructureR(type = "Date", format = format),
      "Unsupported R format",
      info = format
    )
    expect_error(
      DTAColumnSpecStructureR(format = format),
      "Unsupported R format",
      info = format
    )
  }
})

test_that("a refused format puts the offending text in the message as a value", {
  err <- expect_error(DTAColumnSpecStructureR(type = "Date", format = "{x}"), "Unsupported R format")
  expect_match(conditionMessage(err), "'{x}'", fixed = TRUE)
  expect_match(conditionMessage(err), "character '{' is not allowed", fixed = TRUE)

  err <- expect_error(DTAColumnSpecStructureR(type = "Date", format = "%Y-%m-%d %s"), "Unsupported R format")
  expect_match(conditionMessage(err), "directive '%s' is not supported", fixed = TRUE)
})

test_that("a format with braces read from YAML is reported as plain text, never evaluated", {
  # The reader wraps the construction error in its own lint report; braces in
  # the offending format must reach the reader's message as typed.
  for (format in c("{x}", "%d {1+1} %Y", "{.val x}")) {
    path <- tempfile(fileext = ".yaml")
    writeLines(c("columns:", "  A:", "    type: R Date", paste0("    format: 'R ", format, "'")), path)
    err <- expect_error(import_specs_from_yaml(path), class = "dta_lint_error", info = format)
    unlink(path)
    expect_match(conditionMessage(err), "column_spec_invalid", fixed = TRUE)
    expect_match(conditionMessage(err), paste0("'", format, "'"), fixed = TRUE)
  }
})

test_that("repeated directives, doubled seconds and malformed %OS are refused", {
  expect_error(DTAColumnSpecStructureR(type = "Date", format = "%Y-%Y-%m-%d"), "'%Y' appears more than once")
  expect_error(DTAColumnSpecStructureR(type = "POSIXct", format = "%F %Y"), "'%Y' appears more than once")
  expect_error(DTAColumnSpecStructureR(type = "POSIXct", format = "%F %m %H:%M"), "'%m' appears more than once")
  expect_error(DTAColumnSpecStructureR(type = "hms", format = "%T %OS3"), "Seconds appear more than once")
  expect_error(DTAColumnSpecStructureR(type = "hms", format = "%H:%M:%OS35"), "cannot directly follow %OSn")
  expect_error(DTAColumnSpecStructureR(type = "hms", format = "%H:%M:%OS"), "not supported on its own")
})

test_that("a directive outside the whitelist is refused", {
  for (format in c("%Y%%m%%d", "%d %b %Y", "%d %B %Y", "%a %Y-%m-%d", "%y-%m-%d", "%Y-%j", "%I:%M %p", "%Y-%m-%d %z")) {
    expect_error(
      DTAColumnSpecStructureR(type = "Date", format = format),
      "Unsupported R format",
      info = format
    )
  }
  expect_error(DTAColumnSpecStructureR(type = "Date", format = "%d %b %Y"), "directive '%b' is not supported")
  expect_error(DTAColumnSpecStructureR(type = "Date", format = "%Y%%"), "directive '%%' is not supported")
})

test_that("a format of the wrong family for the declared type is refused", {
  expect_error(DTAColumnSpecStructureR(type = "Date", format = "%H:%M"), "describes a hms, which does not fit the type Date")
  expect_error(DTAColumnSpecStructureR(type = "Date", format = "%Y-%m-%d %H:%M:%S"), "describes a POSIXct")
  expect_error(DTAColumnSpecStructureR(type = "POSIXct", format = "%Y-%m-%d"), "describes a Date")
  expect_error(DTAColumnSpecStructureR(type = "hms", format = "%Y-%m-%d"), "describes a Date")
})

test_that("a format that describes no family is refused even without a type", {
  expect_error(DTAColumnSpecStructureR(format = "%Y"), "does not describe a Date, POSIXct or hms")
  expect_error(DTAColumnSpecStructureR(type = "Date", format = "%Y-%m"), "does not describe a Date, POSIXct or hms")
})

test_that("type is inferred from the format when it is omitted", {
  cases <- list(
    list(format = "%Y-%m-%d", type = "Date"),
    list(format = "%d/%m/%Y", type = "Date"),
    list(format = "%Y%m%d", type = "Date"),
    list(format = "%F", type = "Date"),
    list(format = "%Y-%m-%d %H:%M:%S", type = "POSIXct"),
    list(format = "%Y-%m-%dT%H:%M:%OS3Z", type = "POSIXct"),
    list(format = "%F %T", type = "POSIXct"),
    list(format = "%H:%M:%S", type = "hms"),
    list(format = "%H:%M", type = "hms"),
    list(format = "%T", type = "hms")
  )
  for (case in cases) {
    x <- DTAColumnSpecStructureR(format = case$format)
    expect_identical(x@type, case$type, info = case$format)
    expect_identical(x@format, case$format, info = case$format)
  }
})

test_that("a Date, POSIXct or hms with no format stores its default format", {
  expect_identical(DTAColumnSpecStructureR(type = "Date")@format, "%Y-%m-%d")
  expect_identical(DTAColumnSpecStructureR(type = "POSIXct")@format, "%Y-%m-%d %H:%M:%S")
  expect_identical(DTAColumnSpecStructureR(type = "hms")@format, "%H:%M:%S")
  for (type in c("character", "numeric", "integer", "logical")) {
    expect_null(DTAColumnSpecStructureR(type = type)@format, info = type)
  }
})

test_that("an explicit format is stored as given, not replaced by the default", {
  expect_identical(DTAColumnSpecStructureR(type = "Date", format = "%d.%m.%Y")@format, "%d.%m.%Y")
  expect_identical(DTAColumnSpecStructureR(type = "hms", format = "%H:%M")@format, "%H:%M")
  expect_identical(
    DTAColumnSpecStructureR(type = "POSIXct", format = "%Y-%m-%dT%H:%M:%SZ")@format,
    "%Y-%m-%dT%H:%M:%SZ"
  )
})

# ---- the factory and the YAML round trip --------------------------------------

test_that("the factory builds an R structure from the R prefix", {
  x <- DTAColumnSpecStructureFactory(type = "R numeric")
  expect_s3_class(x, "DTAtools::DTAColumnSpecStructureR")
  expect_identical(x@type, "numeric")
  expect_identical(x@backend, "R")

  # The format alone names the backend, as it does for SAS.
  y <- DTAColumnSpecStructureFactory(format = "R %H:%M")
  expect_s3_class(y, "DTAtools::DTAColumnSpecStructureR")
  expect_identical(y@type, "hms")
})

test_that("as.list writes the prefixed type and format, and the factory reads them back", {
  x <- DTAColumnSpecStructureFactory(type = "R Date")
  out <- as.list(x)
  expect_identical(out$type, "R Date")
  expect_identical(out$format, "R %Y-%m-%d")
  expect_null(out$length)

  again <- DTAColumnSpecStructureFactory(type = out$type, format = out$format)
  expect_identical(as.list(again), out)

  chr <- as.list(DTAColumnSpecStructureFactory(type = "R character", length = 9))
  expect_identical(chr, list(type = "R character", length = 9))
  expect_identical(as.list(DTAColumnSpecStructureFactory(type = "R logical")), list(type = "R logical"))
})

test_that("a column specification keeps its R type, format and length through as.list and YAML", {
  cols <- list(
    list(id = "A", type = "R Date"),
    list(id = "B", type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%OS3Z"),
    list(id = "C", type = "R hms", format = "R %H: %M"),
    list(id = "D", type = "R character", length = 12),
    list(id = "E", type = "R double"),
    list(id = "F", type = "R integer"),
    list(id = "G", type = "R logical", nullable = TRUE)
  )
  for (col in cols) {
    spec <- do.call(DTAColumnSpec, col)
    written <- yaml::as.yaml(Filter(Negate(is.null), as.list(spec)))
    read <- yaml::yaml.load(written)
    spec2 <- do.call(DTAColumnSpec, read)
    expect_equal(as.list(spec2), as.list(spec), info = col$id)
    expect_identical(spec2@structure@type, spec@structure@type, info = col$id)
    expect_identical(spec2@structure@format, spec@structure@format, info = col$id)
  }
})

test_that("the factory still refuses a backend that is not supported, and mismatched prefixes", {
  expect_error(
    DTAColumnSpecStructureFactory(type = "Python Char"),
    "supported backends"
  )
  expect_error(
    DTAColumnSpecStructureFactory(type = "R Date", format = "Python %Y"),
    "supported backends"
  )
  expect_error(
    DTAColumnSpecStructureFactory(type = "SAS Char", format = "R %Y-%m-%d"),
    "prefixes must be the same"
  )
  expect_error(
    DTAColumnSpecStructureFactory(type = "R Date", format = "SAS DATE9."),
    "prefixes must be the same"
  )
})

test_that("a pattern on an R logical column is refused with its own class", {
  err <- expect_error(
    DTAColumnSpec(id = "X", type = "R logical", pattern = "a"),
    class = "dta_spec_logical_pattern"
  )
  expect_match(conditionMessage(err), "X: 'pattern' is not supported for an R logical column", fixed = TRUE)
})

test_that("a pattern stays allowed on every other R type and on SAS", {
  expect_no_error(DTAColumnSpec(id = "X", type = "R character", pattern = "^a$"))
  expect_no_error(DTAColumnSpec(id = "X", type = "R integer", pattern = "^[0-9]$"))
  expect_no_error(DTAColumnSpec(id = "X", type = "SAS Char", pattern = "^a$"))
})

# ---- the three mappings -------------------------------------------------------

test_that("as_json_schema_type maps each R type", {
  expected <- c(
    character = "string", numeric = "number", integer = "integer", logical = "boolean",
    Date = "string", POSIXct = "string", hms = "string"
  )
  for (type in r_types) {
    expect_identical(
      as_json_schema_type(DTAColumnSpecStructureR(type = type)),
      unname(expected[type]),
      info = type
    )
  }
  expect_identical(as_json_schema_type(DTAColumnSpecStructureR(length = 4)), "string")
})

test_that("as_r_type maps each R type to its storage type", {
  expected <- c(
    character = "character", numeric = "double", integer = "integer", logical = "logical",
    Date = "character", POSIXct = "character", hms = "character"
  )
  for (type in r_types) {
    expect_identical(
      as_r_type(DTAColumnSpecStructureR(type = type)),
      unname(expected[type]),
      info = type
    )
  }
  expect_identical(as_r_type(DTAColumnSpecStructureR()), "character")
})

test_that("every R type whose JSON type is string is stored as character", {
  for (type in r_types) {
    x <- DTAColumnSpecStructureR(type = type)
    if (identical(as_json_schema_type(x), "string")) {
      expect_identical(as_r_type(x), "character", info = type)
    }
  }
})

test_that("get_arrow_type maps each R type, with no Arrow type for the temporal ones", {
  expected <- c(
    character = "utf8", numeric = "double", integer = "int32", logical = "bool",
    Date = NA_character_, POSIXct = NA_character_, hms = NA_character_
  )
  for (type in r_types) {
    expect_identical(
      get_arrow_type(DTAColumnSpec(id = "X", type = paste("R", type))),
      unname(expected[type]),
      info = type
    )
  }
})

test_that("get_arrow_type for SAS is unchanged by the R arm", {
  expect_identical(get_arrow_type(DTAColumnSpec(id = "X", type = "SAS Char")), "utf8")
  expect_identical(get_arrow_type(DTAColumnSpec(id = "X", type = "SAS Num")), "double")
  expect_identical(get_arrow_type(DTAColumnSpec(id = "X", type = "SAS Int")), "int32")
  expect_identical(get_arrow_type(DTAColumnSpec(id = "X", type = "SAS Date")), NA_character_)
  expect_identical(get_arrow_type(DTAColumnSpec(id = "X", type = "SAS Time")), NA_character_)
  expect_identical(get_arrow_type(DTAColumnSpec(id = "X", type = "SAS DateTime")), NA_character_)
})

test_that("the SAS mappings are unchanged by the R backend", {
  expect_identical(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Char")), "string")
  expect_identical(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Num")), "number")
  expect_identical(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Int")), "integer")
  expect_identical(as_json_schema_type(DTAColumnSpecStructureSAS(type = "Date")), "string")
  expect_identical(as_r_type(DTAColumnSpecStructureSAS(type = "Num")), "double")
  expect_identical(as_r_type(DTAColumnSpecStructureSAS(type = "Date")), "character")
})

# ---- end to end through DTAColumnSpec -----------------------------------------

test_that("a DTAColumnSpec of an R type builds its JSON schema", {
  num <- as_json_schema(DTAColumnSpec(id = "X", type = "R numeric"))
  expect_identical(num$type, "number")
  expect_null(num$maxLength)
  expect_null(num$format)

  int <- as_json_schema(DTAColumnSpec(id = "X", type = "R integer", nullable = TRUE))
  expect_identical(int$type, c("integer", "null"))

  chr <- as_json_schema(DTAColumnSpec(id = "X", type = "R character", length = 8))
  expect_identical(chr$type, "string")
  expect_identical(chr$maxLength, 8)

  lgl <- as_json_schema(DTAColumnSpec(id = "X", type = "R logical", nullable = FALSE))
  expect_identical(lgl$type, "boolean")

  date <- as_json_schema(DTAColumnSpec(id = "X", type = "R Date", values = "2024-01-31"))
  expect_identical(date$type, "string")
  expect_identical(date$const, "2024-01-31")
})

test_that("a nullable logical column with values keeps NA in the schema enum", {
  # Built without the constructor's self-check of declared values, so this pins
  # only what as_json_schema() does with a boolean enum.
  spec <- DTAColumnSpec(id = "X", type = "R logical", nullable = TRUE)
  S7::prop(spec, "values", check = FALSE) <- c("TRUE", "FALSE")
  schema <- as_json_schema(spec)
  expect_identical(schema$type, c("boolean", "null"))
  expect_identical(schema$enum, c(TRUE, FALSE, NA))

  strict <- DTAColumnSpec(id = "X", type = "R logical", nullable = FALSE)
  S7::prop(strict, "values", check = FALSE) <- c("TRUE", "FALSE")
  expect_identical(as_json_schema(strict)$enum, c(TRUE, FALSE))
})

test_that("a nullable string, number and integer enum still gain their missing value", {
  chr <- DTAColumnSpec(id = "X", type = "R character", nullable = TRUE, values = c("a", "b"))
  expect_identical(as_json_schema(chr)$enum, c("a", "b", NA, ""))
  num <- DTAColumnSpec(id = "X", type = "R numeric", nullable = TRUE, values = c(1, 2))
  expect_identical(as_json_schema(num)$enum, c(1, 2, NA))
  int <- DTAColumnSpec(id = "X", type = "R integer", nullable = TRUE, values = c(1L, 2L))
  expect_identical(as_json_schema(int)$enum, c(1L, 2L, NA_integer_))
})

test_that("logical values or examples written as YAML logicals still abort, and the hint names R logical", {
  err <- expect_error(
    DTAColumnSpec(id = "X", type = "R logical", values = c(TRUE, FALSE)),
    class = "dta_spec_logical_values"
  )
  expect_match(conditionMessage(err), "R logical", fixed = TRUE)
  expect_match(conditionMessage(err), "quote them", fixed = TRUE)

  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Char", values = c(TRUE, FALSE)),
    class = "dta_spec_logical_values"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", examples = TRUE),
    class = "dta_spec_logical_values"
  )
})

test_that("print renders an R structure", {
  x <- DTAColumnSpecStructureR(type = "Date")
  # cli_fmt() collects the rendering whatever stream cli writes to.
  out <- cli::cli_fmt(print(x))
  expect_true(any(grepl("DTAColumnSpecStructureR", out, fixed = TRUE)))
  expect_true(any(grepl("%Y-%m-%d", out, fixed = TRUE)))
})

# ---- the one vocabulary table -------------------------------------------------

test_that("dta_backend_types lists each backend's types in picker order", {
  expect_identical(dta_backend_types("SAS"), c("Char", "Num", "Int", "Date", "Time", "DateTime"))
  expect_identical(
    dta_backend_types("R"),
    c("character", "numeric", "integer", "logical", "Date", "POSIXct", "hms")
  )
  expect_identical(dta_backend_types("Python"), character(0))
  expect_identical(dta_backend_types(NULL), character(0))
  expect_identical(dta_backend_types(NA_character_), character(0))
})

test_that("dta_family_of names the kind of column a declared type is", {
  expect_identical(dta_family_of("SAS", "Char"), "text")
  expect_identical(dta_family_of("R", "character"), "text")
  expect_identical(dta_family_of("SAS", "Num"), "number")
  expect_identical(dta_family_of("R", "numeric"), "number")
  expect_identical(dta_family_of("SAS", "Int"), "integer")
  expect_identical(dta_family_of("R", "integer"), "integer")
  expect_identical(dta_family_of("R", "logical"), "logical")
  for (type in c("Date", "Time", "DateTime")) {
    expect_identical(dta_family_of("SAS", type), "temporal", info = type)
  }
  for (type in r_temporal) {
    expect_identical(dta_family_of("R", type), "temporal", info = type)
  }
})

test_that("dta_family_of is NA for anything it does not know and never errors", {
  expect_identical(dta_family_of("SAS", "logical"), NA_character_)
  expect_identical(dta_family_of("SAS", "Bool"), NA_character_)
  expect_identical(dta_family_of("R", "Char"), NA_character_)
  expect_identical(dta_family_of("Python", "Char"), NA_character_)
  expect_identical(dta_family_of("SAS", NULL), NA_character_)
  expect_identical(dta_family_of(NULL, "Char"), NA_character_)
  expect_identical(dta_family_of("SAS", NA_character_), NA_character_)
  expect_identical(dta_family_of(c("SAS", "R"), "Char"), NA_character_)
  expect_identical(dta_family_of("SAS", c("Char", "Num")), NA_character_)
})

test_that("dta_type_family reads a structure, and a missing one is NA", {
  expect_identical(dta_type_family(DTAColumnSpecStructureR(type = "logical")), "logical")
  expect_identical(dta_type_family(DTAColumnSpecStructureSAS(type = "Time")), "temporal")
  expect_identical(dta_type_family(DTAColumnSpecStructureR(length = 3)), NA_character_)
  expect_identical(dta_type_family(NULL), NA_character_)
  spec <- DTAColumnSpec(id = "X", type = "R integer")
  expect_identical(dta_type_family(spec@structure), "integer")
})

test_that("the family table and the class vocabularies agree", {
  # Every type the table lists is accepted by its class, and every type the class
  # accepts is in the table, so a picker built from the table cannot offer a type
  # the constructor refuses.
  for (type in dta_backend_types("SAS")) {
    expect_identical(DTAColumnSpecStructureSAS(type = type)@type, type, info = type)
  }
  for (type in dta_backend_types("R")) {
    expect_identical(DTAColumnSpecStructureR(type = type)@type, type, info = type)
  }
  expect_setequal(
    unname(`__dta_r_type_aliases__`),
    c(dta_backend_types("R"), "numeric")
  )
  expect_setequal(unname(`__dta_sas_type_aliases__`), dta_backend_types("SAS"))
  expect_identical(`__DTAtools_supported_backends__`, c("SAS", "R"))
  expect_setequal(names(`__DTAtools_type_families__`), `__DTAtools_supported_backends__`)
})

# ---- the backend of an R structure ------------------------------------------------

test_that("an R structure cannot be given another backend after construction", {
  x <- DTAColumnSpecStructureR(type = "Date")
  expect_error(S7::`prop<-`(x, "backend", value = "SAS"), "'backend' must be")
  expect_error(S7::`prop<-`(x, "backend", value = "Python"), "'backend' must be")
  # the same refusal when the value reaches the property by assignment
  y <- DTAColumnSpecStructureR(type = "POSIXct")
  expect_error(
    {
      y@backend <- "SAS"
    },
    "'backend' must be"
  )
  # a valid structure is untouched, and the refusal leaves the original intact
  expect_identical(x@backend, "R")
  expect_identical(x@format, "%Y-%m-%d")
  expect_no_error(S7::validate(x))
  # assigning the backend it already has is fine
  expect_no_error(S7::`prop<-`(x, "backend", value = "R"))
})

test_that("a refused format puts a control character in the message as its escape", {
  # in the middle: the constructor trims the ends of a format, a tab there is gone
  err <- expect_error(
    DTAColumnSpecStructureR(type = "hms", format = "%H:\t%M"), "Unsupported R format"
  )
  expect_match(conditionMessage(err), "'%H:\\t%M'", fixed = TRUE)
  expect_match(conditionMessage(err), "The character '\\t' is not allowed", fixed = TRUE)
  expect_false(grepl("\t", conditionMessage(err), fixed = TRUE))
  err <- expect_error(
    DTAColumnSpecStructureR(type = "Date", format = "%Y\n%m"), "Unsupported R format"
  )
  # cli may wrap the message, so the escape is looked for, not the absence of a break
  expect_match(conditionMessage(err), "'%Y\\n%m'", fixed = TRUE)
})
