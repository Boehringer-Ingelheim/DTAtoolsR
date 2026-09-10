# Declared types and typing at read. REQ-TYPE-001 .. REQ-TYPE-011.
#
# Two mappings are exhaustively enumerated here, straight from the source
# rather than from what a user might expect: the six-value type vocabulary
# `DTAColumnSpecStructureSAS` accepts (and the SAS format strings that infer
# one of them), and the two things a declared type is turned into --
# `as_r_type()` for storage, `get_arrow_type()` for the Arrow schema. The rest
# of the file is about the boundary between "typed at read" and "typed in R":
# what a written file preserves and drops, what the reader is allowed to do to
# a column's shape, and where the two typing routes -- the whole-column engine
# and the batching one -- can disagree.

# ---- fixtures ---------------------------------------------------------------

# Writes `lines` verbatim (a header plus one row per remaining element) and
# checks the resulting one-table dataset, exactly as a user would: through a
# written file rather than a data frame handed straight to the engine. That
# matters here specifically because the reader types a declared column before
# anything else in this file sees it -- see REQ-TYPE-008.
td_check <- function(lines, specs, dir, name = "t", stream = "never", ...) {
  path <- file.path(dir, paste0(name, as.integer(stats::runif(1, 1, 1e9)), ".csv"))
  writeLines(lines, path)
  ds <- DTADataSetTabular(
    name = name, specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
  check(ds, persist = FALSE, quiet = TRUE, ...)
}

td_axis <- function(ds) {
  st <- validation_status(ds)
  c(
    ok = st$ok,
    columnspec = as.integer(st$n_columnspec_errors),
    rule = as.integer(st$n_rule_errors),
    import = as.integer(st$n_import_errors)
  )
}

# messages()'s row is a double throughout the package (so a table past
# .Machine$integer.max rows can still be pointed at), so it is narrowed here
# before comparison -- exactly as test-OQ-VAL-columnspec.R's vs_found() does.
td_messages <- function(ds) {
  msgs <- as.data.frame(messages(ds))[, c("source", "row", "column", "keyword")]
  msgs$row <- as.integer(msgs$row)
  rownames(msgs) <- NULL
  msgs
}

td_column <- function(ds, column) {
  as.data.frame(tables(ds)[[1]])[[column]]
}

# ---- the six-value type vocabulary and its two mappings ---------------------
#
# One test case per declared type, each checking three things at once: the
# constructor accepted the backend-prefixed string, `as_r_type()` names the
# storage type import will use, and `get_arrow_type()` names the Arrow type --
# the two mappings REQ-TYPE-004 and REQ-TYPE-005 require to agree wherever
# both are defined.

test_that("OQ-TYPE-001 | a declared Char column maps to R character and Arrow utf8 | REQ-TYPE-003 REQ-TYPE-004 REQ-TYPE-005", {
  spec <- DTAColumnSpec(id = "X", type = "SAS Char")
  qa_step("the constructor records the declared type", "Char", spec@structure@type)
  qa_step("as_r_type() stores it as character", "character", as_r_type(spec@structure))
  qa_step("get_arrow_type() reads it as utf8", "utf8", get_arrow_type(spec))
})

test_that("OQ-TYPE-002 | a declared Num column maps to R double and Arrow double | REQ-TYPE-003 REQ-TYPE-004 REQ-TYPE-005", {
  spec <- DTAColumnSpec(id = "X", type = "SAS Num")
  qa_step("the constructor records the declared type", "Num", spec@structure@type)
  qa_step("as_r_type() stores it as double", "double", as_r_type(spec@structure))
  qa_step("get_arrow_type() reads it as double", "double", get_arrow_type(spec))
})

test_that("OQ-TYPE-003 | a declared Int column maps to R integer and Arrow int32 | REQ-TYPE-003 REQ-TYPE-004 REQ-TYPE-005", {
  spec <- DTAColumnSpec(id = "X", type = "SAS Int")
  qa_step("the constructor records the declared type", "Int", spec@structure@type)
  qa_step("as_r_type() stores it as integer", "integer", as_r_type(spec@structure))
  qa_step("get_arrow_type() reads it as int32", "int32", get_arrow_type(spec))
})

test_that("OQ-TYPE-004 | a declared Date column maps to R character and no Arrow type | REQ-TYPE-003 REQ-TYPE-004 REQ-TYPE-005", {
  spec <- DTAColumnSpec(id = "X", type = "SAS Date")
  qa_step("the constructor records the declared type", "Date", spec@structure@type)
  # Validated as a string (its pattern/format, not a parsed Date), so it is
  # also stored as the text that was read -- parsing it would re-render the
  # value and validate something other than what the file contained.
  qa_step("as_r_type() stores it as character, not Date", "character", as_r_type(spec@structure))
  # get_arrow_type()'s switch() has no Date/Time/DateTime branch and falls to
  # its NA_character_ default -- a documented gap, already pinned at
  # tests/testthat/test-DTAColumnSpec.R:98, restated here because it is part
  # of the same mapping REQ-TYPE-005 states. get_arrow_type() has no caller
  # elsewhere in the package, so the gap has no reader today.
  qa_step("get_arrow_type() has no branch for Date and returns NA", NA_character_, get_arrow_type(spec))
})

test_that("OQ-TYPE-005 | a declared Time column maps to R character and no Arrow type | REQ-TYPE-003 REQ-TYPE-004 REQ-TYPE-005", {
  spec <- DTAColumnSpec(id = "X", type = "SAS Time")
  qa_step("the constructor records the declared type", "Time", spec@structure@type)
  qa_step("as_r_type() stores it as character", "character", as_r_type(spec@structure))
  qa_step("get_arrow_type() has no branch for Time and returns NA", NA_character_, get_arrow_type(spec))
})

test_that("OQ-TYPE-006 | a declared DateTime column maps to R character and no Arrow type | REQ-TYPE-003 REQ-TYPE-004 REQ-TYPE-005", {
  spec <- DTAColumnSpec(id = "X", type = "SAS DateTime")
  qa_step("the constructor records the declared type", "DateTime", spec@structure@type)
  qa_step("as_r_type() stores it as character", "character", as_r_type(spec@structure))
  qa_step("get_arrow_type() has no branch for DateTime and returns NA", NA_character_, get_arrow_type(spec))
})

test_that("OQ-TYPE-007 | the declared-type vocabulary is matched case-insensitively | REQ-TYPE-003", {
  qa_step("lowercase 'char' normalises to 'Char'", "Char", DTAColumnSpec(id = "X", type = "SAS char")@structure@type)
  qa_step("mixed-case 'nUm' normalises to 'Num'", "Num", DTAColumnSpec(id = "X", type = "SAS nUm")@structure@type)
  qa_step("uppercase 'DATETIME' normalises to 'DateTime'", "DateTime", DTAColumnSpec(id = "X", type = "SAS DATETIME")@structure@type)
})

test_that("OQ-TYPE-008 | a type outside the six-value vocabulary is rejected | REQ-TYPE-003", {
  # "Bool" is handled by as_r_type()'s and as_json_schema_type()'s switch()
  # statements (both fall to a "logical"/"boolean" case) but is absent from
  # the validator's own supported_types list -- so it is dead code for the
  # only backend this package implements, not a usable sixth type. Also
  # exercised by OQ-TYPE-023, from the other direction.
  err <- tryCatch(
    {
      DTAColumnSpec(id = "X", type = "SAS Bool")
      NULL
    },
    error = function(e) e
  )
  qa_check("construction with an unsupported type aborts", inherits(err, "condition"))
})

test_that("OQ-TYPE-009 | a column with no declared type falls back to character storage | REQ-TYPE-004", {
  spec <- DTAColumnSpecStructureSAS()
  qa_check("the structure carries no type", is.null(spec@type))
  qa_step("as_r_type() defaults an unset type to character", "character", as_r_type(spec))
})

# ---- SAS format inference ----------------------------------------------------
#
# One test case per format family, each checked without an explicit `type`
# so that only the format's own inference is exercised.

test_that("OQ-TYPE-010 | a $w. format infers Char | REQ-TYPE-001", {
  qa_step("'$12.' infers Char", "Char", DTAColumnSpec(id = "X", format = "SAS $12.")@structure@type)
  # The bare-digit shorthand is normalised to the canonical form first.
  qa_step("the shorthand '$12' is normalised and infers Char the same way", "Char", DTAColumnSpec(id = "X", format = "SAS $12")@structure@type)
})

test_that("OQ-TYPE-011 | a w. format infers Int | REQ-TYPE-001", {
  qa_step("'8.' infers Int", "Int", DTAColumnSpec(id = "X", format = "SAS 8.")@structure@type)
})

test_that("OQ-TYPE-012 | a w.d format infers Num | REQ-TYPE-001", {
  qa_step("'8.2' infers Num", "Num", DTAColumnSpec(id = "X", format = "SAS 8.2")@structure@type)
})

test_that("OQ-TYPE-013 | a BESTw. format infers Int | REQ-TYPE-001", {
  # This is the mapping DEV-001 questions: BESTw. is SAS's general numeric
  # output format and may use scientific notation, so a legitimate decimal
  # could be declared this way. The mapping is stated here as the behaviour
  # on record; the deviation itself is bound once, in test-OQ-CORE-runner.R
  # (OQ-CORE-006), and not re-bound here.
  qa_step("'BEST12.' infers Int", "Int", DTAColumnSpec(id = "X", format = "SAS BEST12.")@structure@type)
})

test_that("OQ-TYPE-014 | a DATEw. format infers Date | REQ-TYPE-001", {
  qa_step("'DATE9.' infers Date", "Date", DTAColumnSpec(id = "X", format = "SAS DATE9.")@structure@type)
})

test_that("OQ-TYPE-015 | a TIMEw.(.d) format infers Time | REQ-TYPE-001", {
  qa_step("'TIME8.' infers Time", "Time", DTAColumnSpec(id = "X", format = "SAS TIME8.")@structure@type)
  qa_step("'TIME8.2', with decimal seconds, also infers Time", "Time", DTAColumnSpec(id = "X", format = "SAS TIME8.2")@structure@type)
})

test_that("OQ-TYPE-016 | a DATETIMEw. format infers DateTime | REQ-TYPE-001", {
  qa_step("'DATETIME18.' infers DateTime", "DateTime", DTAColumnSpec(id = "X", format = "SAS DATETIME18.")@structure@type)
})

# ---- the headline "007" claim ------------------------------------------------

test_that("OQ-TYPE-017 | a declared Char column preserves a leading-zero value through a file round trip | REQ-TYPE-006", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 5, nullable = TRUE)
  ))
  ds <- td_check(c("ID", "007"), specs, dir)
  qa_step("the value survives as the string '007', not the number 7", "007", td_column(ds, "ID"))
  qa_check("the class of the stored column is character", is.character(td_column(ds, "ID")))
})

test_that("OQ-TYPE-018 | a declared Num column parses the same leading-zero text to its numeric value | REQ-TYPE-007", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    N = DTAColumnSpec(id = "N", type = "SAS Num", nullable = TRUE)
  ))
  ds <- td_check(c("N", "007"), specs, dir)
  qa_step("the value parses to the number 7", 7, td_column(ds, "N"))
  qa_check("the class of the stored column is numeric", is.numeric(td_column(ds, "N")))
})

# ---- what the reader is allowed to do to a column's shape -------------------

test_that("OQ-TYPE-019 | one unparseable value in a declared numeric column costs exactly one import error, and the file still reads | REQ-TYPE-008", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    N = DTAColumnSpec(id = "N", type = "SAS Num", nullable = TRUE)
  ))
  ds <- td_check(c("N", "1", "abc", "3"), specs, dir)

  qa_step(
    "the verdict and the three axis counts: one import error, nothing else",
    c(ok = FALSE, columnspec = 0L, rule = 0L, import = 1L),
    td_axis(ds)
  )
  qa_step(
    "the offending cell is reported as not_convertible, on the import axis",
    data.frame(source = "import", row = 2L, column = "N", keyword = "not_convertible", stringsAsFactors = FALSE),
    td_messages(ds)
  )
  # The reader did not give up on the column, or the file: the clean values
  # either side of the bad one are still present, as numbers.
  qa_step("the clean values on either side of the bad cell still read as numbers", c(1, NA, 3), td_column(ds, "N"))
})

# ---- typing at read versus typing in R --------------------------------------
#
# `validate_table_detailed()` -- the un-thrown engine `validate_table()` and
# `check()` are both built on -- is used directly here because it always
# returns the axis counts, whichever axis fails; `validate_table()` itself
# aborts on a rule or import failure instead of returning one. Only counts are
# compared, never message text: the file path types the value first and
# reports the axis a bad conversion belongs to, while a value already typed in
# R was never subject to that conversion at all, so the two routes can
# legitimately name a finding differently while agreeing on how many there
# are.

test_that("OQ-TYPE-020 | a factor column reaches the same columnspec verdict as its text equivalent from a file | REQ-TYPE-010", {
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    SEX = DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F"))
  ))

  df_r <- data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)
  df_r$SEX <- factor(c("M", "X"), levels = c("M", "X"))
  det_r <- validate_table_detailed(specs, df_r, verbose = FALSE)

  dir <- qa_tempdir()
  ds <- td_check(c("ID,SEX", "A001,M", "A002,X"), specs, dir)

  qa_step(
    "a factor's out-of-codelist level costs the same one columnspec error as the same text from a file",
    c(ok = FALSE, columnspec = 1L, rule = 0L, import = 0L),
    c(ok = det_r$ok, columnspec = det_r$n_columnspec_errors, rule = det_r$n_rule_errors, import = det_r$n_import_errors)
  )
  qa_step("...and the file path agrees", c(ok = FALSE, columnspec = 1L, rule = 0L, import = 0L), td_axis(ds))
})

test_that("OQ-TYPE-021 | a Date column reaches the same columnspec verdict as its text equivalent from a file | REQ-TYPE-010", {
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DT = DTAColumnSpec(id = "DT", type = "SAS Date", nullable = FALSE)
  ))

  # A missing Date in a column declared not nullable: dta_base_json_type()
  # reports a Date column as "string", and an NA fails that type check the
  # same way a blank cell would.
  df_r <- data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)
  df_r$DT <- as.Date(c("2020-01-01", NA))
  det_r <- validate_table_detailed(specs, df_r, verbose = FALSE)

  dir <- qa_tempdir()
  ds <- td_check(c("ID,DT", "A001,2020-01-01", "A002,"), specs, dir)

  qa_step(
    "a missing R Date costs the same one columnspec error as a blank cell from a file",
    c(ok = FALSE, columnspec = 1L, rule = 0L, import = 0L),
    c(ok = det_r$ok, columnspec = det_r$n_columnspec_errors, rule = det_r$n_rule_errors, import = det_r$n_import_errors)
  )
  qa_step("...and the file path agrees", c(ok = FALSE, columnspec = 1L, rule = 0L, import = 0L), td_axis(ds))
})

test_that("OQ-TYPE-022 | an integer64 column reaches the same rule verdict as its text equivalent from a file | REQ-TYPE-010", {
  qa_requires("bit64")

  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      QTY = DTAColumnSpec(id = "QTY", type = "SAS Int", nullable = FALSE)
    ),
    rules = list(DTARuleColRange(id = "qty_range", columns = "QTY", range = c(1, 10)))
  )

  # 5,000,000,000 is a valid integer64 value, far outside the 32-bit range a
  # declared Int narrows to (REQ-TYPE-009) and outside the declared range
  # rule -- exercising both an unusual R storage type and the rule axis in
  # the same value.
  df_r <- data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)
  df_r$QTY <- bit64::as.integer64(c(5, 5000000000))
  det_r <- validate_table_detailed(specs, df_r, verbose = FALSE)

  dir <- qa_tempdir()
  ds <- td_check(c("ID,QTY", "A001,5", "A002,5000000000"), specs, dir)

  qa_step(
    "an out-of-range integer64 value costs the same one rule error as the same text from a file",
    c(ok = FALSE, columnspec = 0L, rule = 1L, import = 0L),
    c(ok = det_r$ok, columnspec = det_r$n_columnspec_errors, rule = det_r$n_rule_errors, import = det_r$n_import_errors)
  )
  qa_step("...and the file path agrees", c(ok = FALSE, columnspec = 0L, rule = 1L, import = 0L), td_axis(ds))
})

test_that("OQ-TYPE-023 | a native logical column fails the type check against every declarable column type | REQ-TYPE-011", {
  # The mirror image of OQ-TYPE-008: "Bool" cannot be declared, so no schema
  # this package can generate ever names JSON type "boolean" -- which is
  # exactly the type dta_base_json_type() gives a logical column. A logical
  # value therefore fails type checking against a Char column even though it
  # would satisfy a values= codelist of "TRUE"/"FALSE" if it were text.
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    FLAG = DTAColumnSpec(id = "FLAG", type = "SAS Char", length = 5, nullable = FALSE, values = c("TRUE", "FALSE"))
  ))
  df_r <- data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)
  df_r$FLAG <- c(TRUE, FALSE)
  det_r <- validate_table_detailed(specs, df_r, verbose = FALSE)

  qa_step(
    "both rows fail the type check although their text form would satisfy the codelist",
    c(ok = FALSE, columnspec = 2L),
    c(ok = det_r$ok, columnspec = det_r$n_columnspec_errors)
  )

  # Read from a file, the same conceptual values are plain text and validate
  # cleanly -- the divergence this test pins is specific to a table built
  # directly in R with a logical column, not to the values themselves.
  dir <- qa_tempdir()
  ds <- td_check(c("ID,FLAG", "A001,TRUE", "A002,FALSE"), specs, dir)
  qa_step("the same values, read as text from a file, validate cleanly", c(ok = TRUE, columnspec = 0L), c(ok = validation_status(ds)$ok, columnspec = as.integer(validation_status(ds)$n_columnspec_errors)))
})

# ---- Int narrowing ------------------------------------------------------------

test_that("OQ-TYPE-024 | a value outside the 32-bit integer range is stored as double, on no axis | REQ-TYPE-009", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    N = DTAColumnSpec(id = "N", type = "SAS Int", nullable = TRUE)
  ))

  # An all-whole, in-range column narrows to integer storage, for contrast.
  ds_clean <- td_check(c("N", "1", "2", "3"), specs, dir)
  qa_step("an all-whole, in-range column narrows to R integer", "integer", class(td_column(ds_clean, "N")))

  # One value one past the 32-bit signed range (2^31), everything else
  # ordinary. `.Machine$integer.max` is 2^31 - 1.
  ds_wide <- td_check(c("N", "1", "2", "2147483648"), specs, dir)
  qa_step(
    "the whole column stays double instead, because it cannot be narrowed as a whole",
    "numeric", class(td_column(ds_wide, "N"))
  )
  qa_step(
    "and this is reported on no axis at all -- a silent narrowing failure, pinned as REQ-TYPE-009",
    c(ok = TRUE, columnspec = 0L, rule = 0L, import = 0L),
    td_axis(ds_wide)
  )
  # Bound to the register: when the package starts reporting an out-of-range
  # value on the import axis, this stops reproducing and the run fails until
  # DEV-006 is closed and the two steps above are rewritten as the pass they
  # should have been.
  qa_known_deviation(
    "DEV-006",
    all(td_axis(ds_wide)[c("columnspec", "rule", "import")] == 0L)
  )
  # The most-negative value R would need is one further out than +2^31-1,
  # because .Machine$integer.max is checked against abs(); the same failure
  # to narrow, at the other end.
  ds_neg <- td_check(c("N", "1", "-2147483648"), specs, dir)
  qa_step("the same holds at the negative boundary", "numeric", class(td_column(ds_neg, "N")))
})

test_that("OQ-TYPE-025 | the whole-column and per-batch engines can type the same declared Int column differently | REQ-TYPE-002", {
  # dta_coerce_table_to_specs() is the one function both engines call to type
  # a declared column: load_file(stream = "never") calls it once over the
  # whole table, and check()'s streaming scan calls it once per batch (see
  # R/streamingValidation.R, the loop that builds `coerced <-
  # dta_coerce_table_to_specs(df, specs, ...)`). An Int column narrows to R
  # integer only when every value *currently in hand* is whole and within
  # +/-2^31-1 (R/importConversion.R, dta_coerce_column()) -- so "in hand"
  # meaning the whole column versus one batch is exactly where the two
  # engines can part ways.
  specs <- DTAColumnSpecCollection(columns = list(
    N = DTAColumnSpec(id = "N", type = "SAS Int", nullable = TRUE)
  ))
  batch_rows <- 5L
  rows_a <- as.character(1:5) # batch 1: all whole, all in range
  rows_b <- c("6", "7", "8.5", "9", "10") # batch 2: one fractional value
  lines <- c(rows_a, rows_b)

  whole <- data.frame(N = lines, stringsAsFactors = FALSE)
  first_batch <- data.frame(N = lines[seq_len(batch_rows)], stringsAsFactors = FALSE)

  whole_result <- dta_coerce_table_to_specs(whole, specs)
  batch_result <- dta_coerce_table_to_specs(first_batch, specs)

  # Row 4 ("4") is identical text in both calls; only the scope of "what else
  # is in hand" differs.
  qa_step(
    "row 4's value ('4') is typed as double when the whole column is in hand...",
    "numeric", class(whole_result$table$N[4])
  )
  qa_step(
    "...and as integer when only its own batch is in hand",
    "integer", class(batch_result$table$N[4])
  )
  qa_known_deviation(
    "DEV-004",
    !identical(class(whole_result$table$N), class(batch_result$table$N))
  )

  # This is stated plainly rather than left implicit: verified separately
  # (not asserted here, to keep this case to the one defect it binds) that
  # driving the same file through load_file(stream = "never") versus
  # load_file(stream = "always") + check(batch_rows = 5) reports IDENTICAL
  # validation_status()/messages() on both paths for this scenario. The
  # divergence DEV-004 describes is real and is exactly the one shown above,
  # but it is invisible on the import and column-specification axes here:
  # narrowing failure is never an import error (REQ-TYPE-009), and the
  # column-specification "type" check re-tests whole-ness value by value
  # regardless of storage class (R/columnSpecChecks.R, the `elementwise`
  # branch of the type check), so a whole value such as "4" passes an
  # "integer" schema type whether it is stored as R integer or as double.
})
