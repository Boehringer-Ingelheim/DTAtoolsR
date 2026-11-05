# Column-specification checks. REQ-VAL-003 .. REQ-VAL-013, REQ-VAL-031.
#
# One test case per declared constraint. Each builds a table in which exactly
# one row breaks exactly one rule, so a failure names the constraint rather
# than leaving a reviewer to work out which of several things went wrong.
#
# Everything here goes through a written file rather than handing a data frame
# straight to the engine. That is deliberate: a declared numeric column holding
# text is a type error on every row when the frame is passed directly, because
# nothing has typed it yet, while the reader types it first and reports the one
# cell that could not be converted. The file path is the one users take.

# ---- fixtures ---------------------------------------------------------------

vs_specs <- function(cols, rules = list()) {
  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = rules
  )
}

# Returns the checked dataset. The table name is taken from the file, so it is
# read back rather than assumed.
vs_check <- function(frame, specs, dir, stream = "never", ...) {
  path <- file.path(dir, paste0("t", as.integer(stats::runif(1, 1, 1e9)), ".csv"))
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  ds <- DTADataSetTabular(
    id = "qual", specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
  check(ds, persist = FALSE, quiet = TRUE, ...)
}

# The two engines are one contract (REQ-STREAM): the whole table in memory, and
# the same file scanned in batches. A value-level rule that held in one and not
# the other would give two verdicts on one delivery, so the cases below that
# state such a rule state it for both.
vs_engines <- c(memory = "never", streaming = "always")

# The reported messages reduced to what a requirement is about: which axis,
# which row, which column, which constraint. Message text is excluded because
# it is prose and may be reworded without the verdict changing.
vs_found <- function(ds) {
  msgs <- as.data.frame(messages(ds))
  if (nrow(msgs) == 0) {
    return(data.frame(
      source = character(0), row = integer(0), column = character(0),
      keyword = character(0), stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    source = as.character(msgs$source),
    row = as.integer(msgs$row),
    column = as.character(msgs$column),
    keyword = as.character(msgs$keyword),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$source, out$row, out$column, out$keyword), , drop = FALSE]
  rownames(out) <- NULL
  out
}

vs_expect <- function(source, row, column, keyword) {
  data.frame(
    source = source, row = as.integer(row), column = column, keyword = keyword,
    stringsAsFactors = FALSE
  )
}

# ---- one test case per constraint -------------------------------------------

test_that("OQ-VAL-001 | a declared column absent from the table is reported for every row | REQ-VAL-003", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "MISSING", type = "SAS Char", length = 4, nullable = FALSE)
  ))

  # The count scales with the number of rows, which is the behaviour on record
  # rather than the behaviour one would design. Checking two row counts is what
  # distinguishes "once per row" from "once per table" -- a single table cannot.
  for (n in c(2L, 5L)) {
    frame <- data.frame(ID = sprintf("A%03d", seq_len(n)), stringsAsFactors = FALSE)
    ds <- vs_check(frame, specs, dir)
    qa_step(
      sprintf("an absent column costs one error per row, at %d rows", n),
      n, as.integer(validation_status(ds)$n_columnspec_errors)
    )
    qa_step(
      sprintf("each is reported with the keyword 'required', at %d rows", n),
      rep("required", n), vs_found(ds)$keyword
    )
  }
})

test_that("OQ-VAL-002 | a column the specification does not declare is reported once | REQ-VAL-004", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
  frame <- data.frame(
    ID = c("A001", "A002", "A003"), EXTRA = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)

  # Note the asymmetry with a missing column, which is reported per row. An
  # undeclared column is a fact about the table's shape, so one report is
  # enough; a missing one is evaluated per row by the generated schema.
  qa_step(
    "an undeclared column is reported once for the table",
    vs_expect("columnspec", NA, "EXTRA", "additionalProperties"),
    vs_found(ds)
  )
})

test_that("OQ-VAL-003 | a value of the wrong declared type is reported with its row and column | REQ-VAL-005", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "N", type = "SAS Num", nullable = FALSE)
  ))
  # Row 2 is empty in a column declared not nullable, which is the type check
  # rather than a check of its own.
  frame <- data.frame(
    ID = c("A001", "A002"), N = c("42", NA_character_),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)

  qa_step(
    "a missing value in a column declared not nullable is a type violation",
    vs_expect("columnspec", 2L, "N", "type"),
    vs_found(ds)[vs_found(ds)$source == "columnspec", ]
  )
})

test_that("OQ-VAL-004 | a string longer than its declared length is reported | REQ-VAL-006", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
  frame <- data.frame(ID = c("A001", "TOO-LONG"), stringsAsFactors = FALSE)
  ds <- vs_check(frame, specs, dir)

  qa_step(
    "only the over-length value is reported",
    vs_expect("columnspec", 2L, "ID", "maxLength"),
    vs_found(ds)
  )
})

test_that("OQ-VAL-005 | the declared length counts characters, not bytes | REQ-VAL-006", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "NAME", type = "SAS Char", length = 5, nullable = FALSE)
  ))

  # Row 2 is five characters and more than five bytes in UTF-8. An
  # implementation counting bytes would reject it, and would then reject
  # legitimate values in every language that needs characters outside ASCII --
  # which, for a clinical data transfer, is most of them.
  frame <- data.frame(
    NAME = c("abcde", "äöüßé", "äöüßéx"),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)

  qa_step(
    "five multi-byte characters fit a length of five; six do not",
    vs_expect("columnspec", 3L, "NAME", "maxLength"),
    vs_found(ds)
  )
})

test_that("OQ-VAL-006 | a value outside a codelist of several values is reported as enum | REQ-VAL-007", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    )
  ))
  frame <- data.frame(SEX = c("M", "X", "F"), stringsAsFactors = FALSE)
  ds <- vs_check(frame, specs, dir)

  qa_step(
    "the value outside the codelist is reported as an enum violation",
    vs_expect("columnspec", 2L, "SEX", "enum"),
    vs_found(ds)
  )
})

test_that("OQ-VAL-007 | a codelist of exactly one value is reported as const | REQ-VAL-008", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "DOMAIN", type = "SAS Char", length = 2, nullable = FALSE,
      values = "GF"
    )
  ))
  frame <- data.frame(DOMAIN = c("GF", "ZZ"), stringsAsFactors = FALSE)
  ds <- vs_check(frame, specs, dir)

  # The keyword differs from the many-valued case, so a report reads as
  # "this column may only ever hold GF" rather than "GF was not in the list".
  qa_step(
    "a single permitted value is reported as a const violation, not an enum one",
    vs_expect("columnspec", 2L, "DOMAIN", "const"),
    vs_found(ds)
  )
})

test_that("OQ-VAL-008 | a value not matching the declared pattern is reported | REQ-VAL-009", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "CODE", type = "SAS Char", length = 6, nullable = FALSE,
      pattern = "^[A-Z]{3}[0-9]{3}$"
    )
  ))
  frame <- data.frame(CODE = c("ABC123", "abc123"), stringsAsFactors = FALSE)
  ds <- vs_check(frame, specs, dir)

  qa_step(
    "the pattern is matched case-sensitively, as PCRE",
    vs_expect("columnspec", 2L, "CODE", "pattern"),
    vs_found(ds)
  )
})

test_that("OQ-VAL-009 | a missing value in a nullable column is not an error | REQ-VAL-010", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "NOTE", type = "SAS Char", length = 20, nullable = TRUE)
  ))
  # ID keeps every line populated. Without it the second row would be a blank
  # line, which every CSV reader skips, and the case would silently test
  # blank-line handling instead of nullability.
  frame <- data.frame(
    ID = c("A001", "A002"), NOTE = c("present", NA_character_),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)

  qa_step("a nullable column accepts a missing value", 0L, nrow(vs_found(ds)))
  qa_step("and the table is valid", TRUE, validation_status(ds)$ok)
})

test_that("OQ-VAL-010 | one value breaking two constraints is reported twice | REQ-VAL-011", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "ID", type = "SAS Char", length = 4, nullable = FALSE,
      pattern = "^[A-Z][0-9]{3}$"
    )
  ))
  frame <- data.frame(ID = c("A001", "TOO-LONG"), stringsAsFactors = FALSE)
  ds <- vs_check(frame, specs, dir)

  # Reporting only the first violation would hide the second from a supplier
  # trying to fix the delivery in one pass.
  qa_step(
    "an over-length value that also breaks the pattern is reported under both",
    rbind(
      vs_expect("columnspec", 2L, "ID", "maxLength"),
      vs_expect("columnspec", 2L, "ID", "pattern")
    ),
    vs_found(ds)
  )
})

test_that("OQ-VAL-011 | the error frame carries the columns a report is built from | REQ-VAL-012", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    )
  ))
  ds <- vs_check(data.frame(SEX = c("M", "X"), stringsAsFactors = FALSE), specs, dir)
  details <- validation_errors(ds, names(tables(ds))[[1]])
  full <- details$columnspec_errors$full_error

  qa_step(
    "the column-specification error frame carries its documented columns",
    c("row", "column", "keyword", "message", "columnspec", "data"),
    intersect(c("row", "column", "keyword", "message", "columnspec", "data"), names(full))
  )
  qa_step("the offending value is recorded", "X", as.character(full$data[[1]]))
})

test_that("OQ-VAL-012 | every declared check reports its own outcome | REQ-VAL-013", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "ID", type = "SAS Char", length = 4, nullable = FALSE,
      pattern = "^[A-Z][0-9]{3}$"
    ),
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    )
  ))
  frame <- data.frame(
    ID = c("A001", "A002"), SEX = c("M", "X"),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)
  checks <- validation_errors(ds, names(tables(ds))[[1]])$columnspec_checks

  # A per-check report lets a reader see that a constraint was evaluated and
  # held, which a list of failures alone cannot show.
  qa_step(
    "every check label is accounted for",
    c("extra", "format", "length", "pattern", "presence", "values"),
    sort(unique(as.character(checks$check)))
  )
  qa_step(
    "each status is drawn from the documented vocabulary",
    TRUE,
    all(as.character(checks$status) %in%
      c("passed", "failed", "not_applicable", "not_checked"))
  )
  qa_step(
    "the codelist check is the one reported as failed",
    "failed",
    as.character(checks$status[checks$keyword == "enum"])
  )
  qa_step(
    "and the pattern check, which nothing broke, is reported as passed",
    "passed",
    as.character(checks$status[checks$check == "pattern"])
  )
})

test_that("OQ-VAL-013 | a clean table reports no errors on any axis | REQ-VAL-001 REQ-VAL-002", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(
      id = "ID", type = "SAS Char", length = 8, nullable = FALSE,
      pattern = "^S[0-9]{3}$"
    ),
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    ),
    DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
  frame <- data.frame(
    ID = c("S001", "S002"), SEX = c("M", "F"), AGE = c("30", "40"),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)
  status <- validation_status(ds)

  qa_step(
    "a table satisfying every constraint is valid on all three axes",
    list(ok = TRUE, columnspec = 0L, rule = 0L, import = 0L),
    list(
      ok = status$ok,
      columnspec = as.integer(status$n_columnspec_errors),
      rule = as.integer(status$n_rule_errors),
      import = as.integer(status$n_import_errors)
    )
  )
})

test_that("OQ-VAL-014 | the verdict is the conjunction of the three axes | REQ-VAL-001 REQ-VAL-002", {
  dir <- qa_tempdir()
  specs <- vs_specs(
    list(
      DTAColumnSpec(
        id = "ID", type = "SAS Char", length = 4, nullable = FALSE
      ),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
  )

  # Each case breaks exactly one axis, so the conjunction is tested rather than
  # a single combined failure that any one axis could explain.
  cases <- list(
    clean = list(
      frame = data.frame(ID = c("A001", "A002"), AGE = c("30", "40"), stringsAsFactors = FALSE),
      ok = TRUE, axis = c(0L, 0L, 0L)
    ),
    columnspec_only = list(
      frame = data.frame(ID = c("A001", "TOOLONG"), AGE = c("30", "40"), stringsAsFactors = FALSE),
      ok = FALSE, axis = c(1L, 0L, 0L)
    ),
    rule_only = list(
      frame = data.frame(ID = c("A001", "A002"), AGE = c("30", "99"), stringsAsFactors = FALSE),
      ok = FALSE, axis = c(0L, 1L, 0L)
    ),
    # A value the reader cannot parse is reported on the import axis and NOT
    # also as a violation of the range rule that reads the column. The rule
    # treats it as it treats a missing value, which keeps one bad cell from
    # being counted twice; the delivery is still invalid, because the import
    # axis alone is enough to make it so.
    import_only = list(
      frame = data.frame(ID = c("A001", "A002"), AGE = c("30", "abc"), stringsAsFactors = FALSE),
      ok = FALSE, axis = c(0L, 0L, 1L)
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    status <- validation_status(vs_check(case$frame, specs, dir))
    qa_step(
      sprintf("case '%s': the verdict and the three axis counts", name),
      c(ok = case$ok, columnspec = case$axis[[1]], rule = case$axis[[2]], import = case$axis[[3]]),
      c(
        ok = status$ok,
        columnspec = as.integer(status$n_columnspec_errors),
        rule = as.integer(status$n_rule_errors),
        import = as.integer(status$n_import_errors)
      )
    )
  }
})

test_that("OQ-VAL-036 | values, a pattern and nullability are still checked on a column with no declared type | REQ-VAL-029", {
  dir <- qa_tempdir()
  # Neither SEX nor CODE declares a type, format or length -- both have no
  # @structure at all, only the constraints a caller can still declare
  # without one. `values` and `pattern` are mutually exclusive on one column
  # (DTAColumnSpec's own validator), so the codelist and the pattern are
  # exercised on two different untyped columns rather than one. SEX_T is the
  # typed control REQ-VAL-029 measures against: the same codelist and
  # nullability, declared SAS Char, fed the same values.
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "SEX", nullable = FALSE, values = c("M", "F")),
    DTAColumnSpec(id = "SEX_T", type = "SAS Char", nullable = FALSE, values = c("M", "F")),
    DTAColumnSpec(id = "CODE", nullable = TRUE, pattern = "^[A-Z]{3}$")
  ))
  frame <- data.frame(
    ID = c("A001", "A002", "A003"),
    SEX = c("M", "X", NA_character_),
    SEX_T = c("M", "X", NA_character_),
    CODE = c("ABC", "ab1", "XYZ"),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(frame, specs, dir)
  found <- vs_found(ds)

  # The requirement names three findings: the out-of-codelist value (SEX row
  # 2), the non-nullable column's missing value (SEX row 3) and the value
  # that does not match the pattern (CODE row 2). Before the fix, a column
  # with no @structure had no schema at all and none of them was reported.
  # Which keywords each finding carries is not what this case is about --
  # that is the typed-column behaviour compared below.
  untyped <- unique(found[found$column %in% c("SEX", "CODE"), c("row", "column"), drop = FALSE])
  rownames(untyped) <- NULL
  qa_step(
    "the codelist violation, the missing value and the pattern violation are each reported",
    data.frame(row = c(2L, 2L, 3L), column = c("CODE", "SEX", "SEX"), stringsAsFactors = FALSE),
    untyped
  )

  # "Exactly as a typed column's are": the untyped column's findings are the
  # typed control's, row for row and keyword for keyword.
  by_column <- function(col) {
    out <- found[found$column == col, c("source", "row", "keyword"), drop = FALSE]
    rownames(out) <- NULL
    out
  }
  qa_step(
    "and SEX is reported exactly as the typed control column SEX_T is",
    by_column("SEX_T"),
    by_column("SEX")
  )
  qa_step("and the table is invalid because of them", FALSE, validation_status(ds)$ok)
})

# ---- integer range, numeric patterns and SAS formats -------------------------
#
# Every expected value below follows from the rule in the requirement it names,
# worked out by hand from the values written into the frame. None was read off a
# run.

test_that("OQ-VAL-039 | an integer outside plus or minus 2^31-1 is reported as a type violation | REQ-VAL-005", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "N", type = "SAS Int", nullable = TRUE)
  ))
  # 2^31-1 is 2147483647. Row 1 and row 3 sit on the boundary and fit; row 2 is
  # one past it, row 4 is -2^31 (the value R reserves for a missing integer, so
  # not an integer of this column) and row 5 is far outside. Row 6 is missing in
  # a nullable column.
  frame <- data.frame(
    ID = sprintf("A%03d", 1:6),
    N = c("2147483647", "2147483648", "-2147483647", "-2147483648", "5000000000", NA),
    stringsAsFactors = FALSE
  )
  expected <- vs_expect("columnspec", c(2, 4, 5), "N", "type")

  for (engine in names(vs_engines)) {
    ds <- vs_check(frame, specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("only the three out-of-range rows are type violations (%s engine)", engine),
      expected, vs_found(ds)
    )
  }

  # A whole number inside the range is not a violation.
  small <- data.frame(ID = c("A001", "A002"), N = c("7", "-7"), stringsAsFactors = FALSE)
  qa_step("in-range whole values are valid", TRUE, validation_status(vs_check(small, specs, dir))$ok)
})

test_that("OQ-VAL-040 | a pattern declared on a numeric column is applied to the plain decimal text of the number | REQ-VAL-009", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    # A ten-digit numeric identifier: the shape of SUBJIDN in the bundled
    # example specification.
    DTAColumnSpec(
      id = "SUBJIDN", type = "SAS Num", nullable = FALSE, pattern = "^[0-9]{10}$"
    ),
    # Declared Int, so the value is the number 7, not the text "007".
    DTAColumnSpec(
      id = "CODE", type = "SAS Int", nullable = FALSE, pattern = "^[0-9]{3}$"
    ),
    DTAColumnSpec(
      id = "RATIO", type = "SAS Num", nullable = FALSE, pattern = "^[0-9]+\\.[0-9]$"
    )
  ))
  frame <- data.frame(
    # Row 1 is 10^9: its default R rendering is "1e+09", which a ten-digit
    # pattern would wrongly reject; as plain decimal text it is "1000000000".
    # Row 2 has three digits, row 4 eleven; row 3 has ten.
    SUBJIDN = c("1000000000", "123", "1234567890", "12345678901"),
    # "007" is the number 7 once typed, matched as "7"; "5" is one digit.
    CODE = c("123", "007", "5", "999"),
    # 12.25 has two decimals.
    RATIO = c("0.5", "12.25", "7.5", "0.1"),
    stringsAsFactors = FALSE
  )
  expected <- rbind(
    vs_expect("columnspec", 2, "CODE", "pattern"),
    vs_expect("columnspec", 2, "RATIO", "pattern"),
    vs_expect("columnspec", 2, "SUBJIDN", "pattern"),
    vs_expect("columnspec", 3, "CODE", "pattern"),
    vs_expect("columnspec", 4, "SUBJIDN", "pattern")
  )

  for (engine in names(vs_engines)) {
    ds <- vs_check(frame, specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("exactly the rows whose number does not read as the pattern are reported (%s engine)", engine),
      expected, vs_found(ds)
    )
  }
})

test_that("OQ-VAL-041 | a value too long for a declared $w. format is reported as a format violation | REQ-VAL-031", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    # $5. alone: nothing else limits the length.
    DTAColumnSpec(id = "A", type = "SAS Char", format = "SAS $5.", nullable = TRUE),
    # $5. with a longer declared length: the format is the tighter limit.
    DTAColumnSpec(id = "B", type = "SAS Char", format = "SAS $5.", length = 8, nullable = TRUE),
    # $5. with a length of exactly 5: the length already says all there is.
    DTAColumnSpec(id = "C", type = "SAS Char", format = "SAS $5.", length = 5, nullable = TRUE)
  ))
  # Row 3 is five characters in ten bytes: it fits, because the width is in
  # characters. Row 4 is missing everywhere. Row 5 is ten characters.
  umlauts <- "äöüßé"
  frame <- data.frame(
    ID = sprintf("A%03d", 1:5),
    A = c("ABCDE", "ABCDEF", umlauts, NA, "ABCDEFGHIJ"),
    B = c("ABCDE", "ABCDEF", umlauts, NA, "ABCDEFGHIJ"),
    C = c("ABCDE", "ABCDEF", umlauts, NA, "ABCDEFGHIJ"),
    stringsAsFactors = FALSE
  )
  # A: six and ten characters exceed the width of five.
  # B: the same two rows exceed the format; only the ten-character one also
  #    exceeds the length of eight, so it is reported under both keywords.
  # C: the length is 5, so the over-length rows are reported once, as
  #    maxLength, and never additionally as format.
  expected <- rbind(
    vs_expect("columnspec", 2, "A", "format"),
    vs_expect("columnspec", 2, "B", "format"),
    vs_expect("columnspec", 2, "C", "maxLength"),
    vs_expect("columnspec", 5, "A", "format"),
    vs_expect("columnspec", 5, "B", "format"),
    vs_expect("columnspec", 5, "B", "maxLength"),
    vs_expect("columnspec", 5, "C", "maxLength")
  )

  for (engine in names(vs_engines)) {
    ds <- vs_check(frame, specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("the format, length and both-keyword cases are each reported as stated (%s engine)", engine),
      expected, vs_found(ds)
    )
  }
})

test_that("OQ-VAL-042 | a number that does not fit a declared w.d or w. format is reported | REQ-VAL-031", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "F1", type = "SAS Num", format = "SAS 8.2", nullable = TRUE),
    DTAColumnSpec(id = "F2", type = "SAS Num", format = "SAS 5.", nullable = TRUE)
  ))
  # Width under w.d is the integer digits, a minus sign, and (when d > 0) the
  # decimal point with its d digits.
  #   F1 = 8.2:  1.5 -> 1+3 = 4 fits         | 1.234 -> three decimals, no
  #              12345.67 -> 5+3 = 8 fits    | 123456.78 -> 6+3 = 9, no
  #              -12345.67 -> 5+1+3 = 9, no
  #   F2 = 5.:   12345 -> 5 fits             | 123456 -> 6, no
  #              1.5 -> a fraction, no       | -1234 -> 4+1 = 5 fits
  #              -12345 -> 5+1 = 6, no
  frame <- data.frame(
    ID = sprintf("A%03d", 1:6),
    F1 = c("1.5", "1.234", "12345.67", "123456.78", "-12345.67", NA),
    F2 = c("12345", "123456", "1.5", "-1234", "-12345", NA),
    stringsAsFactors = FALSE
  )
  expected <- rbind(
    vs_expect("columnspec", 2, "F1", "format"),
    vs_expect("columnspec", 2, "F2", "format"),
    vs_expect("columnspec", 3, "F2", "format"),
    vs_expect("columnspec", 4, "F1", "format"),
    vs_expect("columnspec", 5, "F1", "format"),
    vs_expect("columnspec", 5, "F2", "format")
  )

  for (engine in names(vs_engines)) {
    ds <- vs_check(frame, specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("exactly the rows that cannot be displayed in the format are reported (%s engine)", engine),
      expected, vs_found(ds)
    )
  }

  # A value below one in magnitude loses its leading zero when the width is
  # tight, and a fraction in an Int column is a type violation only.
  #   G = 3.2:  0.5 -> ".50", 3 characters, fits
  #             1.5 -> "1.50", 4, no       | -0.5 -> "-.50", 4, no
  #   H = 4.2:  -0.5 -> "-.50", 4, fits    | 12.5 -> "12.50", 5, no
  #             0.5 -> "0.50", 4, fits
  #   I = Int 5.:  12345 -> 5 fits  | 1.5 -> not a whole number: type, once
  #                123456 -> 6 characters, no
  tight_specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "G", type = "SAS Num", format = "SAS 3.2", nullable = FALSE),
    DTAColumnSpec(id = "H", type = "SAS Num", format = "SAS 4.2", nullable = FALSE),
    DTAColumnSpec(id = "I", type = "SAS Int", format = "SAS 5.", nullable = FALSE)
  ))
  tight <- data.frame(
    ID = sprintf("A%03d", 1:3),
    G = c("0.5", "1.5", "-0.5"),
    H = c("-0.5", "12.5", "0.5"),
    I = c("12345", "1.5", "123456"),
    stringsAsFactors = FALSE
  )
  tight_expected <- rbind(
    vs_expect("columnspec", 2, "G", "format"),
    vs_expect("columnspec", 2, "H", "format"),
    vs_expect("columnspec", 2, "I", "type"),
    vs_expect("columnspec", 3, "G", "format"),
    vs_expect("columnspec", 3, "I", "format")
  )
  for (engine in names(vs_engines)) {
    ds <- vs_check(tight, tight_specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("a dropped leading zero fits, and a fraction under Int is not also a format violation (%s engine)", engine),
      tight_expected, vs_found(ds)
    )
  }
})

test_that("OQ-VAL-043 | a date, time or datetime that is not display text of its declared format is reported | REQ-VAL-031", {
  dir <- qa_tempdir()
  specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "D", type = "SAS Date", format = "SAS DATE9.", nullable = TRUE),
    DTAColumnSpec(id = "T", type = "SAS Time", format = "SAS TIME8.", nullable = TRUE),
    DTAColumnSpec(id = "T3", type = "SAS Time", format = "SAS TIME12.3", nullable = TRUE),
    DTAColumnSpec(id = "T5", type = "SAS Time", format = "SAS TIME5.", nullable = TRUE),
    DTAColumnSpec(id = "DT", type = "SAS DateTime", format = "SAS DATETIME19.", nullable = TRUE)
  ))
  # Row 1 fits everywhere. Row 2 is wrong everywhere: ISO instead of display
  # text; a fraction under TIME8. (which declares no decimals); four fraction
  # digits under .3; a time wider than TIME5.. Row 3 is missing. Row 4 is
  # lower-case months and one-digit hours, which are display text. Row 5 breaks
  # D (no such month), T (wrong separator) and DT (a space where the colon
  # goes), while T3 "12:30" and T5 "1:05" are fine.
  frame <- data.frame(
    ID = sprintf("A%03d", 1:5),
    D = c("15AUG2025", "2025-08-15", NA, "15aug2025", "15AGU2025"),
    T = c("12:30:45", "12:30:45.5", NA, "1:05", "12-30-45"),
    T3 = c("12:30:45.123", "12:30:45.1234", NA, "9:15:00.5", "12:30"),
    T5 = c("12:30", "12:30:45", NA, "9:15", "1:05"),
    DT = c("15AUG2025:12:30:45", "2025-08-15T12:30", NA, "15aug25:9:15", "15AUG2025 12:30:45"),
    stringsAsFactors = FALSE
  )
  expected <- rbind(
    vs_expect("columnspec", 2, "D", "format"),
    vs_expect("columnspec", 2, "DT", "format"),
    vs_expect("columnspec", 2, "T", "format"),
    vs_expect("columnspec", 2, "T3", "format"),
    vs_expect("columnspec", 2, "T5", "format"),
    vs_expect("columnspec", 5, "D", "format"),
    vs_expect("columnspec", 5, "DT", "format"),
    vs_expect("columnspec", 5, "T", "format")
  )

  for (engine in names(vs_engines)) {
    ds <- vs_check(frame, specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("exactly the rows that are not display text of their format are reported (%s engine)", engine),
      expected, vs_found(ds)
    )
  }

  # DATE11. is written with hyphens, the day runs from 01 to 31, surrounding
  # spaces are not part of the text, and hours of a TIME value may have any
  # number of digits.
  #   D11 = DATE11.: "15-AUG-2025" (11) and "15AUG2025" fit | day 32 and day 00 do not
  #   DP  = DATE9.:  " 15AUG2025" and "15AUG2025 " are 9 characters once the
  #                  spaces go; "99FEB2025" has no such day; "15AUG2025X" is not a date
  #   TH  = TIME10.: "100:00:00" (9) and " 9:05:00" fit | "1000:00:00" is exactly
  #                  10 and fits | "12345678:00:00" is 14 characters
  more_specs <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "D11", type = "SAS Date", format = "SAS DATE11.", nullable = FALSE),
    DTAColumnSpec(id = "DP", type = "SAS Date", format = "SAS DATE9.", nullable = FALSE),
    DTAColumnSpec(id = "TH", type = "SAS Time", format = "SAS TIME10.", nullable = FALSE)
  ))
  more <- data.frame(
    ID = sprintf("A%03d", 1:4),
    D11 = c("15-AUG-2025", "15AUG2025", "32-JAN-2025", "00JAN2025"),
    DP = c(" 15AUG2025", "15AUG2025 ", "99FEB2025", "15AUG2025X"),
    TH = c("100:00:00", " 9:05:00", "12345678:00:00", "1000:00:00"),
    stringsAsFactors = FALSE
  )
  more_expected <- rbind(
    vs_expect("columnspec", 3, "D11", "format"),
    vs_expect("columnspec", 3, "DP", "format"),
    vs_expect("columnspec", 3, "TH", "format"),
    vs_expect("columnspec", 4, "D11", "format"),
    vs_expect("columnspec", 4, "DP", "format")
  )
  for (engine in names(vs_engines)) {
    ds <- vs_check(more, more_specs, dir, stream = vs_engines[[engine]])
    qa_step(
      sprintf("hyphenated dates, padded text and long hours fit; day 00, 32 and 99 do not (%s engine)", engine),
      more_expected, vs_found(ds)
    )
  }
})

test_that("OQ-VAL-044 | BESTw. constrains nothing, and the format check reports its own outcome | REQ-VAL-031 REQ-VAL-013", {
  dir <- qa_tempdir()
  best <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "N", type = "SAS Num", format = "SAS BEST12.", nullable = FALSE)
  ))
  # Fifteen digits under a width of twelve, and a long fraction: BEST adapts its
  # width to the value, so neither is a violation.
  wide <- data.frame(
    ID = c("A001", "A002"), N = c("123456789012345", "0.1234567890123"),
    stringsAsFactors = FALSE
  )
  ds <- vs_check(wide, best, dir)
  qa_step("BEST12. reports no violation, however wide the value", 0L, nrow(vs_found(ds)))
  checks <- validation_errors(ds, names(tables(ds))[[1]])$columnspec_checks
  qa_step(
    "and the format check, which nothing declared, is not applicable",
    "not_applicable", as.character(checks$status[checks$keyword == "format"])
  )
  qa_step(
    "while the type check beside it, which the column does declare, passed",
    "passed", as.character(checks$status[checks$keyword == "type"])
  )

  # A column that does declare a constraining format moves the row to passed,
  # and a violation moves it to failed -- under the same label as the type row.
  fixed <- vs_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "W", type = "SAS Char", format = "SAS $5.", nullable = FALSE)
  ))
  fits <- vs_check(data.frame(ID = c("A001", "A002"), W = c("ab", "abcde"), stringsAsFactors = FALSE), fixed, dir)
  fits_checks <- validation_errors(fits, names(tables(fits))[[1]])$columnspec_checks
  qa_step(
    "a declared format that every value fits is reported as passed",
    "passed", as.character(fits_checks$status[fits_checks$keyword == "format"])
  )

  broken <- vs_check(data.frame(ID = c("A001", "A002"), W = c("ab", "abcdef"), stringsAsFactors = FALSE), fixed, dir)
  broken_checks <- validation_errors(broken, names(tables(broken))[[1]])$columnspec_checks
  qa_step(
    "one value too long fails the format check, once, in one column",
    list(status = "failed", n_errors = 1L, columns_failed = 1L, failed = "W"),
    list(
      status = as.character(broken_checks$status[broken_checks$keyword == "format"]),
      n_errors = as.integer(broken_checks$n_errors[broken_checks$keyword == "format"]),
      columns_failed = as.integer(broken_checks$columns_failed[broken_checks$keyword == "format"]),
      failed = as.character(broken_checks$failed_columns[broken_checks$keyword == "format"])
    )
  )
  qa_step(
    "and the type check beside it still passed",
    "passed", as.character(broken_checks$status[broken_checks$keyword == "type"])
  )
  qa_step(
    "both rows sit under the one label format",
    c("format", "format"),
    as.character(broken_checks$check[broken_checks$keyword %in% c("type", "format")])
  )
})
