# Column-specification checks. REQ-VAL-003 .. REQ-VAL-013.
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
vs_check <- function(frame, specs, dir, ...) {
  path <- file.path(dir, paste0("t", as.integer(stats::runif(1, 1, 1e9)), ".csv"))
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  ds <- DTADataSetTabular(
    name = "qual", specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE, ...)
}

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
