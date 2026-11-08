# columns_specs_from_word(): reading a specification table back out of Word.

# Write `fixture` (a data frame, one row per column spec) as the only table of a
# fresh .docx and return its path. Column names become the header row.
wd_docx <- function(fixture) {
  path <- withr::local_tempfile(fileext = ".docx", .local_envir = parent.frame())
  doc <- flextable::body_add_flextable(officer::read_docx(), flextable::flextable(fixture))
  print(doc, target = path)
  path
}

wd_fixture <- function(...) {
  base <- data.frame(
    id = "ARM", label = "Arm", type = "SAS Char", length = "8",
    format = "SAS $8.", nullable = "No", description = "Treatment arm",
    stringsAsFactors = FALSE
  )
  over <- list(...)
  for (nm in names(over)) base[[nm]] <- over[[nm]]
  base
}

# The facts a spec carries, in a form expect_identical() can compare.
wd_facts <- function(spec) {
  s <- spec@structure
  list(
    id = spec@id,
    label = spec@label,
    backend = if (is.null(s)) NULL else s@backend,
    type = if (is.null(s)) NULL else s@type,
    format = if (is.null(s)) NULL else s@format,
    length = if (is.null(s)) NULL else s@length,
    nullable = spec@nullable,
    values = spec@values,
    pattern = spec@pattern
  )
}

test_that("a pattern containing the letter n is read whole", {
  # `[^\\n]` excluded the letter n, so this came back as "^(Scree".
  path <- wd_docx(wd_fixture(
    id = "VISIT",
    description = "Visit name\n#@pattern: ^(Screening|Baseline|Week [0-9]+)$"
  ))
  back <- columns_specs_from_word(path)
  expect_identical(back@columns$VISIT@pattern, "^(Screening|Baseline|Week [0-9]+)$")
  expect_identical(back@columns$VISIT@description, "Visit name")
})

test_that("a pattern stops at the end of its line, not at the end of the cell", {
  path <- wd_docx(wd_fixture(
    description = "Arm\n#@pattern: ^[A-Z]+$\nsecond line of text"
  ))
  back <- columns_specs_from_word(path)
  expect_identical(back@columns$ARM@pattern, "^[A-Z]+$")
})

test_that("#@values is split on the separator and unquoted", {
  path <- wd_docx(wd_fixture(description = "Arm\n#@values: \"Placebo\"; 'Active'; Open lab"))
  back <- columns_specs_from_word(path)
  expect_identical(
    unlist(back@columns$ARM@values, use.names = FALSE),
    c("Placebo", "Active", "Open lab")
  )
  expect_null(back@columns$ARM@pattern)

  path2 <- wd_docx(wd_fixture(description = "#@values: A|B"))
  back2 <- columns_specs_from_word(path2, value_sep = "|")
  expect_identical(unlist(back2@columns$ARM@values, use.names = FALSE), c("A", "B"))
  expect_null(back2@columns$ARM@description)
})

test_that("nullable accepts yes, y and true in any case, and leaves a blank cell unset", {
  path <- wd_docx(data.frame(
    id = c("A", "B", "C", "D", "E"),
    label = "x", type = "SAS Char", length = "1", format = "SAS $1.",
    nullable = c("YES", "y", "Yes (if consented)", "No", ""),
    description = "d",
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  expect_identical(
    lapply(back@columns[c("A", "B", "C", "D")], function(s) s@nullable),
    list(A = TRUE, B = TRUE, C = TRUE, D = FALSE)
  )
  expect_null(back@columns$E@nullable)
})

test_that("blank rows are dropped and whitespace is removed from ids", {
  path <- wd_docx(data.frame(
    id = c("SUB JID", "", "AGE"),
    label = c("Subject", "", "Age"), type = c("SAS Char", "", "SAS Num"),
    length = c("10", "", "8"), format = c("SAS $10.", "", "SAS 8."),
    nullable = c("No", "", "Yes"), description = c("a", "", "b"),
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  expect_identical(names(back@columns), c("SUBJID", "AGE"))
  expect_identical(back@columns$SUBJID@id, "SUBJID")
})

test_that("a table narrower or wider than colnames is an error, not a misaligned read", {
  path <- wd_docx(wd_fixture()[, c("id", "label", "type", "nullable", "description")])
  expect_error(columns_specs_from_word(path), class = "dta_word_import_error")
  back <- columns_specs_from_word(
    path,
    colnames = c("id", "label", "type", "nullable", "description")
  )
  expect_identical(back@columns$ARM@structure@type, "Char")
  expect_null(back@columns$ARM@structure@length)
})

test_that("a Length cell that is not a number is an error, not a silent NA", {
  path <- wd_docx(wd_fixture(length = "eight"))
  expect_error(columns_specs_from_word(path), class = "dta_word_import_error")
  expect_error(columns_specs_from_word(path), "eight", fixed = TRUE)
})

test_that("colnames must name the id column", {
  path <- wd_docx(wd_fixture())
  expect_error(
    columns_specs_from_word(
      path,
      colnames = c("x", "label", "type", "length", "format", "nullable", "description")
    ),
    class = "dta_word_import_error"
  )
})

test_that("a table_position that is not a table in the document is an error", {
  path <- wd_docx(wd_fixture())
  for (bad in list(0, 2, 1.5, NA_real_, c(1, 2), "1")) {
    expect_error(
      columns_specs_from_word(path, table_position = bad),
      class = "dta_word_import_error",
      info = deparse(bad)
    )
  }
})

test_that("the table export_specs_table() writes reads back to the same specification", {
  specs <- DTAColumnSpecCollection(columns = list(
    STUDYID = DTAColumnSpec(
      id = "STUDYID", label = "Study Identifier", type = "SAS Char",
      length = 10, nullable = FALSE
    ),
    ARM = DTAColumnSpec(
      id = "ARM", label = "Arm", type = "SAS Char", length = 8,
      nullable = TRUE, values = c("Placebo", "Active")
    ),
    VISIT = DTAColumnSpec(
      id = "VISIT", label = "Visit", type = "SAS Char", format = "SAS $20.",
      length = 20, nullable = TRUE, pattern = "^(Screening|Baseline|Week [0-9]+)$",
      description = "Planned visit"
    ),
    AGE = DTAColumnSpec(id = "AGE", label = "Age", type = "SAS Num", nullable = TRUE)
  ))

  for (layout in list(
    c("Variable Name", "Variable Label", "Type", "Length", "Format", "Nullable", "Description"),
    c("Variable Name", "Variable Label", "Type", "Format", "Nullable", "Description")
  )) {
    docx <- withr::local_tempfile(fileext = ".docx")
    export_specs_table(specs, file = docx, colnames = layout, quiet = TRUE)
    back <- columns_specs_from_word(docx)

    expect_identical(names(back@columns), names(specs@columns))
    for (id in names(specs@columns)) {
      want <- wd_facts(specs@columns[[id]])
      got <- wd_facts(back@columns[[id]])
      if (!"Length" %in% layout) {
        want$length <- NULL
        got$length <- NULL
      }
      expect_identical(got, want, info = paste(id, length(layout), "columns"))
    }
  }
})


word_round_trip_collection <- function() {
  DTAColumnSpecCollection(columns = list(
    TXT = DTAColumnSpec(id = "TXT", label = "Text", type = "SAS Char", length = 10, nullable = FALSE),
    CNT = DTAColumnSpec(id = "CNT", label = "Count", type = "SAS Int", format = "SAS 8.", nullable = TRUE),
    AMT = DTAColumnSpec(id = "AMT", label = "Amount", type = "SAS Num", format = "SAS BEST12.", nullable = TRUE)
  ))
}

test_that("columns_specs_from_word() reads back what export_specs_table() wrote", {
  original <- word_round_trip_collection()
  f <- tempfile(fileext = ".docx")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  export_specs_table(original, file = f, quiet = TRUE)

  back <- columns_specs_from_word(f)

  expect_identical(names(back@columns), c("TXT", "CNT", "AMT"))
  expect_identical(
    vapply(back@columns, function(x) x@structure@type, character(1)),
    c(TXT = "Char", CNT = "Int", AMT = "Num")
  )
  expect_null(back@columns$TXT@structure@format)
  expect_identical(back@columns$CNT@structure@format, "8.")
  expect_identical(back@columns$AMT@structure@format, "BEST12.")
  expect_equal(back@columns$TXT@structure@length, 10)
  expect_null(back@columns$CNT@structure@length)
  expect_identical(back@columns$TXT@nullable, FALSE)
  expect_identical(back@columns$CNT@nullable, TRUE)
})

test_that("columns_specs_from_word() takes the backend of an unprefixed Format from the Type cell", {
  fixture <- data.frame(
    id = c("A", "B"),
    label = c("a", "b"),
    type = c("SAS Char", "SAS Num"),
    length = c("10", ""),
    format = c("$10.", ""),
    nullable = c("No", "Yes"),
    description = c("d1", "d2"),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  doc <- flextable::body_add_flextable(officer::read_docx(), flextable::flextable(fixture))
  print(doc, target = path)

  back <- columns_specs_from_word(path)

  expect_identical(back@columns$A@structure@format, "$10.")
  expect_identical(back@columns$A@structure@backend, "SAS")
  expect_null(back@columns$B@structure@format)
})
