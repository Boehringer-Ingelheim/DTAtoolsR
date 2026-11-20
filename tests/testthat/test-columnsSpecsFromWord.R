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

# One row per id, every other cell the same.
wd_id_rows <- function(ids) {
  rows <- wd_fixture()[rep(1L, length(ids)), , drop = FALSE]
  rows$id <- ids
  rownames(rows) <- NULL
  rows
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

test_that("blank rows are dropped and the spaces of an id are kept as typed", {
  path <- wd_docx(data.frame(
    id = c("SUB JID", "", "AGE"),
    label = c("Subject", "", "Age"), type = c("SAS Char", "", "SAS Num"),
    length = c("10", "", "8"), format = c("SAS $10.", "", "SAS 8."),
    nullable = c("No", "", "Yes"), description = c("a", "", "b"),
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  expect_identical(names(back@columns), c("SUB JID", "AGE"))
  expect_identical(back@columns[["SUB JID"]]@id, "SUB JID")
})

test_that("a Word id keeps interior spaces, including a double space, and punctuation", {
  ids <- c("Subject  ID", "Visit Date (UTC)", "Dose mg/kg", "Wt. (kg)")
  path <- wd_docx(wd_id_rows(ids))
  back <- columns_specs_from_word(path)
  expect_identical(names(back@columns), ids)
  expect_identical(unname(vapply(back@columns, function(s) s@id, "")), ids)
})

test_that("the ends of a Word id are trimmed", {
  path <- wd_docx(wd_id_rows(c("  Padded ID  ", "Tail ID ")))
  back <- columns_specs_from_word(path)
  expect_identical(names(back@columns), c("Padded ID", "Tail ID"))
})

test_that("a line break or tab inside an id cell is deleted, not turned into a space", {
  path <- wd_docx(wd_id_rows(c("SUB\nJID", "CONSENT_\nDATE", "TAB\tID")))
  back <- columns_specs_from_word(path)
  expect_identical(names(back@columns), c("SUBJID", "CONSENT_DATE", "TABID"))
})

test_that("Unicode whitespace inside or at the edge of a Word id is removed, and the import does not abort", {
  # Written as code points so the file stays ASCII: thin space, ideographic
  # space, em space at the front.
  thin <- intToUtf8(0x2009)
  ideographic <- intToUtf8(0x3000)
  em <- intToUtf8(0x2003)
  path <- wd_docx(wd_id_rows(c(
    paste0("STUDY", thin, "ID"),
    paste0("SUBJ", ideographic, "ID"),
    paste0(em, "VISITID"),
    paste0("ARM", thin, "CODE", em)
  )))

  back <- NULL
  expect_no_error(back <- columns_specs_from_word(path))

  expect_identical(names(back@columns), c("STUDYID", "SUBJID", "VISITID", "ARMCODE"))
  expect_identical(unname(vapply(back@columns, function(s) s@id, "")), names(back@columns))
})

test_that("a plain space is kept while the other whitespace in the same id is removed", {
  thin <- intToUtf8(0x2009)
  path <- wd_docx(wd_id_rows(c(
    "STUDY ID",
    paste0("STUDY ID", thin, "CODE"),
    paste0(" ", intToUtf8(0x2003), "Padded ID  ")
  )))

  back <- columns_specs_from_word(path)

  expect_identical(names(back@columns), c("STUDY ID", "STUDY IDCODE", "Padded ID"))
})

test_that("a vertical tab or form feed inside an id is removed, not kept and not an abort", {
  # Word's XML cannot hold these two characters, so no .docx can carry them; an
  # extractor can still hand them back, so the id cleaning is exercised with the
  # table the reader would return.
  table <- wd_id_rows(c("STUDY\vID", "SUBJ\fID", "VISIT \vID"))
  path <- wd_docx(wd_id_rows(c("A", "B", "C")))
  testthat::local_mocked_bindings(
    docx_extract_all_tbls = function(...) list(table),
    .package = "docxtractr"
  )

  back <- NULL
  expect_no_error(back <- columns_specs_from_word(path))

  expect_identical(names(back@columns), c("STUDYID", "SUBJID", "VISIT ID"))
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

test_that("a spaced column id survives export_specs_table() and columns_specs_from_word()", {
  ids <- c("Subject ID", "Visit Date (UTC)", "Dose mg/kg")
  original <- DTAColumnSpecCollection(columns = stats::setNames(
    lapply(ids, function(id) DTAColumnSpec(id = id, label = "x", type = "SAS Char", length = 10, nullable = FALSE)),
    ids
  ))
  f <- tempfile(fileext = ".docx")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  export_specs_table(original, file = f, quiet = TRUE)

  back <- columns_specs_from_word(f)

  expect_identical(names(back@columns), ids)
  expect_identical(unname(vapply(back@columns, function(s) s@id, "")), ids)
})

# ---- the R vocabulary ------------------------------------------------------------

test_that("an R table is read: the type cell carries the backend, a prefixed format is kept", {
  path <- wd_docx(data.frame(
    id = c("NAME", "AGE", "COUNT", "DAY", "STAMP", "TOD"),
    label = c("n", "a", "c", "d", "s", "t"),
    type = c("R character", "R numeric", "R integer", "R Date", "R POSIXct", "R hms"),
    length = c("12", "", "", "", "", ""),
    format = c("", "", "", "R %Y-%m-%d", "R %Y-%m-%dT%H:%M:%SZ", "R %H:%M"),
    nullable = c("No", "Yes", "No", "Yes", "Yes", "Yes"),
    description = "d",
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  facts <- function(id) wd_facts(back@columns[[id]])
  expect_identical(facts("NAME")[c("backend", "type", "format", "length")], list(
    backend = "R", type = "character", format = NULL, length = 12
  ))
  expect_identical(facts("AGE")[c("backend", "type", "format")], list(
    backend = "R", type = "numeric", format = NULL
  ))
  expect_identical(facts("COUNT")$type, "integer")
  expect_identical(facts("DAY")[c("backend", "type", "format")], list(
    backend = "R", type = "Date", format = "%Y-%m-%d"
  ))
  expect_identical(facts("STAMP")$format, "%Y-%m-%dT%H:%M:%SZ")
  expect_identical(facts("TOD")[c("type", "format")], list(type = "hms", format = "%H:%M"))
})

test_that("an R format cell without its prefix takes the backend of the Type cell", {
  path <- wd_docx(data.frame(
    id = c("DAY", "STAMP", "OLD"),
    label = c("d", "s", "o"),
    type = c("R Date", "R POSIXct", "SAS Char"),
    length = c("", "", "6"),
    format = c("%d.%m.%Y", "%Y-%m-%d %H:%M", "$6."),
    nullable = "Yes", description = "x",
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  expect_identical(wd_facts(back@columns$DAY)[c("backend", "type", "format")], list(
    backend = "R", type = "Date", format = "%d.%m.%Y"
  ))
  expect_identical(wd_facts(back@columns$STAMP)[c("backend", "type", "format")], list(
    backend = "R", type = "POSIXct", format = "%Y-%m-%d %H:%M"
  ))
  # the SAS row beside them is restored as before
  expect_identical(wd_facts(back@columns$OLD)[c("backend", "type", "format")], list(
    backend = "SAS", type = "Char", format = "$6."
  ))
})

test_that("a blank format cell on an R date, time or timestamp stores the default format", {
  path <- wd_docx(data.frame(
    id = c("DAY", "STAMP", "TOD"),
    label = "x",
    type = c("R Date", "R POSIXct", "R hms"),
    length = "", format = "", nullable = "Yes", description = "x",
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  expect_identical(back@columns$DAY@structure@format, "%Y-%m-%d")
  expect_identical(back@columns$STAMP@structure@format, "%Y-%m-%d %H:%M:%S")
  expect_identical(back@columns$TOD@structure@format, "%H:%M:%S")
})

test_that("an R type is read case-insensitively and a type that is not an R type is refused", {
  path <- wd_docx(data.frame(
    id = c("A", "B"), label = "x", type = c("R DOUBLE", "R INTEGER"),
    length = "", format = "", nullable = "Yes", description = "x",
    stringsAsFactors = FALSE
  ))
  back <- columns_specs_from_word(path)
  expect_identical(back@columns$A@structure@type, "numeric")
  expect_identical(back@columns$B@structure@type, "integer")

  bad <- wd_docx(wd_fixture(type = "R complex", format = "", length = ""))
  expect_error(columns_specs_from_word(bad), regexp = "must\\s+be\\s+one\\s+of")
})

test_that("a Word table written by export_specs_table() reads back as the same R specs", {
  columns <- list(
    NAME = DTAColumnSpec(id = "NAME", label = "Name", type = "R character", length = 12, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", label = "Age", type = "R numeric", nullable = TRUE),
    COUNT = DTAColumnSpec(id = "COUNT", label = "Count", type = "R integer", nullable = FALSE),
    FLAG = DTAColumnSpec(id = "FLAG", label = "Flag", type = "R logical", nullable = TRUE, values = c("TRUE", "FALSE")),
    DAY = DTAColumnSpec(id = "DAY", label = "Day", type = "R Date", format = "R %d.%m.%Y", nullable = TRUE),
    STAMP = DTAColumnSpec(id = "STAMP", label = "Stamp", type = "R POSIXct", nullable = TRUE),
    TOD = DTAColumnSpec(id = "TOD", label = "Time", type = "R hms", format = "R %H:%M", nullable = TRUE)
  )
  original <- DTAColumnSpecCollection(columns = columns)
  f <- tempfile(fileext = ".docx")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  export_specs_table(original, file = f, quiet = TRUE)

  back <- columns_specs_from_word(f)

  expect_identical(names(back@columns), names(columns))
  for (id in names(columns)) {
    expect_identical(wd_facts(back@columns[[id]]), wd_facts(columns[[id]]), info = id)
  }
  # the default format an R timestamp was given is written, so it is read back
  expect_identical(back@columns$STAMP@structure@format, "%Y-%m-%d %H:%M:%S")
})

test_that("permitted values on an R character column are warned about when YAML read them as numbers", {
  expect_warning(
    specs_from_list(list(list(id = "A", type = "R character", values = c(1.1, 2))), NULL),
    regexp = "read\\s+as\\s+numbers"
  )
  # an R numeric, integer or logical column holds no text: nothing to warn about
  expect_no_warning(specs_from_list(list(list(id = "A", type = "R numeric", values = c(1.1, 2))), NULL))
  expect_no_warning(specs_from_list(list(list(id = "A", type = "R integer", values = c(1, 2))), NULL))
  expect_no_warning(specs_from_list(list(list(id = "A", type = "R logical", values = c("TRUE", "FALSE"))), NULL))
})
