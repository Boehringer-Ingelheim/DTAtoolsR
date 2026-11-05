# columns_specs_from_word() had no test of its own. These round-trip it through
# export_specs_table() and read a hand-built, older-style document.

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
