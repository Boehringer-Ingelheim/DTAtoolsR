# ---------------------------------------------------------------------------
# DTAFileAny -- extensions handling, regression tests for the code review
# fixes (section P6/P7)
# ---------------------------------------------------------------------------

test_that("a multi-part ending like tar.gz matches the whole suffix, not just the last segment", {
  # tools::file_ext() returns only the last dot-separated segment ("gz"), so
  # comparing against that alone could never match a multi-part declaration
  # like "tar.gz". dta_file_extension_allowed() now does a suffix match instead.
  h <- DTAFileAny(filename = "arch.tar.gz", extensions = "tar.gz")

  expect_true(matches_filename(h, "arch.tar.gz"))
})

test_that("a multi-part ending like nii.gz matches the whole suffix", {
  h <- DTAFileAny(filename = "s.nii.gz", extensions = "nii.gz")

  expect_true(matches_filename(h, "s.nii.gz"))
})

test_that("a compressed delivery still satisfies the ending underneath", {
  # report.pdf.gz carries the pdf ending underneath the .gz wrapper.
  h <- DTAFileAny(filename = "report.pdf", extensions = "pdf")

  expect_true(matches_filename(h, "report.pdf.gz"))
})

test_that("a wrong ending is refused", {
  h <- DTAFileAny(filename = "report.csv", extensions = c("pdf", "zip"))

  expect_false(matches_filename(h, "report.csv"))
})

test_that("a YAML boolean or number for extensions aborts, naming the value to quote", {
  # `extensions: no` parses to the logical FALSE, not the string "no" -- the
  # classic YAML boolean trap. as.character() used to coerce it silently into
  # the unmatchable string "FALSE"; it now aborts and tells the author to
  # quote the value instead.
  expect_error(
    dta_file_handlers_from_list(list(type = "any", filename = "a.pdf", extensions = FALSE)),
    "quote it"
  )
})

test_that("direct assignment of an un-normalised extensions value is rejected", {
  # dta_normalise_extensions() only runs in the constructor, so a direct
  # property assignment used to sail past it with a value matches_filename()
  # could never match (the comparison is always against a bare, lower-case
  # ending with no leading dot) -- silently making the handler match nothing.
  h <- DTAFileAny(filename = "report.pdf")

  expect_error(
    {
      h@extensions <- ".PDF"
    },
    "must already be normalised"
  )
})

test_that("matches_filename(DTAFileAny) still returns one logical per declared name for a multi-name handler", {
  # The extensions restriction is ANDed onto the base name/pattern result with
  # `&`, not collapsed with a guard clause that would return a bare scalar and
  # flatten a multi-name handler to a single verdict.
  h <- DTAFileAny(filename = c("a.pdf", "b.pdf"), pattern = TRUE, extensions = "pdf")
  result <- matches_filename(h, "a.pdf")

  expect_length(result, 2)
  expect_true(result[[1]])
  expect_false(result[[2]])
})

# ---------------------------------------------------------------------------
# DTAFileAny prints as itself
# ---------------------------------------------------------------------------

test_that("print() shows the DTAFileAny class and its allowed endings", {
  out <- cli::cli_fmt(print(DTAFileAny(filename = "study_report.pdf", extensions = "pdf")))

  expect_true(any(grepl("DTAFileAny", out, fixed = TRUE)))
  expect_true(any(grepl("Allowed endings: pdf", out, fixed = TRUE)))
})

test_that("print() says 'any' for allowed endings when extensions is NULL", {
  out <- cli::cli_fmt(print(DTAFileAny(filename = "study_report.pdf")))

  expect_true(any(grepl("Allowed endings: any", out, fixed = TRUE)))
})
