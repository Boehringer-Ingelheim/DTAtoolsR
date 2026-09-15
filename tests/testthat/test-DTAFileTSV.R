# create_example_DTAFileTSV() had no test. It has exactly one working path
# (index = 1, wrapping the bundled example TSV) and one abort for anything
# else; both are pinned here.

test_that("create_example_DTAFileTSV() wraps the bundled example file", {
  handler <- create_example_DTAFileTSV()
  expect_true(inherits(handler, "DTAtools::DTAFileTSV"))
  # basename() of the same extdata path the function itself resolves --
  # confirms the handler was actually built from the bundled file, not a
  # hardcoded guess at its name.
  expect_identical(
    handler@filename,
    basename(system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools"))
  )
})

test_that("create_example_DTAFileTSV() only supports index = 1", {
  expect_error(
    create_example_DTAFileTSV(2),
    regexp = "Only index = 1 is supported for create_example_DTAFileTSV().",
    fixed = TRUE
  )
})
