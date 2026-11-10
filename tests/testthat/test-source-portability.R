# `R CMD check --as-cran` raises "checking code files for non-ASCII characters
# ... WARNING" for any non-ASCII byte outside a comment, and the R-CMD-check
# workflow runs with `error-on: "warning"`, so a single stray em dash in a
# string literal turns the whole matrix red. That feedback arrives minutes
# later and only on CI; this test moves it into the local suite.

testthat::test_that("R sources use only ASCII outside comments", {
  files <- source_r_files("[.][RrSsq]$")
  skip_if_no_source_tree("package source not available")

  # Comments are exempt (R CMD check tolerates them), so the file is parsed and
  # only non-COMMENT tokens are inspected. Working on tokens rather than raw
  # lines also keeps a non-ASCII character in a roxygen block from masking one
  # in the code on the same line.
  offenders <- unlist(lapply(files, function(f) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    if (is.null(pd)) {
      return(NULL)
    }
    code <- pd[pd$terminal & pd$token != "COMMENT", ]
    bad <- code$text[grepl("[^\\x01-\\x7f]", code$text, perl = TRUE)]
    if (length(bad) == 0) {
      return(NULL)
    }
    paste0(basename(f), ": ", unique(bad))
  }))

  testthat::expect_identical(offenders, NULL)
})
