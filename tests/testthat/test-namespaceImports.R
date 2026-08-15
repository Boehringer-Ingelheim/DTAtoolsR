test_that("`%||%` is imported, not inherited from base R >= 4.4", {
  # `%||%` is used across the package (e.g. when building rule constraints
  # while reading a DTA from YAML) but only entered base R in 4.4.0, while
  # DESCRIPTION declares R (>= 4.1.0). Without an explicit import the package
  # fails on R 4.1-4.3 with `could not find function "%||%"`, so assert the
  # binding is present in the package's own imports environment rather than
  # relying on the search path.
  imports <- parent.env(asNamespace("DTAtools"))
  expect_true(exists("%||%", envir = imports, inherits = FALSE))

  op <- get("%||%", envir = imports, inherits = FALSE)
  expect_identical(op(NULL, "fallback"), "fallback")
  expect_identical(op("value", "fallback"), "value")
})
