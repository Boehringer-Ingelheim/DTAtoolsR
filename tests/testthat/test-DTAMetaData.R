test_that("DTAMetaData creation", {
  x <- DTAMetaData(version = "0.01", author = "Bla")

  expect_s3_class(x, class = "DTAtools::DTAMetaData")
})
