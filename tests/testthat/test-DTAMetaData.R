test_that("DTAMetaData creation", {
  md <- create_example_DTAMetaData()

  expect_s3_class(md, class = "DTAtools::DTAMetaData")
})


test_that("DTAMetaData creation", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  dta <- read_dta_from_yaml(path)
  md <- metadata(dta)
  expect_s3_class(md, class = "DTAtools::DTAMetaData")
})

