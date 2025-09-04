test_that("DTAContainer object is created and tables are accessible", {
  spec <- DTAColumnSpec(
    id = "STUDYID",
    label = "Study ID",
    type = "Char",
    length = 8,
    nullable = FALSE,
    description = "ID"
  )
  collection <- DTAColumnSpecCollection(columns = list(STUDYID = spec))
  df <- data.frame(STUDYID = c("1234", "5678"))

  data_obj <- DTAContainer(specs = collection, data = list(test = df))

  expect_s3_class(data_obj, "DTAContainer")
  expect_equal(getData(data_obj), df)
  expect_equal(getData(data_obj, 1), df)
  expect_equal(getData(data_obj, "test"), df)
  expect_equal(getData(data_obj, "test")$STUDYID[1], "1234")

  # check getMetadata method
  expect_equal(getMetadata(data_obj), list())
})
