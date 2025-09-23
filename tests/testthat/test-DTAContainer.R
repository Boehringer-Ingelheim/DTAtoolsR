test_that("DTAContainer object is created and tables are accessible", {
  specs <- import_specs_from_yaml(system.file("extdata", "params_gf.yaml", package = "DTAtools"))
  path <- system.file("extdata", "data_gf_small.tsv", package = "DTAtools")
  file_info <- DTAFileInfoTSV("data_gf_small.tsv")
  container <- DTAContainer(specs = specs, fileinfo = file_info)

  expect_equal(max_number_of_files(container), 1)
  expect_equal(min_number_of_files(container), 1)

  expect_s3_class(container, "DTAtools::DTAContainer")

  container2 <- DTAContainer(specs = specs, fileinfo =
                              list(file_info, file_info))
  expect_equal(max_number_of_files(container2), 2)
  expect_equal(min_number_of_files(container2), 2)

  expect_equal(specs(container), specs)




  colspec(container, 1)


  expect_equal(data(container), df)
  expect_equal(data(container, 1), df)
  expect_equal(data(container, "test"), df)
  expect_equal(data(container, "test")$STUDYID[1], "1234")

  # check metadata method
  expect_equal(metadata(container), list())

})
