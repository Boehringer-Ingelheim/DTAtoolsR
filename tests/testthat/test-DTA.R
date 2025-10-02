test_that("DTA object is constructed correctly", {
  # Create dummy data
  table1 <- data.frame(STUDYID = c("1234", "1234"), VISIT = c("V01", "V02"))
  table2 <- data.frame(STUDYID = c("1234", "1234"), VISIT = c("EOT", "V03"))
  tables <- list(table1 = table1, table2 = table2)

  # Dummy DTAColumnSpecCollection (replace with actual constructor if needed)
  col1 <- DTAColumnSpec(id = "STUDYID", type = "Char", nullable = TRUE)
  col2 <- DTAColumnSpec(id = "VISIT", type = "Char", nullable = FALSE)
  collection <- DTAColumnSpecCollection(
    columns = list(STUDYID = col1, VISIT = col2)
  )

  # Create DTADataSet
  container_obj <- DTADataSet(collection, tables)

  # Create DTA object
  dta_obj <- DTA(container = list(main = container_obj), author = "Test Author")

  # Check class
  expect_equal(class(dta_obj), c("DTAtools::DTA", "S7_object"))

  # Check metadata
  meta <- get_metadata(dta_obj)

  expect_equal(class(meta), c("DTAtools::DTAMetaData", "S7_object"))

  expect_equal(meta@author, "Test Author")

  # Check container retrieval
  all_containers <- container(dta_obj)
  expect_type(all_containers, "list")
  expect_named(all_containers, "main")

  # Retrieve by name
  main_container <- container(dta_obj, "main")
  expect_s3_class(main_container, "DTAtools::DTADataSet")

  # Retrieve by vector
  expect_equal(container(dta_obj, c("main")), all_containers[["main"]])

  # Error on missing container
  expect_error(container(dta_obj, "missing"), "not found")
})
