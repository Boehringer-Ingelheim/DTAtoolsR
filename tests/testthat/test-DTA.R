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

  # Create DTAContainer
  container_obj <- DTAContainer(collection, tables)

  # Create DTA object
  dta_obj <- DTA(container = list(main = container_obj), author = "Test Author")

  # Check class
  expect_equal(class(dta_obj), c("DTAtools::DTA", "S7_object"))

  # Check metadata
  meta <- getMetadata(dta_obj)

  expect_equal(class(meta), c("DTAtools::DTAMetaData", "S7_object"))

  expect_equal(meta@author, "Test Author")

  # Check container retrieval
  all_containers <- getContainer(dta_obj)
  expect_type(all_containers, "list")
  expect_named(all_containers, "main")

  # Retrieve by name
  main_container <- getContainer(dta_obj, "main")
  expect_s3_class(main_container, "DTAtools::DTAContainer")

  # Retrieve by vector
  expect_equal(getContainer(dta_obj, c("main")), all_containers[["main"]])

  # Error on missing container
  expect_error(getContainer(dta_obj, "missing"), "not found")
})
