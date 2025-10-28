test_that("Import specs from YAML file", {
  specs <- import_specs_from_yaml(system.file("extdata", "gf_dataset.yaml", package = "DTAtools"))

  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
  expect_named(specs,  c("STUDYID", "DOMAIN", "SUBJIDN", "GFGRPID", "GFREFID", "GFTESTCD", "GFTEST", "GFTSTDTL", 
  "GFCAT", "GFORRES", "GFORRESU", "GFORREF", "GFRESCAT", "GFGENREF", "GFSYM", "GFGENLOC", "GFGENSR", "GFSEQID", "GFPVRID", "GFSTAT", "GFREASND",
  "GFNAM", "GFSPEC", "MATRIX", "GFMETHOD", "GFANMETH", "VISIT", "GFDTC", "GFTPT", "GFTPTREF", "GFTPTNUM", "GFSIGDTL", "SWVER"))
  expect_equal(specs@columns$VISIT@values, c("VISIT 02", "VISIT 05"))
  expect_equal(class(specs@columns[[1]]), c("DTAtools::DTAColumnSpec", "S7_object"))
  expect_equal(class(specs@rules[[1]]), c("DTAtools::DTARuleColCondition", "DTAtools::DTARule", "S7_object"))
  expect_equal(class(specs@rules[[3]]), c("DTAtools::DTARuleColUnique", "DTAtools::DTARule", "S7_object"))
  expect_equal(class(specs@rules[[1]]), c("DTAtools::DTARuleColCondition", "DTAtools::DTARule", "S7_object"))
  expect_equal(class(specs@rules[[1]]), c("DTAtools::DTARuleColCondition", "DTAtools::DTARule", "S7_object"))
})

test_that("Specs to list conversion", {
  specs <- import_specs_from_yaml(system.file("extdata", "gf_dataset.yaml", package = "DTAtools"))

  specs_list <- as.list(specs)
  expect_type(specs_list, "list")
}

test_that("DTAColumnSpecCollection stores and retrieves specs", {
  col1 <- create_example_DTAColumnSpec(1)
  col2 <- create_example_DTAColumnSpec(2)

  specs <- DTAColumnSpecCollection(
    columns = list(STUDYID = col1, VISIT = col2)
  )

  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
  expect_named(specs, c("STUDYID", "VISIT"))
  expect_equal(colspec(specs, "VISIT")@label, "Visit")
})

test_that("specs_from_list constructs valid object and returns rules", {
  col1 <- create_example_DTAColumnSpec(1) #STUDYID
  col2 <- create_example_DTAColumnSpec(2)

  rule1 <- create_example_DTARuleColCondition(1)

  rules <- list(rule1)

  # Run function
  specs <- specs_from_list(
    specs = columns,
    rules = rules
  )

  print(specs)
  # Assertions
  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
  expect_named(specs@columns, c("STUDYID", "VISIT"))
  expect_equal(specs@columns$STUDYID@id, "STUDYID")
  expect_equal(specs@columns$VISIT@values, list("V01", "EOT"))
  expect_equal(class(specs@columns[[1]]), c("DTAtools::DTAColumnSpec", "S7_object"))
  expect_equal(class(specs@rules[[1]]), c("DTAtools::DTARule", "S7_object"))

  # check getMetadata method
  expect_equal(getMetadata(specs), list())

  # Test DTAColumnSpecCollectionToList

  list <- as.list.DTAColumnSpecCollection(specs)
  expect_type(list, "list")
})
