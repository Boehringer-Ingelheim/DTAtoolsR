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

  specs_converted <- specs_from_list(specs_list$columns, specs_list$rules)
  expect_s3_class(specs_converted, "DTAtools::DTAColumnSpecCollection")

  expect_equal(specs_converted, specs)
})


test_that("DTAColumnSpecCollection stores and retrieves specs", {
  col1 <- create_example_DTAColumnSpec(1)
  col2 <- create_example_DTAColumnSpec(2)

  specs <- DTAColumnSpecCollection(
    columns = list(STUDYID = col1, VISIT = col2)
  )

  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
  expect_named(specs, c("STUDYID", "VISIT"))

  expect_equal(specs@columns[[1]], col1)
  expect_equal(specs@columns[[2]], col2)
})

