test_that("DTAColumnSpec constructor works", {
  spec <- DTAColumnSpec(
    id = "STUDYID",
    label = "Study ID",
    type = "Char",
    format = "8",
    nullable = FALSE,
    description = "Study identifier"
  )
  expect_s3_class(spec, "DTAColumnSpec")
  expect_equal(spec@id, "STUDYID")
  expect_false(spec@nullable)
})
