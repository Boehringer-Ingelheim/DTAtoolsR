test_that("Examples are loaded and correctly formatted.", {
  validate_table <- function(data_filename, data_sep, params_filename) {
    table <- read.table(data_filename, sep = data_sep, header = TRUE)

    column_collection <- importDTAColumnSpecCollectionFromYaml(
      params_filename
    )

    expect_no_error(DTAContainer(
      specs = column_collection,
      data = list("DTA" = table)
    ))
  }
  validate_table(
    system.file("extdata", "data_gf_small.tsv", package = "DTAtools"),
    "\t",
    system.file("extdata", "params_gf.yaml", package = "DTAtools")
  )
  validate_table(
    system.file("extdata", "data_spec.csv", package = "DTAtools"),
    ",",
    system.file("extdata", "params_spec.yaml", package = "DTAtools")
  )
})
