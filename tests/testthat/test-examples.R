test_that("Examples are loaded and correctly formatted.", {
  validate_table <- function(data_filename, data_sep, params_filename) {
    table <- read.table(data_filename, sep = data_sep, header = TRUE)

    column_collection <- import_specs_from_yaml(
      params_filename
    )

    expect_no_error(DTAContainer(
      specs = column_collection,
      data = list("DTA" = table)
    ))
  }
  validate_table(
    system.file("extdata", "gf_data_small.tsv", package = "DTAtools"),
    "\t",
    system.file("extdata", "gf_container.yaml", package = "DTAtools")
  )
  validate_table(
    system.file("extdata", "clinical_data.csv", package = "DTAtools"),
    ",",
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
})
