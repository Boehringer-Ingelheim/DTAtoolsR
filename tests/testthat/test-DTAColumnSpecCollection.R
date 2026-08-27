test_that("Import specs from YAML file", {
  specs <- import_specs_from_yaml(system.file("extdata", "gf_dataset.yaml", package = "DTAtools"))
  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
  expect_named(specs, c(
    "STUDYID", "DOMAIN", "SUBJIDN", "GFGRPID", "GFREFID", "GFTESTCD", "GFTEST", "GFTSTDTL",
    "GFCAT", "GFORRES", "GFORRESU", "GFORREF", "GFRESCAT", "GFGENREF", "GFSYM", "GFGENLOC", "GFGENSR", "GFSEQID", "GFPVRID", "GFSTAT", "GFREASND",
    "GFNAM", "GFSPEC", "MATRIX", "GFMETHOD", "GFANMETH", "VISIT", "GFDTC", "GFTPT", "GFTPTREF", "GFTPTNUM", "GFSIGDTL", "SWVER"
  ))
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

test_that("a duplicated column id is rejected rather than silently shadowed", {
  spec <- function(id, pattern) {
    DTAColumnSpec(id = id, type = "SAS Char", length = 8, pattern = pattern)
  }

  # `colspec()` looks a column up with `[[`, which returns the FIRST match, so
  # the second definition used to be invisible to every consumer that goes by
  # id -- while the column spec axis, which iterates the list, evaluated the
  # same table column against both.
  expect_error(
    DTAColumnSpecCollection(
      columns = list(spec("SUBJID", "^A"), spec("SUBJID", "^B")),
      rules = list()
    ),
    "unique"
  )
})

test_that("permitted values a YAML parser turned into numbers are flagged", {
  # `values: [1.10, 2.00]` unquoted arrives as 1.1 and 2, and a text column
  # compares them as text -- so the data the author meant to allow, "1.10",
  # would fail with no hint as to why. The spelling is gone by the time the
  # parser hands the list over, so the only honest move is to say so.
  columns <- list(list(
    id = "VERSION",
    type = "SAS Char",
    length = 8,
    values = c(1.10, 2.00)
  ))

  expect_warning(specs_from_list(columns), "read as numbers")

  # Quoted values are exactly what the author wanted, and say nothing.
  columns_quoted <- columns
  columns_quoted[[1]]$values <- c("1.10", "2.00")
  expect_no_warning(specs_from_list(columns_quoted))
})

test_that("specs_from_list(NULL) means no columns declared, not an error", {
  # An absent `columns:` key is not a specification error -- it is what the
  # serializer itself writes for a dataset with no columns (see
  # dta_dataset_to_list() in the Shiny app). Rejecting NULL here made the
  # package unable to read YAML it had itself just produced.
  specs <- specs_from_list(NULL)

  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
  expect_length(specs@columns, 0)
})

test_that("dta_dataset_from_list() builds a tabular dataset with no columns", {
  # Reachable by deleting a dataset's last column in the app's column editor,
  # and by every newly added dataset, which starts with none.
  ds <- dta_dataset_from_list(list(name = "d", type = "tabular"))

  expect_s3_class(ds, "DTAtools::DTADataSetTabular")
  expect_length(ds@specs@columns, 0)
})

test_that("a tabular dataset with no columns survives a full DTA YAML round trip", {
  # Regression guard: serialize the list shape a columnless dataset actually
  # produces (no `columns:` key at all, matching dta_dataset_to_list() in
  # inst/shiny/dta_app/R/utils_dta.R) and confirm the package can read its own
  # output back.
  dta_list <- list(
    metadata = list(title = "Round trip", version = "1.0"),
    datasets = list(
      list(name = "empty_ds", type = "tabular")
    )
  )
  yaml_text <- yaml::as.yaml(dta_list)

  expect_false(grepl("columns", yaml_text, fixed = TRUE))

  dta <- dta_from_list(yaml::yaml.load(yaml_text))

  expect_s3_class(dta, "DTAtools::DTA")
  ds <- datasets(dta, "empty_ds")
  expect_s3_class(ds, "DTAtools::DTADataSetTabular")
  expect_length(ds@specs@columns, 0)
})
