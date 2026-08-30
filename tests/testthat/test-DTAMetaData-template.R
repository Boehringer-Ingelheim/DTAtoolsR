# Coverage for the machine-owned `template` provenance property on
# DTAMetaData: the S7 property/validator (R/DTAMetaData-class.R), its
# serialisation (R/DTAMetaData-helpers.R's as.list() method), and the app-side
# guard that keeps a template definition from setting it itself
# (inst/shiny/dta_app/R/template_core.R). See helper-shinyapp.R for why the app
# helpers need app_fn()/app_env() rather than a direct call.

# ---- class-level: default, validator ---------------------------------------

test_that("DTAMetaData() defaults template to an empty list", {
  md <- DTAtools::DTAMetaData()

  expect_type(md@template, "list")
  expect_length(md@template, 0)
})

test_that("the validator rejects a template missing id or version, and accepts a well-formed one", {
  expect_error(
    DTAtools::DTAMetaData(title = "t", template = list(version = "1.0")),
    "template\\$id must be a single non-empty character string"
  )
  expect_error(
    DTAtools::DTAMetaData(title = "t", template = list(id = "x")),
    "template\\$version must be a single non-empty character string"
  )

  md <- DTAtools::DTAMetaData(title = "t", template = list(id = "x", version = "1.0"))
  expect_equal(md@template$id, "x")
  expect_equal(md@template$version, "1.0")
})

# ---- as.list(): presence/absence --------------------------------------------

test_that("an empty template is absent from as.list() output, not an empty key", {
  md <- DTAtools::DTAMetaData(title = "t", version = "1.0")

  result <- as.list(md)

  expect_false("template" %in% names(result))
})

test_that("a populated template is present in as.list() output", {
  template_block <- list(id = "biomarker_gf", version = "1.0")
  md <- DTAtools::DTAMetaData(title = "t", version = "1.0", template = template_block)

  result <- as.list(md)

  expect_true("template" %in% names(result))
  expect_equal(result$template, template_block)
})

# ---- as.list() -> yaml -> yaml.load -> DTAMetaData() round trip ------------

test_that("a populated template with a nested datasets list and a lineage vector survives as.list() -> yaml::as.yaml() -> yaml::yaml.load() -> DTAMetaData()", {
  template_block <- list(
    id = "biomarker_gf",
    version = "1.0",
    created = "2026-01-15",
    datasets = list(
      gf_data_specs_pattern = list(source = "gf_dataset.yaml")
    ),
    lineage = c("base_template", "biomarker_gf")
  )
  md <- DTAtools::DTAMetaData(title = "t", version = "1.0", template = template_block)

  yaml_text <- yaml::as.yaml(as.list(md), indent = 2, line.sep = "\n")
  loaded <- yaml::yaml.load(yaml_text)
  md2 <- do.call(DTAtools::DTAMetaData, loaded)

  expect_equal(md2@template$id, "biomarker_gf")
  expect_equal(md2@template$version, "1.0")
  expect_equal(md2@template$created, "2026-01-15")
  expect_equal(md2@template$datasets, template_block$datasets)
  expect_equal(md2@template$lineage, template_block$lineage)
})

# ---- full-document round trip via the app's serialiser ----------------------

test_that("full-document round trip (dta_to_yaml_text -> dta_read_yaml_text) preserves the template provenance block exactly", {
  # app_fixture_dta() reads inst/extdata/clinical_dta.yaml (a full metadata
  # block already); this is the assertion that would catch a missed edit in
  # DTAMetaData-helpers.R's as.list() method, since that is the only place the
  # property could be silently dropped on save.
  dta <- app_fixture_dta()

  template_block <- list(
    id = "biomarker_gf",
    version = "1.0",
    created = "2026-01-15",
    datasets = list(
      gf_data_specs_pattern = list(source = "gf_dataset.yaml")
    ),
    lineage = c("base_template", "biomarker_gf")
  )
  md <- dta@metadata
  S7::prop(md, "template") <- template_block
  dta@metadata <- md

  serialized <- app_fn("dta_to_yaml_text")(dta)
  expect_true(serialized$ok)

  round <- app_fn("dta_read_yaml_text")(serialized$value)
  expect_true(round$ok)

  expect_identical(round$value@metadata@template, template_block)
})

# ---- .dta_stringify_dates() recursion (verified by reading the function) ---

test_that(".dta_stringify_dates() recurses into a nested list (pinned): a Date nested two levels deep is converted to an ISO string", {
  # Read inst/shiny/dta_app/R/utils_dta.R:1577-1584: the function checks
  # inherits(x, "Date") first, and otherwise, when x is a list, recurses via
  # lapply(x, .dta_stringify_dates) -- which recurses again on any element that
  # is itself a list. So it DOES walk into nested lists, e.g.
  # metadata$template$created.
  fn <- app_fn(".dta_stringify_dates")

  nested <- list(template = list(id = "x", created = as.Date("2026-01-15")))
  result <- fn(nested)

  expect_type(result$template$created, "character")
  expect_equal(result$template$created, "2026-01-15")
  expect_equal(result$template$id, "x")
})

# ---- machine-owned field lists ----------------------------------------------

test_that("dta_metadata_machine_fields() lists both import_issues and template", {
  fields <- app_fn("dta_metadata_machine_fields")()

  expect_true("import_issues" %in% fields)
  expect_true("template" %in% fields)
})

test_that("dta_template_metadata_fields() excludes both machine fields but keeps ordinary ones", {
  fields <- app_fn("dta_template_metadata_fields")()

  expect_false("import_issues" %in% fields)
  expect_false("template" %in% fields)
  expect_true("title" %in% fields)
  expect_true("supplier" %in% fields)
  expect_true("transmission" %in% fields)
})

# ---- forgery is rejected -----------------------------------------------------

test_that("create_dta_from_template() rejects a template definition whose base.metadata sets `template` itself", {
  # Mirrors "create_dta_from_template() rejects an unknown base.metadata field
  # without throwing" in test-shinyapp-template.R. create_dta_from_template()
  # returns a dta_try() list (see inst/shiny/dta_app/R/utils_dta.R's dta_try(),
  # which never throws: ok = FALSE and error = conditionMessage(e)), so this
  # asserts on res$ok / res$error rather than expect_error().
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("name: mini_ds", "type: file", "files:", "  filename: mini.csv", "  type: csv"),
    file.path(tmp_dir, "dataset.yaml")
  )
  template_path <- file.path(tmp_dir, "template.yaml")

  template_def <- list(
    base = list(metadata = list(template = list(id = "forged", version = "1.0"))),
    datasets = list("dataset.yaml")
  )
  create_fn <- app_fn("create_dta_from_template")

  res <- create_fn(template_def, template_path, selections = list())

  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_match(res$error, "Unknown base.metadata field", fixed = TRUE)
})
