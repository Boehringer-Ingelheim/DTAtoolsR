# Coverage for the biomarker_gf template FAMILY as it actually ships in
# inst/extdata/library/: the gf_smrnaseq dataset template extracted out of
# what used to be a ~300-line inline `datasets:` block in
# biomarker_gf.dta-template.yaml, the creation template now importing it via
# `template:`/`as:`, its supplier-deviation child biomarker_gf_acme, and the
# two party profiles (supplier_acme, receiver_ourco) it offers through
# `party_slots:`.
#
# Reached via the app_env()/app_fn() harness in helper-shinyapp.R (see that
# file for why: the app's helper files are auto-sourced by Shiny at launch and
# are not part of the package namespace), except where a plain package-level
# call (DTAtools::validate_template(), datasets(), metadata()) is enough on its
# own.

# The real, installed/loaded package library ROOT -- never a temp
# fixture -- because this whole file is about what actually ships.
bundled_library_dir <- function() {
  dir <- system.file("extdata", "library", package = "DTAtools")
  expect_true(nzchar(dir), info = "inst/extdata/library missing from the package")
  dir
}

# A fresh index scoped to EXACTLY the packaged templates directory, built via
# build_template_index() (never the memoised, process-wide
# dta_template_index_cached()), so no other test file's cached state -- built
# under a different DTATOOLS_TEMPLATE_SOURCES override that has since been
# unwound -- can leak into what this file asserts. Mirrors index_over() in
# test-shinyapp-template-create.R, pointed at the real directory instead of a
# temp one.
bundled_templates_index <- function() {
  local_clean_template_env()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", bundled_library_dir()))
  app_fn("build_template_index")()
}

# ---- Dataset-template resolution + provenance stamping ----------------------

test_that("the gf_smrnaseq dataset template resolves via the index and stamps provenance on the built DTADataSet", {
  idx <- bundled_templates_index()
  row <- app_fn("resolve_template_ref")(idx, "gf_smrnaseq@3.0", kind = "dta_dataset_template")
  expect_false(is.null(row))
  expect_equal(row$id[[1]], "gf_smrnaseq")
  expect_equal(row$version[[1]], "3.0")

  read_tpl <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")
  path <- file.path(bundled_library_dir(), "templates", "biomarker_gf.dta-template.yaml")
  parsed <- read_tpl(path)
  expect_true(parsed$ok)

  res <- create_fn(parsed$value, path, selections = list(), index = idx)
  expect_true(res$ok, info = res$error %||% "")

  ds <- datasets(res$value)[["gf_data_specs_pattern"]]
  expect_false(is.null(ds))
  expect_equal(ds@template_source, "gf_smrnaseq")
  expect_equal(ds@template_version, "3.0")
  expect_equal(ds@template_date, "2024-12-17")
})

# ---- No index supplied: the engine builds one on demand ---------------------

test_that("building biomarker_gf with no index still works", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  read_tpl <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")
  path <- system.file(
    "extdata", "library", "templates", "biomarker_gf.dta-template.yaml",
    package = "DTAtools"
  )
  parsed <- read_tpl(path)
  expect_true(parsed$ok)

  res <- create_fn(parsed$value, path, selections = list())
  expect_true(res$ok, info = res$error %||% "")
  expect_equal(names(datasets(res$value)), "gf_data_specs_pattern")

  ds <- datasets(res$value)[["gf_data_specs_pattern"]]
  expect_equal(ds@template_source, "gf_smrnaseq")
})

# ---- vendor_name option flows into GFNAM's values ---------------------------

test_that("the vendor_name option flows into GFNAM's values", {
  idx <- bundled_templates_index()
  read_tpl <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")
  path <- file.path(bundled_library_dir(), "templates", "biomarker_gf.dta-template.yaml")
  parsed <- read_tpl(path)
  expect_true(parsed$ok)

  # No selection at all: the dataset template's own `default:` (the value
  # already there in the original inline column) is used.
  res_default <- create_fn(parsed$value, path, selections = list(), index = idx)
  expect_true(res_default$ok)
  gfnam_default <- datasets(res_default$value)[["gf_data_specs_pattern"]]@specs@columns[["GFNAM"]]
  expect_equal(gfnam_default@values, "ExternalSupplierName")

  # Directly building the dataset template with an explicit selection
  # confirms the option, not just its default, reaches the column.
  ds_tpl_path <- file.path(bundled_library_dir(), "templates", "gf_smrnaseq.dta-dataset-template.yaml")
  read_ds_tpl <- app_fn("read_dataset_template")
  build_ds <- app_fn("build_dataset_from_template")
  ds_def <- read_ds_tpl(ds_tpl_path)
  expect_true(ds_def$ok)
  built <- build_ds(ds_def$value, selections = list(vendor_name = "ACME Labs"))
  expect_true(built$ok)
  gfnam_col <- Filter(function(c) identical(c$id, "GFNAM"), built$value$dataset$columns)[[1]]
  expect_equal(gfnam_col$values, "ACME Labs")
})

# ---- biomarker_gf_acme: extends:, options merge, patch adds/modifies -------

test_that("biomarker_gf_acme inherits the parent's base and options, and its patch adds ACMEID and changes GFNAM's description", {
  idx <- bundled_templates_index()
  load_def <- app_fn("load_template_definition")
  create_fn <- app_fn("create_dta_from_template")

  parent_loaded <- load_def("biomarker_gf@1.0", index = idx)
  expect_true(parent_loaded$ok)
  parent_res <- create_fn(parent_loaded$value$def, parent_loaded$value$path, selections = list(), index = idx)
  expect_true(parent_res$ok)
  parent_ds <- datasets(parent_res$value)[["gf_data_specs_pattern"]]

  child_loaded <- load_def("biomarker_gf_acme@1.0", index = idx)
  expect_true(child_loaded$ok, info = child_loaded$error %||% "")
  def <- child_loaded$value$def

  # Inherited verbatim from the parent: every base.metadata field the child
  # never restates, and the 11 options the child does not override.
  expect_equal(def$base$metadata$title, "BIOMARKER GF DATA TRANSFER SPECIFICATIONS (DTS)")
  expect_equal(def$base$metadata$receiver$affiliation$name, "Our company")
  expect_length(def$options, 12)
  version_opt <- Filter(function(o) identical(o$id, "version"), def$options)[[1]]
  expect_equal(version_opt$target, "metadata.version")

  child_res <- create_fn(def, child_loaded$value$path, selections = list(), index = idx)
  expect_true(child_res$ok, info = child_res$error %||% "")
  md <- metadata(child_res$value)
  # header's option default is overridden by the child; nothing else in base
  # metadata changed.
  expect_equal(md@header, "ACME Labs")
  expect_equal(md@title, "BIOMARKER GF DATA TRANSFER SPECIFICATIONS (DTS)")

  child_ds <- datasets(child_res$value)[["gf_data_specs_pattern"]]
  child_cols <- child_ds@specs@columns
  parent_cols <- parent_ds@specs@columns

  # The patch added exactly one new column.
  expect_setequal(names(child_cols), c(names(parent_cols), "ACMEID"))
  acmeid <- child_cols[["ACMEID"]]
  expect_equal(acmeid@label, "ACME internal identifier")
  expect_true(acmeid@nullable)

  # The patch changed GFNAM's description...
  expect_equal(
    child_cols[["GFNAM"]]@description,
    "Name of the ACME sequencing facility supplying the results."
  )
  expect_false(identical(child_cols[["GFNAM"]]@description, parent_cols[["GFNAM"]]@description))
  # ...and the vendor_name option (set via the child's dataset entry
  # `options:`) changed GFNAM's values to ACME Labs.
  expect_equal(child_cols[["GFNAM"]]@values, "ACME Labs")

  # Every OTHER column is untouched: identical to the parent's, field for
  # field, including GFNAM's own non-description fields.
  other_ids <- setdiff(names(parent_cols), "GFNAM")
  for (id in other_ids) {
    expect_identical(child_cols[[id]], parent_cols[[id]], info = paste("column", id, "differs"))
  }
  gfnam_child_no_desc <- child_cols[["GFNAM"]]
  gfnam_parent_no_desc <- parent_cols[["GFNAM"]]
  S7::prop(gfnam_child_no_desc, "description") <- NULL
  S7::prop(gfnam_parent_no_desc, "description") <- NULL
  S7::prop(gfnam_child_no_desc, "values") <- NULL
  S7::prop(gfnam_parent_no_desc, "values") <- NULL
  expect_identical(gfnam_child_no_desc, gfnam_parent_no_desc)
})

# ---- Party profiles -----------------------------------------------------

test_that("both party profiles load and are eligible for their own slot only", {
  read_party <- app_fn("read_party_profile")
  supplier <- read_party(file.path(bundled_library_dir(), "parties", "supplier_acme.dta-party.yaml"))
  receiver <- read_party(file.path(bundled_library_dir(), "parties", "receiver_ourco.dta-party.yaml"))
  expect_true(supplier$ok, info = supplier$error %||% "")
  expect_true(receiver$ok, info = receiver$error %||% "")
  expect_equal(supplier$value$role, "supplier")
  expect_equal(receiver$value$role, "receiver")

  norm_slots <- app_fn("normalise_party_slots")
  for_slot <- app_fn("party_profiles_for_slot")

  slots <- norm_slots(list(
    list(id = "supplier", target = "metadata.supplier"),
    list(id = "receiver", target = "metadata.receiver")
  ))
  expect_length(slots, 2)

  profiles <- list(supplier$value, receiver$value)
  supplier_slot <- Filter(function(s) identical(s$id, "supplier"), slots)[[1]]
  receiver_slot <- Filter(function(s) identical(s$id, "receiver"), slots)[[1]]

  eligible_for_supplier <- for_slot(profiles, supplier_slot)
  eligible_for_receiver <- for_slot(profiles, receiver_slot)

  expect_equal(vapply(eligible_for_supplier, function(p) p$id, character(1)), "supplier_acme")
  expect_equal(vapply(eligible_for_receiver, function(p) p$id, character(1)), "receiver_ourco")
})

test_that("biomarker_gf's own party_slots resolve to exactly these two profiles through the index", {
  idx <- bundled_templates_index()
  read_tpl <- app_fn("read_dta_creation_template")
  norm_slots <- app_fn("normalise_party_slots")
  party_profiles <- app_fn("template_party_profiles")
  for_slot <- app_fn("party_profiles_for_slot")

  path <- file.path(bundled_library_dir(), "templates", "biomarker_gf.dta-template.yaml")
  parsed <- read_tpl(path)
  expect_true(parsed$ok)

  slots <- norm_slots(parsed$value$party_slots)
  expect_length(slots, 2)
  expect_setequal(vapply(slots, function(s) s$id, character(1)), c("supplier", "receiver"))

  profiles <- party_profiles(idx)
  supplier_slot <- Filter(function(s) identical(s$target, "metadata.supplier"), slots)[[1]]
  receiver_slot <- Filter(function(s) identical(s$target, "metadata.receiver"), slots)[[1]]

  expect_equal(
    vapply(for_slot(profiles, supplier_slot), function(p) p$id, character(1)),
    "supplier_acme"
  )
  expect_equal(
    vapply(for_slot(profiles, receiver_slot), function(p) p$id, character(1)),
    "receiver_ourco"
  )
})

# ---- validate_template(): zero errors on the shipped directory --------------

test_that("validate_template() reports zero severity-'error' rows for the packaged library", {
  result <- validate_template(bundled_library_dir())
  errors <- result[result$severity == "error", , drop = FALSE]
  expect_equal(
    nrow(errors), 0,
    info = paste(utils::capture.output(print(errors)), collapse = "\n")
  )
})

# ---- The packaged assets sit where the layout says -------------------------

test_that("the packaged library has one folder per kind, each holding only its own kind", {
  root <- bundled_library_dir()
  expect_setequal(
    list.files(file.path(root, "templates")),
    c(
      "biomarker_gf.dta-template.yaml", "biomarker_gf_acme.dta-template.yaml",
      "gf_smrnaseq.dta-dataset-template.yaml"
    )
  )
  expect_setequal(
    list.files(file.path(root, "parties")),
    c("receiver_ourco.dta-party.yaml", "supplier_acme.dta-party.yaml")
  )
  expect_setequal(
    list.files(file.path(root, "vocabulary")),
    c("gf_test.dta-vocabulary.yaml", "gf_tstdtl.dta-vocabulary.yaml")
  )
  # Nothing template-shaped sits flat in the root: that would be misplaced.
  expect_length(list.files(root, pattern = "[.]dta-[a-z-]+[.]ya?ml$"), 0)
  result <- validate_template(root)
  expect_false(any(result$code %in% c("template_misplaced", "template_in_subdirectory", "library_subfolder_given")))
})

test_that("the Word reference templates and CI assets are shipped outside the library", {
  word <- system.file("extdata", "word-templates", package = "DTAtools")
  expect_true(nzchar(word))
  expect_true(all(c("clinical_dta_template.docx", "dta_numbered_template.docx") %in% list.files(word)))

  ci <- system.file("extdata", "ci", package = "DTAtools")
  expect_true(nzchar(ci))
  expect_setequal(
    list.files(ci),
    c("validate-templates.yml", "bitbucket-pipelines.yml", "Jenkinsfile")
  )

  # None of it is inside the library root, which is what gets scanned.
  root <- bundled_library_dir()
  expect_length(list.files(root, pattern = "[.](docx|yml)$", recursive = TRUE), 0)
  # ... and nothing is left in the old flat directory (a bare empty folder
  # may linger in a working tree; git does not track it).
  old <- system.file("extdata", "templates", package = "DTAtools")
  expect_true(!nzchar(old) || length(list.files(old, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0)
})
