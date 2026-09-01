# Coverage for inst/shiny/dta_app/R/dataset_template.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why this
# is necessary: the app's helper files are auto-sourced by Shiny at launch and
# are not part of the package namespace).

Sys.setenv(NOT_CRAN = "true")

# A minimal but complete `dta_dataset_template` YAML, matching the shape
# documented at the top of dataset_template.R. `version_line`/`date_line` are
# parameterised so individual tests can swap in the unquoted-numeric /
# unquoted-Date forms that exercise read_dataset_template()'s coercions.
write_dataset_template_fixture <- function(
  dir,
  version_line = 'version: "3.0"',
  date_line = "date: 2024-12-17",
  filename = "template.yaml"
) {
  path <- file.path(dir, filename)
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: gf_smrnaseq",
      version_line,
      "label: GF domain smrnaseq",
      "description: Genomic Findings data table",
      date_line,
      "options:",
      "  - id: vendor_name",
      "    label: Vendor name",
      "    type: text",
      "    target: dataset.columns.GFNAM.values",
      "dataset:",
      "  name: gf_data_specs_pattern",
      "  type: tabular",
      "  files: { filename: gf.tsv, type: tsv }",
      "  columns:",
      "    - { id: STUDYID, label: Study Identifier, type: SAS Char }",
      "    - { id: GFNAM, label: Vendor Name, type: SAS Char }",
      "  rules: []"
    ),
    path
  )
  path
}

# A minimal plain-list dataset spec, the shape apply_dataset_template_path()/
# apply_dataset_patch() operate on directly (never an S7 object).
minimal_dataset_spec <- function() {
  list(
    name = "ds1",
    type = "tabular",
    description = "original description",
    files = list(filename = "ds1.tsv", type = "tsv"),
    columns = list(
      list(id = "STUDYID", label = "Study Identifier", type = "SAS Char"),
      list(id = "GFNAM", label = "Vendor Name", type = "SAS Char")
    ),
    rules = list(
      list(id = "rule_one", description = "first rule", type = "col_condition")
    )
  )
}

# ---- read_dataset_template ---------------------------------------------------

test_that("read_dataset_template() accepts a valid file", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_true(res$ok)
  expect_null(res$error)
  expect_equal(res$value$id, "gf_smrnaseq")
  expect_equal(res$value$version, "3.0")
  expect_equal(res$value$label, "GF domain smrnaseq")
  expect_equal(res$value$description, "Genomic Findings data table")
  expect_equal(res$value$date, "2024-12-17")
  expect_length(res$value$options, 1)
  expect_equal(res$value$dataset$name, "gf_data_specs_pattern")
})

test_that("read_dataset_template() rejects a wrong 'kind'", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: x",
      'version: "1.0"',
      "dataset:",
      "  name: ds1"
    ),
    path
  )

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_false(res$ok)
  expect_null(res$value)
  # App's own hardcoded string, not a base R/yaml message -- fine to assert.
  expect_equal(res$error, "Dataset template 'kind' must be 'dta_dataset_template'.")
})

test_that("read_dataset_template() rejects a missing 'id'", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      'version: "1.0"',
      "dataset:",
      "  name: ds1"
    ),
    path
  )

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_false(res$ok)
  expect_equal(res$error, "Dataset template must define a non-empty 'id'.")
})

test_that("read_dataset_template() rejects a missing 'version'", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: x",
      "dataset:",
      "  name: ds1"
    ),
    path
  )

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_false(res$ok)
  expect_equal(res$error, "Dataset template must define a non-empty 'version'.")
})

test_that("read_dataset_template() rejects a missing 'dataset' section", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: x",
      'version: "1.0"'
    ),
    path
  )

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_false(res$ok)
  # No 'datasets:' present either, so there is nothing to hint at.
  expect_equal(res$error, "Dataset template must contain a 'dataset' section.")
})

test_that("read_dataset_template() rejects a 'datasets:' (plural) body, naming the real defect", {
  # PINNED BUG: read_dataset_template() used to read def$dataset, and R's `$`
  # on a list falls back to partial matching. 'dataset' is a strict PREFIX of
  # 'datasets' -- a DIFFERENT, equally legitimate top-level key (a
  # dta_creation_template's own datasets: array) -- so a dta_dataset_template
  # file that was slipped a 'datasets:' array instead (the obvious mistake
  # when converting one kind of template into the other) had def$dataset
  # silently return that array. is.list() on it was still TRUE, so the
  # 'dataset' section guard passed, and the error that actually surfaced was
  # the unrelated-looking "'dataset' section must contain a 'name'" -- because
  # the (nameless) datasets array has no $name of its own -- rather than
  # anything naming the real problem. Fixed by reading def[["dataset"]], which
  # matches exactly, and reporting the absence with an explicit hint.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: gf_smrnaseq",
      'version: "3.0"',
      "label: GF domain smrnaseq",
      "datasets:",
      "  - name: gf_data_specs_pattern",
      "    type: tabular",
      "    files: { filename: gf.tsv, type: tsv }",
      "    columns:",
      "      - { id: STUDYID, label: Study Identifier, type: SAS Char }"
    ),
    path
  )

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_false(res$ok)
  # The exact, actionable diagnosis -- not the misleading "must contain a
  # 'name'" a reader would see if def$dataset silently returned the datasets
  # array.
  expect_equal(
    res$error,
    paste0(
      "Dataset template must contain a 'dataset' section. Found 'datasets:' ",
      "(plural) instead -- that belongs to a dta_creation_template; a ",
      "dta_dataset_template needs the singular 'dataset:'."
    )
  )
  expect_no_match(res$error, "must contain a 'name'", fixed = TRUE)
})

test_that("build_dataset_from_template() rejects a hand-built def carrying 'datasets:' instead of 'dataset:'", {
  # Same `[[` vs `$` hazard as read_dataset_template() above, but exercised
  # against build_dataset_from_template()'s OWN guard -- it re-checks
  # `def[["dataset"]]` because a caller (a test, a future direct build path)
  # may hand it a `def` that never went through read_dataset_template().
  fn <- app_fn("build_dataset_from_template")
  def <- list(
    id = "gf_smrnaseq", version = "3.0",
    datasets = list(list(name = "gf_data_specs_pattern", type = "tabular"))
  )

  res <- fn(def)

  expect_false(res$ok)
  expect_match(res$error, "Found 'datasets:' \\(plural\\) instead")
})

test_that("read_dataset_template() coerces an unquoted numeric version to character", {
  dir <- withr::local_tempdir()
  # Unquoted 3.0 parses as a double under yaml::read_yaml() -- and
  # as.character(3.0) alone would give "3", not "3.0" (R drops the trailing
  # zero), which is exactly the failure mode the shared
  # dta_template_version_string() coercion exists to avoid.
  path <- write_dataset_template_fixture(dir, version_line = "version: 3.0")

  fn <- app_fn("read_dataset_template")
  # No warning: the version is re-read from the file TEXT, which is exact, so
  # there is nothing to apologise for. A plain parse would have given the
  # double 3.0 and as.character() would have rendered that "3".
  expect_no_warning(res <- fn(path))

  expect_true(res$ok)
  expect_type(res$value$version, "character")
  expect_equal(res$value$version, "3.0")
})

test_that("read_dataset_template() distinguishes an unquoted 1.10 from 1.1", {
  dir <- withr::local_tempdir()
  # THE assertion that proves the exact reader is doing real work. Under a
  # plain yaml parse `version: 1.10` is the double 1.1 before any R code sees
  # it, and 1.10 and 1.1 are different releases -- 1.10 being the LATER one.
  # Getting this wrong silently resolves `gf@1.10` to the wrong file.
  expect_identical(yaml::yaml.load("v: 1.10")$v, 1.1)

  fn <- app_fn("read_dataset_template")
  p10 <- write_dataset_template_fixture(dir, version_line = "version: 1.10")
  expect_equal(fn(p10)$value$version, "1.10")

  dir9 <- withr::local_tempdir()
  p9 <- write_dataset_template_fixture(dir9, version_line = "version: 1.9")
  expect_equal(fn(p9)$value$version, "1.9")
})

test_that("read_dataset_template() takes a quoted version verbatim and stays silent", {
  dir <- withr::local_tempdir()
  # The counterpart to the test above: a quoted version is exact, so there is
  # nothing to warn about. A warning here would train authors to ignore them.
  path <- write_dataset_template_fixture(dir, version_line = 'version: "1.10"')

  fn <- app_fn("read_dataset_template")
  expect_no_warning(res <- fn(path))

  expect_true(res$ok)
  # "1.10" survives ONLY because it was quoted. Unquoted, YAML would have made
  # it the double 1.1 before any R code could see it.
  expect_equal(res$value$version, "1.10")
})

test_that("read_dataset_template() converts a YAML Date value to character", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir, date_line = "date: 2024-12-17")

  fn <- app_fn("read_dataset_template")
  res <- fn(path)

  expect_true(res$ok)
  expect_type(res$value$date, "character")
  expect_equal(res$value$date, "2024-12-17")
})

# ---- dataset_template_path_parts --------------------------------------------

test_that("dataset_template_path_parts() returns the tail of a dataset-rooted path", {
  fn <- app_fn("dataset_template_path_parts")
  expect_equal(fn("dataset.description"), "description")
  expect_equal(fn("dataset.columns.GFNAM.values"), c("columns", "GFNAM", "values"))
})

test_that("dataset_template_path_parts() aborts when the root is not 'dataset'", {
  fn <- app_fn("dataset_template_path_parts")
  expect_error(fn("metadata.title"), regexp = "rooted at 'dataset\\.'")
})

# ---- apply_dataset_template_path --------------------------------------------

test_that("apply_dataset_template_path() sets a top-level scalar field", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  out <- fn(ds, "dataset.description", "new description")

  expect_equal(out$description, "new description")
  # Nothing else in the spec is disturbed by a scalar-field set.
  expect_equal(out$name, "ds1")
  expect_length(out$columns, 2)
})

test_that("apply_dataset_template_path() sets a files.<key> field when files is a single map", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  out <- fn(ds, "dataset.files.filename", "renamed.tsv")

  expect_equal(out$files$filename, "renamed.tsv")
  expect_equal(out$files$type, "tsv") # untouched sibling key
})

test_that("apply_dataset_template_path() aborts naming the dataset when 'files' holds multiple entries", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$files <- list(
    list(filename = "a.tsv", type = "tsv"),
    list(filename = "b.tsv", type = "tsv")
  )

  expect_error(
    fn(ds, "dataset.files.filename", "x"),
    regexp = "ambiguous.*ds1",
    perl = TRUE
  )
})

test_that("apply_dataset_template_path() sets a column field addressed by id", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  out <- fn(ds, "dataset.columns.GFNAM.values", "Acme Corp")

  expect_equal(out$columns[[2]]$values, "Acme Corp")
  # The other column is untouched.
  expect_null(out$columns[[1]]$values)
})

test_that("apply_dataset_template_path() aborts naming the id for an unknown column", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  err <- tryCatch(
    fn(ds, "dataset.columns.NOPE.values", "x"),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "NOPE", fixed = TRUE)
  expect_match(err, "STUDYID", fixed = TRUE)
  expect_match(err, "GFNAM", fixed = TRUE)
})

test_that("apply_dataset_template_path() sets a rule field addressed by id", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  out <- fn(ds, "dataset.rules.rule_one.description", "updated rule text")

  expect_equal(out$rules[[1]]$description, "updated rule text")
})

test_that("apply_dataset_template_path() aborts naming the id for an unknown rule", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  err <- tryCatch(
    fn(ds, "dataset.rules.no_such_rule.description", "x"),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "no_such_rule", fixed = TRUE)
  expect_match(err, "rule_one", fixed = TRUE)
})

test_that("id-based column addressing survives a column being inserted ahead of it", {
  # apply_dataset_patch()'s own add_columns always appends to the END of the
  # existing column list (see the "each op" tests below) -- so appending
  # through it never actually shifts an existing column's position, and would
  # not exercise this regression. The insertion is therefore constructed
  # directly here, simulating a column landing AHEAD of an existing one. What
  # is under test is entirely apply_dataset_template_path()'s id lookup: a
  # positional ("the column at index 1") implementation would silently start
  # updating the wrong column the instant anything landed ahead of the one a
  # caller actually meant, which is exactly the failure mode id-addressing
  # exists to rule out.
  path_fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()

  ds$columns <- c(
    list(list(id = "NEWFIRST", label = "New First Column", type = "SAS Char")),
    ds$columns
  )
  expect_equal(
    vapply(ds$columns, function(c) c$id, character(1)),
    c("NEWFIRST", "STUDYID", "GFNAM")
  )

  out <- path_fn(ds, "dataset.columns.STUDYID.label", "Updated Study Identifier")

  expect_equal(out$columns[[2]]$id, "STUDYID")
  expect_equal(out$columns[[2]]$label, "Updated Study Identifier")
  # Neither the new front column nor the other pre-existing one moved or
  # changed.
  expect_equal(out$columns[[1]]$label, "New First Column")
  expect_equal(out$columns[[3]]$label, "Vendor Name")
})

# ---- apply_dataset_patch -----------------------------------------------------

test_that("apply_dataset_patch() with an absent/empty patch leaves the dataset unchanged", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res_null <- fn(ds, NULL)
  expect_identical(res_null$dataset, ds)
  expect_length(res_null$deviations, 0)

  res_empty <- fn(ds, list())
  expect_identical(res_empty$dataset, ds)
  expect_length(res_empty$deviations, 0)
})

test_that("apply_dataset_patch() remove_columns removes the named column(s)", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(remove_columns = "STUDYID"))

  expect_equal(vapply(res$dataset$columns, function(c) c$id, character(1)), "GFNAM")
  expect_equal(res$deviations, list(list(op = "remove_columns", target = "STUDYID")))
})

test_that("apply_dataset_patch() remove_columns aborts on an id that is not present", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  expect_error(
    fn(ds, list(remove_columns = "NOPE")),
    regexp = "Unknown column id 'NOPE'"
  )
})

test_that("apply_dataset_patch() add_columns appends new columns in order", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(add_columns = list(
    list(id = "NEWA", label = "New A", type = "SAS Char"),
    list(id = "NEWB", label = "New B", type = "SAS Char")
  )))

  expect_equal(
    vapply(res$dataset$columns, function(c) c$id, character(1)),
    c("STUDYID", "GFNAM", "NEWA", "NEWB")
  )
  expect_equal(
    res$deviations,
    list(
      list(op = "add_columns", target = "NEWA"),
      list(op = "add_columns", target = "NEWB")
    )
  )
})

test_that("apply_dataset_patch() add_columns aborts on a duplicate id", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  expect_error(
    fn(ds, list(add_columns = list(list(id = "GFNAM", label = "dup", type = "SAS Char")))),
    regexp = "already exists"
  )
})

test_that("apply_dataset_patch() modify_columns merges fields, child keys win", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(modify_columns = list(
    list(id = "GFNAM", label = "Renamed Vendor Name")
  )))

  gfnam <- res$dataset$columns[[2]]
  expect_equal(gfnam$label, "Renamed Vendor Name")
  expect_equal(gfnam$type, "SAS Char") # untouched, not mentioned in the spec
  expect_equal(res$deviations, list(list(op = "modify_columns", target = "GFNAM")))
})

test_that("apply_dataset_patch() modify_columns aborts on an id that is not present", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  expect_error(
    fn(ds, list(modify_columns = list(list(id = "NOPE", label = "x")))),
    regexp = "Unknown column id 'NOPE'"
  )
})

test_that("apply_dataset_patch() set applies dataset-relative dotted paths", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(set = list(
    description = "patched description",
    "files.filename" = "patched.tsv",
    "columns.GFNAM.values" = "Acme Corp"
  )))

  expect_equal(res$dataset$description, "patched description")
  expect_equal(res$dataset$files$filename, "patched.tsv")
  expect_equal(res$dataset$columns[[2]]$values, "Acme Corp")
  expect_equal(
    res$deviations,
    list(
      list(op = "set", target = "description"),
      list(op = "set", target = "files.filename"),
      list(op = "set", target = "columns.GFNAM.values")
    )
  )
})

test_that("apply_dataset_patch() applies remove/add/modify/set in the documented order", {
  # Order under test: remove_columns, then add_columns, then modify_columns,
  # then set -- see the comment on apply_dataset_patch() for the rationale.
  # This patch is only satisfiable in that order: modify_columns targets a
  # column (`D`) that does not exist until add_columns has already run, and
  # `set` addresses that same column by id, which only resolves against the
  # FINAL column list once add/remove/modify are all done.
  fn <- app_fn("apply_dataset_patch")
  ds <- list(
    name = "ds1",
    type = "tabular",
    description = "original",
    columns = list(
      list(id = "A", label = "Alpha", type = "SAS Char"),
      list(id = "B", label = "Beta", type = "SAS Char"),
      list(id = "C", label = "Gamma", type = "SAS Char")
    ),
    rules = list()
  )

  patch <- list(
    remove_columns = "B",
    add_columns = list(list(id = "D", label = "Delta", type = "SAS Char")),
    modify_columns = list(list(id = "D", label = "Delta Modified")),
    set = list(description = "patched", "columns.D.pattern" = "^D[0-9]+$")
  )

  res <- fn(ds, patch)

  expect_equal(vapply(res$dataset$columns, function(c) c$id, character(1)), c("A", "C", "D"))
  expect_equal(res$dataset$columns[[3]]$label, "Delta Modified")
  expect_equal(res$dataset$columns[[3]]$pattern, "^D[0-9]+$")
  expect_equal(res$dataset$description, "patched")

  expect_equal(
    res$deviations,
    list(
      list(op = "remove_columns", target = "B"),
      list(op = "add_columns", target = "D"),
      list(op = "modify_columns", target = "D"),
      list(op = "set", target = "description"),
      list(op = "set", target = "columns.D.pattern")
    )
  )
})

# ---- dataset_template_selection_values --------------------------------------

test_that("dataset_template_selection_values() prefers a selection over the option default", {
  fn <- app_fn("dataset_template_selection_values")
  def <- list(options = list(
    list(id = "vendor_name", type = "text", target = "dataset.columns.GFNAM.values", default = "Default Corp")
  ))

  expect_equal(fn(def, list(vendor_name = "Chosen Corp")), list(vendor_name = "Chosen Corp"))
  expect_equal(fn(def, list()), list(vendor_name = "Default Corp"))
})

# ---- build_dataset_from_template ---------------------------------------------

test_that("build_dataset_from_template() writes an option's selected value into the dataset", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(def, selections = list(vendor_name = "Acme Corp"))

  expect_true(res$ok)
  expect_equal(res$value$dataset$columns[[2]]$id, "GFNAM")
  expect_equal(res$value$dataset$columns[[2]]$values, "Acme Corp")
})

test_that("build_dataset_from_template() falls back to the option default when no selection is made", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: gf_smrnaseq",
      'version: "1.0"',
      "options:",
      "  - id: vendor_name",
      "    type: text",
      "    target: dataset.columns.GFNAM.values",
      "    default: Fallback Vendor",
      "dataset:",
      "  name: ds1",
      "  type: tabular",
      "  files: { filename: gf.tsv, type: tsv }",
      "  columns:",
      "    - { id: GFNAM, label: Vendor Name, type: SAS Char }",
      "  rules: []"
    ),
    path
  )
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(def, selections = list())

  expect_true(res$ok)
  expect_equal(res$value$dataset$columns[[1]]$values, "Fallback Vendor")
})

test_that("build_dataset_from_template() renames the dataset when as_name is given", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(def, selections = list(vendor_name = "Acme Corp"), as_name = "gf_second_copy")

  expect_true(res$ok)
  expect_equal(res$value$dataset$name, "gf_second_copy")
})

test_that("build_dataset_from_template() applies the patch on top of options", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(
    def,
    selections = list(vendor_name = "Acme Corp"),
    patch = list(remove_columns = "STUDYID")
  )

  expect_true(res$ok)
  expect_equal(vapply(res$value$dataset$columns, function(c) c$id, character(1)), "GFNAM")
  expect_equal(res$value$deviations, list(list(op = "remove_columns", target = "STUDYID")))
  expect_equal(res$value$provenance$deviations, res$value$deviations)
})

test_that("build_dataset_from_template() stamps the three provenance keys", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res_default <- fn(def, selections = list(vendor_name = "Acme Corp"))
  expect_true(res_default$ok)
  expect_equal(res_default$value$dataset$template_source, "gf_smrnaseq")
  expect_equal(res_default$value$dataset$template_version, "3.0")
  expect_equal(res_default$value$dataset$template_date, "2024-12-17")

  res_labelled <- fn(
    def,
    selections = list(vendor_name = "Acme Corp"),
    source_label = "uploaded"
  )
  expect_equal(res_labelled$value$dataset$template_source, "gf_smrnaseq (uploaded)")

  expect_equal(res_default$value$provenance, list(
    name = "gf_data_specs_pattern",
    template = "gf_smrnaseq",
    version = "3.0",
    deviations = list()
  ))
})

test_that("build_dataset_from_template() output round-trips through dta_dataset_from_list() with correct S7 properties", {
  # Deliberately unquoted here (`version: 3.0`, a Date-shaped `date:`) so this
  # test exercises the SAME coercions read_dataset_template() is unit-tested
  # for above, but end to end: the plain list build_dataset_from_template()
  # returns is what actually gets handed to do.call(DTADataSetFactory, x), and
  # this is the assertion that proves that hand-off does not trip the
  # class_character_or_null validator on DTADataSet@template_version /
  # @template_date.
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir, version_line = "version: 3.0")
  # Unquoted on purpose (see above); the exact reader handles it silently.
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(def, selections = list(vendor_name = "Acme Corp"))
  expect_true(res$ok)

  ds_obj <- DTAtools::dta_dataset_from_list(res$value$dataset)

  expect_s3_class(ds_obj, "DTAtools::DTADataSet")
  expect_equal(ds_obj@template_source, "gf_smrnaseq")
  expect_equal(ds_obj@template_version, "3.0")
  expect_equal(ds_obj@template_date, "2024-12-17")
  expect_equal(ds_obj@name, "gf_data_specs_pattern")
})

test_that("build_dataset_from_template() with an empty patch leaves the dataset unchanged besides provenance", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(def, selections = list(vendor_name = "Acme Corp"), patch = NULL)

  expect_true(res$ok)
  expect_length(res$value$deviations, 0)
  # Nothing besides the option effect and the provenance stamp touched the
  # dataset -- the column set, file, and other fields all survive untouched.
  # (`description` here is the FIXTURE's own dataset-level field, which the
  # fixture leaves unset -- the "Genomic Findings data table" text lives on
  # the template's top-level `def$description`, a different field entirely.)
  expect_equal(res$value$dataset$files, list(filename = "gf.tsv", type = "tsv"))
  expect_equal(res$value$dataset$columns[[1]]$id, "STUDYID")
  expect_null(res$value$dataset$description)
  expect_equal(res$value$dataset$type, "tabular")
})

# ---- columns: the shared collection vocabulary ------------------------------

# `remove_columns:`/`add_columns:`/`modify_columns:` are three of the same five
# verbs `options:` and `datasets:` use. `columns:` spells them the shared way
# and adds the two the named ops cannot express -- keep only a subset, and
# replace the column list wholesale.

test_that("apply_dataset_patch() columns: remove drops an inherited column", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()
  before <- length(ds$columns)

  res <- fn(ds, list(columns = list(remove = "GFNAM")))

  ids <- vapply(res$dataset$columns, function(c) c$id, character(1))
  expect_length(res$dataset$columns, before - 1L)
  expect_false("GFNAM" %in% ids)
  # The verbs report in the `*_columns` vocabulary the UI already reads: they
  # are a second spelling of these ops, not a second set of them.
  ops <- vapply(res$deviations, function(d) paste(d$op, d$target), character(1))
  expect_true("remove_columns GFNAM" %in% ops)
})

test_that("apply_dataset_patch() columns: inherit none replaces the column list wholesale", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(columns = list(
    inherit = "none",
    add = list(list(id = "ONLY", label = "Only column", type = "SAS Char"))
  )))

  expect_equal(vapply(res$dataset$columns, function(c) c$id, character(1)), "ONLY")
})

test_that("apply_dataset_patch() columns: inherit [ids] keeps only the named subset", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()
  kept <- ds$columns[[1]]$id

  res <- fn(ds, list(columns = list(inherit = kept)))

  expect_equal(vapply(res$dataset$columns, function(c) c$id, character(1)), kept)
})

test_that("apply_dataset_patch() columns: modify merges and add refuses a duplicate id", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(columns = list(
    modify = list(list(id = "GFNAM", label = "Renamed"))
  )))
  gfnam <- Filter(function(c) identical(c$id, "GFNAM"), res$dataset$columns)[[1]]
  expect_equal(gfnam$label, "Renamed")
  expect_equal(gfnam$type, "SAS Char") # untouched

  expect_error(
    fn(ds, list(columns = list(add = list(list(id = "GFNAM"))))),
    regexp = "already inherited"
  )
})

test_that("apply_dataset_patch() columns: rejects a bare list, naming the named ops", {
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  expect_error(
    fn(ds, list(columns = list(list(id = "GFNAM", label = "x")))),
    regexp = "must be a mapping"
  )
})

test_that("apply_dataset_patch() modify_columns replaces a sequence of mappings instead of discarding it", {
  # KNOWN DEFECT FIXED. modify_columns merged with utils::modifyList(), which
  # recurses whenever both sides are lists -- and an unnamed list has no names
  # to walk, so it returned the PARENT untouched and the child's value vanished
  # in silence. dta_template_merge_value() replaces a sequence wholesale, which
  # is what every other section of the engine already did.
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()
  ds$columns[[2]]$rules <- list(list(kind = "parent_rule"))

  res <- fn(ds, list(modify_columns = list(
    list(id = "GFNAM", rules = list(list(kind = "child_rule")))
  )))

  gfnam <- res$dataset$columns[[2]]
  expect_length(gfnam$rules, 1)
  expect_equal(gfnam$rules[[1]]$kind, "child_rule")
})

test_that("apply_dataset_patch() modify_columns still deletes a property set to null", {
  # The other half of the merge-engine swap: modifyList() deleted on NULL, and
  # dta_template_merge_value() has to keep doing so or `field: ~` in a patch
  # would start writing a null instead of removing the field.
  fn <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()

  res <- fn(ds, list(modify_columns = list(
    list(id = "GFNAM", type = NULL)
  )))

  gfnam <- res$dataset$columns[[2]]
  expect_false("type" %in% names(gfnam))
  expect_equal(gfnam$id, "GFNAM")
})
