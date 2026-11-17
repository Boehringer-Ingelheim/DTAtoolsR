# Coverage for inst/shiny/dta_app/R/dataset_template.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why this
# is necessary: the app's helper files are auto-sourced by Shiny at launch and
# are not part of the package namespace).

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
      "  id: gf_data_specs_pattern",
      "  type: tabular",
      "  name: Genomic Findings data table",
      "  description: Genomic Findings data, delivered as tab-separated files.",
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
    id = "ds1",
    type = "tabular",
    name = "Dataset one",
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
  expect_equal(res$value$dataset$id, "gf_data_specs_pattern")
  expect_equal(res$value$dataset$name, "Genomic Findings data table")
  expect_equal(
    res$value$dataset$description,
    "Genomic Findings data, delivered as tab-separated files."
  )
})

test_that("read_dataset_template() rejects a dataset without an id, even one that has a name", {
  dir <- withr::local_tempdir()
  bodies <- list(
    only_type = c("  type: tabular", "  description: A dataset nobody can address"),
    # `name` is the label, so it can no more stand in for the identifier than
    # nothing at all can.
    only_name = c("  name: Some label", "  type: tabular")
  )
  for (case in names(bodies)) {
    path <- file.path(dir, paste0(case, ".yaml"))
    writeLines(
      c(
        "kind: dta_dataset_template",
        "id: x",
        'version: "1.0"',
        "dataset:",
        bodies[[case]],
        "  files: { filename: gf.tsv, type: tsv }"
      ),
      path
    )

    res <- app_fn("read_dataset_template")(path)

    expect_false(res$ok)
    expect_null(res$value)
    expect_equal(res$error, "Dataset template's 'dataset' section must contain an 'id'.")
  }
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
      "  id: ds1"
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
      "  id: ds1"
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
      "  id: ds1"
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
  # the unrelated-looking "'dataset' section must contain an 'id'" -- because
  # the (id-less) datasets array has no $id of its own -- rather than
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
      "  - id: gf_data_specs_pattern",
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
  # The exact, actionable diagnosis -- not the misleading "must contain an
  # 'id'" a reader would see if def$dataset silently returned the datasets
  # array.
  expect_equal(
    res$error,
    paste0(
      "Dataset template must contain a 'dataset' section. Found 'datasets:' ",
      "(plural) instead -- that belongs to a dta_creation_template; a ",
      "dta_dataset_template needs the singular 'dataset:'."
    )
  )
  expect_no_match(res$error, "must contain an 'id'", fixed = TRUE)
})

test_that("build_dataset_from_template() rejects a hand-built def carrying 'datasets:' instead of 'dataset:'", {
  # Same `[[` vs `$` hazard as read_dataset_template() above, but exercised
  # against build_dataset_from_template()'s OWN guard -- it re-checks
  # `def[["dataset"]]` because a caller (a test, a future direct build path)
  # may hand it a `def` that never went through read_dataset_template().
  fn <- app_fn("build_dataset_from_template")
  def <- list(
    id = "gf_smrnaseq", version = "3.0",
    datasets = list(list(id = "gf_data_specs_pattern", type = "tabular"))
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
  expect_equal(out$id, "ds1")
  expect_equal(out$name, "Dataset one")
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
    id = "ds1",
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
      "  id: ds1",
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

test_that("build_dataset_from_template() sets the dataset's id when as_name is given", {
  dir <- withr::local_tempdir()
  path <- write_dataset_template_fixture(dir)
  def <- app_fn("read_dataset_template")(path)$value
  fn <- app_fn("build_dataset_from_template")

  res <- fn(def, selections = list(vendor_name = "Acme Corp"), as_name = "gf_second_copy")

  expect_true(res$ok)
  expect_equal(res$value$dataset$id, "gf_second_copy")
  # Only the identifier changes: two datasets built from one template may share
  # a label, so the template's name (and description) are left alone.
  expect_equal(res$value$dataset$name, "Genomic Findings data table")
  expect_equal(
    res$value$dataset$description,
    "Genomic Findings data, delivered as tab-separated files."
  )
  # The provenance records the id the dataset ended up with.
  expect_equal(res$value$provenance$id, "gf_second_copy")
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

test_that("build_dataset_from_template() stamps the provenance keys", {
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
    id = "gf_data_specs_pattern",
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
  expect_equal(ds_obj@id, "gf_data_specs_pattern")
  expect_equal(ds_obj@name, "Genomic Findings data table")
  expect_equal(
    ds_obj@description,
    "Genomic Findings data, delivered as tab-separated files."
  )
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
  # (`name` and `description` here are the FIXTURE's own dataset-level fields;
  # the "Genomic Findings data table" text on the template's top-level
  # `def$description` is a different field entirely.)
  expect_equal(res$value$dataset$files, list(filename = "gf.tsv", type = "tsv"))
  expect_equal(res$value$dataset$columns[[1]]$id, "STUDYID")
  expect_equal(res$value$dataset$id, "gf_data_specs_pattern")
  expect_equal(res$value$dataset$name, "Genomic Findings data table")
  expect_equal(
    res$value$dataset$description,
    "Genomic Findings data, delivered as tab-separated files."
  )
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

# ---- column ids that contain spaces or dots ------------------------------------

# A column id is the exact header text, so `Subject ID`, `Wt. (kg)` and `a.b`
# are all legal. Paths are split on ".", which is why an id with a dot cannot
# be told apart from a field path unless it is matched against the declared
# ids: the LONGEST declared id wins (dataset_item_split_path()).

col_ids <- function(ds, key = "columns") {
  vapply(ds[[key]], function(x) x$id, character(1))
}

test_that("a path addresses a column whose id contains spaces", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$columns <- list(
    list(id = "Subject ID", label = "Subject", type = "SAS Char"),
    list(id = "Visit Date (UTC)", label = "Visit", type = "SAS Char"),
    list(id = "Dose mg/kg", label = "Dose", type = "SAS Num")
  )

  out <- fn(ds, "dataset.columns.Visit Date (UTC).values", "2024-01-01")

  expect_identical(col_ids(out), c("Subject ID", "Visit Date (UTC)", "Dose mg/kg"))
  expect_identical(out$columns[[2]]$values, "2024-01-01")
  expect_null(out$columns[[1]]$values)
  expect_null(out$columns[[3]]$values)
  out <- fn(ds, "dataset.columns.Dose mg/kg.description", "mg per kg")
  expect_identical(out$columns[[3]]$description, "mg per kg")
})

test_that("a path addresses a column whose id contains dots, and nothing else", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$columns <- list(
    list(id = "Subject.ID", label = "S", type = "SAS Char"),
    list(id = "Wt. (kg)", label = "W", type = "SAS Num"),
    list(id = "Dose No.", label = "D", type = "SAS Num")
  )

  out <- fn(ds, "dataset.columns.Subject.ID.description", "one")
  expect_identical(out$columns[[1]]$description, "one")
  expect_null(out$columns[[2]]$description)
  # the id is not split into a column `Subject` carrying a field `ID`
  expect_identical(col_ids(out), c("Subject.ID", "Wt. (kg)", "Dose No."))
  expect_false("ID" %in% names(out$columns[[1]]))

  out <- fn(ds, "dataset.columns.Wt. (kg).description", "two")
  expect_identical(out$columns[[2]]$description, "two")
  expect_null(out$columns[[1]]$description)

  # An id that ENDS in a dot: the path reads "Dose No." + "." + "description".
  out <- fn(ds, "dataset.columns.Dose No..description", "three")
  expect_identical(out$columns[[3]]$description, "three")
  expect_null(out$columns[[2]]$description)
})

test_that("a column field below the id may itself be a nested path", {
  # Column fields are not flat (`values_from` is a mapping): the id is the
  # longest declared id that leaves a field behind, not "everything up to the
  # last segment".
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$columns <- list(list(
    id = "Wt. (kg)", label = "W", type = "SAS Char",
    values_from = list(vocabulary = "v@1.0", exclude = list("x"))
  ))

  out <- fn(ds, "dataset.columns.Wt. (kg).values_from.exclude", list("y"))

  expect_identical(col_ids(out), "Wt. (kg)")
  expect_identical(out$columns[[1]]$values_from$exclude, list("y"))
  expect_identical(out$columns[[1]]$values_from$vocabulary, "v@1.0")
})

test_that("the longest declared id wins when one id is a prefix of another", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$columns <- list(
    list(id = "a", label = "A", type = "SAS Char", b = "field b of column a"),
    list(id = "a.b", label = "AB", type = "SAS Char"),
    list(id = "TAIL", label = "T", type = "SAS Char")
  )

  # `columns.a.b.description` is the column `a.b`; column `a` is left alone.
  out <- fn(ds, "dataset.columns.a.b.description", "for a.b")
  expect_identical(out$columns[[2]]$description, "for a.b")
  expect_null(out$columns[[1]]$description)
  expect_identical(out$columns[[1]]$b, "field b of column a")

  # The shorter id still answers for its own fields.
  out <- fn(ds, "dataset.columns.a.description", "for a")
  expect_identical(out$columns[[1]]$description, "for a")
  expect_null(out$columns[[2]]$description)
})

test_that("an unknown id in a dotted path is reported whole, with the declared ids", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$columns <- list(list(id = "Wt. (kg)", label = "W", type = "SAS Num"))

  expect_error(
    fn(ds, "dataset.columns.Wt.nope.values", "x"),
    regexp = "Unknown column id 'Wt.nope'", fixed = TRUE, class = "rlang_error"
  )
})

test_that("dataset_item_split_path() returns the item and the field path below it", {
  split <- app_fn("dataset_item_split_path")
  items <- list(list(id = "a"), list(id = "a.b"), list(id = "a.b.c"), list(id = "Wt. (kg)"))

  hit <- split(items, c("a", "b", "c", "label"), "column", "ds1")
  expect_identical(hit$idx, 3L)
  expect_identical(hit$rest, "label")
  hit <- split(items, c("a", "b", "label"), "column", "ds1")
  expect_identical(hit$idx, 2L)
  expect_identical(hit$rest, "label")
  hit <- split(items, c("a", "values_from", "exclude"), "column", "ds1")
  expect_identical(hit$idx, 1L)
  expect_identical(hit$rest, c("values_from", "exclude"))
  hit <- split(items, c("Wt", " (kg)", "description"), "column", "ds1")
  expect_identical(hit$idx, 4L)
  expect_identical(hit$rest, "description")
  # An id may not eat the whole path: at least one field must remain, so the
  # only reading of `a.b` here is column `a`'s field `b`, and `a` is not
  # declared. The declared id `a.b` with no field behind it is said to be one,
  # not reported as unknown.
  err <- tryCatch(split(list(list(id = "a.b")), c("a", "b"), "column", "ds1"), error = function(e) e)
  expect_s3_class(err, "dta_template_path_no_field")
  expect_match(gsub("\\s+", " ", conditionMessage(err)), "'a.b' is a column id of dataset 'ds1'", fixed = TRUE)
  expect_error(
    split(list(list(id = "z")), c("a", "b"), "column", "ds1"),
    regexp = "Unknown column id 'a' in dataset 'ds1'", fixed = TRUE, class = "rlang_error"
  )
  # A column id must be followed by a column FIELD: `zzz` is none, so neither the
  # id `a.b` nor the id `a` qualifies; the longest declared id is named.
  err <- tryCatch(split(items, c("a", "b", "zzz"), "column", "ds1"), error = function(e) e)
  expect_s3_class(err, "dta_template_path_no_field")
  expect_match(
    gsub("\\s+", " ", conditionMessage(err)),
    "'a.b' is a column id of dataset 'ds1', but 'zzz' is not one of its fields",
    fixed = TRUE
  )
  # and the whole path being a declared id is the same refusal
  err <- tryCatch(split(items, c("a", "b", "c"), "column", "ds1"), error = function(e) e)
  expect_s3_class(err, "dta_template_path_no_field")
  # Only columns and row labels have fields to test; a rule's field path is free.
  # The one-segment id wins when it is declared (what every earlier release
  # read) ...
  rules <- list(list(id = "a"), list(id = "a.b"))
  hit <- split(rules, c("a", "b", "c"), "rule", "ds1")
  expect_identical(hit$idx, 1L)
  expect_identical(hit$rest, c("b", "c"))
  # ... and a dotted rule id is reached when its first segment is not declared.
  hit <- split(list(list(id = "a.b"), list(id = "z")), c("a", "b", "c"), "rule", "ds1")
  expect_identical(hit$idx, 1L)
  expect_identical(hit$rest, "c")
})

test_that("a column path whose segment after the id is not a column field is refused whole", {
  # With columns `Wt` and `Wt. (kg)` both declared, `columns.Wt. (kg)` read as
  # the field " (kg)" of the column `Wt` and was written into it, and the build
  # died later on an unused argument. It is a path with no field: refuse it, and
  # say so -- a declared id is not an UNKNOWN id.
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$columns <- list(
    list(id = "Wt", label = "W", type = "SAS Char"),
    list(id = "Wt. (kg)", label = "WK", type = "SAS Num")
  )

  # the whole path is a declared id, with no field behind it
  err <- tryCatch(fn(ds, "dataset.columns.Wt. (kg)", "L"), error = function(e) e)
  expect_s3_class(err, "dta_template_path_no_field")
  expect_s3_class(err, "rlang_error")
  expect_match(gsub("\\s+", " ", conditionMessage(err)), "'Wt. (kg)' is a column id", fixed = TRUE)
  expect_false(grepl("Unknown", conditionMessage(err), fixed = TRUE))
  # a declared id followed by something that is not one of its fields
  err <- tryCatch(fn(ds, "dataset.columns.Wt.zzz", "L"), error = function(e) e)
  expect_s3_class(err, "dta_template_path_no_field")
  expect_match(gsub("\\s+", " ", conditionMessage(err)), "'zzz' is not one of its fields", fixed = TRUE)
  # no declared id at all stays the plain unknown-id error, without that class
  err <- tryCatch(fn(ds, "dataset.columns.Nope.label", "L"), error = function(e) e)
  expect_s3_class(err, "rlang_error")
  expect_false(inherits(err, "dta_template_path_no_field"))
  expect_match(gsub("\\s+", " ", conditionMessage(err)), "Unknown column id 'Nope'", fixed = TRUE)
  # a real field of either column still goes where it belongs
  out <- fn(ds, "dataset.columns.Wt. (kg).label", "new")
  expect_identical(out$columns[[2]]$label, "new")
  expect_identical(out$columns[[1]]$label, "W")
  out <- fn(ds, "dataset.columns.Wt.label", "other")
  expect_identical(out$columns[[1]]$label, "other")
  expect_identical(out$columns[[2]]$label, "WK")
})

test_that("an option target with no column field is reported by validate_template, not lost", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "templates"))
  write_template <- function(target) {
    writeLines(
      c(
        "kind: dta_dataset_template", "id: weights", "version: \"1.0\"",
        "options:",
        sprintf("  - {id: o1, label: L, type: text, target: '%s', default: x}", target),
        "dataset:", "  id: inl", "  type: tabular", "  files: {filename: inl.csv, type: csv}",
        "  columns:",
        "    - {id: Wt, type: SAS Char, length: 3, nullable: Yes}",
        "    - {id: Wt. (kg), type: SAS Num, nullable: Yes}"
      ),
      file.path(dir, "templates", "weights.dta-dataset-template.yaml")
    )
  }

  write_template("dataset.columns.Wt. (kg)")
  res <- validate_template(dir)
  expect_identical(res$code[res$severity != "info"], "target_invalid")

  write_template("dataset.columns.Wt. (kg).description")
  expect_identical(nrow(validate_template(dir)), 0L)
})

test_that("a rule id and a row-label id that contain dots are addressed whole", {
  fn <- app_fn("apply_dataset_template_path")
  ds <- minimal_dataset_spec()
  ds$rules <- list(
    list(id = "plain", description = "plain", type = "col_condition"),
    list(id = "r.1", description = "dotted", type = "col_condition")
  )
  out <- fn(ds, "dataset.rules.r.1.description", "changed")
  expect_identical(out$rules[[2]]$description, "changed")
  expect_identical(out$rules[[1]]$description, "plain")

  # With a rule `chk` AND a rule `chk.description` declared, `rules.chk.description`
  # is the field `description` of `chk` -- as in every earlier release -- and the
  # dotted rule can only be named as a whole.
  ds$rules <- list(
    list(id = "chk", description = "chk text", type = "col_condition"),
    list(id = "chk.description", description = "sibling text", type = "col_condition")
  )
  out <- fn(ds, "dataset.rules.chk.description", "changed")
  expect_identical(out$rules[[1]]$description, "changed")
  expect_identical(out$rules[[2]]$description, "sibling text")

  mx <- list(
    id = "counts", type = "matrix", files = list(filename = "c.tsv", type = "tsv"),
    row_labels = list(
      list(id = "GENE", header = "gene", type = "SAS Char"),
      list(id = "GENE.ID", header = "gene_id", type = "SAS Char")
    )
  )
  out <- fn(mx, "dataset.row_labels.GENE.ID.nullable", FALSE)
  expect_false(out$row_labels[[2]]$nullable)
  expect_null(out$row_labels[[1]]$nullable)
  expect_error(
    fn(mx, "dataset.row_labels.NOPE.x.nullable", FALSE),
    regexp = "Unknown row label id 'NOPE.x'", fixed = TRUE, class = "rlang_error"
  )
})

test_that("a rules path writes the rule its FIRST segment names, through a patch and an option", {
  # With rules `chk` and `chk.params` declared, `rules.chk.params.min` is the
  # field `params.min` of the rule `chk` -- the reading of every earlier release
  # -- and not the field `min` of the rule `chk.params`. Taking the longest
  # declared id first would write into the sibling and leave `chk` unchanged.
  rule_list <- function() {
    list(
      list(id = "chk", description = "chk text", params = list(min = 1)),
      list(id = "chk.params", description = "sibling text", min = 100)
    )
  }

  patch <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()
  ds$rules <- rule_list()
  res <- patch(ds, list(set = list(
    "rules.chk.description" = "changed",
    "rules.chk.params.min" = 5
  )))
  rules <- res$dataset$rules
  expect_identical(rules[[1]]$description, "changed")
  expect_identical(rules[[1]]$params$min, 5)
  expect_identical(rules[[2]]$description, "sibling text")
  expect_equal(rules[[2]]$min, 100)

  # the same path as an option target, written by build_dataset_from_template()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template", "id: chk_rules", 'version: "1.0"',
      "options:",
      "  - id: minimum",
      "    type: text",
      "    target: dataset.rules.chk.params.min",
      "    default: seven",
      "dataset:",
      "  id: ds1",
      "  type: tabular",
      "  files: { filename: a.csv, type: csv }",
      "  columns:",
      "    - { id: X, label: X, type: SAS Char }",
      "  rules:",
      "    - { id: chk, type: col_unique, columns: [X], description: chk text, params: { min: 1 } }",
      "    - { id: chk.params, type: col_unique, columns: [X], description: sibling text, min: 100 }"
    ),
    path
  )
  def <- app_fn("read_dataset_template")(path)$value
  built <- app_fn("build_dataset_from_template")(def)
  expect_true(built$ok, info = built$error)
  rules <- built$value$dataset$rules
  expect_identical(rules[[1]]$params$min, "seven")
  expect_equal(rules[[2]]$min, 100)
  expect_identical(rules[[2]]$description, "sibling text")
})

test_that("a patch adds, modifies and sets columns whose ids contain spaces and dots", {
  patch <- app_fn("apply_dataset_patch")
  ds <- minimal_dataset_spec()
  ds$columns <- list(
    list(id = "Subject ID", label = "Subject", type = "SAS Char"),
    list(id = "Wt. (kg)", label = "Weight", type = "SAS Num")
  )

  res <- patch(ds, list(
    add_columns = list(list(id = "New Col (x)", label = "New", type = "SAS Char")),
    modify_columns = list(list(id = "Wt. (kg)", label = "Body weight")),
    set = list(
      "columns.New Col (x).description" = "added by the patch",
      "columns.Wt. (kg).description" = "set by the patch"
    )
  ))

  cols <- res$dataset$columns
  expect_identical(col_ids(res$dataset), c("Subject ID", "Wt. (kg)", "New Col (x)"))
  expect_identical(cols[[2]]$label, "Body weight")
  expect_identical(cols[[2]]$description, "set by the patch")
  expect_identical(cols[[3]]$description, "added by the patch")
  expect_null(cols[[1]]$description)
  # a column id the patch removes is matched exactly, spaces and all
  res <- patch(ds, list(remove_columns = "Subject ID"))
  expect_identical(col_ids(res$dataset), "Wt. (kg)")
})

test_that("an option target and a set patch reach spaced and dotted columns through a build", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "template.yaml")
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: spaced",
      'version: "1.0"',
      "options:",
      "  - id: unit",
      "    type: text",
      '    target: "dataset.columns.Wt. (kg).values"',
      "    default: kg",
      "  - id: visit",
      "    type: text",
      '    target: "dataset.columns.Visit Date (UTC).description"',
      "    default: when",
      "dataset:",
      "  id: ds1",
      "  type: tabular",
      "  files: { filename: s.csv, type: csv }",
      "  columns:",
      "    - { id: Subject ID, label: Subject, type: SAS Char }",
      '    - { id: "Visit Date (UTC)", label: Visit, type: SAS Char }',
      '    - { id: "Wt. (kg)", label: Weight, type: SAS Char }',
      '    - { id: "a", label: A, type: SAS Char }',
      '    - { id: "a.b", label: AB, type: SAS Char }',
      "  rules: []"
    ),
    path
  )
  def <- app_fn("read_dataset_template")(path)$value

  res <- app_fn("build_dataset_from_template")(
    def,
    selections = list(unit = "lb"),
    patch = list(set = list("columns.a.b.description" = "the dotted one"))
  )

  expect_true(res$ok, info = res$error)
  cols <- res$value$dataset$columns
  expect_identical(
    vapply(cols, function(x) x$id, character(1)),
    c("Subject ID", "Visit Date (UTC)", "Wt. (kg)", "a", "a.b")
  )
  expect_identical(cols[[3]]$values, "lb")
  expect_identical(cols[[2]]$description, "when")
  expect_identical(cols[[5]]$description, "the dotted one")
  expect_null(cols[[4]]$description)
  # and the result is a valid dataset, every id verbatim
  ds_obj <- DTAtools::dta_dataset_from_list(res$value$dataset)
  expect_identical(
    names(ds_obj@specs@columns),
    c("Subject ID", "Visit Date (UTC)", "Wt. (kg)", "a", "a.b")
  )
})
