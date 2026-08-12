# Coverage for inst/shiny/dta_app/R/template_core.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why this
# is necessary: the app's helper files are auto-sourced by Shiny at launch and
# are not part of the package namespace, and for how app_env() binds a
# caller-visible system.file() into the sourced environment so unqualified
# system.file() calls inside the app helpers resolve bundled assets correctly
# under both devtools::test() and R CMD check).

# ---- dta_creation_templates_dir / list_dta_creation_templates / get_dta_creation_template_path ----

test_that("dta_creation_templates_dir() resolves to a directory that exists", {
  fn <- app_fn("dta_creation_templates_dir")
  dir <- fn()

  expect_true(nzchar(dir))
  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, "biomarker_gf.dta-template.yaml")))
})

test_that("list_dta_creation_templates() finds the real bundled template", {
  fn <- app_fn("list_dta_creation_templates")
  result <- fn()

  expect_true("biomarker_gf.dta-template.yaml" %in% result)
})

test_that("get_dta_creation_template_path() resolves the real bundled template to an existing file", {
  fn <- app_fn("get_dta_creation_template_path")
  result <- fn("biomarker_gf.dta-template.yaml")

  expect_true(file.exists(result))
  expect_equal(basename(result), "biomarker_gf.dta-template.yaml")
  expect_equal(
    normalizePath(result, winslash = "/"),
    normalizePath(
      system.file("extdata", "templates", "biomarker_gf.dta-template.yaml", package = "DTAtools"),
      winslash = "/"
    )
  )
})

test_that("get_dta_creation_template_path() returns NULL without throwing for no/empty name", {
  fn <- app_fn("get_dta_creation_template_path")
  expect_null(fn(NULL))
  expect_null(fn(""))
})

test_that("get_dta_creation_template_path() returns NULL for an unknown template name", {
  fn <- app_fn("get_dta_creation_template_path")
  expect_null(fn("totally-not-a-real-template.dta-template.yaml"))
})

# ---- read_dta_creation_template --------------------------------------------

test_that("read_dta_creation_template() parses the real bundled template", {
  real_path <- system.file(
    "extdata", "templates", "biomarker_gf.dta-template.yaml",
    package = "DTAtools"
  )
  expect_true(nzchar(real_path))

  fn <- app_fn("read_dta_creation_template")
  res <- fn(real_path)

  expect_true(res$ok)
  expect_null(res$error)
  expect_equal(res$value$id, "biomarker_gf")
  expect_equal(res$value$label, "Biomarker GF DTS")
  expect_length(res$value$datasets, 1)
  expect_equal(res$value$datasets[[1]]$name, "gf_data_specs_pattern")
  expect_length(res$value$options, 12)
})

test_that("read_dta_creation_template() fails when 'kind' is missing", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(
    c(
      "base:",
      "  metadata: {}",
      "datasets:",
      "  - name: x",
      "    type: file",
      "    files: {filename: a.csv, type: csv}"
    ),
    tmp
  )

  fn <- app_fn("read_dta_creation_template")
  res <- fn(tmp)

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(nzchar(res$error))
  # App's own hardcoded string, not a base R/yaml message -- fine to assert.
  expect_equal(res$error, "Template 'kind' must be 'dta_creation_template'.")
})

test_that("read_dta_creation_template() fails when 'base' is missing", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(
    c(
      "kind: dta_creation_template",
      "datasets:",
      "  - name: x",
      "    type: file",
      "    files: {filename: a.csv, type: csv}"
    ),
    tmp
  )

  fn <- app_fn("read_dta_creation_template")
  res <- fn(tmp)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_equal(res$error, "Template must contain a 'base' section.")
})

test_that("read_dta_creation_template() fails on syntactically malformed YAML", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(c("kind: dta_creation_template", "base: {", "  bad: ["), tmp)

  fn <- app_fn("read_dta_creation_template")
  res <- fn(tmp)

  expect_false(res$ok)
  expect_null(res$value)
  # yaml package error, translated under a German locale -- do not assert text.
  expect_true(nzchar(res$error))
})

test_that("read_dta_creation_template() fails for a nonexistent path", {
  fn <- app_fn("read_dta_creation_template")
  res <- fn(file.path(tempdir(), "no-such-template-xyz-123.yaml"))

  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_equal(res$error, "Template file not found.")
})

# ---- dta_template_metadata_fields ------------------------------------------

test_that("dta_template_metadata_fields() lists the exact allowed DTAMetaData fields", {
  fn <- app_fn("dta_template_metadata_fields")
  expect_equal(
    fn(),
    c(
      "title", "version", "date", "header",
      "version_history", "receiver", "supplier", "transmission",
      "error_handling", "authorized_for_corrections"
    )
  )
})

# ---- resolve_template_dataset_path -----------------------------------------

test_that("resolve_template_dataset_path() resolves an absolute path directly", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ds_file <- file.path(tmp_dir, "dataset.yaml")
  writeLines("name: x", ds_file)

  fn <- app_fn("resolve_template_dataset_path")
  # template_path points somewhere else entirely -- the absolute ref must win
  # without even consulting it.
  result <- fn(ds_file, "/some/unrelated/dir/template.yaml")

  expect_equal(result, normalizePath(ds_file, winslash = "/", mustWork = TRUE))
})

test_that("resolve_template_dataset_path() resolves a path relative to the template file", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ds_file <- file.path(tmp_dir, "dataset.yaml")
  writeLines("name: x", ds_file)
  template_path <- file.path(tmp_dir, "template.yaml") # need not exist

  fn <- app_fn("resolve_template_dataset_path")
  result <- fn("dataset.yaml", template_path)

  expect_equal(result, normalizePath(ds_file, winslash = "/", mustWork = TRUE))
})

test_that("resolve_template_dataset_path() falls back to the package extdata root", {
  # Neither absolute nor next to the (nonexistent) template file -- only
  # reachable via the third branch's system.file("extdata", ref) fallback.
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  template_path <- file.path(tmp_dir, "template.yaml")

  fn <- app_fn("resolve_template_dataset_path")
  result <- fn("gf_dataset.yaml", template_path)

  expect_equal(
    result,
    normalizePath(
      system.file("extdata", "gf_dataset.yaml", package = "DTAtools"),
      winslash = "/", mustWork = TRUE
    )
  )
})

test_that("resolve_template_dataset_path() returns '' when the reference resolves nowhere", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  template_path <- file.path(tmp_dir, "template.yaml")

  fn <- app_fn("resolve_template_dataset_path")
  expect_equal(fn("does-not-exist-anywhere.yaml", template_path), "")
  expect_equal(fn("", template_path), "")
})

# ---- dta_template_choices / default / allow_custom -------------------------

test_that("dta_template_choices() expands a plain character vector of choices", {
  fn <- app_fn("dta_template_choices")
  expect_equal(fn(list(choices = c("yes", "no"))), c(yes = "yes", no = "no"))
})

test_that("dta_template_choices() expands value/label list entries", {
  fn <- app_fn("dta_template_choices")
  opt <- list(choices = list(
    list(value = "a", label = "Alpha"),
    list(value = "b", label = "Beta")
  ))
  expect_equal(fn(opt), c(Alpha = "a", Beta = "b"))
})

test_that("dta_template_choices() returns an empty named vector when choices is absent", {
  fn <- app_fn("dta_template_choices")
  result <- fn(list())
  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("dta_template_default() returns the first default value, or NULL when absent", {
  fn <- app_fn("dta_template_default")
  expect_equal(fn(list(default = "foo")), "foo")
  expect_null(fn(list()))
})

test_that("dta_template_allow_custom() reads the field and falls back to the default arg", {
  fn <- app_fn("dta_template_allow_custom")
  expect_true(fn(list(allow_custom = TRUE)))
  expect_false(fn(list(allow_custom = FALSE)))
  # field absent -> function's own default (FALSE) unless overridden
  expect_false(fn(list()))
  expect_true(fn(list(), default = TRUE))
})

# ---- list_set_path -----------------------------------------------------

test_that("list_set_path() sets a two-level path into an empty list", {
  fn <- app_fn("list_set_path")
  expect_equal(fn(list(), c("a", "b"), 1), list(a = list(b = 1)))
})

test_that("list_set_path() overwrites an existing leaf and leaves siblings untouched", {
  fn <- app_fn("list_set_path")
  result <- fn(list(a = list(b = 2, c = 3)), c("a", "b"), 99)
  expect_equal(result, list(a = list(b = 99, c = 3)))
})

test_that("list_set_path() creates missing intermediate levels and keeps unrelated keys", {
  fn <- app_fn("list_set_path")
  result <- fn(list(existing = "keep"), c("p", "q", "r"), 5)
  expect_equal(result, list(existing = "keep", p = list(q = list(r = 5))))
})

test_that("list_set_path() handles a single-element key path", {
  fn <- app_fn("list_set_path")
  expect_equal(fn(list(a = 1), "b", 2), list(a = 1, b = 2))
  expect_equal(fn(list(a = 1), "a", 99), list(a = 99))
})

test_that("list_set_path() builds a deep nested path", {
  fn <- app_fn("list_set_path")
  result <- fn(list(), c("a", "b", "c", "d"), "deep")
  expect_equal(result, list(a = list(b = list(c = list(d = "deep")))))
})

test_that("list_set_path() removes the key when value is NULL", {
  fn <- app_fn("list_set_path")
  result <- fn(list(a = 1, b = 2), "a", NULL)
  expect_equal(result, list(b = 2))
})

# ---- collect_option_effects -------------------------------------------

test_that("collect_option_effects() returns the effects for the selected value", {
  fn <- app_fn("collect_option_effects")
  opt <- list(effects = list(yes = list(list(path = "metadata.title", value = "A"))))
  expect_equal(
    fn(opt, "yes"),
    list(list(path = "metadata.title", value = "A"))
  )
})

test_that("collect_option_effects() returns no effects for a selected value with none defined", {
  fn <- app_fn("collect_option_effects")
  opt <- list(effects = list(yes = list(list(path = "metadata.title", value = "A"))))
  expect_equal(fn(opt, "unknown"), list())
})

test_that("collect_option_effects() returns no effects when the option declares none", {
  fn <- app_fn("collect_option_effects")
  expect_equal(fn(list(), "yes"), list())
})

test_that("collect_option_effects() falls back to __selection__ for values outside the choice list", {
  fn <- app_fn("collect_option_effects")
  opt <- list(effects = list(
    `__selection__` = list(list(path = "metadata.title", value = "__selection__"))
  ))
  expect_equal(
    fn(opt, "My Custom Title"),
    list(list(path = "metadata.title", value = "__selection__"))
  )
})

test_that("collect_option_effects() appends effects_all to the per-value effects", {
  fn <- app_fn("collect_option_effects")
  opt <- list(
    effects = list(yes = list(list(path = "a", value = 1))),
    effects_all = list(list(path = "b", value = 2))
  )
  expect_equal(
    fn(opt, "yes"),
    list(list(path = "a", value = 1), list(path = "b", value = 2))
  )
})

# ---- apply_template_metadata_path --------------------------------------

test_that("apply_template_metadata_path() sets a scalar metadata field", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- app_fixture_dta()

  updated <- fn(dta, "metadata.title", "New Title")

  expect_equal(metadata(updated)@title, "New Title")
})

test_that("apply_template_metadata_path() parses and sets a date field", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- app_fixture_dta()

  updated <- fn(dta, "metadata.date", "2026-01-01")

  expect_equal(metadata(updated)@date, as.Date("2026-01-01"))
})

test_that("apply_template_metadata_path() rejects an unparseable date value", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- app_fixture_dta()

  # App's own hardcoded message -- fine to assert.
  expect_error(fn(dta, "metadata.date", "not-a-date"), "Invalid date value")
})

test_that("apply_template_metadata_path() sets a nested field under a list-valued top-level property", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- app_fixture_dta()

  updated <- fn(dta, "metadata.transmission.type", "Secure S3 bucket")

  expect_equal(metadata(updated)@transmission$type, "Secure S3 bucket")
})

test_that("apply_template_metadata_path() rejects a path that does not start with 'metadata'", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- app_fixture_dta()

  expect_error(fn(dta, "notmetadata.foo", 1), "Unsupported effect path")
})

test_that("apply_template_metadata_path() rejects an unsupported metadata top-level field", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- app_fixture_dta()

  expect_error(fn(dta, "metadata.bogus_field", 1), "Unsupported metadata top-level field")
})

# ---- create_dta_from_template -------------------------------------------

test_that("create_dta_from_template() builds a DTA from the real bundled template", {
  real_path <- system.file(
    "extdata", "templates", "biomarker_gf.dta-template.yaml",
    package = "DTAtools"
  )
  read_fn <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")

  parsed <- read_fn(real_path)
  expect_true(parsed$ok)

  selections <- list(
    title = "GENOMIC FINDINGS (GF) DATA TRANSFER SPECIFICATIONS",
    transmission_notification = "none"
  )
  result <- create_fn(parsed$value, real_path, selections = selections)

  expect_true(result$ok)
  expect_null(result$error)
  expect_equal(names(result$value@datasets), "gf_data_specs_pattern")

  md <- metadata(result$value)
  # selection override applied
  expect_equal(md@title, "GENOMIC FINDINGS (GF) DATA TRANSFER SPECIFICATIONS")
  expect_equal(md@transmission$notification, "none")
  # base default left untouched by selections
  expect_equal(md@header, "Test Company 1")
})

test_that("create_dta_from_template() accepts a plain string dataset reference", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("name: mini_ds", "type: file", "files:", "  filename: mini.csv", "  type: csv"),
    file.path(tmp_dir, "dataset.yaml")
  )
  template_path <- file.path(tmp_dir, "template.yaml") # need not exist on disk

  template_def <- list(base = list(), datasets = list("dataset.yaml"))
  create_fn <- app_fn("create_dta_from_template")

  result <- create_fn(template_def, template_path, selections = list())

  expect_true(result$ok)
  expect_equal(names(result$value@datasets), "mini_ds")
})

test_that("create_dta_from_template() accepts a {source: ...} dataset reference", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("name: mini_ds", "type: file", "files:", "  filename: mini.csv", "  type: csv"),
    file.path(tmp_dir, "dataset.yaml")
  )
  template_path <- file.path(tmp_dir, "template.yaml")

  template_def <- list(base = list(), datasets = list(list(source = "dataset.yaml")))
  create_fn <- app_fn("create_dta_from_template")

  result <- create_fn(template_def, template_path, selections = list())

  expect_true(result$ok)
  expect_equal(names(result$value@datasets), "mini_ds")
})

test_that("create_dta_from_template() accepts an inline dataset definition", {
  template_path <- file.path(tempdir(), "template.yaml")

  template_def <- list(
    base = list(),
    datasets = list(list(
      name = "inline_ds",
      type = "file",
      files = list(filename = "mini2.csv", type = "csv")
    ))
  )
  create_fn <- app_fn("create_dta_from_template")

  result <- create_fn(template_def, template_path, selections = list())

  expect_true(result$ok)
  expect_equal(names(result$value@datasets), "inline_ds")
})

test_that("create_dta_from_template() fails without throwing when a dataset reference cannot be resolved", {
  template_path <- file.path(tempdir(), "template.yaml")
  template_def <- list(base = list(), datasets = list("does-not-exist.yaml"))
  create_fn <- app_fn("create_dta_from_template")

  result <- create_fn(template_def, template_path, selections = list())

  expect_false(result$ok)
  expect_null(result$value)
  expect_true(nzchar(result$error))
  expect_match(result$error, "Could not resolve dataset source", fixed = TRUE)
})

test_that("create_dta_from_template() rejects an unknown base.metadata field without throwing", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("name: mini_ds", "type: file", "files:", "  filename: mini.csv", "  type: csv"),
    file.path(tmp_dir, "dataset.yaml")
  )
  template_path <- file.path(tmp_dir, "template.yaml")

  template_def <- list(
    base = list(metadata = list(bogus_field = "x")),
    datasets = list("dataset.yaml")
  )
  create_fn <- app_fn("create_dta_from_template")

  result <- create_fn(template_def, template_path, selections = list())

  expect_false(result$ok)
  expect_true(nzchar(result$error))
  expect_match(result$error, "Unknown base.metadata field", fixed = TRUE)
})
