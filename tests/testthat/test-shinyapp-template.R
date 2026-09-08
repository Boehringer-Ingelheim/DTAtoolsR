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
  # CHANGED (biomarker_gf refactor, see gf_smrnaseq.dta-dataset-template.yaml):
  # the ~300-line inline dataset was extracted into a reusable dataset
  # template, so this entry is now a `{template:, as:}` IMPORT, not an inline
  # dataset with its own `name:` field -- res$value$datasets[[1]]$name would
  # be NULL post-refactor. See test-bundled-templates.R for full coverage of
  # what this import actually builds.
  expect_equal(res$value$datasets[[1]]$template, "gf_smrnaseq@3.0")
  expect_equal(res$value$datasets[[1]]$as, "gf_data_specs_pattern")
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

test_that("dta_template_metadata_fields() is derived from the S7 class, not mirrored", {
  fields <- app_fn("dta_template_metadata_fields")
  props <- app_fn("dta_metadata_properties")
  machine <- app_fn("dta_metadata_machine_fields")
  # The point of deriving: a new DTAMetaData property becomes settable from a
  # template automatically, instead of silently being rejected until someone
  # remembers to update a hand-written vector.
  expect_equal(fields(), setdiff(names(props()), machine()))
  # The machine-owned fields are the deliberate exception, and they are named in
  # ONE place rather than repeated here -- so this assertion keeps testing that
  # the set is derived, rather than re-mirroring the exclusion list it exists to
  # rule out.
  expect_true(all(machine() %in% names(props())))
  expect_false(any(machine() %in% fields()))
})

test_that("the machine-owned metadata fields are exactly import_issues and template", {
  machine <- app_fn("dta_metadata_machine_fields")
  # import_issues records how a file was read; template records which template
  # produced the document. A template that could set either could forge its own
  # provenance, which is what the rebase feature would then trust. Pinning the
  # exact set makes adding a third one a deliberate act.
  expect_setequal(machine(), c("import_issues", "template"))
})

test_that("dta_template_list_fields() names the container metadata fields", {
  list_fields <- app_fn("dta_template_list_fields")
  expect_setequal(
    list_fields(),
    c("version_history", "receiver", "supplier", "transmission")
  )
  # authorized_for_corrections is a union of character, list and NULL, and
  # defaults to NULL, so it must stay a scalar field. Getting this wrong turns a
  # scalar assignment into a nested-path merge.
  expect_false("authorized_for_corrections" %in% list_fields())

  # REGRESSION: this used to be decided by identical(prop$class, S7::class_list),
  # which compares S7 class OBJECTS by identity. That holds under
  # pkgload::load_all() but not once the class is restored from an installed
  # package, so devtools::test() passed while R CMD check failed with
  # "@transmission must be <list>, not <character>". The set must be non-empty
  # under both, which is what actually catches a regression here.
  expect_gt(length(list_fields()), 0)
})

test_that("a nested path into a scalar metadata field is rejected", {
  fn <- app_fn("apply_template_metadata_path")
  dta <- DTA(datasets = list(create_example_DTADataSetTabular(2)))
  expect_error(
    fn(dta, "metadata.title.nested", "x"),
    "holds a single value, not a nested path"
  )
})

# ---- template expressions ($ {today} / $ {version}) -------------------------

test_that("resolve_template_expressions() substitutes without losing names or types", {
  fn <- app_fn("resolve_template_expressions")
  out <- fn(
    list(version = "${version}", nested = list(date = "${today}", keep = 1L)),
    list("${version}" = "9.9", "${today}" = "2026-01-02")
  )
  expect_equal(out$version, "9.9")
  expect_equal(out$nested$date, "2026-01-02")
  # The lapply() recursion must not drop names or coerce non-character leaves.
  expect_equal(names(out), c("version", "nested"))
  expect_equal(out$nested$keep, 1L)
})

test_that("${version} reaches version_history after the options are applied", {
  # REGRESSION: apply_template_expressions() was written, documented and then
  # never called, so metadata.version took the chosen value while
  # version_history[[1]]$version kept the literal "${version}". Nothing caught
  # it because no test overrode version and then read the history back.
  create <- app_fn("create_dta_from_template")
  read_tpl <- app_fn("read_dta_creation_template")
  path <- app_fn("get_dta_creation_template_path")("biomarker_gf.dta-template.yaml")
  expect_false(is.null(path))

  def <- read_tpl(path)
  expect_true(def$ok)

  res <- create(def$value, path, list(version = "3.7"))
  expect_true(res$ok)
  md <- DTAtools::metadata(res$value)
  expect_equal(S7::prop(md, "version"), "3.7")

  hist <- S7::prop(md, "version_history")
  expect_gt(length(hist), 0)
  expect_equal(hist[[1]]$version, "3.7")
})

test_that("${today} resolves to the creation date, not a literal token", {
  create <- app_fn("create_dta_from_template")
  read_tpl <- app_fn("read_dta_creation_template")
  path <- app_fn("get_dta_creation_template_path")("biomarker_gf.dta-template.yaml")
  expect_false(is.null(path))

  res <- create(read_tpl(path)$value, path, list())
  expect_true(res$ok)
  md <- DTAtools::metadata(res$value)
  expect_s3_class(S7::prop(md, "date"), "Date")
  expect_equal(S7::prop(md, "date"), Sys.Date())
  expect_equal(
    S7::prop(md, "version_history")[[1]]$date,
    format(Sys.Date(), "%Y-%m-%d")
  )
})

# ---- target: shorthand and inherited defaults -------------------------------

test_that("target: is shorthand for a __selection__ effect", {
  fn <- app_fn("collect_option_effects")
  eff <- fn(list(id = "x", target = "metadata.title"), "New Title")
  expect_length(eff, 1)
  expect_equal(eff[[1]]$path, "metadata.title")
  expect_equal(eff[[1]]$value, "__selection__")
})

test_that("an explicit effects block wins over target:", {
  fn <- app_fn("collect_option_effects")
  opt <- list(
    id = "x",
    target = "metadata.title",
    effects = list(
      "__selection__" = list(list(path = "metadata.header", value = "__selection__"))
    )
  )
  eff <- fn(opt, "v")
  expect_length(eff, 1)
  expect_equal(eff[[1]]$path, "metadata.header")
})

test_that("an option with no default inherits it from base.metadata", {
  fn <- app_fn("dta_template_default")
  base <- list(title = "From Base", transmission = list(test_upload = TRUE))
  expect_equal(fn(list(id = "t", target = "metadata.title"), base), "From Base")
  expect_true(fn(list(id = "u", target = "metadata.transmission.test_upload"), base))
  # An explicit default still wins over the base value.
  expect_equal(
    fn(list(id = "t", target = "metadata.title", default = "Own"), base),
    "Own"
  )
  # A target with nothing at that path inherits nothing.
  expect_null(fn(list(id = "z", target = "metadata.header"), base))
})

test_that("effect_key_candidates() covers both YAML spellings of a boolean", {
  fn <- app_fn("effect_key_candidates")
  # An unquoted `yes:` key becomes the name "TRUE"; a quoted one stays "yes".
  expect_true(all(c("TRUE", "yes") %in% fn(TRUE)))
  expect_true(all(c("FALSE", "no") %in% fn(FALSE)))
  expect_equal(fn("plain"), "plain")
  expect_length(fn(NULL), 0)
  expect_length(fn(""), 0)
})

# ---- template search path ---------------------------------------------------

test_that("dta_creation_template_dirs() puts DTAtools.template_dir first", {
  fn <- app_fn("dta_creation_template_dirs")
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  old <- options(DTAtools.template_dir = tmp)
  on.exit(options(old), add = TRUE)
  dirs <- fn()
  expect_equal(normalizePath(dirs[[1]]), normalizePath(tmp))
  expect_gt(length(dirs), 1)
})

test_that("get_dta_creation_template_path() prefers an earlier directory and cannot escape", {
  fn <- app_fn("get_dta_creation_template_path")
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  shadow <- file.path(tmp, "biomarker_gf.dta-template.yaml")
  writeLines("kind: dta_creation_template", shadow)

  old <- options(DTAtools.template_dir = tmp)
  on.exit(options(old), add = TRUE)
  expect_equal(
    normalizePath(fn("biomarker_gf.dta-template.yaml")),
    normalizePath(shadow)
  )
  # basename() guard: a traversal attempt resolves to a bare name, not a path.
  expect_null(fn("../../etc/passwd"))
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

test_that("dta_template_version_string() keeps a quoted version exactly as written", {
  fn <- app_fn("dta_template_version_string")
  # A quoted version is unambiguous, so it passes through untouched and
  # silently -- including the multi-part forms numeric_version understands.
  expect_no_warning(out <- fn("1.10"))
  expect_identical(out, "1.10")
  expect_identical(fn("2.5.1"), "2.5.1")
  expect_identical(fn("1.0"), "1.0")
})

test_that("dta_template_version_string() restores the trailing zero an unquoted version loses", {
  fn <- app_fn("dta_template_version_string")
  # yaml reads an unquoted `version: 1.0` as the double 1.0, and R renders that
  # as "1" -- so as.character() alone would silently turn version 1.0 into
  # version 1, which sorts and displays as a different release.
  expect_identical(as.character(1.0), "1")
  expect_warning(out <- fn(1.0, what = "demo"), regexp = "Quote it")
  expect_identical(out, "1.0")
})

test_that("dta_template_version_string() warns when handed an already-parsed number", {
  fn <- app_fn("dta_template_version_string")
  # By the time a value is a double, 1.10 and 1.1 really are the same object
  # and no coercion can separate them -- so this fallback path warns instead of
  # pretending it knew. The FIX is not to reach this path at all: read the
  # field from the file text (see the next test), which loses nothing.
  expect_identical(yaml::yaml.load("v: 1.10")$v, 1.1)
  expect_warning(out <- fn(yaml::yaml.load("v: 1.10")$v), regexp = "cannot be recovered")
  expect_identical(out, "1.1")
})

test_that("dta_template_yaml_handlers() preserve a number's source text exactly", {
  handlers <- app_fn("dta_template_yaml_handlers")()
  y <- "a: 1.0
b: 1.10
c: 1.9
d: 3
e: true
f: 1e3
"

  # The tag names are the whole trick. libyaml never invokes a handler
  # registered under the obvious name "float" -- probe it with that and the
  # feature looks impossible. The tags that fire are int, float#fix, float#exp.
  expect_setequal(names(handlers), c("int", "float#fix", "float#exp"))
  expect_null(yaml::yaml.load(y, handlers = list(float = as.character))$b |> attr("class"))
  expect_type(yaml::yaml.load(y, handlers = list(float = as.character))$b, "double")

  out <- yaml::yaml.load(y, handlers = handlers)
  expect_identical(out$a, "1.0")
  expect_identical(out$b, "1.10")
  expect_identical(out$c, "1.9")
  expect_identical(out$d, "3")
  expect_identical(out$f, "1e3")
  # Booleans are left alone, so these handlers are safe on a whole document.
  expect_true(is.logical(out$e))
})

test_that("dta_template_read_field_exact() reads a version verbatim from the file", {
  fn <- app_fn("dta_template_read_field_exact")
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("kind: dta_creation_template", "id: demo", "version: 1.10"), path)

  # 1.10 is a LATER release than 1.9; a plain parse collapses it to 1.1 and
  # would silently resolve the wrong file.
  expect_identical(fn(path, "version"), "1.10")
  expect_identical(fn(path, "id"), "demo")
  # A missing field and an unreadable file are the caller's problem to report
  # with a file name in hand, not an error thrown from inside a reader -- and
  # not a warning from inside one either. This assertion used to pass while the
  # reader raised base R's connection warning on its way to returning NA:
  # `tryCatch(error = )` does not intercept the warning channel, so a reader
  # documented as reporting by return value signalled anyway. Asserted as the
  # ABSENCE of a warning rather than by matching its text, which arrives in the
  # session language.
  expect_identical(fn(path, "nope"), NA_character_)
  expect_no_warning(
    expect_identical(fn(file.path(tempdir(), "no-such-file-xyz.yaml"), "version"), NA_character_)
  )
})

test_that("dta_template_read_yaml_quiet() reports failure by value, never by signal", {
  fn <- app_fn("dta_template_read_yaml_quiet")

  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("id: demo", "version: 1.10"), path)

  ok <- fn(path)
  expect_true(ok$ok)
  expect_identical(ok$value$id, "demo")
  expect_null(ok$error)

  # A path that does not exist, and one that exists but is not a readable file.
  # Both raise a connection warning before the error; neither may escape.
  for (bad in list(file.path(tempdir(), "no-such-file-xyz.yaml"), tempdir())) {
    res <- expect_no_warning(fn(bad))
    expect_false(res$ok)
    expect_null(res$value)
    # The message is kept for the caller to report with its own file name in
    # hand -- it is simply not signalled from here.
    expect_true(is.character(res$error) && nzchar(res$error))
  }
})

test_that("dta_template_read_yaml_quiet() still applies the version-preserving handlers", {
  fn <- app_fn("dta_template_read_yaml_quiet")
  handlers <- app_fn("dta_template_yaml_handlers")()

  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines("version: 1.10", path)

  # Muffling the warning channel must not disturb what the read produces:
  # 1.10 is a later release than 1.9 and a plain parse collapses it to 1.1.
  expect_identical(fn(path, handlers = handlers)$value$version, "1.10")
  expect_identical(fn(path)$value$version, 1.1)
})

test_that("dta_template_version_string() reports a missing version as NA, not an error", {
  fn <- app_fn("dta_template_version_string")
  # A file with no version is a validation problem for the caller to report
  # with its own file name in hand, not something to abort deep inside a
  # coercion helper that has no idea which file it is looking at.
  expect_identical(fn(NULL), NA_character_)
  expect_identical(fn(list()), NA_character_)
})

test_that("dta_template_version_is_exact() distinguishes a quoted version from a parsed number", {
  fn <- app_fn("dta_template_version_is_exact")
  # validate_template() uses this to fail a private template repository's CI on
  # an ambiguous version, without re-parsing the file or catching a warning.
  expect_true(fn("1.10"))
  expect_false(fn(1.1))
  expect_false(fn(NULL))
})

test_that("a template that extends another may omit base and datasets entirely", {
  fn <- app_fn("read_dta_creation_template")
  tmp <- withr::local_tempfile(fileext = ".yaml")
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: acme_deviation",
      'version: "1.0"',
      "extends: biomarker_gf@1.0",
      "options:",
      "  - id: header",
      "    type: text",
      "    target: metadata.header",
      "    default: ACME"
    ),
    tmp
  )

  res <- fn(tmp)

  # A deviation template that only overrides one option inherits its parent's
  # base and datasets. Requiring it to restate either would make the most
  # useful kind of child template impossible to write -- and there is no way
  # to write "inherit verbatim" otherwise: omitting `base:` used to fail this
  # check, while `base: {}` is an EXPLICITLY empty section that the inheritance
  # merge correctly reads as "replace the parent's with nothing", silently
  # wiping every field the parent set.
  expect_true(res$ok)
  expect_null(res$value$base)
  expect_length(res$value$datasets, 0)
  expect_equal(res$value$id, "acme_deviation")
})

test_that("a template that extends nothing still must declare base and datasets", {
  fn <- app_fn("read_dta_creation_template")
  tmp <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("kind: dta_creation_template", "id: orphan", 'version: "1.0"'), tmp)

  # The relaxation above is scoped to `extends:`. A root template with no base
  # has nothing to inherit one from, so the original error must still fire.
  res <- fn(tmp)
  expect_false(res$ok)
  expect_equal(res$error, "Template must contain a 'base' section.")
})

test_that("a non-list base is rejected whether or not the template extends", {
  fn <- app_fn("read_dta_creation_template")
  tmp <- withr::local_tempfile(fileext = ".yaml")
  writeLines(
    c(
      "kind: dta_creation_template", "id: bad", 'version: "1.0"',
      "extends: parent@1.0", "base: just a string"
    ),
    tmp
  )

  # Absent is now legal; malformed never was. Letting a scalar through here
  # would push the failure into the merge, far from the file that caused it.
  res <- fn(tmp)
  expect_false(res$ok)
  expect_equal(res$error, "Template 'base' must be a mapping/object.")
})

# ---- required ----------------------------------------------------------------

# `required:` is checked at INSTANTIATION rather than at merge, because an
# abstract parent is allowed to leave a required field unset -- being required
# of someone further down is the whole point. These build a real document to
# exercise that, since the check runs after every selection has been applied.

required_fixture <- function(dir) {
  writeLines(
    c("name: mini_ds", "type: file", "files:", "  filename: mini.csv", "  type: csv"),
    file.path(dir, "dataset.yaml")
  )
  list(
    path = file.path(dir, "template.yaml"), # need not exist on disk
    def = list(
      id = "req_tpl",
      base = list(),
      datasets = list("dataset.yaml"),
      required = "base.metadata.header",
      options = list(list(
        id = "header", label = "Header", type = "text", target = "metadata.header"
      ))
    )
  )
}

test_that("create_dta_from_template() refuses to build while a required field is unfilled", {
  create_fn <- app_fn("create_dta_from_template")
  fx <- required_fixture(withr::local_tempdir())

  result <- create_fn(fx$def, fx$path, selections = list())

  expect_false(result$ok)
  expect_match(result$error, "required field", fixed = TRUE)
  expect_match(result$error, "base.metadata.header", fixed = TRUE)
})

test_that("a required field the TEMPLATE fills is satisfied", {
  create_fn <- app_fn("create_dta_from_template")
  fx <- required_fixture(withr::local_tempdir())
  fx$def$base <- list(metadata = list(header = "Set by the template"))

  result <- create_fn(fx$def, fx$path, selections = list())

  expect_true(result$ok)
  expect_equal(DTAtools::metadata(result$value)@header, "Set by the template")
})

test_that("a required field only the USER fills is satisfied too", {
  # This is what the `base.` resolution rule buys, and the assertion that would
  # fail if `required:` were checked against the template definition instead of
  # the built document: the template leaves `base:` empty, and the requirement
  # is met purely by a selection made at creation time.
  create_fn <- app_fn("create_dta_from_template")
  fx <- required_fixture(withr::local_tempdir())

  result <- create_fn(fx$def, fx$path, selections = list(header = "Chosen by the user"))

  expect_true(result$ok)
  expect_equal(DTAtools::metadata(result$value)@header, "Chosen by the user")
})

test_that("a blank value does not satisfy a required field", {
  # "" is a legitimate present-but-blank value everywhere else in the four
  # states, but the point of `required:` is that somebody made a real choice.
  create_fn <- app_fn("create_dta_from_template")
  fx <- required_fixture(withr::local_tempdir())

  result <- create_fn(fx$def, fx$path, selections = list(header = ""))

  expect_false(result$ok)
  expect_match(result$error, "required field", fixed = TRUE)
})

test_that("a required path outside base. is resolved against the template itself", {
  # `options.…` and `datasets.…` have no counterpart in the built document, so
  # they are checked against the resolved definition rather than the DTA.
  create_fn <- app_fn("create_dta_from_template")
  fx <- required_fixture(withr::local_tempdir())
  fx$def$base <- list(metadata = list(header = "h"))
  fx$def$required <- "options.header.default"

  unset <- create_fn(fx$def, fx$path, selections = list())
  expect_false(unset$ok)
  expect_match(unset$error, "options.header.default", fixed = TRUE)

  fx$def$options[[1]]$default <- "A default"
  expect_true(create_fn(fx$def, fx$path, selections = list())$ok)
})

test_that("every unfilled required path is named at once, not one per run", {
  create_fn <- app_fn("create_dta_from_template")
  fx <- required_fixture(withr::local_tempdir())
  fx$def$required <- c("base.metadata.header", "options.header.default")

  result <- create_fn(fx$def, fx$path, selections = list())

  expect_false(result$ok)
  expect_match(result$error, "base.metadata.header", fixed = TRUE)
  expect_match(result$error, "options.header.default", fixed = TRUE)
})
