# Coverage for inst/shiny/dta_app/R/template_create.R -- the keystone that
# joins template_core.R, template_index.R, template_inherit.R, dataset_
# template.R and party_profiles.R into one creation path -- plus the extended
# create_dta_from_template() (template_core.R) that calls it. Reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why the
# app's helper files must be sourced into a private environment for testing).

Sys.setenv(NOT_CRAN = "true")

# Local copy of the isolation helper defined in test-shinyapp-template-
# sources.R -- deliberately duplicated rather than shared, per that file's own
# guidance, so this file does not depend on another test file's internals.
# Every DTATOOLS_TEMPLATE_* variable is cleared for the duration of each test:
# a developer machine with DTATOOLS_TEMPLATE_SOURCES exported would flip the
# app into private-only mode and fail unrelated tests confusingly.
local_clean_template_env <- function(..., .local_envir = parent.frame()) {
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = NA,
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA,
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA,
      DTATOOLS_TEMPLATE_GIT_TOKEN = NA,
      DTATOOLS_TEMPLATE_GIT_USER = NA,
      DTATOOLS_TEMPLATE_GIT_AUTH = NA,
      ...
    ),
    .local_envir = .local_envir
  )
  withr::local_options(
    list(DTAtools.template_dir = NULL),
    .local_envir = .local_envir
  )
}

# cli wraps a long condition message across lines at the console width, which
# breaks a naive substring/regexp match the moment the wrap lands inside the
# very text being asserted on. Collapsing whitespace before matching keeps the
# assertion about the MESSAGE CONTENT rather than about where cli happened to
# wrap it. Copied from test-shinyapp-template-inherit.R for the same reason
# local_clean_template_env() is copied rather than shared.
flat_message <- function(x) gsub("\\s+", " ", x)

# ---- Fixture family ----------------------------------------------------------
#
# One dataset template (gf_dt), one abstract parent creation template
# (base_ct) and one concrete child (child_ct) that `extends:` it, sharing the
# SAME `as: gf_from_template` identity on their dataset entry so
# dta_template_merge_datasets() (template_inherit.R) merges them into ONE
# entry instead of appending a second -- see the comment on
# dta_template_dataset_key() for why `as:` has to match for that to happen.
# Plus one party profile (supplier_acme) and two legacy standalone dataset
# YAMLs, all written into ONE root so every test in this file can share a
# single call to build_template_index() over it.
write_gf_dataset_template <- function(root) {
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: gf_dt",
      'version: "1.0"',
      "label: GF Dataset Template",
      "date: 2024-01-01",
      "options:",
      "  - id: vendor_name",
      "    label: Vendor name",
      "    type: text",
      "    target: dataset.columns.GFNAM.values",
      "dataset:",
      "  name: gf_data",
      "  type: tabular",
      "  files: { filename: gf.tsv, type: tsv }",
      "  columns:",
      "    - { id: STUDYID, label: Study ID, type: SAS Char }",
      "    - { id: GFNAM, label: Vendor Name, type: SAS Char }",
      "  rules: []"
    ),
    file.path(root, "gf_dt.dta-dataset-template.yaml")
  )
}

write_base_ct <- function(root) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: base_ct",
      'version: "1.0"',
      "label: Base CT",
      "abstract: true",
      "base:",
      "  metadata:",
      "    title: Base Title",
      '    version: "1.0"',
      "    header: Base Header",
      "party_slots:",
      "  - id: supplier_choice",
      "    target: metadata.supplier",
      "datasets:",
      "  - template: gf_dt@1.0",
      "    as: gf_from_template",
      "options: []"
    ),
    file.path(root, "base_ct.dta-template.yaml")
  )
}

write_child_ct <- function(root) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: child_ct",
      'version: "1.0"',
      "label: Child CT",
      "extends: base_ct",
      "abstract: false",
      # NOT `base: {}`: dta_template_merge_section() (template_inherit.R)
      # treats an EXPLICITLY EMPTY child mapping as "replace the parent's
      # section with nothing" (documented on that function), not "leave it
      # untouched" -- only an ABSENT `base:` key means that, and
      # read_dta_creation_template() requires `base:` to be present (a list)
      # on every file, extends or not. A child that wants the parent's
      # title/header to survive therefore has to give its OWN `base:` some
      # real, non-empty content -- here, a field the parent never set --
      # rather than an empty placeholder.
      "base:",
      "  metadata:",
      "    error_handling: Child-added handling note",
      "datasets:",
      "  - template: gf_dt@1.0",
      "    as: gf_from_template",
      "    options:",
      "      vendor_name: Acme Corp",
      "    patch:",
      "      remove_columns:",
      "        - STUDYID",
      "options:",
      "  - id: title_opt",
      "    label: Title",
      "    target: metadata.title"
    ),
    file.path(root, "child_ct.dta-template.yaml")
  )
}

write_supplier_profile <- function(root) {
  writeLines(
    c(
      "kind: dta_party_profile",
      "id: supplier_acme",
      'version: "1.0"',
      "role: supplier",
      "label: ACME Labs",
      "affiliation:",
      "  name: Party Supplier",
      "  country: DE"
    ),
    file.path(root, "supplier_acme.dta-party.yaml")
  )
}

write_legacy_dataset_files <- function(root) {
  writeLines(
    c("name: legacy_path_ds", "type: file", "files: { filename: a.csv, type: csv }"),
    file.path(root, "legacy_path_ds.yaml")
  )
  writeLines(
    c("name: legacy_source_ds", "type: file", "files: { filename: b.csv, type: csv }"),
    file.path(root, "legacy_source_ds.yaml")
  )
}

setup_fixture <- function(root) {
  write_gf_dataset_template(root)
  write_base_ct(root)
  write_child_ct(root)
  write_supplier_profile(root)
  write_legacy_dataset_files(root)
  invisible(root)
}

# Build the index over a fixture root, invalidating the memoised index first
# (per the task's own warning: a memoised index leaking between tests is the
# most likely source of flakiness here) and using build_template_index()
# directly rather than the cached accessor, so no test's index can leak into
# another's via the shared TTL cache.
index_over <- function(root) {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  app_fn("build_template_index")()
}

# A trivial inline dataset entry, the exact shape the pre-existing 55 tests
# already use for a minimal, always-buildable dataset.
mini_dataset_entry <- function() {
  list(name = "mini_ds", type = "file", files = list(filename = "mini.csv", type = "csv"))
}

# ---- template_dataset_entry_kind() ------------------------------------------

test_that("template_dataset_entry_kind() classifies all four accepted shapes", {
  fn <- app_fn("template_dataset_entry_kind")

  expect_equal(fn("gf_dataset.yaml"), "path")
  expect_equal(fn(list(source = "gf_dataset.yaml")), "source")
  expect_equal(fn(list(template = "gf@3.0")), "template")
  expect_equal(fn(list(name = "inline_ds", type = "file")), "inline")
  # An (currently hypothetical) entry naming both is resolved as "template",
  # checked first.
  expect_equal(fn(list(template = "gf@3.0", source = "x.yaml")), "template")
  # An empty character still decides "path" -- no nzchar() guard, matching
  # create_dta_from_template()'s original is.character() branch exactly.
  expect_equal(fn(""), "path")
})

# ---- template_index_resolver() ----------------------------------------------

test_that("template_index_resolver() resolves a ref through the index, and returns NULL for an unresolvable one", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  resolver <- app_fn("template_index_resolver")(idx)

  hit <- resolver("base_ct")
  expect_equal(hit$id, "base_ct")
  expect_equal(hit$version, "1.0")
  expect_true(is.list(hit$def))
  expect_equal(hit$def$id, "base_ct")

  expect_null(resolver("no-such-template"))
})

# ---- load_template_definition() ---------------------------------------------

test_that("load_template_definition() resolves 'id@version' and reports source metadata alongside the merged def", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("load_template_definition")
  res <- fn("child_ct@1.0", index = idx)

  expect_true(res$ok)
  expect_equal(res$value$id, "child_ct")
  expect_equal(res$value$version, "1.0")
  expect_equal(normalizePath(res$value$path), normalizePath(file.path(root, "child_ct.dta-template.yaml")))
  expect_true(nzchar(res$value$source_name))
})

test_that("load_template_definition() reports a clear, non-throwing error for an unresolvable reference", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("load_template_definition")
  res <- fn("does-not-exist", index = idx)

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(nzchar(res$error))
})

test_that("an abstract template cannot be instantiated directly", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("load_template_definition")
  res <- fn("base_ct", index = idx)

  expect_false(res$ok)
  expect_match(flat_message(res$error), "abstract")
})

test_that("extends: merges the child onto the parent, and lineage names the parent", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("load_template_definition")
  res <- fn("child_ct", index = idx)

  expect_true(res$ok)
  expect_equal(res$value$lineage, "base_ct@1.0")
  # Inherited from the parent, never restated by the child.
  expect_equal(res$value$def$base$metadata$header, "Base Header")
  expect_equal(res$value$def$base$metadata$title, "Base Title")
  # Added by the child, on top of the inherited fields above.
  expect_equal(res$value$def$base$metadata$error_handling, "Child-added handling note")
  # The child's own option is present alongside whatever the parent declared.
  expect_equal(vapply(res$value$def$options, function(o) o$id, character(1)), "title_opt")
  # The parent's party slot is inherited even though the child never
  # redeclares party_slots at all.
  expect_equal(vapply(res$value$def$party_slots, function(s) s$id, character(1)), "supplier_choice")
  # ONE merged dataset entry, not two -- proves the shared `as:` identity did
  # its job in dta_template_merge_datasets().
  expect_length(res$value$def$datasets, 1)
})

# ---- build_template_datasets(): the "template:" dataset entry --------------

test_that("a template: dataset entry builds the dataset, carries template_source/version/date, and a patch is applied AND recorded", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("child_ct", index = idx)
  expect_true(loaded$ok)

  build_ds <- app_fn("build_template_datasets")
  built <- build_ds(loaded$value$def, idx, list(), source_label = NULL, template_path = loaded$value$path)

  expect_length(built$datasets, 1)
  ds <- built$datasets[[1]]
  expect_s3_class(ds, "DTAtools::DTADataSet")
  expect_equal(ds@name, "gf_from_template")
  expect_equal(ds@template_source, "gf_dt")
  expect_equal(ds@template_version, "1.0")
  expect_equal(ds@template_date, "2024-01-01")
  # vendor_name option applied to the GFNAM column's values.
  expect_equal(ds@specs@columns$GFNAM@values, "Acme Corp")
  # STUDYID removed by the patch.
  expect_false("STUDYID" %in% names(ds@specs@columns))

  expect_length(built$provenance, 1)
  expect_equal(built$provenance[[1]]$template, "gf_dt")
  expect_equal(
    built$provenance[[1]]$deviations,
    list(list(op = "remove_columns", target = "STUDYID"))
  )
})

test_that("an unresolvable template: dataset ref errors naming the ref and the referencing template's id", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  def <- list(
    id = "bad_ref_ct", label = "Bad Ref CT",
    base = list(metadata = list(title = "T", version = "1.0")),
    datasets = list(list(template = "does_not_exist@9.9"))
  )
  create_fn <- app_fn("create_dta_from_template")
  res <- create_fn(def, file.path(root, "bad_ref_ct.dta-template.yaml"), selections = list(), index = idx)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
  flat <- flat_message(res$error)
  expect_match(flat, "does_not_exist@9.9", fixed = TRUE)
  expect_match(flat, "bad_ref_ct", fixed = TRUE)
})

test_that("build_template_datasets() reproduces the ORIGINAL 'could not resolve dataset source' text for a bad legacy path", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("build_template_datasets")
  def <- list(label = "x", datasets = list("does-not-exist.yaml"))

  expect_error(
    fn(def, idx, list(), template_path = file.path(root, "whatever.yaml")),
    regexp = "Could not resolve dataset source 'does-not-exist.yaml' for template 'x'.",
    fixed = TRUE
  )
})

test_that("build_template_datasets() surfaces a malformed inline dataset definition", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("build_template_datasets")
  # `x[[1]]` must itself be list-shaped (not atomic) for dta_dataset_from_
  # list()'s own `x[[1]]$name` probe to return NULL rather than raising a raw
  # "$ operator is invalid for atomic vectors" -- a nested empty list is the
  # simplest value that is safely list-shaped while still lacking `name`.
  def <- list(label = "y", datasets = list(list(columns = list())))

  expect_error(
    fn(def, idx, list(), template_path = file.path(root, "whatever.yaml")),
    regexp = "must contain a 'name' field"
  )
})

# ---- All three legacy `datasets:` forms are unaffected ----------------------

test_that("all three legacy datasets: forms build the identical DTA whether or not an index is supplied", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  def_legacy <- list(
    base = list(metadata = list(title = "Legacy", version = "1.0")),
    label = "legacy",
    datasets = list(
      "legacy_path_ds.yaml",
      list(source = "legacy_source_ds.yaml"),
      list(name = "legacy_inline_ds", type = "file", files = list(filename = "inline.csv", type = "csv"))
    )
  )
  template_path <- file.path(root, "legacy_ct.dta-template.yaml") # need not exist on disk

  create_fn <- app_fn("create_dta_from_template")
  res_no_index <- create_fn(def_legacy, template_path, selections = list())
  res_with_index <- create_fn(def_legacy, template_path, selections = list(), index = idx)

  expect_true(res_no_index$ok)
  expect_true(res_with_index$ok)
  expect_setequal(
    names(res_no_index$value@datasets),
    c("legacy_path_ds", "legacy_source_ds", "legacy_inline_ds")
  )
  expect_equal(
    sort(names(res_with_index$value@datasets)),
    sort(names(res_no_index$value@datasets))
  )
  expect_equal(DTAtools::metadata(res_with_index$value)@title, DTAtools::metadata(res_no_index$value)@title)
})

# ---- carry_over_default_fields() / apply_metadata_carry_over() -------------

test_that("carry_over_default_fields() returns exactly the documented default set", {
  fn <- app_fn("carry_over_default_fields")
  fields <- fn()

  expect_setequal(fields, c("receiver", "supplier", "transmission", "error_handling", "authorized_for_corrections"))
  # title/version/date/version_history are deliberately OFF: a new document
  # must not inherit the old document's own identity or revision history.
  expect_false("title" %in% fields)
  expect_false("version" %in% fields)
  expect_false("date" %in% fields)
  expect_false("version_history" %in% fields)
})

test_that("apply_metadata_carry_over() copies requested fields but silently drops machine-owned ones", {
  fn <- app_fn("apply_metadata_carry_over")
  dta <- app_fixture_dta()
  source_meta <- DTAtools::DTAMetaData(
    title = "Ancestor",
    version = "9.9",
    header = "Carried Header",
    template = list(id = "ancestor_tpl", version = "1.0")
  )

  result <- fn(dta, source_meta, c("header", "template", "import_issues"))

  expect_equal(DTAtools::metadata(result)@header, "Carried Header")
  # Neither machine-owned field was copied, even though both were requested.
  expect_length(DTAtools::metadata(result)@template, 0)
})

test_that("apply_metadata_carry_over() aborts naming an unknown metadata field", {
  fn <- app_fn("apply_metadata_carry_over")
  dta <- app_fixture_dta()
  source_meta <- DTAtools::DTAMetaData(title = "Ancestor", version = "1.0")

  expect_error(fn(dta, source_meta, "not_a_real_field"), regexp = "not_a_real_field", fixed = TRUE)
})

test_that("apply_metadata_carry_over() leaves an already-empty source field untouched", {
  fn <- app_fn("apply_metadata_carry_over")
  dta <- app_fixture_dta()
  before <- DTAtools::metadata(dta)@error_handling
  source_meta <- DTAtools::DTAMetaData(title = "Ancestor", version = "1.0") # error_handling is NULL

  result <- fn(dta, source_meta, "error_handling")

  expect_equal(DTAtools::metadata(result)@error_handling, before)
})

# ---- template_provenance() --------------------------------------------------

test_that("template_provenance() always includes id/version/created/content_hash/selections, and omits empty fields", {
  fn <- app_fn("template_provenance")
  def <- list(id = "t1", version = "1.0", base = list())
  meta <- list(
    id = "t1", version = "1.0",
    source_name = NA_character_, source_kind = NA_character_,
    path = "/x/t1.yaml", resolved_commit = NA_character_
  )

  result <- fn(def, meta, selections = list())

  expect_equal(result$id, "t1")
  expect_equal(result$version, "1.0")
  expect_s3_class(result$created, "Date")
  expect_match(result$content_hash, "^hash:")
  expect_equal(result$selections, list())
  # source_ref = meta$path is NOT empty, so it survives.
  expect_equal(result$source_ref, "/x/t1.yaml")
  # NA/absent fields are omitted, not written as empty/NA keys.
  expect_false("source" %in% names(result))
  expect_false("source_kind" %in% names(result))
  expect_false("resolved_commit" %in% names(result))
  expect_false("lineage" %in% names(result))
  expect_false("datasets" %in% names(result))
  expect_false("carried_over_from" %in% names(result))
})

test_that("template_provenance() includes lineage/datasets/carried_over_from when given", {
  fn <- app_fn("template_provenance")
  def <- list(id = "t1", version = "1.0")
  meta <- list(id = "t1", version = "1.0", source_name = "myrepo", source_kind = "git", path = "/x/t1.yaml", resolved_commit = "abc123")

  result <- fn(
    def, meta,
    selections = list(opt1 = "x"),
    lineage = c("base@1.0"),
    ds_provenance = list(list(name = "ds1")),
    carried_over_from = list(id = "old", version = "0.9")
  )

  expect_equal(result$source, "myrepo")
  expect_equal(result$source_kind, "git")
  expect_equal(result$resolved_commit, "abc123")
  expect_equal(result$lineage, "base@1.0")
  expect_equal(result$datasets, list(list(name = "ds1")))
  expect_equal(result$carried_over_from, list(id = "old", version = "0.9"))
})

test_that("template_provenance()'s content_hash changes when the underlying definition changes", {
  fn <- app_fn("template_provenance")
  meta <- list(id = "t1", version = "1.0")

  h1 <- fn(list(id = "t1", version = "1.0", base = list(x = 1)), meta, selections = list())$content_hash
  h2 <- fn(list(id = "t1", version = "1.0", base = list(x = 2)), meta, selections = list())$content_hash

  expect_false(identical(h1, h2))
  # Labelled "hash:", NEVER "sha256:" -- rlang::hash() is xxhash128, a change
  # detector, not a cryptographic digest.
  expect_match(h1, "^hash:", fixed = FALSE)
  expect_false(grepl("^sha256:", h1))
})

# ---- create_dta_from_template(): provenance round trip ----------------------

test_that("provenance round-trips through dta_to_yaml_text() -> dta_read_yaml_text(), including selections", {
  def <- list(
    base = list(metadata = list(title = "Roundtrip Title", version = "1.0")),
    datasets = list(mini_dataset_entry())
  )
  template_path <- file.path(withr::local_tempdir(), "roundtrip_ct.dta-template.yaml")

  meta <- list(id = "roundtrip_tpl", version = "1.0", source_name = "local", source_kind = "dir", path = template_path, resolved_commit = NA_character_)
  prov_fn <- app_fn("template_provenance")
  prov <- prov_fn(def, meta, selections = list(opt1 = "chosen"), lineage = c("parent_tpl@1.0"))

  create_fn <- app_fn("create_dta_from_template")
  res <- create_fn(def, template_path, selections = list(opt1 = "chosen"), provenance = prov)
  expect_true(res$ok)
  expect_equal(DTAtools::metadata(res$value)@template$id, "roundtrip_tpl")

  serialized <- app_fn("dta_to_yaml_text")(res$value)
  expect_true(serialized$ok)

  round <- app_fn("dta_read_yaml_text")(serialized$value)
  expect_true(round$ok)

  rt_template <- round$value@metadata@template
  expect_equal(rt_template$id, "roundtrip_tpl")
  expect_equal(rt_template$version, "1.0")
  expect_equal(rt_template$selections, list(opt1 = "chosen"))
  expect_equal(rt_template$lineage, "parent_tpl@1.0")
  expect_equal(rt_template$source, "local")
  expect_equal(rt_template$content_hash, prov$content_hash)
})

# ---- create_dta_from_template(): the seven-step order -----------------------

test_that("an option beats a party profile, which beats carry-over, which beats base.metadata", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  def <- list(
    base = list(metadata = list(supplier = list(affiliation = list(name = "Base Supplier")))),
    datasets = list(mini_dataset_entry()),
    party_slots = list(list(id = "supplier_choice", target = "metadata.supplier")),
    options = list(list(id = "supplier_opt", target = "metadata.supplier"))
  )
  ancestor_meta <- DTAtools::DTAMetaData(
    title = "Old", version = "0.9",
    supplier = list(affiliation = list(name = "Carried Supplier"))
  )

  create_fn <- app_fn("create_dta_from_template")
  res <- create_fn(
    def, file.path(root, "prec.yaml"),
    selections = list(supplier_opt = list(affiliation = list(name = "Option Supplier"))),
    index = idx,
    carry_over = list(metadata = ancestor_meta, fields = app_fn("carry_over_default_fields")()),
    party_selections = list(supplier_choice = "supplier_acme")
  )

  expect_true(res$ok)
  expect_equal(DTAtools::metadata(res$value)@supplier$affiliation$name, "Option Supplier")
})

test_that("a party profile beats carry-over when no option overrides the same field", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  def <- list(
    base = list(metadata = list(supplier = list(affiliation = list(name = "Base Supplier")))),
    datasets = list(mini_dataset_entry()),
    party_slots = list(list(id = "supplier_choice", target = "metadata.supplier"))
  )
  ancestor_meta <- DTAtools::DTAMetaData(
    title = "Old", version = "0.9",
    supplier = list(affiliation = list(name = "Carried Supplier"))
  )

  create_fn <- app_fn("create_dta_from_template")
  res <- create_fn(
    def, file.path(root, "prec2.yaml"),
    selections = list(),
    index = idx,
    carry_over = list(metadata = ancestor_meta, fields = app_fn("carry_over_default_fields")()),
    party_selections = list(supplier_choice = "supplier_acme")
  )

  expect_true(res$ok)
  expect_equal(DTAtools::metadata(res$value)@supplier$affiliation$name, "Party Supplier")
})

test_that("carry-over without a matching party/option selection leaves the ancestor's value in place", {
  def <- list(
    base = list(metadata = list(supplier = list(affiliation = list(name = "Base Supplier")))),
    datasets = list(mini_dataset_entry())
  )
  ancestor_meta <- DTAtools::DTAMetaData(
    title = "Old", version = "0.9",
    supplier = list(affiliation = list(name = "Carried Supplier"))
  )

  create_fn <- app_fn("create_dta_from_template")
  res <- create_fn(
    def, file.path(withr::local_tempdir(), "prec3.yaml"),
    selections = list(),
    carry_over = list(metadata = ancestor_meta, fields = app_fn("carry_over_default_fields")())
  )

  expect_true(res$ok)
  expect_equal(DTAtools::metadata(res$value)@supplier$affiliation$name, "Carried Supplier")
})

# ---- The most important test: carry-over never copies template/import_issues

test_that("carry-over never copies template or import_issues: a rebased document's provenance is its OWN", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  def_common <- function() {
    list(
      base = list(metadata = list(title = "T", version = "1.0")),
      datasets = list(mini_dataset_entry())
    )
  }
  create_fn <- app_fn("create_dta_from_template")
  carry_over_fields <- app_fn("carry_over_default_fields")()

  prov_a <- list(id = "doc_a_template", version = "1.0", selections = list())
  doc_a <- create_fn(def_common(), file.path(root, "a.yaml"), selections = list(), index = idx, provenance = prov_a)
  expect_true(doc_a$ok)
  meta_a <- DTAtools::metadata(doc_a$value)
  expect_equal(meta_a@template$id, "doc_a_template")

  # Explicitly ask to carry over "template" and "import_issues" too, on top of
  # the ordinary defaults -- proving they are dropped even when a caller
  # (mistakenly, or a stale UI) requests them by name, not merely when nobody
  # asks.
  prov_b <- list(id = "doc_b_template", version = "2.0", selections = list())
  doc_b <- create_fn(
    def_common(), file.path(root, "b.yaml"),
    selections = list(), index = idx,
    carry_over = list(metadata = meta_a, fields = c(carry_over_fields, "template", "import_issues")),
    provenance = prov_b
  )
  expect_true(doc_b$ok)
  meta_b <- DTAtools::metadata(doc_b$value)

  expect_equal(meta_b@template$id, "doc_b_template")
  expect_equal(meta_b@template$version, "2.0")
  expect_false(identical(meta_b@template, meta_a@template))
  expect_length(meta_b@import_issues, 0)
})

# ---- create_dta_from_template(): full family integration --------------------

test_that("create_dta_from_template() builds the full extends: family through the index, options overriding inherited base", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("child_ct", index = idx)
  expect_true(loaded$ok)

  create_fn <- app_fn("create_dta_from_template")
  res <- create_fn(
    loaded$value$def, loaded$value$path,
    selections = list(title_opt = "Child Title"),
    index = idx
  )

  expect_true(res$ok)
  md <- DTAtools::metadata(res$value)
  expect_equal(md@title, "Child Title") # option overrides base.metadata
  expect_equal(md@header, "Base Header") # inherited from the parent, untouched
  expect_equal(names(res$value@datasets), "gf_from_template")
})

# ---- Backward compatibility: every new argument defaults to a no-op --------

test_that("create_dta_from_template() with none of the four new arguments behaves exactly as before", {
  real_path <- system.file(
    "extdata", "templates", "biomarker_gf.dta-template.yaml",
    package = "DTAtools"
  )
  read_fn <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")

  parsed <- read_fn(real_path)
  expect_true(parsed$ok)

  result <- create_fn(parsed$value, real_path, selections = list())

  expect_true(result$ok)
  # No @template written when `provenance` is not supplied.
  expect_length(DTAtools::metadata(result$value)@template, 0)
})
