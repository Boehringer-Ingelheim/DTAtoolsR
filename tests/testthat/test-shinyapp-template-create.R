# Coverage for inst/shiny/dta_app/R/template_create.R -- the keystone that
# joins template_core.R, template_index.R, template_inherit.R, dataset_
# template.R and party_profiles.R into one creation path -- plus the extended
# create_dta_from_template() (template_core.R) that calls it. Reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why the
# app's helper files must be sourced into a private environment for testing).

# ---- Fixture family ----------------------------------------------------------
#
# One dataset template (gf_dt), one abstract parent creation template
# (base_ct) and one concrete child (child_ct) that `extends:` it, sharing the
# SAME `as: gf_from_template` identity on their dataset entry so
# dta_template_merge_datasets() (template_inherit.R) merges them into ONE
# entry instead of appending a second -- see the comment on
# dta_template_dataset_key() for why `as:` has to match for that to happen.
# Plus one party profile (supplier_acme) and two standalone dataset
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
      "  id: gf_data",
      "  type: tabular",
      "  name: GF data table",
      "  files: { filename: gf.tsv, type: tsv }",
      "  columns:",
      "    - { id: STUDYID, label: Study ID, type: SAS Char }",
      "    - { id: GFNAM, label: Vendor Name, type: SAS Char }",
      "  rules: []"
    ),
    lib_path(root, "gf_dt.dta-dataset-template.yaml")
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
    lib_path(root, "base_ct.dta-template.yaml")
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
    lib_path(root, "child_ct.dta-template.yaml")
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
    lib_path(root, "supplier_acme.dta-party.yaml")
  )
}

# Plain dataset YAMLs: referenced by path relative to the template file, so they
# sit in templates/ beside it.
write_standalone_dataset_files <- function(root) {
  writeLines(
    c("id: plain_path_ds", "type: file", "files: { filename: a.csv, type: csv }"),
    lib_path(root, "plain_path_ds.yaml")
  )
  writeLines(
    c("id: plain_source_ds", "type: file", "files: { filename: b.csv, type: csv }"),
    lib_path(root, "plain_source_ds.yaml")
  )
}

setup_fixture <- function(root) {
  write_gf_dataset_template(root)
  write_base_ct(root)
  write_child_ct(root)
  write_supplier_profile(root)
  write_standalone_dataset_files(root)
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
  list(id = "mini_ds", type = "file", files = list(filename = "mini.csv", type = "csv"))
}

# ---- template_dataset_entry_kind() ------------------------------------------

test_that("template_dataset_entry_kind() classifies all four accepted shapes", {
  fn <- app_fn("template_dataset_entry_kind")

  expect_equal(fn("gf_dataset.yaml"), "path")
  expect_equal(fn(list(source = "gf_dataset.yaml")), "source")
  expect_equal(fn(list(template = "gf@3.0")), "template")
  expect_equal(fn(list(id = "inline_ds", type = "file")), "inline")
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
  expect_equal(normalizePath(res$value$path), normalizePath(lib_path(root, "child_ct.dta-template.yaml")))
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

test_that("a metadata-only abstract base supports a concrete child's own dataset", {
  for (reusable in c(FALSE, TRUE)) {
    root <- withr::local_tempdir()
    parent_path <- lib_path(root, "metadata_base.dta-template.yaml")
    child_path <- lib_path(root, "metadata_child.dta-template.yaml")
    metadata <- list(title = "Shared transfer title", header = "Shared header")
    yaml::write_yaml(list(
      kind = "dta_creation_template", id = "metadata_base", version = "1.0",
      abstract = TRUE, base = list(metadata = metadata)
    ), parent_path)
    dataset <- list(
      id = "child_data", type = "tabular",
      files = list(filename = "child.tsv", type = "tsv"),
      columns = list(list(id = "SUBJECT", label = "Subject", type = "SAS Char"))
    )
    if (reusable) {
      yaml::write_yaml(list(
        kind = "dta_dataset_template", id = "child_dataset", version = "1.0",
        dataset = dataset
      ), lib_path(root, "child.dta-dataset-template.yaml"))
    }
    entry <- if (reusable) list(template = "child_dataset@1.0") else dataset
    yaml::write_yaml(list(
      kind = "dta_creation_template", id = "metadata_child", version = "1.0",
      extends = "metadata_base@1.0", datasets = list(entry)
    ), child_path)
    idx <- index_over(root)

    parent <- app_fn("read_dta_creation_template")(parent_path)
    expect_true(parent$ok)
    expect_identical(parent$value$base$metadata, metadata)
    expect_false("datasets" %in% names(parent$value))
    expect_true(app_fn("read_dta_creation_template")(child_path)$ok)

    loaded <- app_fn("load_template_definition")("metadata_child", index = idx)
    expect_true(loaded$ok, info = loaded$error)
    expect_identical(loaded$value$def$base$metadata, metadata)
    expect_length(loaded$value$def$datasets, 1L)
    expect_identical(loaded$value$lineage, "metadata_base@1.0")
    expect_false(isTRUE(loaded$value$def$abstract))
    built <- app_fn("create_dta_from_template")(
      loaded$value$def, child_path, index = idx
    )
    expect_true(built$ok, info = built$error)
    expect_identical(built$value@metadata@title, metadata$title)
    expect_identical(built$value@metadata@header, metadata$header)
    expect_identical(names(built$value@datasets), "child_data")
    expect_identical(names(built$value@datasets[[1]]@specs@columns), "SUBJECT")
    expect_identical(built$value@datasets[[1]]@files[[1]]@filename, "child.tsv")

    lint <- validate_template(root, strict = TRUE)
    expect_identical(nrow(lint), 0L)
    expect_identical(app_fn("template_picker_entries")(idx)$id, "metadata_child")
    abstract <- app_fn("load_template_definition")("metadata_base", index = idx)
    expect_false(abstract$ok)
    expect_match(abstract$error, "abstract")
  }
})

test_that("a concrete child must have a dataset after inheritance resolves", {
  root <- withr::local_tempdir()
  parent <- list(
    kind = "dta_creation_template", id = "metadata_base", version = "1.0",
    abstract = TRUE, base = list(metadata = list(title = "Shared title"))
  )
  yaml::write_yaml(parent, lib_path(root, "metadata_base.dta-template.yaml"))
  # Also check that explicitly clearing a dataset-bearing parent cannot evade
  # the concrete dataset requirement.
  parent$id <- "dataset_base"
  parent$datasets <- list(mini_dataset_entry())
  yaml::write_yaml(parent, lib_path(root, "dataset_base.dta-template.yaml"))
  children <- list(
    absent = list(extends = "metadata_base"),
    empty = list(extends = "metadata_base", datasets = list()),
    removed = list(extends = "metadata_base", datasets = NULL),
    cleared = list(extends = "dataset_base", datasets = list())
  )
  for (id in names(children)) {
    path <- lib_path(root, paste0(id, ".dta-template.yaml"))
    yaml::write_yaml(c(list(
      kind = "dta_creation_template", id = id, version = "1.0"
    ), children[[id]]), path)
    # The reader must defer this decision until the chain has resolved.
    expect_true(app_fn("read_dta_creation_template")(path)$ok)
  }
  idx <- index_over(root)
  for (id in names(children)) {
    loaded <- app_fn("load_template_definition")(id, index = idx)
    expect_false(loaded$ok)
    expect_match(loaded$error, "at least one dataset")
  }
  expect_error(validate_template(root, strict = TRUE), "instantiate_failed")
  lint <- validate_template(root)
  for (id in names(children)) {
    rows <- lint[lint$id == id, , drop = FALSE]
    expect_identical(rows$code, "instantiate_failed")
    expect_identical(rows$severity, "error")
    expect_match(rows$message, "at least one dataset")
  }
})

test_that("dataset-free abstract bases still need a base mapping and concrete roots need datasets", {
  root <- withr::local_tempdir()
  path <- lib_path(root, "root.dta-template.yaml")
  def <- list(kind = "dta_creation_template", id = "root", abstract = TRUE)
  yaml::write_yaml(def, path)
  expect_match(app_fn("read_dta_creation_template")(path)$error, "'base' section")
  def$base <- "not a mapping"
  yaml::write_yaml(def, path)
  expect_false(app_fn("read_dta_creation_template")(path)$ok)
  def$base <- list(metadata = list(title = "Title"))
  def$abstract <- FALSE
  yaml::write_yaml(def, path)
  expect_match(app_fn("read_dta_creation_template")(path)$error, "at least one dataset")
  def$abstract <- NULL
  yaml::write_yaml(def, path)
  expect_match(app_fn("read_dta_creation_template")(path)$error, "at least one dataset")
  expect_error(
    app_fn("dta_template_finalize_def")(def),
    "at least one dataset",
    class = "rlang_error"
  )
})

test_that("a child of an abstract template is concrete unless it says otherwise", {
  # THE BUG THIS GUARDS: `abstract` used to merge like any other scalar, so a
  # child that did not write `abstract: false` inherited its parent's `true`
  # and was refused by load_template_definition() -- which is why the
  # child_ct fixture above spells `abstract: false` out. The flag describes
  # the file that carries it, like id and version.
  root <- withr::local_tempdir()
  setup_fixture(root)
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: plain_child_ct",
      'version: "1.0"',
      "label: Plain Child CT",
      "extends: base_ct",
      "base:",
      "  metadata:",
      "    error_handling: Child-added handling note"
    ),
    lib_path(root, "plain_child_ct.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: abstract_child_ct",
      'version: "1.0"',
      "label: Abstract Child CT",
      "extends: base_ct",
      "abstract: true",
      "base:",
      "  metadata:",
      "    error_handling: Still abstract"
    ),
    lib_path(root, "abstract_child_ct.dta-template.yaml")
  )
  idx <- index_over(root)
  fn <- app_fn("load_template_definition")

  plain <- fn("plain_child_ct", index = idx)
  expect_true(plain$ok)
  expect_false(isTRUE(plain$value$def$abstract))
  expect_equal(plain$value$lineage, "base_ct@1.0")

  # A child that declares itself abstract still is: only inheritance is gone.
  own <- fn("abstract_child_ct", index = idx)
  expect_false(own$ok)
  expect_match(flat_message(own$error), "abstract")
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
  # `as:` sets the dataset's id; the template's name (its label) is left alone.
  expect_equal(ds@id, "gf_from_template")
  expect_equal(ds@name, "GF data table")
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
  res <- create_fn(def, lib_path(root, "bad_ref_ct.dta-template.yaml"), selections = list(), index = idx)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
  flat <- flat_message(res$error)
  expect_match(flat, "does_not_exist@9.9", fixed = TRUE)
  expect_match(flat, "bad_ref_ct", fixed = TRUE)
})

test_that("build_template_datasets() reproduces the ORIGINAL 'could not resolve dataset source' text for a bad plain-string path", {
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
  # list()'s own `x[[1]]$id` probe to return NULL rather than raising a raw
  # "$ operator is invalid for atomic vectors" -- a nested empty list is the
  # simplest value that is safely list-shaped while still lacking an `id`.
  def <- list(label = "y", datasets = list(list(columns = list())))

  expect_error(
    fn(def, idx, list(), template_path = file.path(root, "whatever.yaml")),
    regexp = "must contain an id field"
  )
})

test_that("build_template_datasets() refuses an inline dataset entry without an id", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  fn <- app_fn("build_template_datasets")
  # `name` is the label; it cannot stand in for the identifier.
  def <- list(
    label = "z",
    datasets = list(list(
      name = "No identifier", type = "file",
      files = list(filename = "inline.csv", type = "csv")
    ))
  )

  expect_error(
    fn(def, idx, list(), template_path = file.path(root, "whatever.yaml")),
    regexp = "needs an .*id"
  )
})

# ---- All three `datasets:` forms build the same DTA --------------------------

test_that("all three datasets: forms build the identical DTA whether or not an index is supplied", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  idx <- index_over(root)

  def_forms <- list(
    base = list(metadata = list(title = "Forms", version = "1.0")),
    label = "forms",
    datasets = list(
      "plain_path_ds.yaml",
      list(source = "plain_source_ds.yaml"),
      list(id = "plain_inline_ds", type = "file", files = list(filename = "inline.csv", type = "csv"))
    )
  )
  template_path <- lib_path(root, "forms_ct.dta-template.yaml") # need not exist on disk

  create_fn <- app_fn("create_dta_from_template")
  res_no_index <- create_fn(def_forms, template_path, selections = list())
  res_with_index <- create_fn(def_forms, template_path, selections = list(), index = idx)

  expect_true(res_no_index$ok)
  expect_true(res_with_index$ok)
  expect_setequal(
    names(res_no_index$value@datasets),
    c("plain_path_ds", "plain_source_ds", "plain_inline_ds")
  )
  expect_equal(
    sort(names(res_with_index$value@datasets)),
    sort(names(res_no_index$value@datasets))
  )
  # Each entry carries its identifier in `id:`, and none had a label to carry
  # over as a name.
  built <- res_no_index$value@datasets
  for (nm in names(built)) {
    expect_equal(built[[nm]]@id, nm)
    expect_null(built[[nm]]@name)
    expect_null(built[[nm]]@description)
  }
  expect_equal(DTAtools::metadata(res_with_index$value)@title, DTAtools::metadata(res_no_index$value)@title)
})

# ---- carry_over_default_fields() / apply_metadata_carry_over() -------------

test_that("carry_over_default_fields() returns exactly the documented default set", {
  fn <- app_fn("carry_over_default_fields")
  fields <- fn()

  expect_setequal(fields, c("receiver", "supplier", "transmission", "error_handling", "authorized_for_corrections", "study"))
  expect_false("sections" %in% fields)
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

test_that("template_provenance() always includes id/version/created/content_hash, and omits empty fields", {
  fn <- app_fn("template_provenance")
  def <- list(id = "t1", version = "1.0", base = list())
  meta <- list(
    id = "t1", version = "1.0",
    source_name = NA_character_, source_kind = NA_character_,
    path = "/x/t1.yaml", resolved_commit = NA_character_
  )

  result <- fn(def, meta)

  expect_equal(result$id, "t1")
  expect_equal(result$version, "1.0")
  expect_s3_class(result$created, "Date")
  expect_match(result$content_hash, "^hash:")
  # Option answers are never recorded: they already sit in the fields they set.
  expect_false("selections" %in% names(result))
  expect_false("vocabulary_selections" %in% names(result))
  # NA/absent fields are omitted, not written as empty/NA keys.
  expect_false("label" %in% names(result))
  expect_false("date" %in% names(result))
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

test_that("template_provenance() records label and the template version's own date", {
  fn <- app_fn("template_provenance")
  meta <- list(id = "t1", version = "2.0")
  # yaml reads an unquoted date as a Date; it is written as plain text.
  def <- list(id = "t1", version = "2.0", label = "Trial DTA", date = as.Date("2026-03-04"))

  result <- fn(def, meta)

  expect_identical(result$label, "Trial DTA")
  expect_identical(result$date, "2026-03-04")
})

test_that("template_provenance() keeps a full source_ref only for a user's own dir: source", {
  fn <- app_fn("template_provenance")
  def <- list(id = "t1", version = "1.0")
  ref_for <- function(kind, name) {
    fn(def, list(
      id = "t1", version = "1.0", source_kind = kind, source_name = name,
      path = "C:/cache/clone-abc/templates/t1.dta-template.yaml"
    ))$source_ref
  }

  # A git clone, an installed package or the bundled templates sit at a
  # machine-specific path: only the file's path within the source is
  # recorded, which includes the folder of its kind.
  expect_identical(ref_for("git", "lib"), "templates/t1.dta-template.yaml")
  expect_identical(ref_for("pkg", "lib"), "templates/t1.dta-template.yaml")
  expect_identical(ref_for("dir", "builtin"), "templates/t1.dta-template.yaml")
  expect_identical(ref_for("dir", "mine"), "C:/cache/clone-abc/templates/t1.dta-template.yaml")

  # The folder follows the template's kind.
  party <- fn(def, list(
    id = "p1", version = "1.0", source_kind = "git", source_name = "lib",
    kind = "dta_party_profile", path = "C:/cache/clone-abc/parties/p1.dta-party.yaml"
  ))
  expect_identical(party$source_ref, "parties/p1.dta-party.yaml")
})

test_that("template_provenance()'s content_hash changes when the underlying definition changes", {
  fn <- app_fn("template_provenance")
  meta <- list(id = "t1", version = "1.0")

  h1 <- fn(list(id = "t1", version = "1.0", base = list(x = 1)), meta)$content_hash
  h2 <- fn(list(id = "t1", version = "1.0", base = list(x = 2)), meta)$content_hash

  expect_false(identical(h1, h2))
  # Labelled "hash:", NEVER "sha256:" -- rlang::hash() is xxhash128, a change
  # detector, not a cryptographic digest.
  expect_match(h1, "^hash:", fixed = FALSE)
  expect_false(grepl("^sha256:", h1))
})

# ---- create_dta_from_template(): provenance round trip ----------------------

test_that("provenance round-trips through dta_to_yaml_text() -> dta_read_yaml_text(), without selections", {
  def <- list(
    base = list(metadata = list(title = "Roundtrip Title", version = "1.0")),
    datasets = list(mini_dataset_entry())
  )
  template_path <- file.path(withr::local_tempdir(), "roundtrip_ct.dta-template.yaml")

  meta <- list(id = "roundtrip_tpl", version = "1.0", source_name = "local", source_kind = "dir", path = template_path, resolved_commit = NA_character_)
  prov_fn <- app_fn("template_provenance")
  prov <- prov_fn(def, meta, lineage = c("parent_tpl@1.0"))

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
  expect_false("selections" %in% names(rt_template))
  expect_false(grepl("selections", serialized$value, fixed = TRUE))
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
    "extdata", "library", "templates", "biomarker_gf.dta-template.yaml",
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

# ---- mixins: load_template_definition() and template_provenance() -----------

write_mixin_fixture <- function(root) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: mx_privacy",
      'version: "1.0"',
      "label: Privacy block",
      "abstract: true",
      "base: {}",
      "options:",
      "  - id: privacy_opt",
      "    label: Privacy",
      "    target: metadata.title"
    ),
    lib_path(root, "mx_privacy.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: mx_assay",
      'version: "1.0"',
      "abstract: true",
      "base: {}",
      "datasets:",
      "  - id: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }"
    ),
    lib_path(root, "mx_assay.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: mx_child",
      'version: "1.0"',
      "label: Mixin child",
      "mixins: [mx_privacy@1.0, mx_assay]",
      "base:",
      "  metadata:",
      "    title: T"
    ),
    lib_path(root, "mx_child.dta-template.yaml")
  )
}

test_that("load_template_definition() carries the resolved mixins, in listed order", {
  root <- withr::local_tempdir()
  write_mixin_fixture(root)
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("mx_child", index = idx)

  expect_true(loaded$ok, info = loaded$error)
  expect_identical(loaded$value$mixins, c("mx_privacy@1.0", "mx_assay@1.0"))
  expect_identical(loaded$value$lineage, character(0))
  expect_identical(vapply(loaded$value$def$options, function(o) o$id, character(1)), "privacy_opt")
  expect_false("mixins" %in% names(loaded$value$def))
})

test_that("load_template_definition() reports a conflicting mixin as a non-throwing error", {
  root <- withr::local_tempdir()
  write_mixin_fixture(root)
  writeLines(
    c(
      "kind: dta_creation_template", "id: mx_clash", 'version: "1.0"', "abstract: true",
      "base: {}", "options:", "  - id: privacy_opt"
    ),
    lib_path(root, "mx_clash.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template", "id: mx_bad_child", 'version: "1.0"',
      "mixins: [mx_privacy, mx_clash]", "base:", "  metadata:", "    title: T"
    ),
    lib_path(root, "mx_bad_child.dta-template.yaml")
  )
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("mx_bad_child", index = idx)

  expect_false(loaded$ok)
  expect_match(flat_message(loaded$error), "privacy_opt", fixed = TRUE)
})

test_that("template_provenance() records the mixins next to the lineage, and omits them when none", {
  fn <- app_fn("template_provenance")
  def <- list(id = "t1", version = "1.0")
  meta <- list(
    id = "t1", version = "1.0", lineage = "base@1.0",
    mixins = c("m1@1.0", "m2@2.0")
  )

  result <- fn(def, meta)
  expect_identical(result$mixins, c("m1@1.0", "m2@2.0"))
  expect_identical(result$lineage, "base@1.0")
  expect_identical(
    which(names(result) == "mixins"), which(names(result) == "lineage") + 1L
  )

  expect_identical(fn(def, meta, mixins = "x@1.0")$mixins, "x@1.0")
  expect_false("mixins" %in% names(fn(def, list(id = "t1", version = "1.0"))))
  expect_false("mixins" %in% names(fn(def, meta, mixins = character(0))))
})

# ---- Cross-folder references in a library ------------------------------------

test_that("a creation template in templates/ builds from files in all three kind folders", {
  root <- withr::local_tempdir()
  # parties/ -- the supplier the party slot offers
  writeLines(
    c(
      "kind: dta_party_profile", "id: xf_supplier", 'version: "1.0"', "role: supplier",
      "label: XF Labs", "affiliation:", "  name: Cross Folder Labs", "  country: DE"
    ),
    lib_path(root, "xf_supplier.dta-party.yaml")
  )
  # vocabulary/ -- the term list the dataset column is bound to
  writeLines(
    c(
      "kind: dta_vocabulary", "id: xf_visits", 'version: "1.0"', "label: Visits", "type: text",
      "terms:", "  - code: SCR", "    label: Screening", "  - code: C1D1", "    label: Cycle 1 Day 1",
      "  - code: EOT", "    label: End of treatment"
    ),
    lib_path(root, "xf_visits.dta-vocabulary.yaml")
  )
  # templates/ -- a dataset template using the vocabulary ...
  writeLines(
    c(
      "kind: dta_dataset_template", "id: xf_ds", 'version: "1.0"', "label: XF dataset",
      "dataset:", "  id: xf_ds", "  type: tabular", "  files: {filename: xf.csv, type: csv}",
      "  columns:", "    - id: VISIT", "      type: SAS Char", "      values_from: xf_visits@1.0"
    ),
    lib_path(root, "xf_ds.dta-dataset-template.yaml")
  )
  # ... and a creation template importing it, with a party slot and a
  # vocabulary slot reaching into the other two folders.
  writeLines(
    c(
      "kind: dta_creation_template", "id: xf_ct", 'version: "1.0"', "label: XF creation",
      "base:", "  metadata:", "    title: Cross folder",
      "datasets:", "  - template: xf_ds@1.0", "    as: xf_built",
      "party_slots:", "  - id: supplier_choice", "    target: metadata.supplier",
      "vocabulary_slots:", "  - id: visit_choice", "    label: Visits",
      "    target: datasets.xf_built.columns.VISIT.values",
      "    vocabulary: xf_visits@1.0", "    default: [SCR, C1D1]"
    ),
    lib_path(root, "xf_ct.dta-template.yaml")
  )
  idx <- index_over(root)

  # Everything is found where the layout says, and nothing is reported misplaced.
  expect_length(attr(idx, "warnings"), 0)
  expect_setequal(idx$id, c("xf_supplier", "xf_visits", "xf_ds", "xf_ct"))
  expect_identical(nrow(app_fn("dta_template_misplaced_files")(root)), 0L)

  loaded <- app_fn("load_template_definition")("xf_ct@1.0", index = idx)
  expect_true(loaded$ok, info = loaded$error)
  create <- app_fn("create_dta_from_template")

  res <- create(
    loaded$value$def, loaded$value$path,
    index = idx, party_selections = list(supplier_choice = "xf_supplier")
  )

  expect_true(res$ok, info = res$error)
  # The dataset came from templates/, its default term selection from vocabulary/ ...
  col <- res$value@datasets[["xf_built"]]@specs@columns[[1]]
  expect_identical(col@id, "VISIT")
  expect_identical(col@values, c("SCR", "C1D1"))
  # ... and the supplier from parties/.
  expect_identical(DTAtools::metadata(res$value)@supplier$affiliation$name, "Cross Folder Labs")
  expect_identical(DTAtools::metadata(res$value)@title, "Cross folder")

  chosen <- create(
    loaded$value$def, loaded$value$path,
    index = idx, vocab_selections = list(visit_choice = "EOT")
  )
  expect_true(chosen$ok, info = chosen$error)
  expect_identical(chosen$value@datasets[["xf_built"]]@specs@columns[[1]]@values, "EOT")
})

test_that("a reference to a file that was left outside its kind folder does not resolve", {
  root <- withr::local_tempdir()
  setup_fixture(root)
  # A second party profile, lying in templates/ instead of parties/.
  writeLines(
    c("kind: dta_party_profile", "id: stray_party", 'version: "1.0"', "role: supplier", "label: Stray"),
    file.path(root, "templates", "stray_party.dta-party.yaml")
  )
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  expect_warning(idx <- app_fn("build_template_index")(), "belongs in parties/")

  expect_false("stray_party" %in% idx$id)
  expect_true("supplier_acme" %in% idx$id)
  expect_null(app_fn("template_index_resolver")(idx)("stray_party"))
})

test_that("a misplaced file is reported under a repository folder with a non-ASCII name", {
  # list.files() returns native-encoded names; a radix sort refuses a
  # non-ASCII one unless it is made UTF-8 first.
  root <- file.path(withr::local_tempdir(), "J\u00fcrgen_repo")
  dir.create(root)
  setup_fixture(root)
  writeLines(
    c("kind: dta_party_profile", "id: stray_party", 'version: "1.0"', "role: supplier", "label: Stray"),
    file.path(root, "templates", "stray_party.dta-party.yaml")
  )
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  expect_warning(idx <- app_fn("build_template_index")(), "belongs in parties/")
  expect_true("supplier_acme" %in% idx$id)
  expect_false("stray_party" %in% idx$id)
})
