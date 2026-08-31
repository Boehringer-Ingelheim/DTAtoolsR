# Coverage for inst/shiny/dta_app/R/template_diff.R -- the diff engine
# (dta_metadata_leaves()/dta_dataset_leaves()/dta_diff()) and the three-way
# rebase built on top of it (materialise_template()/dta_template_provenance()/
# rebase_plan()/rebase_apply()). Reached via the app_env()/app_fn() harness in
# helper-shinyapp.R (see that file for why the app's helper files must be
# sourced into a private environment for testing).

Sys.setenv(NOT_CRAN = "true")

# Local copy of the isolation helper defined in test-shinyapp-template-
# sources.R / test-shinyapp-template-create.R -- deliberately duplicated
# rather than shared, per those files' own guidance, so this file does not
# depend on another test file's internals. Every DTATOOLS_TEMPLATE_* variable
# is cleared for the duration of each test: a developer machine with
# DTATOOLS_TEMPLATE_SOURCES exported would flip the app into private-only mode
# and fail unrelated tests confusingly.
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
# wrap it. Copied from test-shinyapp-template-create.R for the same reason
# local_clean_template_env() is copied rather than shared.
flat_message <- function(x) gsub("\\s+", " ", x)

# Build the index over a fixture root, invalidating the memoised index first
# (a memoised index leaking between tests is the most likely source of
# flakiness here) and using build_template_index() directly rather than the
# cached accessor, so no test's index can leak into another's via the shared
# TTL cache.
index_over <- function(root) {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  app_fn("build_template_index")()
}

# ---- Fixture family: `demo_ct` at 1.0 and 1.1 --------------------------------
#
# One creation template id, two versions, differing in exactly the metadata
# fields the rebase classification tests need to move independently:
#   header                      -- changes 1.0 -> 1.1 (template-only edits)
#   error_handling              -- changes 1.0 -> 1.1
#   supplier.affiliation.name   -- changes 1.0 -> 1.1
#   title, metadata.version     -- IDENTICAL across 1.0/1.1 (a stable baseline
#                                  a hand edit can be layered onto without a
#                                  template-side change muddying the picture)
# Each carries one trivial inline dataset entry (the same minimal shape the
# template_create.R test suite uses) so read_dta_creation_template()'s "must
# define at least one dataset" check is satisfied without depending on the
# dataset-template machinery this file has nothing to do with.
write_demo_ct <- function(root, version, header, error_handling, supplier_name,
                          title = "Ancestor Title") {
  path <- file.path(root, paste0("demo_ct_", gsub("[.]", "_", version, fixed = FALSE), ".dta-template.yaml"))
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: demo_ct",
      sprintf('version: "%s"', version),
      "label: Demo CT",
      "base:",
      "  metadata:",
      sprintf('    title: "%s"', title),
      '    version: "1.0"',
      sprintf('    header: "%s"', header),
      sprintf('    error_handling: "%s"', error_handling),
      "    supplier:",
      "      affiliation:",
      sprintf('        name: "%s"', supplier_name),
      "datasets:",
      "  - name: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }",
      "options: []"
    ),
    path
  )
  invisible(path)
}

# Both versions in one root -- the ordinary case, ancestor resolvable.
setup_demo_family <- function(root) {
  write_demo_ct(root, "1.0", header = "Header A", error_handling = "Standard handling", supplier_name = "ACME v1")
  write_demo_ct(root, "1.1", header = "Header B", error_handling = "Updated handling", supplier_name = "Template ACME v2")
  invisible(root)
}

# ONLY 1.1 -- for the missing-ancestor degradation test: a document recorded
# as built from 1.0, rebased through an index where 1.0 has since been pruned.
setup_demo_target_only <- function(root) {
  write_demo_ct(root, "1.1", header = "Header B", error_handling = "Updated handling", supplier_name = "Template ACME v2")
  invisible(root)
}

# Build the "current" document for the classification fixture: create it from
# demo_ct@1.0 (stamping REAL provenance, exactly like the "Create new from
# template" flow does), then layer on the three kinds of hand edit the
# classification tests need:
#   title           -- USER edits it; the template never touches it (1.0 and
#                      1.1 agree) -> keep_user.
#   error_handling  -- USER edits it to the SAME text 1.1 independently
#                      changes it to -> agree.
#   supplier.affiliation.name -- USER edits it to a THIRD value, different
#                      from both the ancestor's and 1.1's -> conflict.
#   header          -- left untouched by the user -> take_template (1.1
#                      changes it, current does not).
# `lineage` is injected directly onto the freshly-stamped provenance (via
# S7::prop(), the same route test-DTAMetaData-template.R uses) so the
# "preserving lineage" behaviour of rebase_plan()/rebase_apply() has something
# non-empty to actually preserve -- template_provenance() omits `lineage`
# entirely when it is character(0), which would make that assertion trivially
# true for the wrong reason.
build_demo_current <- function(idx, lineage = "grandparent_ct@1.0") {
  load_fn <- app_fn("load_template_definition")
  create_fn <- app_fn("create_dta_from_template")
  prov_fn <- app_fn("template_provenance")

  loaded <- load_fn("demo_ct@1.0", index = idx)
  testthat::expect_true(loaded$ok)

  prov <- prov_fn(loaded$value$def, loaded$value, selections = list())
  built <- create_fn(loaded$value$def, loaded$value$path, selections = list(), index = idx, provenance = prov)
  testthat::expect_true(built$ok)

  dta <- built$value
  md <- DTAtools::metadata(dta)
  tpl <- S7::prop(md, "template")
  tpl$lineage <- lineage
  S7::prop(md, "template") <- tpl
  dta@metadata <- md

  # The three hand edits.
  md <- DTAtools::metadata(dta)
  S7::prop(md, "title") <- "User's Own Title"
  S7::prop(md, "error_handling") <- "Updated handling" # matches 1.1 exactly -> agree
  supplier <- S7::prop(md, "supplier")
  supplier$affiliation$name <- "User's ACME"
  S7::prop(md, "supplier") <- supplier
  dta@metadata <- md

  dta
}

# ---- dta_metadata_leaves() ---------------------------------------------------

test_that("dta_metadata_leaves() flattens nested paths, including array-style version_history", {
  md <- DTAtools::DTAMetaData(
    title = "T", version = "1.0", header = "H",
    supplier = list(affiliation = list(name = "Sup Name")),
    version_history = list(list(version = "0.9", date = as.Date("2024-01-01"), changes = "Initial"))
  )

  leaves <- app_fn("dta_metadata_leaves")(md)

  expect_equal(leaves[["title"]], "T")
  expect_equal(leaves[["header"]], "H")
  expect_equal(leaves[["supplier.affiliation.name"]], "Sup Name")
  expect_equal(leaves[["version_history.1.version"]], "0.9")
  expect_equal(leaves[["version_history.1.changes"]], "Initial")
})

test_that("dta_metadata_leaves() excludes template and import_issues", {
  md <- DTAtools::DTAMetaData(title = "T", version = "1.0")
  S7::prop(md, "template") <- list(id = "tpl", version = "1.0")

  leaves <- app_fn("dta_metadata_leaves")(md)

  expect_false(any(grepl("^template(\\.|$)", names(leaves))))
  expect_false(any(grepl("^import_issues(\\.|$)", names(leaves))))
  # Sanity: the exclusion did not also eat unrelated top-level fields.
  expect_equal(leaves[["title"]], "T")
})

# ---- dta_dataset_leaves() -----------------------------------------------------

test_that("dta_dataset_leaves() keys columns by id and is stable across a column insertion", {
  read_text <- app_fn("dta_read_yaml_text")

  before <- read_text(paste(
    "metadata:",
    "  title: DS Test",
    '  version: "1.0"',
    "datasets:",
    "  - name: ds1",
    "    type: tabular",
    "    files: { filename: a.csv, type: csv }",
    "    columns:",
    "      - { id: COL_A, label: A, type: SAS Char }",
    "      - { id: COL_B, label: B, type: SAS Char }",
    sep = "\n"
  ))
  after <- read_text(paste(
    "metadata:",
    "  title: DS Test",
    '  version: "1.0"',
    "datasets:",
    "  - name: ds1",
    "    type: tabular",
    "    files: { filename: a.csv, type: csv }",
    "    columns:",
    "      - { id: COL_A, label: A, type: SAS Char }",
    "      - { id: COL_NEW, label: New Col, type: SAS Char }",
    "      - { id: COL_B, label: B, type: SAS Char }",
    sep = "\n"
  ))
  expect_true(before$ok)
  expect_true(after$ok)

  leaves_before <- app_fn("dta_dataset_leaves")(before$value)
  leaves_after <- app_fn("dta_dataset_leaves")(after$value)

  expect_equal(leaves_before[["ds1.columns.COL_A.label"]], "A")
  expect_equal(leaves_before[["ds1.columns.COL_B.label"]], "B")
  # COL_B moved from position 2 to position 3 -- its own leaf must be
  # UNCHANGED by that shift, which a positional key would have broken.
  expect_equal(leaves_after[["ds1.columns.COL_B.label"]], leaves_before[["ds1.columns.COL_B.label"]])
  expect_equal(leaves_after[["ds1.columns.COL_NEW.label"]], "New Col")
})

# ---- dta_diff() ---------------------------------------------------------------

test_that("dta_diff() reports added/removed/changed and omits unchanged keys", {
  read_text <- app_fn("dta_read_yaml_text")

  a <- read_text(paste(
    "metadata:",
    "  title: Same Title",
    '  version: "1.0"',
    "  header: Header A",
    "  error_handling: Old Handling",
    "datasets:",
    "  - name: ds1",
    "    type: file",
    "    files: { filename: a.csv, type: csv }",
    sep = "\n"
  ))
  b <- read_text(paste(
    "metadata:",
    "  title: Same Title",
    '  version: "1.0"',
    "  header: Header Changed",
    "  authorized_for_corrections: New Person",
    "datasets:",
    "  - name: ds1",
    "    type: file",
    "    files: { filename: a.csv, type: csv }",
    sep = "\n"
  ))
  expect_true(a$ok)
  expect_true(b$ok)

  diff <- app_fn("dta_diff")(a$value, b$value)
  md_diff <- diff$metadata

  expect_false("title" %in% md_diff$key) # unchanged -- omitted

  header_row <- md_diff[md_diff$key == "header", , drop = FALSE]
  expect_equal(header_row$change, "changed")
  expect_equal(header_row$from, "Header A")
  expect_equal(header_row$to, "Header Changed")

  removed_row <- md_diff[md_diff$key == "error_handling", , drop = FALSE]
  expect_equal(removed_row$change, "removed")
  expect_equal(removed_row$from, "Old Handling")
  expect_true(is.na(removed_row$to))

  added_row <- md_diff[md_diff$key == "authorized_for_corrections", , drop = FALSE]
  expect_equal(added_row$change, "added")
  expect_true(is.na(added_row$from))
  expect_equal(added_row$to, "New Person")
})

# ---- rebase_plan(): three-way classification ---------------------------------

test_that("rebase_plan() classifies take_template / keep_user / agree / conflict correctly", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  current <- build_demo_current(idx)

  plan <- app_fn("rebase_plan")(current, "1.1", index = idx)

  expect_true(plan$ok)
  expect_equal(plan$from_version, "1.0")
  expect_equal(plan$to_version, "1.1")
  expect_true(plan$ancestor_available)

  changes <- plan$changes
  conflicts <- plan$conflicts

  header_row <- changes[changes$key == "header", , drop = FALSE]
  expect_equal(nrow(header_row), 1)
  expect_equal(header_row$resolution, "take_template")
  expect_equal(header_row$target, "Header B")

  title_row <- changes[changes$key == "title", , drop = FALSE]
  expect_equal(nrow(title_row), 1)
  expect_equal(title_row$resolution, "keep_user")
  expect_equal(title_row$current, "User's Own Title")

  eh_row <- changes[changes$key == "error_handling", , drop = FALSE]
  expect_equal(nrow(eh_row), 1)
  expect_equal(eh_row$resolution, "agree")

  # Both changed error_handling to the identical text -- NOT a conflict.
  expect_false("error_handling" %in% conflicts$key)

  supplier_row <- conflicts[conflicts$key == "supplier.affiliation.name", , drop = FALSE]
  expect_equal(nrow(supplier_row), 1)
  expect_equal(supplier_row$ancestor, "ACME v1")
  expect_equal(supplier_row$current, "User's ACME")
  expect_equal(supplier_row$target, "Template ACME v2")
})

# ---- rebase_plan(): missing / partial provenance -----------------------------

test_that("rebase_plan() gives a clear error for a document with no template provenance", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  dta <- app_fixture_dta() # a real DTA, never built from a template

  plan <- app_fn("rebase_plan")(dta, "1.1", index = idx)

  expect_false(plan$ok)
  expect_true(nzchar(plan$error))
  expect_match(flat_message(plan$error), "template provenance", ignore.case = TRUE)
})

test_that("rebase_plan() names the missing field for a partial provenance record", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  dta <- app_fixture_dta()
  md <- DTAtools::metadata(dta)
  # id/version alone: DTAMetaData's own validator requires BOTH whenever
  # @template is non-empty, so this is the only "partial" record that can
  # exist as a legitimate object -- `selections` is not validated there and
  # is the field this test actually exercises being reported as missing.
  S7::prop(md, "template") <- list(id = "demo_ct", version = "1.0")
  dta@metadata <- md

  plan <- app_fn("rebase_plan")(dta, "1.1", index = idx)

  expect_false(plan$ok)
  expect_match(flat_message(plan$error), "selections", ignore.case = TRUE)
})

# ---- rebase_plan(): missing-ancestor degradation -----------------------------

test_that("rebase_plan() degrades to a two-way, all-conflicts comparison when the ancestor cannot be resolved", {
  full_root <- withr::local_tempdir()
  setup_demo_family(full_root)
  full_idx <- index_over(full_root)

  current <- build_demo_current(full_idx)

  target_only_root <- withr::local_tempdir()
  setup_demo_target_only(target_only_root)
  target_only_idx <- index_over(target_only_root)

  plan <- app_fn("rebase_plan")(current, "1.1", index = target_only_idx)

  expect_true(plan$ok)
  expect_false(plan$ancestor_available)
  expect_equal(nrow(plan$changes), 0) # no three-way classification is attempted

  # Every one of the four fields the user/template touched differently must
  # show up as a conflict -- including "header", which was cleanly
  # take_template-able in the three-way case above: with no ancestor, that
  # confidence is gone, and the task's own rule is "every differing key is a
  # conflict requiring an explicit choice".
  expect_true("header" %in% plan$conflicts$key)
  expect_true("title" %in% plan$conflicts$key)
  expect_true("supplier.affiliation.name" %in% plan$conflicts$key)

  header_row <- plan$conflicts[plan$conflicts$key == "header", , drop = FALSE]
  expect_true(is.na(header_row$ancestor))
  expect_equal(header_row$current, "Header A")
  expect_equal(header_row$target, "Header B")

  # error_handling: current and target happen to agree ("Updated handling")
  # even in degraded mode -- that is still not a difference, and must not be
  # reported as a conflict just because the ancestor is unknown.
  expect_false("error_handling" %in% plan$conflicts$key)
})

# ---- rebase_apply() -----------------------------------------------------------

test_that("rebase_apply() with every conflict resolved applies correctly, appends history, and updates provenance", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  current <- build_demo_current(idx)
  plan <- app_fn("rebase_plan")(current, "1.1", index = idx)
  expect_true(plan$ok)

  applied <- app_fn("rebase_apply")(
    current, plan,
    resolutions = list("supplier.affiliation.name" = "target")
  )

  expect_true(applied$ok)
  md <- DTAtools::metadata(applied$value)

  expect_equal(md@header, "Header B") # take_template
  expect_equal(md@title, "User's Own Title") # keep_user, untouched
  expect_equal(md@error_handling, "Updated handling") # agree
  expect_equal(md@supplier$affiliation$name, "Template ACME v2") # conflict -> target

  expect_length(md@version_history, 1)
  expect_equal(md@version_history[[1]]$version, "1.1")
  expect_true(nzchar(md@version_history[[1]]$changes))

  expect_equal(md@template$id, "demo_ct")
  expect_equal(md@template$version, "1.1")
  expect_equal(md@template$lineage, "grandparent_ct@1.0") # preserved, not re-derived
})

test_that("rebase_apply() errors on an unresolved conflict and leaves the input DTA untouched", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  current <- build_demo_current(idx)
  plan <- app_fn("rebase_plan")(current, "1.1", index = idx)
  expect_true(plan$ok)

  before_header <- DTAtools::metadata(current)@header
  before_template_version <- DTAtools::metadata(current)@template$version
  before_history_len <- length(DTAtools::metadata(current)@version_history)

  applied <- app_fn("rebase_apply")(current, plan, resolutions = list())

  expect_false(applied$ok)
  expect_true(nzchar(applied$error))
  expect_match(flat_message(applied$error), "supplier.affiliation.name", fixed = TRUE)

  # `current` itself -- the object the caller still holds -- must be
  # byte-for-byte what it was before the failed call: a half-applied rebase
  # would silently corrupt a signed specification.
  after_md <- DTAtools::metadata(current)
  expect_equal(after_md@header, before_header)
  expect_equal(after_md@template$version, before_template_version)
  expect_length(after_md@version_history, before_history_len)
  expect_equal(after_md@title, "User's Own Title")
})

test_that("a rebase round-trips through dta_to_yaml_text() / dta_read_yaml_text(), including the updated provenance", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  current <- build_demo_current(idx)
  plan <- app_fn("rebase_plan")(current, "1.1", index = idx)
  expect_true(plan$ok)

  applied <- app_fn("rebase_apply")(
    current, plan,
    resolutions = list("supplier.affiliation.name" = "current")
  )
  expect_true(applied$ok)

  serialized <- app_fn("dta_to_yaml_text")(applied$value)
  expect_true(serialized$ok)

  round <- app_fn("dta_read_yaml_text")(serialized$value)
  expect_true(round$ok)

  rt_md <- DTAtools::metadata(round$value)
  expect_equal(rt_md@template$id, "demo_ct")
  expect_equal(rt_md@template$version, "1.1")
  expect_equal(rt_md@template$lineage, "grandparent_ct@1.0")
  expect_equal(rt_md@header, "Header B")
  expect_equal(rt_md@supplier$affiliation$name, "User's ACME") # resolved to "current"
  expect_length(rt_md@version_history, 1)
  expect_equal(rt_md@version_history[[1]]$version, "1.1")
})

# ---- materialise_template() / dta_template_provenance() ---------------------

test_that("materialise_template() rebuilds exactly what the template would produce", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  res <- app_fn("materialise_template")("demo_ct@1.0", index = idx, selections = list())

  expect_true(res$ok)
  md <- DTAtools::metadata(res$value)
  expect_equal(md@header, "Header A")
  expect_equal(md@supplier$affiliation$name, "ACME v1")
})

test_that("materialise_template() reports a clear, non-throwing error for an unresolvable ref", {
  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)

  res <- app_fn("materialise_template")("demo_ct@9.9", index = idx, selections = list())

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(nzchar(res$error))
})

test_that("dta_template_provenance() returns NULL when absent and the record when present", {
  fn <- app_fn("dta_template_provenance")

  plain <- app_fixture_dta()
  expect_null(fn(plain))

  root <- withr::local_tempdir()
  setup_demo_family(root)
  idx <- index_over(root)
  current <- build_demo_current(idx)

  prov <- fn(current)
  expect_equal(prov$id, "demo_ct")
  expect_equal(prov$version, "1.0")
})

# ---- version_history is never rebased ---------------------------------------

# A template family whose base.metadata carries a version_history block, with
# the entry's `version` tracking the template version. This is exactly what the
# packaged biomarker_gf template does via `version: "${version}"`, and it is the
# shape that produces a `version_history.1.version` leaf on any rebase.
write_vh_ct <- function(root, version, header) {
  path <- file.path(root, sprintf("vh_ct_%s.dta-template.yaml", gsub("[.]", "_", version)))
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: vh_ct",
      sprintf('version: "%s"', version),
      "label: VH CT",
      "base:",
      "  metadata:",
      '    title: "VH Title"',
      sprintf('    version: "%s"', version),
      sprintf('    header: "%s"', header),
      "    version_history:",
      sprintf('      - version: "%s"', version),
      '        date: "2026-01-01"',
      '        changes: "Initial template-generated version"',
      "datasets:",
      "  - name: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }",
      "options: []"
    ),
    path
  )
  invisible(path)
}

# Build a document from vh_ct@1.0 WITH provenance -- rebase_plan() requires it,
# and materialise_template() deliberately does not stamp any (it rebuilds what a
# template would produce, which is not the same thing as a saved document).
build_vh_current <- function(idx) {
  loaded <- app_fn("load_template_definition")("vh_ct@1.0", index = idx)
  testthat::expect_true(loaded$ok)
  prov <- app_fn("template_provenance")(loaded$value$def, loaded$value, selections = list())
  built <- app_fn("create_dta_from_template")(
    loaded$value$def, loaded$value$path,
    selections = list(), index = idx, provenance = prov
  )
  testthat::expect_true(built$ok)
  built$value
}

test_that("list_set_path() corrupts a positional version_history key", {
  # THE REASON version_history is excluded from a rebase. The leaf key is
  # positional -- `version_history.1.version` -- but list_set_path() indexes a
  # list by NAME. Writing that key does not update entry one: it APPENDS a
  # second, stub entry literally named "1" holding only that field, turning a
  # one-entry history into a corrupt two-entry one.
  set_path <- app_fn("list_set_path")
  vh <- list(list(version = "1.0", date = "2026-01-01", changes = "Initial"))

  corrupted <- set_path(vh, c("1", "version"), "1.1")

  expect_length(corrupted, 2)
  expect_true("1" %in% names(corrupted))
  expect_equal(corrupted[[1]]$version, "1.0")
})

test_that("rebase_plan() never offers a version_history key as a change or conflict", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_vh_ct(root, "1.0", "Old Header")
  write_vh_ct(root, "1.1", "New Header")
  idx <- index_over(root)

  current <- build_vh_current(idx)

  plan <- app_fn("rebase_plan")(current, "1.1", index = idx)
  expect_true(plan$ok)

  # The template really does move this field between versions -- so the guard
  # is doing work here, not passing vacuously.
  expect_true(any(grepl("^version_history", plan$not_rebased$key)))

  # ...and it reaches neither list a caller can act on.
  expect_false(any(grepl("^version_history", plan$changes$key)))
  expect_false(any(grepl("^version_history", plan$conflicts$key)))

  # The ordinary field still rebases normally, so the exclusion is targeted.
  expect_true("header" %in% plan$changes$key)
})

test_that("rebase_apply() leaves the existing version history intact and appends to it", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_vh_ct(root, "1.0", "Old Header")
  write_vh_ct(root, "1.1", "New Header")
  idx <- index_over(root)

  current <- build_vh_current(idx)
  before <- DTAtools::metadata(current)@version_history
  expect_length(before, 1)

  plan <- app_fn("rebase_plan")(current, "1.1", index = idx)
  applied <- app_fn("rebase_apply")(current, plan, resolutions = list())
  expect_true(applied$ok)
  out <- applied$value

  after <- DTAtools::metadata(out)@version_history
  # Exactly one entry added, and the original is untouched -- a rebase is a new
  # event in this document's history, never a rewrite of what already happened.
  expect_length(after, length(before) + 1L)
  expect_equal(after[[1]], before[[1]])
  # No stub entry named "1" -- the corruption pinned above did not occur.
  expect_null(names(after))
})
