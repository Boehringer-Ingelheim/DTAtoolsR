# Coverage for the biomarker_gf template FAMILY as it actually ships in
# inst/extdata/templates/: the gf_smrnaseq dataset template extracted out of
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

Sys.setenv(NOT_CRAN = "true")

# Local copy of the isolation helper defined in test-shinyapp-template-
# sources.R / test-shinyapp-template-create.R -- deliberately duplicated
# rather than shared, per those files' own guidance, so this file does not
# depend on another test file's internals. Every DTATOOLS_TEMPLATE_* variable
# is cleared for the duration of each test: a developer machine with
# DTATOOLS_TEMPLATE_SOURCES exported would flip the app into private-only mode
# (dta_template_source_roots()'s "private replaces public" contract) and
# silently drop the packaged directory these tests are all about.
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

# The real, installed/loaded package templates directory -- never a temp
# fixture -- because this whole file is about what actually ships.
bundled_templates_dir <- function() {
  dir <- system.file("extdata", "templates", package = "DTAtools")
  expect_true(nzchar(dir), info = "inst/extdata/templates missing from the package")
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
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", bundled_templates_dir()))
  app_fn("build_template_index")()
}

# ---- Equivalence: the entire justification for the refactor -----------------

test_that("biomarker_gf, rebuilt through the dataset-template import, is equivalent to the pre-refactor inline template", {
  # This is not a convenience skip: every CI runner and every dev checkout of
  # this repository is a git checkout with git on PATH. The only environment
  # where this could legitimately be missing is a source tarball with no
  # .git/ at all, which is not how this suite is ever run.
  skip_if_not(nzchar(Sys.which("git")), "git is not available on PATH")
  root <- testthat::test_path("..", "..")
  # `.git` is a DIRECTORY in an ordinary clone but a plain FILE (a "gitdir:
  # ../.git/worktrees/<name>" pointer) inside a git worktree -- file.exists()
  # is true for both, dir.exists() only for the former, which would wrongly
  # skip this test in every worktree checkout.
  skip_if_not(file.exists(file.path(root, ".git")), "not a git checkout")

  # Find the newest revision of this file that STILL had the inline dataset.
  #
  # This used to read HEAD, which was correct exactly until the refactor was
  # committed. After that, HEAD IS the refactored file, so the test compared it
  # against itself -- passing the equality vacuously and failing the "these two
  # must differ" provenance assertion. Pinning a literal SHA would not survive
  # either, since releases reach master as squash merges.
  #
  # So walk the file's own history and take the newest revision that still
  # carries the hand-typed marker and has no `template:` import entry.
  rel <- "inst/extdata/templates/biomarker_gf.dta-template.yaml"
  shas <- suppressWarnings(system2(
    "git", shQuote(c("-C", root, "log", "--format=%H", "--", rel)),
    stdout = TRUE, stderr = FALSE
  ))
  skip_if(length(shas) == 0, "no git history for the bundled template in this clone")

  orig_lines <- NULL
  for (sha in utils::head(shas, 50L)) {
    candidate <- suppressWarnings(system2(
      "git", shQuote(c("-C", root, "show", paste0(sha, ":", rel))),
      stdout = TRUE, stderr = FALSE
    ))
    if (!identical(as.integer(attr(candidate, "status") %||% 0L), 0L)) next
    is_inline <- any(grepl("GF domain smrnaseq", candidate, fixed = TRUE)) &&
      !any(grepl("^[[:space:]]*-[[:space:]]*template:", candidate))
    if (is_inline) {
      orig_lines <- candidate
      break
    }
  }
  skip_if(
    is.null(orig_lines),
    "pre-refactor template not reachable (shallow clone or squashed history)"
  )
  expect_gt(length(orig_lines), 0)

  orig_path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(orig_lines, orig_path)

  read_tpl <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")
  to_list_fn <- app_fn("dta_to_list")

  # OLD: the exact template content this package shipped at the tip of this
  # branch's history, before the refactor -- a fully inline `datasets:` block,
  # built with the ORIGINAL no-index code path (create_dta_from_template()
  # never needs an index for a template with no `template:` dataset entry).
  old_parsed <- read_tpl(orig_path)
  expect_true(old_parsed$ok, info = old_parsed$error %||% "")
  old_res <- create_fn(old_parsed$value, orig_path, selections = list())
  expect_true(old_res$ok, info = old_res$error %||% "")
  old_list <- to_list_fn(old_res$value)

  # NEW: the bundled, refactored template -- `datasets:` now a single
  # `template:`/`as:` import entry, resolved via the on-demand index (no
  # `index=` supplied here either; see the dedicated "no index" test below for
  # that behaviour in isolation).
  new_path <- system.file(
    "extdata", "templates", "biomarker_gf.dta-template.yaml",
    package = "DTAtools"
  )
  new_parsed <- read_tpl(new_path)
  expect_true(new_parsed$ok, info = new_parsed$error %||% "")
  new_res <- create_fn(new_parsed$value, new_path, selections = list())
  expect_true(new_res$ok, info = new_res$error %||% "")
  new_list <- to_list_fn(new_res$value)

  # The document metadata is untouched by this refactor entirely -- base,
  # options and their effects never moved.
  expect_equal(new_list$metadata, old_list$metadata)

  expect_length(old_list$datasets, 1)
  expect_length(new_list$datasets, 1)
  old_ds <- old_list$datasets[[1]]
  new_ds <- new_list$datasets[[1]]

  # Same set of top-level dataset fields on both sides -- nothing dropped,
  # nothing invented.
  expect_setequal(names(old_ds), names(new_ds))

  # The three provenance fields are the ONLY permitted difference: they are
  # now machine-stamped by build_dataset_from_template() (dataset_template.R)
  # from the dataset template's own id/version/date, rather than hand-typed
  # into the creation template's inline `datasets:` entry.
  prov_fields <- c("template_source", "template_version", "template_date")
  expect_equal(old_ds$template_source, "GF domain smrnaseq")
  expect_equal(new_ds$template_source, "gf_smrnaseq")
  expect_false(identical(old_ds$template_source, new_ds$template_source))
  # version and date happen to carry the same literal text in both places
  # (the dataset template's own header states the identical version/date the
  # inline entry used to hand-type) but are excluded from the equality below
  # on principle, not because they coincidentally still match -- machine-
  # stamped provenance is never asserted via the same "must be identical"
  # rule as author-controlled specification content.
  expect_equal(old_ds$template_version, "3.0")
  expect_equal(new_ds$template_version, "3.0")
  expect_equal(old_ds$template_date, "2024-12-17")
  expect_equal(new_ds$template_date, "2024-12-17")

  strip_provenance <- function(d) d[setdiff(names(d), prov_fields)]

  # Column ids, in order, are identical.
  old_col_ids <- vapply(old_ds$columns, function(c) c$id, character(1))
  new_col_ids <- vapply(new_ds$columns, function(c) c$id, character(1))
  expect_identical(old_col_ids, new_col_ids)
  expect_length(old_col_ids, 33)

  # Every column's FULL spec -- label, type, format, nullable, description,
  # pattern, values, examples, everything dta_column_to_list() emits -- is
  # identical, one column at a time, so a mismatch names the offending column
  # id directly rather than a wall of nested list diff.
  for (i in seq_along(old_ds$columns)) {
    expect_identical(
      new_ds$columns[[i]], old_ds$columns[[i]],
      info = sprintf("column '%s' differs after the refactor", old_ds$columns[[i]]$id)
    )
  }

  # files: and rules: are untouched.
  expect_identical(new_ds$files, old_ds$files)
  expect_identical(new_ds$rules, old_ds$rules)

  # And, as a single summary assertion: the whole dataset section is
  # identical once the three provenance fields are excluded.
  expect_identical(strip_provenance(new_ds), strip_provenance(old_ds))
})

# ---- Dataset-template resolution + provenance stamping ----------------------

test_that("the gf_smrnaseq dataset template resolves via the index and stamps provenance on the built DTADataSet", {
  idx <- bundled_templates_index()
  row <- app_fn("resolve_template_ref")(idx, "gf_smrnaseq@3.0", kind = "dta_dataset_template")
  expect_false(is.null(row))
  expect_equal(row$id[[1]], "gf_smrnaseq")
  expect_equal(row$version[[1]], "3.0")

  read_tpl <- app_fn("read_dta_creation_template")
  create_fn <- app_fn("create_dta_from_template")
  path <- file.path(bundled_templates_dir(), "biomarker_gf.dta-template.yaml")
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
    "extdata", "templates", "biomarker_gf.dta-template.yaml",
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
  path <- file.path(bundled_templates_dir(), "biomarker_gf.dta-template.yaml")
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
  ds_tpl_path <- file.path(bundled_templates_dir(), "gf_smrnaseq.dta-dataset-template.yaml")
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
  supplier <- read_party(file.path(bundled_templates_dir(), "supplier_acme.dta-party.yaml"))
  receiver <- read_party(file.path(bundled_templates_dir(), "receiver_ourco.dta-party.yaml"))
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

  path <- file.path(bundled_templates_dir(), "biomarker_gf.dta-template.yaml")
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

test_that("validate_template() reports zero severity-'error' rows for the packaged templates directory", {
  result <- validate_template(bundled_templates_dir())
  errors <- result[result$severity == "error", , drop = FALSE]
  expect_equal(
    nrow(errors), 0,
    info = paste(utils::capture.output(print(errors)), collapse = "\n")
  )
})
