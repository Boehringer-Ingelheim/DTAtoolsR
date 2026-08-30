# `@latest` references: a supplier deviation should follow the current standard
# without being edited every time the standard is published.
#
# The behaviour has two halves, and both matter:
#   - a NEW document built from the deviation picks up the newest parent;
#   - an ALREADY-BUILT document does not move, because creation resolves
#     `@latest` once and records the concrete version it chose in
#     metadata.template. A finished specification stays pinned to what it was
#     actually built from.

local_clean_template_env <- function(..., .local_envir = parent.frame()) {
  withr::local_envvar(
    c(
      list(
        DTATOOLS_TEMPLATE_SOURCES = NA,
        DTATOOLS_TEMPLATE_GIT_TOKEN = NA,
        DTATOOLS_TEMPLATE_GIT_USER = NA,
        DTATOOLS_TEMPLATE_GIT_AUTH = NA,
        DTATOOLS_TEMPLATE_CACHE_DIR = NA,
        DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
        DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA
      ),
      list(...)
    ),
    .local_envir = .local_envir
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
  app_fn("dta_template_index_invalidate")()
}

write_base_ct <- function(root, version, title) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: base_ct",
      sprintf('version: "%s"', version),
      "label: Base CT",
      "base:",
      "  metadata:",
      sprintf('    title: "%s"', title),
      '    header: "Base header"',
      "datasets:",
      "  - name: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }",
      "options: []"
    ),
    file.path(root, sprintf("base_%s.dta-template.yaml", gsub("[.]", "_", version)))
  )
}

write_deviation <- function(root, ref) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: acme_ct",
      'version: "1.0"',
      "label: ACME deviation",
      sprintf("extends: %s", ref),
      "options:",
      "  - id: header",
      "    type: text",
      "    target: metadata.header",
      '    default: "ACME Labs"'
    ),
    file.path(root, "acme.dta-template.yaml")
  )
}

index_over <- function(root) {
  app_fn("build_template_index")()
}

test_that("resolve_template_ref() treats @latest and a bare id identically", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_base_ct(root, "1.0", "v1.0")
  write_base_ct(root, "1.10", "v1.10")
  withr::local_envvar(c(DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root)))
  app_fn("dta_template_index_invalidate")()
  idx <- index_over(root)

  fn <- app_fn("resolve_template_ref")
  # 1.10 is NEWER than 1.9/1.0 -- a string sort would pick 1.9, which is the
  # whole reason versions are ranked with numeric_version().
  expect_equal(fn(idx, "base_ct@latest")$version, "1.10")
  expect_equal(fn(idx, "base_ct")$version, "1.10")
  expect_equal(fn(idx, "base_ct@1.0")$version, "1.0")
})

test_that("extends: <id>@latest inherits from the newest parent", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_base_ct(root, "1.0", "Title from 1.0")
  write_base_ct(root, "1.10", "Title from 1.10")
  write_deviation(root, "base_ct@latest")
  withr::local_envvar(c(DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root)))
  app_fn("dta_template_index_invalidate")()
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("acme_ct@1.0", index = idx)

  expect_true(loaded$ok)
  expect_equal(loaded$value$def$base$metadata$title, "Title from 1.10")
  # Lineage records the RESOLVED version, never the literal "@latest" -- rebase
  # reconstructs a document's ancestor from this, and "@latest" would name a
  # moving target rather than the template the document actually came from.
  expect_equal(loaded$value$lineage, "base_ct@1.10")
})

test_that("publishing a newer parent moves @latest without editing the deviation", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_base_ct(root, "1.0", "Title from 1.0")
  write_deviation(root, "base_ct@latest")
  withr::local_envvar(c(DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root)))

  app_fn("dta_template_index_invalidate")()
  first <- app_fn("load_template_definition")("acme_ct@1.0", index = index_over(root))
  expect_equal(first$value$lineage, "base_ct@1.0")

  # Publish a newer standard. The deviation file is NOT touched -- this is the
  # requirement: a supplier template must not need editing every time the base
  # template is updated.
  deviation_before <- readLines(file.path(root, "acme.dta-template.yaml"))
  write_base_ct(root, "1.1", "Title from 1.1")
  app_fn("dta_template_index_invalidate")()

  second <- app_fn("load_template_definition")("acme_ct@1.0", index = index_over(root))

  expect_equal(second$value$lineage, "base_ct@1.1")
  expect_equal(second$value$def$base$metadata$title, "Title from 1.1")
  expect_equal(readLines(file.path(root, "acme.dta-template.yaml")), deviation_before)
})

test_that("a pinned extends does NOT move when a newer parent is published", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_base_ct(root, "1.0", "Title from 1.0")
  write_deviation(root, "base_ct@1.0")
  withr::local_envvar(c(DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root)))

  # The counterpart to the test above: pinning must still mean pinned, so a
  # deviation agreed against one release of a standard stays there until it is
  # deliberately re-agreed.
  write_base_ct(root, "1.1", "Title from 1.1")
  app_fn("dta_template_index_invalidate")()

  loaded <- app_fn("load_template_definition")("acme_ct@1.0", index = index_over(root))

  expect_equal(loaded$value$lineage, "base_ct@1.0")
  expect_equal(loaded$value$def$base$metadata$title, "Title from 1.0")
})

test_that("a document built from an @latest deviation records the concrete parent version", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  write_base_ct(root, "1.0", "Title from 1.0")
  write_base_ct(root, "1.1", "Title from 1.1")
  write_deviation(root, "base_ct@latest")
  withr::local_envvar(c(DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root)))
  app_fn("dta_template_index_invalidate")()
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("acme_ct@1.0", index = idx)
  prov <- app_fn("template_provenance")(loaded$value$def, loaded$value, selections = list())
  built <- app_fn("create_dta_from_template")(
    loaded$value$def, loaded$value$path,
    selections = list(), index = idx, provenance = prov
  )
  expect_true(built$ok)

  tpl <- S7::prop(DTAtools::metadata(built$value), "template")
  # The finished document is pinned to what it was actually built from, so it
  # does not silently mean something different once the parent moves again.
  expect_equal(tpl$lineage, "base_ct@1.1")
  expect_false(any(grepl("latest", unlist(tpl), fixed = TRUE)))
})

test_that("a dataset import may also use @latest", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  ds <- function(version, label) {
    writeLines(
      c(
        "kind: dta_dataset_template",
        "id: mini_dst",
        sprintf('version: "%s"', version),
        sprintf("label: %s", label),
        "dataset:",
        "  name: mini_ds",
        "  type: tabular",
        "  files: { filename: mini.csv, type: csv }",
        "  columns:",
        "    - { id: SUBJID, label: Subject, type: SAS Char, format: SAS $20. }"
      ),
      file.path(root, sprintf("mini_%s.dta-dataset-template.yaml", gsub("[.]", "_", version)))
    )
  }
  ds("1.0", "Mini v1")
  ds("2.0", "Mini v2")
  writeLines(
    c(
      "kind: dta_creation_template", "id: uses_latest_ds", 'version: "1.0"',
      "label: Uses latest dataset", "base:", "  metadata:", '    title: "T"',
      "datasets:", "  - template: mini_dst@latest", "options: []"
    ),
    file.path(root, "uses.dta-template.yaml")
  )
  withr::local_envvar(c(DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root)))
  app_fn("dta_template_index_invalidate")()
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("uses_latest_ds@1.0", index = idx)
  built <- app_fn("create_dta_from_template")(
    loaded$value$def, loaded$value$path,
    selections = list(), index = idx
  )

  expect_true(built$ok)
  ds_obj <- DTAtools::datasets(built$value)[[1]]
  # Stamped with the version that was actually resolved, not the literal ref.
  expect_equal(ds_obj@template_version, "2.0")
})

test_that("the packaged ACME deviation follows the current biomarker_gf", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  idx <- app_fn("build_template_index")()

  loaded <- app_fn("load_template_definition")("biomarker_gf_acme", index = idx)

  expect_true(loaded$ok)
  # The shipped worked example uses `extends: biomarker_gf@latest`, so it does
  # not need editing when biomarker_gf is republished -- and the lineage it
  # reports is the concrete version, not the reference it was written with.
  expect_match(loaded$value$lineage, "^biomarker_gf@[0-9]")
  expect_false(grepl("latest", loaded$value$lineage, fixed = TRUE))
  # ...and it really did inherit, rather than restating the parent.
  expect_true(length(loaded$value$def$datasets) > 0)
  expect_false(is.null(loaded$value$def$base$metadata$title))
})
