# Ordered agreement sections are a single rebase value, unlike the ordinary
# nested metadata fields. All fixtures are synthetic.
atomic_rebase_sections <- function() {
  list(
    scope = list(title = "Scope", text = "First line\n\nSecond line\n"),
    review = list(title = "Review", text = "")
  )
}

atomic_rebase_family <- function(ancestor, target, .local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root),
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false",
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA,
      DTATOOLS_TEMPLATE_GIT_TOKEN = NA,
      DTATOOLS_TEMPLATE_GIT_USER = NA,
      DTATOOLS_TEMPLATE_GIT_AUTH = NA
    ),
    .local_envir = .local_envir
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
  collections <- list("1.0" = ancestor, "2.0" = target)
  for (version in names(collections)) {
    def <- list(
      kind = "dta_creation_template", id = "atomic_agreement", version = version,
      label = "Atomic agreement",
      base = list(metadata = list(
        title = "Agreement", version = "1.0", sections = collections[[version]]
      )),
      datasets = list(list(
        name = "records", type = "tabular",
        columns = list(list(
          id = "SUBJECT", type = "SAS Char",
          nullable = identical(version, "2.0")
        ))
      )),
      options = list()
    )
    yaml::write_yaml(def, file.path(root, paste0(version, ".dta-template.yaml")))
  }
  index <- app_fn("build_template_index")()
  loaded <- app_fn("load_template_definition")("atomic_agreement@1.0", index)
  expect_true(loaded$ok, info = loaded$error)
  provenance <- app_fn("template_provenance")(loaded$value$def, loaded$value, selections = list())
  current <- app_fn("create_dta_from_template")(
    loaded$value$def, loaded$value$path, index = index, provenance = provenance
  )
  expect_true(current$ok, info = current$error)
  list(current = current$value, index = index)
}

test_that("metadata leaves preserve sections as one ordered value and flatten other fields unchanged", {
  md <- DTAMetaData(
    title = "Agreement", study = list(id = "S"),
    supplier = list(affiliation = list(name = "Supplier")),
    transmission = list(security = "Encrypted"),
    sections = atomic_rebase_sections(),
    template = list(id = "machine", version = "1.0")
  )
  leaves <- app_fn("dta_metadata_leaves")(md)
  expect_identical(leaves$sections, md@sections)
  expect_false(any(startsWith(names(leaves), "sections.")))
  ordinary <- as.list(md)
  ordinary$sections <- NULL
  ordinary$template <- NULL
  expect_identical(
    leaves[names(leaves) != "sections"],
    app_fn(".dta_leaf_flatten")(ordinary)
  )
  expect_false(any(startsWith(names(leaves), "template")))
  expect_false("sections" %in% names(app_fn("dta_metadata_leaves")(DTAMetaData())))
})

test_that("a section reorder is visible in the document diff as one collection change", {
  current <- app_fixture_dta()
  current@metadata@sections <- atomic_rebase_sections()
  target <- current
  target@metadata@sections <- rev(target@metadata@sections)
  difference <- app_fn("dta_diff")(current, target)
  expect_identical(difference$metadata$key, "sections")
  expect_identical(difference$metadata$change, "changed")
  expect_identical(
    difference$metadata$from,
    app_fn("dta_diff_display_value")(current@metadata@sections)
  )
  expect_identical(
    difference$metadata$to,
    app_fn("dta_diff_display_value")(target@metadata@sections)
  )
  expect_equal(nrow(difference$datasets), 0)
})

for (action in c("add", "remove", "reorder", "replace")) {
  test_that(paste("rebase can", action, "complete sections without changing datasets"), {
    sections <- atomic_rebase_sections()
    case <- switch(action,
      add = list(ancestor = list(), target = sections),
      remove = list(ancestor = sections, target = list()),
      reorder = list(ancestor = sections, target = rev(sections)),
      replace = list(
        ancestor = sections,
        target = list(delivery = list(title = "Delivery", text = "Replacement\nclause"))
      )
    )
    fixture <- atomic_rebase_family(case$ancestor, case$target)
    current <- fixture$current
    before <- current
    plan <- app_fn("rebase_plan")(current, "2.0", fixture$index)
    expect_true(plan$ok, info = plan$error)
    expect_identical(plan$changes$key, "sections")
    expect_identical(plan$changes$resolution, "take_template")
    expect_equal(nrow(plan$conflicts), 0)
    expect_true(plan$datasets_reportable_only)
    expect_gt(nrow(plan$dataset_changes), 0)

    applied <- app_fn("rebase_apply")(current, plan)
    expect_true(applied$ok, info = applied$error)
    expect_identical(applied$value@metadata@sections, case$target)
    expect_identical(names(applied$value@metadata@sections), names(case$target))
    expect_identical(applied$value@datasets, current@datasets)
    expect_identical(current, before)
    expect_identical(applied$value@metadata@template, plan$target_provenance)
    expect_length(applied$value@metadata@version_history, 1)
    if (length(case$target) == 0) {
      expect_false("sections" %in% names(as.list(applied$value@metadata)))
    }
  })
}

test_that("disjoint clause edits require an explicit whole-collection conflict choice", {
  ancestor <- atomic_rebase_sections()
  target <- ancestor
  target$review$text <- "Target review clause"
  fixture <- atomic_rebase_family(ancestor, target)
  current <- fixture$current
  current@metadata@sections$scope$text <- "Current scope clause"
  current@metadata@sections <- rev(current@metadata@sections)
  before <- current
  plan <- app_fn("rebase_plan")(current, "2.0", fixture$index)
  expect_true(plan$ok, info = plan$error)
  expect_identical(plan$conflicts$key, "sections")
  expect_equal(nrow(plan$changes), 0)

  for (resolutions in list(list(), list(sections = "merge"))) {
    failed <- app_fn("rebase_apply")(current, plan, resolutions)
    expect_false(failed$ok)
    expect_match(failed$error, "sections")
    expect_null(failed$value)
    expect_identical(current, before)
  }
  for (choice in c("current", "target")) {
    applied <- app_fn("rebase_apply")(current, plan, list(sections = choice))
    expect_true(applied$ok, info = applied$error)
    expected <- if (choice == "current") current@metadata@sections else target
    expect_identical(applied$value@metadata@sections, expected)
    expect_identical(applied$value@datasets, before@datasets)
    expect_identical(current, before)
  }
})

test_that("a removal conflict retains or removes the entire optional collection explicitly", {
  fixture <- atomic_rebase_family(atomic_rebase_sections(), list())
  current <- fixture$current
  current@metadata@sections$scope$text <- "Keep my wording"
  plan <- app_fn("rebase_plan")(current, "2.0", fixture$index)
  expect_true(plan$ok, info = plan$error)
  expect_identical(plan$conflicts$key, "sections")
  kept <- app_fn("rebase_apply")(current, plan, list(sections = "current"))
  removed <- app_fn("rebase_apply")(current, plan, list(sections = "target"))
  expect_true(kept$ok, info = kept$error)
  expect_true(removed$ok, info = removed$error)
  expect_identical(kept$value@metadata@sections, current@metadata@sections)
  expect_identical(removed$value@metadata@sections, list())
  expect_false("sections" %in% names(as.list(removed$value@metadata)))
})

test_that("current-only and identical section changes keep existing three-way precedence", {
  ancestor <- atomic_rebase_sections()
  for (same_target in c(FALSE, TRUE)) {
    target <- if (same_target) rev(ancestor) else ancestor
    fixture <- atomic_rebase_family(ancestor, target)
    current <- fixture$current
    current@metadata@sections <- rev(ancestor)
    plan <- app_fn("rebase_plan")(current, "2.0", fixture$index)
    expect_true(plan$ok, info = plan$error)
    expect_identical(plan$changes$key, "sections")
    expect_identical(plan$changes$resolution, if (same_target) "agree" else "keep_user")
    expect_equal(nrow(plan$conflicts), 0)
    applied <- app_fn("rebase_apply")(current, plan)
    expect_true(applied$ok, info = applied$error)
    expect_identical(applied$value@metadata@sections, current@metadata@sections)
  }
})

test_that("failure after a section write still leaves the original document unchanged", {
  fixture <- atomic_rebase_family(list(), atomic_rebase_sections())
  current <- fixture$current
  before <- current
  plan <- app_fn("rebase_plan")(current, "2.0", fixture$index)
  expect_true(plan$ok, info = plan$error)
  plan$target_provenance <- list(id = "", version = "2.0")
  failed <- app_fn("rebase_apply")(current, plan)
  expect_false(failed$ok)
  expect_match(failed$error, "template.*id")
  expect_null(failed$value)
  expect_identical(current, before)
})
