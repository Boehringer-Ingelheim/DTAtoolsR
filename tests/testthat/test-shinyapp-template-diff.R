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
        id = "records", type = "tabular",
        columns = list(list(
          id = "SUBJECT", type = "SAS Char",
          nullable = identical(version, "2.0")
        ))
      )),
      options = list()
    )
    yaml::write_yaml(def, lib_path(root, paste0(version, ".dta-template.yaml")))
  }
  index <- app_fn("build_template_index")()
  loaded <- app_fn("load_template_definition")("atomic_agreement@1.0", index)
  expect_true(loaded$ok, info = loaded$error)
  provenance <- app_fn("template_provenance")(loaded$value$def, loaded$value)
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

# ---- Containers in the dataset leaves ---------------------------------------------

diff_ctr_dta <- function(ref = NULL, containers = list()) {
  DTAtools::DTA(
    datasets = list(DTAtools::DTADataSetTabular(
      id = "ae", specs = DTAtools::create_example_DTAColumnSpecCollection(1),
      files = list(DTAtools::DTAFileFactory(type = "csv", filename = "ae.csv", container = ref))
    )),
    metadata = DTAtools::create_example_DTAMetaData(),
    containers = containers
  )
}

test_that("dataset leaves carry the entry's own reference resolved to its file name", {
  leaves <- app_fn("dta_dataset_leaves")
  ctr <- list(DTAtools::DTAContainer("adam", "adam.zip", default = TRUE))
  # An id is written as the file name, so an id and the file name it stands for
  # are the same leaf (a materialised implied container is no change).
  expect_identical(leaves(diff_ctr_dta("adam", ctr))[["ae.files.container"]], "adam.zip")
  expect_identical(leaves(diff_ctr_dta("adam.zip"))[["ae.files.container"]], "adam.zip")
  expect_identical(leaves(diff_ctr_dta("none", ctr))[["ae.files.container"]], "none")
  # The default container is NOT applied to an entry without its own reference.
  expect_null(leaves(diff_ctr_dta(NULL, ctr))[["ae.files.container"]])
})

test_that("dta_diff() has a containers section named by file name", {
  a <- diff_ctr_dta(NULL, list(DTAtools::DTAContainer("adam", "adam.zip")))
  b <- diff_ctr_dta(NULL, list(
    DTAtools::DTAContainer("adam", "adam.zip", description = "ADaM"),
    DTAtools::DTAContainer("sdtm", "sdtm.zip")
  ))
  d <- app_fn("dta_diff")(a, b)
  expect_setequal(d$containers$key, c("adam.zip.description", "sdtm.zip.filename"))
  expect_identical(d$containers$change[d$containers$key == "adam.zip.description"], "added")
  expect_identical(nrow(app_fn("dta_diff")(a, a)$containers), 0L)
})

# ---- Two sides with no leaves at all -----------------------------------------

# An empty leaf map has NULL names, so the union of two is NULL and
# sort(NULL, method = "radix") used to abort with "not a vector". Expected
# frames are spelled out here rather than read back from the app's own
# empty-frame helpers.
test_that("a diff of two empty leaf maps is the empty change frame, not an error", {
  expect_identical(
    app_fn("dta_diff_two_way")(list(), list()),
    data.frame(
      key = character(0), change = character(0),
      from = character(0), to = character(0),
      stringsAsFactors = FALSE
    )
  )
})

test_that("dta_diff() of two documents with no metadata fields and no datasets reports nothing", {
  empty <- function() DTAtools::DTA(datasets = list(), metadata = DTAtools::DTAMetaData())
  d <- app_fn("dta_diff")(empty(), empty())
  expect_named(d, c("metadata", "containers", "datasets"))
  for (section in d) {
    expect_identical(names(section), c("key", "change", "from", "to"))
    expect_identical(nrow(section), 0L)
  }
})

test_that("rebase classification of three empty leaf maps yields empty change and conflict frames", {
  conflicts <- data.frame(
    key = character(0), ancestor = character(0),
    current = character(0), target = character(0),
    stringsAsFactors = FALSE
  )
  changes <- data.frame(
    key = character(0), resolution = character(0),
    ancestor = character(0), current = character(0), target = character(0),
    stringsAsFactors = FALSE
  )
  expect_identical(
    app_fn(".rebase_classify_three_way")(list(), list(), list()),
    list(changes = changes, conflicts = conflicts)
  )
  expect_identical(
    app_fn(".rebase_classify_two_way")(list(), list()),
    list(changes = changes, conflicts = conflicts)
  )
})
