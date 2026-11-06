# Coverage for inst/shiny/dta_app/R/template_history.R -- the version history
# computed by building consecutive versions and diffing them -- and for the
# history section of the picker's detail panel (template_ui.R).

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
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
}

index_over <- function(root, .local_envir = parent.frame()) {
  local_clean_template_env(.local_envir = .local_envir)
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root),
    .local_envir = .local_envir
  )
  app_fn("dta_template_index_invalidate")()
  app_fn("build_template_index")()
}

# One creation template file. `${today}` and `${version}` are in the metadata
# on purpose: they must resolve identically on both sides of a comparison.
write_ct <- function(root, id, version, header = "Header A",
                     columns = "COL_A", options = character(0), extends = NULL) {
  opt_lines <- if (length(options) == 0) {
    "options: []"
  } else {
    c("options:", unlist(lapply(options, function(o) {
      c(sprintf("  - id: %s", o), "    type: text", sprintf("    label: %s", o))
    })))
  }
  body <- if (is.null(extends)) {
    c(
      "base:",
      "  metadata:",
      "    title: Demo",
      '    date: "${today}"',
      sprintf('    header: "%s"', header),
      "    version_history:",
      '      - { version: "${version}", date: "${today}", changes: Initial }',
      "datasets:",
      "  - id: ds1",
      "    name: ds1",
      "    type: tabular",
      "    files: { filename: a.csv, type: csv }",
      "    columns:",
      sprintf("      - { id: %s, label: %s, type: SAS Char }", columns, columns)
    )
  } else {
    sprintf("extends: %s", extends)
  }
  writeLines(
    c(
      "kind: dta_creation_template",
      sprintf("id: %s", id),
      sprintf('version: "%s"', version),
      sprintf("label: %s", id),
      body,
      opt_lines
    ),
    file.path(root, sprintf("%s_%s.dta-template.yaml", id, gsub(".", "_", version, fixed = TRUE)))
  )
}

entry_for <- function(history, type, version) {
  hit <- Filter(function(e) e$type == type && identical(e$version, version), history)
  testthat::expect_length(hit, 1)
  hit[[1]]
}

test_that("a version's entry lists exactly what its author changed, one line per new thing", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1",
    header = "Header B", columns = c("COL_A", "COL_NEW"), options = "extra"
  )
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "demo")
  expect_equal(vapply(history, `[[`, "", "type"), c("version", "initial"))

  ch <- entry_for(history, "version", "1.1")$changes
  expect_null(entry_for(history, "version", "1.1")$error)
  expect_setequal(
    paste(ch$area, ch$what, ch$change, sep = " | "),
    c(
      "Metadata | header | changed",
      "Datasets | Dataset 'ds1', column 'COL_NEW' | added",
      "Template | Option 'extra' | added"
    )
  )
  hdr <- ch[ch$what == "header", ]
  expect_equal(c(hdr$from, hdr$to), c("Header A", "Header B"))
})

test_that("versions with identical content produce an empty entry, placeholders included", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "2.0")
  idx <- index_over(root)

  e <- entry_for(app_fn("template_history")(idx, "demo"), "version", "2.0")
  expect_null(e$error)
  expect_equal(nrow(e$changes), 0)
})

test_that("versions are ordered numerically, not as strings", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.9")
  write_ct(root, "demo", "1.10", header = "Header B")
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "demo")
  expect_equal(history[[1]]$title, "1.10 (compared with 1.9)")
  expect_equal(history[[2]]$type, "initial")
})

test_that("a release of a followed parent appears in the child's history", {
  root <- withr::local_tempdir()
  write_ct(root, "parent", "1.0", header = "Old")
  write_ct(root, "parent", "1.1", header = "New")
  write_ct(root, "child", "1.0", extends = "parent@latest")
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "child")
  up <- Filter(function(e) e$type == "upstream", history)
  expect_length(up, 1)
  expect_equal(up[[1]]$title, "parent 1.0 -> 1.1, followed by child@1.0")
  ch <- up[[1]]$changes
  expect_true(all(c("header", "Based on") %in% ch$what))
  expect_equal(ch$to[ch$what == "header"], "New")
})

test_that("a pinned parent's later releases do not appear", {
  root <- withr::local_tempdir()
  write_ct(root, "parent", "1.0", header = "Old")
  write_ct(root, "parent", "1.1", header = "New")
  write_ct(root, "child", "1.0", extends = "parent@1.0")
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "child")
  expect_equal(vapply(history, `[[`, "", "type"), "initial")
})

test_that("a release of a dataset template imported via @latest appears, as one line per new column", {
  root <- withr::local_tempdir()
  write_dst <- function(version, columns) {
    writeLines(
      c(
        "kind: dta_dataset_template",
        "id: shared_ds",
        sprintf('version: "%s"', version),
        "dataset:",
        "  id: shared",
        "  name: shared",
        "  type: tabular",
        "  files: { filename: s.tsv, type: tsv }",
        "  columns:",
        sprintf("    - { id: %s, label: %s, type: SAS Char }", columns, columns),
        "  rules: []"
      ),
      file.path(root, sprintf("shared_%s.dta-dataset-template.yaml", gsub(".", "_", version, fixed = TRUE)))
    )
  }
  write_dst("1.0", "COL_A")
  write_dst("2.0", c("COL_A", "COL_B"))
  writeLines(
    c(
      "kind: dta_creation_template", "id: user_ct", 'version: "1.0"', "label: User",
      "base:", "  metadata:", "    title: Demo",
      "datasets:", "  - template: shared_ds@latest", "options: []"
    ),
    file.path(root, "user_ct.dta-template.yaml")
  )
  idx <- index_over(root)

  up <- Filter(function(e) e$type == "upstream", app_fn("template_history")(idx, "user_ct"))
  expect_length(up, 1)
  expect_equal(up[[1]]$title, "shared_ds 1.0 -> 2.0, followed by user_ct@1.0")
  ch <- up[[1]]$changes
  expect_equal(
    ch$what[ch$change == "added"],
    "Dataset 'shared', column 'COL_B'"
  )
})

test_that("an option that is nothing but its id is still reported when added", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1")
  # Rewrite 1.1's options block as a bare id.
  f <- file.path(root, "demo_1_1.dta-template.yaml")
  writeLines(c(sub("^options: \\[\\]$", "options:", readLines(f)), "  - id: bare"), f)
  idx <- index_over(root)

  ch <- entry_for(app_fn("template_history")(idx, "demo"), "version", "1.1")$changes
  expect_equal(paste(ch$what, ch$change), "Option 'bare' added")
})

test_that("a version that no longer builds is reported, not dropped", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1", extends = "ghost@1.0")
  idx <- index_over(root)

  e <- entry_for(app_fn("template_history")(idx, "demo"), "version", "1.1")
  expect_null(e$changes)
  expect_match(e$error, "ghost", fixed = TRUE)
})

test_that("the detail panel shows the selected version's changes and the full history", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1", header = "Header B")
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("demo@1.1", index = idx)
  versions <- app_fn("template_id_versions")(idx, "demo")
  html <- paste(as.character(
    app_fn("template_picker_detail_ui")(loaded, versions, "1.1", idx)
  ), collapse = "")

  expect_match(html, "What changed in 1.1", fixed = TRUE)
  expect_match(html, "Header A → Header B", fixed = TRUE)
  expect_match(html, "Full history (2 entries)", fixed = TRUE)
  expect_match(html, "First version of this template.", fixed = TRUE)
})

history_variant_dataset <- function(columns = "COL_A") {
  list(
    id = "ds1", name = "ds1", type = "tabular",
    files = list(filename = "a.csv", type = "csv"),
    columns = lapply(columns, function(id) list(id = id, label = id, type = "SAS Char"))
  )
}

history_variant_definition <- function(version = "1.0") {
  list(
    kind = "dta_creation_template", id = "platforms", version = version,
    base = list(metadata = list(title = "Demo", transmission = list(method = "EDT"))),
    options = list(list(
      id = "medium", type = "text", target = "metadata.transmission.method",
      choices = list("EDT")
    )),
    variants = list(
      id = "trial_platform", label = "Trial platform",
      choices = list(
        list(value = "brave", label = "BRAVE", datasets = list(history_variant_dataset())),
        list(
          value = "veeva", label = "Veeva",
          base = list(metadata = list(transmission = list(method = "CDB"))),
          options = list(list(id = "medium", choices = list("EDT", "CDB"))),
          datasets = list(history_variant_dataset())
        )
      )
    )
  )
}

write_history_definition <- function(root, def, suffix = ".dta-template.yaml") {
  yaml::write_yaml(def, file.path(root, paste0(def$id, "_", def$version, suffix)))
}

test_that("variant history builds every choice and reports only the changed realization", {
  for (change in c("column", "options", "default")) {
    root <- withr::local_tempdir()
    before <- history_variant_definition()
    after <- history_variant_definition("2.0")
    if (change == "column") {
      after$variants$choices[[2]]$datasets <- list(history_variant_dataset(c("COL_A", "COL_NEW")))
    } else if (change == "options") {
      after$variants$choices[[2]]$options[[1]]$choices <- list("EDT", "CDB", "Portal")
    } else {
      after$variants$choices[[2]]$options[[1]]$default <- "EDT"
    }
    write_history_definition(root, before)
    write_history_definition(root, after)
    idx <- index_over(root)

    builds <- lapply(c("1.0", "2.0"), function(version) {
      app_fn("template_history_build")(paste0("platforms@", version), idx)
    })
    expect_true(all(vapply(builds, function(b) isTRUE(b$ok), logical(1))))
    expect_null(builds[[1]]$error)
    expect_null(builds[[2]]$error)
    expect_setequal(names(builds[[1]]$value$variants), paste("trial_platform", c("brave", "veeva"), sep = "\n"))
    expect_identical(
      metadata(builds[[1]]$value$variants[["trial_platform\nbrave"]]$dta)@transmission$method,
      "EDT"
    )
    expect_identical(
      metadata(builds[[1]]$value$variants[["trial_platform\nveeva"]]$dta)@transmission$method,
      "CDB"
    )
    e <- entry_for(app_fn("template_history")(idx, "platforms"), "version", "2.0")
    expect_null(e$error)
    ch <- e$changes
    expect_gt(nrow(ch), 0)
    expect_identical(names(ch), c("area", "what", "change", "from", "to"))
    expect_true(all(startsWith(ch$what, "Variant 'trial_platform', choice 'veeva' (Veeva): ")))
    expect_false(any(grepl("BRAVE|brave", ch$what)))
    if (change == "column") {
      expect_equal(ch$what, "Variant 'trial_platform', choice 'veeva' (Veeva): Dataset 'ds1', column 'COL_NEW'")
      expect_equal(ch$change, "added")
    } else if (change == "options") {
      expect_true(all(ch$area == "Template"))
      expect_true(all(grepl("Option 'medium'", ch$what, fixed = TRUE)))
      expect_equal(ch$from, "EDT, CDB")
      expect_equal(ch$to, "EDT, CDB, Portal")
    } else {
      md <- ch[ch$area == "Metadata", , drop = FALSE]
      expect_equal(c(md$from, md$to), c("CDB", "EDT"))
    }
  }
})

test_that("variant history is unchanged when choices are reordered", {
  root <- withr::local_tempdir()
  before <- history_variant_definition()
  after <- history_variant_definition("2.0")
  after$variants$choices <- rev(after$variants$choices)
  write_history_definition(root, before)
  write_history_definition(root, after)
  idx <- index_over(root)

  e <- entry_for(app_fn("template_history")(idx, "platforms"), "version", "2.0")
  expect_null(e$error)
  expect_equal(nrow(e$changes), 0)
  expect_identical(names(e$changes), c("area", "what", "change", "from", "to"))
})

test_that("variant history tops up vocabulary defaults for every realization", {
  root <- withr::local_tempdir()
  def <- history_variant_definition()
  def$vocabulary_slots <- list(list(
    id = "visits", target = "datasets.ds1.columns.COL_A.values",
    vocabulary = "visit", default = list("SCR"), min = 2
  ))
  write_history_definition(root, def)
  write_history_definition(root, list(
    kind = "dta_vocabulary", id = "visit", version = "1.0", type = "text",
    terms = list(list(code = "SCR", label = "Screening"), list(code = "EOT", label = "End"))
  ), ".dta-vocabulary.yaml")
  idx <- index_over(root)

  built <- app_fn("template_history_build")("platforms@1.0", idx)
  expect_true(built$ok, info = built$error)
  expect_null(built$error)
  expect_length(built$value$variants, 2)
  for (branch in built$value$variants) {
    expect_identical(branch$dta@datasets[[1]]@specs@columns[[1]]@values, c("SCR", "EOT"))
  }
})

test_that("followed dataset releases inside one variant appear only for that branch", {
  root <- withr::local_tempdir()
  def <- history_variant_definition()
  def$variants$choices[[2]]$datasets <- list(list(template = "shared_ds@latest"))
  write_history_definition(root, def)
  for (version in c("1.0", "2.0")) {
    cols <- if (version == "1.0") "COL_A" else c("COL_A", "COL_NEW")
    write_history_definition(root, list(
      kind = "dta_dataset_template", id = "shared_ds", version = version,
      dataset = history_variant_dataset(cols)
    ), ".dta-dataset-template.yaml")
  }
  idx <- index_over(root)

  up <- Filter(function(e) e$type == "upstream", app_fn("template_history")(idx, "platforms"))
  expect_length(up, 1)
  expect_null(up[[1]]$error)
  expect_equal(up[[1]]$title, "shared_ds 1.0 -> 2.0, followed by platforms@1.0")
  expect_setequal(
    up[[1]]$changes$what,
    c(
      "Variant 'trial_platform', choice 'veeva' (Veeva): Dataset 'ds1', column 'COL_NEW'",
      "Variant 'trial_platform', choice 'veeva' (Veeva): Dataset 'ds1': template_version"
    )
  )
  column <- up[[1]]$changes[grepl("column 'COL_NEW'", up[[1]]$changes$what, fixed = TRUE), ]
  provenance <- up[[1]]$changes[grepl(": template_version", up[[1]]$changes$what, fixed = TRUE), ]
  expect_equal(column$change, "added")
  expect_equal(provenance$change, "changed")
  expect_equal(c(provenance$from, provenance$to), c("1.0", "2.0"))
})

test_that("followed vocabulary releases inside one variant appear only for that branch", {
  root <- withr::local_tempdir()
  def <- history_variant_definition()
  def$variants$choices[[2]]$datasets[[1]]$columns[[1]]$values_from <- "visit"
  write_history_definition(root, def)
  for (version in c("1.0", "2.0")) {
    codes <- if (version == "1.0") "SCR" else c("SCR", "EOT")
    write_history_definition(root, list(
      kind = "dta_vocabulary", id = "visit", version = version, type = "text",
      terms = lapply(codes, function(code) list(code = code, label = code))
    ), ".dta-vocabulary.yaml")
  }
  idx <- index_over(root)

  up <- Filter(function(e) e$type == "upstream", app_fn("template_history")(idx, "platforms"))
  expect_length(up, 1)
  expect_null(up[[1]]$error)
  expect_equal(up[[1]]$title, "visit 1.0 -> 2.0, followed by platforms@1.0")
  ch <- up[[1]]$changes
  expect_gt(nrow(ch), 0)
  expect_true(all(ch$area == "Datasets"))
  expect_true(all(startsWith(ch$what, "Variant 'trial_platform', choice 'veeva' (Veeva): ")))
  expect_true(any(grepl("values", ch$what, fixed = TRUE)))
  expect_true(any(grepl("EOT", ch$to, fixed = TRUE)))
  expect_false(any(grepl("BRAVE|brave", ch$what)))
})

test_that("added and removed choices are explicit without diffing unrelated branches", {
  root <- withr::local_tempdir()
  before <- history_variant_definition()
  after <- history_variant_definition("2.0")
  after$variants$choices[[1]]$value <- "rave"
  after$variants$choices[[1]]$label <- "Rave"
  write_history_definition(root, before)
  write_history_definition(root, after)
  idx <- index_over(root)

  e <- entry_for(app_fn("template_history")(idx, "platforms"), "version", "2.0")
  expect_null(e$error)
  expect_setequal(
    paste(e$changes$what, e$changes$change),
    c(
      "Variant 'trial_platform', choice 'brave' (BRAVE) removed",
      "Variant 'trial_platform', choice 'rave' (Rave) added"
    )
  )
  expect_true(all(e$changes$area == "Template"))
})

test_that("ordinary and variant templates can replace each other in history", {
  root <- withr::local_tempdir()
  ordinary <- history_variant_definition()
  ordinary$variants <- NULL
  ordinary$datasets <- list(history_variant_dataset())
  write_history_definition(root, ordinary)
  write_history_definition(root, history_variant_definition("2.0"))
  ordinary$version <- "3.0"
  write_history_definition(root, ordinary)
  idx <- index_over(root)
  history <- app_fn("template_history")(idx, "platforms")

  for (version in c("2.0", "3.0")) {
    e <- entry_for(history, "version", version)
    expect_null(e$error)
    ch <- e$changes
    expect_identical(names(ch), c("area", "what", "change", "from", "to"))
    summaries <- ch[ch$what %in% c(
      "Variant 'trial_platform', choice 'brave' (BRAVE)",
      "Variant 'trial_platform', choice 'veeva' (Veeva)"
    ), , drop = FALSE]
    expect_equal(nrow(summaries), 2)
    expect_true(all(summaries$change == if (version == "2.0") "added" else "removed"))
  }
})

test_that("one broken variant is an explicit build and history error", {
  root <- withr::local_tempdir()
  write_history_definition(root, history_variant_definition())
  broken <- history_variant_definition("2.0")
  broken$variants$choices[[2]]$datasets <- list(list(template = "missing_ds"))
  write_history_definition(root, broken)
  idx <- index_over(root)

  built <- app_fn("template_history_build")("platforms@2.0", idx)
  expect_false(built$ok)
  expect_null(built$value)
  expect_match(built$error, "trial_platform=veeva", fixed = TRUE)
  expect_match(built$error, "missing_ds", fixed = TRUE)
  e <- entry_for(app_fn("template_history")(idx, "platforms"), "version", "2.0")
  expect_null(e$changes)
  expect_match(e$error, "trial_platform=veeva", fixed = TRUE)
})

test_that("ordinary history builds retain their original result and comparison shape", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "2.0")
  idx <- index_over(root)
  a <- app_fn("template_history_build")("demo@1.0", idx)
  b <- app_fn("template_history_build")("demo@2.0", idx)

  expect_true(a$ok, info = a$error)
  expect_true(b$ok, info = b$error)
  expect_identical(names(a$value), c("def", "lineage", "mixins", "dta"))
  ch <- app_fn("template_history_compare")(a$value, b$value)
  expect_identical(names(ch), c("area", "what", "change", "from", "to"))
  expect_equal(nrow(ch), 0)
})

# ---- prior_versions: history from before the library ------------------------

# Append a `prior_versions:` block to a file written by write_ct().
add_prior <- function(root, id, version, lines) {
  path <- file.path(root, sprintf("%s_%s.dta-template.yaml", id, gsub(".", "_", version, fixed = TRUE)))
  write(c("prior_versions:", lines), path, append = TRUE)
}

test_that("prior_versions entries follow the first library version, newest first and exact", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "5.0")
  add_prior(root, "demo", "5.0", c(
    # Unquoted on purpose: 4.10 must not become the number 4.1.
    "  - { version: 3.0, changes: Renamed columns. }",
    "  - { version: 4.10, date: 2024-03-01, changes: Added the PK dataset. }"
  ))
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "demo")
  expect_equal(vapply(history, `[[`, "", "type"), c("initial", "manual", "manual"))
  expect_equal(vapply(history[2:3], `[[`, "", "version"), c("4.10", "3.0"))
  expect_equal(history[[2]]$title, "4.10 (2024-03-01, recorded by hand)")
  expect_equal(history[[2]]$note, "Added the PK dataset.")
  expect_equal(history[[3]]$title, "3.0 (recorded by hand)")
})

test_that("the newest version that declares prior_versions speaks for the past", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "5.0")
  add_prior(root, "demo", "5.0", "  - { version: \"4.0\", changes: Old text. }")
  write_ct(root, "demo", "5.1")
  add_prior(root, "demo", "5.1", "  - { version: \"4.0\", changes: Corrected text. }")
  write_ct(root, "demo", "5.2")
  idx <- index_over(root)

  manual <- Filter(function(e) e$type == "manual", app_fn("template_history")(idx, "demo"))
  expect_length(manual, 1)
  expect_equal(manual[[1]]$note, "Corrected text.")
})

test_that("a child never inherits its parent's prior_versions through extends", {
  root <- withr::local_tempdir()
  write_ct(root, "parent", "1.0")
  add_prior(root, "parent", "1.0", "  - { version: \"0.9\", changes: Parent past. }")
  write_ct(root, "child", "1.0", extends = "parent@1.0")
  idx <- index_over(root)

  expect_equal(vapply(app_fn("template_history")(idx, "child"), `[[`, "", "type"), "initial")
  expect_equal(
    vapply(app_fn("template_history")(idx, "parent"), `[[`, "", "type"), c("initial", "manual")
  )
})

test_that("a malformed or overlapping entry is reported, the good ones still shown", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "4.0")
  write_ct(root, "demo", "5.0")
  add_prior(root, "demo", "5.0", c(
    "  - { version: \"3.0\", changes: Fine. }",
    "  - { version: \"4.0\", changes: Already in the library. }",
    "  - { version: \"2.0\" }",
    "  - { version: \"1.0\", date: March, changes: Bad date. }"
  ))
  idx <- index_over(root)

  manual <- Filter(function(e) e$type == "manual", app_fn("template_history")(idx, "demo"))
  expect_equal(vapply(manual, function(e) e$version, ""), c("3.0", NA_character_))
  err <- manual[[2]]$error
  expect_match(err, "entry 2 (4.0) is not older than 4.0", fixed = TRUE)
  expect_match(err, "entry 3 (2.0) has no changes text", fixed = TRUE)
  expect_match(err, "entry 4 (1.0): date 'March'", fixed = TRUE)
  expect_equal(manual[[2]]$title, "Earlier versions (prior_versions in demo@5.0)")
})

test_that("the detail panel shows hand-written entries and says where the library starts", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "5.0")
  add_prior(root, "demo", "5.0", "  - { version: \"4.0\", date: 2024-03-01, changes: Added the PK dataset. }")
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("demo@5.0", index = idx)
  versions <- app_fn("template_id_versions")(idx, "demo")
  html <- paste(as.character(
    app_fn("template_picker_detail_ui")(loaded, versions, "5.0", idx)
  ), collapse = "")

  expect_match(html, "Full history (2 entries)", fixed = TRUE)
  expect_match(html, "First version in the template library.", fixed = TRUE)
  expect_match(html, "4.0 (2024-03-01, recorded by hand)", fixed = TRUE)
  expect_match(html, "Added the PK dataset.", fixed = TRUE)
})

test_that("an empty prior_versions in a newer file does not hide an older file's entries", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "5.0")
  add_prior(root, "demo", "5.0", "  - { version: \"4.0\", changes: Kept. }")
  write_ct(root, "demo", "5.1")
  write("prior_versions: []", file.path(root, "demo_5_1.dta-template.yaml"), append = TRUE)
  idx <- index_over(root)

  manual <- Filter(function(e) e$type == "manual", app_fn("template_history")(idx, "demo"))
  expect_equal(vapply(manual, function(e) e$note, ""), "Kept.")
})

test_that("prior_versions keys match exactly and dates must be YYYY-MM-DD", {
  root <- withr::local_tempdir()
  writeLines(c(
    "prior_versions:",
    # `change` must not be read as `changes` through partial matching.
    "  - { version: \"3.0\", change: Misspelt. }",
    "  - { version: \"2.0\", date: 2024-3-1, changes: Short date. }",
    "  - just a string"
  ), path <- file.path(root, "p.yaml"))

  res <- app_fn("template_prior_versions")(path, before = "5.0")
  expect_length(res$entries, 0)
  expect_true(any(grepl("entry 1 (3.0) has no changes text", res$problems, fixed = TRUE)))
  expect_true(any(grepl("entry 1 (3.0) has unknown key(s): change", res$problems, fixed = TRUE)))
  expect_true(any(grepl("entry 2 (2.0): date '2024-3-1'", res$problems, fixed = TRUE)))
  expect_true(any(grepl("entry 3 is not a mapping", res$problems, fixed = TRUE)))
})
