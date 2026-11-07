# Coverage for inst/shiny/dta_app/R/template_index.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why the
# app's helper files must be sourced into a private environment for testing).

# Local copy of the isolation helper defined in
# test-shinyapp-template-sources.R -- deliberately duplicated rather than
# shared, per that file's own guidance, so this file does not depend on
# another test file's internals. Every DTATOOLS_TEMPLATE_* variable is
# cleared for the duration of each test: a developer machine with
# DTATOOLS_TEMPLATE_SOURCES exported would flip the app into private-only
# mode and fail unrelated tests confusingly.
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

# The column set build_template_index() promises, spelled out literally here
# (rather than by calling the app's own dta_template_index_columns()) so this
# is a black-box check against the specification, not a check that the
# implementation agrees with itself.
expected_index_columns <- function() {
  c(
    "kind", "id", "version", "label", "description", "path",
    "source_name", "source_kind", "resolved_commit", "abstract", "extends"
  )
}

# Write a minimal template fixture file. `version` is inserted UNQUOTED and
# verbatim -- callers rely on that to exercise the YAML numeric-coercion trap
# (an unquoted `version: 1.0` parses, by default, as the double 1).
write_template_fixture <- function(dir, filename, kind, id, version, label = NULL,
                                   extra_lines = character(0)) {
  lines <- c(
    sprintf("kind: %s", kind),
    sprintf("id: %s", id),
    sprintf("version: %s", version)
  )
  if (!is.null(label)) {
    lines <- c(lines, sprintf("label: %s", label))
  }
  # `dir` is the library ROOT; the file lands in its kind's folder.
  kind_dir <- file.path(dir, app_fn("dta_template_kind_subdir")(kind))
  dir.create(kind_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(c(lines, extra_lines), file.path(kind_dir, filename))
}

# ---- dta_template_kind_pattern() --------------------------------------------

test_that("dta_template_kind_pattern() for creation templates excludes dataset-template files", {
  fn <- app_fn("dta_template_kind_pattern")
  pat <- fn("dta_creation_template")

  expect_true(grepl(pat, "foo.dta-template.yaml"))
  expect_true(grepl(pat, "foo.dta-template.yml"))
  # The trap named in the spec: a dataset template's longer suffix must NOT
  # satisfy the shorter creation-template pattern.
  expect_false(grepl(pat, "foo.dta-dataset-template.yaml"))
  expect_false(grepl(pat, "foo.dta-dataset-template.yml"))
})

test_that("dta_template_kind_pattern() matches each kind's own suffix and no other kind's", {
  fn <- app_fn("dta_template_kind_pattern")

  ds_pat <- fn("dta_dataset_template")
  expect_true(grepl(ds_pat, "foo.dta-dataset-template.yaml"))
  expect_true(grepl(ds_pat, "foo.dta-dataset-template.yml"))
  expect_false(grepl(ds_pat, "foo.dta-template.yaml"))
  expect_false(grepl(ds_pat, "foo.dta-party.yaml"))

  party_pat <- fn("dta_party_profile")
  expect_true(grepl(party_pat, "foo.dta-party.yaml"))
  expect_true(grepl(party_pat, "foo.dta-party.yml"))
  expect_false(grepl(party_pat, "foo.dta-template.yaml"))
  expect_false(grepl(party_pat, "foo.dta-dataset-template.yaml"))
})

# ---- read_template_header() -------------------------------------------------

test_that("read_template_header() reads every header field", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "foo.dta-template.yaml")
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: foo",
      "version: 1.0",
      "label: Foo Template",
      "description: A test template.",
      "extends: base@1.0",
      "abstract: true",
      "role: sponsor",
      "date: 2024-01-15",
      "base:",
      "  metadata: {}"
    ),
    path
  )

  fn <- app_fn("read_template_header")
  res <- fn(path)

  expect_true(res$ok)
  expect_equal(res$kind, "dta_creation_template")
  expect_equal(res$id, "foo")
  expect_equal(res$version, "1.0")
  expect_equal(res$label, "Foo Template")
  expect_equal(res$description, "A test template.")
  expect_equal(res$extends, "base@1.0")
  expect_true(res$abstract)
  expect_equal(res$role, "sponsor")
  expect_equal(res$date, "2024-01-15")
  expect_equal(res$path, path)
  expect_true(is.na(res$error))
})

test_that("read_template_header() preserves an unquoted 'version: 1.0' as the character '1.0'", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "foo.dta-template.yaml")
  # NOT quoted: this is the whole point of the test. yaml::read_yaml() would,
  # left to its default int/float handling, parse this as the double 1 --
  # and as.character(1) == "1", not "1.0".
  writeLines(c("kind: dta_creation_template", "id: foo", "version: 1.0"), path)

  fn <- app_fn("read_template_header")
  res <- fn(path)

  expect_true(res$ok)
  expect_identical(res$version, "1.0")
})

test_that("read_template_header() preserves '1.10' distinctly from '1.9' (no numeric truncation)", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "foo.dta-template.yaml")
  writeLines(c("kind: dta_creation_template", "id: foo", "version: 1.10"), path)

  fn <- app_fn("read_template_header")
  res <- fn(path)

  expect_identical(res$version, "1.10")
})

test_that("read_template_header() defaults 'abstract' to FALSE when absent", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "foo.dta-template.yaml")
  writeLines(c("kind: dta_creation_template", "id: foo", "version: 1.0"), path)

  fn <- app_fn("read_template_header")
  res <- fn(path)

  expect_true(res$ok)
  expect_identical(res$abstract, FALSE)
})

test_that("read_template_header() returns ok = FALSE, not an error, for syntactically malformed YAML", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "bad.dta-template.yaml")
  writeLines(c("kind: dta_creation_template", "id: [", "version: 1.0"), path)

  fn <- app_fn("read_template_header")
  # If this were NOT tolerant, the next line would throw and fail the test.
  res <- fn(path)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_equal(res$path, path)
})

test_that("read_template_header() returns ok = FALSE for a file missing 'id'", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "foo.dta-template.yaml")
  writeLines(c("kind: dta_creation_template", "version: 1.0"), path)

  fn <- app_fn("read_template_header")
  res <- fn(path)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("read_template_header() returns ok = FALSE for a nonexistent path, without throwing", {
  fn <- app_fn("read_template_header")
  res <- fn(file.path(tempdir(), "no-such-template-xyz.yaml"))

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- build_template_index() -------------------------------------------------

test_that("build_template_index() indexes all three kinds from one root", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "ct1.dta-template.yaml", "dta_creation_template", "ct1", "1.0", "CT One")
  write_template_fixture(root, "dt1.dta-dataset-template.yaml", "dta_dataset_template", "dt1", "1.0", "DT One")
  write_template_fixture(root, "pp1.dta-party.yaml", "dta_party_profile", "pp1", "1.0", "PP One")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  fn <- app_fn("build_template_index")
  idx <- fn()

  expect_equal(nrow(idx), 3)
  expect_setequal(colnames(idx), expected_index_columns())
  expect_setequal(idx$kind, c("dta_creation_template", "dta_dataset_template", "dta_party_profile"))
  expect_setequal(idx$id, c("ct1", "dt1", "pp1"))
  # The dataset template must be indexed under its OWN kind, never mistaken
  # for a creation template because of the filename suffix overlap.
  expect_equal(idx$kind[idx$id == "dt1"], "dta_dataset_template")
})

test_that("build_template_index() keeps the EARLIER root on a kind+id+version collision and warns naming both paths", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root1 <- withr::local_tempdir()
  root2 <- withr::local_tempdir()
  write_template_fixture(root1, "dup.dta-template.yaml", "dta_creation_template", "dup", "1.0", "First")
  write_template_fixture(root2, "dup.dta-template.yaml", "dta_creation_template", "dup", "1.0", "Second")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root1, ";dir:", root2))

  fn <- app_fn("build_template_index")
  idx <- fn()

  expect_equal(nrow(idx), 1)
  expect_equal(idx$label[[1]], "First")

  first_path <- normalizePath(file.path(root1, "templates", "dup.dta-template.yaml"), winslash = "/")
  second_path <- normalizePath(file.path(root2, "templates", "dup.dta-template.yaml"), winslash = "/")
  expect_equal(normalizePath(idx$path[[1]], winslash = "/"), first_path)

  msgs <- attr(idx, "warnings")
  expect_true(any(
    grepl(first_path, msgs, fixed = TRUE) & grepl(second_path, msgs, fixed = TRUE)
  ))
})

test_that("build_template_index() skips a malformed file (warning) while the good files still index", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "good.dta-template.yaml", "dta_creation_template", "good", "1.0", "Good")
  writeLines(
    c("kind: dta_creation_template", "id: [", "version: 1.0"),
    file.path(root, "templates", "bad.dta-template.yaml")
  )

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  fn <- app_fn("build_template_index")
  idx <- fn()

  expect_equal(nrow(idx), 1)
  expect_equal(idx$id[[1]], "good")

  msgs <- attr(idx, "warnings")
  expect_true(any(grepl("bad.dta-template.yaml", msgs, fixed = TRUE)))
})

test_that("build_template_index() returns a zero-row frame with the full column set when nothing resolves", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  missing_dir <- file.path(withr::local_tempdir(), "does-not-exist")
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", missing_dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  )

  fn <- app_fn("build_template_index")
  idx <- fn()

  expect_equal(nrow(idx), 0)
  expect_setequal(colnames(idx), expected_index_columns())
})

# ---- template_version_rank() ------------------------------------------------

test_that("template_version_rank() ranks '1.10' higher than '1.9' (not a string sort)", {
  fn <- app_fn("template_version_rank")

  expect_true(fn("1.10") > fn("1.9"))
})

test_that("template_version_rank() returns NA, not an error, for an unparseable version", {
  fn <- app_fn("template_version_rank")

  expect_true(is.na(fn("not-a-version")))
})

# ---- resolve_template_ref() -------------------------------------------------

test_that("resolve_template_ref() picks the highest version for a bare id, ranking 1.10 above 1.9", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "foo-1.9.dta-template.yaml", "dta_creation_template", "foo", "1.9", "Foo 1.9")
  write_template_fixture(root, "foo-1.10.dta-template.yaml", "dta_creation_template", "foo", "1.10", "Foo 1.10")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  resolve_fn <- app_fn("resolve_template_ref")
  row <- resolve_fn(idx, "foo")

  expect_false(is.null(row))
  expect_equal(nrow(row), 1)
  expect_equal(row$version, "1.10")
})

test_that("resolve_template_ref() with an exact id@version returns exactly that version", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "foo-1.9.dta-template.yaml", "dta_creation_template", "foo", "1.9", "Foo 1.9")
  write_template_fixture(root, "foo-1.10.dta-template.yaml", "dta_creation_template", "foo", "1.10", "Foo 1.10")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  resolve_fn <- app_fn("resolve_template_ref")
  row <- resolve_fn(idx, "foo@1.9")

  expect_equal(row$version, "1.9")
})

test_that("resolve_template_ref() id@latest matches the bare-id result", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "foo-1.9.dta-template.yaml", "dta_creation_template", "foo", "1.9", "Foo 1.9")
  write_template_fixture(root, "foo-1.10.dta-template.yaml", "dta_creation_template", "foo", "1.10", "Foo 1.10")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  resolve_fn <- app_fn("resolve_template_ref")
  bare <- resolve_fn(idx, "foo")
  latest <- resolve_fn(idx, "foo@latest")

  expect_equal(bare$version, latest$version)
  expect_equal(bare$path, latest$path)
})

test_that("resolve_template_ref() returns NULL for a nonexistent id", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "foo.dta-template.yaml", "dta_creation_template", "foo", "1.0", "Foo")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  resolve_fn <- app_fn("resolve_template_ref")
  expect_null(resolve_fn(idx, "does-not-exist"))
})

test_that("resolve_template_ref() excludes an unparseable version from bare/@latest but resolves it by exact reference", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "bar-good.dta-template.yaml", "dta_creation_template", "bar", "1.0", "Bar Good")
  write_template_fixture(root, "bar-bad.dta-template.yaml", "dta_creation_template", "bar", "not-a-version", "Bar Bad")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  resolve_fn <- app_fn("resolve_template_ref")

  bare <- resolve_fn(idx, "bar")
  expect_equal(bare$version, "1.0")

  exact <- resolve_fn(idx, "bar@not-a-version")
  expect_false(is.null(exact))
  expect_equal(exact$label, "Bar Bad")
})

# ---- list_template_index_entries() ------------------------------------------

test_that("list_template_index_entries() excludes abstract templates by default and includes them on request", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "concrete.dta-template.yaml", "dta_creation_template", "concrete", "1.0", "Concrete")
  write_template_fixture(
    root, "base.dta-template.yaml", "dta_creation_template", "base", "1.0", "Base",
    extra_lines = "abstract: true"
  )

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  list_fn <- app_fn("list_template_index_entries")

  visible <- list_fn(idx, kind = "dta_creation_template")
  expect_equal(visible$id, "concrete")

  everything <- list_fn(idx, kind = "dta_creation_template", include_abstract = TRUE)
  expect_setequal(everything$id, c("concrete", "base"))
})

test_that("list_template_index_entries() returns only the requested kind", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "ct1.dta-template.yaml", "dta_creation_template", "ct1", "1.0", "CT")
  write_template_fixture(root, "dt1.dta-dataset-template.yaml", "dta_dataset_template", "dt1", "1.0", "DT")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  list_fn <- app_fn("list_template_index_entries")
  rows <- list_fn(idx, kind = "dta_dataset_template")

  expect_equal(nrow(rows), 1)
  expect_equal(rows$id, "dt1")
})

test_that("list_template_index_entries() orders by label ascending, then by version descending within a tied label", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "b1.dta-template.yaml", "dta_creation_template", "b1", "1.0", "Beta")
  write_template_fixture(root, "a1.dta-template.yaml", "dta_creation_template", "a1", "1.0", "Alpha")
  write_template_fixture(root, "a2.dta-template.yaml", "dta_creation_template", "a2", "2.0", "Alpha")

  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  idx <- app_fn("build_template_index")()

  list_fn <- app_fn("list_template_index_entries")
  rows <- list_fn(idx, kind = "dta_creation_template")

  expect_equal(rows$label, c("Alpha", "Alpha", "Beta"))
  expect_equal(rows$version, c("2.0", "1.0", "1.0"))
})

# ---- dta_template_index_cached() / dta_template_index_invalidate() ---------

test_that("dta_template_index_cached() does not re-scan until invalidated", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "t1.dta-template.yaml", "dta_creation_template", "t1", "1.0", "T1")

  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root),
    DTATOOLS_TEMPLATE_REFRESH_SECONDS = "3600"
  )

  cached_fn <- app_fn("dta_template_index_cached")
  invalidate_fn <- app_fn("dta_template_index_invalidate")

  first <- cached_fn()
  expect_equal(nrow(first), 1)

  # Add a second template file AFTER the index has already been built and
  # cached: the memo must keep serving the ORIGINAL scan.
  write_template_fixture(root, "t2.dta-template.yaml", "dta_creation_template", "t2", "1.0", "T2")

  still_cached <- cached_fn()
  expect_equal(nrow(still_cached), 1)
  expect_false("t2" %in% still_cached$id)

  invalidate_fn()
  after_invalidate <- cached_fn()
  expect_equal(nrow(after_invalidate), 2)
  expect_true("t2" %in% after_invalidate$id)
})

test_that("dta_template_index_cached(refresh = TRUE) re-scans even within the TTL", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "t1.dta-template.yaml", "dta_creation_template", "t1", "1.0", "T1")

  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root),
    DTATOOLS_TEMPLATE_REFRESH_SECONDS = "3600"
  )

  cached_fn <- app_fn("dta_template_index_cached")

  first <- cached_fn()
  expect_equal(nrow(first), 1)

  write_template_fixture(root, "t2.dta-template.yaml", "dta_creation_template", "t2", "1.0", "T2")

  refreshed <- cached_fn(refresh = TRUE)
  expect_equal(nrow(refreshed), 2)
})

# ---- library layout: kind -> sub-folder table -------------------------------

# Run `expr`, muffling and returning every warning it signalled, so a test can
# count them (expect_warning() only sees the first match).
collect_warnings <- function(expr) {
  caught <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      caught[[length(caught) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = caught)
}

# Write `filename` (a stub body is fine for layout-only checks) under
# `root/<sub>`, creating the folder; `sub = ""` means the root itself.
touch_in <- function(root, sub, filename) {
  dir <- if (nzchar(sub)) file.path(root, sub) else root
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, filename)
  writeLines("x: 1", path)
  path
}

test_that("the layout table maps every kind to its library sub-folder", {
  subdir <- app_fn("dta_template_kind_subdir")

  expect_identical(subdir("dta_creation_template"), "templates")
  expect_identical(subdir("dta_dataset_template"), "templates")
  expect_identical(subdir("dta_party_profile"), "parties")
  expect_identical(subdir("dta_vocabulary"), "vocabulary")

  # The distinct indexed folders are exactly the image of the four kinds, and
  # the Word templates' folder is not one of them.
  expect_setequal(
    app_fn("dta_template_kind_subdirs")(),
    vapply(app_fn("dta_template_all_kinds")(), subdir, character(1))
  )
  expect_identical(app_fn("dta_export_templates_subdir")(), "export-templates")
  expect_false("export-templates" %in% app_fn("dta_template_kind_subdirs")())
})

test_that("the layout table refuses a kind it does not know", {
  # The message is authored by the package (cli), not translated by R.
  expect_error(app_fn("dta_template_kind_subdir")("dta_nonsense"), "Unknown template kind")
  expect_error(app_fn("dta_template_kind_dir")("/some/root", "dta_nonsense"), "Unknown template kind")
  expect_error(app_fn("dta_template_kind_subdir")(NA_character_), "Unknown template kind")
})

test_that("dta_template_kind_dir() joins the library root and the kind's folder", {
  kind_dir <- app_fn("dta_template_kind_dir")

  expect_identical(kind_dir("/lib", "dta_creation_template"), file.path("/lib", "templates"))
  expect_identical(kind_dir("/lib", "dta_dataset_template"), file.path("/lib", "templates"))
  expect_identical(kind_dir("/lib", "dta_party_profile"), file.path("/lib", "parties"))
  expect_identical(kind_dir("/lib", "dta_vocabulary"), file.path("/lib", "vocabulary"))
})

# ---- dta_template_misplaced_files() -----------------------------------------

test_that("dta_template_misplaced_files() is empty for a correctly laid-out library", {
  root <- withr::local_tempdir()
  touch_in(root, "templates", "a.dta-template.yaml")
  touch_in(root, "templates", "b.dta-dataset-template.yaml")
  touch_in(root, "parties", "c.dta-party.yaml")
  touch_in(root, "vocabulary", "d.dta-vocabulary.yaml")
  touch_in(root, "export-templates", "e.docx")
  touch_in(root, "", "dta-app.yaml")
  touch_in(root, "", "README.md")

  bad <- app_fn("dta_template_misplaced_files")(root)

  expect_s3_class(bad, "data.frame")
  expect_identical(colnames(bad), c("file", "kind", "expected_dir"))
  expect_identical(nrow(bad), 0L)
})

test_that("dta_template_misplaced_files() finds a kind-suffix file lying directly in the root", {
  root <- withr::local_tempdir()
  flat <- touch_in(root, "", "flat.dta-template.yaml")

  bad <- app_fn("dta_template_misplaced_files")(root)

  expect_identical(nrow(bad), 1L)
  expect_identical(bad$file, flat)
  expect_identical(bad$kind, "dta_creation_template")
  expect_identical(bad$expected_dir, "templates")
})

test_that("dta_template_misplaced_files() finds a file sitting in another kind's folder", {
  root <- withr::local_tempdir()
  party <- touch_in(root, "templates", "p.dta-party.yaml")
  vocab <- touch_in(root, "parties", "v.dta-vocabulary.yml")
  tmpl <- touch_in(root, "vocabulary", "t.dta-template.yaml")
  dset <- touch_in(root, "parties", "s.dta-dataset-template.yaml")

  bad <- app_fn("dta_template_misplaced_files")(root)

  expect_identical(nrow(bad), 4L)
  want_dir <- stats::setNames(bad$expected_dir, bad$file)
  expect_identical(want_dir[[party]], "parties")
  expect_identical(want_dir[[vocab]], "vocabulary")
  expect_identical(want_dir[[tmpl]], "templates")
  expect_identical(want_dir[[dset]], "templates")
  want_kind <- stats::setNames(bad$kind, bad$file)
  expect_identical(want_kind[[party]], "dta_party_profile")
  expect_identical(want_kind[[dset]], "dta_dataset_template")
})

test_that("a dataset template in templates/ is not mistaken for a misplaced creation template", {
  root <- withr::local_tempdir()
  touch_in(root, "templates", "s.dta-dataset-template.yaml")

  expect_identical(nrow(app_fn("dta_template_misplaced_files")(root)), 0L)
})

test_that("dta_template_misplaced_files() ignores directories named like a template file", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "looks-like.dta-template.yaml"))
  dir.create(file.path(root, "templates", "looks-like.dta-party.yaml"), recursive = TRUE)

  expect_identical(nrow(app_fn("dta_template_misplaced_files")(root)), 0L)
})

test_that("dta_template_misplaced_files() is not recursive", {
  root <- withr::local_tempdir()
  touch_in(root, "templates/deeper", "p.dta-party.yaml")
  touch_in(root, "other", "q.dta-template.yaml")

  expect_identical(nrow(app_fn("dta_template_misplaced_files")(root)), 0L)
})

test_that("dta_template_misplaced_files() lists files in radix (byte) order", {
  root <- withr::local_tempdir()
  # Upper case sorts before lower case under radix; a locale collation would
  # interleave them, which is what the committed-output rule forbids.
  b <- touch_in(root, "", "b.dta-template.yaml")
  upper <- touch_in(root, "", "Z.dta-template.yaml")
  a <- touch_in(root, "", "a.dta-template.yaml")

  bad <- app_fn("dta_template_misplaced_files")(root)

  expect_identical(bad$file, c(upper, a, b))
})

test_that("dta_template_misplaced_files() copes with a missing or non-path root", {
  fn <- app_fn("dta_template_misplaced_files")

  expect_identical(nrow(fn(file.path(withr::local_tempdir(), "nope"))), 0L)
  expect_identical(nrow(fn(NA_character_)), 0L)
  expect_identical(nrow(fn("")), 0L)
  expect_identical(nrow(fn(NULL)), 0L)
})

# ---- dta_template_locate_root() ----------------------------------------------

test_that("dta_template_locate_root() returns the library root of a file in its own kind folder", {
  fn <- app_fn("dta_template_locate_root")
  root <- file.path(normalizePath(withr::local_tempdir(), winslash = "/"), "lib")

  expect_identical(fn(file.path(root, "templates", "a.dta-template.yaml")), root)
  expect_identical(fn(file.path(root, "templates", "a.dta-dataset-template.yml")), root)
  expect_identical(fn(file.path(root, "parties", "a.dta-party.yaml")), root)
  expect_identical(fn(file.path(root, "vocabulary", "a.dta-vocabulary.yaml")), root)
})

test_that("dta_template_locate_root() is NA for a file in the wrong folder", {
  fn <- app_fn("dta_template_locate_root")
  root <- file.path(normalizePath(withr::local_tempdir(), winslash = "/"), "lib")

  expect_true(is.na(fn(file.path(root, "parties", "a.dta-template.yaml"))))
  expect_true(is.na(fn(file.path(root, "templates", "a.dta-vocabulary.yaml"))))
  expect_true(is.na(fn(file.path(root, "a.dta-template.yaml"))))
  expect_true(is.na(fn(file.path(root, "other", "a.dta-party.yaml"))))
  expect_type(fn(file.path(root, "a.dta-template.yaml")), "character")
})

test_that("dta_template_locate_root() is NA for a file whose name is not a template name", {
  fn <- app_fn("dta_template_locate_root")
  root <- file.path(normalizePath(withr::local_tempdir(), winslash = "/"), "lib")

  expect_true(is.na(fn(file.path(root, "templates", "notes.txt"))))
  expect_true(is.na(fn(file.path(root, "templates", "a.yaml"))))
  expect_true(is.na(fn(file.path(root, "templates", "a.dta-template.json"))))
})

# ---- build_template_index() and the layout ----------------------------------

test_that("build_template_index() indexes files in the correct folders of all four kinds", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "ct.dta-template.yaml", "dta_creation_template", "ct", "1.0", "CT")
  write_template_fixture(root, "dt.dta-dataset-template.yaml", "dta_dataset_template", "dt", "1.0", "DT")
  write_template_fixture(root, "pp.dta-party.yaml", "dta_party_profile", "pp", "1.0", "PP")
  write_template_fixture(root, "vv.dta-vocabulary.yaml", "dta_vocabulary", "vv", "1.0", "VV")
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  res <- collect_warnings(app_fn("build_template_index")())
  idx <- res$value

  expect_length(res$warnings, 0)
  expect_identical(nrow(idx), 4L)
  expect_setequal(idx$kind, app_fn("dta_template_all_kinds")())
  expect_length(attr(idx, "warnings"), 0)
  # Each row's path is inside the folder its kind belongs to.
  parent <- stats::setNames(basename(dirname(idx$path)), idx$id)
  expect_identical(parent[["ct"]], "templates")
  expect_identical(parent[["dt"]], "templates")
  expect_identical(parent[["pp"]], "parties")
  expect_identical(parent[["vv"]], "vocabulary")
})

test_that("build_template_index() skips misplaced files, reports each in attr 'warnings' and warns once", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "ok.dta-template.yaml", "dta_creation_template", "ok", "1.0", "OK")
  # Valid, parseable files -- only their location is wrong.
  flat <- file.path(root, "flat.dta-template.yaml")
  writeLines(c("kind: dta_creation_template", "id: flat", "version: 1.0"), flat)
  wrong_dir <- file.path(root, "templates", "pp.dta-party.yaml")
  writeLines(c("kind: dta_party_profile", "id: pp", "version: 1.0"), wrong_dir)
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  res <- collect_warnings(app_fn("build_template_index")())
  idx <- res$value

  # Only the correctly placed file is indexed.
  expect_identical(idx$id, "ok")

  # One entry per skipped file, naming the file, its kind and the right folder.
  msgs <- attr(idx, "warnings")
  flat_msg <- msgs[grepl("flat.dta-template.yaml", msgs, fixed = TRUE)]
  pp_msg <- msgs[grepl("pp.dta-party.yaml", msgs, fixed = TRUE)]
  expect_length(flat_msg, 1)
  expect_length(pp_msg, 1)
  expect_match(flat_msg, "belongs in templates/", fixed = TRUE)
  expect_match(pp_msg, "belongs in parties/", fixed = TRUE)
  # People read this in the picker: the human kind label, never the kind id,
  # and the source name plus a path relative to the source root, never the
  # absolute server path.
  src_name <- basename(root)
  expect_identical(
    flat_msg,
    paste0(
      src_name, ": flat.dta-template.yaml - a creation template belongs in templates/ ",
      "(library layout: templates/, parties/, vocabulary/, export-templates/)"
    )
  )
  expect_identical(
    pp_msg,
    paste0(
      src_name, ": templates/pp.dta-party.yaml - a party profile belongs in parties/ ",
      "(library layout: templates/, parties/, vocabulary/, export-templates/)"
    )
  )
  expect_no_match(c(flat_msg, pp_msg), "dta_party_profile|dta_creation_template")
  expect_no_match(c(flat_msg, pp_msg), root, fixed = TRUE)
  # The same entries, kept apart for the picker.
  expect_setequal(attr(idx, "layout_warnings"), c(flat_msg, pp_msg))

  # ... and exactly ONE aggregated condition carries all of them.
  layout <- Filter(function(w) grepl("belongs in", conditionMessage(w)), res$warnings)
  expect_length(layout, 1)
  expect_s3_class(layout[[1]], "warning")
  expect_match(conditionMessage(layout[[1]]), "flat.dta-template.yaml", fixed = TRUE)
  expect_match(conditionMessage(layout[[1]]), "pp.dta-party.yaml", fixed = TRUE)
})

test_that("build_template_index() warns with the package's own layout text", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  writeLines(
    c("kind: dta_vocabulary", "id: v", "version: 1.0"),
    file.path(root, "v.dta-vocabulary.yaml")
  )
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  expect_warning(idx <- app_fn("build_template_index")(), "belongs in vocabulary/")
  expect_identical(nrow(idx), 0L)
})

test_that("build_template_index() does not warn about layout for a correct library", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root <- withr::local_tempdir()
  write_template_fixture(root, "ok.dta-template.yaml", "dta_creation_template", "ok", "1.0", "OK")
  dir.create(file.path(root, "export-templates"))
  writeLines("x", file.path(root, "export-templates", "w.docx"))
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))

  res <- collect_warnings(app_fn("build_template_index")())

  expect_length(res$warnings, 0)
  expect_identical(nrow(res$value), 1L)
})

test_that("a source root that is itself a kind folder yields one 'library sub-folder' warning", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  lib <- withr::local_tempdir()
  # A source pointed one level too deep: the root IS templates/, with the
  # files directly inside it and no kind folders beneath.
  deep <- file.path(lib, "templates")
  dir.create(deep)
  writeLines(c("kind: dta_creation_template", "id: a", "version: 1.0"), file.path(deep, "a.dta-template.yaml"))
  writeLines(c("kind: dta_creation_template", "id: b", "version: 1.0"), file.path(deep, "b.dta-template.yaml"))
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", deep))

  res <- collect_warnings(app_fn("build_template_index")())
  idx <- res$value

  expect_identical(nrow(idx), 0L)
  msgs <- attr(idx, "warnings")
  # One entry for the root, not one per file.
  expect_length(msgs, 1)
  expect_match(msgs, "looks like a library sub-folder", fixed = TRUE)
  expect_match(msgs, "parent directory", fixed = TRUE)
  expect_match(msgs, "flat library", fixed = TRUE)
  expect_match(msgs, "Reorganising an older repository", fixed = TRUE)
  expect_identical(attr(idx, "layout_warnings"), msgs)
  # The source name and the folder's own name -- not its absolute path.
  expect_match(msgs, "^templates: templates/ looks like")
  expect_no_match(msgs, deep, fixed = TRUE)
  expect_length(res$warnings, 1)
  expect_match(conditionMessage(res$warnings[[1]]), "library sub-folder", fixed = TRUE)
})

test_that("a misplaced file never shadows a correctly placed one from a later root", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root1 <- withr::local_tempdir()
  root2 <- withr::local_tempdir()
  # Same kind+id+version; root1's copy is misplaced (flat), root2's is fine.
  writeLines(
    c("kind: dta_creation_template", "id: dup", "version: 1.0", "label: Misplaced"),
    file.path(root1, "dup.dta-template.yaml")
  )
  write_template_fixture(root2, "dup.dta-template.yaml", "dta_creation_template", "dup", "1.0", "Proper")
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root1, ";dir:", root2))

  res <- collect_warnings(app_fn("build_template_index")())
  idx <- res$value

  expect_identical(nrow(idx), 1L)
  expect_identical(idx$label, "Proper")
  # The skip is reported as a layout problem, not as a duplicate.
  msgs <- attr(idx, "warnings")
  expect_false(any(grepl("Duplicate", msgs, fixed = TRUE)))
  expect_true(any(grepl("belongs in templates/", msgs, fixed = TRUE)))
})

test_that("the same kind+id+version in two correctly laid-out roots keeps the earlier root", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  root1 <- withr::local_tempdir()
  root2 <- withr::local_tempdir()
  suffixes <- c(
    dta_creation_template = "dta-template", dta_dataset_template = "dta-dataset-template",
    dta_party_profile = "dta-party", dta_vocabulary = "dta-vocabulary"
  )
  for (kind in names(suffixes)) {
    file <- paste0("x.", suffixes[[kind]], ".yaml")
    write_template_fixture(root1, file, kind, "x", "1.0", "First")
    write_template_fixture(root2, file, kind, "x", "1.0", "Second")
  }
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root1, ";dir:", root2))

  res <- collect_warnings(app_fn("build_template_index")())
  idx <- res$value

  expect_identical(nrow(idx), 4L)
  expect_true(all(idx$label == "First"))
  expect_true(all(startsWith(
    normalizePath(idx$path, winslash = "/"),
    normalizePath(root1, winslash = "/")
  )))
  expect_identical(sum(grepl("Duplicate", attr(idx, "warnings"), fixed = TRUE)), 4L)
})
