Sys.setenv(NOT_CRAN = "true")

# Coverage for R/createTemplateRepo.R -- the exported, package-namespace
# create_template_repo(), which is the WRITE side of the feature
# validate_template() (R/validateTemplate.R) is the read side of: it scaffolds
# a directory that already passes validate_template(path, strict = TRUE)
# unmodified. See that file's header comment for the split between the
# skeleton it copies (inst/extdata/template-repo-skeleton/) and the packaged
# examples (inst/extdata/templates/) used elsewhere.

# ---- Fixture helpers --------------------------------------------------------

# Every regular file below `dir`, relative path, recursive and including
# dotfiles/dot-directories (`.gitignore`, `.github/`) -- what "the directory
# byte-for-byte unchanged" needs to be checked against.
list_all_files <- function(dir) {
  list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = FALSE)
}

# md5sum() of every file below `dir`, keyed by relative path -- used to prove
# a failed, non-overwriting call left an existing directory untouched.
md5_snapshot <- function(dir) {
  files <- list_all_files(dir)
  sums <- tools::md5sum(file.path(dir, files))
  stats::setNames(unname(sums), files)
}

# The four starter template files' `kind:` fields, keyed by filename -- read
# back with yaml::read_yaml() rather than asserted via file.exists(), per this
# project's house rule that a file-producing function is verified by reading
# the file back.
template_kinds <- function(dir) {
  files <- list.files(dir, pattern = "[.]dta-.*[.]ya?ml$", full.names = FALSE)
  stats::setNames(
    vapply(files, function(f) yaml::read_yaml(file.path(dir, f))$kind, character(1)),
    files
  )
}

# ---- The round trip -- the anchor test --------------------------------------

test_that("create_template_repo() scaffolds a directory that validate_template() accepts outright", {
  # This is what ties the scaffold to the validator: if either drifts out of
  # sync with the other, this is the test that fails.
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  result <- validate_template(dir)
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
  expect_no_error(validate_template(dir, strict = TRUE))
})

# ---- The four starter template kinds ----------------------------------------

test_that("the scaffold writes exactly one file of each of the four template kinds", {
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  kinds <- template_kinds(dir)
  expect_setequal(
    kinds,
    c("dta_creation_template", "dta_dataset_template", "dta_party_profile", "dta_vocabulary")
  )
  expect_equal(length(kinds), 4)
})

# ---- The starter set actually cross-references ------------------------------

test_that("the starter dataset template's STATUS column is bound via values_from:", {
  # A starter set of four unrelated files would teach nothing; this pins that
  # the worked example stays wired together.
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  ds_files <- list.files(dir, pattern = "[.]dta-dataset-template[.]ya?ml$", full.names = TRUE)
  expect_length(ds_files, 1)
  ds_def <- yaml::read_yaml(ds_files[[1]])

  columns <- ds_def$dataset$columns
  bindings <- Filter(function(col) !is.null(col$values_from), columns)
  expect_gt(length(bindings), 0)
})

test_that("the starter creation template imports the starter dataset template by template:", {
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  tpl_files <- list.files(dir, pattern = "[.]dta-template[.]ya?ml$", full.names = TRUE)
  expect_length(tpl_files, 1)
  tpl_def <- yaml::read_yaml(tpl_files[[1]])

  ds_files <- list.files(dir, pattern = "[.]dta-dataset-template[.]ya?ml$", full.names = TRUE)
  ds_id <- yaml::read_yaml(ds_files[[1]])$id

  refs <- vapply(tpl_def$datasets, function(entry) as.character(entry$template %||% ""), character(1))
  expect_true(any(startsWith(refs, paste0(ds_id, "@"))))
})

# ---- ci = TRUE / ci = FALSE --------------------------------------------------

test_that("ci = TRUE writes .github/workflows/validate-templates.yml, naming validate_template", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = TRUE)

  workflow <- file.path(dir, ".github", "workflows", "validate-templates.yml")
  expect_true(file.exists(workflow))
  content <- paste(readLines(workflow), collapse = "\n")
  expect_match(content, "validate_template", fixed = TRUE)
})

test_that("ci = FALSE creates no .github directory at all", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)

  expect_false(dir.exists(file.path(dir, ".github")))
})

# ---- ci as a character vector of providers ------------------------------------

test_that("ci = TRUE (the default) writes only the GitHub workflow -- no Bitbucket or Jenkins files", {
  # Backwards-compatibility guard: `ci` grew from a bare logical into also
  # accepting a character vector of providers. TRUE must still mean "GitHub
  # only" for every caller already passing a bare logical -- this is the
  # single most important test in this file for that reason.
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  expect_true(file.exists(file.path(dir, ".github", "workflows", "validate-templates.yml")))
  expect_false(file.exists(file.path(dir, "bitbucket-pipelines.yml")))
  expect_false(file.exists(file.path(dir, "Jenkinsfile")))
})

test_that("ci = c(\"github\", \"bitbucket\", \"jenkins\") writes all three CI files at their documented paths", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("github", "bitbucket", "jenkins"))

  expect_true(file.exists(file.path(dir, ".github", "workflows", "validate-templates.yml")))
  expect_true(file.exists(file.path(dir, "bitbucket-pipelines.yml")))
  expect_true(file.exists(file.path(dir, "Jenkinsfile")))
})

test_that("a repository with all three CI files still validates clean", {
  # bitbucket-pipelines.yml and Jenkinsfile land in the repository ROOT --
  # exactly where validate_template()'s flat scan, and its recursive
  # template_in_subdirectory check, are both looking. Pins that neither file
  # is mistaken for a template, and neither trips the subdirectory warning.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("github", "bitbucket", "jenkins"))

  result <- validate_template(dir)
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
  expect_no_error(validate_template(dir, strict = TRUE))
})

test_that("ci = \"jenkins\" writes the Jenkinsfile and creates no .github directory at all", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = "jenkins")

  expect_true(file.exists(file.path(dir, "Jenkinsfile")))
  expect_false(dir.exists(file.path(dir, ".github")))
})

test_that("duplicate and reordered ci providers write exactly the canonical set once each", {
  # Duplicates are silently de-duplicated and the write order is canonicalised
  # regardless of what order the caller passed in -- this exercises both at
  # once.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("jenkins", "github", "jenkins"))

  expect_true(file.exists(file.path(dir, "Jenkinsfile")))
  expect_true(file.exists(file.path(dir, ".github", "workflows", "validate-templates.yml")))
  expect_false(file.exists(file.path(dir, "bitbucket-pipelines.yml")))
  expect_length(list.files(dir, pattern = "^Jenkinsfile$"), 1)
})

test_that("an unknown ci provider aborts", {
  dir <- withr::local_tempdir()
  expect_error(create_template_repo(dir, ci = "gitlab"), class = "rlang_error")
})

test_that("ci = NA and ci = character(0) abort", {
  dir <- withr::local_tempdir()
  expect_error(create_template_repo(dir, ci = NA), class = "rlang_error")
  expect_error(create_template_repo(dir, ci = character(0)), class = "rlang_error")
})

test_that("the written bitbucket-pipelines.yml parses as YAML and the Jenkinsfile is a declarative pipeline that runs validate_template", {
  # Confirms the two new assets are what they claim to be, read back from the
  # files this function actually wrote -- never asserted via file.exists().
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("bitbucket", "jenkins"))

  pipeline <- yaml::read_yaml(file.path(dir, "bitbucket-pipelines.yml"))
  expect_true(is.list(pipeline))
  expect_type(pipeline$image, "character")

  jenkinsfile <- paste(readLines(file.path(dir, "Jenkinsfile")), collapse = "\n")
  expect_match(jenkinsfile, "pipeline {", fixed = TRUE)
  expect_match(jenkinsfile, "validate_template", fixed = TRUE)
})

# ---- examples = FALSE --------------------------------------------------------

test_that("examples = FALSE writes no template files, and validate_template() reports no_templates on the result", {
  # Ties the two features together: an examples = FALSE scaffold is exactly
  # the shape no_templates (R/validateTemplate.R) exists to catch.
  dir <- withr::local_tempdir()
  create_template_repo(dir, examples = FALSE)

  expect_length(list.files(dir, pattern = "[.]dta-.*[.]ya?ml$"), 0)

  result <- validate_template(dir)
  rows <- result[result$code == "no_templates", , drop = FALSE]
  expect_equal(nrow(rows), 1)
  expect_equal(rows$severity, "error")
})

# ---- overwrite = FALSE / overwrite = TRUE -----------------------------------

test_that("overwrite = FALSE on a populated directory aborts and leaves every existing file byte-identical", {
  # The all-or-nothing guarantee: a partially-written repository would leave a
  # caller unable to tell which half of a family of cross-referencing
  # templates they were looking at.
  dir <- withr::local_tempdir()
  create_template_repo(dir)
  before <- md5_snapshot(dir)

  expect_error(create_template_repo(dir, overwrite = FALSE), class = "rlang_error")

  after <- md5_snapshot(dir)
  expect_equal(after, before)
})

test_that("overwrite = FALSE still aborts and leaves every file byte-identical with multiple CI providers", {
  # Same all-or-nothing guarantee as above, exercised with two CI targets at
  # once -- pins that folding CI files into the pre-write existence check
  # didn't quietly narrow the guarantee to just the non-CI files.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("github", "jenkins"))
  before <- md5_snapshot(dir)

  expect_error(
    create_template_repo(dir, ci = c("github", "jenkins"), overwrite = FALSE),
    class = "rlang_error"
  )

  after <- md5_snapshot(dir)
  expect_equal(after, before)
})

test_that("overwrite = TRUE succeeds on an already-populated directory", {
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  expect_no_error(create_template_repo(dir, overwrite = TRUE))
  expect_equal(nrow(validate_template(dir)), 0)
})

# ---- Path handling ------------------------------------------------------------

test_that("a missing parent directory is created", {
  parent <- withr::local_tempdir()
  dir <- file.path(parent, "does", "not", "exist", "yet")
  expect_false(dir.exists(dir))

  create_template_repo(dir)

  expect_true(dir.exists(dir))
  expect_equal(nrow(validate_template(dir)), 0)
})

test_that("a path naming an existing file aborts", {
  parent <- withr::local_tempdir()
  file_path <- file.path(parent, "im_a_file")
  writeLines("not a directory", file_path)

  expect_error(create_template_repo(file_path), class = "rlang_error")
})

test_that("a non-scalar path aborts", {
  expect_error(create_template_repo(character(0)), class = "rlang_error")
  expect_error(create_template_repo(NA_character_), class = "rlang_error")
  expect_error(create_template_repo(c("a", "b")), class = "rlang_error")
})

# ---- The shipped skeleton itself -----------------------------------------------

test_that("the bundled template-repo-skeleton directory is itself clean", {
  # Guards the assets directly, independent of create_template_repo()'s own
  # copy logic -- a future validator change that would reject them fails a
  # test here instead of shipping a broken scaffold.
  dir <- system.file("extdata", "template-repo-skeleton", package = "DTAtools")
  expect_true(nzchar(dir), info = "inst/extdata/template-repo-skeleton missing from the package")

  result <- validate_template(dir)
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
})
