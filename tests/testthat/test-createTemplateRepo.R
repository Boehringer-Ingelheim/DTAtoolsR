# Coverage for R/createTemplateRepo.R -- the exported, package-namespace
# create_template_repo(), which is the WRITE side of the feature
# validate_template() (R/validateTemplate.R) is the read side of: it scaffolds
# a directory that already passes validate_template(path, strict = TRUE)
# unmodified. See that file's header comment for the split between the
# skeleton it copies (inst/extdata/template-repo-skeleton/) and the packaged
# library (inst/extdata/library/) used elsewhere.

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
  # One folder per kind: templates/ (two kinds), parties/, vocabulary/.
  files <- list.files(
    file.path(dir, c("templates", "parties", "vocabulary")),
    pattern = "[.]dta-.*[.]ya?ml$", full.names = TRUE
  )
  stats::setNames(
    vapply(files, function(f) yaml::read_yaml(f)$kind, character(1)),
    basename(files)
  )
}

# Content assertions for the three CI files, read back from disk. A GitHub
# workflow must parse as YAML with a `jobs` map and a trigger map (YAML 1.1
# readers turn the bare key `on` into TRUE, hence both spellings); a Bitbucket
# file must parse with a `pipelines` map; a Jenkinsfile must be a non-empty
# declarative pipeline.
expect_github_workflow <- function(dir) {
  path <- file.path(dir, ".github", "workflows", "validate-templates.yml")
  expect_gt(file.size(path), 0)
  wf <- yaml::read_yaml(path)
  expect_true(is.list(wf$jobs) && length(wf$jobs) > 0)
  expect_true(any(c("on", "TRUE") %in% names(wf)))
}
expect_bitbucket_pipeline <- function(dir) {
  path <- file.path(dir, "bitbucket-pipelines.yml")
  expect_gt(file.size(path), 0)
  expect_true(is.list(yaml::read_yaml(path)$pipelines))
}
expect_jenkinsfile <- function(dir) {
  path <- file.path(dir, "Jenkinsfile")
  expect_gt(file.size(path), 0)
  expect_match(paste(readLines(path, warn = FALSE), collapse = "\n"), "pipeline", fixed = TRUE)
}

# The files of a default (examples = TRUE) scaffold, apart from CI.
scaffold_example_files <- c(
  "README.md", ".gitignore", "dta-app.yaml",
  "templates/starter.dta-template.yaml",
  "templates/starter_dataset.dta-dataset-template.yaml",
  "parties/starter_supplier.dta-party.yaml",
  "vocabulary/starter_terms.dta-vocabulary.yaml",
  "export-templates/README.md"
)

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

  ds_files <- list.files(file.path(dir, "templates"), pattern = "[.]dta-dataset-template[.]ya?ml$", full.names = TRUE)
  expect_length(ds_files, 1)
  ds_def <- yaml::read_yaml(ds_files[[1]])

  columns <- ds_def$dataset$columns
  bindings <- Filter(function(col) !is.null(col$values_from), columns)
  expect_gt(length(bindings), 0)
})

test_that("the starter creation template imports the starter dataset template by template:", {
  dir <- withr::local_tempdir()
  create_template_repo(dir)

  tpl_files <- list.files(file.path(dir, "templates"), pattern = "[.]dta-template[.]ya?ml$", full.names = TRUE)
  expect_length(tpl_files, 1)
  tpl_def <- yaml::read_yaml(tpl_files[[1]])

  ds_files <- list.files(file.path(dir, "templates"), pattern = "[.]dta-dataset-template[.]ya?ml$", full.names = TRUE)
  ds_id <- yaml::read_yaml(ds_files[[1]])$id

  refs <- vapply(tpl_def$datasets, function(entry) as.character(entry$template %||% ""), character(1))
  expect_true(any(startsWith(refs, paste0(ds_id, "@"))))
})

# ---- ci = TRUE / ci = FALSE --------------------------------------------------

test_that("ci = TRUE writes .github/workflows/validate-templates.yml, naming lint_template_repo", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = TRUE)

  workflow <- file.path(dir, ".github", "workflows", "validate-templates.yml")
  expect_true(file.exists(workflow))
  content <- paste(readLines(workflow), collapse = "\n")
  expect_match(content, "lint_template_repo", fixed = TRUE)
})

test_that("the GitHub workflow installs remotes, which its placeholder install examples call", {
  # setup-r-dependencies hands `packages` straight to pak::lockfile_create(),
  # so every entry must be a resolvable package reference: the earlier
  # `packages: none` named a CRAN package that does not exist and failed the
  # job before validation ever ran.
  dir <- withr::local_tempdir()
  create_template_repo(dir, examples = FALSE)

  workflow <- yaml::read_yaml(file.path(dir, ".github", "workflows", "validate-templates.yml"))
  steps <- workflow$jobs[["validate-templates"]]$steps
  uses <- vapply(steps, function(s) s$uses %||% "", character(1))
  deps <- steps[[grep("setup-r-dependencies", uses, fixed = TRUE)]]$with$packages
  expect_identical(deps, "any::remotes")

  runs <- vapply(steps, function(s) s$run %||% "", character(1))
  expect_true(any(grepl("lint_template_repo(\".\")", runs, fixed = TRUE)))
})

test_that("the Bitbucket and Jenkins assets install remotes, and Jenkins uses a writable user library", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, examples = FALSE, ci = c("bitbucket", "jenkins"))

  pipeline <- yaml::read_yaml(file.path(dir, "bitbucket-pipelines.yml"))
  script <- unlist(pipeline$definitions$steps[[1]]$step$script)
  expect_true(any(grepl("install.packages(\"remotes\")", script, fixed = TRUE)))

  jenkinsfile <- paste(readLines(file.path(dir, "Jenkinsfile")), collapse = "\n")
  expect_match(jenkinsfile, "install.packages(\"remotes\")", fixed = TRUE)
  # R_LIBS, not R_LIBS_USER: rocker's Renviron.site fills R_LIBS with the
  # unwritable site library unless it is already set, and R_LIBS is searched
  # first.
  expect_match(jenkinsfile, "R_LIBS = '/tmp/R-library'", fixed = TRUE)
  expect_match(jenkinsfile, "mkdir -p \"$R_LIBS\"", fixed = TRUE)
})

test_that("the Bitbucket and Jenkins assets run the repository linter", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, examples = FALSE, ci = c("bitbucket", "jenkins"))

  call <- "DTAtools::lint_template_repo(\".\")"
  pipeline <- yaml::read_yaml(file.path(dir, "bitbucket-pipelines.yml"))
  script <- unlist(pipeline$definitions$steps[[1]]$step$script)
  expect_true(any(grepl(call, script, fixed = TRUE)))

  jenkinsfile <- paste(readLines(file.path(dir, "Jenkinsfile")), collapse = "\n")
  expect_match(jenkinsfile, call, fixed = TRUE)
})

test_that("examples = FALSE does not suggest a validate_template() call that must fail", {
  # Whitespace (including cli's wrapping and non-breaking spaces) is squeezed
  # out so the assertions do not depend on where cli breaks the line.
  scaffold_message <- function(...) {
    msgs <- character()
    withCallingHandlers(
      create_template_repo(withr::local_tempdir(), ...),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    gsub("[[:space:] ]+", "", paste(msgs, collapse = ""))
  }

  # Positive control: the default scaffold suggests the repository linter,
  # not the strict validator.
  default <- scaffold_message()
  expect_match(default, "DTAtools::lint_template_repo(\"", fixed = TRUE)
  expect_no_match(default, "strict=TRUE", fixed = TRUE)

  bare <- scaffold_message(examples = FALSE)
  expect_match(bare, "no_templates", fixed = TRUE)
  expect_no_match(bare, "lint_template_repo", fixed = TRUE)
  expect_no_match(bare, "strict=TRUE", fixed = TRUE)
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

  expect_github_workflow(dir)
  expect_false(file.exists(file.path(dir, "bitbucket-pipelines.yml")))
  expect_false(file.exists(file.path(dir, "Jenkinsfile")))
})

test_that("ci = c(\"github\", \"bitbucket\", \"jenkins\") writes all three CI files at their documented paths", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("github", "bitbucket", "jenkins"))

  expect_github_workflow(dir)
  expect_bitbucket_pipeline(dir)
  expect_jenkinsfile(dir)
})

test_that("a repository with all three CI files still validates clean", {
  # bitbucket-pipelines.yml and Jenkinsfile land in the repository ROOT --
  # exactly where validate_template()'s placement check (which looks for
  # template files outside their kind folder) is looking. Pins that neither
  # file is mistaken for a template, and neither trips a placement row.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("github", "bitbucket", "jenkins"))

  result <- validate_template(dir)
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
  expect_no_error(validate_template(dir, strict = TRUE))
})

test_that("ci = \"jenkins\" writes the Jenkinsfile and creates no .github directory at all", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = "jenkins")

  expect_jenkinsfile(dir)
  expect_false(dir.exists(file.path(dir, ".github")))
})

test_that("duplicate and reordered ci providers write exactly the canonical set once each", {
  # Duplicates are silently de-duplicated and the write order is canonicalised
  # regardless of what order the caller passed in -- this exercises both at
  # once.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("jenkins", "github", "jenkins"))

  expect_jenkinsfile(dir)
  expect_github_workflow(dir)
  expect_false(file.exists(file.path(dir, "bitbucket-pipelines.yml")))
  expect_length(list.files(dir, pattern = "^Jenkinsfile$"), 1)
})

test_that("an unknown ci provider aborts", {
  dir <- withr::local_tempdir()
  expect_error(create_template_repo(dir, ci = "gitlab"), regexp = "Unknown\\s+CI\\s+provider", class = "rlang_error")
  # Nothing is written for a rejected provider.
  expect_length(list_all_files(dir), 0)
})

test_that("ci = NA and ci = character(0) abort", {
  dir <- withr::local_tempdir()
  # Anchored on the `ci` argument: the examples/overwrite aborts share the
  # "single TRUE/FALSE" tail.
  single_flag <- "`ci`\\s+must\\s+be\\s+a\\s+single"
  expect_error(create_template_repo(dir, ci = NA), regexp = single_flag, class = "rlang_error")
  expect_error(create_template_repo(dir, ci = character(0)), regexp = single_flag, class = "rlang_error")
  expect_length(list_all_files(dir), 0)
})

test_that("the written bitbucket-pipelines.yml parses as YAML and the Jenkinsfile is a declarative pipeline that runs lint_template_repo", {
  # Confirms the two new assets are what they claim to be, read back from the
  # files this function actually wrote -- never asserted via file.exists().
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = c("bitbucket", "jenkins"))

  pipeline <- yaml::read_yaml(file.path(dir, "bitbucket-pipelines.yml"))
  expect_true(is.list(pipeline))
  expect_type(pipeline$image, "character")

  jenkinsfile <- paste(readLines(file.path(dir, "Jenkinsfile")), collapse = "\n")
  expect_match(jenkinsfile, "pipeline {", fixed = TRUE)
  expect_match(jenkinsfile, "lint_template_repo", fixed = TRUE)
})

# ---- examples = FALSE --------------------------------------------------------

test_that("examples = FALSE writes no template files, and validate_template() reports no_templates on the result", {
  # Ties the two features together: an examples = FALSE scaffold is exactly
  # the shape no_templates (R/validateTemplate.R) exists to catch.
  dir <- withr::local_tempdir()
  create_template_repo(dir, examples = FALSE)

  expect_setequal(list_all_files(dir), c(".gitignore", "README.md", ".github/workflows/validate-templates.yml"))

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

  expect_error(create_template_repo(dir, overwrite = FALSE), regexp = "already\\s+exist", class = "rlang_error")

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
    regexp = "already\\s+exist",
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

  expect_error(create_template_repo(file_path), regexp = "names\\s+an\\s+existing\\s+file", class = "rlang_error")
  # The file is still a file with its original content.
  expect_false(dir.exists(file_path))
  expect_identical(readLines(file_path), "not a directory")
})

test_that("a non-scalar path aborts", {
  bad_path <- "single\\s+non-empty\\s+directory\\s+path"
  expect_error(create_template_repo(character(0)), regexp = bad_path, class = "rlang_error")
  expect_error(create_template_repo(NA_character_), regexp = bad_path, class = "rlang_error")
  expect_error(create_template_repo(c("a", "b")), regexp = bad_path, class = "rlang_error")
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


test_that("the scaffold puts README.md and dta-app.yaml at the root and one kind per folder", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  expect_setequal(list_all_files(dir), scaffold_example_files)
  # No template file sits in the root, and no folder holds another kind's file.
  expect_length(list.files(dir, pattern = "[.]dta-.*[.]ya?ml$"), 0)
  expect_setequal(
    list.files(file.path(dir, "templates")),
    c("starter.dta-template.yaml", "starter_dataset.dta-dataset-template.yaml")
  )
  expect_identical(list.files(file.path(dir, "parties")), "starter_supplier.dta-party.yaml")
  expect_identical(list.files(file.path(dir, "vocabulary")), "starter_terms.dta-vocabulary.yaml")
  expect_identical(list.files(file.path(dir, "export-templates")), "README.md")
})

test_that("examples = TRUE and each ci provider write exactly the documented file set", {
  ci_files <- list(
    github = ".github/workflows/validate-templates.yml",
    bitbucket = "bitbucket-pipelines.yml",
    jenkins = "Jenkinsfile"
  )
  for (provider in names(ci_files)) {
    dir <- withr::local_tempdir()
    create_template_repo(dir, ci = provider)
    expect_setequal(list_all_files(dir), c(scaffold_example_files, ci_files[[provider]]))
  }

  none <- withr::local_tempdir()
  create_template_repo(none, ci = FALSE)
  expect_setequal(list_all_files(none), scaffold_example_files)

  all3 <- withr::local_tempdir()
  create_template_repo(all3, ci = names(ci_files))
  expect_setequal(list_all_files(all3), c(scaffold_example_files, unlist(ci_files)))
})

test_that("examples = FALSE writes only README.md, .gitignore and the CI file(s), no kind folders", {
  bare <- withr::local_tempdir()
  create_template_repo(bare, ci = FALSE, examples = FALSE)
  expect_setequal(list_all_files(bare), c("README.md", ".gitignore"))
  expect_false(any(dir.exists(file.path(bare, c("templates", "parties", "vocabulary", "export-templates")))))

  with_ci <- withr::local_tempdir()
  create_template_repo(with_ci, ci = c("github", "jenkins"), examples = FALSE)
  expect_setequal(
    list_all_files(with_ci),
    c("README.md", ".gitignore", ".github/workflows/validate-templates.yml", "Jenkinsfile")
  )
})

test_that("dta-app.yaml is scaffolded in the root with the examples, is not a template, and parses with no problems", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  cfg <- file.path(dir, "dta-app.yaml")
  expect_true(file.exists(cfg))
  expect_false(file.exists(file.path(dir, "templates", "dta-app.yaml")))
  parsed <- DTAtools:::.dta_template_engine_get("parse_dta_app_config")(cfg)
  expect_equal(nrow(parsed$problems), 0)

  bare <- withr::local_tempdir()
  create_template_repo(bare, ci = FALSE, examples = FALSE)
  expect_false(file.exists(file.path(bare, "dta-app.yaml")))
  # No stray copy of the app config anywhere in the bare scaffold either.
  expect_false(any(basename(list_all_files(bare)) == "dta-app.yaml"))
  expect_false(any(basename(list_all_files(dir)) == "dta-app.yaml" &
    dirname(list_all_files(dir)) != "."))
})

test_that("a scaffold file moved into the wrong folder is template_misplaced, pointing at the right one", {
  moves <- list(
    list(from = "parties/starter_supplier.dta-party.yaml", to = "templates", right = "parties"),
    list(from = "vocabulary/starter_terms.dta-vocabulary.yaml", to = "parties", right = "vocabulary"),
    list(from = "templates/starter.dta-template.yaml", to = "vocabulary", right = "templates"),
    list(from = "templates/starter_dataset.dta-dataset-template.yaml", to = ".", right = "templates")
  )
  for (mv in moves) {
    dir <- withr::local_tempdir()
    create_template_repo(dir, ci = FALSE)
    dest <- if (mv$to == ".") file.path(dir, basename(mv$from)) else file.path(dir, mv$to, basename(mv$from))
    expect_true(file.rename(file.path(dir, mv$from), dest))

    result <- validate_template(dir)
    rows <- result[result$code == "template_misplaced", , drop = FALSE]
    expect_equal(nrow(rows), 1, info = mv$from)
    expect_identical(rows$severity, "error", info = mv$from)
    expect_identical(rows$file, dest, info = mv$from)
    expect_match(rows$message, sprintf("'%s/'", mv$right), fixed = TRUE, info = mv$from)
    expect_error(validate_template(dir, strict = TRUE), class = "rlang_error", regexp = "template_misplaced")
  }
})

test_that("a scaffold dta-app.yaml moved into templates/ is app_config_misplaced", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  file.rename(file.path(dir, "dta-app.yaml"), file.path(dir, "templates", "dta-app.yaml"))
  result <- validate_template(dir)
  expect_identical(result$code, "app_config_misplaced")
})

test_that("pointing validate_template() at a scaffold's kind folder is library_subfolder_given", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  for (sub in c("templates", "parties", "vocabulary")) {
    result <- validate_template(file.path(dir, sub))
    expect_identical(result$code, "library_subfolder_given", info = sub)
  }
})

test_that("a non-docx file dropped into the scaffold's export-templates/ is export_template_invalid, a warning", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  writeLines("x", file.path(dir, "export-templates", "notes.txt"))
  result <- validate_template(dir, strict = TRUE)
  expect_identical(result$code, "export_template_invalid")
  expect_identical(result$severity, "warning")
})

test_that("the scaffold README is byte-identical to the complete skeleton README", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  skeleton <- system.file("extdata", "template-repo-skeleton", "README.md", package = "DTAtools")
  readme <- file.path(dir, "README.md")
  expect_identical(unname(tools::md5sum(readme)), unname(tools::md5sum(skeleton)))
  headings <- grep("^## ", readLines(readme, warn = FALSE), value = TRUE)
  expect_true(all(c(
    "## Folder layout", "## Connecting the DTAtools app",
    "## Validating", "## Troubleshooting"
  ) %in% headings))
})

test_that("every folder the scaffold writes is one the engine reads", {
  # The scaffold copies a directory tree; the engine owns the folder names.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  engine <- function(f) DTAtools:::.dta_template_engine_get(f)
  folders <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  expect_setequal(folders, c(engine("dta_template_kind_subdirs")(), engine("dta_export_templates_subdir")()))
  for (kind in engine("dta_template_all_kinds")()) {
    expect_true(dir.exists(engine("dta_template_kind_dir")(dir, kind)), info = kind)
  }
})

test_that("a templates/ collision aborts before writing any scaffold file", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "templates"))
  writeLines("existing template", file.path(dir, "templates", "starter.dta-template.yaml"))
  before <- md5_snapshot(dir)
  expect_error(create_template_repo(dir), "already\\s+exist", class = "rlang_error")
  expect_error(create_template_repo(dir), "starter.dta-template.yaml", fixed = TRUE)
  expect_identical(md5_snapshot(dir), before)
  # The colliding file keeps its content and is the only file in the directory.
  expect_identical(readLines(file.path(dir, "templates", "starter.dta-template.yaml")), "existing template")
  expect_identical(list_all_files(dir), "templates/starter.dta-template.yaml")
})

test_that("overwrite = FALSE conflict message shows braces in a path literally", {
  withr::local_options(cli.width = 500)
  dir <- file.path(withr::local_tempdir(), "repo{1+1}")
  create_template_repo(dir)
  err <- expect_error(
    create_template_repo(dir, overwrite = FALSE),
    "already\\s+exist",
    class = "rlang_error"
  )
  expect_match(conditionMessage(err), "repo{1+1}", fixed = TRUE)
  expect_no_match(conditionMessage(err), "repo2", fixed = TRUE)
})
