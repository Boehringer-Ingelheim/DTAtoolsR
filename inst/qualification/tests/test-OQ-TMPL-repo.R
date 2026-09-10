# create_template_repo(): scaffolding a new template repository, and the
# guarantee that both it and every packaged example validate cleanly.
# REQ-TMPL-026 .. REQ-TMPL-030.
#
# Every call here writes into qa_tempdir() -- never into the package's own
# inst/extdata/ -- and every create_template_repo() call is wrapped in
# suppressMessages() because its cli_inform() progress note is not itself
# under test; what it wrote and what it returned are.

tmpl_fixture <- function(...) file.path(qa_suite_root(), "fixtures", "tpl", ...)

# The four skeleton template files, discovered the same way
# .dta_create_repo_example_files() itself does -- so this list can never
# silently drift from what the function under test actually copies.
tmpl_skeleton_example_files <- function() {
  skeleton_dir <- system.file("extdata", "template-repo-skeleton", package = "DTAtools")
  list.files(skeleton_dir, pattern = "[.]ya?ml$")
}

# ---- REQ-TMPL-026: the default write set, and examples = FALSE ------------

test_that("OQ-TMPL-049 | the defaults write the scaffolding, the four skeleton templates and a GitHub workflow | REQ-TMPL-026", {
  dir <- qa_tempdir()
  out <- suppressMessages(create_template_repo(dir))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  expected <- sort(c(
    ".gitignore", "README.md", tmpl_skeleton_example_files(),
    ".github/workflows/validate-templates.yml"
  ))
  qa_step("exactly the documented default file set is written", expected, written)
  qa_step("the return value names the directory just written to", normalizePath(dir, winslash = "/"), out)
})

test_that("OQ-TMPL-050 | examples = FALSE omits the four templates but keeps the scaffolding | REQ-TMPL-026", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  qa_step(
    "only .gitignore, README.md and the default GitHub workflow remain",
    sort(c(".gitignore", "README.md", ".github/workflows/validate-templates.yml")), written
  )
})

# ---- REQ-TMPL-027: the ci argument ------------------------------------------

test_that("OQ-TMPL-051 | ci = FALSE writes no CI file and no .github directory | REQ-TMPL-027", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE, ci = FALSE))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  qa_step("only the two scaffolding files", sort(c(".gitignore", "README.md")), written)
  qa_check("no .github directory was created at all", !dir.exists(file.path(dir, ".github")))
})

test_that("OQ-TMPL-052 | ci accepts a combination of providers, each to its own destination | REQ-TMPL-027", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE, ci = c("github", "bitbucket", "jenkins")))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  expected <- sort(c(
    ".gitignore", "README.md", "bitbucket-pipelines.yml", "Jenkinsfile",
    ".github/workflows/validate-templates.yml"
  ))
  qa_step("each provider's file lands at its own documented path", expected, written)
})

test_that("OQ-TMPL-053 | an unrecognised ci provider, or an empty ci vector, is rejected | REQ-TMPL-027", {
  err_unknown <- tryCatch(create_template_repo(qa_tempdir(), ci = "gitlab"), error = function(e) e)
  qa_check("an unknown provider name raises a condition", inherits(err_unknown, "condition"))
  qa_check("naming the bad value", grepl("gitlab", conditionMessage(err_unknown), fixed = TRUE))

  # character(0) is not the same instruction as FALSE, even though both name
  # "no providers" in plain language -- only FALSE is accepted for that.
  err_empty <- tryCatch(create_template_repo(qa_tempdir(), ci = character(0)), error = function(e) e)
  qa_check("an empty character vector is rejected too, not treated as FALSE", inherits(err_empty, "condition"))
})

# ---- REQ-TMPL-028: the overwrite guard --------------------------------------

test_that("OQ-TMPL-054 | without overwrite, one conflicting file aborts the whole call and writes nothing new | REQ-TMPL-028", {
  dir <- qa_tempdir()
  writeLines("pre-existing", file.path(dir, ".gitignore"))

  err <- tryCatch(create_template_repo(dir), error = function(e) e)
  qa_check("a condition is raised", inherits(err, "condition"))
  qa_check("naming the conflicting file", grepl(".gitignore", conditionMessage(err), fixed = TRUE))

  qa_step(
    "nothing else was written -- not even the files that did NOT conflict",
    ".gitignore", list.files(dir, all.files = TRUE, no.. = TRUE)
  )
})

test_that("OQ-TMPL-055 | overwrite = TRUE replaces every conflicting file | REQ-TMPL-028", {
  dir <- qa_tempdir()
  writeLines("pre-existing", file.path(dir, ".gitignore"))

  suppressMessages(create_template_repo(dir, overwrite = TRUE))
  qa_step(
    "the placeholder content is gone, replaced by the real .gitignore",
    c(".Rproj.user", ".Rhistory", "*.Rproj"), readLines(file.path(dir, ".gitignore"))
  )
})

test_that("OQ-TMPL-056 | a path naming an existing file, not a directory, is rejected outright | REQ-TMPL-028", {
  file <- tempfile()
  writeLines("x", file)
  withr::defer(unlink(file))

  err <- tryCatch(create_template_repo(file, overwrite = TRUE), error = function(e) e)
  qa_check("rejected even with overwrite = TRUE", inherits(err, "condition"))
})

# ---- REQ-TMPL-029: return value ---------------------------------------------

test_that("OQ-TMPL-057 | create_template_repo() returns the normalised absolute path, invisibly | REQ-TMPL-029", {
  dir <- qa_tempdir()
  result <- withVisible(suppressMessages(create_template_repo(dir)))

  qa_check("the call is invisible at the top level", !result$visible)
  qa_step("and the value is the normalised absolute path", normalizePath(dir, winslash = "/"), result$value)
})

# ---- REQ-TMPL-030: the shipped examples and skeleton validate cleanly ------

test_that("OQ-TMPL-058 | a freshly scaffolded repository validates cleanly, unmodified | REQ-TMPL-030", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir))

  outcome <- tryCatch(
    {
      validate_template(dir, strict = TRUE)
      "no condition"
    },
    error = function(e) conditionMessage(e)
  )
  qa_step("strict = TRUE raises nothing against what create_template_repo() itself just wrote", "no condition", outcome)
})

test_that("OQ-TMPL-059 | every template bundled under inst/extdata/templates validates with zero errors and zero warnings | REQ-TMPL-030", {
  result <- validate_template(system.file("extdata", "templates", package = "DTAtools"))
  qa_step("zero rows of any severity", 0L, nrow(result))
})

test_that("OQ-TMPL-060 | the bundled skeleton itself validates cleanly under strict = TRUE | REQ-TMPL-030", {
  skeleton_dir <- system.file("extdata", "template-repo-skeleton", package = "DTAtools")
  outcome <- tryCatch(
    {
      validate_template(skeleton_dir, strict = TRUE)
      "no condition"
    },
    error = function(e) conditionMessage(e)
  )
  qa_step("the packaged skeleton is not itself the broken example it warns others against", "no condition", outcome)
})
