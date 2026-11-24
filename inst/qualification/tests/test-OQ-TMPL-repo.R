# create_template_repo(): scaffolding a new template repository, and the
# guarantee that both it and every packaged example validate cleanly.
# REQ-TMPL-026 .. REQ-TMPL-030, REQ-TMPL-060, REQ-TMPL-065.
#
# Every call here writes into qa_tempdir() -- never into the package's own
# inst/extdata/ -- and every create_template_repo() call is wrapped in
# suppressMessages() because its cli_inform() progress note is not itself
# under test; what it wrote and what it returned are.

tmpl_fixture <- function(...) file.path(qa_suite_root(), "fx", "t", ...)

# Expected scaffold paths below are written from REQ-TMPL-026, REQ-TMPL-060
# and REQ-TMPL-065, not discovered from the installed skeleton or the function
# under qualification.
editor_files <- c(
  "schemas/dta-template-library.schema.json",
  "schemas/dta-app.schema.json",
  ".vscode/settings.json"
)
skill_files <- file.path(
  ".claude/skills/dta-template-authoring",
  c(
    "SKILL.md", "references/composition.md", "references/linting.md",
    "references/repository-architecture.md"
  )
)

# ---- REQ-TMPL-026: the default write set, and examples = FALSE ------------

test_that("OQ-TMPL-049 | the defaults write the scaffolding, the skeleton library files, a GitHub workflow and the agent skill | REQ-TMPL-026 REQ-TMPL-060", {
  dir <- qa_tempdir()
  out <- suppressMessages(create_template_repo(dir))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  expected <- sort(c(
    ".gitignore", "README.md",
    "templates/starter.dta-template.yaml",
    "templates/starter_dataset.dta-dataset-template.yaml",
    "parties/starter_supplier.dta-party.yaml",
    "vocabulary/starter_terms.dta-vocabulary.yaml",
    "dta-app.yaml",
    "export-templates/README.md",
    ".github/workflows/validate-templates.yml",
    editor_files,
    skill_files
  ))
  qa_step("exactly the documented default file set is written", expected, written)
  bundled <- system.file("extdata", "agent-skills", "dta-template-authoring", package = "DTAtools")
  qa_step(
    "with the defaults too, every skill file is byte-identical to the bundled copy",
    unname(tools::md5sum(file.path(bundled, sub("^[.]claude/skills/dta-template-authoring/", "", skill_files)))),
    unname(tools::md5sum(file.path(dir, skill_files)))
  )
  qa_step("the return value names the directory just written to", normalizePath(dir, winslash = "/"), out)
})

test_that("OQ-TMPL-050 | examples = FALSE omits the library files but keeps the scaffolding | REQ-TMPL-026", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  qa_step(
    "only .gitignore, README.md, the editor assets, the default GitHub workflow and the agent skill remain",
    sort(c(".gitignore", "README.md", editor_files, ".github/workflows/validate-templates.yml", skill_files)), written
  )
})

# ---- REQ-TMPL-027: the ci argument ------------------------------------------

test_that("OQ-TMPL-051 | ci = FALSE writes no CI file and no .github directory | REQ-TMPL-027", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE, ci = FALSE, agent_skill = FALSE))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  qa_step("only the scaffolding files and the editor assets", sort(c(".gitignore", "README.md", editor_files)), written)
  qa_check("no .github directory was created at all", !dir.exists(file.path(dir, ".github")))
})

test_that("OQ-TMPL-052 | ci accepts a combination of providers, each to its own destination | REQ-TMPL-027", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(
    dir,
    examples = FALSE, ci = c("github", "bitbucket", "jenkins"), agent_skill = FALSE
  ))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  expected <- sort(c(
    ".gitignore", "README.md", "bitbucket-pipelines.yml", "Jenkinsfile",
    ".github/workflows/validate-templates.yml", editor_files
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

# ---- REQ-TMPL-060: the agent skill ------------------------------------------

test_that("OQ-TMPL-138 | the agent skill is written byte-identical, with the front matter an agent loads it by | REQ-TMPL-060", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE, ci = FALSE))

  written <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  qa_step(
    "examples = FALSE and ci = FALSE still write the skill beside the scaffolding",
    sort(c(".gitignore", "README.md", editor_files, skill_files)), written
  )

  bundled <- system.file("extdata", "agent-skills", "dta-template-authoring", package = "DTAtools")
  rel <- sub("^[.]claude/skills/dta-template-authoring/", "", skill_files)
  qa_step(
    "every skill file is byte-identical to the bundled copy",
    unname(tools::md5sum(file.path(bundled, rel))),
    unname(tools::md5sum(file.path(dir, skill_files)))
  )

  lines <- readLines(file.path(dir, skill_files[[1]]), encoding = "UTF-8")
  fences <- which(lines == "---")
  qa_check("SKILL.md opens with a front matter block", length(fences) >= 2 && fences[[1]] == 1L)
  front <- yaml::yaml.load(paste(lines[(fences[[1]] + 1L):(fences[[2]] - 1L)], collapse = "\n"))
  qa_step("whose name is the skill's folder name", "dta-template-authoring", front$name)
  qa_check(
    "and whose description is a non-empty string",
    is.character(front$description) && length(front$description) == 1L && nzchar(front$description)
  )
})

test_that("OQ-TMPL-139 | agent_skill = FALSE writes no .claude directory, and a non-logical value is rejected | REQ-TMPL-060", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, agent_skill = FALSE))
  qa_check("no .claude directory exists", !dir.exists(file.path(dir, ".claude")))

  bad <- qa_tempdir()
  err <- tryCatch(create_template_repo(bad, agent_skill = "yes"), error = function(e) e)
  qa_check("a character value raises a condition", inherits(err, "condition"))
  qa_check("naming agent_skill", grepl("agent_skill", conditionMessage(err), fixed = TRUE))
  qa_step("and nothing is written", character(0), list.files(bad, all.files = TRUE, no.. = TRUE))
})

test_that("OQ-TMPL-140 | a pre-existing skill file is a conflict under the overwrite guard | REQ-TMPL-060 REQ-TMPL-028", {
  dir <- qa_tempdir()
  skill_md <- file.path(dir, skill_files[[1]])
  dir.create(dirname(skill_md), recursive = TRUE)
  writeLines("team edit", skill_md)

  err <- tryCatch(create_template_repo(dir), error = function(e) e)
  qa_check("without overwrite, a condition is raised", inherits(err, "condition"))
  qa_check("naming the conflicting SKILL.md", grepl("SKILL.md", conditionMessage(err), fixed = TRUE))
  qa_step("the existing file is left as it was", "team edit", readLines(skill_md))
  qa_step(
    "and nothing else is written",
    skill_files[[1]], list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  )

  suppressMessages(create_template_repo(dir, overwrite = TRUE))
  bundled <- system.file("extdata", "agent-skills", "dta-template-authoring", "SKILL.md", package = "DTAtools")
  qa_step(
    "overwrite = TRUE replaces it with the bundled SKILL.md",
    unname(tools::md5sum(bundled)), unname(tools::md5sum(skill_md))
  )
})

# ---- REQ-TMPL-065: the editor assets ----------------------------------------

test_that("OQ-TMPL-151 | the JSON Schemas and the VS Code setting are written byte-identical, whatever examples, ci and agent_skill say | REQ-TMPL-065", {
  bundled <- c(
    system.file("extdata", "schemas", "dta-template-library.schema.json", package = "DTAtools"),
    system.file("extdata", "schemas", "dta-app.schema.json", package = "DTAtools"),
    system.file("extdata", "editor", "yaml-schema-mapping.json", package = "DTAtools")
  )
  qa_check("the three bundled sources are installed", all(nzchar(bundled)))

  defaults <- qa_tempdir()
  suppressMessages(create_template_repo(defaults))
  qa_step(
    "with the defaults every editor asset is byte-identical to its bundled source",
    unname(tools::md5sum(bundled)), unname(tools::md5sum(file.path(defaults, editor_files)))
  )

  bare <- qa_tempdir()
  suppressMessages(create_template_repo(bare, examples = FALSE, ci = FALSE, agent_skill = FALSE))
  qa_step(
    "and with examples, ci and agent_skill all off",
    unname(tools::md5sum(bundled)), unname(tools::md5sum(file.path(bare, editor_files)))
  )
})

test_that("OQ-TMPL-152 | both schemas declare draft-07 and the setting assigns each its file name patterns by paths that resolve | REQ-TMPL-065", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, examples = FALSE, ci = FALSE, agent_skill = FALSE))

  for (name in c("dta-template-library.schema.json", "dta-app.schema.json")) {
    text <- paste(readLines(file.path(dir, "schemas", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    qa_check(paste(name, "is valid JSON"), jsonlite::validate(text))
    qa_step(
      paste(name, "declares the draft-07 meta-schema"),
      "http://json-schema.org/draft-07/schema#",
      jsonlite::fromJSON(text, simplifyVector = FALSE)[["$schema"]]
    )
  }

  schemas <- jsonlite::fromJSON(
    file.path(dir, ".vscode", "settings.json"),
    simplifyVector = FALSE
  )[["yaml.schemas"]]
  qa_step(
    "yaml.schemas names exactly the two schema files, relative to the repository root",
    c("./schemas/dta-app.schema.json", "./schemas/dta-template-library.schema.json"),
    sort(names(schemas))
  )
  qa_check(
    "and each name resolves to a file of the written repository",
    all(file.exists(file.path(dir, names(schemas))))
  )

  # The patterns are checked as written, not by emulating an editor's glob
  # dialect: what an editor does with them is outside this suite.
  qa_step(
    "the library schema is assigned the pattern of each of the four template kinds",
    c(
      "**/*.dta-dataset-template.y*ml", "**/*.dta-party.y*ml",
      "**/*.dta-template.y*ml", "**/*.dta-vocabulary.y*ml"
    ),
    sort(unlist(schemas[["./schemas/dta-template-library.schema.json"]]))
  )
  qa_step(
    "and the app schema the pattern of dta-app.yaml and dta-app.yml",
    "dta-app.y*ml",
    unlist(schemas[["./schemas/dta-app.schema.json"]])
  )
})

test_that("OQ-TMPL-153 | a pre-existing .vscode/settings.json is a conflict under the overwrite guard | REQ-TMPL-065 REQ-TMPL-028", {
  dir <- qa_tempdir()
  settings <- file.path(dir, ".vscode", "settings.json")
  dir.create(dirname(settings), recursive = TRUE)
  writeLines("team edit", settings)

  err <- tryCatch(create_template_repo(dir), error = function(e) e)
  qa_check("without overwrite, a condition is raised", inherits(err, "condition"))
  qa_check("naming the conflicting settings.json", grepl("settings.json", conditionMessage(err), fixed = TRUE))
  qa_step("the existing file is left as it was", "team edit", readLines(settings))
  qa_step(
    "and nothing else is written",
    ".vscode/settings.json", list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  )

  suppressMessages(create_template_repo(dir, overwrite = TRUE))
  bundled <- system.file("extdata", "editor", "yaml-schema-mapping.json", package = "DTAtools")
  qa_step(
    "overwrite = TRUE replaces it with the bundled setting",
    unname(tools::md5sum(bundled)), unname(tools::md5sum(settings))
  )
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

  # A conflict at a destination inside the library root (the app configuration
  # file) aborts in the same way and writes nothing else.
  dir2 <- qa_tempdir()
  writeLines("pre-existing", file.path(dir2, "dta-app.yaml"))
  err2 <- tryCatch(create_template_repo(dir2), error = function(e) e)
  qa_check("a conflicting dta-app.yaml raises a condition naming it", inherits(err2, "condition") && grepl("dta-app.yaml", conditionMessage(err2), fixed = TRUE))
  qa_step(
    "and nothing else was written there either",
    "dta-app.yaml", list.files(dir2, all.files = TRUE, no.. = TRUE)
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
  file <- file.path(qa_tempdir(), "existing-file")
  writeLines("x", file)

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

test_that("OQ-TMPL-059 | the packaged demo library under inst/extdata/library validates with zero errors and zero warnings | REQ-TMPL-030", {
  result <- validate_template(system.file("extdata", "library", package = "DTAtools"))
  qa_step("zero rows of any severity", 0L, nrow(result))
})

test_that("OQ-TMPL-060 | the bundled skeleton validates cleanly as a library root under strict = TRUE, and a kind folder of it does not | REQ-TMPL-030", {
  skeleton_dir <- system.file("extdata", "template-repo-skeleton", package = "DTAtools")
  outcome <- tryCatch(
    {
      validate_template(skeleton_dir, strict = TRUE)
      "no condition"
    },
    error = function(e) conditionMessage(e)
  )
  qa_step("the packaged skeleton is not itself the broken example it warns others against", "no condition", outcome)

  folder <- validate_template(file.path(skeleton_dir, "templates"))
  qa_step(
    "validating its templates/ folder instead of the root is the sub-folder error, not a clean pass",
    "library_subfolder_given", folder$code
  )
})
