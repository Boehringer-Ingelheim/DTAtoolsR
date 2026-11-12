# lint_template_repo(): the repository linter. REQ-TMPL-046 and REQ-TMPL-047.
#
# It returns every validate_template() row plus three repository checks
# (folder_name_case, id_filename_mismatch, file_ignored) and aborts on findings
# according to fail_on. Every repository below is built in qa_tempdir() from a
# scaffold of create_template_repo(), which REQ-TMPL-030 already requires to be
# clean; the Word files the skeleton does not carry are not needed here.

lint_columns <- c("file", "kind", "id", "version", "severity", "code", "message")

# A fresh scaffolded repository; the directory is removed when the calling test ends.
lint_scaffold <- function(env = parent.frame(), ...) {
  dir <- qa_tempdir(env)
  suppressMessages(create_template_repo(dir, ...))
  dir
}

# The text lines of the scaffold's party profile, with these fields replaced.
lint_party_lines <- function(dir, id = "starter_supplier", version = '"1.0"') {
  lines <- readLines(file.path(dir, "parties", "starter_supplier.dta-party.yaml"))
  lines <- sub("^id: .*$", paste0("id: ", id), lines)
  sub("^version: .*$", paste0("version: ", version), lines)
}

lint_write <- function(dir, rel, lines) {
  dir.create(dirname(file.path(dir, rel)), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(dir, rel))
  invisible(file.path(dir, rel))
}

# Everything printed while the call runs, as one string; the call's own errors
# propagate.
lint_printed <- function(expr) {
  seen <- character(0)
  withCallingHandlers(
    expr,
    message = function(m) {
      seen <<- c(seen, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  gsub("\\s+", " ", paste(seen, collapse = " "))
}

lint_class_of <- function(expr) {
  tryCatch(
    {
      expr
      "no condition"
    },
    error = function(e) class(e)
  )
}

# ---- REQ-TMPL-046: what is reported ------------------------------------------

test_that("OQ-TMPL-107 | a fresh scaffold and the packaged library lint to zero rows, even at fail_on = warning | REQ-TMPL-046", {
  dir <- lint_scaffold()
  result <- suppressMessages(lint_template_repo(dir, fail_on = "warning"))
  qa_step("a fresh create_template_repo() has zero rows", 0L, nrow(result))
  qa_step("and the full column set", lint_columns, names(result))

  bundled <- system.file("extdata", "library", package = "DTAtools")
  result <- suppressMessages(lint_template_repo(bundled, fail_on = "warning"))
  qa_step("the packaged library has zero rows", 0L, nrow(result))
  qa_step("and the full column set", lint_columns, names(result))
})

test_that("OQ-TMPL-108 | the result holds every validate_template() row plus the repository rows, sorted by file and code | REQ-TMPL-046", {
  dir <- lint_scaffold()
  lint_write(dir, "parties/starter_supplier.dta-party.yaml", lint_party_lines(dir, version = "1.10"))
  lint_write(dir, "parties/acme.dta-party.yaml", lint_party_lines(dir, id = "other_party"))

  validated <- validate_template(dir)
  linted <- suppressMessages(lint_template_repo(dir, fail_on = "none"))
  key <- function(d) paste(d$file, d$code)
  qa_check("the unquoted version is among the validate_template() rows", "version_unquoted" %in% validated$code)
  qa_check("every validate_template() row is in the lint result", all(key(validated) %in% key(linted)))
  qa_step(
    "the only additional row is the id_filename_mismatch for acme.dta-party.yaml",
    "id_filename_mismatch",
    setdiff(linted$code, validated$code)
  )
  qa_step("and it is the only one", nrow(validated) + 1L, nrow(linted))
  qa_step("rows are sorted by file", linted$file, sort(linted$file, method = "radix"))
  qa_step("row names are the plain sequence", seq_len(nrow(linted)), as.integer(rownames(linted)))
})

test_that("OQ-TMPL-109 | id_filename_mismatch is a warning on a file named after neither its id nor id plus a qualifier | REQ-TMPL-046", {
  dir <- lint_scaffold()
  lint_write(dir, "parties/acme.dta-party.yaml", lint_party_lines(dir, id = "other_party"))
  lint_write(dir, "parties/starter_supplier_old.dta-party.yaml", lint_party_lines(dir, version = '"0.9"'))
  result <- suppressMessages(lint_template_repo(dir, fail_on = "none"))
  hit <- result[result$code == "id_filename_mismatch", ]
  qa_step("exactly one id_filename_mismatch row", 1L, nrow(hit))
  qa_step("on the file named acme", "acme.dta-party.yaml", basename(hit$file))
  qa_step("at severity warning", "warning", hit$severity)
  qa_step("carrying the id the file declares", "other_party", hit$id)
  qa_step(
    "the file named starter_supplier_old for id starter_supplier has no such row",
    0L, sum(basename(result$file) == "starter_supplier_old.dta-party.yaml" & result$code == "id_filename_mismatch")
  )
})

test_that("OQ-TMPL-110 | a kind folder spelled with other letter case is folder_name_case, an error | REQ-TMPL-046", {
  dir <- lint_scaffold()
  unlink(file.path(dir, "vocabulary"), recursive = TRUE)
  dir.create(file.path(dir, "Vocabulary"))
  file.copy(
    system.file("extdata", "template-repo-skeleton", "vocabulary", "starter_terms.dta-vocabulary.yaml", package = "DTAtools"),
    file.path(dir, "Vocabulary")
  )
  result <- suppressMessages(lint_template_repo(dir, fail_on = "none"))
  hit <- result[result$code == "folder_name_case", ]
  qa_step("exactly one folder_name_case row", 1L, nrow(hit))
  qa_step("on the folder Vocabulary", "Vocabulary", basename(hit$file))
  qa_step("at severity error", "error", hit$severity)
  qa_check("with no kind", is.na(hit$kind))
  qa_check("whose message names the correct spelling", grepl("vocabulary", hit$message, fixed = TRUE))
  qa_check(
    "the default fail_on aborts with the lint condition class",
    "dta_template_lint_error" %in% lint_class_of(suppressMessages(lint_template_repo(dir)))
  )
})

test_that("OQ-TMPL-111 | file_ignored is a warning on a YAML file that declares a template kind but lacks its suffix; other YAML is left alone | REQ-TMPL-046", {
  dir <- lint_scaffold()
  starter <- readLines(file.path(dir, "templates", "starter.dta-template.yaml"))
  lint_write(dir, "templates/forgot_suffix.yaml", sub("^id: .*$", "id: forgot_suffix", starter))
  lint_write(dir, "templates/source_data.yaml", "name: x")
  result <- suppressMessages(lint_template_repo(dir, fail_on = "none"))
  hit <- result[result$code == "file_ignored", ]
  qa_step("exactly one file_ignored row", 1L, nrow(hit))
  qa_step("on the template without its suffix", "forgot_suffix.yaml", basename(hit$file))
  qa_step("at severity warning", "warning", hit$severity)

  # A pipeline definition at the root and the app configuration are not templates.
  bitbucket <- lint_scaffold(ci = "bitbucket")
  qa_check("the Bitbucket pipeline file is at the root", file.exists(file.path(bitbucket, "bitbucket-pipelines.yml")))
  qa_check("the app configuration is at the root", file.exists(file.path(bitbucket, "dta-app.yaml")))
  qa_step(
    "neither the pipeline file nor dta-app.yaml is reported",
    0L, sum(suppressMessages(lint_template_repo(bitbucket, fail_on = "none"))$code == "file_ignored")
  )
})

# ---- REQ-TMPL-047: output, fail_on and arguments -------------------------------

test_that("OQ-TMPL-112 | fail_on decides whether the call aborts, the return is invisible, bad arguments are refused | REQ-TMPL-047", {
  # Only a warning: an id that differs from the file name.
  dir <- lint_scaffold()
  lint_write(dir, "parties/acme.dta-party.yaml", lint_party_lines(dir, id = "other_party"))
  shown <- withVisible(suppressMessages(lint_template_repo(dir)))
  qa_check("a warning alone does not abort the default", is.data.frame(shown$value))
  qa_step("and the return is invisible", FALSE, shown$visible)
  qa_step("with the warning in it", 1L, sum(shown$value$code == "id_filename_mismatch"))
  qa_check(
    "fail_on = 'warning' aborts with the lint condition class",
    "dta_template_lint_error" %in% lint_class_of(suppressMessages(lint_template_repo(dir, fail_on = "warning")))
  )
  qa_check(
    "fail_on = 'none' does not abort",
    is.data.frame(suppressMessages(lint_template_repo(dir, fail_on = "none")))
  )

  # An error: an unquoted version.
  broken <- lint_scaffold()
  lint_write(broken, "parties/starter_supplier.dta-party.yaml", lint_party_lines(broken, version = "1.10"))
  qa_check(
    "an error aborts the default with the lint condition class",
    "dta_template_lint_error" %in% lint_class_of(suppressMessages(lint_template_repo(broken)))
  )
  kept <- withVisible(suppressMessages(lint_template_repo(broken, fail_on = "none")))
  qa_check("fail_on = 'none' returns the rows with the error in them", "version_unquoted" %in% kept$value$code)
  qa_step("invisibly", FALSE, kept$visible)

  # One printed line per finding, and a success line when there is none.
  tidy <- lint_scaffold()
  qa_check(
    "the printout names the code of the finding",
    grepl("id_filename_mismatch", lint_printed(lint_template_repo(dir)), fixed = TRUE)
  )
  qa_check(
    "and says that a clean repository has no findings",
    grepl("No findings", lint_printed(lint_template_repo(tidy)), fixed = TRUE)
  )

  # Arguments are refused before any file is read.
  file <- file.path(qa_tempdir(), "a_file.txt")
  writeLines("x", file)
  refused <- function(expr) "rlang_error" %in% lint_class_of(expr)
  qa_check("a directory that does not exist", refused(lint_template_repo(file.path(dir, "no_such_folder"))))
  qa_check("a path naming a file", refused(lint_template_repo(file)))
  qa_check("a fail_on outside the three values", refused(lint_template_repo(dir, fail_on = "bogus")))
  qa_check("a fail_on of length two", refused(lint_template_repo(dir, fail_on = c("error", "warning"))))
})

test_that("OQ-TMPL-113 | Rscript -e lint_template_repo() exits non-zero when it aborts and zero on a clean repository | REQ-TMPL-047 | tags: subprocess", {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  # A qualification run exercises the installed package; a run from a source
  # checkout has none installed, so the child loads the tree instead (the
  # same fallback PQ-PERF-034 uses).
  source_tree <- tryCatch(pkgload::pkg_path(), error = function(e) getwd())
  # Paths travel in environment variables, never spliced into the source:
  # a Windows tempfile() path holds backslashes the child cannot parse. The
  # parent's library path is handed down so the child tests the same
  # installed copy, as PQ-PERF-034 does.
  code <- paste(
    "if (!requireNamespace('DTAtools', quietly = TRUE))",
    "pkgload::load_all(Sys.getenv('QA_SOURCE_TREE'), quiet = TRUE);",
    "DTAtools::lint_template_repo(Sys.getenv('QA_REPO'))"
  )
  run <- function(dir) {
    withr::with_envvar(
      c(
        QA_SOURCE_TREE = source_tree, QA_REPO = normalizePath(dir, winslash = "/"),
        R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep)
      ),
      suppressWarnings(system2(
        rscript, c("--vanilla", "--no-init-file", "-e", shQuote(code)),
        stdout = TRUE, stderr = TRUE
      ))
    )
  }

  broken <- lint_scaffold()
  lint_write(broken, "parties/starter_supplier.dta-party.yaml", lint_party_lines(broken, version = "1.10"))
  out <- run(broken)
  qa_step("a repository with an error ends the process with status 1", 1L, attr(out, "status"))
  qa_check("and the output names the finding", any(grepl("version_unquoted", out, fixed = TRUE)))

  repo <- lint_scaffold()
  clean <- run(repo)
  qa_check("a clean repository ends the process with status zero", is.null(attr(clean, "status")))
})
