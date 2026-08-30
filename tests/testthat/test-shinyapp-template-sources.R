# Coverage for inst/shiny/dta_app/R/template_sources.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why the
# app's helper files must be sourced into a private environment for testing).

# Every DTATOOLS_TEMPLATE_* variable is cleared for the duration of each test.
# WHY: a developer machine with DTATOOLS_TEMPLATE_SOURCES exported would flip
# the app into private-only mode and fail unrelated tests confusingly.
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

# A local bare git repo (no network) with one committed template file, used
# by the resolve_git_source()/resolve_template_source() tests below. Returns
# the bare repo path.
make_bare_template_repo <- function() {
  bare_dir <- withr::local_tempdir(.local_envir = parent.frame())
  work_dir <- withr::local_tempdir(.local_envir = parent.frame())

  bare_dir <- normalizePath(bare_dir, winslash = "/", mustWork = TRUE)
  work_dir <- normalizePath(work_dir, winslash = "/", mustWork = TRUE)

  run_git <- function(args, wd) {
    old <- setwd(wd)
    on.exit(setwd(old))
    # shQuote() every arg: system2() on Windows only quotes the command
    # itself, so "add template" as a single -m argument would otherwise
    # split into two command-line tokens for git.
    system2("git", shQuote(args), stdout = TRUE, stderr = TRUE)
  }

  run_git(c("init", "--bare", "--quiet", bare_dir), dirname(bare_dir))
  run_git(c("clone", "--quiet", bare_dir, work_dir), dirname(work_dir))
  run_git(c("config", "user.email", "test@example.com"), work_dir)
  run_git(c("config", "user.name", "Test User"), work_dir)
  writeLines("kind: dta_creation_template", file.path(work_dir, "example.dta-template.yaml"))
  run_git(c("add", "."), work_dir)
  run_git(c("commit", "--quiet", "-m", "add template"), work_dir)
  run_git(c("push", "--quiet", "origin", "HEAD:main"), work_dir)
  run_git(c("symbolic-ref", "HEAD", "refs/heads/main"), bare_dir)

  bare_dir
}

# ---- parse_template_sources() -----------------------------------------------

test_that("parse_template_sources() returns an empty list for an empty spec", {
  fn <- app_fn("parse_template_sources")
  expect_equal(fn(""), list())
  expect_equal(fn("   "), list())
})

test_that("parse_template_sources() parses a single dir: entry", {
  fn <- app_fn("parse_template_sources")
  result <- fn("dir:/opt/dta-templates")

  expect_length(result, 1)
  expect_equal(result[[1]]$scheme, "dir")
  expect_equal(result[[1]]$locator, "/opt/dta-templates")
  expect_equal(result[[1]]$name, "dta-templates")
  expect_true(is.na(result[[1]]$ref))
})

test_that("parse_template_sources() parses three mixed entries in order", {
  fn <- app_fn("parse_template_sources")
  result <- fn("dir:/opt/a; pkg:mypkg; git:https://host/repo.git#main")

  expect_length(result, 3)
  expect_equal(vapply(result, `[[`, character(1), "scheme"), c("dir", "pkg", "git"))
  expect_equal(result[[2]]$locator, "mypkg")
  expect_equal(result[[3]]$locator, "https://host/repo.git")
  expect_equal(result[[3]]$ref, "main")
})

test_that("parse_template_sources() honours an explicit name= prefix", {
  fn <- app_fn("parse_template_sources")
  result <- fn("myname=dir:/opt/a")

  expect_equal(result[[1]]$name, "myname")
})

test_that("parse_template_sources() derives the default name from the locator, stripping .git", {
  fn <- app_fn("parse_template_sources")
  result <- fn("git:https://host/team/templates.git")

  expect_equal(result[[1]]$name, "templates")
})

test_that("parse_template_sources() splits git:https://... on the FIRST colon only", {
  fn <- app_fn("parse_template_sources")
  result <- fn("git:https://host/x.git#main")

  expect_equal(result[[1]]$scheme, "git")
  expect_equal(result[[1]]$locator, "https://host/x.git")
  expect_equal(result[[1]]$ref, "main")
})

test_that("parse_template_sources() skips a trailing empty entry", {
  fn <- app_fn("parse_template_sources")
  result <- fn("dir:/opt/a;")

  expect_length(result, 1)
})

test_that("parse_template_sources() defaults a git: ref to HEAD when absent", {
  fn <- app_fn("parse_template_sources")
  result <- fn("git:https://host/x.git")

  expect_equal(result[[1]]$ref, "HEAD")
})

test_that("parse_template_sources() warns and drops a #ref on a dir: source", {
  fn <- app_fn("parse_template_sources")
  expect_warning(fn("dir:/opt/a#main"), regexp = "Ignoring.*#ref")

  result <- suppressWarnings(fn("dir:/opt/a#main"))
  expect_true(is.na(result[[1]]$ref))
})

test_that("parse_template_sources() aborts on an unknown scheme", {
  fn <- app_fn("parse_template_sources")
  expect_error(fn("ftp:/opt/a"), regexp = "unknown scheme")
})

test_that("parse_template_sources() aborts on a missing colon", {
  fn <- app_fn("parse_template_sources")
  expect_error(fn("notasource"), regexp = "missing the required")
})

test_that("parse_template_sources() aborts on an empty locator", {
  fn <- app_fn("parse_template_sources")
  expect_error(fn("dir:"), regexp = "empty locator")
})

# ---- dta_template_cache_dir() ------------------------------------------------

test_that("dta_template_cache_dir() honours the override env var and is writable", {
  local_clean_template_env()
  tmp <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_CACHE_DIR = tmp)

  fn <- app_fn("dta_template_cache_dir")
  result <- fn()

  expect_true(dir.exists(result))
  probe <- file.path(result, "probe.txt")
  expect_error(writeLines("x", probe), regexp = NA)
  unlink(probe)
})

# ---- redact_secrets() --------------------------------------------------------

test_that("redact_secrets() removes the configured git token", {
  local_clean_template_env()
  withr::local_envvar(DTATOOLS_TEMPLATE_GIT_TOKEN = "supersecrettoken123")

  fn <- app_fn("redact_secrets")
  out <- fn("fatal: authentication failed for token supersecrettoken123 at https://host")

  expect_false(grepl("supersecrettoken123", out, fixed = TRUE))
  expect_true(grepl("<redacted>", out, fixed = TRUE))
})

test_that("redact_secrets() rewrites an embedded user:pass@host credential", {
  local_clean_template_env()
  fn <- app_fn("redact_secrets")
  out <- fn("remote: https://myuser:mypassword@example.com/repo.git not found")

  expect_false(grepl("mypassword", out, fixed = TRUE))
  expect_true(grepl("https://<redacted>@example.com/repo.git", out, fixed = TRUE))
})

test_that("redact_secrets() passes non-character input through unchanged", {
  fn <- app_fn("redact_secrets")
  expect_equal(fn(42L), 42L)
  expect_null(fn(NULL))
})

# ---- dta_template_git_env() --------------------------------------------------

test_that("dta_template_git_env() basic mode produces a decodable Basic header", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken", user = "myuser", mode = "basic")

  header <- vars[["GIT_CONFIG_VALUE_0"]]
  expect_match(header, "^Authorization: Basic ")
  encoded <- sub("^Authorization: Basic ", "", header)
  decoded <- rawToChar(jsonlite::base64_dec(encoded))
  expect_equal(decoded, "myuser:mytoken")
})

test_that("dta_template_git_env() bearer mode produces an Authorization: Bearer header", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken", mode = "bearer")

  expect_equal(vars[["GIT_CONFIG_VALUE_0"]], "Authorization: Bearer mytoken")
})

test_that("dta_template_git_env() always sets GIT_TERMINAL_PROMPT to 0", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken")

  expect_equal(vars[["GIT_TERMINAL_PROMPT"]], "0")

  vars_anon <- fn(token = "")
  expect_equal(vars_anon[["GIT_TERMINAL_PROMPT"]], "0")
})

test_that("dta_template_git_env() puts the token in no value except GIT_CONFIG_VALUE_0", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mysecrettoken", user = "myuser", mode = "bearer")

  other <- vars[names(vars) != "GIT_CONFIG_VALUE_0"]
  expect_false(any(vapply(other, grepl, logical(1), pattern = "mysecrettoken", fixed = TRUE)))
})

test_that("dta_template_git_env() with an empty token sets no http.extraHeader", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "")

  expect_false("http.extraHeader" %in% vars)
  expect_false("GIT_CONFIG_COUNT" %in% names(vars))
  # The (non-secret) credential-helper reset still happens -- just via a
  # `-c` command-line flag in dta_template_git_run(), not an env var, because
  # `Sys.setenv(X = "")` is indistinguishable from unset on Windows.
  expect_equal(unname(vars[["GIT_TERMINAL_PROMPT"]]), "0")
})

# ---- resolve_template_source(): dir: -----------------------------------------

test_that("resolve_template_source() succeeds for an existing dir: source", {
  local_clean_template_env()
  tmp <- withr::local_tempdir()
  parse_fn <- app_fn("parse_template_sources")
  fn <- app_fn("resolve_template_source")

  src <- parse_fn(paste0("dir:", tmp))[[1]]
  result <- fn(src)

  expect_true(result$ok)
  expect_equal(normalizePath(result$root), normalizePath(tmp))
})

test_that("resolve_template_source() fails for a non-existent dir: source", {
  local_clean_template_env()
  parse_fn <- app_fn("parse_template_sources")
  fn <- app_fn("resolve_template_source")

  missing_dir <- file.path(withr::local_tempdir(), "does-not-exist")
  src <- parse_fn(paste0("dir:", missing_dir))[[1]]
  result <- fn(src)

  expect_false(result$ok)
  expect_true(is.na(result$root))
  expect_true(grepl(missing_dir, result$error, fixed = TRUE))
})

# ---- resolve_template_source(): pkg: -----------------------------------------

test_that("resolve_template_source() resolves pkg:DTAtools", {
  local_clean_template_env()
  parse_fn <- app_fn("parse_template_sources")
  fn <- app_fn("resolve_template_source")

  src <- parse_fn("pkg:DTAtools")[[1]]
  result <- fn(src)

  # DTAtools ships its templates under extdata/templates, not
  # inst/dta-templates, so a package source resolves but reports the (real)
  # absence of that specific directory rather than crashing.
  expect_false(result$ok)
  expect_true(grepl("DTAtools", result$error, fixed = TRUE))
})

test_that("resolve_template_source() reports a missing package by name", {
  local_clean_template_env()
  parse_fn <- app_fn("parse_template_sources")
  fn <- app_fn("resolve_template_source")

  src <- parse_fn("pkg:definitelyNotARealPackageXYZ")[[1]]
  result <- fn(src)

  expect_false(result$ok)
  expect_true(grepl("definitelyNotARealPackageXYZ", result$error, fixed = TRUE))
})

# ---- git: sources, against a local bare repo (no network) -------------------

test_that("git: sources resolve, cache, and degrade to stale without network", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(
    DTATOOLS_TEMPLATE_CACHE_DIR = cache_dir,
    DTATOOLS_TEMPLATE_REFRESH_SECONDS = "3600"
  )

  bare_dir <- make_bare_template_repo()

  parse_fn <- app_fn("parse_template_sources")
  resolve_git_fn <- app_fn("resolve_git_source")

  src <- parse_fn(paste0("git:", bare_dir, "#main"))[[1]]

  # First resolve: clones fresh.
  first <- resolve_git_fn(src)
  expect_true(first$ok)
  expect_false(first$stale)
  expect_true(file.exists(file.path(first$root, "example.dta-template.yaml")))

  # Make the origin unreachable by renaming the bare repo away.
  moved_dir <- paste0(bare_dir, "-moved")
  file.rename(bare_dir, moved_dir)

  # Second resolve, still within the TTL: must NOT touch the (now-broken)
  # network at all, so it stays fresh.
  second <- resolve_git_fn(src)
  expect_true(second$ok)
  expect_false(second$stale)

  # Forced refresh with the origin unreachable: serves the cached checkout,
  # but flags it stale.
  third <- resolve_git_fn(src, refresh = TRUE)
  expect_true(third$ok)
  expect_true(third$stale)

  # A brand new cache directory with the origin still unreachable: nothing to
  # fall back to, so resolution fails outright.
  withr::local_envvar(DTATOOLS_TEMPLATE_CACHE_DIR = withr::local_tempdir())
  fourth <- resolve_git_fn(src)
  expect_false(fourth$ok)
})

# ---- dta_template_source_roots() ---------------------------------------------

test_that("dta_template_source_roots() matches the legacy list when nothing private is configured", {
  local_clean_template_env()
  legacy_fn <- app_fn("dta_creation_template_dirs")
  fn <- app_fn("dta_template_source_roots")

  result <- fn()

  expect_true(result$ok)
  expect_equal(result$roots, legacy_fn())
  expect_length(result$errors, 0)
})

test_that("dta_template_source_roots() excludes the packaged dir when a private dir: source is configured", {
  local_clean_template_env()
  tmp <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", tmp))

  fn <- app_fn("dta_template_source_roots")
  packaged_fn <- app_fn("dta_creation_templates_dir")

  result <- fn()

  expect_true(result$ok)
  expect_true(normalizePath(tmp) %in% normalizePath(result$roots))
  expect_false(normalizePath(packaged_fn()) %in% normalizePath(result$roots))
})

test_that("dta_template_source_roots() appends builtins after the private root when opted in", {
  local_clean_template_env()
  tmp <- withr::local_tempdir()
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", tmp),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "true"
  )

  fn <- app_fn("dta_template_source_roots")
  packaged_fn <- app_fn("dta_creation_templates_dir")

  result <- fn()

  private_idx <- which(normalizePath(result$roots) == normalizePath(tmp))
  packaged_idx <- which(normalizePath(result$roots) == normalizePath(packaged_fn()))

  expect_length(private_idx, 1)
  expect_length(packaged_idx, 1)
  expect_true(private_idx < packaged_idx)
})

test_that("dta_template_source_roots() fails hard when the only private source is broken and builtins are off", {
  local_clean_template_env()
  missing_dir <- file.path(withr::local_tempdir(), "does-not-exist")
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", missing_dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  )

  fn <- app_fn("dta_template_source_roots")
  result <- fn()

  expect_false(result$ok)
  expect_length(result$roots, 0)
  expect_gt(length(result$errors), 0)
})
