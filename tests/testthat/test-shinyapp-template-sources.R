# Coverage for inst/shiny/dta_app/R/template_sources.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why the
# app's helper files must be sourced into a private environment for testing).

# A local bare git repo (no network) with one committed template file, used
# by the resolve_git_source()/resolve_template_source() tests below. The repo
# is a library: the file sits in its templates/ folder. `flat = TRUE` commits
# it at the repo root instead (the retired layout). Returns the bare repo path.
#
# `before_commit`, when given, is called with the working-tree path just before
# the commit, so a test can add files of its own (a tracked symlink, a file the
# app also uses internally).
make_bare_template_repo <- function(flat = FALSE, before_commit = NULL) {
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
  template_dir <- if (flat) work_dir else file.path(work_dir, "templates")
  dir.create(template_dir, showWarnings = FALSE)
  writeLines(
    c("kind: dta_creation_template", "id: example", 'version: "1.0"'),
    file.path(template_dir, "example.dta-template.yaml")
  )
  if (is.function(before_commit)) {
    before_commit(work_dir)
  }
  run_git(c("add", "."), work_dir)
  run_git(c("commit", "--quiet", "-m", "add template"), work_dir)
  run_git(c("push", "--quiet", "origin", "HEAD:main"), work_dir)
  run_git(c("--git-dir", bare_dir, "symbolic-ref", "HEAD", "refs/heads/main"), bare_dir)

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

LOCATOR <- "https://git.example.com/org/lib.git"

test_that("dta_template_git_env() basic mode produces a decodable Basic header", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken", user = "myuser", mode = "basic", locator = LOCATOR)

  header <- vars[["GIT_CONFIG_VALUE_0"]]
  expect_match(header, "^Authorization: Basic ")
  encoded <- sub("^Authorization: Basic ", "", header)
  decoded <- rawToChar(jsonlite::base64_dec(encoded))
  expect_equal(decoded, "myuser:mytoken")
})

test_that("dta_template_git_env() bearer mode produces an Authorization: Bearer header", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken", mode = "bearer", locator = LOCATOR)

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
  vars <- fn(token = "mysecrettoken", user = "myuser", mode = "bearer", locator = LOCATOR)

  other <- vars[names(vars) != "GIT_CONFIG_VALUE_0"]
  expect_false(any(vapply(other, grepl, logical(1), pattern = "mysecrettoken", fixed = TRUE)))
})

# The header must reach the locator's own origin and nothing else. An unscoped
# `http.extraHeader` applies to every server git (or a helper such as git-lfs
# reading the inherited GIT_CONFIG_* variables) contacts, so a library
# repository could steer the credential to a host of its choosing.
test_that("dta_template_git_env() scopes the header to the locator's origin, never unscoped", {
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken", mode = "bearer", locator = LOCATOR)

  expect_identical(
    unname(vars[["GIT_CONFIG_KEY_0"]]),
    "http.https://git.example.com/.extraHeader"
  )
  expect_false("http.extraHeader" %in% unname(vars))
})

test_that("dta_template_git_env() scopes to the port and drops user info from the locator", {
  fn <- app_fn("dta_template_git_env")

  with_port <- fn(token = "t", mode = "bearer", locator = "http://127.0.0.1:8080/r.git")
  expect_identical(unname(with_port[["GIT_CONFIG_KEY_0"]]), "http.http://127.0.0.1:8080/.extraHeader")

  userinfo <- fn(token = "t", mode = "bearer", locator = "HTTPS://me:pw@Host.Example.com/a/b.git")
  expect_identical(unname(userinfo[["GIT_CONFIG_KEY_0"]]), "http.https://Host.Example.com/.extraHeader")
  expect_false(any(grepl("pw", userinfo, fixed = TRUE)))
})

test_that("dta_template_git_env() sends no header for a locator it cannot scope the token to", {
  fn <- app_fn("dta_template_git_env")
  for (locator in list(NULL, "", "/srv/repos/lib.git", "git@host:org/lib.git", "ssh://host/lib.git", "C:/repos/lib.git")) {
    vars <- fn(token = "mytoken", mode = "bearer", locator = locator)
    expect_false("GIT_CONFIG_COUNT" %in% names(vars), info = paste0("locator: ", paste(locator, collapse = "")))
    expect_false(any(grepl("mytoken", vars, fixed = TRUE)), info = paste0("locator: ", paste(locator, collapse = "")))
    expect_equal(unname(vars[["GIT_TERMINAL_PROMPT"]]), "0", info = paste0("locator: ", paste(locator, collapse = "")))
  }
})

# Ask git itself which header it would send to a URL, so the scoping is checked
# against git's URL matching rather than against this file's reading of it.
test_that("git applies the scoped header to the locator's host and to no other", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  fn <- app_fn("dta_template_git_env")
  vars <- fn(token = "mytoken", mode = "bearer", locator = LOCATOR)
  withr::local_envvar(as.list(vars[c("GIT_CONFIG_COUNT", "GIT_CONFIG_KEY_0", "GIT_CONFIG_VALUE_0")]))

  sent_to <- function(url) {
    suppressWarnings(system2(
      "git", shQuote(c("config", "--get-urlmatch", "http.extraHeader", url)),
      stdout = TRUE, stderr = FALSE
    ))
  }

  expect_identical(sent_to("https://git.example.com/org/lib.git"), "Authorization: Bearer mytoken")
  expect_length(sent_to("https://evil.example.net/org/lib.git"), 0)
  expect_length(sent_to("https://git.example.com.evil.net/org/lib.git"), 0)
  expect_length(sent_to("https://git.example.com:8443/org/lib.git"), 0)
  expect_length(sent_to("http://git.example.com/org/lib.git"), 0)
})

# Locator shapes the string tests only compare textually: git's own matcher
# must agree that the header goes to exactly that origin.
test_that("git matches the scoped key for host case, ports, IPv6 and user info", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  fn <- app_fn("dta_template_git_env")
  sent_to <- function(locator, url) {
    vars <- fn(token = "mytoken", mode = "bearer", locator = locator)
    withr::local_envvar(as.list(vars[c("GIT_CONFIG_COUNT", "GIT_CONFIG_KEY_0", "GIT_CONFIG_VALUE_0")]))
    suppressWarnings(system2(
      "git", shQuote(c("config", "--get-urlmatch", "http.extraHeader", url)),
      stdout = TRUE, stderr = FALSE
    ))
  }
  header <- "Authorization: Bearer mytoken"

  expect_identical(sent_to("https://Git.Example.com/r.git", "https://git.example.com/r.git"), header)
  expect_identical(sent_to("http://127.0.0.1:8080/r.git", "http://127.0.0.1:8080/x/r.git"), header)
  expect_length(sent_to("http://127.0.0.1:8080/r.git", "http://127.0.0.1:9090/r.git"), 0)
  expect_identical(sent_to("http://[::1]:8080/r.git", "http://[::1]:8080/r.git"), header)
  expect_identical(sent_to("https://me:pw@git.example.com/r.git", "https://git.example.com/r.git"), header)
  expect_length(sent_to("https://git.example.com?x@evil.example.net/r.git", "https://evil.example.net/r.git"), 0)
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

  # DTAtools ships its library under extdata/library, not inst/dta-templates,
  # so a package source resolves but reports the (real) absence of that
  # specific directory rather than crashing.
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
  expect_true(file.exists(file.path(first$root, "templates", "example.dta-template.yaml")))

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

# The env tests above prove the key is scoped; this proves resolve_git_source()
# hands the locator to every git call that carries the token. Without it the
# token silently stops reaching private HTTPS libraries.
test_that("resolve_git_source() passes the locator to the clone and to the fetch", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()
  cache <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_CACHE_DIR = cache, DTATOOLS_TEMPLATE_GIT_TOKEN = "mytoken")

  calls <- list()
  # The app env is shared by every test, so put the real function back after.
  real_run <- app_fn("dta_template_git_run")
  withr::defer(assign("dta_template_git_run", real_run, envir = app_env()))
  assign(
    "dta_template_git_run",
    function(args, workdir = NULL, token = NULL, locator = NULL) {
      calls[[length(calls) + 1L]] <<- list(verb = args[[1]], token = token, locator = locator)
      list(ok = FALSE, output = "mocked", status = 1L)
    },
    envir = app_env()
  )
  src <- app_fn("parse_template_sources")("git:https://git.example.com/org/lib.git#main")[[1]]
  resolve_git_fn <- app_fn("resolve_git_source")

  resolve_git_fn(src)
  expect_identical(calls[[1]]$verb, "clone")
  expect_identical(calls[[1]]$locator, "https://git.example.com/org/lib.git")
  expect_identical(calls[[1]]$token, "mytoken")

  # An existing checkout makes the next resolve a fetch.
  root <- file.path(cache, "git", rlang::hash("https://git.example.com/org/lib.git#main"))
  dir.create(file.path(root, ".git"), recursive = TRUE)
  calls <- list()
  resolve_git_fn(src, refresh = TRUE)
  expect_identical(calls[[1]]$verb, "fetch")
  expect_identical(calls[[1]]$locator, "https://git.example.com/org/lib.git")
  expect_identical(calls[[1]]$token, "mytoken")
})

test_that("git: sources still resolve with a token configured and a locator it does not apply to", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()
  withr::local_envvar(
    DTATOOLS_TEMPLATE_CACHE_DIR = withr::local_tempdir(),
    DTATOOLS_TEMPLATE_GIT_TOKEN = "mytoken"
  )
  bare_dir <- make_bare_template_repo()
  src <- app_fn("parse_template_sources")(paste0("git:", bare_dir, "#main"))[[1]]

  result <- app_fn("resolve_git_source")(src)

  expect_true(result$ok)
  expect_true(file.exists(file.path(result$root, "templates", "example.dta-template.yaml")))
})

# ---- the refresh stamp stays out of the checkout ------------------------------

# The checkout holds whatever the library repository tracks. A tracked file (or
# symlink) named like the stamp must neither be overwritten by the stamp write
# nor redirect it.
test_that("a library file named like the old stamp is left alone, and the cache still honours the TTL", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()
  withr::local_envvar(
    DTATOOLS_TEMPLATE_CACHE_DIR = withr::local_tempdir(),
    DTATOOLS_TEMPLATE_REFRESH_SECONDS = "3600"
  )
  bare_dir <- make_bare_template_repo(
    before_commit = function(work_dir) writeLines("KEEP", file.path(work_dir, ".dta-last-refresh"))
  )
  src <- app_fn("parse_template_sources")(paste0("git:", bare_dir, "#main"))[[1]]
  resolve_git_fn <- app_fn("resolve_git_source")

  first <- resolve_git_fn(src)
  expect_true(first$ok)
  expect_identical(readLines(file.path(first$root, ".dta-last-refresh")), "KEEP")

  # Origin gone: only a TTL hit can still answer as fresh, so this proves the
  # stamp was written somewhere, and read back from there.
  expect_true(file.rename(bare_dir, paste0(bare_dir, "-moved")))
  second <- resolve_git_fn(src)
  expect_true(second$ok)
  expect_false(second$stale)

  third <- resolve_git_fn(src, refresh = TRUE)
  expect_true(third$stale)
  expect_identical(readLines(file.path(third$root, ".dta-last-refresh")), "KEEP")
})

test_that("a tracked symlink named like the old stamp cannot redirect the stamp write", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()
  withr::local_envvar(DTATOOLS_TEMPLATE_CACHE_DIR = withr::local_tempdir())

  victim <- file.path(withr::local_tempdir(), "victim.txt")
  writeLines("KEEP", victim)
  probe <- file.path(withr::local_tempdir(), "probe-link")
  skip_if(!isTRUE(suppressWarnings(file.symlink(victim, probe))), "cannot create symlinks here")
  unlink(probe)

  bare_dir <- make_bare_template_repo(
    before_commit = function(work_dir) file.symlink(victim, file.path(work_dir, ".dta-last-refresh"))
  )
  src <- app_fn("parse_template_sources")(paste0("git:", bare_dir, "#main"))[[1]]
  resolve_git_fn <- app_fn("resolve_git_source")

  first <- resolve_git_fn(src)
  expect_true(first$ok)
  # Guard against a vacuous pass: git on this host must really have checked the
  # link out, or the assertion below proves nothing.
  skip_if(!nzchar(Sys.readlink(file.path(first$root, ".dta-last-refresh"))), "git did not check out a symlink")
  expect_identical(readLines(victim), "KEEP")

  expect_true(resolve_git_fn(src, refresh = TRUE)$ok)
  expect_identical(readLines(victim), "KEEP")
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


# ---- Library layout: the source root is the library root --------------------

# A library root is used as configured: there is no templates/ guessing, no
# "templates/ wins over the root" rule. The kind folders live inside it.

test_that("the library layout table is the one the sources resolve against", {
  # templates/ holds creation AND dataset templates; parties/ and vocabulary/
  # one kind each. Pinned literally so a renamed folder fails here.
  subdir <- app_fn("dta_template_kind_subdir")
  expect_identical(
    vapply(app_fn("dta_template_all_kinds")(), subdir, character(1), USE.NAMES = FALSE),
    c("templates", "templates", "parties", "vocabulary")
  )
  expect_identical(app_fn("dta_template_kind_subdirs")(), c("templates", "parties", "vocabulary"))
  expect_identical(app_fn("dta_export_templates_subdir")(), "export-templates")
  # The retired helpers are gone, not merely unused.
  expect_error(app_fn("dta_template_library_dir"), "not defined")
  expect_error(app_fn("dta_template_library_subdir"), "not defined")
})

test_that("dir sources use the configured directory as the library root", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  # Even with a templates/ folder inside, the root stays the directory itself.
  lib <- file.path(root, "templates")
  dir.create(lib)
  writeLines("kind: dta_creation_template", file.path(lib, "x.dta-template.yaml"))
  parse <- app_fn("parse_template_sources")
  resolve <- app_fn("resolve_template_source")

  result <- resolve(parse(paste0("dir:", root))[[1]])

  expect_true(result$ok)
  expect_identical(result$checkout, normalizePath(root, winslash = "/"))
  expect_identical(result$root, result$checkout)

  # Pointing at the kind folder itself is NOT corrected to its parent.
  direct <- resolve(parse(paste0("dir:", lib))[[1]])
  expect_true(direct$ok)
  expect_identical(direct$root, normalizePath(lib, winslash = "/"))
  expect_identical(direct$checkout, direct$root)
  expect_false(identical(direct$root, result$root))
})

test_that("git sources use the checkout as the library root, for commit lookups and refresh alike", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()
  withr::local_envvar(DTATOOLS_TEMPLATE_CACHE_DIR = withr::local_tempdir())
  bare <- make_bare_template_repo()
  src <- app_fn("parse_template_sources")(paste0("git:", bare, "#main"))[[1]]
  resolve <- app_fn("resolve_template_source")
  first <- resolve(src)
  expect_true(first$ok)
  expect_false(first$stale)
  expect_identical(first$root, first$checkout)
  expect_true(dir.exists(file.path(first$checkout, ".git")))
  expect_true(file.exists(file.path(first$root, "templates", "example.dta-template.yaml")))
  expect_match(first$resolved_commit, "^[0-9a-f]{40}$")
  expect_identical(first$resolved_commit, app_fn(".dta_template_git_commit")(first$checkout))
  refreshed <- resolve(src, refresh = TRUE)
  expect_true(refreshed$ok)
  expect_identical(refreshed$root, first$root)
  expect_identical(refreshed$resolved_commit, first$resolved_commit)
  moved <- paste0(bare, "-moved")
  expect_true(file.rename(bare, moved))
  withr::defer(unlink(moved, recursive = TRUE))
  stale <- resolve(src, refresh = TRUE)
  expect_true(stale$ok)
  expect_true(stale$stale)
  expect_identical(stale$root, first$root)
  expect_identical(stale$resolved_commit, first$resolved_commit)
})

test_that("a git repository with the retired flat layout resolves but indexes nothing", {
  skip_if(!nzchar(Sys.which("git")), "git not on PATH")
  local_clean_template_env()
  withr::local_envvar(DTATOOLS_TEMPLATE_CACHE_DIR = withr::local_tempdir())
  bare <- make_bare_template_repo(flat = TRUE)
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("git:", bare, "#main"))
  app_fn("dta_template_index_invalidate")()

  expect_warning(index <- app_fn("build_template_index")(), "belongs in templates/")

  expect_identical(nrow(index), 0L)
  expect_true(any(grepl("example.dta-template.yaml", attr(index, "warnings"), fixed = TRUE)))
})

test_that("source roots and the index load only the kind folders of the root", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  lib <- file.path(root, "templates")
  dir.create(lib)
  writeLines(
    c("kind: dta_creation_template", "id: live", 'version: "1.0"'),
    file.path(lib, "live.dta-template.yaml")
  )
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("test=dir:", root))
  roots <- app_fn("dta_template_source_roots")()
  expect_identical(roots$roots, normalizePath(root, winslash = "/"))
  index <- app_fn("build_template_index")()
  expect_identical(index$id, "live")
  expect_identical(basename(dirname(index$path)), "templates")

  # A template lying beside the kind folders is not indexed; it is reported.
  writeLines(
    c("kind: dta_creation_template", "id: invisible", 'version: "1.0"'),
    file.path(root, "invisible.dta-template.yaml")
  )
  expect_warning(index <- app_fn("build_template_index")(), "belongs in templates/")
  expect_identical(index$id, "live")
  expect_true(any(grepl("invisible.dta-template.yaml", attr(index, "warnings"), fixed = TRUE)))
})

test_that("template_dir roots are kept as given, with no templates/ autodetection", {
  local_clean_template_env()
  root <- withr::local_tempdir()
  lib <- file.path(root, "templates")
  dir.create(lib)
  writeLines("kind: dta_creation_template", file.path(lib, "x.dta-template.yaml"))
  withr::local_options(list(DTAtools.template_dir = c(root, lib)))

  dirs <- app_fn("dta_creation_template_dirs")()

  expect_identical(dirs[1:2], c(root, lib))
  expect_true(root %in% dirs)
  expect_true(lib %in% dirs)
})

test_that("parse_template_sources() rejects a git locator or ref that git would read as an option", {
  fn <- app_fn("parse_template_sources")
  expect_error(
    fn("git:https://host/repo.git#--upload-pack=touch /tmp/pwned"),
    "read as an option"
  )
  expect_error(fn("git:--upload-pack=x"), "read as an option")
  # A hyphen anywhere else is ordinary.
  expect_identical(fn("git:https://host/my-repo.git#release-1")[[1]]$ref, "release-1")
})
