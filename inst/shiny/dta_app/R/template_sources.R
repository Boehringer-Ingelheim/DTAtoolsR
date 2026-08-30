# -----------------------------------------------------------------------------
# Private/remote template sources for "Create new from template".
#
# template_core.R's dta_creation_template_dirs() only ever looks at local
# directories (an option, a project-relative folder, the packaged demo). That
# is enough for a laptop, but a Posit Connect deployment wants its template
# family to come from somewhere it does not have to redeploy the app to
# change: a private git repo, or a companion R package. This file adds that
# layer ahead of the legacy one, entirely driven by environment variables so
# it is configurable per-deployment without a code change:
#
#   DTATOOLS_TEMPLATE_SOURCES           "[name=]scheme:locator[#ref]; ..."
#   DTATOOLS_TEMPLATE_INCLUDE_BUILTIN   keep the packaged/legacy dirs too
#   DTATOOLS_TEMPLATE_REFRESH_SECONDS   git source TTL before re-fetching
#   DTATOOLS_TEMPLATE_CACHE_DIR         override for the git clone cache
#   DTATOOLS_TEMPLATE_GIT_TOKEN         credential for a private git source
#   DTATOOLS_TEMPLATE_GIT_USER          basic-auth username (default "git")
#   DTATOOLS_TEMPLATE_GIT_AUTH          "basic" (default) or "bearer"
#
# Design goal: once ANY private source is configured, it REPLACES the
# packaged demo rather than merely adding to it (see
# dta_template_source_roots() below) -- an internal deployment that ships a
# real template family must not let a failed private source silently fall
# back to the "biomarker_gf" example, or someone will author a real
# specification from a demo file by mistake.
#
# `%||%` is already bound in the app helper environment (see template_core.R
# / 00_helpers.R) -- it is not redefined here.
# -----------------------------------------------------------------------------

# ---- Config readers ---------------------------------------------------------

# Sys.getenv() wrapper used everywhere below: trims incidental whitespace (an
# env var set via a shell script or a Connect "Vars" UI often picks up a
# trailing newline or space) and normalises "unset" and "set to empty string"
# to the same `default`.
dta_template_env <- function(name, default = "") {
  val <- trimws(Sys.getenv(name, unset = ""))
  if (nzchar(val)) val else default
}

# The raw source spec, trimmed. Kept as its own function (rather than inlined
# everywhere) because "is a private source configured" is a question asked in
# several places below and must always agree on the answer.
dta_template_sources_spec <- function() {
  dta_template_env("DTATOOLS_TEMPLATE_SOURCES", "")
}

dta_template_private_configured <- function() {
  nzchar(dta_template_sources_spec())
}

# Whether the legacy/packaged directories are kept alongside a configured
# private source. Meaningless when there is no private source -- with nothing
# else configured, the builtins are the only templates there are, so this
# always reads as TRUE in that case regardless of the env var.
dta_template_include_builtin <- function() {
  if (!dta_template_private_configured()) {
    return(TRUE)
  }
  val <- tolower(dta_template_env("DTATOOLS_TEMPLATE_INCLUDE_BUILTIN", ""))
  val %in% c("1", "true", "yes")
}

# TTL, in seconds, before a cached git source is re-fetched. A bad value
# (unparseable, zero, negative) must not silently disable caching or hang the
# refresh logic -- it falls back to the documented default with a warning
# rather than aborting, since a template lookup should not go down because of
# a typo in an unrelated tuning knob.
dta_template_refresh_seconds <- function() {
  raw <- dta_template_env("DTATOOLS_TEMPLATE_REFRESH_SECONDS", "900")
  val <- suppressWarnings(as.numeric(raw))
  if (is.na(val) || val <= 0) {
    cli::cli_warn(
      "{.envvar DTATOOLS_TEMPLATE_REFRESH_SECONDS} value {.val {raw}} is not a positive number; using 900 seconds."
    )
    return(900)
  }
  val
}

# ---- Grammar -----------------------------------------------------------------

# Parse the `DTATOOLS_TEMPLATE_SOURCES` grammar:
#
#   [name=]scheme:locator[#ref]
#
# with entries separated by `;`. This is a flat string rather than a YAML/JSON
# blob on purpose: it lives in a single environment variable set by ops
# tooling (a Connect "Vars" field, a Helm chart, a systemd unit file), where a
# multi-line or quoted value is an easy way to get silently mangled in transit.
#
# scheme is always the first token before the first `:` -- this matters for
# `git:`, whose locator is itself a URL containing `://`: `git:https://
# host/x.git` must yield scheme "git" and locator "https://host/x.git", not
# split again on the colon inside "https://".
parse_template_sources <- function(spec) {
  spec <- trimws(spec %||% "")
  if (!nzchar(spec)) {
    return(list())
  }

  entries <- strsplit(spec, ";", fixed = TRUE)[[1]]
  out <- list()

  for (entry in entries) {
    entry <- trimws(entry)
    if (!nzchar(entry)) {
      next
    }

    # An optional `name=` prefix precedes the scheme. Only an `=` that occurs
    # BEFORE the first `:` counts as that prefix -- a locator (a URL query
    # string, in principle) may contain `=` of its own after that point.
    eq_pos <- regexpr("=", entry, fixed = TRUE)
    colon_pos <- regexpr(":", entry, fixed = TRUE)
    name <- NA_character_
    rest <- entry
    if (eq_pos > 0 && (colon_pos < 0 || eq_pos < colon_pos)) {
      name <- trimws(substr(entry, 1, eq_pos - 1))
      rest <- substr(entry, eq_pos + 1, nchar(entry))
    }

    scheme_colon <- regexpr(":", rest, fixed = TRUE)
    if (scheme_colon < 0) {
      cli::cli_abort(
        "Template source {.val {entry}} is missing the required {.code scheme:locator} separator."
      )
    }
    scheme <- substr(rest, 1, scheme_colon - 1)
    remainder <- substr(rest, scheme_colon + 1, nchar(rest))

    if (!scheme %in% c("dir", "pkg", "git")) {
      cli::cli_abort(
        "Template source {.val {entry}} has unknown scheme {.val {scheme}}; expected one of {.val {c('dir', 'pkg', 'git')}}."
      )
    }

    # `#ref` splits on the LAST `#` in the remainder, so a locator that (in
    # principle) contains an earlier `#` of its own is not mistaken for the
    # ref separator.
    hash_positions <- gregexpr("#", remainder, fixed = TRUE)[[1]]
    has_hash <- hash_positions[1] != -1
    if (has_hash) {
      last_hash <- hash_positions[length(hash_positions)]
      locator <- substr(remainder, 1, last_hash - 1)
      ref <- substr(remainder, last_hash + 1, nchar(remainder))
    } else {
      locator <- remainder
      ref <- NA_character_
    }

    if (!nzchar(locator)) {
      cli::cli_abort(
        "Template source {.val {entry}} has an empty locator."
      )
    }

    if (scheme == "git") {
      ref <- if (has_hash && nzchar(ref)) ref else "HEAD"
    } else {
      if (has_hash) {
        cli::cli_warn(
          "Ignoring {.code #ref} on {.val {scheme}:} template source {.val {entry}} -- only {.code git:} sources are versioned."
        )
      }
      ref <- NA_character_
    }

    if (is.na(name) || !nzchar(name)) {
      bn <- sub("\\.git$", "", basename(locator))
      name <- if (nzchar(bn)) bn else scheme
    }

    out[[length(out) + 1]] <- list(
      name = name, scheme = scheme, locator = locator, ref = ref
    )
  }

  out
}

# ---- Cache -------------------------------------------------------------------

# Directory that reads and removes a probe file, proving `dir` is genuinely
# writable. file.access() reports POSIX permission bits, which say nothing
# about Windows ACLs or a Posit Connect content sandbox -- both can leave a
# directory looking writable to file.access() while every actual write fails.
.dta_template_dir_writable <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) {
    return(FALSE)
  }
  probe <- file.path(dir, paste0(".dta-write-probe-", Sys.getpid()))
  ok <- isTRUE(tryCatch(
    {
      writeLines("probe", probe)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  ))
  if (file.exists(probe)) {
    unlink(probe)
  }
  ok
}

# Where git clones (and, indirectly, resolved source metadata) are cached.
#
# 1. DTATOOLS_TEMPLATE_CACHE_DIR if set, else tools::R_user_dir() -- the
#    standard per-user cache location.
# 2. PROBE writability (see .dta_template_dir_writable()); a Posit Connect
#    content account frequently cannot write R_user_dir(), and file.access()
#    would say otherwise.
# 3. Fall back to a directory under tempdir(), which the R process that is
#    running this code is, by construction, able to write.
dta_template_cache_dir <- function() {
  base <- dta_template_env("DTATOOLS_TEMPLATE_CACHE_DIR", "")
  if (!nzchar(base)) {
    base <- tools::R_user_dir("DTAtools", "cache")
  }

  if (.dta_template_dir_writable(base)) {
    return(normalizePath(base, winslash = "/", mustWork = TRUE))
  }

  fallback <- file.path(tempdir(), "DTAtools-templates")
  if (!.dta_template_dir_writable(fallback)) {
    cli::cli_abort(
      "No writable directory available for the template cache (tried {.path {base}} and {.path {fallback}})."
    )
  }
  normalizePath(fallback, winslash = "/", mustWork = TRUE)
}

# ---- Redaction -----------------------------------------------------------

# Strip credentials from `x` before it is returned, logged, or shown anywhere
# (a Shiny error banner, an app log file). Vectorised over character; anything
# else passes through unchanged so this can be applied defensively without a
# type check at every call site.
redact_secrets <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  token <- dta_template_env("DTATOOLS_TEMPLATE_GIT_TOKEN", "")
  if (nzchar(token)) {
    x <- gsub(token, "<redacted>", x, fixed = TRUE)
  }

  # scheme://user:secret@host -> scheme://<redacted>@host, in case a URL with
  # embedded credentials ever makes it into a git error message (e.g. a
  # misconfigured locator) rather than through the header-based auth path
  # below, which never puts the token in a URL at all.
  gsub(
    "([A-Za-z][A-Za-z0-9+.-]*://)[^/@\\s]+:[^/@\\s]+@",
    "\\1<redacted>@",
    x,
    perl = TRUE
  )
}

# ---- Git ---------------------------------------------------------------------

dta_template_git_available <- function() {
  nzchar(Sys.which("git"))
}

# Environment variables for a git child process -- this is the security core
# of the private-source feature.
#
# WHY env vars and not the clone URL or the command line: a token embedded in
# `https://user:TOKEN@host/repo.git` persists into the checked-out
# `.git/config` forever (anyone who later reads that file, or `git remote -v`,
# sees it in plaintext); a token passed as a command-line argument is visible
# to every other user on the machine via the process table (`ps aux` / Task
# Manager). The environment block of a child process, by contrast, is legible
# only to the process's own owner. So the token goes into an HTTP header,
# delivered via `http.extraHeader`, itself passed through GIT_CONFIG_KEY/
# VALUE_n env vars rather than a `-c` command-line flag for the identical
# reason.
#
# Neutralising the credential helper (so a stray system-wide helper cannot
# pop a GUI prompt or inject a credential behind our back) carries no secret,
# so it is NOT done through this env-var mechanism: `Sys.setenv(X = "")` is
# indistinguishable from unset on Windows (SetEnvironmentVariable() with an
# empty value deletes the variable at the OS level, not an R quirk), which
# would leave git with a GIT_CONFIG_KEY_n and no matching VALUE_n and abort
# with "missing config value". dta_template_git_run() instead passes
# `-c credential.helper=` on the command line, unconditionally, for every
# invocation -- safe there precisely because it holds no secret.
dta_template_git_env <- function(token,
                                 user = dta_template_env("DTATOOLS_TEMPLATE_GIT_USER", "git"),
                                 mode = dta_template_env("DTATOOLS_TEMPLATE_GIT_AUTH", "basic")) {
  hardening <- c(
    # A bad or expired token must fail the clone/fetch immediately, not block
    # the Shiny process on an interactive credential prompt it can never
    # answer.
    GIT_TERMINAL_PROMPT = "0",
    GIT_CONFIG_NOSYSTEM = "1",
    GIT_ASKPASS = ""
  )

  if (is.null(token) || !nzchar(token)) {
    # No credential to send: an anonymous clone of a public repo must still
    # work, so only the hardening vars apply.
    return(hardening)
  }

  header_value <- if (identical(tolower(mode), "bearer")) {
    paste0("Authorization: Bearer ", token)
  } else {
    encoded <- jsonlite::base64_enc(charToRaw(paste0(user, ":", token)))
    encoded <- gsub("[\r\n]", "", encoded)
    paste0("Authorization: Basic ", encoded)
  }

  c(
    GIT_CONFIG_COUNT = "1",
    GIT_CONFIG_KEY_0 = "http.extraHeader",
    GIT_CONFIG_VALUE_0 = header_value,
    hardening
  )
}

# Set `vars` in the CURRENT process's environment for the duration of `expr`,
# then restore exactly what was there before -- including Sys.unsetenv() for
# a variable that had no prior value, so a git call never leaks its auth
# header into the rest of the running R/Shiny session.
#
# This exists because system2()'s `env=` argument is documented as being
# honoured "only... on Unix-alikes" -- it is a no-op on Windows. Setting and
# restoring the parent process's own environment is the one mechanism that
# works identically on every platform this app runs on.
# Run `expr` with `vars` set in the environment, restoring the prior state --
# including unsetting what was previously unset -- on the way out.
#
# system2()'s own `env=` argument would be the obvious tool and is NOT supported
# on Windows, which is why this exists at all.
#
# LIMITATION, deliberate: Sys.setenv() mutates the environment of the WHOLE R
# process, and a Shiny app on Posit Connect serves many sessions from one
# process. Two sessions refreshing a git source at the same instant can
# therefore stomp each other's variables and fail one of the two fetches. That
# is a failed refresh which the caller reports and the cache absorbs, never a
# leak or a wrong result -- and the refresh TTL makes a genuine collision rare.
# A process-wide lock would remove even that, and is not worth the deadlock
# surface here.
.dta_template_with_env <- function(vars, expr) {
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(vars))
  on.exit(
    {
      to_unset <- names(old)[is.na(old)]
      if (length(to_unset) > 0) {
        Sys.unsetenv(to_unset)
      }
      to_restore <- old[!is.na(old)]
      if (length(to_restore) > 0) {
        do.call(Sys.setenv, as.list(to_restore))
      }
    },
    add = TRUE
  )
  expr
}

# Run one git command with the auth/hardening environment from
# dta_template_git_env(), optionally inside `workdir`, and return a
# redacted, structured result rather than a raw system2() capture.
dta_template_git_run <- function(args, workdir = NULL, token = NULL) {
  env_vars <- dta_template_git_env(token)
  # `-c credential.helper=` and `-c core.askPass=` unconditionally: neither
  # carries a secret, so the command line is fine (see the comment on
  # dta_template_git_env() for why these cannot be env vars on Windows), and
  # both must apply whether or not a token was supplied so nothing gets a
  # chance to prompt.
  #
  # core.askPass matters on its own: GIT_CONFIG_NOSYSTEM suppresses only the
  # SYSTEM gitconfig, not the user's global ~/.gitconfig, and an askPass helper
  # configured there -- VS Code installs one -- would pop a GUI dialog that a
  # headless Shiny process can never answer, hanging the fetch until it is
  # killed rather than failing it.
  full_args <- c("-c", "credential.helper=", "-c", "core.askPass=", args)

  if (!is.null(workdir)) {
    old_wd <- getwd()
    setwd(workdir)
    on.exit(setwd(old_wd), add = TRUE)
  }

  # system2() on Windows only shQuote()s the command itself, never the
  # individual `args` -- an argument containing an embedded space (a commit
  # message, or a path under a Windows user profile whose name has a space
  # in it, e.g. "C:/Users/John Doe/...") would otherwise silently split into
  # two command-line tokens for the child process.
  out <- .dta_template_with_env(
    env_vars,
    suppressWarnings(system2("git", shQuote(full_args), stdout = TRUE, stderr = TRUE))
  )

  status <- attr(out, "status")
  if (is.null(status)) {
    status <- 0L
  }

  list(
    ok = identical(as.integer(status), 0L),
    output = redact_secrets(out),
    status = as.integer(status)
  )
}

# Read the current HEAD commit of the repo at `root` (first 40 characters of
# `git rev-parse HEAD`), or NA_character_ when that cannot be determined.
.dta_template_git_commit <- function(root) {
  res <- dta_template_git_run(c("rev-parse", "HEAD"), workdir = root)
  if (!isTRUE(res$ok) || length(res$output) == 0) {
    return(NA_character_)
  }
  substr(trimws(res$output[[1]]), 1, 40)
}

# Resolve a single `git:` source to a local checkout, using a shallow clone
# cached under dta_template_cache_dir(), refreshed at most once per
# dta_template_refresh_seconds().
#
# The cache key hashes locator + ref rather than `name`, since `name` is only
# a display label and two entries could legitimately share one (or a locator
# could be re-pointed at a different name across a config change without the
# stale checkout leaking through under a similarly named but different
# repository).
resolve_git_source <- function(src, refresh = FALSE) {
  root <- file.path(
    dta_template_cache_dir(), "git",
    rlang::hash(paste0(src$locator, "#", src$ref))
  )
  stamp_file <- file.path(root, ".dta-last-refresh")
  present <- dir.exists(file.path(root, ".git"))

  stamp_age <- function() {
    if (!file.exists(stamp_file)) {
      return(NA_real_)
    }
    stamp <- suppressWarnings(as.numeric(readLines(stamp_file, warn = FALSE, n = 1)))
    if (length(stamp) == 0 || is.na(stamp)) {
      return(NA_real_)
    }
    as.numeric(Sys.time()) - stamp
  }

  fresh_result <- function() {
    list(
      root = normalizePath(root, winslash = "/", mustWork = TRUE),
      ok = TRUE, stale = FALSE, stale_age = NA_real_,
      resolved_commit = .dta_template_git_commit(root),
      error = NA_character_
    )
  }

  # Cache hit within the TTL: return without touching the network at all.
  if (present && !refresh) {
    age <- stamp_age()
    if (!is.na(age) && age < dta_template_refresh_seconds()) {
      return(fresh_result())
    }
  }

  token <- dta_template_env("DTATOOLS_TEMPLATE_GIT_TOKEN", "")

  if (!dta_template_git_available()) {
    run_result <- list(ok = FALSE, output = "git is not available on PATH.", status = NA_integer_)
  } else if (!present) {
    dir.create(dirname(root), recursive = TRUE, showWarnings = FALSE)
    args <- c("clone", "--depth", "1", "--no-tags")
    if (!identical(src$ref, "HEAD")) {
      args <- c(args, "--branch", src$ref)
    }
    args <- c(args, src$locator, root)
    run_result <- dta_template_git_run(args, token = token)
  } else {
    run_result <- dta_template_git_run(
      c("fetch", "--depth", "1", "--no-tags", "origin", src$ref),
      workdir = root, token = token
    )
    if (isTRUE(run_result$ok)) {
      run_result <- dta_template_git_run(c("reset", "--hard", "FETCH_HEAD"), workdir = root)
    }
  }

  if (isTRUE(run_result$ok)) {
    writeLines(as.character(as.numeric(Sys.time())), stamp_file)
    return(fresh_result())
  }

  err <- paste(run_result$output, collapse = "\n")

  if (present) {
    # A refresh attempt failed but a previous checkout still exists: serve it
    # stale rather than breaking template creation over a transient network
    # or credential problem.
    return(list(
      root = normalizePath(root, winslash = "/", mustWork = TRUE),
      ok = TRUE, stale = TRUE, stale_age = stamp_age(),
      resolved_commit = .dta_template_git_commit(root),
      error = err
    ))
  }

  list(
    root = "", ok = FALSE, stale = NA, stale_age = NA_real_,
    resolved_commit = NA_character_, error = err
  )
}

# ---- Resolution ----------------------------------------------------------

# Resolve one parsed source (as returned by parse_template_sources()) to a
# local directory. Always returns every field, with NA_character_/NA_real_
# for anything not applicable -- never NULL, so callers can bind rows of
# these into a data frame without special-casing missing fields.
resolve_template_source <- function(src, refresh = FALSE) {
  base <- list(
    name = src$name, scheme = src$scheme, origin = src$locator, ref = src$ref,
    root = NA_character_, ok = FALSE, stale = FALSE, stale_age = NA_real_,
    resolved_commit = NA_character_, error = NA_character_
  )

  if (src$scheme == "dir") {
    if (dir.exists(src$locator)) {
      base$root <- normalizePath(src$locator, winslash = "/", mustWork = TRUE)
      base$ok <- TRUE
    } else {
      base$error <- paste0("Template directory does not exist: ", src$locator)
    }
    return(base)
  }

  if (src$scheme == "pkg") {
    if (!requireNamespace(src$locator, quietly = TRUE)) {
      base$error <- paste0("Package not installed: ", src$locator)
      return(base)
    }
    dir <- system.file("dta-templates", package = src$locator)
    if (!nzchar(dir)) {
      base$error <- paste0("Package ", src$locator, " has no dta-templates directory.")
      return(base)
    }
    base$root <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    base$ok <- TRUE
    return(base)
  }

  # scheme == "git"
  git_result <- resolve_git_source(src, refresh = refresh)
  base$root <- if (nzchar(git_result$root %||% "")) git_result$root else NA_character_
  base$ok <- isTRUE(git_result$ok)
  base$stale <- isTRUE(git_result$stale)
  base$stale_age <- git_result$stale_age %||% NA_real_
  base$resolved_commit <- git_result$resolved_commit %||% NA_character_
  base$error <- redact_secrets(git_result$error) %||% NA_character_
  base
}

# Every root directory the "Create new from template" picker should search,
# in precedence order, plus enough metadata to explain a failure to an admin.
#
# WHY a configured-but-entirely-failed private source is a hard failure
# (`ok = FALSE`) rather than a silent fallback to the packaged directory:
# private-replaces-public means the packaged "biomarker_gf" demo is not a
# safe substitute for a real internal template family. A silent fallback
# would let someone author a genuine specification from an example template
# without ever finding out that the real source was unreachable.
dta_template_source_roots <- function(refresh = FALSE) {
  sources <- parse_template_sources(dta_template_sources_spec())
  resolved <- lapply(sources, resolve_template_source, refresh = refresh)

  roots <- character(0)
  errors <- character(0)
  for (r in resolved) {
    if (isTRUE(r$ok) && !is.na(r$root) && nzchar(r$root)) {
      roots <- c(roots, r$root)
    }
    if (!isTRUE(r$ok) && !is.na(r$error) && nzchar(r$error)) {
      errors <- c(errors, r$error)
    }
  }
  roots <- unique(roots)

  private_configured <- length(sources) > 0
  any_private_ok <- any(vapply(resolved, function(r) isTRUE(r$ok), logical(1)))

  if (dta_template_include_builtin()) {
    roots <- unique(c(roots, dta_creation_template_dirs()))
  }

  ok <- if (private_configured) any_private_ok else TRUE

  list(roots = roots, sources = resolved, ok = ok, errors = errors)
}
