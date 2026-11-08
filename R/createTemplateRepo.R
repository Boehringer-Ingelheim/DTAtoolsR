# -----------------------------------------------------------------------------
# create_template_repo(): scaffold a new PRIVATE DTAtools template repository.
#
# This is the write side of the feature validateTemplate.R is the read side
# of: that file lints an existing directory of `*.dta-*.yaml` files; this one
# creates a starting directory that already passes every one of those checks,
# so "start a private template repository" is one function call rather than a
# newcomer reverse-engineering the four file kinds from the vignette alone.
#
# The worked example it copies from -- inst/extdata/template-repo-skeleton/ --
# mirrors the output layout: README.md and dta-app.yaml at the root, then one
# folder per file kind (templates/, parties/, vocabulary/, export-templates/).
# It is deliberately its own directory, not a subset of inst/extdata/library/
# (the packaged biomarker_gf family used in examples and tests elsewhere).
# Those two serve different audiences: inst/extdata/library/ demonstrates
# every feature the template engine has, at the cost of being a fairly
# involved read; the skeleton is meant to be copied verbatim into a fresh
# repository and understood in one sitting, so it stays deliberately small.
# -----------------------------------------------------------------------------

# Locate the bundled skeleton, failing loudly rather than copying nothing.
#
# system.file() returns "" (not an error) when it cannot find the requested
# path, and an empty "" silently degrades to the current working directory --
# list.files("") would then either error confusingly or, worse, quietly list
# whatever unrelated files the caller's R session happens to be sitting in.
# A development checkout missing `inst/`, or a broken installation, are the
# realistic causes; both deserve a clear message naming the missing path
# rather than a repository that LOOKS created but has nothing useful in it.
.dta_create_repo_skeleton_dir <- function() {
  dir <- system.file("extdata", "template-repo-skeleton", package = "DTAtools")
  if (!nzchar(dir) || !dir.exists(dir) || length(list.files(dir)) == 0) {
    cli::cli_abort(c(
      "Could not locate the bundled template-repository skeleton
        ({.file inst/extdata/template-repo-skeleton}).",
      "i" = "This usually means a development checkout is missing {.file inst/},
        or DTAtools was installed from a broken build."
    ))
  }
  dir
}

# Locate a bundled CI asset (the GitHub workflow, the Bitbucket Pipelines
# definition, or the Jenkinsfile). Same failure mode and same fix as above --
# kept as a separate lookup (rather than treating these as just more files
# under the skeleton directory) because all three are copied from OUTSIDE
# template-repo-skeleton/, and validate_template()'s own roxygen block names
# the GitHub workflow's exact path as the ready-to-copy one, so both
# functions agree on where it lives. Generalised across the three providers,
# rather than duplicated once per provider, because the failure mode
# (system.file() silently returning "") is identical for all three.
.dta_create_repo_ci_asset_file <- function(asset_name) {
  file <- system.file("extdata", "ci", asset_name, package = "DTAtools")
  if (!nzchar(file) || !file.exists(file)) {
    cli::cli_abort(c(
      "Could not locate the bundled CI asset ({.file inst/extdata/ci/{asset_name}}).",
      "i" = "This usually means a development checkout is missing {.file inst/},
        or DTAtools was installed from a broken build."
    ))
  }
  file
}

# The agent skill (instructions a coding agent loads when a task touches the
# library) is written beneath `.claude/skills/` of the new repository. It is
# shipped outside template-repo-skeleton/ for the reason `.gitignore` is
# generated below: a dot-prefixed directory does not survive `R CMD build`
# from `inst/`. Its files are discovered from disk, like the example files, so
# a new reference file in the skill needs no change here.
.dta_create_repo_skill_name <- "dta-template-authoring"

.dta_create_repo_skill_dir <- function() {
  dir <- system.file("extdata", "agent-skills", .dta_create_repo_skill_name, package = "DTAtools")
  if (!nzchar(dir) || !file.exists(file.path(dir, "SKILL.md"))) {
    bundled <- file.path("inst", "extdata", "agent-skills", .dta_create_repo_skill_name)
    cli::cli_abort(c(
      "Could not locate the bundled agent skill ({.file {bundled}}).",
      "i" = "This usually means a development checkout is missing {.file inst/},
        or DTAtools was installed from a broken build."
    ))
  }
  dir
}

# `.gitignore` is generated here, not shipped as a skeleton file, because a
# dotfile (and, for the same reason, a dot-prefixed directory like `.github/`)
# is not reliably carried through `R CMD build` from `inst/` -- the packaged
# skeleton deliberately holds no hidden paths, and this function writes them
# itself instead.
.dta_create_repo_gitignore_lines <- function() {
  c(".Rproj.user", ".Rhistory", "*.Rproj")
}

# The example files shipped in the skeleton -- every file under it, relative
# to the skeleton, except the root README.md (which .dta_create_repo_targets()
# adds on its own, whatever `examples` says). Discovered from the directory's
# actual contents rather than hard-coded, so a future change to the skeleton
# (renaming a starter file, adding a folder) cannot silently drift out of sync
# with what this function copies. A folder's own README.md (such as
# export-templates/README.md) is an example file like any other.
.dta_create_repo_example_files <- function(skeleton_dir) {
  files <- list.files(skeleton_dir, recursive = TRUE, full.names = FALSE, all.files = FALSE)
  files[files != "README.md"]
}

# Where each CI provider's bundled asset is written inside the created
# repository, and which file under inst/extdata/ci/ it is copied from.
# A named list keyed by provider name rather than three near-identical `if`
# blocks, so a fourth provider (Bamboo, GitLab CI, ...) is one new list entry
# rather than a new branch that has to stay in sync with the provider names
# .dta_create_repo_validate_ci() knows about.
.dta_create_repo_ci_asset_targets <- list(
  github = c(dest = file.path(".github", "workflows", "validate-templates.yml"), asset = "validate-templates.yml"),
  bitbucket = c(dest = "bitbucket-pipelines.yml", asset = "bitbucket-pipelines.yml"),
  jenkins = c(dest = "Jenkinsfile", asset = "Jenkinsfile")
)

# Validate `ci` and normalise it to a de-duplicated character vector of
# provider names (possibly empty) in the canonical order above, regardless of
# the order or duplication the caller supplied. `TRUE` maps to `"github"` --
# the historical, still-default behaviour -- and `FALSE` to no providers at
# all, so existing callers passing a bare logical see no change.
.dta_create_repo_validate_ci <- function(ci) {
  known <- names(.dta_create_repo_ci_asset_targets)

  if (is.logical(ci)) {
    if (length(ci) != 1 || is.na(ci)) {
      cli::cli_abort("{.arg ci} must be a single {.code TRUE}/{.code FALSE}, or a
        character vector naming one or more of {.val {known}}.")
    }
    return(if (isTRUE(ci)) "github" else character(0))
  }

  if (!is.character(ci) || length(ci) == 0 || anyNA(ci)) {
    cli::cli_abort("{.arg ci} must be a single {.code TRUE}/{.code FALSE}, or a
      character vector naming one or more of {.val {known}}.")
  }

  unknown <- setdiff(ci, known)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown CI provider{?s} in {.arg ci}: {unknown}.",
      "i" = "Known provider{?s}: {known}."
    ))
  }

  intersect(known, ci)
}

# The full set of files THIS call would write, as a named character vector:
# name = destination path relative to `path`, value = the absolute source
# file to copy, or NA to mean "generated content this function writes
# itself" (currently only `.gitignore`).
#
# Building this list is split out from create_template_repo() itself so the
# "compute every target, then check, then write" structure the overwrite
# contract requires is visible as one small function with one job, rather
# than interleaved with the writing loop where it would be easy to grow a
# target that only the check OR only the write loop knows about.
.dta_create_repo_targets <- function(skeleton_dir, examples, ci_providers, agent_skill) {
  targets <- c(
    ".gitignore" = NA_character_,
    "README.md" = file.path(skeleton_dir, "README.md")
  )

  if (isTRUE(examples)) {
    example_files <- .dta_create_repo_example_files(skeleton_dir)
    targets <- c(targets, stats::setNames(file.path(skeleton_dir, example_files), example_files))
  }

  for (provider in ci_providers) {
    spec <- .dta_create_repo_ci_asset_targets[[provider]]
    targets <- c(targets, stats::setNames(.dta_create_repo_ci_asset_file(spec[["asset"]]), spec[["dest"]]))
  }

  if (isTRUE(agent_skill)) {
    skill_dir <- .dta_create_repo_skill_dir()
    skill_files <- list.files(skill_dir, recursive = TRUE)
    targets <- c(targets, stats::setNames(
      file.path(skill_dir, skill_files),
      file.path(".claude", "skills", .dta_create_repo_skill_name, skill_files)
    ))
  }

  targets
}

#' @title Scaffold a new private DTAtools template repository
#' @description
#' Creates a directory laid out the way a private DTAtools template
#' repository is meant to be: a small, self-consistent worked example of the
#' four template kinds (a creation template and a dataset template in
#' `templates/`, a party profile in `parties/`, a controlled vocabulary in
#' `vocabulary/`), an app configuration `dta-app.yaml` and an
#' `export-templates/` folder for Word templates, a root-level
#' `README.md` with the full authoring guide, a
#' `.gitignore`, and -- by default -- a GitHub Actions workflow that runs
#' [lint_template_repo()] against the repository on every push and pull
#' request, and -- by default -- an agent skill that tells coding agents how
#' to author, release and lint the library's templates. With the default
#' `examples = TRUE`, the result already passes
#' `validate_template(path, strict = TRUE)` unmodified, so it is a starting
#' point to edit rather than a stub to fill in from nothing. See `vignette("private-templates", package =
#' "DTAtools")` for how to point the bundled Shiny app at the directory this
#' creates.
#' @details
#' The scaffold mirrors the bundled skeleton, with the full authoring guide
#' at the root and one folder per kind of file:
#'
#' ```text
#' <path>/README.md
#' <path>/.gitignore
#' <path>/dta-app.yaml                            (app configuration, all commented out)
#' <path>/templates/starter.dta-template.yaml
#' <path>/templates/starter_dataset.dta-dataset-template.yaml
#' <path>/parties/starter_supplier.dta-party.yaml
#' <path>/vocabulary/starter_terms.dta-vocabulary.yaml
#' <path>/export-templates/README.md              (drop Word .docx templates here)
#' <path>/.github/workflows/validate-templates.yml  (ci = "github", default)
#' <path>/bitbucket-pipelines.yml                 (ci = "bitbucket")
#' <path>/Jenkinsfile                             (ci = "jenkins")
#' <path>/.claude/skills/dta-template-authoring/SKILL.md      (agent_skill = TRUE, default)
#' <path>/.claude/skills/dta-template-authoring/references/   (its three reference files)
#' ```
#'
#' Only the selected CI files are written. [validate_template()] detects the
#' layout from the repository root: point it (and the app's template sources)
#' at `<path>` itself, never at one of the folders inside it.
#' @param path A single non-empty character string: the directory to create
#'   the repository in. Created recursively when it does not already exist.
#'   Aborts via [cli::cli_abort()] if `path` names an existing file rather
#'   than a directory.
#' @param examples A single `TRUE`/`FALSE` (default `TRUE`). When `TRUE`,
#'   writes the four starter templates into `templates/`, `parties/` and
#'   `vocabulary/` beneath `path`, plus the commented-out `dta-app.yaml` and
#'   `export-templates/README.md`. `README.md` stays at the repository root.
#'   When `FALSE`, only `README.md`,
#'   `.gitignore` and (if `ci` asks for any provider) its CI file(s) are
#'   written -- no template files, for a caller who wants the scaffolding
#'   without the worked example. Such a repository has no templates yet, so
#'   [validate_template()] reports the error `no_templates` and the generated
#'   CI workflow fails until a template is added.
#' @param ci Either a single `TRUE`/`FALSE` (default `TRUE`), or a character
#'   vector naming one or more of `"github"`, `"bitbucket"`, `"jenkins"`
#'   (duplicates are silently dropped). `TRUE` is shorthand for `"github"`
#'   only, matching this argument's original, still-default behaviour;
#'   `FALSE` writes no CI file at all, and no `.github` directory. Each
#'   provider writes one ready-to-run file that lints the repository with
#'   `lint_template_repo(".")`: `"github"` writes
#'   `.github/workflows/validate-templates.yml` (GitHub Actions, triggered on
#'   push and pull request); `"bitbucket"` writes `bitbucket-pipelines.yml`
#'   (Bitbucket **Cloud** Pipelines only -- Bitbucket Server / Data Center has
#'   no Pipelines feature); `"jenkins"` writes a declarative `Jenkinsfile`,
#'   which is what a Bitbucket Server / Data Center deployment needs instead,
#'   with its trigger configured on the Jenkins job rather than in the file.
#' @param overwrite A single `TRUE`/`FALSE` (default `FALSE`). When `FALSE`,
#'   this call aborts via [cli::cli_abort()], naming every conflicting file,
#'   and writes nothing at all, if any file it would write already exists at
#'   `path` -- a partially-written repository would leave a caller unable to
#'   tell which half of a family of cross-referencing templates they were
#'   looking at. When `TRUE`, those files are replaced.
#' @param agent_skill A single `TRUE`/`FALSE` (default `TRUE`). When `TRUE`,
#'   copies the agent skill bundled with DTAtools
#'   (`inst/extdata/agent-skills/dta-template-authoring/`) to
#'   `.claude/skills/dta-template-authoring/`, the folder from which coding
#'   agents that support the Agent Skills format load it. The skill covers
#'   designing a template family (extends, mixins, sealing), releasing changes
#'   without altering released versions, and running and acting on
#'   [validate_template()]'s findings. It is written whatever `examples` says.
#'   To add it to, or refresh it in, an existing repository, copy that folder
#'   from `system.file("extdata", "agent-skills", package = "DTAtools")`.
#' @return Invisibly, the normalised absolute path to `path` (see
#'   [base::normalizePath()]).
#' @export
#' @examples
#' dir <- file.path(tempdir(), "my-templates")
#' create_template_repo(dir)
#' list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
create_template_repo <- function(path, examples = TRUE, ci = TRUE, overwrite = FALSE,
                                 agent_skill = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty directory path.")
  }
  if (!is.logical(examples) || length(examples) != 1 || is.na(examples)) {
    cli::cli_abort("{.arg examples} must be a single {.code TRUE}/{.code FALSE}.")
  }
  if (!is.logical(agent_skill) || length(agent_skill) != 1 || is.na(agent_skill)) {
    cli::cli_abort("{.arg agent_skill} must be a single {.code TRUE}/{.code FALSE}.")
  }
  ci_providers <- .dta_create_repo_validate_ci(ci)
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be a single {.code TRUE}/{.code FALSE}.")
  }
  # file.exists() is TRUE for a directory too, so a FALSE dir.exists() next
  # to a TRUE file.exists() is exactly "something is there and it is not a
  # directory" -- the one shape genuinely incompatible with what this
  # function does next (dir.create(path, recursive = TRUE), which is
  # harmless when `path` is already a directory).
  if (file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("{.arg path} names an existing file, not a directory: {.file {path}}")
  }

  skeleton_dir <- .dta_create_repo_skeleton_dir()
  targets <- .dta_create_repo_targets(skeleton_dir, examples, ci_providers, agent_skill)

  # Computed and checked BEFORE anything is written -- see `overwrite`'s
  # @param above for why a half-written repository is worse than the
  # conflict it would have avoided.
  dest_paths <- file.path(path, names(targets))
  existing <- dest_paths[file.exists(dest_paths)]
  if (!isTRUE(overwrite) && length(existing) > 0) {
    cli::cli_abort(c(
      "{.arg overwrite} is {.code FALSE} and {length(existing)} file{?s} already
        exist{?s/} at {.arg path}:",
      stats::setNames(existing, rep("x", length(existing))),
      "i" = "Set {.code overwrite = TRUE} to replace {cli::qty(length(existing))}
        {?it/them}, or choose an empty {.arg path}."
    ))
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(targets)) {
    dest <- dest_paths[[i]]
    src <- targets[[i]]
    # Needed for the kind folders and the workflow's .github/workflows/
    # destination; a no-op everywhere else, since `path` itself was already
    # created above.
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

    ok <- if (is.na(src)) {
      writeLines(.dta_create_repo_gitignore_lines(), dest)
      TRUE
    } else {
      file.copy(src, dest, overwrite = TRUE)
    }
    if (!isTRUE(ok)) {
      cli::cli_abort("Failed to write {.file {dest}}.")
    }
  }

  path_out <- normalizePath(path, winslash = "/", mustWork = TRUE)
  # Without the starter files there is nothing to validate yet: the suggested
  # command would only fail with `no_templates`, and so would any CI just
  # written, until the first template is added.
  next_step <- if (isTRUE(examples)) {
    "Next: {.code DTAtools::lint_template_repo(\"{path_out}\")}"
  } else {
    "Next: add your template files to {.file templates/}, {.file parties/} and {.file vocabulary/}; until then {.fn validate_template} (and any CI written) fails with {.code no_templates}."
  }
  cli::cli_inform(c(
    "v" = "Created a template repository at {.file {path_out}} (files go in {.file templates/}, {.file parties/}, {.file vocabulary/} and {.file export-templates/}).",
    "*" = "Wrote {.file {names(targets)}}.",
    "i" = next_step
  ))
  invisible(path_out)
}
