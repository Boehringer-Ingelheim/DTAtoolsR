#' Lint a template repository
#'
#' Runs every check of [validate_template()] on the repository rooted at
#' `path` and adds three checks on the repository's layout and file naming,
#' then prints every finding and, by default, aborts when any is an error. It
#' is the command a template repository's CI runs, and the one to run by hand
#' before pushing: `Rscript -e 'DTAtools::lint_template_repo(".")'`
#' exits with a non-zero status when it aborts.
#'
#' @details
#' The returned rows are those of [validate_template()] plus the following
#' repository-only codes (`validate_template()` itself never reports them).
#'
#' Errors:
#' * `folder_name_case`: a folder directly below `path` is spelled
#'   differently from `templates`, `parties`, `vocabulary` or
#'   `export-templates` only by letter case (`Vocabulary`). Windows and macOS
#'   read it; the Linux server does not.
#'
#' Warnings:
#' * `id_filename_mismatch`: the file name stem (the name without the kind
#'   suffix) is neither the template's `id` nor the `id` followed by `_`, `-`,
#'   `.` or `@` and a qualifier, so `biomarker_gf_2.dta-template.yaml` is
#'   accepted for `id: biomarker_gf`.
#' * `file_ignored`: a YAML file in the repository root or in a kind folder
#'   declares a template `kind` but its file name lacks that kind's suffix, so
#'   the app never loads it. YAML files that declare no template kind (CI
#'   configuration, `dta-app.yaml`, a dataset specification referenced by
#'   `source:`) are not reported.
#'
#' @param path A single string: the root of the template repository.
#'   Defaults to the working directory.
#' @param fail_on One of `"error"` (default), `"warning"` or `"none"`: the
#'   lowest severity that makes the call abort. With `"none"` the call never
#'   aborts.
#' @return Invisibly, a data frame with the columns of [validate_template()]
#'   (`file`, `kind`, `id`, `version`, `severity`, `code`, `message`), sorted
#'   by file and code; zero rows when the repository is clean. When a finding
#'   reaches `fail_on`, the call aborts with class `dta_template_lint_error`
#'   after printing the findings.
#' @seealso [validate_template()], [create_template_repo()]
#' @export
#' @examples
#' dir <- file.path(tempdir(), "lint-example")
#' create_template_repo(dir, overwrite = TRUE)
#' lint_template_repo(dir)
lint_template_repo <- function(path = ".", fail_on = "error") {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
    !dir.exists(path)) {
    cli::cli_abort(
      "{.arg path} must be an existing directory: the root of the template repository."
    )
  }
  if (!is.character(fail_on) || length(fail_on) != 1L ||
    !fail_on %in% c("error", "warning", "none")) {
    cli::cli_abort("{.arg fail_on} must be one of {.val error}, {.val warning} or {.val none}.")
  }

  findings <- .dta_template_bind_rows(list(
    validate_template(path),
    .dta_lint_folder_case(path),
    .dta_lint_id_filename(path),
    .dta_lint_file_ignored(path)
  ))
  findings <- findings[order(findings$file, findings$code, method = "radix"), , drop = FALSE]
  rownames(findings) <- NULL

  .dta_lint_template_report(findings, path)

  failing <- switch(fail_on,
    error = sum(findings$severity == "error"),
    warning = nrow(findings),
    none = 0L
  )
  if (failing > 0) {
    cli::cli_abort(
      "Template repository lint failed: {failing} finding{?s} at severity {.val {fail_on}} or above.",
      class = "dta_template_lint_error"
    )
  }
  invisible(findings)
}

# Print every finding as one line, errors marked "x" and warnings "!".
# .cli_escape() because file names and messages may hold braces.
.dta_lint_template_report <- function(findings, path) {
  if (nrow(findings) == 0) {
    cli::cli_alert_success("No findings in {.file {path}}.")
    return(invisible(NULL))
  }
  cli::cli_bullets(stats::setNames(
    .cli_escape(sprintf("%s [%s] %s", findings$file, findings$code, findings$message)),
    ifelse(findings$severity == "error", "x", "!")
  ))
  n_err <- sum(findings$severity == "error")
  n_warn <- sum(findings$severity == "warning")
  cli::cli_text("{n_err} error{?s}, {n_warn} warning{?s}.")
  invisible(NULL)
}

# Folders that differ from a canonical folder name only by letter case.
.dta_lint_folder_case <- function(path) {
  canon <- c(
    .dta_template_engine_get("dta_template_kind_subdirs")(),
    .dta_template_engine_get("dta_export_templates_subdir")()
  )
  dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  bad <- dirs[tolower(dirs) %in% canon & !(dirs %in% canon)]
  rows <- lapply(bad, function(d) {
    right <- canon[match(tolower(d), canon)]
    .dta_template_row(
      file = file.path(path, d), kind = NA, id = NA, version = NA,
      severity = "error", code = "folder_name_case",
      message = sprintf(
        "Folder `%s` must be spelled `%s`: the Linux server does not read it under this name.",
        d, right
      )
    )
  })
  .dta_template_bind_rows(rows)
}

# A file's name stem should be its id, or the id plus a qualifier.
.dta_lint_id_filename <- function(path) {
  kind_pattern <- .dta_template_engine_get("dta_template_kind_pattern")
  rows <- list()
  for (k in .dta_template_engine_get("dta_template_all_kinds")()) {
    for (f in .dta_template_list_dir(path, k)) {
      rec <- .dta_template_read_raw(f)
      if (!rec$ok || is.na(rec$id) || !nzchar(rec$id)) next
      stem <- sub(kind_pattern(k), "", basename(f), ignore.case = TRUE)
      if (stem == rec$id || any(startsWith(stem, paste0(rec$id, c("_", "-", ".", "@"))))) next
      rows[[length(rows) + 1L]] <- .dta_template_row(
        file = f, kind = k, id = rec$id, version = rec$version,
        severity = "warning", code = "id_filename_mismatch",
        message = sprintf(
          "File name `%s` does not start with the template id `%s`; rename the file to `%s`.",
          basename(f), rec$id, sub(stem, rec$id, basename(f), fixed = TRUE)
        )
      )
    }
  }
  .dta_template_bind_rows(rows)
}

# YAML files the app never loads although they declare a template kind.
.dta_lint_file_ignored <- function(path) {
  g <- .dta_template_engine_get
  kinds <- g("dta_template_all_kinds")()
  patterns <- vapply(kinds, g("dta_template_kind_pattern"), character(1))
  read_quiet <- g("dta_template_read_yaml_quiet")
  rows <- list()
  for (d in c(path, file.path(path, g("dta_template_kind_subdirs")()))) {
    if (!dir.exists(d)) next
    files <- list.files(d, pattern = "\\.ya?ml$", ignore.case = TRUE, full.names = TRUE)
    files <- files[!dir.exists(files)]
    for (f in files) {
      if (any(vapply(patterns, grepl, logical(1), x = basename(f), ignore.case = TRUE))) next
      read <- read_quiet(f)
      if (!isTRUE(read$ok) || !is.list(read$value) || is.null(names(read$value))) next
      kind <- .dta_template_scalar_chr(read$value[["kind"]])
      if (!isTRUE(kind %in% kinds)) next
      rows[[length(rows) + 1L]] <- .dta_template_row(
        file = f, kind = kind,
        id = .dta_template_scalar_chr(read$value[["id"]]),
        version = .dta_template_scalar_chr(read$value[["version"]]),
        severity = "warning", code = "file_ignored",
        message = sprintf(
          "File declares kind %s but its file name lacks that kind's suffix, so the app never loads it; rename it to `<id>.<suffix>` (see File naming conventions).",
          kind
        )
      )
    }
  }
  .dta_template_bind_rows(rows)
}
