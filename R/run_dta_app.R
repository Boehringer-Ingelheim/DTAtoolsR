#' @title Launch the DTAtools Shiny Application
#' @description
#' Starts the interactive DTAtools Shiny app: a modern user interface for
#' loading a DTA YAML specification, uploading data files per dataset, running
#' validation (`check()`), inspecting errors, editing metadata, and exporting
#' Word/PDF documents.
#'
#' The app is shipped inside the package under `inst/shiny/dta_app`.
#'
#' @param launch.browser Logical. Whether to open the app in the system browser.
#'   Passed to [shiny::runApp()]. Default `TRUE`.
#' @param port Optional integer port for the app. Passed to [shiny::runApp()].
#' @param restore_session Logical. Whether the app may autosave your work and
#'   offer to restore it after a reload. Default `TRUE`, which is right for the
#'   single-user local run this function performs. See Details.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @details
#' Requires the suggested packages **shiny**, **bslib**, and **DT**. If any are
#' missing, an informative error explains what to install.
#'
#' # Session restore and who can see it
#'
#' The autosave that backs "Restore previous session" is a file on disk, and one
#' R process serves every browser connected to it. On a shared deployment that
#' makes the saved work readable by whoever connects next, so the app writes it
#' only when this function has explicitly enabled it -- which it does by
#' pointing `options(DTAtools.app.session_dir)` at a directory created for this
#' launch. An app started any other way (Shiny Server, Connect, a bare
#' `shiny::runApp()` on the app directory) sees no such option and does not
#' autosave or offer to restore at all.
#'
#' Pass `restore_session = FALSE` to turn it off even locally.
#'
#' @return Called for its side effect of running the Shiny app. Invisibly
#'   returns `NULL`.
#'
#' @examples
#' \dontrun{
#' run_dta_app()
#' }
#' @export
run_dta_app <- function(launch.browser = TRUE, port = NULL,
                        restore_session = TRUE, ...) {
  required <- c("shiny", "bslib", "DT")
  missing <- required[!vapply(
    required,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "The DTAtools app needs additional package{?s}: {.pkg {missing}}.",
      "i" = 'Install with: install.packages(c({paste0(sprintf("\\"%s\\"", missing), collapse = ", ")}))'
    ))
  }

  app_dir <- system.file("shiny", "dta_app", package = "DTAtools")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    cli::cli_abort(
      "Could not find the app directory. Try reinstalling {.pkg DTAtools}."
    )
  }

  if (isTRUE(restore_session)) {
    session_dir <- file.path(tempdir(), "dtatools-app-session")
    dir.create(session_dir, showWarnings = FALSE, recursive = TRUE)
    previous <- getOption("DTAtools.app.session_dir")
    options(DTAtools.app.session_dir = session_dir)
    on.exit(options(DTAtools.app.session_dir = previous), add = TRUE)
  }

  args <- list(appDir = app_dir, launch.browser = launch.browser, ...)
  if (!is.null(port)) {
    args$port <- port
  }

  do.call(shiny::runApp, args)
  invisible(NULL)
}
