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
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @details
#' Requires the suggested packages **shiny**, **bslib**, and **DT**. If any are
#' missing, an informative error explains what to install.
#'
#' @return Called for its side effect of running the Shiny app. Invisibly
#'   returns `NULL`.
#'
#' @examples
#' \dontrun{
#' run_dta_app()
#' }
#' @export
run_dta_app <- function(launch.browser = TRUE, port = NULL, ...) {
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

  args <- list(appDir = app_dir, launch.browser = launch.browser, ...)
  if (!is.null(port)) {
    args$port <- port
  }

  do.call(shiny::runApp, args)
  invisible(NULL)
}
