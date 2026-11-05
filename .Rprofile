# Connect deploys the repository root, so this file ships in the bundle and is
# sourced at content start-up. POSIT_PRODUCT is current; RSTUDIO_PRODUCT is
# its deprecated predecessor.
if ("CONNECT" %in% Sys.getenv(c("POSIT_PRODUCT", "RSTUDIO_PRODUCT"))) {
  # Shiny sources an app directory's R/ before its app.R. At the root, R/ is
  # the package source -- which app.R installs properly instead -- so sourcing
  # it loose fails. app.R switches this back on for the real app directory.
  # (Shiny's other switch, R/_disable_autoload.R, is not an option: roxygen
  # adds it to Collate, and R rejects it as an invalid code file name.)
  options(shiny.autoload.r = FALSE)
} else {
  # Not on Connect: the renv project library. There, Connect manages the
  # library itself and this project's renv library does not exist.
  source("renv/activate.R")
}
