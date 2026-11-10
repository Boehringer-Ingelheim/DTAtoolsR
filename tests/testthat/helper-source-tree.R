# The package's R/ source files, when the tests run from a source tree.
#
# Empty when they run against an installed copy -- R CMD check, and covr,
# whose package_coverage() installs the package and runs tests/ from inside
# the installation. There R/ still EXISTS (it holds the lazy-load database),
# so a dir.exists() check passes and a test then finds no sources and fails.
# Tests that read the sources skip on an empty result instead.
source_r_files <- function(pattern = "[.]R$") {
  r_dir <- testthat::test_path("..", "..", "R")
  if (!dir.exists(r_dir)) {
    return(character(0))
  }
  list.files(r_dir, pattern = pattern, full.names = TRUE)
}

# Skip when there is no source tree -- unless the runner says there must be one.
# Under R CMD check / covr the package runs from its installation, so the
# source-reading and package-load tests below can never run there. A CI step
# that runs `devtools::test(filter = "generic-guards|source-portability|
# templateGuide-collect")` from the checkout sets DTATOOLS_REQUIRE_SOURCE_TREE=true,
# which turns the skip into a failure so those tests cannot go silently dead.
skip_if_no_source_tree <- function(message = "package source not available") {
  if (length(source_r_files()) > 0) {
    return(invisible())
  }
  if (identical(Sys.getenv("DTATOOLS_REQUIRE_SOURCE_TREE"), "true")) {
    testthat::fail(paste0("DTATOOLS_REQUIRE_SOURCE_TREE=true but no source tree: ", message))
    return(invisible())
  }
  testthat::skip(message)
}
