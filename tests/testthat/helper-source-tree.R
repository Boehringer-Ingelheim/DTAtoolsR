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
