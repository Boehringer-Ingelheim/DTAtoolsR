#!/usr/bin/env Rscript

# unname(): read.dcf() returns a matrix whose single cell keeps the field name,
# and identical() compares attributes -- so without this the comparison below is
# FALSE even when the two version strings agree character for character.
desc_version <- unname(trimws(read.dcf("DESCRIPTION", fields = "Version")[1, 1]))
version_file <- "inst/shiny/dta_app/VERSION"

if (!file.exists(version_file)) {
  cat(sprintf("Missing required version file: %s\n", version_file))
  quit(status = 1)
}

file_lines <- readLines(version_file, warn = FALSE, encoding = "UTF-8")
if (length(file_lines) == 0) {
  cat(sprintf("%s is empty.\n", version_file))
  quit(status = 1)
}

file_version <- trimws(file_lines[[1]])
if (!nzchar(file_version)) {
  cat(sprintf("First line of %s is empty.\n", version_file))
  quit(status = 1)
}

if (!identical(desc_version, file_version)) {
  cat("Shiny VERSION drift detected.\n")
  cat(sprintf("  DESCRIPTION Version: %s\n", desc_version))
  cat(sprintf("  %s: %s\n", version_file, file_version))
  cat("Update inst/shiny/dta_app/VERSION to match DESCRIPTION Version.\n")
  quit(status = 1)
}

cat("check_shiny_version_file.R: OK -- inst/shiny/dta_app/VERSION matches DESCRIPTION Version.\n")
