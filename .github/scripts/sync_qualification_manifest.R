#!/usr/bin/env Rscript
# Keep the installed-file manifest that the installation qualification checks.
#
# The IQ stage compares the files it finds in the installed package against
# this manifest, so that a deployment can prove the bytes on the target machine
# are the bytes that were released. That claim only means something if the
# manifest is generated from the repository rather than from the installation
# it is meant to check, which is what this script does.
#
#   Rscript .github/scripts/sync_qualification_manifest.R            # write
#   Rscript .github/scripts/sync_qualification_manifest.R --check    # verify
#
# `--check` is what the r-style workflow runs: it fails, and does not fix, so
# that a change to inst/ arrives together with the manifest describing it.

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args

manifest_path <- "inst/qualification/baseline/inst-files.sha256"

hash_fun <- if (exists("sha256sum", asNamespace("tools"))) {
  get("sha256sum", asNamespace("tools"))
} else {
  # R < 4.5 has no sha256sum. The manifest then records md5, and says so in its
  # header, rather than silently mixing algorithms between machines.
  tools::md5sum
}
algorithm <- if (exists("sha256sum", asNamespace("tools"))) "sha256" else "md5"

files <- list.files("inst", recursive = TRUE, all.files = FALSE)
# The manifest cannot describe itself. Everything else under baseline/ is
# measured on the machine that runs the qualification rather than released
# with the package, so the whole directory is excluded.
files <- files[!startsWith(files, "qualification/baseline/")]
# Radix ordering, so the file is byte-identical whatever locale it was written
# in. The default collation on this project's development machines is not the
# C collation continuous integration uses.
files <- sort(files, method = "radix")

hashes <- unname(vapply(file.path("inst", files), hash_fun, character(1)))
lines <- c(
  sprintf("# %s of every file under inst/, excluding qualification/baseline/.", algorithm),
  "# Regenerate with: Rscript .github/scripts/sync_qualification_manifest.R",
  sprintf("%s  %s", hashes, files)
)

if (check_only) {
  if (!file.exists(manifest_path)) {
    cat("sync_qualification_manifest.R: the manifest does not exist.\n")
    cat("Create it with: Rscript .github/scripts/sync_qualification_manifest.R\n")
    quit(status = 1)
  }
  current <- readLines(manifest_path, warn = FALSE)
  if (!identical(current, lines)) {
    old <- setdiff(current, lines)
    new <- setdiff(lines, current)
    cat("sync_qualification_manifest.R: the manifest is out of date.\n\n")
    if (length(old)) {
      cat("No longer matching:\n")
      cat(paste0("  ", utils::head(old, 20), "\n"), sep = "")
    }
    if (length(new)) {
      cat("Now expected:\n")
      cat(paste0("  ", utils::head(new, 20), "\n"), sep = "")
    }
    cat("\nRegenerate with: Rscript .github/scripts/sync_qualification_manifest.R\n")
    quit(status = 1)
  }
  cat(sprintf(
    "sync_qualification_manifest.R: OK -- %d files match the manifest.\n",
    length(files)
  ))
} else {
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  # Through a binary connection, so the file is LF on every platform. A text
  # connection on Windows writes CRLF, which .gitattributes then normalises in
  # the index -- leaving a working copy that differs from what was committed
  # and a `mixed-line-ending` hook that rejects the commit. `.github/scripts/
  # style.R` repairs the same thing after styler for the same reason.
  con <- file(manifest_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, sep = "
")
  close(con)
  cat(sprintf(
    "sync_qualification_manifest.R: wrote %s (%d files, %s).\n",
    manifest_path, length(files), algorithm
  ))
}
