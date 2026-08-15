#!/usr/bin/env Rscript

# Single source of truth for the package version across every file that carries
# it. Run from the repository root:
#
#   Rscript .github/scripts/bump_version.R 0.17.4     # rewrite every site
#   Rscript .github/scripts/bump_version.R --check    # report drift, write nothing
#
# `--check` is what CI runs (see check_version_sync.R); it exits non-zero on any
# mismatch. The two modes share the site definitions below, so the checker can
# never fall out of step with the writer.
#
# Four files, six places. Only DESCRIPTION <-> VERSION used to be checked, which
# is exactly why manifest.json and docs/index.html had both silently drifted
# behind by the time this script was written.
#
# manifest.json is patched line by line rather than round-tripped through
# jsonlite deliberately. It is a 3100-line rsconnect manifest whose `packages`
# block records the locally installed renv library and whose header records
# `"locale": "de_DE"`; regenerating it with rsconnect::writeManifest() would
# rewrite all of that to match whatever machine ran the bump, burying the two
# lines that actually changed. A textual patch keeps the diff to those lines.

# --- helpers -----------------------------------------------------------------

# Read/write as UTF-8 with explicit LF endings. docs/index.html carries em
# dashes and emoji, and the repo is LF throughout (pre-commit's
# mixed-line-ending hook enforces it) -- writeLines() to a text connection on
# Windows would silently emit CRLF and turn a two-line change into a whole-file
# diff.
read_lines_utf8 <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

write_lines_utf8 <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(lines), con, sep = "\n", useBytes = TRUE)
}

fail <- function(...) {
  cat("bump_version.R: ", sprintf(...), "\n", sep = "")
  quit(status = 1)
}

# Locate the single line matching `pattern`. Anything other than exactly one hit
# means the file's shape changed and the caller's assumption no longer holds --
# better to stop loudly than to patch the wrong line or silently patch nothing.
sole_match <- function(lines, pattern, what, from = 1L) {
  idx <- grep(pattern, lines)
  idx <- idx[idx >= from]
  if (length(idx) != 1L) {
    fail(
      "expected exactly one %s, found %d. The file's structure changed; update this script.",
      what, length(idx)
    )
  }
  idx
}

# unname(): read.dcf() returns a matrix whose single cell keeps the field name,
# and identical() compares attributes -- so without this the comparison against a
# plain string is FALSE even when the two versions agree character for character.
# (Inherited from check_shiny_version_file.R, which this script replaces.)
description_version <- function() {
  unname(trimws(read.dcf("DESCRIPTION", fields = "Version")[1, 1]))
}

# --- the sites ---------------------------------------------------------------
#
# Each site knows how to read its current value and how to rewrite it. `read`
# returns the version (or checksum) as currently recorded; `write` takes the
# target version and rewrites the file.

MANIFEST <- "inst/shiny/dta_app/manifest.json"
VERSION_FILE <- "inst/shiny/dta_app/VERSION"
DOCS <- "docs/index.html"

# manifest.json: the DTAtools entry inside `packages`. Scoped to the lines after
# the `"DTAtools": {` key so the ~90 other "Version" lines (one per renv
# package) cannot be hit by accident.
manifest_pkg_version_line <- function(lines) {
  anchor <- sole_match(lines, '^\\s*"DTAtools": \\{\\s*$', "DTAtools package entry in the manifest")
  rel <- grep('^\\s*"Version": ".*",?\\s*$', lines[anchor:length(lines)])
  if (length(rel) == 0L) fail("no \"Version\" line found under the manifest's DTAtools entry.")
  anchor + rel[1L] - 1L
}

# manifest.json: the recorded md5 of the VERSION file, inside `files`.
manifest_version_checksum_line <- function(lines) {
  anchor <- sole_match(lines, '^\\s*"VERSION": \\{\\s*$', "VERSION entry in the manifest's files block")
  cs <- anchor + 1L
  if (!grepl('"checksum"', lines[cs])) {
    fail("the manifest's VERSION entry is not followed by a checksum line.")
  }
  cs
}

extract <- function(line, pattern) sub(pattern, "\\1", line)

sites <- list(
  list(
    name = "DESCRIPTION Version",
    read = description_version,
    write = function(v) {
      lines <- read_lines_utf8("DESCRIPTION")
      i <- sole_match(lines, "^Version:", "Version: field in DESCRIPTION")
      lines[i] <- paste0("Version: ", v)
      write_lines_utf8(lines, "DESCRIPTION")
    }
  ),
  list(
    name = paste0(VERSION_FILE),
    read = function() {
      lines <- read_lines_utf8(VERSION_FILE)
      if (length(lines) == 0L) fail("%s is empty.", VERSION_FILE)
      trimws(lines[[1L]])
    },
    write = function(v) write_lines_utf8(v, VERSION_FILE)
  ),
  list(
    name = "manifest.json DTAtools Version",
    read = function() {
      lines <- read_lines_utf8(MANIFEST)
      extract(lines[manifest_pkg_version_line(lines)], '^\\s*"Version": "([^"]*)".*$')
    },
    write = function(v) {
      lines <- read_lines_utf8(MANIFEST)
      i <- manifest_pkg_version_line(lines)
      lines[i] <- sub('"Version": "[^"]*"', sprintf('"Version": "%s"', v), lines[i])
      write_lines_utf8(lines, MANIFEST)
    }
  ),
  list(
    name = "manifest.json VERSION checksum",
    # Compares the recorded md5 against the VERSION file as it stands on disk,
    # so this site drifts whenever the version file is edited without the
    # manifest being repatched.
    read = function() {
      lines <- read_lines_utf8(MANIFEST)
      extract(lines[manifest_version_checksum_line(lines)], '^\\s*"checksum": "([^"]*)".*$')
    },
    expected = function(v) unname(tools::md5sum(VERSION_FILE)),
    write = function(v) {
      # Runs after the VERSION file has been rewritten, so md5sum() sees the new
      # contents. Order matters; see the ordering note in bump().
      lines <- read_lines_utf8(MANIFEST)
      i <- manifest_version_checksum_line(lines)
      lines[i] <- sub(
        '"checksum": "[^"]*"',
        sprintf('"checksum": "%s"', unname(tools::md5sum(VERSION_FILE))),
        lines[i]
      )
      write_lines_utf8(lines, MANIFEST)
    }
  ),
  list(
    name = "docs/index.html version badge",
    read = function() {
      lines <- read_lines_utf8(DOCS)
      i <- sole_match(lines, 'class="version-badge">v', "version badge in docs/index.html")
      extract(lines[i], '^.*class="version-badge">v([0-9][^<]*)<.*$')
    },
    write = function(v) {
      lines <- read_lines_utf8(DOCS)
      i <- sole_match(lines, 'class="version-badge">v', "version badge in docs/index.html")
      lines[i] <- sub(
        '(class="version-badge">v)[0-9][^<]*',
        sprintf("\\1%s", v), lines[i]
      )
      write_lines_utf8(lines, DOCS)
    }
  ),
  list(
    name = "docs/index.html footer",
    read = function() {
      lines <- read_lines_utf8(DOCS)
      i <- sole_match(lines, "DTAtools v[0-9]", "footer version in docs/index.html")
      extract(lines[i], "^.*DTAtools v([0-9][0-9.]*).*$")
    },
    write = function(v) {
      lines <- read_lines_utf8(DOCS)
      i <- sole_match(lines, "DTAtools v[0-9]", "footer version in docs/index.html")
      lines[i] <- sub("(DTAtools v)[0-9][0-9.]*", sprintf("\\1%s", v), lines[i])
      write_lines_utf8(lines, DOCS)
    }
  )
)

# --- CHANGELOG ---------------------------------------------------------------
#
# Write-only, and idempotent. The documented workflow (CLAUDE.md) is that
# changes accumulate under `## [Unreleased]`, so a bump promotes that heading to
# the released version. Falls back to inserting a fresh stanza when there is
# nothing to promote, and does nothing at all when the version already has one --
# which is what makes re-running the bump at the current version safe.
update_changelog <- function(v) {
  path <- "CHANGELOG.md"
  lines <- read_lines_utf8(path)
  today <- format(Sys.Date(), "%Y-%m-%d")
  heading <- sprintf("## [%s] - %s", v, today)

  if (any(grepl(sprintf("^## \\[%s\\]", gsub(".", "\\.", v, fixed = TRUE)), lines))) {
    cat("  CHANGELOG.md          already has a stanza for", v, "- left alone\n")
    return(invisible(FALSE))
  }

  unreleased <- grep("^## \\[Unreleased\\]", lines)
  if (length(unreleased) == 1L) {
    lines[unreleased] <- heading
    write_lines_utf8(lines, path)
    cat("  CHANGELOG.md          promoted [Unreleased] ->", heading, "\n")
    return(invisible(TRUE))
  }

  first <- grep("^## \\[", lines)
  if (length(first) == 0L) fail("CHANGELOG.md has no '## [' version heading to insert before.")
  at <- first[1L]
  stanza <- c(heading, "", "### Changed", "", "- TODO: describe this release.", "")
  lines <- append(lines, stanza, after = at - 1L)
  write_lines_utf8(lines, path)
  cat("  CHANGELOG.md          inserted", heading, "(fill in the TODO)\n")
  invisible(TRUE)
}

# --- modes -------------------------------------------------------------------

# What each site *should* say. Usually the target version; the manifest checksum
# site overrides this with the md5 of the VERSION file on disk.
expected_for <- function(site, v) {
  if (is.null(site$expected)) v else site$expected(v)
}

check <- function() {
  v <- description_version()
  cat("Version sync check (DESCRIPTION is the source of truth)\n\n")
  cat(sprintf("  %-34s %s\n", "DESCRIPTION Version", v))
  cat(sprintf("  %-34s %s\n\n", strrep("-", 34), strrep("-", 12)))

  drift <- character(0)
  for (site in sites[-1L]) {
    want <- expected_for(site, v)
    got <- site$read()
    ok <- identical(want, got)
    if (!ok) drift <- c(drift, site$name)
    cat(sprintf(
      "  %-34s %-34s %s\n",
      site$name, got, if (ok) "OK" else paste0("DRIFT (expected ", want, ")")
    ))
  }

  if (length(drift) > 0L) {
    cat("\nVersion drift detected in:\n")
    for (d in drift) cat("  - ", d, "\n", sep = "")
    cat(sprintf(
      "\nRun `Rscript .github/scripts/bump_version.R %s` from the repository root to resync.\n",
      v
    ))
    quit(status = 1)
  }

  cat("\nbump_version.R --check: OK -- every version site agrees with DESCRIPTION.\n")
  invisible(TRUE)
}

bump <- function(v) {
  cat("Bumping to", v, "\n\n")
  # Ordered deliberately: the VERSION file (site 2) is rewritten before the
  # manifest checksum (site 4) is recomputed from it. Reordering `sites` without
  # preserving that relationship records the md5 of the *old* file.
  for (site in sites) {
    before <- tryCatch(site$read(), error = function(e) NA_character_)
    site$write(v)
    after <- site$read()
    cat(sprintf(
      "  %-34s %s\n", site$name,
      if (identical(before, after)) paste0(after, " (unchanged)") else paste0(before, " -> ", after)
    ))
  }
  update_changelog(v)
  cat("\nDone. Review with `git diff`, then commit on `dev`.\n")
  invisible(TRUE)
}

# --- entry point -------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (!file.exists("DESCRIPTION")) {
    fail("run this from the repository root (no DESCRIPTION here).")
  }

  if (length(args) == 0L || identical(args[[1L]], "--help")) {
    cat("Usage:\n")
    cat("  Rscript .github/scripts/bump_version.R <version>   rewrite every version site\n")
    cat("  Rscript .github/scripts/bump_version.R --check     report drift, write nothing\n")
    quit(status = if (length(args) == 0L) 1 else 0)
  }

  if (identical(args[[1L]], "--check")) {
    check()
    return(invisible(NULL))
  }

  v <- trimws(args[[1L]])
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?$", v)) {
    fail("'%s' is not a valid version. Expected x.y.z or x.y.z.w (no leading 'v').", v)
  }
  bump(v)
}

main()
