#!/usr/bin/env Rscript

# Single source of truth for the package version across every file that carries
# it. Run from the repository root:
#
#   Rscript .github/scripts/bump_version.R 0.17.4              # rewrite every site
#   Rscript .github/scripts/bump_version.R --check              # report drift, write nothing
#   Rscript .github/scripts/bump_version.R --sync-manifest      # rebuild the Connect manifest's
#                                                               # file list and checksums
#
# `--check` is what CI runs (see check_version_sync.R); it exits non-zero on any
# mismatch. The two modes share the site definitions below, so the checker can
# never fall out of step with the writer.
#
# Three files, four places. Only DESCRIPTION <-> VERSION used to be checked,
# which is exactly why docs/index.html had silently drifted behind by the time
# this script was written.
#
# `--sync-manifest` is a separate axis, not part of a version bump: it rewrites
# the per-file checksums in the root manifest.json, which Posit Connect deploys.
# Only app.R is listed, so this is needed only when app.R changes;
# .github/scripts/check_manifest.R (run in r-style.yaml) fails until it is run.
#
# The manifest carries no DTAtools entry and no version: Connect deploys the
# repository root and the root app.R installs the package from that same
# checkout, so there is nothing to pin. (It used to list DTAtools as a GitHub
# dependency pinned by SHA, which always deployed a different commit than the
# app's own code.)
#
# The `packages` block is never touched here. It records one machine's renv
# library and its header records `"locale": "de_DE"`; regenerating it with
# rsconnect::writeManifest() would rewrite all of that to match whatever machine
# ran the script.

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

MANIFEST <- "manifest.json"
VERSION_FILE <- "inst/shiny/dta_app/VERSION"
DOCS <- "docs/index.html"

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

# --- the manifest's `files` block --------------------------------------------
#
# Unlike `packages` -- a frozen snapshot of one machine's renv library, which
# must never be touched (see the header comment) -- the `files` block is
# DERIVED: one entry per file, each carrying that file's md5. So it is
# regenerated wholesale rather than patched.
#
# It lists only the entry point. For a Git-backed deploy Connect ignores this
# list and bundles the whole directory with `git archive` (Connect user guide,
# "Git-backed content" > "Deployment restrictions"), so listing the whole
# repository would be a second, unchecked copy of `git ls-files`.
#
# Kept sorted with `method = "radix"` (byte order, locale-independent) so the
# block cannot flip between a German developer machine and CI's C collation.
app_bundle_files <- function() sort("app.R", method = "radix")

# The block's line span. `files` is the last key before `users`, so the two
# anchors bracket it exactly.
files_block_span <- function(lines) {
  start <- sole_match(lines, '^  "files": \\{$', "\"files\" block opening in the manifest")
  end <- sole_match(lines, '^  "users":', "\"users\" key in the manifest")
  if (end <= start) {
    fail("the manifest's \"users\" key precedes its \"files\" block; the structure changed.")
  }
  c(start = start, end = end)
}

read_files_block <- function(lines) {
  span <- files_block_span(lines)
  blk <- lines[seq(span[["start"]], span[["end"]] - 1L)]
  paths <- sub('^    "(.*)": \\{$', "\\1", grep('^    ".*": \\{$', blk, value = TRUE))
  sums <- sub('^      "checksum": "(.*)"$', "\\1", grep('^      "checksum":', blk, value = TRUE))
  if (length(paths) != length(sums)) {
    fail(
      "the manifest's \"files\" block is malformed: %d paths but %d checksums.",
      length(paths), length(sums)
    )
  }
  names(sums) <- paths
  sums
}

# Emit the block at the indentation rsconnect uses (2/4/6 spaces), with the
# trailing comma on every entry but the last.
render_files_block <- function(files) {
  out <- '  "files": {'
  for (i in seq_along(files)) {
    f <- files[[i]]
    # md5sum() returns NA for an unreadable or vanished file rather than
    # erroring. Writing "NA" as a checksum would produce a manifest that looks
    # structurally fine and fails only at deploy, so refuse instead.
    md5 <- unname(tools::md5sum(f))
    if (is.na(md5)) fail("could not compute an md5 for %s.", f)
    out <- c(
      out,
      sprintf('    "%s": {', f),
      sprintf('      "checksum": "%s"', md5),
      sprintf("    }%s", if (i < length(files)) "," else "")
    )
  }
  c(out, "  },")
}

sync_manifest <- function() {
  cat("Syncing the \"files\" block of", MANIFEST, "\n\n")

  lines <- read_lines_utf8(MANIFEST)
  before <- read_files_block(lines)
  files <- app_bundle_files()

  span <- files_block_span(lines)
  # `before`/`after` are carried over verbatim, so the `packages` snapshot above
  # the block and the `users` key below it cannot be disturbed by the splice.
  rewritten <- c(
    lines[seq_len(span[["start"]] - 1L)],
    render_files_block(files),
    lines[seq(span[["end"]], length(lines))]
  )

  # Validate the rendered result BEFORE it reaches the disk, so a failed run
  # never leaves a bad manifest in the working tree.
  staged <- read_files_block(rewritten)
  if (!identical(sort(names(staged), method = "radix"), files)) {
    fail("the rewritten \"files\" block does not list the bundle files; nothing written.")
  }

  # Only touch the file when the bytes actually differ, so a no-op run leaves
  # the mtime alone.
  #
  # Tracked separately from the added/updated/removed tallies below, which are
  # keyed on file NAME and checksum and so cannot see a pure reordering.
  wrote <- !identical(rewritten, lines)
  if (wrote) write_lines_utf8(rewritten, MANIFEST)

  # Read back rather than trusting the write, the same way bump() reports each
  # site from a fresh read.
  after <- read_files_block(read_lines_utf8(MANIFEST))

  added <- setdiff(names(after), names(before))
  removed <- setdiff(names(before), names(after))
  changed <- Filter(
    function(f) !identical(before[[f]], after[[f]]),
    intersect(names(after), names(before))
  )

  for (f in sort(names(after), method = "radix")) {
    mark <- if (f %in% added) {
      "added"
    } else if (f %in% changed) {
      paste0("checksum updated (was ", before[[f]], ")")
    } else {
      "unchanged"
    }
    cat(sprintf("  %-34s %s\n", f, mark))
  }
  for (f in sort(removed, method = "radix")) {
    cat(sprintf("  %-34s removed (was %s)\n", f, before[[f]]))
  }

  n <- length(added) + length(changed) + length(removed)
  if (!wrote) {
    cat("\nAlready in sync -- nothing written.\n")
  } else if (n == 0L) {
    cat("\nRewrote the block (entry order only). Review with `git diff`.\n")
  } else {
    cat(sprintf(
      "\nDone. %d added, %d updated, %d removed. Review with `git diff`.\n",
      length(added), length(changed), length(removed)
    ))
  }
  invisible(n)
}

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

check <- function() {
  v <- description_version()
  cat("Version sync check (DESCRIPTION is the source of truth)\n\n")
  cat(sprintf("  %-34s %s\n", "DESCRIPTION Version", v))
  cat(sprintf("  %-34s %s\n\n", strrep("-", 34), strrep("-", 12)))

  drift <- character(0)
  for (site in sites[-1L]) {
    want <- v
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
    cat("  Rscript .github/scripts/bump_version.R <version>            rewrite every version site\n")
    cat("  Rscript .github/scripts/bump_version.R --check               report drift, write nothing\n")
    cat("  Rscript .github/scripts/bump_version.R --sync-manifest       rebuild the Connect manifest's\n")
    cat("                                                              file list and checksums\n")
    quit(status = if (length(args) == 0L) 1 else 0)
  }

  if (identical(args[[1L]], "--check")) {
    check()
    return(invisible(NULL))
  }

  # Independent of a version bump: the checksum goes stale whenever app.R
  # changes.
  if (identical(args[[1L]], "--sync-manifest")) {
    sync_manifest()
    return(invisible(NULL))
  }

  v <- trimws(args[[1L]])
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?$", v)) {
    fail("'%s' is not a valid version. Expected x.y.z or x.y.z.w (no leading 'v').", v)
  }
  bump(v)
}

main()
