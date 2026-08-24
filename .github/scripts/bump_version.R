#!/usr/bin/env Rscript

# Single source of truth for the package version across every file that carries
# it. Run from the repository root:
#
#   Rscript .github/scripts/bump_version.R 0.17.4              # rewrite every site
#   Rscript .github/scripts/bump_version.R --check              # report drift, write nothing
#   Rscript .github/scripts/bump_version.R --sync-manifest      # rebuild the app manifest's
#                                                               # file list and checksums
#   Rscript .github/scripts/bump_version.R --set-deploy-sha <sha>
#                                                               # pin RemoteSha/GithubSHA1 to the
#                                                               # commit being deployed
#   Rscript .github/scripts/bump_version.R --set-deploy-ref <ref>
#                                                               # point RemoteRef/GithubRef at a
#                                                               # tag or a branch
#
# `--check` is what CI runs (see check_version_sync.R); it exits non-zero on any
# mismatch. The two modes share the site definitions below, so the checker can
# never fall out of step with the writer.
#
# Four files, eight places. Only DESCRIPTION <-> VERSION used to be checked,
# which is exactly why manifest.json and docs/index.html had both silently
# drifted behind by the time this script was written.
#
# `--sync-manifest` is a separate axis, not part of a version bump: the app
# manifest's per-file checksums go stale whenever the app SOURCE changes, which
# happens far more often than a release. It is wired into
# .github/workflows/manifest-sync.yml, which pushes the repair to the PR branch
# so nobody has to copy an md5 by hand -- doing that by hand is what put a wrong
# app.R checksum into release 0.16.0. The structural half of the guarantee (the
# file list, and the fields that must NOT be present) lives in
# .github/scripts/check_manifest.R.
#
# `--set-deploy-sha` is a third axis: it pins RemoteSha/GithubSHA1, the commit
# RemoteRef/GithubRef resolve to. Posit Connect builds the package's archive
# download URL from that SHA, so a manifest without one makes every deploy fail
# with "argument is of length zero" -- not eventually, immediately.
#
# BOTH long-lived branches are deployed to a Connect instance: `master` (at a
# release tag) and `dev` (at the branch tip). So the SHA is not a release-only
# concern, and the ref is not always a tag. See is_release_ref() for the
# distinction that follows from that, and which of the two shapes each mode
# applies to.
#
# `--set-deploy-sha` is not part of a version bump -- a bump commit cannot know
# the SHA of a release commit that does not exist yet. It runs afterwards:
# .github/workflows/manifest-release-sha.yml on `release: published` for master,
# .github/workflows/manifest-dev-sha.yml on `push: dev` for dev.
# `--set-deploy-ref` is rarer still -- it moves the ref itself between a tag and
# a branch, which happens when a branch changes what it deploys, not per release.
# `--set-release-sha` remains accepted as an alias for `--set-deploy-sha`.
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

# is_release_ref(), shared with check_manifest.R so the writer and the checker
# cannot disagree about which refs are tags -- see that file's header. Sourced
# rather than duplicated, and relative to the repository root like every other
# path here.
REF_SHAPE <- ".github/scripts/ref_shape.R"
if (!file.exists(REF_SHAPE)) {
  fail("cannot find %s (run this from the repository root).", REF_SHAPE)
}
source(REF_SHAPE)

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

# manifest.json: a named string field inside the DTAtools entry of `packages`.
# Scoped to the lines after the `"DTAtools": {` key so the ~90 other entries (one
# per renv package) cannot be hit by accident -- every one of them has a
# "Version", and several also carry a "GithubRef".
#
# `required = FALSE` returns NA instead of failing when the field is absent --
# used for RemoteSha/GithubSHA1, which are legitimately missing between a
# version bump and the next release (see manifest_ref_site()).
manifest_pkg_field_line <- function(lines, field, required = TRUE) {
  anchor <- sole_match(lines, '^\\s*"DTAtools": \\{\\s*$', "DTAtools package entry in the manifest")

  # Bound the search at the END of the DTAtools entry -- the `    },` that closes
  # it, at the 4-space indent rsconnect uses for package keys. Searching to the
  # end of the file instead would mean a field MISSING from the DTAtools entry
  # silently resolves to the same field in some LATER package's entry, and the
  # bump would then patch a CRAN package's line. `Version` is present on all ~90
  # entries and `GithubRef` on several, so this is not hypothetical -- it is only
  # the anchoring that keeps them apart.
  tail_lines <- lines[seq(anchor + 1L, length(lines))]
  rel_end <- grep("^    \\},?\\s*$", tail_lines)
  if (length(rel_end) == 0L) {
    fail("could not find the end of the manifest's DTAtools entry; the file's structure changed.")
  }
  entry <- lines[seq(anchor, anchor + rel_end[1L])]

  rel <- grep(sprintf('^\\s*"%s": ".*",?\\s*$', field), entry)
  if (length(rel) == 0L) {
    if (!required) return(NA_integer_)
    fail("no \"%s\" line found under the manifest's DTAtools entry.", field)
  }
  anchor + rel[1L] - 1L
}

manifest_pkg_version_line <- function(lines) manifest_pkg_field_line(lines, "Version")

# Insert `"field": "value",` immediately after `after_field`'s line, matching
# its indentation -- or overwrite `field`'s value in place if it is already
# present, so this is safe to call whether or not a previous run (or the
# hand-patch it replaces) already added the line. `after_field` is assumed
# present (RemoteRef/GithubRef always are; they are version-bump sites).
manifest_set_field_after <- function(lines, field, value, after_field) {
  existing <- manifest_pkg_field_line(lines, field, required = FALSE)
  if (!is.na(existing)) {
    lines[existing] <- sub(
      sprintf('"%s": "[^"]*"', field), sprintf('"%s": "%s"', field, value), lines[existing]
    )
    return(lines)
  }
  after <- manifest_pkg_field_line(lines, after_field)
  indent <- sub('^(\\s*)".*$', "\\1", lines[after])
  append(lines, sprintf('%s"%s": "%s",', indent, field, value), after = after)
}

# Delete `field`'s line from the DTAtools entry if present; a no-op if it
# isn't. Used when a version bump moves RemoteRef/GithubRef forward -- see
# manifest_ref_site().
manifest_clear_field <- function(lines, field) {
  i <- manifest_pkg_field_line(lines, field, required = FALSE)
  if (is.na(i)) return(lines)
  lines[-i]
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

# manifest.json: `RemoteRef` and `GithubRef` in the DTAtools entry. These record
# the same fact as `Version`, spelled `v<version>`, and until this was added only
# `Version` was maintained -- so the file shipped 0.17.3 with `RemoteRef` still
# reading `v0.17.2`, and shipped 0.18.0 with hand-added Github/RemoteSha fields
# pointing at the 0.17.3 release commit. Making them sites means the writer and
# the checker cannot disagree about them, which is the whole point of this file.
#
# The sibling `GithubSHA1`/`RemoteSha` fields are NOT bump-version sites: a bump
# commit cannot know the SHA of the release commit that will eventually contain
# it, so there is nothing to write here. For a RELEASE ref, `write()` CLEARS them
# if present -- moving the ref forward makes any existing SHA stale by
# definition, since it names the commit the *previous* tag resolved to, not this
# one. They are repopulated once the new tag exists, by `--set-deploy-sha`.
#
# A ref is a RELEASE ref when it is tag-shaped (`v1.2.3`); anything else -- in
# practice the branch name `dev` -- is a BRANCH ref. The two behave differently
# under a bump, and conflating them is what broke the dev deploy:
#
#   release ref  tracks DESCRIPTION. A bump moves it to v<version>, which
#                invalidates any recorded SHA -> rewrite the ref, clear the SHA.
#   branch ref   does not track the version at all. A bump must leave BOTH the
#                ref and the SHA alone: the branch still points where it did,
#                and its SHA still names a real commit on it.
#
# Before this distinction existed, bumping on `dev` rewrote its deploy ref to
# `v0.20.0` -- a tag that was never cut -- and cleared a SHA that was still
# perfectly valid. The result was a manifest naming a nonexistent tag with no
# SHA at all, i.e. a `dev` Connect instance that could not deploy at all. Note
# that clearing is only "fail loudly rather than deploy the wrong code" when
# somebody is going to SEE the failure; for a branch that is deployed
# continuously it is just breakage. check_manifest.R now requires the SHA to be
# present precisely so that neither shape can go out the door without one.
#
# is_release_ref() itself lives in .github/scripts/ref_shape.R, sourced above.
manifest_ref_site <- function(field) {
  sha_field <- c(RemoteRef = "RemoteSha", GithubRef = "GithubSHA1")[[field]]
  read_ref <- function() {
    lines <- read_lines_utf8(MANIFEST)
    extract(lines[manifest_pkg_field_line(lines, field)], '^\\s*"[^"]*": "([^"]*)".*$')
  }
  list(
    name = paste0("manifest.json DTAtools ", field),
    # A branch ref is its own expectation -- it is not derived from the version,
    # so there is no drift for `--check` to report. Whether it actually resolves
    # to the recorded SHA is check_manifest.R's job: that needs git, which this
    # script deliberately does without (see the header's note on why it has no
    # library to restore).
    expected = function(v) {
      cur <- read_ref()
      if (is_release_ref(cur)) paste0("v", v) else cur
    },
    read = read_ref,
    write = function(v) {
      lines <- read_lines_utf8(MANIFEST)
      i <- manifest_pkg_field_line(lines, field)
      if (!is_release_ref(extract(lines[i], '^\\s*"[^"]*": "([^"]*)".*$'))) {
        return(invisible(NULL))
      }
      lines[i] <- sub('": "[^"]*"', sprintf('": "v%s"', v), lines[i])
      had_sha <- !is.na(manifest_pkg_field_line(lines, sha_field, required = FALSE))
      lines <- manifest_clear_field(lines, sha_field)
      if (had_sha) {
        cat(sprintf(
          "  %-34s cleared (pinned the previous release)\n",
          paste0("manifest.json DTAtools ", sha_field)
        ))
      }
      write_lines_utf8(lines, MANIFEST)
    }
  )
}

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
  manifest_ref_site("RemoteRef"),
  manifest_ref_site("GithubRef"),
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

# --- the manifest's deploy SHA fields -----------------------------------------
#
# RemoteSha/GithubSHA1 record the commit RemoteRef/GithubRef resolve to, and
# Connect builds the package's archive URL from them. Not a bump-version site
# (see manifest_ref_site()) -- this runs afterwards, from
# manifest-release-sha.yml on master and manifest-dev-sha.yml on dev.
#
# The guard is that both refs agree, plus -- for a release ref only -- that they
# read `v<DESCRIPTION Version>`, which makes "the tag exists" and "DESCRIPTION
# says this version" the same fact so a SHA cannot be attached to the wrong tag.
# A branch ref has no version to compare against, so the useful check is instead
# "does this ref really resolve to this SHA" -- and that needs git. It lives in
# check_manifest.R, which already shells out to git and runs with fetch-depth: 0;
# putting it here would give this script a git dependency it does not otherwise
# have, for a check that would then run in two places and be able to disagree.
read_ref_field <- function(lines, field) {
  extract(lines[manifest_pkg_field_line(lines, field)], '^\\s*"[^"]*": "([^"]*)".*$')
}

set_deploy_sha <- function(sha) {
  if (!grepl("^[0-9a-f]{40}$", sha)) {
    fail("'%s' is not a 40-character git commit SHA.", sha)
  }

  lines <- read_lines_utf8(MANIFEST)
  remote_ref <- read_ref_field(lines, "RemoteRef")
  github_ref <- read_ref_field(lines, "GithubRef")
  if (!identical(remote_ref, github_ref)) {
    fail(
      "manifest.json's RemoteRef (\"%s\") and GithubRef (\"%s\") disagree; fix that before pinning a SHA.",
      remote_ref, github_ref
    )
  }

  if (is_release_ref(remote_ref)) {
    want_ref <- paste0("v", description_version())
    if (!identical(remote_ref, want_ref)) {
      fail(
        "manifest.json's ref is \"%s\", not \"%s\" (DESCRIPTION Version). Bump the version first.",
        remote_ref, want_ref
      )
    }
  }

  lines <- manifest_set_field_after(lines, "RemoteSha", sha, "RemoteRef")
  lines <- manifest_set_field_after(lines, "GithubSHA1", sha, "GithubRef")
  write_lines_utf8(lines, MANIFEST)

  cat(sprintf("RemoteSha/GithubSHA1 now point at %s (%s).\n", sha, remote_ref))
  invisible(TRUE)
}

# Move RemoteRef/GithubRef between a tag and a branch. Rare -- it changes WHAT a
# branch deploys, not which commit of it -- and used here once, to replace dev's
# never-cut `v0.20.0` tag with the `dev` branch it is actually deployed from.
#
# Clears any recorded SHA: it describes the ref being moved away from, so it
# cannot survive the move. --set-deploy-sha is what repopulates it.
set_deploy_ref <- function(ref) {
  if (!nzchar(ref) || grepl('["\\\\]', ref)) {
    fail("'%s' is not a usable git ref.", ref)
  }

  lines <- read_lines_utf8(MANIFEST)
  for (field in c("RemoteRef", "GithubRef")) {
    i <- manifest_pkg_field_line(lines, field)
    lines[i] <- sub('": "[^"]*"', sprintf('": "%s"', ref), lines[i])
  }
  cleared <- character(0)
  for (sha_field in c("RemoteSha", "GithubSHA1")) {
    if (!is.na(manifest_pkg_field_line(lines, sha_field, required = FALSE))) {
      cleared <- c(cleared, sha_field)
      lines <- manifest_clear_field(lines, sha_field)
    }
  }
  write_lines_utf8(lines, MANIFEST)

  cat(sprintf("RemoteRef/GithubRef now read \"%s\".\n", ref))
  if (length(cleared) > 0L) {
    cat(sprintf("  cleared %s -- it described the previous ref.\n", paste(cleared, collapse = "/")))
  }
  cat("Pin the new one with --set-deploy-sha <sha>.\n")
  invisible(TRUE)
}

# --- the manifest's `files` block --------------------------------------------
#
# Unlike `packages` -- a frozen snapshot of one machine's renv library, which
# must never be touched (see the header comment) -- the `files` block is
# entirely DERIVED from the app directory: one entry per file, each carrying
# that file's md5. So it is regenerated wholesale rather than patched.
#
# Regenerating is both simpler than a line patch and the only way to handle a
# file being added or removed, which a per-line patch cannot see at all. Until
# this existed only the VERSION checksum was maintained, so the six other
# entries went stale whenever the app changed: release 0.16.0 shipped with
# app.R recorded as df2b7079... while the file on disk was 9d798811....

APP_DIR <- "inst/shiny/dta_app"

# Every file in the bundle except the manifest itself.
#
# `method = "radix"` is load-bearing, not a micro-optimisation: it sorts by byte
# value, ignoring the collation locale. Plain sort() is locale-dependent -- under
# German_Germany.utf8 it collates case-insensitively and yields
# app.R, R/..., VERSION, www/...; under the C collation CI runs with it yields
# R/..., VERSION, app.R, www/... (which is what rsconnect wrote here). Using
# sort() would therefore reorder the block one way on a developer machine and
# back again in CI -- a manifest that flips on every hop between machines, i.e.
# precisely the drift this script exists to stop.
app_bundle_files <- function() {
  found <- list.files(APP_DIR, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  sort(setdiff(found, "manifest.json"), method = "radix")
}

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
    md5 <- unname(tools::md5sum(file.path(APP_DIR, f)))
    if (is.na(md5)) fail("could not compute an md5 for %s.", file.path(APP_DIR, f))
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
  if (length(files) == 0L) fail("no files found under %s.", APP_DIR)

  span <- files_block_span(lines)
  # `before`/`after` are carried over verbatim, so the `packages` snapshot above
  # the block and the `users` key below it cannot be disturbed by the splice.
  rewritten <- c(
    lines[seq_len(span[["start"]] - 1L)],
    render_files_block(files),
    lines[seq(span[["end"]], length(lines))]
  )

  # Validate the rendered result BEFORE it reaches the disk. Writing first and
  # aborting afterwards would leave the bad content in the working tree, and the
  # manifest-sync workflow commits whatever it finds there -- so a failed run
  # would push exactly the file this check exists to reject.
  staged <- read_files_block(rewritten)
  if (!identical(sort(names(staged), method = "radix"), files)) {
    fail("the rewritten \"files\" block does not list the app directory; nothing written.")
  }

  # Only touch the file when the bytes actually differ. A no-op write would
  # still update the mtime, and the auto-commit workflow decides whether to
  # push by asking git whether anything changed.
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
    cat("  Rscript .github/scripts/bump_version.R <version>            rewrite every version site\n")
    cat("  Rscript .github/scripts/bump_version.R --check               report drift, write nothing\n")
    cat("  Rscript .github/scripts/bump_version.R --sync-manifest       rebuild the app manifest's\n")
    cat("                                                              file list and checksums\n")
    cat("  Rscript .github/scripts/bump_version.R --set-deploy-sha <sha>\n")
    cat("                                                              pin RemoteSha/GithubSHA1 to the\n")
    cat("                                                              commit being deployed\n")
    cat("  Rscript .github/scripts/bump_version.R --set-deploy-ref <ref>\n")
    cat("                                                              point RemoteRef/GithubRef at a\n")
    cat("                                                              tag or a branch\n")
    quit(status = if (length(args) == 0L) 1 else 0)
  }

  if (identical(args[[1L]], "--check")) {
    check()
    return(invisible(NULL))
  }

  # Independent of a version bump: the checksums go stale whenever the app
  # source changes, which happens far more often than a release.
  if (identical(args[[1L]], "--sync-manifest")) {
    sync_manifest()
    return(invisible(NULL))
  }

  # --set-release-sha kept as an alias: it is what the manifest-release-sha.yml
  # already on master invokes, and that workflow must keep working through the
  # window where master has the old copy and dev the new one.
  if (args[[1L]] %in% c("--set-deploy-sha", "--set-release-sha")) {
    if (length(args) < 2L) fail("%s requires a commit SHA argument.", args[[1L]])
    set_deploy_sha(trimws(args[[2L]]))
    return(invisible(NULL))
  }

  if (identical(args[[1L]], "--set-deploy-ref")) {
    if (length(args) < 2L) fail("--set-deploy-ref requires a git ref argument.")
    set_deploy_ref(trimws(args[[2L]]))
    return(invisible(NULL))
  }

  v <- trimws(args[[1L]])
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?$", v)) {
    fail("'%s' is not a valid version. Expected x.y.z or x.y.z.w (no leading 'v').", v)
  }
  bump(v)
}

main()
