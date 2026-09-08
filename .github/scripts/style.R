#!/usr/bin/env Rscript
# The single definition of "styled" for this project. Both the developer and
# the r-style workflow run THIS file, so the two cannot drift apart:
#
#   Rscript .github/scripts/style.R            # restyle in place, then force LF
#   Rscript .github/scripts/style.R --check    # list what is unstyled, exit 1
#
# Two things live here that a bare `styler::style_pkg()` does not do.
#
# 1. inst/ is styled too. style_pkg() visits .R under R/, tests/, data-raw/
#    and demo/, plus the root .Rprofile and vignettes/ -- 121 files here -- and
#    it never descends into inst/. That is where this project's entire Shiny
#    app and template engine live: 15 files, ~17k lines, more R code than R/
#    itself. Without the second call all of it is invisible to the check,
#    which is exactly how an unstyled
#    `for (eng in engines) if (...) {` survived in utils_dta.R until someone
#    happened to run style_file() by hand. style_dir() takes the same
#    arguments and the same tidyverse_style, so the two calls cannot enforce
#    different things.
#
# 2. Line endings are forced back to LF afterwards. styler's write path is
#    `writeLines(text, con)` where `con` is a path, so R opens it in TEXT mode
#    -- and on Windows text mode translates \n to \r\n. Every file styler
#    actually rewrites therefore comes back CRLF, which the `mixed-line-ending`
#    pre-commit hook then reverts, failing the commit and forcing a re-stage.
#    Styling a file and un-CRLF-ing it are not two jobs a contributor should
#    have to remember to do in order, so this script does both.
#
# --check does NOT normalise: it must not write. It is the CI mode, and CI runs
# on Linux where styler emits LF in the first place. A CRLF file that reaches
# the repository some other way is caught by `mixed-line-ending` in the
# pre-commit workflow, which is the authority on that and stays so.
#
# --check uses styler's dry = "on" (report, do not write) rather than
# dry = "fail" (abort on the first offender). Same verdict, three advantages:
# every unstyled file is named instead of just the first, inst/ is still
# reached when R/ is already dirty, and a real failure -- styler not installed,
# say -- surfaces as its own error instead of being mistaken for a style
# violation.

args <- commandArgs(trailingOnly = TRUE)
unknown <- setdiff(args, "--check")
if (length(unknown) > 0L) {
  cat("style.R: unknown argument(s):", paste(unknown, collapse = ", "), "\n")
  cat("Usage: Rscript .github/scripts/style.R [--check]\n")
  quit(status = 1)
}
check_only <- "--check" %in% args

if (!file.exists("DESCRIPTION")) {
  cat("style.R: no DESCRIPTION in", getwd(), "\n")
  cat("Run this from the repository root.\n")
  quit(status = 1)
}

# Rewrite `path` with LF line endings, returning TRUE if it needed changing.
#
# writeBin() is the point: it is the one write path R does NOT put through the
# Windows text-mode translation that caused the problem in the first place.
#
# Both CRLF and a lone CR become LF, which is precisely what
# `mixed-line-ending --fix=lf` does. Matching it exactly is deliberate -- if
# this script left behind something that hook would still "fix", the commit
# would be rejected anyway and the whole exercise would be pointless.
normalise_eol <- function(path) {
  size <- file.size(path)
  if (is.na(size) || size == 0) {
    return(FALSE)
  }
  bytes <- readBin(path, "raw", size)
  if (!any(bytes == as.raw(13L))) {
    return(FALSE)
  }
  # Drop the CR of every CRLF pair, then promote any surviving lone CR to LF.
  next_byte <- c(bytes[-1L], as.raw(0L))
  bytes <- bytes[!(bytes == as.raw(13L) & next_byte == as.raw(10L))]
  bytes[bytes == as.raw(13L)] <- as.raw(10L)
  writeBin(bytes, path)
  TRUE
}

dry <- if (check_only) "on" else "off"

# style_dir() runs `withr::with_dir(path, ...)`, so the paths it reports back
# are relative to the directory it styled, not to the repository root:
# inst/shiny/dta_app/R/theme.R comes back as shiny/dta_app/R/theme.R. Left
# uncorrected that is silent, not loud -- the normalise step below would find
# no such file, do nothing, and report success while every file under inst/
# kept its CRLF. Hence the prefix, and hence the existence guard after it.
rooted <- function(result, dir = NULL) {
  out <- as.data.frame(result)
  if (!is.null(dir) && nrow(out) > 0L) {
    out$file <- file.path(dir, out$file)
  }
  out
}

visited <- rbind(
  rooted(styler::style_pkg(
    style = styler::tidyverse_style,
    scope = "tokens",
    dry = dry
  )),
  rooted(styler::style_dir(
    "inst",
    style = styler::tidyverse_style,
    scope = "tokens",
    dry = dry,
    recursive = TRUE
  ), "inst")
)

missing <- visited$file[!file.exists(visited$file)]
if (length(missing) > 0L) {
  cat(
    "style.R: styler reported", length(missing),
    "file(s) it cannot locate from the repository root, e.g.",
    missing[1L], "\n"
  )
  cat("styler's path convention has changed; fix rooted() above.\n")
  quit(status = 1)
}

if (check_only) {
  # NA means styler could not process the file at all; it has already warned
  # about it by name. Not treated as a style violation, because dry = "fail"
  # did not treat it as one either and this change is not the place to widen
  # what the check rejects.
  unstyled <- visited$file[which(visited$changed)]
  if (length(unstyled) > 0L) {
    cat("\n")
    cat(sprintf("::error file=%s::Not styled\n", unstyled), sep = "")
    cat(
      "::error::", length(unstyled), " file(s) are not styled. Run",
      " 'Rscript .github/scripts/style.R' locally and commit the result.\n",
      sep = ""
    )
    quit(status = 1L)
  }
  cat(sprintf("styler: %d file(s) checked, all styled\n", nrow(visited)))
} else {
  # Normalise every VISITED file, not just the ones styler reports as changed.
  # A file that is already correctly styled but happens to hold CRLF is never
  # rewritten by styler -- it compares the parsed text, which carries no line
  # endings, finds it identical and skips the write -- so a `changed`-only
  # filter would walk straight past exactly the file that needs fixing.
  normalised <- vapply(visited$file, normalise_eol, logical(1L), USE.NAMES = FALSE)
  cat(sprintf(
    "styler: %d file(s) visited, %d restyled, %d line ending(s) normalised to LF\n",
    nrow(visited), sum(visited$changed, na.rm = TRUE), sum(normalised)
  ))
}
