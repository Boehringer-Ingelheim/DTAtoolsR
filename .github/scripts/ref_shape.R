#!/usr/bin/env Rscript

# What shape is the app manifest's deploy ref?
#
# The DTAtools entry in inst/shiny/dta_app/manifest.json carries a
# RemoteRef/GithubRef naming what Connect installs the package from. Two shapes
# exist, and they behave differently everywhere:
#
#   release ref  `v1.2.3` -- an immutable tag. Tracks DESCRIPTION, so a version
#                bump moves it and invalidates the recorded SHA.
#   branch ref   `dev` -- a moving branch. Does NOT track the version: a bump
#                must leave both it and its SHA alone.
#
# This predicate lives in its own file because BOTH bump_version.R (which
# decides what to rewrite) and check_manifest.R (which decides what to demand)
# have to agree about it, and they are separate executables -- check_manifest.R
# cannot source bump_version.R without running its main(). Defining it twice is
# what the repo already refuses to do for the version sites themselves (see the
# header of check_version_sync.R): two implementations of one rule are two
# things that can disagree. Here the cost of disagreement is specifically that a
# branch gets treated as a release or vice versa -- i.e. exactly the
# conflation that left `dev` pinned to a `v0.20.0` tag nobody ever cut.
#
# Anchored and fully-qualified deliberately. A looser `^v[0-9]` would classify a
# branch legitimately named `v2-migration` or `v3-rewrite` as a release ref,
# silently opting it into tag semantics: a bump would rewrite the branch name to
# a version string and clear a SHA that was still valid. The pattern matches the
# same x.y.z / x.y.z.w that bump_version.R's main() accepts as a version
# argument, so "is a release ref" and "is a version this script can bump to"
# cannot drift apart.
is_release_ref <- function(ref) {
  grepl("^v[0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?$", ref)
}
