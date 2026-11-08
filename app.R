# Posit Connect entry point. Connect deploys the repository root, so this
# checkout's package source ships in the same bundle as the app. Installing it
# here means the app and DTAtools always come from the same commit; a
# DTAtools installed as a separate manifest dependency could not guarantee that.
#
# The install takes 45-60 s -- at Connect's default 60 s initial timeout, so
# raise that for this content -- and is cached in the bundle directory (writable
# for Shiny content on Connect, and new for every deploy) under a key hashed
# from the package source: only the first worker after a deploy pays for it,
# and a local checkout never reuses an install of different sources.

cache <- ".dtatools-lib"
src <- c(
  "DESCRIPTION", "NAMESPACE",
  list.files(c("R", "inst", "src", "data"), recursive = TRUE, full.names = TRUE)
)
digest_file <- tempfile()
writeLines(paste(src, tools::md5sum(src)), digest_file)
lib <- file.path(cache, unname(tools::md5sum(digest_file)))

if (!dir.exists(file.path(lib, "DTAtools"))) {
  # Install into a staging directory and rename it into place: the rename is
  # atomic, so a worker starting concurrently either sees a complete install or
  # none. Losing the race just leaves the winner's copy in place.
  staging <- tempfile("staging-", tmpdir = cache)
  if (!dir.create(staging, recursive = TRUE)) {
    cli::cli_abort("Cannot create {.path {staging}}; the app needs a writable {.path {cache}} in {.path {getwd()}}.")
  }
  utils::install.packages(
    ".",
    lib = staging, repos = NULL, type = "source",
    INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-test-load")
  )
  if (!dir.exists(file.path(staging, "DTAtools"))) {
    # Never let a failed install into the cache: it would fail every later
    # worker of this deploy. Fall through to the abort below.
    unlink(staging, recursive = TRUE)
  } else if (!file.rename(staging, lib)) {
    # Lost the race. Use the winner's copy if complete, else our own.
    if (dir.exists(file.path(lib, "DTAtools"))) {
      unlink(staging, recursive = TRUE)
    } else {
      lib <- staging
    }
  }
}
.libPaths(c(lib, .libPaths()))

# install.packages() only warns when an install fails, which would silently
# fall through to whatever DTAtools the server has installed.
app_dir <- system.file("shiny", "dta_app", package = "DTAtools", lib.loc = lib)
if (!nzchar(app_dir)) {
  cli::cli_abort("Installing DTAtools from {.path {getwd()}} failed; see the log above.")
}

# .Rprofile turned Shiny's R/ autoloading off so the package source at the
# root is not sourced loose; the app's own R/ helpers do need it.
options(shiny.autoload.r = TRUE)
shiny::shinyAppDir(app_dir)
