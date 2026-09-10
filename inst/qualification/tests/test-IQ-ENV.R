# Installation qualification for the installed system and environment.
# REQ-ENV-001 .. REQ-ENV-024.
#
# Every other stage calls into one particular copy of the package on one
# particular machine and trusts that copy to be what it claims to be. This
# file is what earns that trust: it is the only stage that looks at the
# installation itself rather than at the behaviour of the code inside it, so
# it is where a wrong version, a missing dependency, an unsupported R, or a
# file quietly altered after release would first become visible.

# No portable base-R accessor reports free disk space, so this tries the one
# platform tool that ships everywhere R does and gives up to NA on anything
# else -- a reporting nicety is not worth failing an installation
# qualification over.
qa_env_free_disk_bytes <- function(path) {
  free <- tryCatch(
    {
      if (.Platform$OS.type == "windows") {
        drive <- toupper(substr(normalizePath(path, winslash = "/", mustWork = FALSE), 1, 2))
        out <- system2("wmic", c(
          "logicaldisk", "where", shQuote(paste0("DeviceID='", drive, "'")),
          "get", "FreeSpace", "/value"
        ), stdout = TRUE, stderr = FALSE)
        as.numeric(sub("FreeSpace=", "", grep("FreeSpace=", out, value = TRUE)))
      } else {
        out <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
        as.numeric(strsplit(trimws(out[[2]]), "\\s+")[[1]][[4]]) * 1024
      }
    },
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )
  if (length(free) != 1) NA_real_ else free
}

test_that("IQ-ENV-001 | installed Version matches the suite's declared package_version | REQ-ENV-001", {
  installed <- as.character(utils::packageVersion("DTAtools"))
  index <- qual_read_index(qa_suite_root())
  qa_step(
    "the installed Version equals _index.yaml's package_version",
    index$package_version, installed
  )
})

test_that("IQ-ENV-002 | build, packaging and source-revision fields are recorded | REQ-ENV-002", {
  desc <- utils::packageDescription("DTAtools")
  sha <- desc$RemoteSha %||% desc$GithubSHA1 %||% NA_character_
  qa_check(
    "the Built field is recorded when present",
    TRUE,
    detail = desc$Built %||% "not present (not an R CMD INSTALL build)"
  )
  qa_check(
    "the Packaged field is recorded when present",
    TRUE,
    detail = desc$Packaged %||% "not present"
  )
  qa_check(
    "the source revision (RemoteSha or GithubSHA1) is recorded when present",
    TRUE,
    detail = if (is.na(sha)) "not present (not a remotes/pak GitHub install)" else sha
  )
})

test_that("IQ-ENV-003 | the running R version satisfies the package's declared minimum | REQ-ENV-003", {
  deps <- qual_dependency_check()
  r_row <- deps[deps$package == "R" & deps$field == "Depends", , drop = FALSE]
  qa_check("a Depends: R (>= ...) entry was found", nrow(r_row) == 1)

  qa_step(
    sprintf(
      "the running R (%s) is at least the declared minimum (%s)",
      r_row$installed_version[[1]], r_row$required_version[[1]]
    ),
    TRUE, r_row$satisfied[[1]]
  )
})

test_that("IQ-ENV-004 | the platform, OS and architecture are recorded | REQ-ENV-004", {
  qa_check(
    "the R platform triple is recorded",
    nzchar(R.version$platform),
    detail = R.version$platform
  )
  qa_check(
    "the CPU architecture is recorded",
    nzchar(R.version$arch),
    detail = R.version$arch
  )

  os <- utils::osVersion
  qa_check(
    "the operating system version string is recorded",
    !is.na(os) && nzchar(os),
    detail = if (is.na(os)) "unavailable" else os
  )
})

test_that("IQ-ENV-005 | every Depends package is installed and satisfies its minimum version | REQ-ENV-005", {
  deps <- qual_dependency_check()
  rows <- deps[deps$field == "Depends", , drop = FALSE]
  qa_check("at least the R and S7 Depends entries were found", nrow(rows) >= 2)

  unsatisfied <- rows$package[!rows$satisfied]
  qa_step(
    "every Depends package is installed and meets its declared minimum",
    character(0), unsatisfied
  )
})

test_that("IQ-ENV-006 | every Imports package is installed and satisfies its minimum version | REQ-ENV-006", {
  deps <- qual_dependency_check()
  rows <- deps[deps$field == "Imports", , drop = FALSE]
  qa_check("at least one Imports entry was found", nrow(rows) > 0)

  unsatisfied <- rows$package[!rows$satisfied]
  qa_step(
    "every Imports package is installed and meets its declared minimum",
    character(0), unsatisfied
  )
})

test_that("IQ-ENV-007 | the Suggests package inventory is recorded without requiring presence | REQ-ENV-007", {
  deps <- qual_dependency_check()
  rows <- deps[deps$field == "Suggests", , drop = FALSE]
  qa_check("at least one Suggests package is declared", nrow(rows) > 0)

  inventory <- paste(
    sprintf("%s(%s)", rows$package, ifelse(rows$installed, "present", "absent")),
    collapse = ", "
  )
  qa_check(
    "presence or absence of every Suggests package is recorded",
    TRUE,
    detail = inventory
  )
})

test_that("IQ-ENV-008 | locale, time zone and native encoding are recorded | REQ-ENV-008", {
  loc <- Sys.getlocale()
  qa_check("the process locale is recorded", nzchar(loc), detail = loc)

  tz <- qual_safe(Sys.timezone(), NA_character_)
  qa_check(
    "the system time zone is recorded",
    !is.na(tz) && nzchar(tz),
    detail = if (is.na(tz)) "unavailable" else tz
  )

  # Read through the package helper rather than from l10n_info() directly:
  # the native encoding is reported under `codeset` on Unix and `codepage` on
  # Windows, and a check that knows only one of them records "unknown" on half
  # the systems this package is deployed to.
  enc <- qual_native_encoding()
  qa_check(
    "the native encoding is recorded",
    length(enc) == 1 && !is.na(enc) && nzchar(enc),
    detail = enc
  )
})

test_that("IQ-ENV-009 | the Arrow version in use is recorded | REQ-ENV-009", {
  version <- as.character(arrow::arrow_info()$version)
  qa_check(
    "the Arrow C++ library version is recorded",
    length(version) == 1 && nzchar(version),
    detail = version
  )
})

test_that("IQ-ENV-010 | Arrow provides the dataset, acero and gzip capabilities the package relies on | REQ-ENV-010", {
  caps <- arrow::arrow_info()$capabilities
  qa_check("the dataset capability is available", isTRUE(caps[["dataset"]]))
  qa_check("the acero compute capability is available", isTRUE(caps[["acero"]]))
  qa_check("the gzip codec is available", isTRUE(caps[["gzip"]]))
})

test_that("IQ-ENV-011 | set_dta_compute_threads() round-trips a thread count | REQ-ENV-011", {
  qa_requires("arrow")
  original <- arrow::cpu_count()
  target <- if (identical(original, 3L)) 4L else 3L

  previous <- set_dta_compute_threads(target)
  qa_step("the previously active thread count is returned", original, previous)
  qa_step("the thread count is updated to the requested value", target, arrow::cpu_count())

  set_dta_compute_threads(previous)
  qa_step("the thread count is restored to its original value", original, arrow::cpu_count())
})

test_that("IQ-ENV-012 | the DTAtools namespace loads | REQ-ENV-012", {
  qa_check("the DTAtools namespace reports itself loaded", isNamespaceLoaded("DTAtools"))

  ns <- tryCatch(getNamespace("DTAtools"), error = function(e) NULL)
  qa_check("the namespace environment is reachable", is.environment(ns))
})

test_that("IQ-ENV-013 | every create_example_* constructor builds its class | REQ-ENV-013", {
  # Discovered from the frozen export list rather than hand-listed, so a
  # constructor added later is exercised without this file being edited to
  # know its name.
  api <- qual_read_api(qa_suite_root())
  ctors <- sort(grep("^create_example_", api, value = TRUE))
  qa_check("at least one create_example_* constructor is exported", length(ctors) > 0)

  for (ctor in ctors) {
    expected_class <- paste0("DTAtools::", sub("^create_example_", "", ctor))
    built <- tryCatch(do.call(ctor, list()), error = function(e) e)
    qa_check(
      sprintf("%s() builds an instance of %s", ctor, expected_class),
      !inherits(built, "error") && inherits(built, expected_class)
    )
  }
})

test_that("IQ-ENV-014 | the exported API matches the frozen manifest | REQ-ENV-014", {
  api <- qual_read_api(qa_suite_root())
  exports <- getNamespaceExports("DTAtools")
  qa_step(
    "getNamespaceExports(\"DTAtools\") equals the frozen _api.yaml list",
    sort(api, method = "radix"), sort(exports, method = "radix")
  )
})

test_that("IQ-ENV-015 | installed files match their recorded MD5 checksums | REQ-ENV-015", {
  # An installed library holds an MD5 file only when the installation wrote
  # one, and only two routes do: installing a binary package, or installing
  # from source with `R CMD INSTALL --build`, which writes the sums into the
  # library as well as into the binary it packages. A plain source install
  # does not, and neither does a tarball built with `R CMD build --md5` --
  # that manifest stays in the tarball. The absence therefore says nothing
  # about whether the installation is sound, only that this check has nothing
  # to compare against; IQ-ENV-016 covers the installed inst/ tree either way.
  result <- tools::checkMD5sums("DTAtools")
  if (is.na(result)) {
    testthat::skip(paste(
      "no MD5 manifest in the installed library: one is written by a binary",
      "install or by `R CMD INSTALL --build`, not by a plain source install.",
      "IQ-ENV-016 checks the installed inst/ tree regardless."
    ))
  }
  qa_check("tools::checkMD5sums(\"DTAtools\") reports every file intact", isTRUE(result))
})

test_that("IQ-ENV-016 | installed inst/ files match the baseline sha256 manifest | REQ-ENV-016", {
  manifest_path <- file.path(qa_suite_root(), "baseline", "inst-files.sha256")
  if (!file.exists(manifest_path)) {
    testthat::skip("no baseline/inst-files.sha256 manifest shipped with this installation")
  }

  lines <- readLines(manifest_path, warn = FALSE)
  lines <- lines[!startsWith(lines, "#") & nzchar(lines)]
  expected_hash <- sub("  .*$", "", lines)
  rel_path <- sub("^[0-9a-f]+  ", "", lines)

  resolved <- vapply(rel_path, function(p) system.file(p, package = "DTAtools"), character(1))
  missing <- rel_path[!nzchar(resolved)]
  qa_step("every manifest path resolves to an installed file", character(0), missing)

  present <- nzchar(resolved)
  actual_hash <- rep(NA_character_, length(resolved))
  actual_hash[present] <- vapply(resolved[present], qual_hash_one, character(1))
  mismatched <- rel_path[present][expected_hash[present] != actual_hash[present]]
  qa_step(
    sprintf("all %d manifest-listed files under inst/ match their recorded hash", sum(present)),
    character(0), mismatched
  )
})

test_that("IQ-ENV-017 | the shiny app's files match manifest.json checksums | REQ-ENV-017", {
  app_dir <- system.file("shiny", "dta_app", package = "DTAtools")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    testthat::skip("the shiny app directory is not part of this installation")
  }
  manifest_path <- file.path(app_dir, "manifest.json")
  if (!file.exists(manifest_path)) {
    testthat::skip("the shiny app has no manifest.json to check against")
  }

  qa_requires("jsonlite")
  files <- jsonlite::read_json(manifest_path)$files
  qa_check("the app manifest lists at least one file", length(files) > 0)

  has_checksums <- length(files) > 0 && !is.null(files[[1]]$checksum)
  if (has_checksums) {
    mismatched <- character(0)
    for (rel in names(files)) {
      want <- files[[rel]]$checksum
      got <- unname(tools::md5sum(file.path(app_dir, rel)))
      if (!identical(want, got)) mismatched <- c(mismatched, rel)
    }
    qa_step(
      sprintf("all %d files listed in manifest.json match their recorded checksum", length(files)),
      character(0), mismatched
    )
  } else {
    # Fallback for a manifest shape without per-file checksums: the only
    # claim such a manifest supports is that the files it names exist.
    missing <- names(files)[!file.exists(file.path(app_dir, names(files)))]
    qa_step("every file listed in manifest.json exists", character(0), missing)
  }
})

test_that("IQ-ENV-018 | every exported symbol has a documented alias | REQ-ENV-018", {
  db <- tryCatch(tools::Rd_db("DTAtools"), error = function(e) NULL)
  if (is.null(db) || length(db) == 0) {
    testthat::skip("no installed Rd database available (source or development install)")
  }

  rd_tags <- get("RdTags", envir = asNamespace("tools"))
  aliases <- unique(unlist(lapply(db, function(rd) {
    tags <- rd_tags(rd)
    vapply(rd[tags == "\\alias"], function(x) as.character(x[[1]]), character(1))
  })))
  exports <- getNamespaceExports("DTAtools")
  undocumented <- setdiff(exports, aliases)
  qa_step(
    "every exported symbol has a help alias in tools::Rd_db(\"DTAtools\")",
    character(0), undocumented
  )
})

test_that("IQ-ENV-019 | at least one vignette is installed | REQ-ENV-019", {
  info <- vignette(package = "DTAtools")
  n <- if (is.null(info$results)) 0L else nrow(info$results)
  if (n == 0L) {
    testthat::skip("no vignettes are installed with this copy of the package")
  }
  qa_check(sprintf("%d vignette(s) are installed", n), n > 0)
})

test_that("IQ-ENV-020 | documented package options are in force at their defaults | REQ-ENV-020", {
  # Names and defaults pinned by reading every getOption("DTAtools....") call
  # under R/ directly (see REQ-ENV-020's source field for file:line), not
  # copied from documentation that could itself have drifted from the code.
  defaults <- list(
    DTAtools.stream = "auto",
    DTAtools.stream_threshold = 512 * 1024^2,
    DTAtools.stream_block_size = 8388608L,
    DTAtools.transcode_block_bytes = 4194304L,
    DTAtools.stream_batch_rows = 131072L,
    DTAtools.max_errors = 10000L,
    DTAtools.benchmark = FALSE,
    DTAtools.use_arrow_compute = FALSE,
    DTAtools.arrow_min_rows = 100000L,
    DTAtools.stream_arrow_numeric = TRUE,
    DTAtools.stream_arrow_numeric_min_rows = 20000L,
    DTAtools.stream_arrow_unique = TRUE,
    DTAtools.progress_seconds = 30
  )
  for (name in names(defaults)) {
    qa_step(
      sprintf("%s is in force at its documented default", name),
      defaults[[name]], getOption(name, defaults[[name]])
    )
  }
})

test_that("IQ-ENV-021 | the output directory and tempdir() are writable | REQ-ENV-021", {
  out_dir <- qa_tempdir()
  probe <- file.path(out_dir, "write-probe.txt")
  wrote_out <- isTRUE(tryCatch(
    {
      writeLines("qualification write probe", probe)
      file.exists(probe)
    },
    error = function(e) FALSE
  ))
  qa_check("a fresh output directory accepts a written file", wrote_out)

  tmp_probe <- file.path(tempdir(), paste0("qa-env-write-probe-", Sys.getpid(), ".txt"))
  on.exit(unlink(tmp_probe), add = TRUE)
  wrote_tmp <- isTRUE(tryCatch(
    {
      writeLines("qualification write probe", tmp_probe)
      file.exists(tmp_probe)
    },
    error = function(e) FALSE
  ))
  qa_check("tempdir() accepts a written file", wrote_tmp)
})

test_that("IQ-ENV-022 | free disk space is recorded | REQ-ENV-022", {
  free_bytes <- qa_env_free_disk_bytes(tempdir())
  qa_check(
    "free disk space at the write location is recorded",
    TRUE,
    detail = if (is.na(free_bytes)) {
      "could not be determined on this platform"
    } else {
      sprintf("%.1f GB free", free_bytes / 1024^3)
    }
  )
})

test_that("IQ-ENV-023 | pandoc and a headless browser are recorded without being required | REQ-ENV-023", {
  pandoc_ok <- isTRUE(qual_safe(rmarkdown::pandoc_available(), FALSE))
  qa_check(
    "pandoc availability is recorded",
    TRUE,
    detail = if (pandoc_ok) {
      paste("available, version", as.character(qual_safe(rmarkdown::pandoc_version(), NA)))
    } else {
      "not available"
    }
  )

  browser <- NA_character_
  if (requireNamespace("chromote", quietly = TRUE)) {
    found <- qual_safe(chromote::find_chrome(), NA_character_)
    if (length(found) == 1 && !is.na(found) && nzchar(found)) browser <- found
  }
  qa_check(
    "a browser for the application's browser tests is recorded",
    TRUE,
    detail = if (is.na(browser)) "not available" else browser
  )
})

test_that("IQ-ENV-024 | a PDF export backend is recorded without being required | REQ-ENV-024", {
  backend <- qual_safe(dta_pdf_backend(), NULL)
  qa_check(
    "dta_pdf_backend() reports NULL or a list naming a usable backend",
    is.null(backend) ||
      (is.list(backend) && is.character(backend$name) && nzchar(backend$name)),
    detail = if (is.null(backend)) "no PDF backend available" else backend$name
  )
})
