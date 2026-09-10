# Development-time guard for the qualification suite.
#
# The suite itself is run by hand on a target system and takes as long as it
# takes. This file is what runs on every pull request instead: it proves the
# runner still works and that the suite is internally consistent, without
# executing the operational or performance stages.
#
# It deliberately does NOT assert that the overall verdict is PASS. Until every
# area package has landed, the export-coverage check reports the functions no
# requirement claims yet, and that failure is the truth about the suite's
# completeness. What must always hold is everything below.

test_that("the test-title grammar accepts well-formed names and rejects the rest", {
  ok <- qual_parse_test_name("OQ-VAL-007 | codelist violation reported per row | REQ-VAL-001 REQ-VAL-009")
  expect_true(ok$ok)
  expect_identical(ok$tc_id, "OQ-VAL-007")
  expect_identical(ok$stage, "OQ")
  expect_identical(ok$area, "VAL")
  expect_identical(ok$title, "codelist violation reported per row")
  expect_identical(ok$req_ids, c("REQ-VAL-001", "REQ-VAL-009"))

  tagged <- qual_parse_test_name(
    "PQ-PERF-001 | ten million rows | REQ-PERF-001 | tags: slow,scale-full"
  )
  expect_true(tagged$ok)
  expect_identical(tagged$tags, c("slow", "scale-full"))

  # Each of these is a way a real test name has gone wrong: no requirement, a
  # malformed id, a lower-case area, and a plain sentence.
  for (bad in c(
    "OQ-VAL-007 | no requirements named",
    "OQ-VAL-7 | sequence is not three digits | REQ-VAL-001",
    "OQ-val-007 | area must be upper case | REQ-VAL-001",
    "checks that the thing works"
  )) {
    expect_false(qual_parse_test_name(bad)$ok, info = bad)
  }
})

test_that("every shipped requirement satisfies the schema and is reachable", {
  reqs <- qualification_requirements()
  expect_gt(nrow(reqs), 0)
  expect_false(any(duplicated(reqs$id)))
  expect_true(all(grepl("^REQ-[A-Z]{2,8}-[0-9]{3}$", reqs$id)))
  expect_true(all(reqs$risk %in% c("high", "medium", "low")))
  expect_true(all(nzchar(reqs$text)))
  expect_true(all(lengths(reqs$covers) > 0))

  # `covers` names the exported symbols a requirement constrains. A name that
  # is not exported means the requirement is about something a user cannot
  # call, which is a mistake in the requirement rather than in the code.
  api <- qual_read_api(qual_root())
  expect_setequal(api, getNamespaceExports("DTAtools"))
  expect_true(all(unique(unlist(reqs$covers)) %in% api))
})

test_that("every test case in the suite is traced to a requirement that exists", {
  root <- qual_root()
  discovered <- qual_discover_tests(file.path(root, "tests"))
  expect_gt(nrow(discovered), 0)

  untagged <- discovered[!discovered$ok, c("file", "line")]
  expect_identical(nrow(untagged), 0L, info = paste(capture.output(print(untagged)), collapse = "\n"))
  expect_false(any(duplicated(discovered$tc_id)))

  referenced <- unique(unlist(strsplit(discovered$req_ids, " ")))
  expect_true(all(referenced %in% qualification_requirements()$id))

  # A test file must live in the stage and area its cases claim, or a stage
  # filter would silently skip them.
  expect_true(all(mapply(
    function(file, stage, area) startsWith(file, paste0("test-", stage, "-", area)),
    discovered$file, discovered$stage, discovered$area
  )))
})

test_that("no expected value in the qualification suite comes from a snapshot", {
  expect_identical(
    qual_find_snapshot_calls(file.path(qual_root(), "tests")),
    character(0)
  )
})

test_that("every open deviation is bound to a test and every id is unique", {
  devs <- qual_deviations_df(qual_root())
  ids <- c(devs$deviations$id, devs$limitations$id)
  expect_false(any(duplicated(ids)))
  expect_true(all(devs$deviations$status %in% c("open", "pending", "closed")))

  bound <- qual_discover_deviation_refs(file.path(qual_root(), "tests"))
  open <- devs$deviations$id[devs$deviations$status == "open"]
  expect_true(all(open %in% bound))
})

test_that("the runner produces a complete evidence bundle", {
  skip_on_cran()
  out <- withr::local_tempdir()
  bundle <- run_qualification(
    out,
    stages = character(0), scale = "quick", formats = "md", quiet = TRUE
  )

  expected <- c(
    "SUMMARY.txt", "run.log", "report/qualification-report.md",
    "results/results.json", "results/tests.csv", "results/expectations.csv",
    "results/traceability.csv", "results/requirements.csv",
    "results/coverage.csv", "results/meta_checks.csv",
    "results/environment.json", "results/sessioninfo.txt"
  )
  expect_true(all(file.exists(file.path(bundle$bundle_dir, expected))))

  # The manifest must cover the evidence, and its hashes must be of the files
  # as they stand: a manifest that lists the wrong digest is worse than none.
  manifest <- list.files(bundle$bundle_dir, pattern = "^(SHA256|MD5)SUMS$", full.names = TRUE)
  expect_length(manifest, 1)
  listed <- readLines(manifest)
  paths <- sub("^[0-9a-f]+  ", "", listed)
  expect_setequal(
    paths,
    setdiff(list.files(bundle$bundle_dir, recursive = TRUE), basename(manifest))
  )
  # Every file, not a sample. The manifest is the tamper evidence, and the one
  # way it can be wrong that matters is a digest taken before the file was
  # finished -- which is exactly what a check of a single arbitrary entry
  # misses, because the file still being written when the manifest is taken is
  # the log, and the log is not the first entry.
  recomputed <- vapply(
    file.path(bundle$bundle_dir, paths), qual_hash_one, character(1),
    USE.NAMES = FALSE
  )
  mismatched <- paths[recomputed != sub("  .*$", "", listed)]
  expect_identical(mismatched, character(0))

  json <- jsonlite::read_json(file.path(bundle$bundle_dir, "results", "results.json"))
  expect_identical(json$schema_version, 1L)
  expect_identical(json$generator$package, "DTAtools")
  expect_true(nzchar(json$run$id))
})

test_that("the runner refuses to overwrite an existing evidence bundle", {
  skip_on_cran()
  out <- withr::local_tempdir()
  for (id in qual_run_id(time = Sys.time() + seq(0, 20))) {
    dir.create(file.path(out, id), recursive = TRUE, showWarnings = FALSE)
  }
  expect_error(
    run_qualification(out, stages = character(0), formats = "md", quiet = TRUE),
    class = "rlang_error"
  )
})

test_that("bad arguments to the runner are rejected by name", {
  expect_error(run_qualification(1L), class = "rlang_error")
  expect_error(run_qualification(tempfile(), stages = "SIT"), class = "rlang_error")
  expect_error(run_qualification(tempfile(), formats = "epub"), class = "rlang_error")
  expect_error(run_qualification(tempfile(), filter = c("a", "b")), class = "rlang_error")
  expect_error(run_qualification(tempfile(), scale = "enormous"))
})

test_that("the generated fixture's expected result is arithmetic, not a recording", {
  skip_on_cran()
  # The oracle must agree with a real validation, or every correctness claim in
  # the performance stage rests on nothing. It is checked here at the smallest
  # size so that a pull request notices a drift in the validation engine's
  # counting the same day it lands.
  suite <- file.path(qual_root(), "tests")
  # Parented on the namespace, not on the global environment. The helpers call
  # the package's own constructors unqualified, which resolve through
  # globalenv() only while the package happens to be attached -- true under
  # devtools::test(), false when this suite is run from an installed library,
  # where the failure is a bare "could not find function".
  env <- new.env(parent = asNamespace("DTAtools"))
  sys.source(file.path(suite, "helper-00-qa.R"), envir = env)
  sys.source(file.path(suite, "helper-generators.R"), envir = env)

  dir <- withr::local_tempdir()
  path <- file.path(dir, "oracle.csv")
  env$qa_write_large_csv(1000L, path)

  ds <- check(env$qa_dta(path), persist = FALSE, quiet = TRUE)
  status <- validation_status(ds)
  expected <- env$qa_oracle()

  expect_identical(as.integer(status$n_columnspec_errors), as.integer(expected$n_columnspec_errors))
  expect_identical(as.integer(status$n_rule_errors), as.integer(expected$n_rule_errors))
  expect_identical(as.integer(status$n_import_errors), as.integer(expected$n_import_errors))
  expect_false(status$ok)
  expect_equal(
    env$qa_counts(ds),
    env$qa_oracle_counts(status$table[[1]]),
    ignore_attr = TRUE
  )
})
