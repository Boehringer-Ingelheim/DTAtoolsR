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

test_that("a requirement a live deviation affects is verified with deviation, not verified", {
  req_df <- data.frame(id = c("R-1", "R-2", "R-3", "R-4"), area = "X", risk = "high")
  disc_df <- data.frame(
    tc_id = c("T-1", "T-2", "T-3", "T-4"), req_ids = c("R-1", "R-2", "R-3", "R-4"), stage = "OQ"
  )
  tests_df <- data.frame(tc_id = c("T-1", "T-2", "T-3", "T-4"), status = c("pass", "pass", "pass", "fail"))
  dev_df <- data.frame(
    id = c("D-1", "D-2", "D-3"), status = c("open", "pending", "closed"),
    affects = c("R-2 R-4", "R-2", "R-3")
  )

  req <- attr(qual_traceability(req_df, disc_df, tests_df, "OQ", dev_df), "requirements")
  expect_identical(req$status, c("verified", "verified_with_deviation", "verified", "failed"))
  expect_identical(req$deviations, c("", "D-1 D-2", "", "D-1"))

  # Without a register the status is decided by the test cases alone.
  plain <- attr(qual_traceability(req_df, disc_df, tests_df, "OQ"), "requirements")
  expect_identical(plain$status, c("verified", "verified", "verified", "failed"))
  expect_identical(plain$deviations, rep("", 4))
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

# A minimal `dta_qualification`-shaped object, just enough for
# qual_write_bundle() to walk without needing a real run. Every data frame is
# a stand-in; what these tests exercise is which files get written, not what
# is in them.
qual_bundle_fixture <- function() {
  # Each slot gets its own value, so a file written from the wrong slot shows.
  stub <- function(v) data.frame(a = v, stringsAsFactors = FALSE)
  trace <- stub(3)
  attr(trace, "requirements") <- stub(2)
  structure(
    list(
      run = list(id = "Q-fixture", hash_algorithm = "md5"),
      environment = list(package = list(version = "9.9.9")),
      summary = list(verdict = "PASS"),
      tests = stub(1), expectations = stub(4), traceability = trace,
      requirements = stub(5), deviations = stub(6), limitations = stub(7),
      deviation_status = stub(8), meta_checks = stub(9), performance = NULL,
      unit_tests = list(run = FALSE, results = NULL)
    ),
    class = "dta_qualification"
  )
}

test_that("qual_write_bundle() records a failed evidence write and still writes the rest", {
  dir <- withr::local_tempdir()
  x <- qual_bundle_fixture()

  real_write_csv <- qual_write_csv
  testthat::local_mocked_bindings(
    qual_write_csv = function(x, path) {
      if (identical(basename(path), "tests.csv")) {
        stop("disk full")
      }
      real_write_csv(x, path)
    }
  )

  bundle <- qual_write_bundle(x, dir)

  expect_identical(bundle$failed, "tests.csv")
  expect_false(file.exists(file.path(dir, "results", "tests.csv")))
  # The surviving files must hold the fixture's content, not merely exist.
  # Expected values are the fixture's per-slot constants: coverage.csv comes
  # from the traceability "requirements" attribute, the rest from their slot.
  expected <- c(
    expectations.csv = 4L, coverage.csv = 2L, traceability.csv = 3L,
    requirements.csv = 5L, deviations.csv = 6L, limitations.csv = 7L,
    meta_checks.csv = 9L
  )
  for (nm in names(expected)) {
    csv <- utils::read.csv(file.path(dir, "results", nm))
    expect_identical(names(csv), "a")
    expect_identical(csv$a, unname(expected[[nm]]))
  }
  json <- jsonlite::read_json(file.path(dir, "results", "results.json"))
  expect_identical(json$schema_version, 1L)
  expect_identical(json$run$id, "Q-fixture")
  expect_identical(unlist(json$run$evidence_write_failures), "tests.csv")
  expect_identical(json$expectations[[1]]$a, 4L)
  expect_identical(json$coverage[[1]]$a, 2L)
})

test_that("qual_write_bundle() leaves no results.json when it fails to serialise, and reports it", {
  dir <- withr::local_tempdir()
  x <- qual_bundle_fixture()
  # An environment has no jsonlite method, so payload$run fails to serialise
  # while environment.json (built from x$environment alone) still succeeds.
  x$run$unserialisable <- new.env()

  bundle <- qual_write_bundle(x, dir)

  expect_identical(bundle$failed, "results.json")
  expect_false(file.exists(file.path(dir, "results", "results.json")))
  # qual_hash_one() must not error over a file that was never written.
  expect_true(is.na(qual_hash_one(bundle$json_path)))
})

test_that("a failed evidence write downgrades the run verdict to FAIL and is recorded everywhere", {
  skip_on_cran()
  out <- withr::local_tempdir()

  real_write_csv <- qual_write_csv
  testthat::local_mocked_bindings(
    qual_write_csv = function(x, path) {
      if (identical(basename(path), "meta_checks.csv")) {
        stop("disk full")
      }
      real_write_csv(x, path)
    }
  )

  bundle <- run_qualification(
    out,
    stages = character(0), scale = "quick", formats = "md", quiet = TRUE
  )

  expect_match(bundle$verdict, "^FAIL")
  expect_identical(bundle$summary$verdict, bundle$verdict)
  expect_identical(bundle$run$evidence_write_failures, "meta_checks.csv")
  expect_false(file.exists(file.path(bundle$bundle_dir, "results", "meta_checks.csv")))

  run_json <- jsonlite::read_json(file.path(bundle$bundle_dir, "results", "run.json"))
  expect_identical(unlist(run_json$evidence_write_failures), "meta_checks.csv")

  # results.json is written inside the bundle step, after the CSVs: it must
  # already carry the downgraded verdict, not the one the tests produced.
  results_json <- jsonlite::read_json(file.path(bundle$bundle_dir, "results", "results.json"))
  expect_identical(results_json$summary$verdict, bundle$verdict)
  expect_identical(unlist(results_json$run$evidence_write_failures), "meta_checks.csv")

  summary_txt <- readLines(file.path(bundle$bundle_dir, "SUMMARY.txt"))
  expect_true(any(grepl("EVIDENCE BUNDLE INCOMPLETE", summary_txt, fixed = TRUE)))

  report_md <- readLines(file.path(bundle$bundle_dir, "report", "qualification-report.md"))
  expect_true(any(grepl("Evidence bundle incomplete", report_md, fixed = TRUE)))
})

test_that("the runner refuses to overwrite an existing evidence bundle", {
  skip_on_cran()
  out <- withr::local_tempdir()
  ids <- qual_run_id(time = Sys.time() + seq(0, 20))
  for (id in ids) {
    dir.create(file.path(out, id), recursive = TRUE, showWarnings = FALSE)
    writeLines("precious", file.path(out, id, "marker.txt"))
  }
  expect_error(
    run_qualification(out, stages = character(0), formats = "md", quiet = TRUE),
    "evidence bundle already exists",
    class = "rlang_error"
  )
  # Refusing means leaving every existing bundle exactly as it was.
  for (id in ids) {
    expect_identical(list.files(file.path(out, id)), "marker.txt")
    expect_identical(readLines(file.path(out, id, "marker.txt")), "precious")
  }
})

test_that("bad arguments to the runner are rejected by name", {
  expect_error(
    run_qualification(1L), "must be a single directory path",
    class = "rlang_error", fixed = TRUE
  )
  expect_error(
    run_qualification(tempfile(), stages = "SIT"), "`stages` must be a subset",
    class = "rlang_error", fixed = TRUE
  )
  expect_error(
    run_qualification(tempfile(), formats = "epub"), "`formats` must be a subset",
    class = "rlang_error", fixed = TRUE
  )
  expect_error(
    run_qualification(tempfile(), filter = c("a", "b")),
    "`filter` must be `NULL` or a single regular expression",
    class = "rlang_error", fixed = TRUE
  )
  # `scale` and `include_unit_tests` used base::match.arg(), whose failure is a
  # plain simpleError carrying a message in the system language -- so the only
  # thing assertable about it was "some base R error happened", which a typo in
  # the call satisfies just as well as the rejection. They now fail the way
  # their four siblings above do, which is what REQ-ROBUST-015 requires of a
  # user-facing error.
  expect_error(
    run_qualification(tempfile(), scale = "enormous"), "must be one of",
    class = "rlang_error", fixed = TRUE
  )
  expect_error(
    run_qualification(tempfile(), include_unit_tests = "sometimes"),
    "must be one of",
    class = "rlang_error", fixed = TRUE
  )
})

test_that("the runner still accepts the abbreviations match.arg() accepted", {
  # Changing HOW an argument is rejected must not change WHICH values it
  # accepts. match.arg() resolves a prefix that fits exactly one choice, so
  # `scale = "qu"` has always meant "quick"; a caller's script that spelled it
  # that way is not a caller this change is entitled to break.
  expect_identical(qual_match_arg("qu", qual_scales(), "scale"), "quick")
  expect_identical(qual_match_arg("st", qual_scales(), "scale"), "standard")
  expect_identical(qual_match_arg("f", qual_scales(), "scale"), "full")
  expect_identical(
    qual_match_arg("n", qual_unit_test_modes(), "include_unit_tests"), "never"
  )
  # Exact spellings, the whole default vector (meaning "not supplied"), and an
  # ambiguous prefix -- "a" fits both "auto" and "always", which match.arg()
  # refuses too.
  expect_identical(qual_match_arg("quick", qual_scales(), "scale"), "quick")
  expect_identical(qual_match_arg(qual_scales(), qual_scales(), "scale"), "full")
  expect_error(
    qual_match_arg("a", qual_unit_test_modes(), "include_unit_tests"),
    "must be one of",
    class = "rlang_error", fixed = TRUE
  )

  # And through the exported function: an abbreviated `scale` gets past the
  # matcher, leaving the deliberately bad `formats` to be the thing that
  # aborts. Were the abbreviation rejected, the message would name `scale`.
  err <- expect_error(
    run_qualification(tempfile(), scale = "qu", formats = "epub"),
    "`formats` must be a subset",
    fixed = TRUE
  )
  expect_false(grepl("`scale` must", conditionMessage(err), fixed = TRUE))
  err <- expect_error(
    run_qualification(tempfile(), include_unit_tests = "n", formats = "epub"),
    "`formats` must be a subset",
    fixed = TRUE
  )
  expect_false(grepl("`include_unit_tests` must", conditionMessage(err), fixed = TRUE))
})

test_that("the runner's declared defaults are the vocabulary they are checked against", {
  # `qual_match_arg()` treats a value identical to the whole choice vector as
  # "not supplied", which is only correct while the signature's default and the
  # vocabulary function are the same vector. Nothing else would notice them
  # drifting apart: the default would simply stop being recognised as one.
  expect_identical(eval(formals(run_qualification)$scale), qual_scales())
  expect_identical(
    eval(formals(run_qualification)$include_unit_tests), qual_unit_test_modes()
  )
  expect_identical(eval(formals(run_qualification)$stages), qual_stages())
  expect_identical(eval(formals(run_qualification)$formats), qual_formats())
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

test_that("the report's evidence-inventory section claims only what it contains", {
  # The section said "Every file below is covered by SHA256SUMS", but it
  # listed no files, so the sentence promised an inventory the report never
  # held. The hash manifest IS the inventory; the section has to say that and
  # tell the reader how to verify it.
  run <- list(hash_manifest = "SHA256SUMS", hash_algorithm = "sha256")
  out <- qual_report_inventory(list(run = run))
  text <- paste(out, collapse = "\n")

  expect_equal(out[[1]], "## 12. Evidence inventory")
  expect_no_match(text, "below", fixed = TRUE)
  expect_match(text, "`SHA256SUMS` lists", fixed = TRUE)
  expect_match(text, "sha256sum -c SHA256SUMS", fixed = TRUE)
})

test_that("the evidence-inventory section still reads sensibly with no manifest recorded", {
  out <- qual_report_inventory(list(run = list(hash_algorithm = "sha256")))
  text <- paste(out, collapse = "\n")

  expect_match(text, "the hash manifest", fixed = TRUE)
  expect_no_match(text, "below", fixed = TRUE)
})

test_that("report cells and failure messages are escaped, so rendered documents keep the evidence verbatim", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")

  path <- "C:\\Users\\someone\\data\\x.csv"
  re <- "\\d+ *a* b"
  msg <- paste0("Expected <data.frame> got <script>alert(1)</script> at ", path, "; pattern ", re, " | c")
  # The failure line comes from the real report code, fed a fixture holding the
  # hostile message, so dropping qual_md_escape() there fails this test.
  fixture <- list(
    expectations = data.frame(
      tc_id = "OQ-X-001", type = "failure", file = "f.R", line = 1L, message = msg,
      stringsAsFactors = FALSE
    )
  )
  rows <- data.frame(tc_id = "OQ-X-001", status = "fail", title = "t", stringsAsFactors = FALSE)
  detail <- qual_report_stage_detail(fixture, rows)
  expect_true(any(grepl("`f.R:1` [failure]", detail, fixed = TRUE)))
  md <- c(
    "# t", "",
    qual_md_table(data.frame(id = "OQ-X-001", message = msg)),
    "",
    detail
  )
  dir <- withr::local_tempdir()
  md_path <- file.path(dir, "r.md")
  writeLines(md, md_path)
  html_path <- rmarkdown::render(
    md_path,
    output_format = rmarkdown::html_document(self_contained = FALSE),
    quiet = TRUE
  )
  html <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  body <- gsub("[[:space:]]+", " ", sub("(?s)^.*<body[^>]*>", "", html, perl = TRUE))

  # No element was created out of the message, and nothing was emphasised.
  expect_no_match(body, "<script>alert", fixed = TRUE)
  expect_no_match(body, "<em>", fixed = TRUE)
  # Both renderings keep the literal characters: the path, the regular
  # expression, the asterisks and the text after the angle brackets.
  expect_gte(lengths(regmatches(body, gregexpr("&lt;script&gt;alert(1)&lt;/script&gt;", body, fixed = TRUE))), 2L)
  expect_gte(lengths(regmatches(body, gregexpr(path, body, fixed = TRUE))), 2L)
  expect_gte(lengths(regmatches(body, gregexpr(re, body, fixed = TRUE))), 2L)
  expect_gte(lengths(regmatches(body, gregexpr("<td>", body, fixed = TRUE))), 2L)
})

test_that("a run filter that selects no file in a stage is an empty stage, not an aborted one", {
  skip_on_cran()
  tests_dir <- withr::local_tempdir()
  writeLines(
    'test_that("OQ-AAA-001 | one | REQ-AAA-001", { testthat::expect_true(TRUE) })',
    file.path(tests_dir, "test-OQ-AAA-one.R")
  )
  writeLines(
    'test_that("OQ-BBB-001 | two | REQ-BBB-001", { testthat::expect_true(TRUE) })',
    file.path(tests_dir, "test-OQ-BBB-two.R")
  )

  expect_identical(qual_stage_files("OQ", tests_dir), c("OQ-AAA-one", "OQ-BBB-two"))
  expect_identical(qual_stage_files("IQ", tests_dir, "AAA"), character(0))
  # An anchored filter is applied to the whole name, as written.
  expect_identical(qual_stage_files("OQ", tests_dir, "^OQ-BBB"), "OQ-BBB-two")

  # No IQ file exists, so the stage is empty rather than "No test files found".
  expect_identical(qual_run_stage("IQ", tests_dir, "AAA"), list())

  res <- qual_run_stage("OQ", tests_dir, "^OQ-AAA")
  expect_false(inherits(res, "qual_stage_abort"))
  expect_identical(vapply(res, function(r) r$file, character(1)), "test-OQ-AAA-one.R")
})

test_that("the installed-file hash honours the manifest's algorithm and never substitutes md5 for sha256", {
  f <- withr::local_tempfile()
  writeLines("abc", f)

  # Expected value from the independent base-R implementation, not from the
  # function under test.
  expect_identical(qual_hash_one(f, algo = "md5"), unname(tools::md5sum(f)))

  # An R before 4.5 has no tools::sha256sum (simulated here).
  testthat::local_mocked_bindings(qual_has_sha256 = function() FALSE)
  expect_identical(qual_hash_algorithm(), "md5")
  expect_error(qual_hash_one(f, algo = "sha256"), "sha256 is not available", fixed = TRUE)
  expect_identical(qual_hash_one(f), unname(tools::md5sum(f)))
})

test_that("a run filter that selects no test in any stage aborts instead of passing", {
  skip_on_cran()
  out <- withr::local_tempdir()
  expect_error(
    run_qualification(
      out,
      stages = "IQ", filter = "ZZZNOPE", include_unit_tests = "never",
      scale = "quick", formats = "md", quiet = TRUE
    ),
    "matched no qualification test",
    class = "rlang_error"
  )
  # The abort comes after the bundle was created (it needs the stage results),
  # so the bundle is removed on that path: no half-written evidence is left.
  expect_length(list.files(out), 0L)
  # Not a filter run, or no stage to select from: nothing to mistype.
  # A second run needs its own output dir: the first leaves a bundle behind and
  # bundle names carry a one-second timestamp.
  bundle <- run_qualification(
    withr::local_tempdir(),
    stages = character(0), filter = "ZZZNOPE", scale = "quick", formats = "md", quiet = TRUE
  )
  expect_s3_class(bundle, "dta_qualification")
})
