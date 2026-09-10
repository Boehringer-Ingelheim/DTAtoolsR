# Qualification of the qualification runner. REQ-CORE-001 .. REQ-CORE-008.
#
# The runner is the instrument every other measurement in this suite is taken
# with, so it is checked like anything else the package exports. Most cases
# here drive a synthetic two-test suite in a temporary directory rather than
# the real one: that keeps the evidence about the runner's mechanics separate
# from whatever the real suite happens to contain today.

# A throwaway suite whose outcomes are known because this function wrote them.
qa_core_fixture <- function(dir, file_error = FALSE) {
  writeLines(
    c(
      "test_that(\"OQ-XX-001 | a passing case | REQ-XX-001\", {",
      "  qa_step(\"two is two\", 2L, 2L)",
      "})",
      "",
      "test_that(\"OQ-XX-002 | a failing case | REQ-XX-002\", {",
      "  qa_step(\"deliberate mismatch\", 5L, 4L)",
      "})"
    ),
    file.path(dir, "test-OQ-XX-basic.R")
  )
  if (file_error) {
    writeLines(
      c(
        "stop(\"deliberate top-level failure\")",
        "",
        "test_that(\"OQ-XX-003 | never reached | REQ-XX-003\", {",
        "  qa_step(\"unreachable\", 1L, 1L)",
        "})"
      ),
      file.path(dir, "test-OQ-XX-broken.R")
    )
  }
  file.copy(
    file.path(qa_suite_root(), "tests", "helper-00-qa.R"),
    file.path(dir, "helper-00-qa.R")
  )
  dir
}

qa_core_run <- function(dir) {
  withr::with_envvar(
    c(TESTTHAT_PARALLEL = "FALSE"),
    testthat::test_dir(
      dir,
      reporter = "silent", stop_on_failure = FALSE, stop_on_warning = FALSE,
      package = "DTAtools", load_package = "none"
    )
  )
}

test_that("OQ-CORE-001 | the evidence bundle contains every required file | REQ-CORE-001", {
  out <- qa_tempdir()
  bundle <- run_qualification(
    out,
    stages = character(0), scale = "quick", formats = "md", quiet = TRUE
  )

  expected <- c(
    "SUMMARY.txt", "run.log",
    "report/qualification-report.md",
    "results/results.json", "results/tests.csv", "results/expectations.csv",
    "results/traceability.csv", "results/requirements.csv",
    "results/coverage.csv", "results/deviations.csv",
    "results/meta_checks.csv", "results/environment.json",
    "results/sessioninfo.txt"
  )
  present <- file.exists(file.path(bundle$bundle_dir, expected))
  qa_step(
    "every required bundle file is written",
    expected, expected[present]
  )

  manifest <- file.path(bundle$bundle_dir, "SHA256SUMS")
  if (!file.exists(manifest)) manifest <- file.path(bundle$bundle_dir, "MD5SUMS")
  qa_check("a hash manifest is written", file.exists(manifest))

  # The manifest is worthless if it does not actually cover the evidence, so
  # the covered set is compared against the files on disk rather than counted.
  listed <- sub("^[0-9a-f]+  ", "", readLines(manifest))
  on_disk <- setdiff(
    list.files(bundle$bundle_dir, recursive = TRUE),
    basename(manifest)
  )
  qa_step(
    "the hash manifest covers every other file in the bundle",
    sort(on_disk), sort(listed)
  )

  # And the hashes must be of the files as they stand, not of anything else.
  first <- listed[[1]]
  qa_step(
    sprintf("the recorded hash of %s matches the file", first),
    qa_hash_file(file.path(bundle$bundle_dir, first)),
    sub("  .*$", "", readLines(manifest)[[1]])
  )
})

test_that("OQ-CORE-002 | an existing evidence bundle is never overwritten | REQ-CORE-002", {
  out <- qa_tempdir()
  first <- run_qualification(
    out,
    stages = character(0), scale = "quick", formats = "md", quiet = TRUE
  )
  marker <- file.path(first$bundle_dir, "results", "results.json")
  before <- qa_hash_file(marker)

  # A run id carries the time to the second, so two ordinary runs never
  # collide. Forcing the collision means occupying the directory the next run
  # will want: the ids for the next few seconds are claimed in advance, which
  # makes the collision certain rather than a race.
  claimed <- qual_run_id(time = Sys.time() + seq(0, 20))
  for (id in claimed) {
    dir.create(file.path(out, id), showWarnings = FALSE, recursive = TRUE)
  }
  err <- tryCatch(
    run_qualification(
      out,
      stages = character(0), scale = "quick", formats = "md", quiet = TRUE
    ),
    error = function(e) e
  )
  qa_check(
    "a run whose bundle directory already exists aborts",
    inherits(err, "condition")
  )
  qa_step(
    "the first run's results are untouched",
    before, qa_hash_file(marker)
  )
})

test_that("OQ-CORE-003 | a step records its expected and observed values with a source line | REQ-CORE-003", {
  dir <- qa_core_fixture(qa_tempdir())
  results <- qa_core_run(dir)
  exps <- qual_expectations_df(results, "OQ")

  passing <- exps[exps$tc_id == "OQ-XX-001", , drop = FALSE]
  qa_step(
    "a passing step is recorded with its expected and observed values",
    "two is two | expected: 2 | actual: 2",
    passing$message[[1]]
  )
  failing <- exps[exps$tc_id == "OQ-XX-002", , drop = FALSE]
  qa_step(
    "a failing step is recorded with its expected and observed values",
    "deliberate mismatch | expected: 5 | actual: 4",
    failing$message[[1]]
  )
  qa_step(
    "the step is anchored to the test file that called it",
    "test-OQ-XX-basic.R", failing$file[[1]]
  )
  # The fixture puts the failing qa_step() on line 6. The line that must be
  # recorded is that one, not a line inside qa_step() itself: a reviewer
  # following the evidence has to land on the check, not on the helper every
  # check in the suite goes through.
  qa_step(
    "the step is anchored to the calling line, not to the helper",
    6L, as.integer(failing$line[[1]])
  )
})

test_that("OQ-CORE-004 | the verdict is FAIL whenever any evidence says so | REQ-CORE-004", {
  clean <- data.frame(status = c("pass", "pass"), stringsAsFactors = FALSE)
  meta_ok <- data.frame(ok = TRUE, stringsAsFactors = FALSE)
  dev_ok <- data.frame(status_run = "reproduced", stringsAsFactors = FALSE)

  qa_step(
    "all evidence clean gives PASS",
    "PASS", qual_verdict(clean, meta_ok, dev_ok, FALSE)
  )
  qa_step(
    "a skipped test is reported, not hidden",
    "PASS WITH NOT-EXECUTED TESTS",
    qual_verdict(
      data.frame(status = c("pass", "skip"), stringsAsFactors = FALSE),
      meta_ok, dev_ok, FALSE
    )
  )
  qa_step(
    "a failing test gives FAIL",
    "FAIL",
    qual_verdict(
      data.frame(status = c("pass", "fail"), stringsAsFactors = FALSE),
      meta_ok, dev_ok, FALSE
    )
  )
  qa_step(
    "a failing meta check gives FAIL even when every test passed",
    "FAIL",
    qual_verdict(clean, data.frame(ok = FALSE), dev_ok, FALSE)
  )
  qa_step(
    "a deviation that stopped reproducing gives FAIL",
    "FAIL",
    qual_verdict(clean, meta_ok, data.frame(status_run = "not_reproduced"), FALSE)
  )
  qa_step(
    "a filtered run is marked partial",
    "PASS (PARTIAL)", qual_verdict(clean, meta_ok, dev_ok, TRUE)
  )

  # A failing step must not stop the file: the second case in the fixture runs
  # after the first one fails, or a run would report only its first defect.
  dir <- qa_core_fixture(qa_tempdir())
  tests <- qual_tests_df(qa_core_run(dir), "OQ")
  qa_step(
    "a failing case does not stop the ones after it",
    c("OQ-XX-001", "OQ-XX-002"), sort(tests$tc_id)
  )
  qa_step(
    "the failing case is recorded as failed",
    "fail", tests$status[tests$tc_id == "OQ-XX-002"]
  )
})

test_that("OQ-CORE-005 | a file that errors outside a test case is reported, not silently dropped | REQ-CORE-005", {
  dir <- qa_core_fixture(qa_tempdir(), file_error = TRUE)
  results <- qa_core_run(dir)
  tests <- qual_tests_df(results, "OQ")

  qa_check(
    "the broken file is recorded as a file-level failure",
    "file_error" %in% tests$status
  )
  qa_step(
    "the file-level failure names the file it came from",
    "test-OQ-XX-broken.R",
    tests$tc_id[tests$status == "file_error"][[1]]
  )

  # The case declared below the error never ran. Static discovery is what
  # notices, and the meta check is what turns that into a failure.
  discovered <- qual_discover_tests(dir)
  qa_step(
    "static discovery finds the case the broken file never reached",
    "OQ-XX-003",
    setdiff(discovered$tc_id[discovered$ok], tests$tc_id)
  )
  meta <- qual_meta_checks(
    req_df = qualification_requirements()[0, ],
    dev_df = data.frame(id = character(0), status = character(0)),
    lim_df = data.frame(id = character(0)),
    disc_df = discovered, tests_df = tests,
    exp_df = qual_expectations_df(results, "OQ"),
    api = character(0), index = list(), tests_dir = dir, stages_run = "OQ"
  )
  qa_step(
    "the unreached case fails the completeness check",
    FALSE, meta$ok[meta$check_id == "M10"]
  )
})

test_that("OQ-CORE-006 | a registered deviation is reported as reproduced while it still occurs | REQ-CORE-006", {
  # The defect itself: BESTw. is SAS's general numeric output format, so
  # inferring "Int" from it makes the generated schema reject a legitimate
  # decimal. This binds DEV-001 to the run. When the inference is fixed, this
  # step FAILS on purpose and the register must be updated.
  qa_known_deviation(
    "DEV-001",
    identical(
      DTAColumnSpecStructureSAS(format = "BEST12.", length = 12)@type,
      "Int"
    )
  )

  # And the other half of the contract: an unregistered id is a mistake in the
  # test, not a silent pass.
  err <- tryCatch(qa_known_deviation("DEV-999", TRUE), error = function(e) e)
  qa_check("an unregistered deviation id aborts", inherits(err, "condition"))
})

test_that("OQ-CORE-007 | the requirements load completely or not at all | REQ-CORE-007", {
  reqs <- qualification_requirements()
  qa_check("requirements are returned", nrow(reqs) > 0)
  qa_step(
    "every requirement carries the columns the report needs",
    c("id", "area", "text", "risk", "category", "covers"),
    intersect(c("id", "area", "text", "risk", "category", "covers"), names(reqs))
  )
  # The expected set is read out of the shipped file by an independent means --
  # a regex over its text rather than the YAML loader under test -- so that a
  # loader which silently drops entries is caught, and so that adding a
  # requirement does not require editing a number here. The floor in
  # _index.yaml is what stops the area being emptied.
  core_file <- file.path(qa_suite_root(), "requirements", "REQ-CORE.yaml")
  declared <- sub("^[[:space:]]*- id:[[:space:]]*", "", grep(
    "^[[:space:]]*- id:[[:space:]]*REQ-CORE-[0-9]{3}[[:space:]]*$",
    readLines(core_file, warn = FALSE),
    value = TRUE
  ))
  qa_check("the shipped CORE file declares requirements at all", length(declared) > 0)
  qa_step(
    "every requirement the CORE file declares is returned, and no other",
    sort(declared), sort(qualification_requirements(area = "CORE")$id)
  )
  qa_check(
    "every requirement names at least one exported symbol",
    all(lengths(reqs$covers) > 0)
  )

  # A malformed requirement file must stop the run rather than shrink the set
  # it reports on: a silently truncated requirement list would report full
  # coverage of a subset.
  broken <- qa_tempdir()
  dir.create(file.path(broken, "requirements"))
  writeLines(
    c("area: BAD", "title: Broken", "requirements:", "  - id: REQ-BAD-001", "    text: no risk or category given"),
    file.path(broken, "requirements", "REQ-BAD.yaml")
  )
  err <- tryCatch(qual_requirements_df(broken), error = function(e) e)
  qa_check("a requirement missing its risk rating aborts the read", inherits(err, "condition"))
})

test_that("OQ-CORE-008 | no expected value in this suite comes from a snapshot | REQ-CORE-008", {
  tests_dir <- file.path(qa_suite_root(), "tests")
  qa_step(
    "the suite contains no snapshot expectations",
    character(0), qual_find_snapshot_calls(tests_dir)
  )

  # The check has to be able to see one, or it proves nothing.
  planted <- qa_tempdir()
  writeLines(
    c("test_that(\"x\", {", "  expect_snapshot(print(1))", "})"),
    file.path(planted, "test-planted.R")
  )
  qa_step(
    "the check detects a snapshot expectation when one is there",
    1L, length(qual_find_snapshot_calls(planted))
  )

  # And it must not be fooled by a mention. This very file names the forbidden
  # function in prose and in a string; a text search would report the check
  # that enforces the rule as the first thing violating it.
  mention <- qa_tempdir()
  writeLines(
    c("# expect_snapshot is what this file must not call.", "x <- \"expect_snapshot\""),
    file.path(mention, "test-mention.R")
  )
  qa_step(
    "a mention in a comment or a string is not a violation",
    character(0), qual_find_snapshot_calls(mention)
  )
})

test_that("OQ-CORE-009 | developer evidence is reported by name, and says it does not gate the verdict | REQ-CORE-009", {
  # Built directly rather than by running the developer suite: that suite takes
  # tens of minutes, and what is being qualified here is how its result is
  # reported, not the result itself.
  failures <- data.frame(
    file = c("test-thing.R", "test-other.R"),
    test = c("a thing holds", "another thing holds"),
    type = c("failure", "error"),
    message = c("expected 1 got 2", "could not find function 'nope'"),
    stringsAsFactors = FALSE
  )
  summary <- list(
    path = "/some/library/DTAtools/tests/testthat",
    n_tests = 1897L, passed = 8911L, failed = 2L, errors = 1L, skipped = 1L
  )

  with_failures <- qual_report_unit(
    list(unit_tests = list(run = TRUE, reason = NA_character_, summary = summary, failures = failures))
  )
  text <- paste(with_failures, collapse = "
")

  qa_step(
    "every failing developer test is named in the report",
    c(TRUE, TRUE),
    c(grepl("test-thing.R", text, fixed = TRUE), grepl("test-other.R", text, fixed = TRUE))
  )
  qa_step(
    "with the assertion that failed, not only the file",
    c(TRUE, TRUE),
    c(grepl("a thing holds", text, fixed = TRUE), grepl("expected 1 got 2", text, fixed = TRUE))
  )
  # The reviewer has to be able to tell, from the document alone, that a
  # failure here is not a failure of the qualification. Leaving that to be
  # inferred invites the opposite reading of a report a quality unit signs.
  qa_step(
    "and the report states that these do not contribute to the verdict",
    TRUE,
    grepl("not because they change the verdict", text, fixed = TRUE)
  )

  clean <- qual_report_unit(
    list(unit_tests = list(run = TRUE, reason = NA_character_, summary = summary, failures = NULL))
  )
  qa_step(
    "a suite that passed adds no failure list",
    FALSE,
    grepl("did not pass", paste(clean, collapse = "
"), fixed = TRUE)
  )

  not_run <- qual_report_unit(
    list(unit_tests = list(run = FALSE, reason = "the package was installed without its tests"))
  )
  qa_step(
    "and a suite that did not run reports why",
    TRUE,
    grepl("installed without its tests", paste(not_run, collapse = "
"), fixed = TRUE)
  )
})

test_that("OQ-CORE-010 | one unwritable evidence file does not cost the run its evidence | REQ-CORE-010", {
  dir <- qa_tempdir()

  # testthat's own per-test frame carries a list column, which is exactly the
  # shape that a plain write.csv() refuses. The developer suite produces this
  # frame on every run that executes it, so the flattening is not a defensive
  # nicety -- without it the bundle write aborts after every stage has passed.
  awkward <- data.frame(file = "test-a.R", passed = 1L, stringsAsFactors = FALSE)
  awkward$result <- list(list(structure(list(), class = "expectation_success")))
  qa_check("a frame carrying a list column is a real case", is.list(awkward$result))

  written <- file.path(dir, "awkward.csv")
  qa_step("it is written rather than refused", TRUE, qual_write_evidence(awkward, written))
  qa_step("and reads back with its rows intact", 1L, nrow(utils::read.csv(written)))

  # The path that matters more: a write that cannot succeed must not take the
  # run down with it.
  unwritable <- file.path(dir, "no-such-directory", "deeper", "x.csv")
  outcome <- tryCatch(qual_write_evidence(awkward, unwritable), error = function(e) "aborted")
  qa_step("an impossible write reports failure rather than aborting", FALSE, outcome)
  qa_step("and leaves no file behind", FALSE, file.exists(unwritable))

  # The same frame reaches results.json, where the list column is worse than
  # awkward: the expectation objects it holds are conditions, and jsonlite has
  # no representation for one. The frame belongs in the CSV and not in the
  # JSON, so the machine-readable evidence carries the counts and the names
  # rather than the objects.
  unit <- list(
    run = TRUE, reason = NA_character_,
    summary = list(n_tests = 1L, passed = 1L, failed = 0L, errors = 0L, skipped = 0L, path = dir),
    failures = NULL,
    results = awkward
  )
  qa_check(
    "the raw frame is not serialisable, which is why it is dropped",
    inherits(tryCatch(jsonlite::toJSON(unit), error = function(e) e), "condition")
  )
  stripped <- qual_unit_tests_json(unit)
  qa_step("the per-test frame is dropped for the JSON", TRUE, is.null(stripped$results))
  qa_step(
    "while the counts a machine reader needs are kept",
    c(n_tests = 1L, passed = 1L, failed = 0L),
    c(
      n_tests = stripped$summary$n_tests,
      passed = stripped$summary$passed,
      failed = stripped$summary$failed
    )
  )
  qa_check(
    "and what is left serialises",
    !inherits(tryCatch(jsonlite::toJSON(stripped), error = function(e) e), "condition")
  )
})
