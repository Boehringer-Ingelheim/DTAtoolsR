# print.dta_qualification() and summary.dta_qualification() are exported S3
# methods that nothing in this suite ever invoked -- the qualification suite
# that produces a real `dta_qualification` object lives under
# inst/qualification/, which does not run on pull requests, so these methods
# were effectively uncovered. Both destructure a number of nested fields by
# name (x$run$stages_run, x$meta_checks$ok, object$summary$per_stage,
# object$expectations$tc_id, ...); a mock built by hand, with every counted
# value chosen so it can be verified by arithmetic rather than by running the
# methods and reading off what they printed, is enough to pin the
# destructuring without running the full qualification suite.
#
# Both methods render mostly through cli, which by default writes to the
# message stream, not stdout -- capture.output(..., type = "message") is the
# convention already used elsewhere in this suite for cli-based print methods.
# Exact glyphs (bullets, checkmarks, rule lines) depend on the terminal's
# unicode/colour support, so assertions match fixed substrings of the text
# content instead of whole lines.

qual_mock <- structure(
  list(
    verdict = "PASS",
    run = list(id = "Q-2026-001", stages_run = c("IQ", "OQ")),
    environment = list(package = list(version = "9.9.9")),
    # IQ: 2 pass, 1 fail. OQ: 1 pass, 1 error, 1 skip.
    tests = data.frame(
      tc_id = c("IQ-001", "IQ-002", "IQ-003", "OQ-001", "OQ-002", "OQ-003"),
      stage = c("IQ", "IQ", "IQ", "OQ", "OQ", "OQ"),
      status = c("pass", "pass", "fail", "pass", "error", "skip"),
      stringsAsFactors = FALSE
    ),
    # 2 of 3 meta-consistency checks failed.
    meta_checks = data.frame(ok = c(TRUE, FALSE, FALSE), stringsAsFactors = FALSE),
    bundle_dir = "Z:/fake/bundle/dir",
    summary = list(per_stage = data.frame(stage = c("IQ", "OQ"), n = c(3L, 3L))),
    # Only OQ-002 has a recorded expectation; IQ-003 and OQ-003 (also not
    # "pass") deliberately have none, to exercise both the "message found"
    # and "no message recorded" branches of summary()'s per-row lookup.
    expectations = data.frame(
      tc_id = c("OQ-002"),
      type = c("failure"),
      message = c("expected 5 but got 6"),
      stringsAsFactors = FALSE
    )
  ),
  class = "dta_qualification"
)

test_that("print.dta_qualification() reports verdict, run, per-stage counts and meta-check failures", {
  out <- paste(capture.output(print(qual_mock), type = "message"), collapse = "\n")

  expect_match(out, "PASS", fixed = TRUE)
  expect_match(out, "Q-2026-001", fixed = TRUE)
  expect_match(out, "9.9.9", fixed = TRUE)
  expect_match(out, "IQ: 2 passed, 1 failed, 0 errored, 0 not executed", fixed = TRUE)
  expect_match(out, "OQ: 1 passed, 0 failed, 1 errored, 1 not executed", fixed = TRUE)
  expect_match(out, "2 meta-consistency checks failed.", fixed = TRUE)
  expect_match(out, "Z:/fake/bundle/dir", fixed = TRUE)
})

test_that("print.dta_qualification() reports success rather than a warning when every meta-check passes", {
  clean <- qual_mock
  clean$meta_checks <- data.frame(ok = c(TRUE, TRUE), stringsAsFactors = FALSE)
  out <- paste(capture.output(print(clean), type = "message"), collapse = "\n")

  expect_false(grepl("meta-consistency", out, fixed = TRUE))
})

test_that("summary.dta_qualification() returns the per-stage summary and lists failing cases", {
  # summary()'s own print(per_stage) call writes to stdout, not the message
  # stream (it is a plain data.frame print, not cli); wrap the whole thing in
  # an outer output capture so that leaks into the test run's console rather
  # than the assertions, while the inner capture -- type = "message" -- still
  # gets the cli-rendered "Test cases not passing" section.
  msgs <- character()
  result <- NULL
  invisible(capture.output(
    msgs <- capture.output(result <- summary(qual_mock), type = "message"),
    type = "output"
  ))
  out <- paste(msgs, collapse = "\n")

  expect_identical(result, qual_mock$summary$per_stage)

  expect_match(out, "Test cases not passing", fixed = TRUE)
  # No expectation was recorded for IQ-003, so its line carries no message.
  expect_match(out, "IQ-003 [fail]", fixed = TRUE)
  # OQ-002's recorded expectation message is appended.
  expect_match(out, "OQ-002 [error] expected 5 but got 6", fixed = TRUE)
  expect_match(out, "OQ-003 [skip]", fixed = TRUE)
})

test_that("summary.dta_qualification() omits the failing-cases section when everything passed", {
  clean <- qual_mock
  clean$tests$status <- "pass"
  msgs <- character()
  invisible(capture.output(
    msgs <- capture.output(summary(clean), type = "message"),
    type = "output"
  ))
  out <- paste(msgs, collapse = "\n")

  expect_false(grepl("Test cases not passing", out, fixed = TRUE))
})
