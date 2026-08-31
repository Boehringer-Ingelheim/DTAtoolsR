# The two lines check() prints to close a run: one per dataset, one for the DTA.
#
# Both defects these pin lived in a branch combination no fixture could produce
# at the time, which is why the logic is now a pure function: every combination
# of counts is reachable here, whether or not a document can currently be built
# that produces it.
#
# The invariant throughout: a target carrying no verdict is never folded into
# either outcome. It is not a pass and it is not a failure, and it has to be
# said out loud in both directions.

vs_msg <- function(...) dta_dataset_summary_message(...)
vs_all <- function(...) dta_overall_summary_message(...)


test_that("a dataset where nothing was judged does not report success", {
  # The defect: the success branch was reached whenever n_invalid == 0, without
  # consulting n_unchecked -- which was already computed two lines above it for
  # the rollup that does consult it. A dataset whose only target carried no
  # verdict printed a green "0 tables validated: all valid", contradicting the
  # dataset's own "0 of 1 table valid; 1 not checked" on the line directly
  # above.
  msg <- vs_msg(n_targets = 1, n_validated = 0, n_valid = 0, n_invalid = 0, n_unchecked = 1)

  expect_equal(msg$severity, "warning")
  expect_equal(msg$text, "0 of 1 table valid; 1 target not checked")
  expect_false(grepl("all valid", msg$text, fixed = TRUE))
})


test_that("unchecked targets are named alongside a failure, not hidden behind it", {
  # The failure branch used to win outright, so a run with one invalid and one
  # unjudged target mentioned only the failure -- and repairing that failure
  # flipped the run to incomplete for a reason that had been true, and silent,
  # the whole time.
  msg <- vs_msg(n_targets = 2, n_validated = 1, n_valid = 0, n_invalid = 1, n_unchecked = 1)

  expect_equal(msg$severity, "danger")
  expect_match(msg$text, "1 INVALID", fixed = TRUE)
  expect_match(msg$text, "1 target not checked", fixed = TRUE)
})


test_that("a fully judged, fully clean dataset still reports success", {
  msg <- vs_msg(n_targets = 3, n_validated = 3, n_valid = 3, n_invalid = 0, n_unchecked = 0)

  expect_equal(msg$severity, "success")
  expect_equal(msg$text, "3 tables validated: all valid")
})


test_that("a failure with nothing outstanding reports no unchecked tail", {
  msg <- vs_msg(n_targets = 2, n_validated = 2, n_valid = 1, n_invalid = 1, n_unchecked = 0)

  expect_equal(msg$severity, "danger")
  expect_equal(msg$text, "2 tables validated: 1 valid, 1 INVALID")
  expect_false(grepl("not checked", msg$text, fixed = TRUE))
})


test_that("the incomplete line agrees with the count that governs its noun", {
  # `n_validated` governs the noun in the other two branches, correctly, for
  # their own sentences. Reusing it here reads "1 of 3 table valid", because
  # n_validated is exactly the count that is low when targets went unchecked.
  many <- vs_msg(n_targets = 3, n_validated = 1, n_valid = 1, n_invalid = 0, n_unchecked = 2)
  expect_equal(many$text, "1 of 3 tables valid; 2 targets not checked")

  one <- vs_msg(n_targets = 1, n_validated = 0, n_valid = 0, n_invalid = 0, n_unchecked = 1)
  expect_equal(one$text, "0 of 1 table valid; 1 target not checked")
})


test_that("the overall line names unchecked targets alongside either failure", {
  by_verdict <- vs_all(total_invalid = 1, total_import_errors = 0, total_unchecked = 2)
  expect_equal(by_verdict$severity, "danger")
  expect_equal(
    by_verdict$text,
    "Validation FAILED: 1 table with validation errors; 2 targets not checked"
  )

  # The import axis fails the run on its own, and must carry the same tail.
  by_import <- vs_all(total_invalid = 0, total_import_errors = 3, total_unchecked = 1)
  expect_equal(by_import$severity, "danger")
  expect_match(by_import$text, "3 values could not be imported", fixed = TRUE)
  expect_match(by_import$text, "1 target not checked", fixed = TRUE)
})


test_that("the overall line keeps its established wording when nothing is outstanding", {
  expect_equal(
    vs_all(total_invalid = 2, total_import_errors = 0, total_unchecked = 0)$text,
    "Validation FAILED: 2 tables with validation errors"
  )
  expect_equal(
    vs_all(total_invalid = 0, total_import_errors = 1, total_unchecked = 0)$text,
    "Validation FAILED: 1 value could not be imported in the declared type"
  )
  expect_equal(
    vs_all(total_invalid = 0, total_import_errors = 0, total_unchecked = 1)$text,
    "Validation INCOMPLETE: 1 target was not checked"
  )
  expect_equal(
    vs_all(total_invalid = 0, total_import_errors = 0, total_unchecked = 2)$text,
    "Validation INCOMPLETE: 2 targets were not checked"
  )

  clean <- vs_all(total_invalid = 0, total_import_errors = 0, total_unchecked = 0)
  expect_equal(clean$severity, "success")
  expect_equal(clean$text, "Validation PASSED: All datasets are valid")
})


test_that("success is unreachable while anything is unchecked", {
  # The property both defects violated, asserted over the whole grid rather than
  # at the two points that happened to be reported.
  for (targets in 1:3) {
    for (invalid in 0:targets) {
      for (unchecked in 0:(targets - invalid)) {
        valid <- targets - invalid - unchecked
        msg <- vs_msg(
          n_targets = targets, n_validated = valid + invalid,
          n_valid = valid, n_invalid = invalid, n_unchecked = unchecked
        )
        if (unchecked > 0) {
          expect_false(
            identical(msg$severity, "success"),
            info = sprintf("targets=%d invalid=%d unchecked=%d", targets, invalid, unchecked)
          )
          expect_match(msg$text, "not checked", fixed = TRUE)
        } else if (invalid == 0) {
          expect_equal(msg$severity, "success")
        }
      }
    }
  }
})


# ---- end to end --------------------------------------------------------------

vs_specced <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
}

# A dataset declaring no columns is the reachable source of an unjudged target:
# check() records status "unspecified" with ok = NA rather than validating it.
vs_unchecked_dataset <- function() {
  DTADataSetTabular(
    name = "unchecked",
    specs = DTAColumnSpecCollection(columns = list()),
    tables = list(tab = data.frame(ID = "A001", stringsAsFactors = FALSE))
  )
}

vs_invalid_dataset <- function() {
  DTADataSetTabular(
    name = "bad",
    specs = vs_specced(),
    tables = list(tab = data.frame(ID = "TOO-LONG", stringsAsFactors = FALSE))
  )
}

vs_console <- function(expr) {
  gsub("\\s+", " ", paste(testthat::capture_messages(expr), collapse = " "))
}


test_that("check() does not print a green summary over a dataset it never judged", {
  dta <- DTA(
    datasets = list(unchecked = vs_unchecked_dataset()),
    metadata = create_example_DTAMetaData()
  )

  out <- vs_console(check(dta, persist = FALSE, quiet = FALSE))

  expect_false(grepl("all valid", out, fixed = TRUE))
  expect_match(out, "1 target not checked", fixed = TRUE)
  expect_match(out, "Validation INCOMPLETE", fixed = TRUE)
})


test_that("check() reports an unchecked dataset even when another one failed", {
  dta <- DTA(
    datasets = list(bad = vs_invalid_dataset(), unchecked = vs_unchecked_dataset()),
    metadata = create_example_DTAMetaData()
  )

  out <- vs_console(check(dta, persist = FALSE, quiet = FALSE))

  expect_match(out, "Validation FAILED", fixed = TRUE)
  # Repairing the failing dataset would otherwise turn this run into
  # "Validation INCOMPLETE" for a reason nothing had mentioned.
  expect_match(out, "1 target not checked", fixed = TRUE)
})


test_that("a clean DTA still reports PASSED", {
  dta <- DTA(
    datasets = list(good = DTADataSetTabular(
      name = "good", specs = vs_specced(),
      tables = list(tab = data.frame(ID = "A001", stringsAsFactors = FALSE))
    )),
    metadata = create_example_DTAMetaData()
  )

  out <- vs_console(check(dta, persist = FALSE, quiet = FALSE))

  expect_match(out, "Validation PASSED", fixed = TRUE)
  expect_match(out, "all valid", fixed = TRUE)
  expect_false(grepl("not checked", out, fixed = TRUE))
})


test_that("an unrecognised severity aborts rather than printing nothing", {
  # `switch()` without a default arm returns NULL invisibly for an unmatched
  # string, so a severity added to a builder but not to the renderer would make
  # that summary line vanish silently -- the same class of defect as the two
  # this change fixes.
  expect_error(
    dta_emit_summary_message(list(severity = "info", text = "x")),
    "Unknown validation summary severity"
  )
  expect_error(
    dta_emit_summary_message(list(text = "x")),
    "exactly one severity"
  )
  expect_error(
    dta_emit_summary_message(list(severity = c("danger", "success"), text = "x")),
    "exactly one severity"
  )
})


test_that("every severity the builders can produce is renderable", {
  # The other half of the guard above: the renderer must actually handle each
  # severity the builders emit, so neither can drift from the other.
  produced <- unique(c(
    vs_msg(1, 1, 1, 0, 0)$severity,
    vs_msg(2, 1, 0, 1, 0)$severity,
    vs_msg(1, 0, 0, 0, 1)$severity,
    vs_all(0, 0, 0)$severity,
    vs_all(1, 0, 0)$severity,
    vs_all(0, 0, 1)$severity
  ))
  expect_setequal(produced, c("success", "danger", "warning"))

  for (severity in produced) {
    expect_no_error(dta_emit_summary_message(list(severity = severity, text = "x")))
  }
})
