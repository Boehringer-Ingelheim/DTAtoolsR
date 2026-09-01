test_that("DTAMetaData constructor builds minimal valid object", {
  md <- DTAMetaData(title = "Clinical Data Transfer", version = "1.0")

  expect_s3_class(md, class = "DTAtools::DTAMetaData")
  expect_equal(md@title, "Clinical Data Transfer")
  expect_equal(md@version, "1.0")
  expect_null(md@date)
  expect_equal(length(md@version_history), 0)
  expect_equal(length(md@transmission), 0)
})

test_that("DTAMetaData constructor converts top-level character date", {
  md <- DTAMetaData(
    title = "Clinical Data Transfer",
    version = "1.0",
    date = "2026-07-24"
  )

  expect_s3_class(md@date, "Date")
  expect_identical(as.character(md@date), "2026-07-24")
})

test_that("DTAMetaData constructor converts transmission date strings and preserves phrases", {
  md <- DTAMetaData(
    title = "Transmission Test",
    transmission = list(
      date_first_transfer = "2026-01-01",
      date_last_transfer = "after approval"
    )
  )

  expect_s3_class(md@transmission$date_first_transfer, "Date")
  expect_identical(as.character(md@transmission$date_first_transfer), "2026-01-01")
  expect_type(md@transmission$date_last_transfer, "character")
  expect_identical(md@transmission$date_last_transfer, "after approval")
})

test_that("a qualified transmission date is converted and recorded as an import error", {
  # The constructor coerces a transmission date string with as.Date(), and
  # strptime() ignores TRAILING text, so "2026-12-31 at the earliest" parses as
  # 2026-12-31 and the qualification disappears. In a data transfer agreement
  # that silently turns a lower bound into a committed deadline.
  #
  # The value is still converted - validate_transmission_dates() and the exported
  # documents need a real Date - but the conversion is no longer silent: the
  # original string is recorded verbatim in @import_issues, and an import error
  # cannot pass check() (see test-DTA.R).
  md <- DTAMetaData(
    title = "Qualified Date",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )

  expect_s3_class(md@transmission$date_last_transfer, "Date")
  expect_identical(as.character(md@transmission$date_last_transfer), "2026-12-31")

  issues <- metadata_import_errors(md)
  expect_identical(names(issues), names(dta_empty_import_errors()))
  expect_equal(nrow(issues), 1)
  expect_true(is.na(issues$row))
  expect_identical(issues$column, "transmission$date_last_transfer")
  # The discarded text is not lost: `raw` keeps what was actually written.
  expect_identical(issues$raw, "2026-12-31 at the earliest")
  expect_identical(issues$declared_type, "Date")
  expect_identical(issues$reason, "trailing_residue")

  # date_first_transfer is coerced by the same helper and reports under its own
  # field path.
  md_first <- DTAMetaData(
    title = "Qualified First Date",
    transmission = list(date_first_transfer = "2026-02-01 or later")
  )
  expect_identical(
    metadata_import_errors(md_first)$column,
    "transmission$date_first_transfer"
  )
})

test_that("transfer phrases without a leading date stay character and raise no import error", {
  # Documented, legitimate input: a transfer date may be a free-text phrase. Only
  # a string that STARTS with an ISO date loses information when coerced, so only
  # that case is flagged. Everything below must remain untouched.
  phrases <- c(
    "after approval",
    "2 weeks after approval",
    "final transfer by 2026-12-31",
    "Final transfer by 2026-12-31"
  )

  for (phrase in phrases) {
    md <- DTAMetaData(
      title = "Phrase",
      transmission = list(date_last_transfer = phrase)
    )
    expect_type(md@transmission$date_last_transfer, "character")
    expect_identical(md@transmission$date_last_transfer, phrase)
    expect_equal(nrow(metadata_import_errors(md)), 0)
  }

  # A bare ISO date converts cleanly and is likewise not an import error.
  md_clean <- DTAMetaData(
    title = "Bare Date",
    transmission = list(date_last_transfer = "2026-12-31")
  )
  expect_s3_class(md_clean@transmission$date_last_transfer, "Date")
  expect_equal(nrow(metadata_import_errors(md_clean)), 0)

  # A digit-shaped string that is not a real calendar date is not a date prefix.
  md_not_a_date <- DTAMetaData(
    title = "Not A Date",
    transmission = list(date_last_transfer = "2026-02-30 rescheduled")
  )
  expect_type(md_not_a_date@transmission$date_last_transfer, "character")
  expect_equal(nrow(metadata_import_errors(md_not_a_date)), 0)
})

test_that("a qualified top-level date is converted and recorded as an import error", {
  # The top-level @date runs through the same as.Date() coercion and had the same
  # defect, with nothing pinning it: "2026-07-24 provisional" became a firm date.
  md <- DTAMetaData(title = "Provisional", date = "2026-07-24 provisional")

  expect_s3_class(md@date, "Date")
  expect_identical(as.character(md@date), "2026-07-24")

  issues <- metadata_import_errors(md)
  expect_equal(nrow(issues), 1)
  expect_identical(issues$column, "date")
  expect_identical(issues$raw, "2026-07-24 provisional")
  expect_identical(issues$reason, "trailing_residue")

  # A clean date is not an issue.
  expect_equal(
    nrow(metadata_import_errors(DTAMetaData(title = "Clean", date = "2026-07-24"))),
    0
  )

  # Both axes report together, each under its own field path.
  md_both <- DTAMetaData(
    title = "Both",
    date = "2026-07-24 provisional",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )
  expect_identical(
    metadata_import_errors(md_both)$column,
    c("date", "transmission$date_last_transfer")
  )
})

test_that("metadata import errors surface through messages()", {
  md <- DTAMetaData(
    title = "Qualified Date",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )

  msgs <- messages(md, as_tibble = FALSE)

  expect_named(
    msgs,
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )
  expect_equal(nrow(msgs), 1)
  expect_identical(msgs$source, "import")
  expect_identical(msgs$severity, "error")
  expect_identical(msgs$target, "metadata")
  expect_identical(msgs$keyword, "trailing_residue")
  # The message must quote what was written, not only what was kept.
  expect_true(grepl("2026-12-31 at the earliest", msgs$message, fixed = TRUE))

  clean <- messages(DTAMetaData(title = "Clean"), as_tibble = FALSE)
  expect_equal(nrow(clean), 0)
})

test_that("DTAMetaData validator rejects invalid basics", {
  expect_error(DTAMetaData(title = "", version = "1.0"), "title")
  expect_error(DTAMetaData(title = "ok", version = ""), "version")
})

test_that("DTAMetaData validator checks version_history shape and changes", {
  expect_error(
    DTAMetaData(
      title = "invalid history",
      version_history = list("not_a_list")
    ),
    "must be a list"
  )

  expect_error(
    DTAMetaData(
      title = "missing fields",
      version_history = list(list(version = "1.0"))
    ),
    "missing required fields"
  )

  expect_error(
    DTAMetaData(
      title = "empty changes",
      version_history = list(list(
        version = "1.0",
        date = as.Date("2026-01-01"),
        changes = ""
      ))
    ),
    "changes cannot be an empty string"
  )
})

test_that("DTAMetaData validator checks transmission date field types", {
  expect_error(
    DTAMetaData(
      title = "bad first date",
      transmission = list(date_first_transfer = 123)
    ),
    "date_first_transfer"
  )

  expect_error(
    DTAMetaData(
      title = "bad last date",
      transmission = list(date_last_transfer = TRUE)
    ),
    "date_last_transfer"
  )
})

test_that("create_example_DTAMetaData returns expected structures", {
  md1 <- create_example_DTAMetaData(1)
  md2 <- create_example_DTAMetaData(2)
  md3 <- create_example_DTAMetaData(3)

  expect_s3_class(md1, "DTAtools::DTAMetaData")
  expect_s3_class(md2, "DTAtools::DTAMetaData")
  expect_s3_class(md3, "DTAtools::DTAMetaData")

  expect_equal(md2@version, "2.0")
  expect_equal(length(md2@version_history), 3)
  expect_true(length(md2@receiver$contacts) >= 1)
  expect_true(length(md2@supplier$contacts) >= 1)
  expect_s3_class(md2@transmission$date_first_transfer, "Date")
  expect_s3_class(md2@transmission$date_last_transfer, "Date")

  expect_type(md3@transmission$date_first_transfer, "character")
  expect_type(md3@transmission$date_last_transfer, "character")

  expect_error(create_example_DTAMetaData(99), "Invalid index")
})

test_that("metadata can be read from YAML into DTAMetaData", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  dta <- read_dta_from_yaml(path)
  md <- metadata(dta)

  expect_s3_class(md, class = "DTAtools::DTAMetaData")
  expect_true(nchar(md@title) > 0)
})

test_that("as.list(DTAMetaData) preserves key metadata fields", {
  md <- create_example_DTAMetaData(2)
  out <- as.list(md)

  expect_type(out, "list")
  expect_equal(out$title, md@title)
  expect_equal(out$version, md@version)
  expect_equal(as.character(out$date), as.character(md@date))
  expect_true("transmission" %in% names(out))
  expect_true("receiver" %in% names(out))
  expect_true("supplier" %in% names(out))
})

test_that("as.list(DTAMetaData) renders dates as ISO strings and omits import_issues", {
  md <- create_example_DTAMetaData(2)
  out <- as.list(md)

  expect_identical(out$date, "2026-01-15")
  expect_identical(
    vapply(out$version_history, function(h) h$date, character(1)),
    c("2025-10-01", "2025-12-01", "2026-01-15")
  )
  expect_identical(out$transmission$date_first_transfer, "2026-02-01")
  expect_identical(out$transmission$date_last_transfer, "2026-03-31")

  # Non-date transmission entries pass through unchanged.
  expect_identical(out$transmission$type, "Secure SFTP server")
  expect_false(out$transmission$test_upload)

  # @import_issues is a runtime artifact, not spec content: exporting it would
  # change every YAML round trip.
  expect_false("import_issues" %in% names(out))
})

test_that("DTAMetaData survives a write_yaml/read_yaml round trip", {
  # yaml has no date type: write_yaml() renders a Date as its day number
  # (date: 20468.0), and the constructor only coerces when is.character(date), so
  # reading it back used to abort with "@date must be S3<Date> ..., not <double>"
  # -- the DTA could be written but never read again.
  md <- create_example_DTAMetaData(2)

  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  yaml::write_yaml(as.list(md), path)

  # The file must carry dates as strings, not numbers.
  written <- readLines(path)
  expect_true(any(grepl("date: '2026-01-15'", written, fixed = TRUE)))
  expect_false(any(grepl("20468", written, fixed = TRUE)))

  round_tripped <- do.call(DTAMetaData, yaml::read_yaml(path))

  expect_s3_class(round_tripped, "DTAtools::DTAMetaData")
  expect_identical(round_tripped@date, md@date)
  expect_identical(
    get_transmission_dates(round_tripped),
    get_transmission_dates(md)
  )
  expect_equal(get_version_history_df(round_tripped), get_version_history_df(md))

  # Reading a faithfully written file must not itself invent import errors.
  expect_equal(nrow(metadata_import_errors(round_tripped)), 0)
})

test_that("get_authorized_for_corrections returns configured values", {
  md2 <- create_example_DTAMetaData(2)
  md3 <- create_example_DTAMetaData(3)

  auth2 <- get_authorized_for_corrections(md2)
  auth3 <- get_authorized_for_corrections(md3)

  expect_true(is.character(auth2) || is.list(auth2))
  expect_true(length(auth2) >= 1)
  expect_type(auth3, "character")
  expect_true(nchar(auth3) > 0)
})

test_that("get_receiver_reviewers handles reviewer extraction", {
  md <- create_example_DTAMetaData(2)

  reviewer_names <- get_receiver_reviewers(md, name_only = TRUE)
  reviewer_full <- get_receiver_reviewers(md, name_only = FALSE)

  expect_type(reviewer_names, "character")
  expect_true(length(reviewer_names) >= 1)
  expect_true(is.list(reviewer_full))
  expect_true(all(vapply(reviewer_full, function(x) isTRUE(x$reviewer), logical(1))))

  md_empty <- DTAMetaData(title = "No Receiver")
  expect_null(get_receiver_reviewers(md_empty))
})

test_that("get_receiver_reviewers selects only flagged contacts", {
  md <- DTAMetaData(
    title = "Mixed Contacts",
    receiver = list(contacts = list(
      list(name = "Bob", role = "Data Manager", email = "bob@example.com", reviewer = TRUE),
      list(name = "Eve", role = "Statistician", email = "eve@example.com")
    ))
  )

  expect_identical(get_receiver_reviewers(md, name_only = TRUE), "Bob")

  full <- get_receiver_reviewers(md, name_only = FALSE)
  expect_length(full, 1)
  expect_identical(full[[1]]$email, "bob@example.com")
})

test_that("get_receiver_reviewers returns list() when no contact is a reviewer (documented gap)", {
  # Deferred: with contacts present but none flagged `reviewer = TRUE`, the
  # empty filter result is returned as-is, so the return type depends on the
  # data rather than on `name_only`. Callers that expect character(0) from
  # `name_only = TRUE` (the type returned whenever at least one reviewer exists)
  # instead get an empty list, so `paste(collapse = ", ")` and friends behave
  # differently for "no reviewers" than for "some reviewers".
  #
  # Pinned, not asserted as correct: the desired result is character(0) for
  # name_only = TRUE, which needs a fix in R/DTAMetaData-helpers.R.
  md <- DTAMetaData(
    title = "No Reviewer Flag",
    receiver = list(contacts = list(
      list(name = "Bob", role = "Data Manager", email = "bob@example.com")
    ))
  )

  expect_identical(get_receiver_reviewers(md, name_only = TRUE), list())
  expect_identical(get_receiver_reviewers(md, name_only = FALSE), list())
})

test_that("get_transmission_dates returns expected first/last transfer", {
  md2 <- create_example_DTAMetaData(2)
  dates2 <- get_transmission_dates(md2)

  expect_true(all(c("first_transfer", "last_transfer") %in% names(dates2)))
  expect_s3_class(dates2$first_transfer, "Date")
  expect_s3_class(dates2$last_transfer, "Date")

  md_empty <- DTAMetaData(title = "No Transmission")
  dates_empty <- get_transmission_dates(md_empty)
  expect_null(dates_empty$first_transfer)
  expect_null(dates_empty$last_transfer)
})

test_that("get_version_history_df returns complete and empty shapes", {
  md2 <- create_example_DTAMetaData(2)
  hist2 <- get_version_history_df(md2)

  expect_true(is.data.frame(hist2))
  expect_equal(colnames(hist2), c("version", "date", "changes"))
  expect_equal(nrow(hist2), 3)
  expect_true(all(!is.na(hist2$version)))
  expect_true(all(!is.na(hist2$date)))

  md_empty <- DTAMetaData(title = "No History")
  hist_empty <- get_version_history_df(md_empty)
  expect_true(is.data.frame(hist_empty))
  expect_equal(nrow(hist_empty), 0)
  expect_equal(colnames(hist_empty), c("version", "date", "changes"))
  expect_s3_class(hist_empty$date, "Date")
})

test_that("get_version_history_df accepts an all-character history", {
  md <- DTAMetaData(
    title = "Character Dates",
    version_history = list(
      list(version = "1.0", date = "2026-01-01", changes = "Initial"),
      list(version = "2.0", date = "2026-02-01", changes = "Revised")
    )
  )

  hist <- get_version_history_df(md)
  expect_equal(nrow(hist), 2)
  expect_s3_class(hist$date, "Date")
  expect_identical(as.character(hist$date), c("2026-01-01", "2026-02-01"))
  expect_identical(hist$version, c("1.0", "2.0"))
})

test_that("get_version_history_df accepts a history mixing Date and character dates", {
  # A YAML-loaded DTA whose history was appended to in R produces exactly this
  # mix. sapply() used to flatten it to character, as.Date("20468") then failed,
  # and the blanket tryCatch reported "x must be a DTAMetaData object with
  # @version_history property" -- untrue, and it pointed at the wrong thing.
  md <- DTAMetaData(
    title = "Mixed Dates",
    version_history = list(
      list(version = "1.0", date = as.Date("2026-01-01"), changes = "Initial"),
      list(version = "2.0", date = "2026-02-01", changes = "Revised")
    )
  )

  hist <- get_version_history_df(md)

  expect_equal(nrow(hist), 2)
  expect_s3_class(hist$date, "Date")
  expect_identical(as.character(hist$date), c("2026-01-01", "2026-02-01"))
  expect_identical(hist$version, c("1.0", "2.0"))
  expect_identical(hist$changes, c("Initial", "Revised"))
})

test_that("get_version_history_df tolerates missing and unparseable record dates", {
  md <- DTAMetaData(
    title = "Sparse Dates",
    version_history = list(
      list(version = "1.0", date = as.Date("2026-01-01"), changes = "Initial"),
      list(version = "2.0", date = NULL, changes = "No date recorded"),
      list(version = "3.0", date = "not a date", changes = "Unparseable")
    )
  )

  hist <- get_version_history_df(md)

  expect_equal(nrow(hist), 3)
  expect_s3_class(hist$date, "Date")
  expect_identical(is.na(hist$date), c(FALSE, TRUE, TRUE))
  expect_identical(hist$version, c("1.0", "2.0", "3.0"))

  # A record without a date used to print as "v2.0 (NULL)", because
  # format(NULL, "%Y-%m-%d") returns the string "NULL" and `%||%` never fires.
  out <- capture.output(print_info(md), type = "message")
  expect_true(any(grepl("v1.0 (2026-01-01)", out, fixed = TRUE)))
  expect_true(any(grepl("v2.0 (N/A)", out, fixed = TRUE)))
  expect_false(any(grepl("NULL", out, fixed = TRUE)))
})

test_that("get_version_history_df still rejects a non-metadata object", {
  # Companion to the two tests above: narrowing the tryCatch to the property
  # access only (so that a genuine internal failure is no longer mislabelled)
  # must not remove the guard for the case the message was actually written for.
  expect_error(
    get_version_history_df("not metadata"),
    "version_history"
  )
})

test_that("validate_transmission_dates validates Date and phrase cases", {
  md_valid_dates <- DTAMetaData(
    title = "Valid Dates",
    transmission = list(
      date_first_transfer = as.Date("2026-01-01"),
      date_last_transfer = as.Date("2026-02-01")
    )
  )
  res_valid_dates <- validate_transmission_dates(md_valid_dates)
  expect_true(res_valid_dates$is_valid)

  md_valid_phrases <- DTAMetaData(
    title = "Valid Phrases",
    transmission = list(
      date_first_transfer = "after DB lock",
      date_last_transfer = "final transfer by 2026-12-31"
    )
  )
  res_valid_phrases <- validate_transmission_dates(md_valid_phrases)
  expect_true(res_valid_phrases$is_valid)

  md_bad_order <- DTAMetaData(
    title = "Bad Date Order",
    transmission = list(
      date_first_transfer = as.Date("2026-03-01"),
      date_last_transfer = as.Date("2026-02-01")
    )
  )
  res_bad_order <- validate_transmission_dates(md_bad_order)
  expect_false(res_bad_order$is_valid)
  expect_true(any(grepl("cannot be after", res_bad_order$messages)))

  md_bad_empty <- DTAMetaData(
    title = "Bad Empty Phrase",
    transmission = list(
      date_first_transfer = "",
      date_last_transfer = ""
    )
  )
  res_bad_empty <- validate_transmission_dates(md_bad_empty)
  expect_false(res_bad_empty$is_valid)
  expect_true(any(grepl("empty", res_bad_empty$messages)))

  md_empty <- DTAMetaData(title = "No Transmission")
  res_empty <- validate_transmission_dates(md_empty)
  expect_true(res_empty$is_valid)
})

test_that("print methods render the metadata they are given", {
  # cli writes its alerts to the message connection, so the output has to be
  # captured with type = "message". The previous version of this test captured
  # stdout (always empty here) and only asserted expect_no_error(), which would
  # have passed even if the print methods emitted nothing at all.
  md <- create_example_DTAMetaData(2)

  out <- capture.output(print(md), type = "message")
  expect_true(any(grepl("DTAMetaData", out, fixed = TRUE)))
  expect_true(any(grepl("Clinical Data Transfer Agreement", out, fixed = TRUE)))
  expect_true(any(grepl("Version: 2.0", out, fixed = TRUE)))
  expect_true(any(grepl("2026-01-15", out, fixed = TRUE)))

  info <- capture.output(print_info(md), type = "message")
  expect_true(any(grepl("Version History", info, fixed = TRUE)))
  expect_true(any(grepl("Initial version", info, fixed = TRUE)))
  expect_true(any(grepl("Receiver", info, fixed = TRUE)))
  expect_true(any(grepl("Supplier", info, fixed = TRUE)))
  expect_true(any(grepl("Transmission", info, fixed = TRUE)))
  expect_true(any(grepl("Alice Smith", info, fixed = TRUE)))

  short <- capture.output(print_short_info(md), type = "message")
  expect_length(short, 1)
  expect_match(short, "Clinical Data Transfer Agreement")
  expect_match(short, "2.0", fixed = TRUE)
  expect_match(short, "2026-01-15", fixed = TRUE)

  # print_short_info() stays short: it must not spill the full print_info body.
  expect_lt(length(short), length(info))
})

test_that("as.list(DTAMetaData) omits an absent field but keeps a blank one", {
  # One omission rule for every field. It used to be three -- four fields
  # written unconditionally, some gated on length() > 0, the rest on
  # !is.null() -- so a field nothing had set still reached the file as
  # `header: ~`. ABSENT is not written; present-but-blank is written, blank.
  # Keeping those apart is what lets a template say `key: null` (drop, not
  # shown) and `key: ""` (empty, shown blank) and have both survive to the file.
  absent <- DTAMetaData(title = "T", version = "1")
  blank <- DTAMetaData(title = "T", version = "1", header = "")

  expect_false("header" %in% names(as.list(absent)))
  expect_true("header" %in% names(as.list(blank)))
  expect_identical(as.list(blank)$header, "")
})

test_that("a field absent from as.list() is absent from the written YAML too", {
  # Asserting on the written file rather than on the list is the point: the
  # rule only matters if it survives yaml::write_yaml, which renders a NULL
  # element as `~` rather than dropping it.
  md <- DTAMetaData(title = "T", version = "1")
  path <- withr::local_tempfile(fileext = ".yaml")

  yaml::write_yaml(as.list(md), path)
  written <- yaml::read_yaml(path)

  expect_false("header" %in% names(written))
  expect_false("error_handling" %in% names(written))
  expect_equal(written$title, "T")
})

test_that("as.list(DTAMetaData) keeps a deliberately blank authorized_for_corrections", {
  # `authorized_for_corrections` defaults to NULL, so an empty list is an
  # author's explicit "no one, and I am saying so" -- the present-but-blank
  # state, not absence. The collection-valued properties below default to an
  # empty list, where the same shape means nothing was ever set; a single
  # omission rule over both would have to get one of them wrong.
  blank <- DTAMetaData(title = "T", version = "1", authorized_for_corrections = list())
  unset <- DTAMetaData(title = "T", version = "1")

  expect_true("authorized_for_corrections" %in% names(as.list(blank)))
  expect_false("authorized_for_corrections" %in% names(as.list(unset)))
  # An empty receiver IS the unset state for that property, so it stays out.
  expect_false("receiver" %in% names(as.list(blank)))
})
