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

test_that("qualified transmission dates lose their qualification (documented defect)", {
  # DEFECT, deferred: the constructor coerces a transmission date string with
  # strptime()/as.Date(), and those ignore TRAILING text. A phrase is therefore
  # only preserved when it does NOT start with a parseable date:
  #   "after approval"              -> kept as character (leading text)
  #   "2026-12-31 at the earliest"  -> silently becomes Date 2026-12-31
  #
  # The second case drops a contractual qualification from a data transfer
  # agreement: "at the earliest" states a lower bound, but the stored value
  # reads as a firm date, and downstream consumers (get_transmission_dates(),
  # validate_transmission_dates(), the exported DTA document) all see a hard
  # deadline that was never agreed. The fix belongs in the coercion helper in
  # R/DTAMetaData-class.R, which must reject trailing residue instead of
  # discarding it.
  #
  # Pinned here, NOT asserted as correct: the desired assertion is that the
  # value stays the character string "2026-12-31 at the earliest".
  md <- DTAMetaData(
    title = "Qualified Date",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )

  expect_s3_class(md@transmission$date_last_transfer, "Date")
  expect_identical(as.character(md@transmission$date_last_transfer), "2026-12-31")

  # Contrast: a phrase whose leading characters are unparseable survives intact,
  # which is why the existing "after approval" test above passes.
  md_leading <- DTAMetaData(
    title = "Leading Phrase",
    transmission = list(date_last_transfer = "final transfer by 2026-12-31")
  )
  expect_type(md_leading@transmission$date_last_transfer, "character")
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

