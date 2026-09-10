# Transfer metadata. REQ-META-001 .. REQ-META-017.
#
# DTAMetaData describes the AGREEMENT rather than the data: who is sending
# what to whom, under whose authority a correction may be requested, and by
# when. None of it passes through the three validation axes, so a defect here
# is invisible to check() and would otherwise reach a signed document
# unnoticed.

# ---- fixtures -----------------------------------------------------------------

# One of each documented date-parsing outcome, built by hand so every branch
# is deliberately hit exactly once: a bare ISO date (top-level), an ISO date
# with trailing text (transmission), and a free-text phrase with no leading
# date (transmission).
qmeta_dates <- function() {
  DTAMetaData(
    title = "Date parsing probe",
    date = "2026-05-01",
    transmission = list(
      date_first_transfer = "2026-12-31 at the earliest",
      date_last_transfer = "2 weeks after approval"
    )
  )
}

# Every key the specification accepts, populated - including template, which
# create_example_DTAMetaData() never sets - so the YAML round trip below
# exercises the whole surface at once rather than the bundled example's
# particular subset of it.
qmeta_full <- function() {
  DTAMetaData(
    title = "Clinical Data Transfer Agreement",
    version = "2.0",
    date = as.Date("2026-01-15"),
    header = "Boehringer Ingelheim",
    version_history = list(
      list(version = "1.0", date = as.Date("2025-10-01"), changes = "Initial version"),
      list(version = "2.0", date = as.Date("2026-01-15"), changes = "Final review and approval")
    ),
    receiver = list(
      affiliation = list(name = "Test Company", country = "USA"),
      contacts = list(list(name = "Alice Smith", role = "Lead Data Manager", reviewer = TRUE))
    ),
    supplier = list(
      affiliation = list(name = "Supplier Company Inc.", country = "Germany")
    ),
    transmission = list(
      type = "Secure SFTP server",
      date_first_transfer = as.Date("2026-02-01"),
      date_last_transfer = as.Date("2026-03-31")
    ),
    error_handling = "Critical errors must be reported within 24 hours.",
    authorized_for_corrections = c("Alice Smith"),
    template = list(id = "clinical-transfer", version = "1.0")
  )
}

# ---- the validator --------------------------------------------------------------

test_that("OQ-META-001 | title and version reject an empty string, but not an unset one | REQ-META-001", {
  e_title <- tryCatch(DTAMetaData(title = ""), error = function(e) e)
  qa_check(
    "an empty title is rejected",
    inherits(e_title, "error") &&
      grepl("'title' cannot be an empty string", conditionMessage(e_title), fixed = TRUE)
  )

  e_version <- tryCatch(DTAMetaData(version = ""), error = function(e) e)
  qa_check(
    "an empty version is rejected",
    inherits(e_version, "error") &&
      grepl("'version' cannot be an empty string", conditionMessage(e_version), fixed = TRUE)
  )

  # Simply not supplying title/version is not the same thing as supplying an
  # empty one - only the empty STRING is rejected, never its absence.
  md <- DTAMetaData()
  qa_step("an unset title stays NULL", NULL, md@title)
  qa_step("an unset version stays NULL", NULL, md@version)
})

test_that("OQ-META-002 | version_history rejects a record that is not a list, or one missing a required field | REQ-META-002", {
  e_not_list <- tryCatch(DTAMetaData(version_history = list("v1.0")), error = function(e) e)
  qa_check(
    "a record that is not a list is rejected",
    inherits(e_not_list, "error") &&
      grepl("version_history[[1]] must be a list", conditionMessage(e_not_list), fixed = TRUE)
  )

  e_missing <- tryCatch(
    DTAMetaData(version_history = list(list(version = "1.0"))),
    error = function(e) e
  )
  qa_check(
    "the missing fields are named in the condition",
    inherits(e_missing, "error") &&
      grepl("missing required fields: date, changes", conditionMessage(e_missing), fixed = TRUE)
  )

  md <- DTAMetaData(version_history = list(
    list(version = "1.0", date = as.Date("2026-01-01"), changes = "Initial")
  ))
  qa_step("a complete record is accepted and kept as supplied", "1.0", md@version_history[[1]]$version)
})

test_that("OQ-META-003 | version_history rejects an empty changes string | REQ-META-003", {
  e <- tryCatch(
    DTAMetaData(version_history = list(
      list(version = "1.0", date = as.Date("2026-01-01"), changes = "")
    )),
    error = function(e) e
  )
  qa_check(
    "an empty changes string is rejected",
    inherits(e, "error") &&
      grepl("changes cannot be an empty string", conditionMessage(e), fixed = TRUE)
  )
})

test_that("OQ-META-004 | transmission dates reject a value that is neither Date nor character | REQ-META-004", {
  e_first <- tryCatch(DTAMetaData(transmission = list(date_first_transfer = 123)), error = function(e) e)
  qa_check(
    "a numeric date_first_transfer is rejected",
    inherits(e_first, "error") &&
      grepl("date_first_transfer must be a Date or character string", conditionMessage(e_first), fixed = TRUE)
  )

  e_last <- tryCatch(DTAMetaData(transmission = list(date_last_transfer = TRUE)), error = function(e) e)
  qa_check(
    "a logical date_last_transfer is rejected",
    inherits(e_last, "error") &&
      grepl("date_last_transfer must be a Date or character string", conditionMessage(e_last), fixed = TRUE)
  )
})

test_that("OQ-META-005 | template, when present, requires a non-empty id and version | REQ-META-005", {
  e_no_version <- tryCatch(DTAMetaData(template = list(id = "tpl1")), error = function(e) e)
  qa_check(
    "a template missing version is rejected",
    inherits(e_no_version, "error") &&
      grepl("template$version must be a single non-empty character string", conditionMessage(e_no_version), fixed = TRUE)
  )

  e_empty_id <- tryCatch(DTAMetaData(template = list(id = "", version = "1.0")), error = function(e) e)
  qa_check(
    "an empty template id is rejected",
    inherits(e_empty_id, "error") &&
      grepl("template$id must be a single non-empty character string", conditionMessage(e_empty_id), fixed = TRUE)
  )

  md <- DTAMetaData(template = list(id = "tpl1", version = "1.0"))
  qa_step("a complete template is kept verbatim", list(id = "tpl1", version = "1.0"), md@template)
})

# ---- date parsing -----------------------------------------------------------------

test_that("OQ-META-006 | a bare ISO date, a date with trailing text, and a dateless phrase parse three different ways | REQ-META-006 REQ-META-007 REQ-META-008", {
  md <- qmeta_dates()

  qa_step("a bare ISO date becomes a Date", "2026-05-01", format(md@date, "%Y-%m-%d"))
  qa_step(
    "a date with trailing text also becomes a Date",
    "2026-12-31", format(md@transmission$date_first_transfer, "%Y-%m-%d")
  )
  qa_step(
    "a phrase with no leading date is kept verbatim as text",
    "2 weeks after approval", md@transmission$date_last_transfer
  )
  qa_check(
    "the phrase is stored as character, never coerced towards Date",
    is.character(md@transmission$date_last_transfer)
  )

  issues <- metadata_import_errors(md)
  qa_step("exactly one import issue is recorded, for the trailing-residue field only", 1L, nrow(issues))
  qa_step(
    "the import issue names the field, keeps the raw value verbatim, and gives the reason",
    list(
      column = "transmission$date_first_transfer",
      raw = "2026-12-31 at the earliest",
      reason = "trailing_residue"
    ),
    list(column = issues$column[[1]], raw = issues$raw[[1]], reason = issues$reason[[1]])
  )
  qa_step("the import issue carries no row - metadata has none", TRUE, is.na(issues$row[[1]]))
})

test_that("OQ-META-007 | the top-level date field cannot hold a phrase, and is silently coerced to NA | REQ-META-009", {
  # Pinned, not endorsed (see REQ-META-009 notes): unlike a transmission date,
  # the top-level `date` property is typed Date and so cannot store a phrase
  # verbatim. A phrase with no leading ISO date supplied there is coerced the
  # way as.Date() would coerce it - to NA - and, unlike the trailing-residue
  # case, records no import issue at all: the loss is completely silent.
  md <- DTAMetaData(title = "t", date = "after approval")

  qa_step("the date becomes NA", TRUE, is.na(md@date))
  qa_step("the property keeps its Date class even when NA", "Date", class(md@date))
  qa_step("no import issue is recorded for the silently discarded phrase", 0L, length(md@import_issues))
  qa_known_deviation(
    "DEV-009",
    is.na(md@date) && length(md@import_issues) == 0L
  )
})

# ---- validate_transmission_dates() -------------------------------------------------

test_that("OQ-META-008 | validate_transmission_dates flags a first-transfer date after the last, and accepts everything else | REQ-META-010", {
  bad <- DTAMetaData(transmission = list(
    date_first_transfer = as.Date("2026-06-01"),
    date_last_transfer = as.Date("2026-01-01")
  ))
  result_bad <- validate_transmission_dates(bad)
  qa_step("an inverted date range is invalid", FALSE, result_bad$is_valid)
  qa_step(
    "the message names the problem",
    "date_first_transfer cannot be after date_last_transfer",
    result_bad$messages
  )

  ok <- DTAMetaData(transmission = list(
    date_first_transfer = as.Date("2026-01-01"),
    date_last_transfer = as.Date("2026-06-01")
  ))
  qa_step(
    "dates in order are valid, with the documented fixed message",
    list(is_valid = TRUE, messages = "All transmission dates are valid"),
    validate_transmission_dates(ok)
  )

  none <- DTAMetaData(title = "no transmission")
  qa_step(
    "no transmission data at all is reported valid, not an error",
    list(is_valid = TRUE, messages = "No transmission data to validate"),
    validate_transmission_dates(none)
  )
})

# ---- accessors ----------------------------------------------------------------------

test_that("OQ-META-009 | get_version_history_df returns its three documented columns, normalising every date form | REQ-META-011", {
  md <- DTAMetaData(version_history = list(
    list(version = "1.0", date = as.Date("2025-10-01"), changes = "Initial version"),
    list(version = "1.1", date = "2025-11-01", changes = "Loaded from YAML"),
    list(version = "1.2", date = NULL, changes = "No date supplied")
  ))
  hist <- get_version_history_df(md)

  qa_step("the frame carries exactly its documented columns", c("version", "date", "changes"), names(hist))
  qa_step("one row per record", 3L, nrow(hist))
  qa_step(
    "a Date record and a character record (as loaded from YAML) both normalise to the same Date column",
    c("2025-10-01", "2025-11-01"),
    format(hist$date[1:2], "%Y-%m-%d")
  )
  qa_step("a record with no date becomes NA rather than aborting the whole call", TRUE, is.na(hist$date[[3]]))
  qa_step("the date column stays typed Date throughout", "Date", class(hist$date))
})

test_that("OQ-META-010 | get_version_history_df on an empty history returns a typed zero-row frame | REQ-META-011", {
  hist <- get_version_history_df(DTAMetaData(title = "no history"))
  qa_step("the frame carries exactly its documented columns", c("version", "date", "changes"), names(hist))
  qa_step("it has zero rows", 0L, nrow(hist))
  qa_step("the date column is still typed Date, not logical or character", "Date", class(hist$date))
})

test_that("OQ-META-011 | get_authorized_for_corrections returns the property verbatim | REQ-META-012", {
  as_vector <- DTAMetaData(authorized_for_corrections = c("Alice Smith", "Bob Johnson"))
  qa_step(
    "a character vector is returned unchanged",
    c("Alice Smith", "Bob Johnson"), get_authorized_for_corrections(as_vector)
  )

  as_text <- DTAMetaData(authorized_for_corrections = "BI Data Management Team")
  qa_step("a single string is returned unchanged", "BI Data Management Team", get_authorized_for_corrections(as_text))

  unset <- DTAMetaData(title = "unset")
  qa_step("an unset property is NULL", NULL, get_authorized_for_corrections(unset))
})

test_that("OQ-META-012 | get_receiver_reviewers filters to reviewers and respects name_only | REQ-META-013", {
  md <- DTAMetaData(receiver = list(contacts = list(
    list(name = "Alice", reviewer = TRUE),
    list(name = "Bob", reviewer = FALSE),
    list(name = "Carol", reviewer = TRUE)
  )))

  qa_step(
    "name_only = TRUE (the default) returns only the reviewers' names, in contact order",
    c("Alice", "Carol"), get_receiver_reviewers(md)
  )

  full <- get_receiver_reviewers(md, name_only = FALSE)
  qa_step("name_only = FALSE returns the full contact records instead of just names", 2L, length(full))
  qa_step(
    "and every record returned really does carry the reviewer flag",
    c(TRUE, TRUE), vapply(full, function(contact) isTRUE(contact$reviewer), logical(1))
  )
})

test_that("OQ-META-013 | get_receiver_reviewers returns NULL when there is no receiver or no contacts | REQ-META-013", {
  no_receiver <- DTAMetaData(title = "no receiver")
  qa_step("no receiver at all", NULL, get_receiver_reviewers(no_receiver))

  no_contacts <- DTAMetaData(receiver = list(affiliation = list(name = "Acme")))
  qa_step("a receiver with an affiliation but no contacts list", NULL, get_receiver_reviewers(no_contacts))
})

test_that("OQ-META-014 | get_transmission_dates returns the two transfer dates, or NULL when unset | REQ-META-014", {
  md <- DTAMetaData(transmission = list(
    date_first_transfer = as.Date("2026-02-01"),
    date_last_transfer = as.Date("2026-03-31")
  ))
  qa_step(
    "both dates are returned verbatim, under their documented names",
    list(first_transfer = as.Date("2026-02-01"), last_transfer = as.Date("2026-03-31")),
    get_transmission_dates(md)
  )

  qa_step(
    "no transmission data returns both elements as NULL rather than aborting",
    list(first_transfer = NULL, last_transfer = NULL),
    get_transmission_dates(DTAMetaData(title = "none"))
  )
})

test_that("OQ-META-015 | metadata_import_errors carries the canonical import-error columns | REQ-META-015", {
  clean <- metadata_import_errors(DTAMetaData(title = "clean"))
  qa_step(
    "the frame carries exactly its documented columns",
    c("row", "column", "raw", "declared_type", "reason"), names(clean)
  )
  qa_step("a clean metadata object has zero import errors", 0L, nrow(clean))

  dirty <- metadata_import_errors(DTAMetaData(
    title = "dirty", transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  ))
  qa_step("one row is recorded for the one coerced value", 1L, nrow(dirty))
  qa_step("row is always NA - metadata has no rows to point at", TRUE, is.na(dirty$row[[1]]))
  qa_step("column carries the metadata field path", "transmission$date_last_transfer", dirty$column[[1]])
})

test_that("OQ-META-016 | messages surfaces a metadata object's own import issues | REQ-META-016", {
  clean <- as.data.frame(messages(DTAMetaData(title = "clean")))
  qa_step(
    "the frame carries the same ten columns as a table's messages",
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message"),
    names(clean)
  )
  qa_step("a clean metadata object reports no messages", 0L, nrow(clean))

  md <- DTAMetaData(title = "dirty", date = "2026-05-01 pending signature")
  msgs <- as.data.frame(messages(md))
  qa_step("one message is reported for the one import issue", 1L, nrow(msgs))
  qa_step(
    "it is tagged as an import finding against the metadata target, not any dataset",
    list(source = "import", target = "metadata", dataset = NA_character_),
    list(source = msgs$source[[1]], target = msgs$target[[1]], dataset = msgs$dataset[[1]])
  )
  qa_check(
    "the message names both the part that was kept and the part that was dropped",
    grepl("2026-05-01", msgs$message[[1]], fixed = TRUE) &&
      grepl("pending signature", msgs$message[[1]], fixed = TRUE)
  )
})

# ---- YAML round trip ------------------------------------------------------------------

test_that("OQ-META-017 | a full metadata document survives a YAML round trip | REQ-META-017", {
  original <- qmeta_full()
  path <- file.path(qa_tempdir(), "metadata.yaml")
  as_list <- as.list(original)
  yaml::write_yaml(as_list, path)

  expected_keys <- c(
    "authorized_for_corrections", "date", "error_handling", "header", "receiver",
    "supplier", "template", "title", "transmission", "version", "version_history"
  )
  qa_step("every accepted key the fixture set is written", expected_keys, sort(names(as_list)))

  raw <- qa_read_yaml(path)
  qa_step("the written date is an ISO string, not the underlying numeric day count", "2026-01-15", raw$date)
  qa_check("it is character, never numeric", is.character(raw$date))

  restored <- do.call(DTAMetaData, raw)

  qa_step("title survives", original@title, restored@title)
  qa_step("version survives", original@version, restored@version)
  qa_step("date survives", original@date, restored@date)
  qa_step("header survives", original@header, restored@header)
  qa_step("error_handling survives", original@error_handling, restored@error_handling)
  qa_step(
    "authorized_for_corrections survives",
    original@authorized_for_corrections, restored@authorized_for_corrections
  )
  qa_step("receiver survives, contacts and all", original@receiver, restored@receiver)
  qa_step("supplier survives", original@supplier, restored@supplier)
  qa_step("template survives - it is machine-owned provenance and must round-trip exactly", original@template, restored@template)
  qa_step(
    "transmission's own dates survive, once read back through the same date parser that wrote them",
    format(original@transmission$date_first_transfer, "%Y-%m-%d"),
    format(restored@transmission$date_first_transfer, "%Y-%m-%d")
  )

  qa_step(
    "version_history's own dates come back as character (as loaded from YAML), which the class documents as legitimate",
    "character", class(restored@version_history[[1]]$date)
  )
  qa_step(
    "and get_version_history_df() still normalises them back to the original Date",
    format(original@version_history[[1]]$date, "%Y-%m-%d"),
    format(get_version_history_df(restored)$date[[1]], "%Y-%m-%d")
  )
})

test_that("OQ-META-018 | import_issues is never written to YAML, and a round trip permanently loses the qualifying text that caused one | REQ-META-017 REQ-META-018", {
  original <- DTAMetaData(title = "t", transmission = list(date_last_transfer = "2026-12-31 at the earliest"))
  qa_step("the source object has one import issue", 1L, nrow(metadata_import_errors(original)))

  as_list <- as.list(original)
  qa_check(
    "as.list() never carries import_issues - it is derived from the data, not author-supplied",
    !"import_issues" %in% names(as_list)
  )
  qa_step(
    "and the qualifying text itself is gone too: only the resolved Date is serialized",
    "2026-12-31", as_list$transmission$date_last_transfer
  )

  path <- file.path(qa_tempdir(), "dirty.yaml")
  yaml::write_yaml(as_list, path)
  restored <- do.call(DTAMetaData, qa_read_yaml(path))

  # Pinned, not endorsed (see REQ-META-018 notes): this is NOT the same
  # import issue recomputed - it is gone. The reloaded object reads back as a
  # clean, unqualified date, with nothing left to say it was ever qualified.
  qa_step(
    "the reloaded document reports zero import issues, not the one it started with",
    0L, nrow(metadata_import_errors(restored))
  )
  qa_step(
    "because the date itself now reads back as a plain, unqualified ISO date",
    "2026-12-31", format(restored@transmission$date_last_transfer, "%Y-%m-%d")
  )
  qa_known_deviation(
    "DEV-010",
    nrow(metadata_import_errors(original)) == 1L &&
      nrow(metadata_import_errors(restored)) == 0L
  )
})
