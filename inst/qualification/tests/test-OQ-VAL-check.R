# check() and its arguments. REQ-VAL-014 .. REQ-VAL-021; DEV-002 under REQ-VAL-015.
#
# Every argument of check() changes what a validation run does or what it
# records, and each is tested against a stated expected outcome rather than
# against whatever the run produced. The arguments that decide whether work is
# repeated matter most: a stale verdict presented as a current one is worse
# than no verdict at all.

vc_specs <- function(cols, rules = list()) {
  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = rules
  )
}

vc_id_specs <- function() {
  vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
}

# A loaded, unchecked dataset backed by a file on disk, which is the shape a
# real delivery arrives in.
vc_dataset <- function(frame, specs, dir, id = "qual", stream = "never") {
  path <- file.path(dir, paste0("c", as.integer(stats::runif(1, 1, 1e9)), ".csv"))
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  ds <- DTADataSetTabular(
    id = id, specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  load_file(ds, file = path, handler_index = 1, stream = stream)
}

# What THIS call did, as opposed to what is on record for the table.
vc_call_status <- function(ds) {
  attr(ds, "last_validation_summary")$status[[1]]
}

test_that("OQ-VAL-020 | only the named tables are validated | REQ-VAL-014", {
  dir <- qa_tempdir()
  specs <- vc_id_specs()

  # Two deliveries in one dataset, which is what a transfer with several files
  # looks like. The table names come from the file names.
  one <- file.path(dir, "one.csv")
  two <- file.path(dir, "two.csv")
  utils::write.csv(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE), one, row.names = FALSE)
  utils::write.csv(data.frame(ID = c("B001", "B002"), stringsAsFactors = FALSE), two, row.names = FALSE)

  ds <- DTADataSetTabular(
    id = "qual", specs = specs,
    files = list(DTAFileCSV(filename = "one.csv"), DTAFileCSV(filename = "two.csv"))
  )
  ds <- load_file(ds, file = one, handler_index = 1)
  ds <- load_file(ds, file = two, handler_index = 2)
  qa_step("both deliveries are loaded before the check", c("one", "two"), sort(names(tables(ds))))

  status <- validation_status(check(ds, tables = "one", persist = FALSE, quiet = TRUE))
  qa_step(
    "naming one table validates that one and leaves the other untouched",
    c(one = "validated", two = "not_validated"),
    c(
      one = status$status[status$table == "one"],
      two = status$status[status$table == "two"]
    )
  )
})

test_that("OQ-VAL-021 | an unchanged table is not revalidated, and says so | REQ-VAL-015", {
  dir <- qa_tempdir()
  ds <- vc_dataset(
    data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE),
    vc_id_specs(), dir
  )

  first <- check(ds, persist = FALSE, quiet = TRUE)
  qa_step("the first call does the work", "validated", vc_call_status(first))

  second <- check(first, persist = FALSE, quiet = TRUE)
  qa_step("the second call reuses the verdict", "skipped", vc_call_status(second))

  # The two are different questions and a reviewer has to be able to tell them
  # apart: the call was skipped, the table is still validated.
  qa_step(
    "the recorded verdict is unchanged by a skipped call",
    "validated", validation_status(second)$status
  )
  qa_step(
    "and the verdict itself survives",
    TRUE, validation_status(second)$ok
  )
})

test_that("OQ-VAL-022 | force revalidates, and so does a change to either input | REQ-VAL-016", {
  dir <- qa_tempdir()
  ds <- check(
    vc_dataset(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE), vc_id_specs(), dir),
    persist = FALSE, quiet = TRUE
  )

  qa_step(
    "force revalidates a table nothing has changed",
    "validated", vc_call_status(check(ds, persist = FALSE, quiet = TRUE, force = TRUE))
  )

  # A changed specification must not reuse a verdict reached under the old one.
  # This is the case that would silently pass a delivery against rules that no
  # longer apply.
  widened <- ds
  widened@specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "EXTRA", type = "SAS Char", length = 4, nullable = TRUE)
  ))
  qa_step(
    "a changed specification forces revalidation on its own",
    "validated", vc_call_status(check(widened, persist = FALSE, quiet = TRUE))
  )
  qa_step(
    "and the new specification is the one applied",
    FALSE, validation_status(check(widened, persist = FALSE, quiet = TRUE))$ok
  )
})

test_that("OQ-VAL-023 | a persisted verdict is readable back from its artifact | REQ-VAL-017", {
  dir <- qa_tempdir()
  artifacts <- file.path(dir, "artifacts")
  specs <- vc_specs(list(
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    )
  ))
  ds <- check(
    vc_dataset(data.frame(SEX = c("M", "X"), stringsAsFactors = FALSE), specs, dir),
    persist = TRUE, quiet = TRUE, artifact_dir = artifacts
  )
  table_name <- names(tables(ds))[[1]]

  from_memory <- validation_errors(ds, table_name, source = "memory")
  from_artifact <- validation_errors(ds, table_name, source = "artifact")

  qa_step(
    "the artifact reports the same verdict as the run that wrote it",
    list(ok = from_memory$ok, n = as.integer(from_memory$n_columnspec_errors)),
    list(ok = from_artifact$ok, n = as.integer(from_artifact$n_columnspec_errors))
  )
  qa_check("an artifact file was written", length(list.files(artifacts, recursive = TRUE)) > 0)

  # An artifact that has been moved or deleted must be an error, never an
  # empty result that would read as a clean table.
  unlink(list.files(artifacts, recursive = TRUE, full.names = TRUE), force = TRUE)
  err <- tryCatch(
    validation_errors(ds, table_name, source = "artifact"),
    error = function(e) e
  )
  qa_check("a missing artifact raises a condition", inherits(err, "condition"))
})

test_that("OQ-VAL-024 | max_errors caps what is kept, never what is counted | REQ-VAL-018", {
  dir <- qa_tempdir()
  specs <- vc_specs(list(
    DTAColumnSpec(
      id = "S", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    )
  ))
  frame <- data.frame(S = rep("X", 50L), stringsAsFactors = FALSE)
  ds <- check(vc_dataset(frame, specs, dir), persist = FALSE, quiet = TRUE, max_errors = 5L)

  # Fifty rows are wrong. A cap that also truncated the count would report a
  # delivery as five errors bad when it is fifty, which understates it exactly
  # when it is worst.
  qa_step(
    "every violation is counted",
    50L, as.integer(validation_status(ds)$n_columnspec_errors)
  )
  qa_step(
    "while only the capped number of details is retained",
    5L, nrow(as.data.frame(messages(ds)))
  )
})

test_that("OQ-VAL-025 | thread use changes scheduling, not the answer | REQ-VAL-019", {
  dir <- qa_tempdir()
  specs <- vc_specs(
    list(
      DTAColumnSpec(
        id = "ID", type = "SAS Char", length = 4, nullable = FALSE,
        pattern = "^[A-Z][0-9]{3}$"
      ),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
  )
  frame <- data.frame(
    ID = c("A001", "bad", "A003", "TOOLONG"),
    AGE = c("30", "99", "abc", "40"),
    stringsAsFactors = FALSE
  )

  # The same file both times. Two files would differ in name, and the target
  # column would then differ for a reason that has nothing to do with threads.
  path <- file.path(dir, "threads.csv")
  utils::write.csv(frame, path, row.names = FALSE, na = "")
  load_once <- function() {
    ds <- DTADataSetTabular(
      id = "qual", specs = specs,
      files = list(DTAFileCSV(filename = "threads.csv"))
    )
    load_file(ds, file = path, handler_index = 1, stream = "never")
  }
  threaded <- check(load_once(), persist = FALSE, quiet = TRUE, use_threads = TRUE)
  serial <- check(load_once(), persist = FALSE, quiet = TRUE, use_threads = FALSE)

  qa_step(
    "the same file reports the same messages with and without threads",
    qa_messages_norm(serial), qa_messages_norm(threaded)
  )
})

test_that("OQ-VAL-026 | a specification with no columns is unspecified, not clean | REQ-VAL-020", {
  dir <- qa_tempdir()
  ds <- suppressWarnings(check(
    vc_dataset(data.frame(A = c("1", "2"), stringsAsFactors = FALSE), vc_specs(list()), dir),
    persist = FALSE, quiet = TRUE
  ))
  status <- validation_status(ds)

  # Reporting an unchecked table as valid would be the worst failure this
  # package could have, so the distinct status carries real weight.
  qa_step("the status says the table was not specified", "unspecified", status$status)
  qa_step("and the verdict is unknown rather than true", NA, status$ok)

  # A table with nothing to check has nothing to report: zero rows in the
  # documented ten-column frame (REQ-VAL-023), not an abort.
  msgs <- messages(ds, as_tibble = FALSE)
  qa_step("messages() reports no rows for a table that had nothing to check", 0L, nrow(msgs))
  qa_step(
    "and still has the ten documented columns",
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message"),
    names(msgs)
  )
})

test_that("OQ-VAL-027 | clearing a validation returns the dataset to unvalidated | REQ-VAL-021", {
  dir <- qa_tempdir()
  ds <- check(
    vc_dataset(data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE), vc_id_specs(), dir),
    persist = FALSE, quiet = TRUE
  )
  qa_step("the dataset starts out validated", "validated", validation_status(ds)$status)

  cleared <- clear_validation(ds)
  qa_step(
    "clearing it discards the verdict rather than keeping a stale one",
    "not_validated", validation_status(cleared)$status
  )
  qa_step(
    "and the counts go with it",
    TRUE, is.na(validation_status(cleared)$ok)
  )
})

test_that("OQ-VAL-028 | validate_table reaches the same verdict without a dataset | REQ-VAL-027", {
  specs <- vc_specs(list(
    DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
      values = c("M", "F")
    )
  ))

  # The return is polymorphic: the table itself when it satisfies the
  # specification, and the error detail when it does not. Pinned as observed,
  # because a caller has to branch on the type rather than on a verdict flag.
  clean <- validate_table(
    specs = specs,
    table = data.frame(SEX = c("M", "F"), stringsAsFactors = FALSE),
    verbose = FALSE
  )
  qa_step(
    "a table satisfying the specification comes back as itself",
    c("data.frame", "2", "1"),
    c(class(clean)[[1]], as.character(nrow(clean)), as.character(ncol(clean)))
  )

  dirty <- validate_table(
    specs = specs,
    table = data.frame(SEX = c("M", "X"), stringsAsFactors = FALSE),
    verbose = FALSE
  )
  qa_step(
    "a violating table comes back as the error detail instead",
    "list", class(dirty)[[1]]
  )
  qa_step(
    "which names the one violation and its keyword",
    list(rows = 1L, keyword = "enum"),
    list(
      rows = nrow(dirty$full_error),
      keyword = as.character(dirty$full_error$keyword[[1]])
    )
  )
  qa_step(
    "and reports the other two axes as clean",
    c(rules = TRUE, import = TRUE),
    c(rules = dirty$rules_valid, import = dirty$import_valid)
  )
})

test_that("OQ-VAL-029 | a repeated column name is refused, not validated | REQ-VAL-028", {
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = FALSE)
  ))

  # The second VAL holds values outside anything the specification permits for
  # a declared numeric column, so a verdict of "clean" could only mean the
  # column was never looked at. Expected from how the frame is built, not from
  # what the engine says about it.
  repeated <- data.frame(
    ID = c("A001", "A002"),
    VAL = c(50, 60),
    VAL = c(999, -999),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  qa_step(
    "the frame really does carry one name twice",
    c(n_columns = 3L, n_distinct_names = 2L),
    c(
      n_columns = ncol(repeated),
      n_distinct_names = length(unique(names(repeated)))
    )
  )

  outcome <- tryCatch(
    {
      validate_table(specs = specs, table = repeated, verbose = FALSE)
      "returned a verdict"
    },
    error = function(e) "refused"
  )
  qa_step(
    "validating it is refused rather than answered",
    "refused", outcome
  )

  condition <- tryCatch(
    validate_table(specs = specs, table = repeated, verbose = FALSE),
    error = function(e) e
  )
  qa_check(
    "the refusal names the repeated column",
    grepl("VAL", conditionMessage(condition), fixed = TRUE),
    conditionMessage(condition)
  )

  # Repetition alone is what is refused: the same specification and the same
  # values, carried under distinct names, still reach a verdict.
  distinct <- data.frame(
    ID = c("A001", "A002"),
    VAL = c(50, 60),
    stringsAsFactors = FALSE
  )
  qa_step(
    "a table with distinct names is validated as before",
    "data.frame",
    class(validate_table(specs = specs, table = distinct, verbose = FALSE))[[1]]
  )
})

test_that("OQ-VAL-038 | an unchanged table rebuilt from its own contents is not revalidated | REQ-VAL-015", {
  # REQ-VAL-015 is about contents, not about which R object holds them: a
  # table rebuilt from the table on record carries the same contents, so the
  # next check() must reuse the verdict. Rebuilt the way a caller would --
  # read the stored table out and put an Arrow copy of it back.
  specs <- vc_specs(list(
    DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  ))
  frame <- function(tz) {
    data.frame(
      ID = c("a", "b"), VAL = c("1", "2"),
      W = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-02 11:00:00"), tz = tz),
      stringsAsFactors = FALSE
    )
  }
  rebuild_and_check <- function(tz) {
    ds <- DTADataSetTabular(id = "d", specs = specs, tables = list(t = frame(tz)))
    ds <- check(ds, persist = FALSE, quiet = TRUE)
    before <- validation_status(ds)
    ds@tables <- list(t = arrow::arrow_table(as.data.frame(ds@tables[["t"]])))
    ds <- check(ds, persist = FALSE, quiet = TRUE)
    list(call = vc_call_status(ds), before = before, after = validation_status(ds))
  }

  # A timestamp with a named timezone: the requirement holds.
  named <- rebuild_and_check("UTC")
  qa_step("a rebuilt table with a named timezone is skipped", "skipped", named$call)

  # A timestamp with no named timezone is the one type that is not stable
  # under repeated Arrow round trips, so the rebuild hashes differently and
  # the same contents are scanned again.
  unnamed <- rebuild_and_check("")
  qa_known_deviation("DEV-002", identical(unnamed$call, "validated"))

  # The deviation's impact statement: cost only, never a different answer.
  counts <- c("ok", "n_columnspec_errors", "n_rule_errors", "n_import_errors")
  qa_step(
    "the rescan reaches the same verdict and the same counts",
    unnamed$before[, counts],
    unnamed$after[, counts]
  )
})
