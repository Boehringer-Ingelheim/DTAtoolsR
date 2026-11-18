# The `container` property of a file handler: the name of the .zip archive or
# .xpt SAS transport file a file arrives inside. Only load_delivery() consults
# it; these tests cover the property, its validation, the matching rule
# and the places that carry it.
#
# Every expected value is written out from the specification (the documented
# sentences, the glob rules), never taken from the code under test.

test_that("every handler type keeps a declared container", {
  for (type in c("csv", "tsv", "any", "sas7bdat", "xpt")) {
    handler <- DTAFileFactory(
      type = type,
      filename = paste0("x.", type),
      container = "sdtm.zip"
    )
    expect_identical(handler@container, "sdtm.zip", info = type)
  }

  expect_identical(
    DTAFileDelim("x.psv", sep = "|", container = "a.zip")@container,
    "a.zip"
  )
})

test_that("container defaults to NULL", {
  expect_null(DTAFileCSV("x.csv")@container)
  expect_null(dta_handler_container(DTAFileCSV("x.csv")))
})

test_that("a container must be one .zip or .xpt file name", {
  refused <- list(
    "sdtm.tar", "dir/sdtm.zip", "dir\\sdtm.zip", " sdtm.zip", "sdtm.zip ",
    "j\n.zip", "sdtm.zip.gz", c("a.zip", "b.zip"), "", NA_character_, 3, TRUE
  )
  for (v in refused) {
    expect_error(
      DTAFileCSV("x.csv", container = v),
      class = "dta_container_invalid"
    )
  }

  accepted <- c(
    "SDTM.ZIP", "lib.XPT", "sdtm.xpt.gz", "sdtm_*.zip", "STUDY?.xpt",
    "sdtm (final).zip"
  )
  for (v in accepted) {
    expect_identical(DTAFileCSV("x.csv", container = v)@container, v)
  }
})

test_that("the messages are the documented sentences", {
  expect_identical(
    dta_container_value_problem("sdtm.tar"),
    "The container 'sdtm.tar' must name a .zip archive or a .xpt SAS transport file."
  )
  expect_identical(
    dta_container_value_problem("deliveries/sdtm.zip"),
    "The container 'deliveries/sdtm.zip' must be a file name, not a path."
  )
  expect_identical(
    dta_container_value_problem(" sdtm.zip"),
    "The container ' sdtm.zip' must be a file name without control characters or leading or trailing spaces."
  )
  for (v in list(c("a.zip", "b.zip"), "", NA_character_, 3)) {
    expect_identical(
      dta_container_value_problem(v),
      "The container of a file entry must be a single non-empty file name."
    )
  }
  expect_null(dta_container_value_problem(NULL))
  expect_null(dta_container_value_problem("sdtm.zip"))

  # A name holding cli braces is shown as typed: the validator's sentence goes
  # through "{problem}", it is never the format string itself.
  e <- expect_error(
    DTAFileCSV("x.csv", container = "{1+1}.tar"),
    class = "dta_container_invalid"
  )
  expect_match(conditionMessage(e), "{1+1}", fixed = TRUE)
})

test_that("assigning a bad container is refused", {
  h <- DTAFileCSV("x.csv")
  expect_error(h@container <- "x.tar", class = "dta_container_invalid")
})

test_that("a YAML entry carries container through dta_file_handlers_from_list", {
  handler <- dta_file_handlers_from_list(
    list(type = "csv", filename = "ae.csv", container = "sdtm.zip")
  )[[1]]
  expect_identical(handler@container, "sdtm.zip")

  expect_error(
    dta_file_handlers_from_list(
      list(type = "csv", filename = "ae.csv", container = list("a.zip", "b.zip"))
    ),
    class = "dta_container_invalid"
  )
})

test_that("wildcards match whole names, every other character literally", {
  m <- function(g, x) grepl(dta_container_regex(g), x, perl = TRUE)

  expect_identical(
    m("sdtm.zip", c("sdtm.zip", "sdtm.zip.bak", "old_sdtm.zip", "SDTM.zip", "sdtmXzip")),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    m("sdtm_*.zip", c("sdtm_20251101.zip", "sdtm_.zip", "sdtm.zip", "sdtm_1.zip.bak")),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_identical(
    m("sdtm_????????.zip", c("sdtm_20251101.zip", "sdtm_2025.zip")),
    c(TRUE, FALSE)
  )
  for (g in c("sdtm (final).zip", "sdtm+adam.zip", "sdtm[1].zip", "a^b$.zip", "a{1}.zip", "a|b.zip")) {
    expect_true(m(g, g), info = g)
  }
})

test_that("glob matching ignores pattern and a compression suffix", {
  expect_true(dta_glob_matches("sdtm_*.zip", "sdtm_20251101.zip"))
  expect_false(dta_glob_matches("sdtm_*.zip", "sdtm.zip"))
  expect_true(dta_glob_matches("sdtm_*.zip", c("lib.zip", "sdtm_1.zip")))
  expect_true(dta_glob_matches("sdtm.xpt", "sdtm.xpt.gz"))

  # Letter case must match, unless asked otherwise.
  expect_false(dta_glob_matches("sdtm.zip", "SDTM.zip"))
  expect_true(dta_glob_matches("sdtm.zip", "SDTM.zip", ignore.case = TRUE))

  # Nothing to match against.
  expect_false(dta_glob_matches("sdtm.zip", character()))
  expect_false(dta_glob_matches(NULL, "sdtm.zip"))

  # The regex of a glob is built once.
  testthat::local_mocked_bindings(
    dta_container_regex = function(glob) stop("rebuilt")
  )
  expect_true(dta_glob_matches("sdtm_*.zip", "sdtm_9.zip"))
})

test_that("references are disjoint by syntax", {
  # ids
  for (ok in c("sdtm", "S_d-1", "a")) {
    expect_identical(DTAFileCSV("x.csv", container = ok)@container, ok)
  }
  expect_identical(DTAFileCSV("x.csv", container = "none")@container, "none")
  for (bad in list("1a", "a/b", "a b", "-a", "_a")) {
    e <- expect_error(DTAFileCSV("x.csv", container = bad), class = "dta_container_invalid")
    expect_match(conditionMessage(e), "must be a .zip/.xpt file name or a container id", fixed = TRUE)
  }
  expect_error(DTAFileCSV("x.csv", container = 3), class = "dta_container_invalid")
  # a dotted value is a file name and keeps its 0.27.0 judge
  expect_identical(
    dta_container_ref_problem("sdtm.tar"),
    dta_container_value_problem("sdtm.tar")
  )
  expect_null(dta_container_ref_problem(NULL))
  # assignment is refused too
  h <- DTAFileCSV("x.csv")
  expect_error(h@container <- "a/b", class = "dta_container_invalid")
  h@container <- "sdtm"
  expect_identical(h@container, "sdtm")
})

test_that("an entry takes a container kind by its type", {
  csv <- DTAFileCSV("a.csv")
  xpt <- DTAFileXPT("a.xpt")
  any <- DTAFileAny("a.pdf")
  expect_identical(dta_entry_takes_kind(csv, c("zip", "xpt", NA)), c(FALSE, FALSE, TRUE))
  expect_identical(dta_entry_takes_kind(xpt, c("zip", "xpt", NA)), c(FALSE, TRUE, TRUE))
  expect_identical(dta_entry_takes_kind(any, c("zip", "xpt", NA)), c(TRUE, TRUE, TRUE))
})

test_that("load_file refuses a container, naming the delivery route", {
  ds <- tab_ds("ae", list(DTAFileCSV("^.*$", pattern = TRUE)))
  p <- cont_write("deliveries.zip", "x")
  e <- expect_error(load_file(ds, file = p), class = "dta_load_file_container")
  expect_match(conditionMessage(e), "load_delivery(", fixed = TRUE)
  expect_match(conditionMessage(e), "'deliveries.zip'", fixed = TRUE)
  expect_no_match(conditionMessage(e), dirname(p), fixed = TRUE)

  # A .xpt into a transport-file entry loads.
  fx <- cont_fx("adverse.xpt")
  ds2 <- tab_ds("ae", list(DTAFileXPT("adverse.xpt")))
  expect_gt(nrow(as.data.frame(tables(load_file(ds2, file = fx))[["adverse"]])), 0L)
})

test_that("a handler restored without the property reads as NULL", {
  # An autosave written before `container` existed restores handlers of a class
  # that lacks the property.
  Old <- S7::new_class("Old", properties = list(filename = S7::class_character))
  expect_null(dta_handler_container(Old(filename = "a.csv")))
})

test_that("read_file ignores a declared container", {
  p <- cont_write("ae.csv", CONT_AE_CSV)
  expect_equal(nrow(read_file(DTAFileCSV("ae.csv", container = "sdtm.zip"), p)), 2)
})

test_that("a file dataset keeps the container when it rebuilds a reader entry", {
  ds <- DTADataSetFile(
    id = "d",
    files = list(DTAFileCSV("x.csv", container = "a.zip"))
  )
  rebuilt <- ds@files[[1]]
  expect_true(inherits(rebuilt, "DTAtools::DTAFileAny"))
  expect_identical(rebuilt@container, "a.zip")
})

test_that("a missing declared file names its container", {
  ds <- DTADataSetFile(
    id = "define",
    files = list(DTAFileAny("define.xml", container = "sdtm.zip"))
  )
  ds2 <- check(ds, quiet = TRUE, persist = FALSE)
  expect_match(
    messages(ds2)$message,
    "Expected file 'define.xml' was not delivered inside 'sdtm.zip'.",
    fixed = TRUE
  )
})

test_that("a .zip given to a reading handler points to a delivery", {
  expect_error(
    read_file(DTAFileCSV("d.zip"), cont_write("d.zip", "x")),
    class = "dta_zip_not_readable"
  )
})

test_that("a list is never a container", {
  expect_error(
    DTAFileCSV("x.csv", container = list("a.zip")),
    class = "dta_container_invalid"
  )
})

test_that("type: zip points to container", {
  expect_error(
    DTAFileFactory(type = "zip", filename = "a.zip"),
    regexp = "container:",
    fixed = TRUE
  )
})

test_that("a missing file names the resolved glob when check() is given one", {
  ds <- DTADataSetFile(id = "d", files = list(DTAFileAny("define.xml", container = "sdtm")))
  named <- check(ds, quiet = TRUE, persist = FALSE, .container_globs = c("1" = "STUDY_*.zip"))
  expect_match(messages(named)$message, "was not delivered inside 'STUDY_*.zip'", fixed = TRUE)
  none <- check(ds, quiet = TRUE, persist = FALSE, .container_globs = c("1" = ""))
  expect_no_match(messages(none)$message, "delivered inside", fixed = TRUE)
})
