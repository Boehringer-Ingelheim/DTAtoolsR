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

test_that("container matching ignores pattern and a compression suffix", {
  h <- DTAFileCSV("ae.csv", container = "sdtm_*.zip")
  expect_true(dta_matches_container(h, "sdtm_20251101.zip"))
  expect_false(dta_matches_container(h, "sdtm.zip"))
  expect_true(dta_matches_container(h, c("lib.zip", "sdtm_1.zip")))

  expect_true(dta_matches_container(
    DTAFileXPT("ae.xpt", container = "sdtm.xpt"),
    "sdtm.xpt.gz"
  ))

  # `pattern` describes the file name; the container is matched on its own.
  hp <- DTAFileCSV("^ae", pattern = TRUE, container = "sdtm.zip")
  expect_true(dta_matches_container(hp, "sdtm.zip"))
  expect_false(dta_matches_container(hp, "sdtmXzip"))

  # Letter case must match.
  expect_false(dta_matches_container(
    DTAFileCSV("ae.csv", container = "sdtm.zip"),
    "SDTM.zip"
  ))

  # A handler without a container matches no name.
  expect_false(dta_matches_container(DTAFileCSV("ae.csv"), c("sdtm.zip", "lib.xpt")))
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
