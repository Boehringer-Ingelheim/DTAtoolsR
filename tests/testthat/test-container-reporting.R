test_that("the Files table gains Delivered In only when an entry declares a container", {
  plain <- .file_specs_df(list(DTAFileCSV("ae.csv"), DTAFileCSV("dm.csv")))
  expect_identical(
    names(plain),
    c("File Name / Pattern", "Match Type", "Format", "Expected Count", "Description")
  )

  declared <- .file_specs_df(list(DTAFileCSV("ae.csv", container = "sdtm.zip"), DTAFileCSV("dm.csv")))
  expect_identical(
    names(declared),
    c("File Name / Pattern", "Match Type", "Format", "Expected Count", "Description", "Delivered In")
  )
  expect_identical(declared[["Delivered In"]], c("sdtm.zip", ""))
})

test_that("the Word Files table has six columns when a container is declared", {
  with_container <- .build_file_specs_table(list(DTAFileCSV("ae.csv", container = "sdtm.zip")))
  expect_identical(flextable::ncol_keys(with_container), 6L)
  # The columns are rescaled to the page width together, so the specified
  # widths (1.2 for the sixth, 2.5 for the fifth) survive as a ratio.
  w <- with_container$body$colwidths
  expect_equal(w[[6]] / w[[5]], 1.2 / 2.5)

  without <- .build_file_specs_table(list(DTAFileCSV("ae.csv")))
  expect_identical(flextable::ncol_keys(without), 5L)
})

test_that("the format label names SAS transport", {
  expect_identical(.file_format_label(DTAFileXPT("ae.xpt")), "SAS transport (.xpt)")
  expect_identical(.file_format_label(DTAFileSAS7BDAT("ae.sas7bdat")), "SAS dataset (.sas7bdat)")
  expect_identical(.tgc_file_type(DTAFileXPT("ae.xpt")), "SAS transport (.xpt)")
})

test_that("the Markdown Files table shows Delivered In", {
  ds <- create_example_DTADataSetTabular(2)
  out <- withr::local_tempfile(fileext = ".md")
  ds@files <- list(DTAFileCSV(filename = "x.csv", container = "sdtm.zip"))
  write_dataset_metadata(ds, out, format = "md", quiet = TRUE)
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  expect_true(
    "| File Name / Pattern | Match Type | Format | Expected Count | Delivered In |" %in% lines
  )
})

test_that("report inputs name the container through the extraction path", {
  d <- suppressMessages(load_delivery(cont_dta("csv"), cont_fx("sdtm_csv.zip"), stream = "always"))
  df <- .report_inputs_df(d)
  shown <- df$`Source file`[df$Table == "ae"]
  expect_length(shown, 1L)
  expect_true(grepl("1-sdtm_csv.zip", shown, fixed = TRUE))
})

test_that("the template guide states the container", {
  with_container <- DTADataSetTabular(
    id = "ae", specs = cont_specs("ae"),
    files = list(DTAFileCSV("ae.csv", container = "sdtm.zip"))
  )
  kv <- .tgc_about(with_container, 3, 0)
  expect_identical(kv$value[kv$key == "Delivered in"], "sdtm.zip")
  # It sits right after the file type; the other keys are unchanged.
  expect_identical(kv$key, c("File name", "File type", "Delivered in", "Columns", "Rules"))

  # Several entries: the distinct containers, in order, joined with ", ".
  several <- DTADataSetTabular(
    id = "ae", specs = cont_specs("ae"),
    files = list(
      DTAFileCSV("ae.csv", container = "sdtm.zip"),
      DTAFileCSV("ae2.csv", container = "sdtm.zip"),
      DTAFileCSV("ae3.csv", container = "extra.zip"),
      DTAFileCSV("ae4.csv")
    )
  )
  kv <- .tgc_about(several, 3, 0)
  expect_identical(kv$value[kv$key == "Delivered in"], "sdtm.zip, extra.zip")

  without <- DTADataSetTabular(
    id = "ae", specs = cont_specs("ae"),
    files = list(DTAFileCSV("ae.csv"))
  )
  kv <- .tgc_about(without, 3, 0)
  expect_false("Delivered in" %in% kv$key)
  expect_identical(kv$key, c("File name", "File type", "Columns", "Rules"))
})
