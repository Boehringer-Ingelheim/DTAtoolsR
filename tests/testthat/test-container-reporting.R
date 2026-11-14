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

# ---- WP5: document side ----------------------------------------------------

test_that("the template guide shows nothing for a `none` entry", {
  ds <- DTADataSetTabular(
    id = "ae", specs = cont_specs("ae"),
    files = list(DTAFileCSV("ae.csv", container = "none"), DTAFileCSV("ae2.csv", container = "sdtm.zip"))
  )
  kv <- .tgc_about(ds, 3, 0)
  expect_identical(kv$value[kv$key == "Delivered in"], "sdtm.zip")

  only_none <- DTADataSetTabular(
    id = "ae", specs = cont_specs("ae"),
    files = list(DTAFileCSV("ae.csv", container = "none"))
  )
  expect_false("Delivered in" %in% .tgc_about(only_none, 3, 0)$key)
})

# ---- WP4 report: container findings and the Container column ------------------

# ae must arrive inside sdtm_csv.zip (the default container); dm opts out.
# ae and dm come out of the zip; vs is delivered loose (outside); dm is then
# read again on its own, so its provenance has no container.
wp4_checked <- function() {
  d <- DTA(
    datasets = list(
      tab_ds("ae", list(DTAFileCSV("ae.csv"))),
      tab_ds("dm", list(DTAFileCSV("dm.csv", container = "none"))),
      tab_ds("vs", list(DTAFileCSV("vs.csv")))
    ),
    containers = list(DTAContainer("sdtm", "sdtm_csv.zip", default = TRUE))
  )
  d <- ld(d, cont_fx("sdtm_csv.zip"))
  d <- ld(d, cont_write("vs.csv", "USUBJID,VSTESTCD,VSSTRESN,VSDT\nS-001,SBP,120,01JAN2020\n"))
  suppressMessages(suppressWarnings(check(d, quiet = TRUE, persist = FALSE)))
}

wp4_findings <- function(dataset, status, message) {
  data.frame(
    dataset = dataset, item = dataset, file = paste0(dataset, ".csv"), container = "x.zip",
    arrived = "loose", status = status, message = message, stringsAsFactors = FALSE
  )
}

test_that("container rows: failed for outside, pending for the others", {
  x <- DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv")))))
  attr(x, "last_container_findings") <- rbind(
    wp4_findings("ae", "outside", "m1"),
    wp4_findings("dm", "unverified", "m2"),
    wp4_findings("vs", "unrecorded", "m3")
  )
  res <- .report_with_container_rows(x, data.frame(
    dataset = "ae", target = "ae", target_type = "table", status = "validated",
    n_columnspec_errors = 0L, n_rule_errors = 0L, n_import_errors = 0L, stringsAsFactors = FALSE
  ))
  added <- res[startsWith(res$target, "container"), ]
  expect_identical(added$dataset, c("ae", "dm", "vs"))
  expect_identical(added$target, c("container: ae.csv", "container: dm.csv", "container: vs.csv"))
  expect_identical(vapply(added$status, .report_status_label, "", USE.NAMES = FALSE), c("fail", "pending", "pending"))
  expect_identical(nrow(res), 4L)
})

test_that("container messages carry the source and the severity", {
  x <- DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv")))))
  attr(x, "last_container_findings") <- rbind(
    wp4_findings("ae", "outside", "m1"),
    wp4_findings("dm", "unverified", "m2")
  )
  msgs <- data.frame(
    id = 1:2, dataset = "ae", target = "ae", severity = "error", source = "columnspec",
    message = c("a", "b"), stringsAsFactors = FALSE
  )
  out <- .report_with_container_messages(x, msgs)
  added <- out[out$source == "container", ]
  expect_identical(added$id, 3:4)
  expect_identical(added$severity, c("error", "warning"))
  expect_identical(added$message, c("m1", "m2"))
  expect_identical(added$target, c("container: ae.csv", "container: dm.csv"))
})

test_that("without findings the report frames are returned unchanged", {
  x <- DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv")))))
  res <- data.frame(dataset = "ae", target = "ae", status = "validated")
  expect_identical(.report_with_container_rows(x, res), res)
  attr(x, "last_container_findings") <- wp4_findings("ae", "outside", "m")[0, ]
  expect_identical(.report_with_container_rows(x, res), res)
})

test_that("the inputs table names the container a file came from", {
  x <- wp4_checked()
  df <- .report_inputs_df(x)
  expect_identical(names(df)[ncol(df)], "Container")
  expect_identical(df$Container[df$Table == "ae"], "sdtm_csv.zip")
  expect_identical(df$Container[df$Table == "vs"], "loose")
  # dm opts out of containers: nothing to say, read again on its own or not.
  x <- load_file(x, dataset = "dm", file = cont_write("dm.csv", CONT_DM_CSV))
  expect_identical(.report_inputs_df(x)$Container[.report_inputs_df(x)$Dataset == "dm"], "")
  # A file under a container entry, read on its own, or bound without a record.
  y <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))), tab_ds("vs", list(DTAFileCSV("vs.csv")))),
    containers = list(DTAContainer("sdtm", "sdtm_csv.zip", default = TRUE))
  )
  y <- load_file(y, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  ds <- y@datasets[["vs"]]
  ds@tables <- list(vs = arrow::arrow_table(USUBJID = "S-001"))
  y@datasets[["vs"]] <- ds
  df <- .report_inputs_df(y)
  expect_identical(df$Container[df$Table == "ae"], "on its own")
  expect_identical(df$Container[df$Table == "vs"], "not recorded")
})

test_that("an unchecked or empty frame still has the Container column", {
  expect_true("Container" %in% names(.report_inputs_df(DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))))))))
})

test_that("the generated report shows the finding, its message and the Container column", {
  x <- wp4_checked()
  f <- attr(x, "last_container_findings")
  expect_identical(f$status, "outside")
  expect_identical(f$dataset, "vs")
  out <- withr::local_tempfile(fileext = ".html")
  write_validation_report(x, out, quiet = TRUE)
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("<th>Container</th>", html, fixed = TRUE))
  expect_true(grepl("<td>sdtm_csv.zip</td>", html, fixed = TRUE))
  expect_true(grepl("arrived loose; the agreement requires it inside", html, fixed = TRUE))
  expect_true(grepl('data-dataset="vs" data-target="container: vs.csv" data-status="fail"', html, fixed = TRUE))
  expect_true(grepl('data-dataset="vs" data-target="container: vs.csv" data-source="container"', html, fixed = TRUE))
})

test_that("the report cards tell outside, unverified and unrecorded apart", {
  d <- DTA(
    datasets = list(
      tab_ds("ae", list(DTAFileCSV("ae.csv"))),
      tab_ds("dm", list(DTAFileCSV("dm.csv"))),
      tab_ds("vs", list(DTAFileCSV("vs.csv")))
    ),
    containers = list(DTAContainer("sdtm", "sdtm_csv.zip", default = TRUE))
  )
  d <- ld(d, cont_write("ae.csv", CONT_AE_CSV))
  d <- load_file(d, dataset = "dm", file = cont_write("dm.csv", CONT_DM_CSV))
  ds <- d@datasets[["vs"]]
  ds@tables <- list(vs = arrow::arrow_table(
    USUBJID = "S-001", VSTESTCD = "SBP", VSSTRESN = 120, VSDT = as.Date("2020-01-01")
  ))
  d@datasets[["vs"]] <- ds
  x <- suppressMessages(suppressWarnings(check(d, quiet = TRUE, persist = FALSE)))
  expect_identical(sort(attr(x, "last_container_findings")$status), c("outside", "unrecorded", "unverified"))
  out <- withr::local_tempfile(fileext = ".html")
  write_validation_report(x, out, quiet = TRUE)
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  count <- function(p) sum(gregexpr(p, html, fixed = TRUE)[[1]] > 0)
  # One overview row and one message card per finding, each with its own target.
  expect_identical(count('data-target="container: ae.csv" data-status="fail"'), 1L)
  expect_identical(count('data-target="container: dm.csv" data-status="pending"'), 1L)
  expect_identical(count('data-target="container: vs" data-status="pending"'), 1L)
  expect_identical(count('data-source="container"'), 3L)
  expect_true(grepl("ae.csv (dataset &#39;ae&#39;) arrived loose", html, fixed = TRUE))
  expect_true(grepl("dm.csv (dataset &#39;dm&#39;) was checked on its own", html, fixed = TRUE))
  expect_true(grepl("it is not recorded how this file arrived", html, fixed = TRUE))
  # The inputs table says how each arrived.
  expect_true(grepl("<td>loose</td>", html, fixed = TRUE))
  expect_true(grepl("<td>on its own</td>", html, fixed = TRUE))
  expect_true(grepl("<td>not recorded</td>", html, fixed = TRUE))
})
