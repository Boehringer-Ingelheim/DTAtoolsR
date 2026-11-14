# DTADataSet@provenance: one record per bound item, written by load_file().

test_that("a tabular load_file records the file it read, on its own", {
  ds <- tab_ds("ae", list(DTAFileCSV("ae.csv")))
  ds <- load_file(ds, file = cont_write("ae.csv", CONT_AE_CSV))
  expect_identical(
    dta_provenance(ds)[["ae"]],
    list(file = "ae.csv", container = character(), via = "file")
  )
})

test_that("a streamed table is recorded too", {
  ds <- tab_ds("ae", list(DTAFileCSV("ae.csv")))
  ds <- load_file(ds, file = cont_write("ae.csv", CONT_AE_CSV), stream = "always")
  expect_identical(
    dta_provenance(ds)[["ae"]],
    list(file = "ae.csv", container = character(), via = "file")
  )
})

test_that("a file dataset is keyed by the bound path; a replacement drops the old key", {
  ds <- file_ds("docs", DTAFileAny("define.pdf"))
  p1 <- cont_write("define.pdf", "one")
  p2 <- cont_write("define.pdf", "two")
  ds <- load_file(ds, file = p1)
  expect_identical(names(dta_provenance(ds)), p1)
  expect_identical(
    dta_provenance(ds)[[p1]],
    list(file = "define.pdf", container = character(), via = "file")
  )
  # Same name from another directory replaces the first.
  ds <- load_file(ds, file = p2)
  expect_identical(names(dta_provenance(ds)), p2)
  expect_identical(ds@file_paths, p2)
})

test_that("the helpers are write-safe", {
  ds <- tab_ds("ae", list(DTAFileCSV("ae.csv")))
  expect_identical(dta_provenance(ds), list())
  ds2 <- dta_set_provenance(ds, "k", list(file = "f", container = character(), via = "file"))
  expect_named(dta_provenance(ds2), "k")
  expect_identical(dta_provenance(dta_set_provenance(ds2, "k", NULL)), list())
  # An object without the slot comes back unchanged.
  Old <- S7::new_class("Old", properties = list(id = S7::class_character))
  o <- Old(id = "x")
  expect_identical(dta_set_provenance(o, "k", list()), o)
  expect_identical(dta_provenance(o), list())
})

test_that("provenance travels with a dataset inside a DTA", {
  d <- dta_of(tab_ds("ae", list(DTAFileCSV("ae.csv"))))
  d <- load_file(d, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  expect_named(dta_provenance(datasets(d, "ae")), "ae")
})

test_that("dta_set_provenance() skips only an object without the slot; other errors propagate", {
  ds <- tab_ds("ae", list(DTAFileCSV("ae.csv")))
  # A key that cannot index a list is a real error, not a reason to drop the record.
  expect_error(dta_set_provenance(ds, character(0), list(file = "f", container = character(), via = "file")))
})
