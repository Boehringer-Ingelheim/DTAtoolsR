# upgrade_dta(): objects restored from an older version keep the class they
# were saved with. The tests build old-shaped objects (classes without the
# newer properties), round-trip them through saveRDS()/readRDS() and use them.

# The class `cls` as an older version defined it: same name and package, own
# properties minus `drop`, and a validator that accepts anything unless
# `validators` (named by class name) says otherwise.
old_shape_class <- function(cls, drop = character(), validators = list()) {
  parent <- cls@parent
  parent_old <- if (inherits(parent, "S7_class") && !identical(parent@name, "S7_object")) {
    old_shape_class(parent, drop, validators)
  } else {
    S7::S7_object
  }
  own <- setdiff(names(cls@properties), names(parent@properties))
  v <- validators[[cls@name]]
  S7::new_class(
    cls@name,
    package = "DTAtools", parent = parent_old,
    properties = cls@properties[setdiff(own, drop)],
    validator = if (is.null(v)) function(self) NULL else v
  )
}

# Every DTAtools object inside `obj`, given its old shape.
age_object <- function(obj, drop = c("containers", "provenance", "file_handlers"), validators = list()) {
  if (is.environment(obj) || is.data.frame(obj)) {
    return(obj)
  }
  if (inherits(obj, "S7_object")) {
    cur <- attr(obj, "S7_class", exact = TRUE)
    if (!identical(cur@package, "DTAtools")) {
      return(obj)
    }
    for (p in setdiff(names(attributes(obj)), c("class", "S7_class"))) {
      attr(obj, p) <- age_object(attr(obj, p), drop, validators)
    }
    for (p in intersect(drop, names(cur@properties))) attr(obj, p) <- NULL
    attr(obj, "S7_class") <- old_shape_class(cur, drop, validators)
    return(obj)
  }
  if (is.list(obj)) {
    for (i in seq_along(obj)) if (!is.null(obj[[i]])) obj[[i]] <- age_object(obj[[i]], drop, validators)
  }
  obj
}

roundtrip <- function(x) {
  f <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  saveRDS(x, f)
  readRDS(f)
}

current_dta <- function() {
  dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    file_ds("docs", DTAFileAny("define.pdf"))
  )
}

# The 0.27.0 validator of a handler: only a file name is a container.
old_handler_validator <- function(self) {
  p <- dta_container_value_problem(dta_handler_container(self))
  if (!is.null(p)) cli::cli_abort("{p}", class = "dta_container_invalid")
}

test_that("an old-shaped object fails without help, and is stale", {
  old <- roundtrip(age_object(current_dta()))
  # The stored (old) classes lack the properties, so reading them fails; judged
  # by the class's property list, never by S7's (translatable) error text.
  stored_props <- function(o) names(attr(attr(o, "S7_class"), "properties"))
  expect_false("containers" %in% stored_props(old))
  expect_false("provenance" %in% stored_props(old@datasets$ae))
  expect_false("file_handlers" %in% stored_props(old@datasets$docs))
  expect_true(inherits(tryCatch(old@containers, error = function(e) e), "error"))
  expect_true(dta_is_stale(old))
  expect_true(dta_is_stale(old@datasets$ae))
})

test_that("entry points upgrade an old-shaped object by themselves", {
  old <- roundtrip(age_object(current_dta()))
  ae_csv <- cont_write("ae.csv", CONT_AE_CSV)
  pdf <- cont_write("define.pdf", "x")

  d <- load_file(old, dataset = "ae", file = ae_csv)
  d <- load_file(d, dataset = "docs", file = pdf)
  expect_identical(nrow(cont_table(d, "ae", "ae")), 2L)
  expect_identical(datasets(d, "docs")@file_paths, pdf)
  expect_false(dta_is_stale(d))
  expect_named(dta_provenance(datasets(d, "ae")), "ae")

  # A dataset on its own: load_file() and check().
  old_ds <- roundtrip(age_object(current_dta()))@datasets$docs
  ds <- load_file(old_ds, file = pdf)
  ds <- check(ds, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(ds)$ok)
  old_ae <- roundtrip(age_object(current_dta()))@datasets$ae
  ae <- check(load_file(old_ae, file = ae_csv), quiet = TRUE, persist = FALSE)
  expect_true(all(validation_status(ae)$ok))
})

test_that("a restored handler gets the current validator, not just new properties", {
  old <- roundtrip(age_object(current_dta(), validators = list(DTAFile = old_handler_validator)))
  h_old <- old@datasets$ae@files[[1]]
  # The old validator refuses an id ...
  expect_error(h_old@container <- "sdtm", class = "dta_container_invalid")
  up <- upgrade_dta(old)
  h <- up@datasets$ae@files[[1]]
  h@container <- "sdtm"
  expect_identical(h@container, "sdtm")
})

test_that("a current object comes back with every property unchanged", {
  d <- current_dta()
  d <- load_file(d, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  rt <- roundtrip(d)
  expect_false(dta_is_stale(rt))
  up <- upgrade_dta(rt)
  expect_identical(up@datasets, rt@datasets)
  expect_identical(up@metadata, rt@metadata)
  expect_identical(up@containers, rt@containers)
  expect_identical(dta_provenance(datasets(up, "ae")), dta_provenance(datasets(rt, "ae")))
  expect_identical(up@datasets$ae@id, d@datasets$ae@id)
  expect_identical(up@datasets$ae@files, d@datasets$ae@files)
})

test_that("check() upgrades an aged dataset with a bound path by itself", {
  pdf <- cont_write("define.pdf", "x")
  bound <- load_file(current_dta()@datasets$docs, file = pdf)
  old <- roundtrip(age_object(bound))
  expect_true(dta_is_stale(old))
  ds <- check(old, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(ds)$ok)
  expect_identical(ds@file_paths, pdf)
  expect_false(dta_is_stale(ds))
})

test_that("a re-stamp also rebuilds class(), so a changed hierarchy dispatches", {
  # DTAFileXPT as an older version defined it: directly under DTAFile.
  OldXPT <- S7::new_class("DTAFileXPT", package = "DTAtools", parent = DTAFile, properties = list())
  fresh <- DTAFileXPT("ae.xpt")
  aged <- fresh
  attr(aged, "S7_class") <- OldXPT
  class(aged) <- c("DTAtools::DTAFileXPT", "DTAtools::DTAFile", "S7_object")
  aged <- roundtrip(aged)
  expect_false(inherits(aged, "DTAtools::DTAFileSAS7BDAT"))
  up <- dta_upgrade(aged)
  expect_identical(class(up), class(fresh))
  expect_true(inherits(up, "DTAtools::DTAFileSAS7BDAT"))
})

test_that("upgrading restores defaults and keeps what was there", {
  d <- load_file(current_dta(), dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  old <- roundtrip(age_object(d))
  up <- upgrade_dta(old)
  expect_identical(up@containers, list())
  expect_identical(up@datasets$docs@file_handlers, integer())
  expect_identical(up@datasets$ae@provenance, list())
  expect_identical(up@datasets$ae@id, "ae")
  # (an Arrow table does not survive saveRDS(); the binding does)
  expect_identical(names(up@datasets$ae@tables), "ae")
  expect_false(dta_is_stale(up))
  expect_error(upgrade_dta("x"), "must be a")
})
