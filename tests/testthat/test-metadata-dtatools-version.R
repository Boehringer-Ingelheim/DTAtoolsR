# metadata.dtatools_version: the DTAtools release that last saved a document.
#
# The package has no YAML writer; the Shiny app is the only thing that writes
# one, so the stamp is applied in the app's serializer (dta_to_list()) and the
# package only has to accept, vet and round-trip the field. Machine-owned
# behaviour (carry-over, diffs, template targets) is pinned next to the other
# machine-owned tests: test-shinyapp-template-create.R, -rebase.R and
# test-validateTemplate.R.
#
# The expected release is always derived (packageVersion()), never typed in, so
# the suite does not break on the next version bump.

# ---- The property ----------------------------------------------------------

test_that("dtatools_version accepts a bare version number and nothing else", {
  expect_null(DTAMetaData()@dtatools_version)

  for (ok in c("0.29.0", "1", "1.0", "0.29.0.9000", "1.2-3")) {
    expect_identical(DTAMetaData(dtatools_version = ok)@dtatools_version, ok)
  }

  bad <- list(
    "", " ", "0.29.0 ", "1.0\n", "v0.29", "abc", "0.29.", NA_character_,
    c("1.0", "2.0"), 0.29, 1L, TRUE
  )
  for (value in bad) {
    expect_error(DTAMetaData(dtatools_version = value), "dtatools_version")
  }
})

test_that("as.list writes dtatools_version beside template and leaves it out when unset", {
  md <- DTAMetaData(
    title = "T", version = "1.0", study = list(id = "S"),
    template = list(id = "tpl", version = "1"), dtatools_version = "0.1.0"
  )
  lst <- as.list(md)
  expect_identical(lst$dtatools_version, "0.1.0")
  expect_lt(match("dtatools_version", names(lst)), match("template", names(lst)))

  expect_false("dtatools_version" %in% names(as.list(DTAMetaData(title = "T", version = "1.0"))))
})

test_that("a two-part version is written as text and read back as text", {
  # yaml quotes "1.0" only because it would otherwise read back as the number 1.
  path <- withr::local_tempfile(fileext = ".yaml")
  md <- DTAMetaData(title = "T", version = "1.0", dtatools_version = "1.0")
  yaml::write_yaml(list(metadata = as.list(md)), path)

  expect_identical(yaml::read_yaml(path)$metadata$dtatools_version, "1.0")
  expect_identical(
    metadata(read_dta_from_yaml(path))@dtatools_version, "1.0"
  )
})

# ---- Linting ---------------------------------------------------------------

lint_with_stamp <- function(raw_value) {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(
    c("metadata:", "  title: T", "  version: '1'", paste0("  dtatools_version: ", raw_value)),
    path
  )
  lint_dta(path)
}

test_that("lint_dta accepts a quoted version and names a malformed one", {
  accepted <- lint_with_stamp("'0.29.0'")
  expect_false("metadata_invalid" %in% accepted$code)
  expect_false("yaml_unreadable" %in% accepted$code)
  expect_false(any(grepl("dtatools_version", accepted$message, fixed = TRUE)))

  # Unquoted, YAML reads 0.29 as the number it looks like.
  for (raw in c("0.29", "v1", "''")) {
    out <- lint_with_stamp(raw)
    row <- out[out$code == "metadata_invalid", ]
    expect_identical(nrow(row), 1L)
    expect_identical(row$severity, "error")
    expect_match(row$message, "dtatools_version", fixed = TRUE)
  }
})

# ---- The app stamps on every save --------------------------------------------

test_that("the app writes the running release and replaces an older one", {
  running <- as.character(utils::packageVersion("DTAtools"))
  dta <- app_fn("dta_create_empty")("Stamped", "1.0")$value
  expect_null(metadata(dta)@dtatools_version)

  saved <- app_fn("dta_to_yaml_text")(dta)
  expect_true(saved$ok)
  expect_identical(metadata(app_fn("dta_read_yaml_text")(saved$value)$value)@dtatools_version, running)

  # A document read from a file written by another release names that release
  # until it is saved again; saving replaces it, and does not touch the object.
  md <- metadata(dta)
  S7::prop(md, "dtatools_version") <- "0.0.1"
  dta@metadata <- md
  resaved <- app_fn("dta_to_yaml_text")(dta)
  expect_identical(metadata(app_fn("dta_read_yaml_text")(resaved$value)$value)@dtatools_version, running)
  expect_identical(metadata(dta)@dtatools_version, "0.0.1")
})

test_that("saving a saved document again changes nothing, so the stamp does not move", {
  # as.list() decides where the key goes. Splicing it onto the end for an
  # unstamped document and leaving it in place for a stamped one would shift it
  # between the first and second save and make every second save a diff.
  dta <- app_fn("dta_create_empty")("Stable", "1.0")$value
  first <- app_fn("dta_to_yaml_text")(dta)
  again <- app_fn("dta_to_yaml_text")(app_fn("dta_read_yaml_text")(first$value)$value)

  expect_true(grepl("dtatools_version", first$value, fixed = TRUE))
  expect_identical(again$value, first$value)
})

test_that("a document with no metadata of its own is not stamped", {
  # A metadata block holding only the stamp would not load: title and version
  # are required whenever the block is present.
  dta <- DTA(datasets = list())
  expect_null(app_fn("dta_to_list")(dta)$metadata)

  yaml_text <- app_fn("dta_to_yaml_text")(dta)
  expect_true(yaml_text$ok)
  expect_false(grepl("dtatools_version", yaml_text$value, fixed = TRUE))
})
