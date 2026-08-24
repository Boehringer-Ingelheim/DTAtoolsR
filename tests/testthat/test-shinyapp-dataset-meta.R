# Dataset-metadata editing in the Shiny app (Edit -> Metadata).
#
# These are the DATASET's own properties -- name, description, template_* -- not
# the document-level DTAMetaData. The dangerous one is `name`: `dta@datasets` is
# a named list that every lookup keys into, while the app's upload slots, example
# pickers and nav buttons are keyed by the dataset's POSITION in that list. A
# rename therefore has to move the entry without moving the dataset, and must not
# be allowed to collide with another dataset's name. These tests cover the two
# helpers behind the dialog; test-shinyapp-server.R covers what the server does
# with them.

# ---- Reading the fields -----------------------------------------------------

test_that("dta_dataset_meta_fields reads the fixture's name, type and description", {
  fn <- app_fn("dta_dataset_meta_fields")

  f <- fn(app_fixture_dta(), "clinical_data")

  expect_equal(f$name, "clinical_data")
  expect_equal(f$type, "tabular")
  expect_equal(f$description, "Clinical data table")
})

test_that("dta_dataset_meta_fields reports an unset property as an empty string", {
  fn <- app_fn("dta_dataset_meta_fields")

  # The bundled clinical spec carries no template_* fields at all.
  f <- fn(app_fixture_dta(), "clinical_data")

  expect_equal(f$template_source, "")
  expect_equal(f$template_version, "")
  expect_equal(f$template_date, "")
})

test_that("template_date is a string, not a Date, so the text field is the right control", {
  # Why the editor offers template_date as a plain textInput rather than a
  # dateInput: the property is typed character-or-NULL, so the class REFUSES a
  # Date outright. A date picker would force ISO on a field the specification
  # allows to carry any template revision string.
  ds <- DTAtools::datasets(app_fixture_dta(), "clinical_data")

  # Matched on the property name, which S7 puts in the message verbatim: the
  # type names around it are S7's own English and the rest is not worth pinning.
  expect_error(
    S7::prop(ds, "template_date") <- as.Date("2026-01-15"),
    "template_date"
  )

  S7::prop(ds, "template_date") <- "2026-01-15"
  expect_equal(ds@template_date, "2026-01-15")
})

test_that("dta_dataset_meta_fields returns NULL for a dataset that does not exist", {
  fn <- app_fn("dta_dataset_meta_fields")

  expect_null(fn(app_fixture_dta(), "no_such_dataset"))
})

# ---- Writing them back ------------------------------------------------------

test_that("dta_set_dataset_meta stores the description and template fields", {
  fn <- app_fn("dta_dataset_meta_fields")
  set <- app_fn("dta_set_dataset_meta")

  r <- set(app_fixture_dta(), "clinical_data",
    name = "clinical_data",
    description = "Vitals collected at every visit",
    template_source = "CDISC SDTM",
    template_version = "3.4",
    template_date = "2026-01-15"
  )

  expect_true(r$ok)
  f <- fn(r$value, "clinical_data")
  expect_equal(f$description, "Vitals collected at every visit")
  expect_equal(f$template_source, "CDISC SDTM")
  expect_equal(f$template_version, "3.4")
  expect_equal(f$template_date, "2026-01-15")
})

test_that("a blank field unsets the property rather than storing an empty string", {
  set <- app_fn("dta_set_dataset_meta")

  r <- set(app_fixture_dta(), "clinical_data",
    name = "clinical_data", description = "   "
  )

  expect_true(r$ok)
  expect_null(DTAtools::datasets(r$value, "clinical_data")@description)
})

test_that("an unset description is omitted from the serialized dataset YAML", {
  # The point of storing NULL rather than "": .dta_compact() drops the key, so a
  # cleared field disappears from the specification instead of reappearing as an
  # empty value the next time it is loaded.
  #
  # Asserted on the PARSED document, not on the text. Every column in this
  # fixture carries its own `description:` too, so a plain regexp match tests
  # nothing -- it stays true no matter what the dataset-level key does.
  set <- app_fn("dta_set_dataset_meta")
  to_yaml <- app_fn("dta_dataset_to_yaml_text")
  parsed <- function(dta) yaml::yaml.load(to_yaml(dta, "clinical_data")$value)

  expect_equal(parsed(app_fixture_dta())$description, "Clinical data table")

  cleared <- set(app_fixture_dta(), "clinical_data",
    name = "clinical_data", description = ""
  )$value

  expect_null(parsed(cleared)$description)
  # The columns' own descriptions are untouched -- only the dataset's went.
  expect_equal(parsed(cleared)$columns[[1]]$description, "Unique study ID")
})

test_that("template fields round-trip through read -> write unchanged", {
  fields <- app_fn("dta_dataset_meta_fields")
  set <- app_fn("dta_set_dataset_meta")

  seeded <- set(app_fixture_dta(), "clinical_data",
    name = "clinical_data", description = "A description",
    template_source = "CDISC SDTM", template_version = "3.4",
    template_date = "2026-01-15"
  )$value

  f <- fields(seeded, "clinical_data")
  round <- set(seeded, "clinical_data",
    name = f$name, description = f$description,
    template_source = f$template_source,
    template_version = f$template_version,
    template_date = f$template_date
  )

  expect_true(round$ok)
  expect_equal(fields(round$value, "clinical_data"), f)
})

# ---- Renaming ---------------------------------------------------------------

test_that("renaming moves the dataset to its new key", {
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  r <- set(app_fixture_dta(), "clinical_data", name = "renamed")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), "renamed")
  expect_equal(DTAtools::datasets(r$value, "renamed")@name, "renamed")
})

test_that("renaming keeps the dataset at its original position", {
  # THE regression this guards: every upload slot, example picker and nav button
  # in the app is keyed by the dataset's position and resolves its name only at
  # click time. Rebuilding the list as `[[old]] <- NULL; [[new]] <- ds` moves the
  # dataset to the END, after which those controls address the wrong dataset --
  # silently, with no error anywhere.
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  dta <- DTAtools::create_example_DTA()
  expect_equal(names_fn(dta), c("demographics", "vitals"))

  r <- set(dta, "demographics", name = "subjects")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), c("subjects", "vitals"))
})

test_that("renaming preserves the dataset's columns, rules and bound tables", {
  set <- app_fn("dta_set_dataset_meta")

  dta <- app_fixture_dta_with_data()
  before <- DTAtools::datasets(dta, "clinical_data")

  r <- set(dta, "clinical_data", name = "renamed")

  expect_true(r$ok)
  after <- DTAtools::datasets(r$value, "renamed")
  expect_equal(names(after@specs@columns), names(before@specs@columns))
  expect_equal(length(after@specs@rules), length(before@specs@rules))
  expect_equal(names(after@tables), names(before@tables))
})

test_that("a name already used by another dataset is rejected, changing nothing", {
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  dta <- DTAtools::create_example_DTA()

  r <- set(dta, "demographics", name = "vitals")

  expect_false(r$ok)
  expect_match(r$error, "already exists")
  # The DTA handed in is untouched: the collision was caught before any write.
  expect_equal(names_fn(dta), c("demographics", "vitals"))
})

test_that("keeping a dataset's own name is not treated as a collision", {
  set <- app_fn("dta_set_dataset_meta")

  dta <- DTAtools::create_example_DTA()

  r <- set(dta, "vitals", name = "vitals", description = "Vital signs")

  expect_true(r$ok)
  expect_equal(DTAtools::datasets(r$value, "vitals")@description, "Vital signs")
})

test_that("an empty name is rejected with a message rather than an S7 abort", {
  set <- app_fn("dta_set_dataset_meta")

  for (bad in list("", "   ", NULL)) {
    r <- set(app_fixture_dta(), "clinical_data", name = bad)
    expect_false(r$ok)
    expect_match(r$error, "name is required")
  }
})

test_that("a name is trimmed before it is stored", {
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  r <- set(app_fixture_dta(), "clinical_data", name = "  renamed  ")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), "renamed")
})

test_that("editing a dataset that does not exist fails with a message", {
  set <- app_fn("dta_set_dataset_meta")

  r <- set(app_fixture_dta(), "no_such_dataset", name = "whatever")

  expect_false(r$ok)
  expect_match(r$error, "not found")
})

# ---- `type` is not editable -------------------------------------------------

test_that("dta_set_dataset_meta cannot change a dataset's type", {
  # `type` is fixed by the concrete S7 class, but the property itself is a plain
  # character whose validator only checks set membership -- so `ds@type <- "file"`
  # on a tabular dataset SUCCEEDS and yields an object that claims to be
  # file-backed while still carrying @specs and @tables, which everything
  # downstream keeps dispatching on. The helper therefore takes no `type`
  # argument at all, so there is no route through it that can set one.
  #
  # Asserted on the formals rather than on the error from passing `type =`:
  # R renders "unused argument" in the system language, and this suite never
  # matches translated text.
  set <- app_fn("dta_set_dataset_meta")

  expect_false("type" %in% names(formals(set)))
  # And a normal edit leaves the type exactly where it was.
  r <- set(app_fixture_dta(), "clinical_data",
    name = "clinical_data", description = "changed"
  )
  expect_equal(DTAtools::datasets(r$value, "clinical_data")@type, "tabular")
})
