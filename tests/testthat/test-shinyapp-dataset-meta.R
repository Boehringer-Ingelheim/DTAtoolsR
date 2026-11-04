# Dataset-metadata editing in the Shiny app (Edit -> Details).
#
# These are the DATASET's own properties -- id, name, description, template_* --
# not the document-level DTAMetaData. The dangerous one is `id`: `dta@datasets`
# is a named list keyed by it that every lookup keys into, while the app's
# upload slots, example pickers and nav buttons are keyed by the dataset's
# POSITION in that list. Changing the id therefore has to move the entry without
# moving the dataset, and must not be allowed to collide with another dataset's
# id. `name` (the human-readable label) and `description` (long free text) are
# plain optional strings with no such trap. These tests cover the two helpers
# behind the dialog; test-shinyapp-server.R covers what the server does with
# them.

# ---- Reading the fields -----------------------------------------------------

test_that("dta_dataset_meta_fields reads the fixture's id, name, type and description", {
  fn <- app_fn("dta_dataset_meta_fields")

  f <- fn(app_fixture_dta(), "clinical_data")

  expect_equal(f$id, "clinical_data")
  expect_equal(f$name, "Clinical data table")
  expect_equal(f$type, "tabular")
  # The long description is a different property from the label: the fixture's
  # folded block, not "Clinical data table".
  expect_match(
    f$description,
    "^Subject-level clinical observations for the study, one row per subject"
  )
})

test_that("dta_dataset_meta_fields returns all seven fields the dialog reads", {
  fn <- app_fn("dta_dataset_meta_fields")

  f <- fn(app_fixture_dta(), "clinical_data")

  expect_named(
    f,
    c(
      "id", "name", "type", "description",
      "template_source", "template_version", "template_date"
    )
  )
})

test_that("dta_dataset_meta_fields reports an unset property as an empty string", {
  fn <- app_fn("dta_dataset_meta_fields")

  # The bundled clinical spec carries no template_* fields at all.
  f <- fn(app_fixture_dta(), "clinical_data")

  expect_equal(f$template_source, "")
  expect_equal(f$template_version, "")
  expect_equal(f$template_date, "")
})

test_that("an unset name and description both read back as empty strings", {
  fn <- app_fn("dta_dataset_meta_fields")
  set <- app_fn("dta_set_dataset_meta")

  bare <- set(app_fixture_dta(), "clinical_data", id = "clinical_data")$value
  f <- fn(bare, "clinical_data")

  expect_equal(f$id, "clinical_data")
  expect_equal(f$name, "")
  expect_equal(f$description, "")
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

test_that("dta_set_dataset_meta stores the name, description and template fields", {
  fn <- app_fn("dta_dataset_meta_fields")
  set <- app_fn("dta_set_dataset_meta")

  r <- set(app_fixture_dta(), "clinical_data",
    id = "clinical_data",
    name = "Vital signs",
    description = "Vitals collected at every visit",
    template_source = "CDISC SDTM",
    template_version = "3.4",
    template_date = "2026-01-15"
  )

  expect_true(r$ok)
  f <- fn(r$value, "clinical_data")
  expect_equal(f$id, "clinical_data")
  expect_equal(f$name, "Vital signs")
  expect_equal(f$description, "Vitals collected at every visit")
  expect_equal(f$template_source, "CDISC SDTM")
  expect_equal(f$template_version, "3.4")
  expect_equal(f$template_date, "2026-01-15")
})

test_that("a name-only edit keeps the id, the key and the validation state", {
  # The label is presentation: it takes no part in validation, so changing it
  # must leave the dataset under the same id/key and every recorded result
  # exactly as it was. Only `@name` moves.
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  dta <- app_fixture_dta_with_data(checked = TRUE)
  before <- DTAtools::datasets(dta, "clinical_data")
  expect_equal(DTAtools::results(dta)$status, "validated")

  r <- set(dta, "clinical_data",
    id = "clinical_data",
    name = "A new label",
    description = before@description
  )

  expect_true(r$ok)
  expect_equal(names_fn(r$value), "clinical_data")
  after <- DTAtools::datasets(r$value, "clinical_data")
  expect_equal(after@id, "clinical_data")
  expect_equal(after@name, "A new label")
  expect_equal(after@description, before@description)
  # Validation survives untouched: the same results table, the same stored
  # index, and the bound table still there.
  expect_identical(DTAtools::results(r$value), DTAtools::results(dta))
  expect_identical(after@validation_index, before@validation_index)
  expect_equal(names(after@tables), names(before@tables))
})

test_that("a blank name or description unsets the property rather than storing an empty string", {
  set <- app_fn("dta_set_dataset_meta")

  # The fixture starts with BOTH set, so a blank can only be seen to unset.
  start <- DTAtools::datasets(app_fixture_dta(), "clinical_data")
  expect_true(nzchar(start@name))
  expect_true(nzchar(start@description))

  for (blank in list("", "   ", NULL)) {
    r <- set(app_fixture_dta(), "clinical_data",
      id = "clinical_data", name = blank, description = blank
    )

    expect_true(r$ok)
    ds <- DTAtools::datasets(r$value, "clinical_data")
    expect_null(ds@name)
    expect_null(ds@description)
  }
})

test_that("a blank description alone unsets only the description", {
  set <- app_fn("dta_set_dataset_meta")

  r <- set(app_fixture_dta(), "clinical_data",
    id = "clinical_data", name = "Clinical data table", description = "   "
  )

  expect_true(r$ok)
  ds <- DTAtools::datasets(r$value, "clinical_data")
  expect_null(ds@description)
  expect_equal(ds@name, "Clinical data table")
})

test_that("an unset name and description are omitted from the serialized dataset YAML", {
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

  start <- parsed(app_fixture_dta())
  expect_equal(start$id, "clinical_data")
  expect_equal(start$name, "Clinical data table")
  expect_match(start$description, "^Subject-level clinical observations")

  cleared <- set(app_fixture_dta(), "clinical_data",
    id = "clinical_data", name = "", description = ""
  )$value

  expect_equal(parsed(cleared)$id, "clinical_data")
  expect_null(parsed(cleared)$name)
  expect_null(parsed(cleared)$description)
  # The columns' own descriptions are untouched -- only the dataset's went.
  expect_equal(parsed(cleared)$columns[[1]]$description, "Unique study ID")
})

test_that("template fields round-trip through read -> write unchanged", {
  fields <- app_fn("dta_dataset_meta_fields")
  set <- app_fn("dta_set_dataset_meta")

  seeded <- set(app_fixture_dta(), "clinical_data",
    id = "clinical_data", name = "A label", description = "A description",
    template_source = "CDISC SDTM", template_version = "3.4",
    template_date = "2026-01-15"
  )$value

  f <- fields(seeded, "clinical_data")
  round <- set(seeded, "clinical_data",
    id = f$id, name = f$name, description = f$description,
    template_source = f$template_source,
    template_version = f$template_version,
    template_date = f$template_date
  )

  expect_true(round$ok)
  expect_equal(fields(round$value, "clinical_data"), f)
})

# ---- Changing the id --------------------------------------------------------

test_that("changing the id moves the dataset to its new key", {
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  r <- set(app_fixture_dta(), "clinical_data", id = "renamed")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), "renamed")
  expect_equal(DTAtools::datasets(r$value, "renamed")@id, "renamed")
})

test_that("changing the id keeps the dataset at its original position", {
  # THE regression this guards: every upload slot, example picker and nav button
  # in the app is keyed by the dataset's position and resolves its id only at
  # click time. Rebuilding the list as `[[old]] <- NULL; [[new]] <- ds` moves the
  # dataset to the END, after which those controls address the wrong dataset --
  # silently, with no error anywhere.
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  dta <- DTAtools::create_example_DTA()
  expect_equal(names_fn(dta), c("demographics", "vitals"))

  r <- set(dta, "demographics", id = "subjects")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), c("subjects", "vitals"))
  # The key and the object's own id agree, and the second dataset is untouched.
  expect_equal(r$value@datasets[[1]]@id, "subjects")
  expect_identical(r$value@datasets[[2]], dta@datasets[[2]])
})

test_that("changing the id preserves the dataset's columns, rules and bound tables", {
  set <- app_fn("dta_set_dataset_meta")

  dta <- app_fixture_dta_with_data()
  before <- DTAtools::datasets(dta, "clinical_data")

  r <- set(dta, "clinical_data", id = "renamed")

  expect_true(r$ok)
  after <- DTAtools::datasets(r$value, "renamed")
  expect_equal(names(after@specs@columns), names(before@specs@columns))
  expect_equal(length(after@specs@rules), length(before@specs@rules))
  expect_equal(names(after@tables), names(before@tables))
})

test_that("an id already used by another dataset is rejected, changing nothing", {
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  dta <- DTAtools::create_example_DTA()

  r <- set(dta, "demographics", id = "vitals")

  expect_false(r$ok)
  expect_match(r$error, "A dataset with ID 'vitals' already exists.", fixed = TRUE)
  # The DTA handed in is untouched: the collision was caught before any write.
  expect_equal(names_fn(dta), c("demographics", "vitals"))
})

test_that("keeping a dataset's own id is not treated as a collision", {
  set <- app_fn("dta_set_dataset_meta")

  dta <- DTAtools::create_example_DTA()

  r <- set(dta, "vitals", id = "vitals", description = "Vital signs, every visit")

  expect_true(r$ok)
  expect_equal(
    DTAtools::datasets(r$value, "vitals")@description,
    "Vital signs, every visit"
  )
})

test_that("two datasets may share a name: only the id has to be unique", {
  set <- app_fn("dta_set_dataset_meta")

  dta <- DTAtools::create_example_DTA()

  r <- set(dta, "demographics", id = "demographics", name = "Vital signs")

  expect_true(r$ok)
  expect_equal(DTAtools::datasets(r$value, "demographics")@name, "Vital signs")
  expect_equal(DTAtools::datasets(r$value, "vitals")@name, "Vital signs")
})

test_that("an empty id is rejected with a message rather than an S7 abort", {
  set <- app_fn("dta_set_dataset_meta")

  for (bad in list("", "   ", NULL)) {
    r <- set(app_fixture_dta(), "clinical_data", id = bad)
    expect_false(r$ok)
    expect_match(r$error, "A dataset ID is required.", fixed = TRUE)
  }
})

test_that("an id is trimmed before it is stored", {
  set <- app_fn("dta_set_dataset_meta")
  names_fn <- app_fn("dta_dataset_names")

  r <- set(app_fixture_dta(), "clinical_data", id = "  renamed  ")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), "renamed")
})

test_that("editing a dataset that does not exist fails with a message", {
  set <- app_fn("dta_set_dataset_meta")

  r <- set(app_fixture_dta(), "no_such_dataset", id = "whatever")

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
    id = "clinical_data", description = "changed"
  )
  expect_equal(DTAtools::datasets(r$value, "clinical_data")@type, "tabular")
})
