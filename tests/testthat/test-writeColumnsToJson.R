# write_columns_to_json() had no test at all, unlike its YAML sibling
# (write_columns_to_yaml(), round-tripped in test-DTAColumnSpecStructure.R).
# There is no JSON reader to round-trip through -- write_columns_to_json()'s
# own docs are explicit that it serialises as.list(), not a schema, and
# nothing in R/ reads it back -- so this reads the JSON file back with
# jsonlite directly and checks the fields that matter survive the trip
# unchanged, the same way the YAML test reads the written file back rather
# than trusting file.exists().

test_that("write_columns_to_json() round-trips a spec collection's fields", {
  original <- DTAColumnSpecCollection(
    columns = list(
      STUDYID = DTAColumnSpec(id = "STUDYID", label = "Study", type = "SAS Char"),
      AGE = DTAColumnSpec(id = "AGE", label = "Age", type = "SAS Int", nullable = TRUE)
    ),
    rules = list()
  )

  f <- tempfile(fileext = ".json")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  write_columns_to_json(original, f)

  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)

  expect_length(back$columns, 2)
  expect_identical(back$columns[[1]]$id, "STUDYID")
  expect_identical(back$columns[[1]]$label, "Study")
  # as.list() on a SAS structure prefixes type/format with the backend name
  # (the same convention the YAML round trip pins), so the written value is
  # "SAS Char", not the bare structure type.
  expect_identical(back$columns[[1]]$type, "SAS Char")
  expect_identical(back$columns[[2]]$id, "AGE")
  expect_identical(back$columns[[2]]$type, "SAS Int")
  # auto_unbox = TRUE is what keeps a length-1 logical as `true`/`false`
  # rather than a JSON array of one; jsonlite reads either back as a plain
  # logical, so this also confirms that option actually took effect.
  expect_identical(back$columns[[2]]$nullable, TRUE)
})

test_that("write_columns_to_json() writes an empty rules array when there are none", {
  original <- DTAColumnSpecCollection(
    columns = list(A = DTAColumnSpec(id = "A", type = "SAS Char")),
    rules = list()
  )

  f <- tempfile(fileext = ".json")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  write_columns_to_json(original, f)

  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_length(back$rules, 0)
})
