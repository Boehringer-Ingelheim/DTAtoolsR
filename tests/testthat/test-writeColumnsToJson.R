# write_columns_to_json() is round-tripped through import_specs_from_json()
# below; the first tests read the written file back with jsonlite directly,
# the same way the YAML test reads the written file back rather than trusting
# file.exists().

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

json_round_trip_collection <- function() {
  DTAColumnSpecCollection(
    columns = list(
      BARE = DTAColumnSpec(id = "BARE"),
      FULL = DTAColumnSpec(
        id = "FULL", label = "Full", type = "SAS Char", length = 12,
        format = "SAS $12.", nullable = FALSE, description = "All set",
        values = c("007", "Y"), colclass = "study_info"
      ),
      NUM = DTAColumnSpec(id = "NUM", type = "SAS Num", pattern = "^[0-9]+$", nullable = TRUE),
      INT = DTAColumnSpec(id = "INT", type = "SAS Int", nullable = TRUE)
    ),
    rules = list(DTARuleColUnique(id = "u", columns = c("BARE", "FULL")))
  )
}

test_that("import_specs_from_json() reads back every property written by write_columns_to_json()", {
  original <- json_round_trip_collection()
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  write_columns_to_json(original, f)

  back <- import_specs_from_json(f)

  expect_s3_class(back, "DTAtools::DTAColumnSpecCollection")
  expect_identical(names(back@columns), c("BARE", "FULL", "NUM", "INT"))
  for (id in names(original@columns)) {
    expect_equal(
      as.list(back@columns[[id]]), as.list(original@columns[[id]]),
      info = id
    )
  }
  expect_null(back@columns$BARE@label)
  expect_null(back@columns$BARE@structure)
  expect_identical(back@columns$FULL@values, c("007", "Y"))
  expect_equal(back@columns$FULL@structure@length, 12)
  expect_identical(back@columns$FULL@structure@format, "$12.")
  expect_identical(back@columns$NUM@pattern, "^[0-9]+$")

  expect_length(back@rules, 1L)
  expect_identical(as.list(back@rules[[1]]), as.list(original@rules[[1]]))
})

test_that("a JSON round trip keeps an empty rules array and the shipped example's rules", {
  empty <- DTAColumnSpecCollection(
    columns = list(A = DTAColumnSpec(id = "A", type = "SAS Char")),
    rules = list()
  )
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  write_columns_to_json(empty, f)
  expect_length(import_specs_from_json(f)@rules, 0L)

  yaml <- yaml::read_yaml(system.file("extdata", "gf_dataset.yaml", package = "DTAtools"))
  gf <- specs_from_list(yaml$columns, yaml$rules)
  write_columns_to_json(gf, f)
  back <- import_specs_from_json(f)
  expect_identical(as.list(back), as.list(gf))
})

test_that("import_specs_from_json() aborts on a file without a columns section", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f, force = TRUE), add = TRUE)
  writeLines('{"rules": []}', f)
  expect_error(import_specs_from_json(f), regexp = "columns", class = "rlang_error")
})
