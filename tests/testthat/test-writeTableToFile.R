# write_table_to_file() had no direct tests; two behaviours fixed in the
# streaming-stability pass are pinned here.

test_that("write_table_to_file() preserves non-syntactic column names", {
  # write.table()'s internal data.frame() coercion used to rewrite the header
  # ("Subject ID" -> "Subject.ID"), so an exported file no longer matched the
  # specs its dataset validated against.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "x.csv")),
    tables = list(t1 = data.frame(
      `Subject ID` = c("A", "B"),
      `2024 VAL` = c(1, 2),
      check.names = FALSE
    ))
  )
  out <- file.path(tempdir(), "wt_names.tsv")
  on.exit(unlink(out), add = TRUE)

  write_table_to_file(
    ds, "t1", out,
    quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
  )
  expect_identical(readLines(out, n = 1), "Subject ID\t2024 VAL")
})

test_that("write_table_to_file() refuses to overwrite unless told to", {
  # The signature default said TRUE while the docs said FALSE, so the
  # file.exists guard never fired and existing exports were silently
  # clobbered. FALSE -- the documented, safe default -- now wins.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "x.csv")),
    tables = list(t1 = data.frame(A = c("x", "y")))
  )
  out <- file.path(tempdir(), "wt_overwrite.tsv")
  on.exit(unlink(out), add = TRUE)

  write_table_to_file(
    ds, "t1", out,
    quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
  )
  expect_error(
    write_table_to_file(
      ds, "t1", out,
      quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
    ),
    regexp = "already exists"
  )

  write_table_to_file(
    ds, "t1", out,
    overwrite = TRUE,
    quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
  )
  expect_identical(readLines(out, n = 1), "A")
})
