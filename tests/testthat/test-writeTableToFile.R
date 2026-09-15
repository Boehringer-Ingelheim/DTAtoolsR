# write_table_to_file() had no direct tests; two behaviours fixed in the
# streaming-stability pass are pinned here, alongside the gzip branch and the
# crash-safety guarantee the CHANGELOG documents for both branches: a write
# that cannot finish leaves the previous contents exactly as they were.

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

test_that("write_table_to_file() writes a genuinely gzip-compressed file", {
  # The gzip branch had zero coverage: nothing in this file exercised
  # compression = "gzip" at all. A file merely named *.gz proves nothing --
  # read the magic bytes back to confirm the bytes on disk are actually
  # compressed, then decompress and re-read the table to confirm compression
  # didn't corrupt the content.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "x.csv")),
    tables = list(t1 = data.frame(A = c("x", "y"), B = c(1, 2)))
  )
  out <- file.path(tempdir(), "wt_gzip.tsv.gz")
  on.exit(unlink(out), add = TRUE)

  write_table_to_file(
    ds, "t1", out,
    compression = "gzip",
    quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
  )

  # RFC 1952: every gzip member starts with 0x1f 0x8b.
  expect_identical(readBin(out, "raw", n = 2), as.raw(c(0x1f, 0x8b)))

  back <- read.table(gzfile(out), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  expect_identical(back$A, c("x", "y"))
  expect_equal(back$B, c(1, 2))
})

test_that("write_table_to_file() leaves the previous file untouched when the export cannot be installed", {
  # The rewrite this pins: both branches write to a *.part file beside the
  # destination and only put it in place once the whole table has converted,
  # specifically so that a write that cannot finish leaves the previous
  # contents exactly as they were (CHANGELOG, Unreleased). A destination file
  # another process still has open is a genuine, everyday way for the final
  # install step to fail -- Windows will not let file.rename() replace a file
  # that is currently open elsewhere -- and it happens only after the *.part
  # file has already been written in full, so this is a real failure of the
  # "put it in place" step, not a contrived one. Read the bytes back and
  # compare rather than trusting file.exists(): a corrupt partial replacement
  # would still exist.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "x.csv")),
    tables = list(t1 = data.frame(A = c("new1", "new2")))
  )
  out <- file.path(tempdir(), "wt_locked.tsv")
  on.exit(unlink(out), add = TRUE)
  writeLines("A\r\noriginal1\r\noriginal2", out, sep = "")
  original_bytes <- readBin(out, "raw", n = file.info(out)$size)

  con <- file(out, open = "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  # file.rename() itself warns about the access-denied reason on its way to
  # returning FALSE; that OS-level warning is the expected side effect of the
  # lock, not the thing under test here, so it is suppressed rather than left
  # to print in the test log.
  expect_error(
    suppressWarnings(write_table_to_file(
      ds, "t1", out,
      overwrite = TRUE,
      quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
    ))
  )

  close(con)
  expect_identical(readBin(out, "raw", n = file.info(out)$size), original_bytes)
})

test_that("write_table_to_file() aborts with a clear message when the rename fails", {
  # Companion to the byte-preservation test above, focused on the abort
  # itself rather than the outcome: file.rename() returning FALSE must not
  # pass silently, and the message must tell the caller their old file is
  # safe, not just that something went wrong.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "x.csv")),
    tables = list(t1 = data.frame(A = c("new1", "new2")))
  )
  out <- file.path(tempdir(), "wt_locked_msg.tsv")
  on.exit(unlink(out), add = TRUE)
  writeLines("A", out)

  con <- file(out, open = "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  expect_error(
    suppressWarnings(write_table_to_file(
      ds, "t1", out,
      overwrite = TRUE,
      quiet = TRUE, get_md5sum = FALSE, write_md5sum_to_file = FALSE
    )),
    regexp = "Could not move the finished export into place"
  )

  close(con)
})
