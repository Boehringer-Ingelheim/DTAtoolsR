# Integrity of what is written, and isolation from everything else.
# REQ-ROBUST-004 .. REQ-ROBUST-009.
#
# A validation run happens inside somebody else's session, usually inside a
# larger pipeline. What it writes has to be exactly what it says it wrote, and
# what it touches has to be exactly what it was pointed at. A function that
# leaves the session subtly different damages work that has nothing to do with
# it, and the damage surfaces somewhere else entirely.

rs_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
}

rs_checked <- function(dir, name = "iso.csv") {
  path <- file.path(dir, name)
  utils::write.csv(
    data.frame(
      ID = c("A0000001", "A0000002", "A0000003"),
      AGE = c("30", "40", "50"), stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  ds <- DTADataSetTabular(
    name = "iso", specs = rs_specs(),
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

test_that("OQ-ROBUST-004 | a refused overwrite leaves the existing file untouched | REQ-ROBUST-004", {
  dir <- qa_tempdir()
  ds <- rs_checked(dir)
  table_name <- names(tables(ds))[[1]]
  target <- file.path(dir, "export.tsv")

  invisible(write_table_to_file(ds, table = table_name, filename = target, quiet = TRUE))
  original <- unname(tools::md5sum(target))
  qa_check("the first write produces a file", file.exists(target))

  err <- tryCatch(
    write_table_to_file(ds, table = table_name, filename = target, quiet = TRUE),
    error = function(e) e
  )
  qa_check("writing again without permission raises a condition", inherits(err, "condition"))

  # The important half. An overwrite guard that raised after truncating the
  # file would have destroyed the record it was protecting.
  qa_step(
    "and the existing file is byte-for-byte what it was",
    original, unname(tools::md5sum(target))
  )

  invisible(write_table_to_file(
    ds,
    table = table_name, filename = target, quiet = TRUE, overwrite = TRUE
  ))
  qa_check("while an explicit overwrite is allowed", file.exists(target))
})

test_that("OQ-ROBUST-005 | a write that cannot complete leaves nothing that looks complete | REQ-ROBUST-005", {
  dir <- qa_tempdir()
  ds <- rs_checked(dir)
  table_name <- names(tables(ds))[[1]]

  # A path whose parent does not exist. The write cannot succeed, and what
  # matters is that it does not leave a partial file a later reader would take
  # for a whole one.
  target <- file.path(dir, "no-such-directory", "export.tsv")
  err <- tryCatch(
    write_table_to_file(ds, table = table_name, filename = target, quiet = TRUE),
    error = function(e) e
  )
  qa_check("the write raises a condition", inherits(err, "condition"))
  qa_step("and no file is left behind", FALSE, file.exists(target))
  qa_step(
    "nor is a checksum sidecar left claiming one exists",
    FALSE, file.exists(paste0(target, ".md5"))
  )
})

test_that("OQ-ROBUST-006 | the recorded checksum follows the bytes on disk | REQ-ROBUST-006", {
  dir <- qa_tempdir()
  ds <- rs_checked(dir)
  table_name <- names(tables(ds))[[1]]
  target <- file.path(dir, "checked.tsv")

  written <- write_table_to_file(
    ds,
    table = table_name, filename = target, quiet = TRUE, overwrite = TRUE
  )
  recorded <- as.character(written$md5sum[[1]])

  qa_step(
    "the recorded checksum is the checksum of the file",
    unname(tools::md5sum(target)), recorded
  )

  # And it has to be a checksum of the contents, not of the name or the size.
  # A digest that did not change when the bytes did would certify a corrupted
  # transfer as intact, which is worse than shipping no checksum at all.
  altered <- readLines(target)
  altered[[2]] <- sub("A0000001", "B0000001", altered[[2]])
  writeLines(altered, target)
  qa_step(
    "and changing one value in the file changes it",
    FALSE, identical(recorded, unname(tools::md5sum(target)))
  )
})

test_that("OQ-ROBUST-007 | validating leaves the session exactly as it was | REQ-ROBUST-007", {
  dir <- qa_tempdir()

  options_before <- options()
  locale_before <- Sys.getlocale()
  wd_before <- getwd()
  env_before <- Sys.getenv()

  invisible(rs_checked(dir, "session.csv"))

  # Comparing the whole option list, not a chosen few: a package that set an
  # option nobody thought to look for would still change how unrelated code
  # behaves for the rest of the session.
  qa_step(
    "no option is added",
    character(0), setdiff(names(options()), names(options_before))
  )
  changed <- names(options_before)[
    !vapply(
      names(options_before),
      function(n) identical(options_before[[n]], getOption(n)),
      logical(1)
    )
  ]
  qa_step("and none is altered", character(0), changed)
  qa_step("the locale is unchanged", locale_before, Sys.getlocale())
  qa_step("the working directory is unchanged", wd_before, getwd())
  qa_step(
    "and no environment variable is added",
    character(0), setdiff(names(Sys.getenv()), names(env_before))
  )
})

test_that("OQ-ROBUST-008 | validating writes only where it was pointed | REQ-ROBUST-008", {
  dir <- qa_tempdir()
  watched <- qa_tempdir()
  writeLines("sentinel", file.path(watched, "sentinel.txt"))
  before <- list.files(watched, recursive = TRUE)

  invisible(rs_checked(dir, "scoped.csv"))

  # A directory the run was never told about must be untouched. This is the
  # difference between a tool that can be run on a shared server and one that
  # cannot.
  qa_step("an unrelated directory is untouched", before, list.files(watched, recursive = TRUE))
  qa_step(
    "and its contents are unchanged",
    "sentinel", readLines(file.path(watched, "sentinel.txt"))
  )
})

test_that("OQ-ROBUST-009 | validating does not disturb the random number stream | REQ-ROBUST-009", {
  dir <- qa_tempdir()

  # A pipeline that seeds a random process, validates a delivery, and then
  # draws its sample must get the sample it would have got without the
  # validation. Consuming the stream would make an analysis depend on whether a
  # check happened to run first.
  set.seed(4242)
  expected <- stats::runif(3)

  set.seed(4242)
  invisible(rs_checked(dir, "rng.csv"))
  observed <- stats::runif(3)

  qa_step(
    "the same seed yields the same draw whether or not a validation ran",
    expected, observed
  )
})
