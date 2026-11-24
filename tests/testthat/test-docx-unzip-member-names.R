# A library-supplied .docx is untrusted input. utils::unzip() only neutralises
# '..' members from R 4.5.1 on, and the package supports R >= 4.2.0, so both
# extraction sites (the package's .replace_docx_placeholders() and the app's
# template_has_placeholders()) refuse hostile member names *before* extracting.
# The tests therefore watch whether extraction was reached at all, which does
# not depend on the R version running them.

# A stored zip holding only empty members, hand-built because zip::zip() and
# utils::zip() refuse to write a '../' name.
.dummy_zip <- function(path, names) {
  le <- function(x, n) writeBin(as.integer(x), raw(), size = n, endian = "little")
  local <- central <- raw()
  for (nm in names) {
    nraw <- charToRaw(nm)
    offset <- length(local)
    local <- c(
      local, le(67324752L, 4), le(20, 2), le(0, 2), le(0, 2), le(0, 2), le(0, 2),
      le(0, 4), le(0, 4), le(0, 4), le(length(nraw), 2), le(0, 2), nraw
    )
    central <- c(
      central, le(33639248L, 4), le(20, 2), le(20, 2), le(0, 2), le(0, 2),
      le(0, 2), le(0, 2), le(0, 4), le(0, 4), le(0, 4), le(length(nraw), 2),
      le(0, 2), le(0, 2), le(0, 2), le(0, 2), le(0, 4), le(offset, 4), nraw
    )
  }
  writeBin(
    c(
      local, central, le(101010256L, 4), le(0, 2), le(0, 2), le(length(names), 2),
      le(length(names), 2), le(length(central), 4), le(length(local), 4), le(0, 2)
    ),
    path
  )
  path
}

# Records whether utils::unzip() was asked to extract (list = TRUE is a read).
.watch_extraction <- function(env = parent.frame()) {
  seen <- new.env()
  seen$extracted <- FALSE
  real <- utils::unzip
  testthat::local_mocked_bindings(
    unzip = function(zipfile, files = NULL, list = FALSE, ...) {
      if (!list) seen$extracted <- TRUE
      real(zipfile, files = files, list = list, ...)
    },
    .package = "utils",
    .env = env
  )
  seen
}

hostile_names <- list(
  parent = "../dta_zipslip_probe.txt",
  deep = "word/../../dta_zipslip_probe.txt",
  backslash = "..\\dta_zipslip_probe.txt",
  absolute = "/tmp/dta_zipslip_probe.txt",
  drive = "C:/dta_zipslip_probe.txt",
  dot = "./word/document.xml"
)

test_that("the dummy archive is well formed and lists the hostile name", {
  zp <- .dummy_zip(withr::local_tempfile(fileext = ".docx"), c("word/document.xml", "../x.txt"))
  expect_identical(utils::unzip(zp, list = TRUE)$Name, c("word/document.xml", "../x.txt"))
})

for (case in names(hostile_names)) {
  local({
    case <- case
    nm <- hostile_names[[case]]

    test_that(paste0(".dta_unzip_docx_checked refuses a '", case, "' member before extracting"), {
      zp <- .dummy_zip(withr::local_tempfile(fileext = ".docx"), c("word/document.xml", nm))
      exdir <- withr::local_tempdir()
      seen <- .watch_extraction()

      expect_identical(DTAtools:::.dta_unzip_docx_checked(zp, exdir), character(0))
      expect_false(seen$extracted)
      expect_length(list.files(exdir, recursive = TRUE, all.files = TRUE), 0)
      expect_false(file.exists(file.path(dirname(exdir), "dta_zipslip_probe.txt")))
    })

    test_that(paste0("template_has_placeholders is FALSE for a '", case, "' member, without extracting"), {
      zp <- .dummy_zip(withr::local_tempfile(fileext = ".docx"), c("word/document.xml", nm))
      seen <- .watch_extraction()

      expect_false(app_fn("template_has_placeholders")(zp))
      expect_false(seen$extracted)
    })

    test_that(paste0(".replace_docx_placeholders aborts on a '", case, "' member, without extracting"), {
      zp <- .dummy_zip(withr::local_tempfile(fileext = ".docx"), c("word/document.xml", nm))
      out <- withr::local_tempfile(fileext = ".docx")
      seen <- .watch_extraction()

      expect_error(
        DTAtools:::.replace_docx_placeholders(zp, list(), NULL, out, quiet = TRUE),
        "not a readable DOCX"
      )
      expect_false(seen$extracted)
      expect_false(file.exists(out))
    })
  })
}

test_that("the app's member-name check and the package's agree on every name", {
  # The app cannot call dta_zip_entry_escape(), so it carries a mirror; this is
  # what stops the two drifting apart.
  names_to_try <- c(
    unlist(hostile_names, use.names = FALSE),
    "", "..", ".", "a//b", "a//", "x/", "word/", "word/document.xml", "a..b/c.xml",
    "C:", "\\\\srv\\share", "/", "\\", "word\\..\\x", "ok/.hidden", "café/x",
    rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  )
  app_hostile <- app_fn("docx_member_is_hostile")
  for (nm in names_to_try) {
    expect_identical(
      app_hostile(nm),
      !is.na(DTAtools:::dta_zip_entry_escape(nm)),
      info = encodeString(nm)
    )
  }
})

test_that("an ordinary archive, directory entries included, is still extracted", {
  zp <- .dummy_zip(
    withr::local_tempfile(fileext = ".docx"),
    c("word/", "word/document.xml", "word/_rels/document.xml.rels", "a..b/c.xml")
  )
  exdir <- withr::local_tempdir()
  got <- DTAtools:::.dta_unzip_docx_checked(zp, exdir)
  expect_true(file.exists(file.path(exdir, "word", "document.xml")))
  expect_gt(length(got), 0)
})

test_that("an unreadable file yields no extraction rather than an error", {
  junk <- withr::local_tempfile(fileext = ".docx")
  writeLines("not a zip", junk)
  expect_identical(DTAtools:::.dta_unzip_docx_checked(junk, withr::local_tempdir()), character(0))
})
