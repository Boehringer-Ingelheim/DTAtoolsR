# What the document exports tell the caller, and what the documents say about
# file counts. REQ-EXPORT-047 .. REQ-EXPORT-049.
#
# Every expected value below is written by hand from the requirement text or
# from the declared counts of a fixture this file builds -- never read back from
# an export or from the function under qualification. Output goes to
# qa_tempdir() only.

cond_stub_pdf <- function(pdf_file) {
  writeBin(charToRaw("%PDF-1.7\n1 0 obj\n<<>>\nendobj\n%%EOF\n"), pdf_file)
  invisible(pdf_file)
}

# A template holding the given lines as paragraphs.
cond_template <- function(lines, dir) {
  path <- tempfile(fileext = ".docx", tmpdir = dir)
  doc <- officer::read_docx()
  for (ln in lines) doc <- officer::body_add_par(doc, ln, style = "Normal")
  print(doc, target = path)
  path
}

# ---- REQ-EXPORT-047: a PDF from a fallback backend is announced ------------------

test_that("OQ-EXPORT-078 | a PDF made by a backend after the first warns, from every PDF export, whatever quiet says | REQ-EXPORT-047 | tags: white-box", {
  dir <- qa_tempdir()
  # Word is the first backend and fails; pandoc is the second and works.
  testthat::local_mocked_bindings(
    .pdf_backends_available = function() c("word", "pandoc"),
    .word_docx_to_pdf = function(docx_file, pdf_file) stop("Word refused the call"),
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) cond_stub_pdf(pdf_file)
  )
  dta <- create_example_DTA()
  ds <- create_example_DTADataSetTabular(2)
  template <- cond_template("Title: {DTA_TITLE}", dir)

  runs <- list(
    `write_dta` = function(f, quiet) write_dta(dta, f, format = "pdf", quiet = quiet),
    `write_dta with a template` = function(f, quiet) {
      write_dta(dta, f, format = "pdf", template = template, quiet = quiet)
    },
    `write_dataset_metadata` = function(f, quiet) write_dataset_metadata(ds, f, format = "pdf", quiet = quiet)
  )
  for (name in names(runs)) {
    for (quiet in c(TRUE, FALSE)) {
      out <- tempfile(fileext = ".pdf", tmpdir = dir)
      caught <- NULL
      suppressMessages(withCallingHandlers(
        runs[[name]](out, quiet),
        warning = function(w) {
          if (inherits(w, "dta_pdf_backend_fallback")) caught <<- w
          invokeRestart("muffleWarning")
        }
      ))
      label <- sprintf("%s, quiet = %s", name, quiet)
      qa_check(paste(label, "- the fallback warning was raised"), !is.null(caught))
      qa_step(paste(label, "- the backend that made the PDF is named"), "pandoc", caught$used)
      qa_step(paste(label, "- the backend that failed is named"), "word", names(caught$failed))
      qa_check(
        paste(label, "- the reason it failed is carried"),
        grepl("Word refused the call", caught$failed[["word"]], fixed = TRUE)
      )
      qa_step(paste(label, "- pandoc re-typesets, so the header and footer are reported lost"), TRUE, caught$furniture_lost)
      qa_check(
        paste(label, "- the message names the backend that failed and the one that made the file"),
        grepl("word", conditionMessage(caught), fixed = TRUE) && grepl("pandoc", conditionMessage(caught), fixed = TRUE)
      )
      qa_step(
        paste(label, "- the PDF itself is still written"),
        charToRaw("%PDF"), readBin(out, "raw", n = 4L)
      )
    }
  }
})

test_that("OQ-EXPORT-079 | no fallback warning when the first backend works, when it is the only one, or when a faithful backend took over | REQ-EXPORT-047 | tags: white-box", {
  dir <- qa_tempdir()
  dta <- create_example_DTA()
  out <- tempfile(fileext = ".pdf", tmpdir = dir)

  warned <- function(expr) {
    got <- FALSE
    suppressMessages(withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "dta_pdf_backend_fallback")) got <<- TRUE
        invokeRestart("muffleWarning")
      }
    ))
    got
  }

  # The first of two backends works: nothing fell back.
  testthat::local_mocked_bindings(
    .pdf_backends_available = function() c("libreoffice", "pandoc"),
    .soffice_docx_to_pdf = function(docx_file, pdf_file) cond_stub_pdf(pdf_file),
    .pandoc_docx_to_pdf = function(docx_file, pdf_file) stop("never reached")
  )
  qa_step(
    "a working first backend raises no fallback warning",
    FALSE, warned(write_dta(dta, out, format = "pdf", overwrite = TRUE, quiet = TRUE))
  )

  # A machine with TinyTeX alone is not a fallback, whatever TinyTeX loses.
  testthat::local_mocked_bindings(
    .pdf_backends_available = function() "tinytex",
    .tinytex_docx_to_pdf = function(docx_file, pdf_file) cond_stub_pdf(pdf_file)
  )
  qa_step(
    "a machine whose only backend is TinyTeX is not warned",
    FALSE, warned(write_dta(dta, out, format = "pdf", overwrite = TRUE, quiet = TRUE))
  )

  # Word fails and LibreOffice, which keeps the running header and footer, takes
  # over: the warning is raised, but it does not claim the loss.
  testthat::local_mocked_bindings(
    .pdf_backends_available = function() c("word", "libreoffice"),
    .word_docx_to_pdf = function(docx_file, pdf_file) stop("word down"),
    .soffice_docx_to_pdf = function(docx_file, pdf_file) cond_stub_pdf(pdf_file)
  )
  caught <- NULL
  suppressMessages(withCallingHandlers(
    write_dta(dta, out, format = "pdf", overwrite = TRUE, quiet = TRUE),
    warning = function(w) {
      if (inherits(w, "dta_pdf_backend_fallback")) caught <<- w
      invokeRestart("muffleWarning")
    }
  ))
  qa_check("a fallback from Word to LibreOffice still warns", !is.null(caught))
  qa_step("but it reports no loss of the header and footer", FALSE, caught$furniture_lost)
})

# ---- REQ-EXPORT-048: unrendered placeholders are always reported -----------------

test_that("OQ-EXPORT-080 | a block placeholder or region marker that stays in the document is reported with quiet = TRUE as well | REQ-EXPORT-048", {
  dir <- qa_tempdir()
  dta <- create_example_DTA()

  # Template lines, and the tokens a reader finds left in the document.
  cases <- list(
    `a block naming an unknown dataset` = list("{COLUMN_SPECS:NOPE}", "{COLUMN_SPECS:NOPE}", FALSE),
    `a block inside a sentence` = list("Columns: {COLUMN_SPECS}", "{COLUMN_SPECS}", FALSE),
    `a region that is never closed` = list(c("BEFORE", "{#DATASETS}", "text"), "{#DATASETS}", TRUE),
    `a closing marker with no opening one` = list("{/DATASETS}", "{/DATASETS}", TRUE)
  )
  catch <- function(expr) {
    caught <- NULL
    suppressMessages(withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "dta_template_blocks_unrendered")) caught <<- w
        invokeRestart("muffleWarning")
      }
    ))
    caught
  }

  for (name in names(cases)) {
    case <- cases[[name]]
    template <- cond_template(case[[1]], dir)
    for (quiet in c(TRUE, FALSE)) {
      via_template <- catch(export_with_template(
        dta, template, tempfile(fileext = ".docx", tmpdir = dir),
        quiet = quiet, fallback = FALSE
      ))
      via_write_dta <- catch(write_dta(
        dta, tempfile(fileext = ".docx", tmpdir = dir),
        template = template, quiet = quiet
      ))
      for (via in c("export_with_template", "write_dta")) {
        w <- if (identical(via, "export_with_template")) via_template else via_write_dta
        label <- sprintf("%s, %s, quiet = %s", name, via, quiet)
        qa_check(paste(label, "- warned"), !is.null(w))
        qa_step(paste(label, "- names what was left"), case[[2]], w$tokens)
        qa_step(
          paste(label, "- a malformed marker is recognisable by its own class"),
          case[[3]], inherits(w, "dta_template_region_malformed")
        )
      }
    }
  }

  # An inline placeholder without a value has its own class and lists its token.
  inline_template <- cond_template("Hello {NOT_A_FIELD}", dir)
  inline <- NULL
  for (quiet in c(TRUE, FALSE)) {
    suppressMessages(withCallingHandlers(
      write_dta(dta, tempfile(fileext = ".docx", tmpdir = dir), template = inline_template, quiet = quiet),
      warning = function(w) {
        if (inherits(w, "dta_template_placeholders_unresolved")) inline <<- w
        invokeRestart("muffleWarning")
      }
    ))
    qa_step(sprintf("an unknown inline placeholder is reported, quiet = %s", quiet), "{NOT_A_FIELD}", inline$tokens)
    inline <- NULL
  }

  # quiet = TRUE silences the progress message, which is all it is for.
  progress <- function(quiet) {
    seen <- 0L
    withCallingHandlers(
      suppressWarnings(write_dta(
        dta, tempfile(fileext = ".docx", tmpdir = dir),
        template = cond_template("{COLUMN_SPECS:NOPE}", dir), quiet = quiet
      )),
      message = function(m) {
        seen <<- seen + 1L
        invokeRestart("muffleMessage")
      }
    )
    seen
  }
  qa_step("quiet = TRUE prints no progress message", 0L, progress(TRUE))
  qa_check("quiet = FALSE prints one", progress(FALSE) >= 1L)

  # And a template that renders completely stays silent.
  ok_template <- cond_template(c("{COLUMN_SPECS}", "{#DATASETS}", "{DATASET_ID}", "{/DATASETS}"), dir)
  qa_step(
    "a template whose blocks all render raises no such warning",
    TRUE,
    is.null(catch(export_with_template(
      dta, ok_template, tempfile(fileext = ".docx", tmpdir = dir),
      quiet = TRUE, fallback = FALSE
    )))
  )
})

# ---- REQ-EXPORT-049: an open maximum number of files ---------------------------

test_that("OQ-EXPORT-081 | a file entry with no maximum reads 'or more', never a range that ends at 0 | REQ-EXPORT-049", {
  dir <- qa_tempdir()
  # one.csv is exactly 1 file; part_*.csv is at least 2 with no upper limit.
  ds <- DTADataSetFile(
    id = "parts",
    files = list(
      DTAFileCSV("one.csv"),
      DTAFileCSV("part_.*\\.csv$", pattern = TRUE, min_number_of_files = 2)
    )
  )

  md <- file.path(dir, "parts.md")
  write_dataset_metadata(ds, md, quiet = TRUE)
  md_lines <- readLines(md, encoding = "UTF-8")
  qa_check(
    "the Markdown sentence totals 1 + at-least-2 as 'or more': at least 3",
    any(startsWith(md_lines, "3 or more files are expected for this dataset in total."))
  )
  qa_check(
    "the Markdown table row of the open entry reads '2 or more'",
    any(grepl("Regex pattern", md_lines, fixed = TRUE) & grepl("| 2 or more |", md_lines, fixed = TRUE))
  )
  qa_step("no Markdown line ends a range at 0", FALSE, any(grepl("to 0", md_lines, fixed = TRUE)))
  qa_step("nor states a total below its own minimum", FALSE, any(grepl("3 to 1", md_lines, fixed = TRUE)))

  docx <- file.path(dir, "parts.docx")
  write_dataset_metadata(ds, docx, quiet = TRUE)
  text <- qa_docx_summary(docx)$text
  qa_check(
    "the Word sentence reads '3 or more files are expected'",
    any(startsWith(text, "3 or more files are expected for this dataset in total."))
  )
  qa_check("the Word table cell of the open entry reads '2 or more'", "2 or more" %in% text)
  qa_step("no Word cell ends a range at 0", FALSE, any(grepl("to 0", text, fixed = TRUE)))

  # The same through write_dta(), Markdown and Word.
  dta <- create_example_DTA()
  dta@datasets <- list(parts = ds)
  dta_md <- file.path(dir, "dta.md")
  write_dta(dta, dta_md, quiet = TRUE)
  dta_md_lines <- readLines(dta_md, encoding = "UTF-8")
  qa_check(
    "write_dta() Markdown reads '3 or more files' and '2 or more'",
    any(startsWith(dta_md_lines, "3 or more files are expected for this dataset in total.")) &&
      any(grepl("Regex pattern", dta_md_lines, fixed = TRUE) & grepl("| 2 or more |", dta_md_lines, fixed = TRUE))
  )
  dta_docx <- file.path(dir, "dta.docx")
  write_dta(dta, dta_docx, quiet = TRUE)
  dta_text <- qa_docx_summary(dta_docx)$text
  qa_check(
    "write_dta() Word reads '3 or more files' and '2 or more'",
    any(startsWith(dta_text, "3 or more files are expected for this dataset in total.")) && "2 or more" %in% dta_text
  )
  qa_step("and neither ends a range at 0", FALSE, any(grepl("to 0", c(dta_md_lines, dta_text), fixed = TRUE)))

  # A bounded entry keeps its range: [1, 1] + [1, 3] is 2 to 4.
  bounded <- DTADataSetFile(
    id = "bounded",
    files = list(
      DTAFileCSV("a.csv"),
      DTAFileCSV("b_.*", pattern = TRUE, min_number_of_files = 1, max_number_of_files = 3)
    )
  )
  bounded_md <- file.path(dir, "bounded.md")
  write_dataset_metadata(bounded, bounded_md, quiet = TRUE)
  qa_check(
    "a bounded dataset still reads '2 to 4 files'",
    any(startsWith(readLines(bounded_md, encoding = "UTF-8"), "2 to 4 files are expected for this dataset in total."))
  )
})
