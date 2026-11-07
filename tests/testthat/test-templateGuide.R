# Renderer (.template_guide_docx) is tested on a hand-made `guide` list built
# from the documented contract, so it does not depend on the collector.

tg_df <- function(...) {
  data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
}

tg_dataset <- function(variant = "") {
  list(
    name = "gf", title = "Genomic Findings data table", variant = variant,
    about = tg_df(key = c("File name", "Columns"), value = c("gf.tsv", "3")),
    source = "GF domain smrnaseq",
    columns = tg_df(
      Column = c("ACMEID\nAcme identifier", "GFNAM\nGene name"),
      Type = c("SAS Char\n$9.", "SAS Char\n$40."),
      `Can be empty` = c("No", "Yes"),
      `Allowed values` = c("Any", "ACME Labs, Other Labs"),
      Description = c("Identifier assigned by ACME.", "Name of the gene.")
    ),
    same_columns = "All other 31 columns are exactly as in the dataset template 'GF domain smrnaseq'.",
    removed_columns = c("OLDCOL"),
    rules = tg_df(Rule = character(), `What it checks` = character()),
    same_rules = "All rules are exactly as in the dataset template."
  )
}

tg_guide <- function() {
  empty <- function(...) tg_df(...)[0, , drop = FALSE]
  list(
    library = list(
      name = "Fixture library", path = "x", generated = "2026-11-06",
      package_version = "9.9.9"
    ),
    overview = tg_df(
      Name = c("Acme GF", "Acme Co"), Kind = c("DTA template", "Party profile"),
      ID = c("gf_acme", "acme"), Version = c("1.0", "1.0"),
      `What it is` = c("Genomic findings for ACME.", "ACME Labs profile.")
    ),
    creation = list(list(
      title = "Genomic Findings ACME", id = "gf_acme", version = "1.0",
      description = "A DTA for ACME genomic findings.",
      summary = "Produces 1 dataset. Asks 2 questions.",
      at_a_glance = tg_df(key = c("Status", "Based on"), value = c("Ready to use", "Genomic Findings")),
      parent = "Genomic Findings",
      changes = tg_df(What = "Header / organization", Change = "Pre-filled answer: Test Company 1 → ACME Labs"),
      questions = tg_df(
        Question = c("Header / organization", "Transfer frequency"),
        `Answer type` = c("Pick one from the list", "Free text, with suggestions"),
        Choices = c("ACME Labs\nOther Labs", "Daily\nWeekly"),
        `Pre-filled answer` = c("ACME Labs", "—"),
        `Appears in the DTA as` = c("Supplier", "Transmission › Frequency"),
        Explanation = c("Who sends the data.", "How often."),
        `Must answer / fixed` = c("Must answer", "—")
      ),
      variants = empty(Question = character(), Choice = character(), `What it changes` = character()),
      parties = tg_df(
        Party = "Supplier", `Appears in the DTA as` = "Supplier",
        `Available profiles` = "ACME Labs", `Must choose` = "Yes"
      ),
      term_choices = empty(
        Choice = character(), Explanation = character(), `Term list` = character(),
        `How it works` = character(), `Selected by default` = character(),
        `Minimum to select` = character()
      ),
      prefilled = empty(Field = character(), Value = character()),
      locked = empty(Field = character(), Rule = character()),
      datasets = list(tg_dataset()),
      build_note = "A sample DTA could not be built from this template: boom",
      technical = "ID gf_acme · version 1.0 · file gf_acme.dta-template.yaml"
    )),
    datasets = list(list(
      title = "GF domain smrnaseq", id = "gf_smrnaseq", version = "1.0",
      description = "Genomic findings columns.", summary = "Defines 33 columns.",
      at_a_glance = tg_df(key = "Dataset name", value = "gf"),
      settings = empty(
        Setting = character(), `Answer type` = character(), `Pre-filled answer` = character(),
        `Appears in the DTA as` = character(), Explanation = character()
      ),
      columns = tg_dataset()$columns,
      rules = empty(Rule = character(), `What it checks` = character()),
      technical = "ID gf_smrnaseq"
    )),
    vocabularies = list(list(
      title = "Gene panels", id = "panels", version = "1.0", description = "",
      summary = "2 terms.", at_a_glance = tg_df(key = "Version", value = "1.0"),
      terms = tg_df(Code = c("P1", "P2"), Label = c("Panel one", "Panel two"), Description = c("", "Second")),
      technical = "ID panels"
    )),
    parties = list(list(
      title = "ACME Labs", id = "acme", version = "1.0", description = "",
      at_a_glance = tg_df(key = "Role", value = "Supplier"),
      contacts = tg_df(
        Name = "Jane Doe", Role = "PI", Email = "jane@acme.test", Phone = "—", Signs = "Yes"
      ),
      technical = "ID acme"
    )),
    problems = tg_df(File = "broken.yaml", Problem = "Could not be read.")
  )
}

tg_summary <- function(guide) {
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  doc <- .template_guide_docx(guide)
  print(doc, target = out)
  list(
    summary = officer::docx_summary(officer::read_docx(out)),
    xml = {
      d <- tempfile()
      utils::unzip(out, files = "word/document.xml", exdir = d)
      paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE), collapse = "")
    }
  )
}

test_that("renderer writes the fixed sections and every chapter heading", {
  res <- tg_summary(tg_guide())
  s <- res$summary
  heads <- s$text[!is.na(s$style_name) & grepl("^heading", s$style_name)]
  expect_true(all(c(
    "How to read this guide", "What is in this library", "DTA templates",
    "Dataset templates", "Term lists", "Party profiles", "Problems found",
    "Genomic Findings ACME", "GF domain smrnaseq", "Gene panels", "ACME Labs",
    "Questions you will be asked", "Datasets it creates",
    "What it changes compared to Genomic Findings"
  ) %in% heads))
  cells <- s$text[s$content_type == "table cell"]
  expect_true(any(grepl("ACME Labs", cells, fixed = TRUE)))
  expect_true(any(grepl("Jane Doe", cells, fixed = TRUE)))
  expect_true(any(grepl("ACMEID", cells, fixed = TRUE)))
  txt <- s$text[s$content_type == "paragraph"]
  expect_true(any(grepl("Everything else is the same as in Genomic Findings.", txt, fixed = TRUE)))
  expect_true(any(grepl("All other 31 columns", txt, fixed = TRUE)))
  expect_true(any(grepl("OLDCOL", txt, fixed = TRUE)))
  expect_true(any(grepl("could not be built", txt, fixed = TRUE)))
})

test_that("multi-line cells become line breaks and header rows repeat", {
  res <- tg_summary(tg_guide())
  expect_match(res$xml, "<w:br/>", fixed = TRUE)
  expect_match(res$xml, "w:tblHeader", fixed = TRUE)
  # a TOC field is present
  expect_match(res$xml, "TOC", fixed = TRUE)
})

test_that("empty tables become one sentence, never an empty grid", {
  res <- tg_summary(tg_guide())
  txt <- res$summary$text[res$summary$content_type == "paragraph"]
  expect_true(any(grepl("fills in no fields automatically", txt, fixed = TRUE)))
  expect_true(any(grepl("No term lists are chosen", txt, fixed = TRUE)))
  # no variants / fixed rows: their headings are omitted entirely
  heads <- res$summary$text[!is.na(res$summary$style_name) & grepl("^heading", res$summary$style_name)]
  expect_false("Variants" %in% heads)
  expect_false("Fixed and mandatory fields" %in% heads)
  # the empty prefilled table's header cells never reach the document
  cells <- res$summary$text[res$summary$content_type == "table cell"]
  expect_false("Field" %in% cells)
})

test_that("a library without problems has no Problems chapter, and an empty one still renders", {
  g <- tg_guide()
  g$problems <- g$problems[0, , drop = FALSE]
  res <- tg_summary(g)
  heads <- res$summary$text[!is.na(res$summary$style_name) & grepl("^heading", res$summary$style_name)]
  expect_false("Problems found" %in% heads)

  g$overview <- g$overview[0, , drop = FALSE]
  g$creation <- g$datasets <- g$vocabularies <- g$parties <- list()
  res2 <- tg_summary(g)
  txt <- res2$summary$text[res2$summary$content_type == "paragraph"]
  expect_true(any(grepl("This library contains no templates.", txt, fixed = TRUE)))
  expect_true(any(grepl("This library has no DTA templates.", txt, fixed = TRUE)))
})

test_that("variant datasets carry the variant in their heading", {
  g <- tg_guide()
  g$creation[[1]]$datasets <- list(tg_dataset("For choice 'Long'"))
  s <- tg_summary(g)$summary
  expect_true(any(grepl("For choice 'Long'", s$text, fixed = TRUE)))
})

# ---- API guards -------------------------------------------------------------

test_that("write_template_guide refuses a bad format before doing any work", {
  out <- tempfile(fileext = ".html")
  expect_error(
    write_template_guide(tempfile("missing"), out),
    class = "dta_template_guide_format"
  )
  expect_error(
    write_template_guide(tempfile("missing"), tempfile(fileext = ".docx"), format = "md"),
    class = "dta_template_guide_format"
  )
  expect_false(file.exists(out))
})

test_that("write_template_guide leaves an existing file untouched without overwrite", {
  out <- tempfile(fileext = ".docx")
  writeLines("precious", out)
  before <- unname(tools::md5sum(out))
  expect_error(
    write_template_guide(tempfile("missing"), out),
    class = "dta_template_guide_exists"
  )
  expect_identical(unname(tools::md5sum(out)), before)
})

# ---- end to end on the bundled library --------------------------------------

test_that("the bundled library produces a readable guide", {
  lib <- system.file("extdata", "templates", package = "DTAtools")
  skip_if(!nzchar(lib), "bundled templates not installed")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  expect_identical(write_template_guide(lib, out, quiet = TRUE), out)
  s <- officer::docx_summary(officer::read_docx(out))
  heads <- s$text[!is.na(s$style_name) & grepl("^heading", s$style_name)]
  expect_true(all(c(
    "How to read this guide", "What is in this library", "DTA templates"
  ) %in% heads))
  cells <- s$text[s$content_type == "table cell"]
  expect_true(any(grepl("ACME Labs", cells, fixed = TRUE)))

  # overwrite guard on a real file
  expect_error(write_template_guide(lib, out, quiet = TRUE), class = "dta_template_guide_exists")
  expect_identical(write_template_guide(lib, out, overwrite = TRUE, quiet = TRUE), out)
})

test_that("a .pdf extension writes a real PDF", {
  skip_if(is.null(dta_pdf_backend()), "no PDF backend on this machine")
  lib <- system.file("extdata", "templates", package = "DTAtools")
  skip_if(!nzchar(lib), "bundled templates not installed")
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  write_template_guide(lib, out, quiet = TRUE)
  expect_identical(rawToChar(readBin(out, "raw", 4)), "%PDF")
})
