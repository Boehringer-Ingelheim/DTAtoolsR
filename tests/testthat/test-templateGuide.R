# The RENDER half of write_template_guide(): one document per template, read
# back from the written docx. Expected values come from the YAML files (or are
# spelled out), never from the software. Messages are never asserted on, only
# condition classes (R and cli translate them).

tg_lib <- function() system.file("extdata", "library", package = "DTAtools")

# Library files live in kind folders: templates/, parties/, vocabulary/.
tg_path <- function(root, folder, name) {
  dir.create(file.path(root, folder), showWarnings = FALSE, recursive = TRUE)
  file.path(root, folder, name)
}
tg_folder_of <- function(name) {
  if (grepl("\\.dta-vocabulary\\.yaml$", name)) {
    "vocabulary"
  } else if (grepl("\\.dta-party\\.yaml$", name)) {
    "parties"
  } else {
    "templates"
  }
}
# A scratch copy of the bundled kind folders.
tg_copy_lib <- function(to) {
  file.copy(file.path(tg_lib(), c("templates", "parties", "vocabulary")), to, recursive = TRUE)
}

tg_yaml <- function(id) {
  yaml::read_yaml(file.path(tg_lib(), "templates", paste0(id, ".dta-template.yaml")))
}

# Bundled guides are written once per test file.
tg_cache <- new.env()
tg_written <- function(org = "Example Pharma", unit = "Biometrics") {
  key <- paste(org, unit)
  if (is.null(tg_cache[[key]])) {
    dir <- tempfile("guides")
    tg_cache$dirs <- c(tg_cache$dirs, dir)
    tg_cache[[key]] <- write_template_guide(
      tg_lib(), dir,
      organisation = org, organisation_unit = unit, quiet = TRUE
    )
  }
  tg_cache[[key]]
}

tg_unzip <- function(file) {
  ex <- tempfile("unz")
  utils::unzip(file, exdir = ex)
  ex
}

tg_xml_texts <- function(file, part_pattern) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  parts <- list.files(file.path(ex, "word"), part_pattern, full.names = TRUE)
  vapply(parts, function(p) {
    x <- xml2::read_xml(p)
    paste(xml2::xml_text(xml2::xml_find_all(x, ".//*[local-name()='t' or local-name()='instrText']")), collapse = "")
  }, character(1))
}

tg_text <- function(file) {
  s <- officer::docx_summary(officer::read_docx(file))
  paste(s$text[!is.na(s$text)], collapse = "\n")
}

# Every table with its section's orientation and text width (twips).
tg_tables <- function(file) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  body <- xml2::xml_children(xml2::xml_find_first(
    xml2::read_xml(file.path(ex, "word", "document.xml")), "//*[local-name()='body']"
  ))
  out <- list()
  pending <- list()
  close_section <- function(sect) {
    sz <- xml2::xml_find_first(sect, "./*[local-name()='pgSz']")
    mar <- xml2::xml_find_first(sect, "./*[local-name()='pgMar']")
    num <- function(node, a) as.numeric(xml2::xml_attr(node, a))
    info <- list(
      orient = xml2::xml_attr(sz, "orient"),
      text_w = num(sz, "w") - num(mar, "left") - num(mar, "right")
    )
    for (t in pending) {
      grid <- as.numeric(xml2::xml_attr(
        xml2::xml_find_all(t, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"), "w"
      ))
      out[[length(out) + 1L]] <<- c(info, list(grid = grid))
    }
    pending <<- list()
  }
  for (node in body) {
    nm <- xml2::xml_name(node)
    if (nm == "tbl") {
      pending[[length(pending) + 1L]] <- node
    } else if (nm == "p") {
      sect <- xml2::xml_find_first(node, "./*[local-name()='pPr']/*[local-name()='sectPr']")
      if (!inherits(sect, "xml_missing")) close_section(sect)
    } else if (nm == "sectPr") {
      close_section(node)
    }
  }
  out
}

# ---- selection and file names ----------------------------------------------

test_that("one file per non-abstract template, named from the YAML", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  written <- tg_written()
  files <- list.files(file.path(tg_lib(), "templates"), "\\.dta-template\\.yaml$", full.names = TRUE)
  y <- lapply(files, yaml::read_yaml)
  y <- Filter(function(d) !isTRUE(d$abstract), y)
  expected <- vapply(y, function(d) sprintf("%s_v%s_template_guide.docx", d$id, d$version), character(1))
  expect_setequal(basename(written), expected)
  expect_setequal(names(written), vapply(y, function(d) d$id, character(1)))
  expect_true(all(file.exists(written)))
  expect_identical(
    sort(list.files(dirname(written[[1]])), method = "radix"),
    sort(expected, method = "radix")
  )
})

test_that("a building block has no guide of its own", {
  dir <- withr::local_tempdir()
  tpl <- function(id, label, ...) {
    c(
      "kind: dta_creation_template", paste0("id: ", id), "version: '1.0'",
      paste0("label: ", label), ..., "base:", "  metadata:", "    title: T"
    )
  }
  writeLines(tpl("shared", "Shared", "abstract: true"), tg_path(dir, "templates", "shared.dta-template.yaml"))
  writeLines(tpl("child", "Child", "extends: shared@1.0"), tg_path(dir, "templates", "child.dta-template.yaml"))
  out <- withr::local_tempdir()
  expect_error(
    write_template_guide(dir, out, templates = "shared", quiet = TRUE),
    class = "dta_template_guide_abstract"
  )
  expect_error(
    write_template_guide(dir, out, templates = "nope", quiet = TRUE),
    class = "dta_template_guide_unknown"
  )
  expect_length(list.files(out), 0L)
  # By default only the usable template is documented, not its building block.
  # (the child has no datasets, so it cannot be built and is warned about)
  expect_warning(
    got <- write_template_guide(dir, out, quiet = TRUE),
    class = "dta_template_guide_problems"
  )
  expect_identical(names(got), "child")
})

test_that("an existing output aborts before anything is written", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  precious <- file.path(out, "biomarker_gf_acme_v1.0_template_guide.docx")
  writeLines("precious", precious)
  before <- unname(tools::md5sum(precious))
  expect_error(
    write_template_guide(tg_lib(), out, templates = c("biomarker_gf", "biomarker_gf_acme"), quiet = TRUE),
    class = "dta_template_guide_exists"
  )
  expect_identical(list.files(out), basename(precious))
  expect_identical(unname(tools::md5sum(precious)), before)
  got <- write_template_guide(tg_lib(), out,
    templates = c("biomarker_gf", "biomarker_gf_acme"), overwrite = TRUE, quiet = TRUE
  )
  expect_length(list.files(out), 2L)
  expect_identical(basename(got[["biomarker_gf_acme"]]), basename(precious))
})

test_that("a bad format is refused before any work, and a single file selects its template", {
  expect_error(
    write_template_guide(tempfile("missing"), tempfile(), format = "md"),
    class = "dta_template_guide_format"
  )
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  got <- write_template_guide(
    file.path(tg_lib(), "templates", "biomarker_gf_acme.dta-template.yaml"), out,
    quiet = TRUE
  )
  expect_identical(names(got), "biomarker_gf_acme")
})

test_that("a PDF made through a re-typesetting backend warns about the lost layout", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  local_mocked_bindings(
    dta_pdf_backend = function() list(name = "tinytex", engine = "", available = "tinytex"),
    .convert_docx_to_pdf = function(docx_file, pdf_file) writeLines("%PDF", pdf_file)
  )
  out <- withr::local_tempdir()
  expect_warning(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", format = "pdf", quiet = TRUE),
    class = "dta_template_guide_pdf_layout"
  )
  expect_identical(list.files(out), "biomarker_gf_v1.0_template_guide.pdf")
})

# ---- cover, properties, furniture -------------------------------------------

test_that("cover title, properties, header and footer come from the template", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  written <- tg_written()
  for (id in c("biomarker_gf", "biomarker_gf_acme")) {
    y <- tg_yaml(id)
    file <- written[[id]]
    s <- officer::docx_summary(officer::read_docx(file))
    paras <- Filter(nzchar, s$text[s$content_type == "paragraph"])
    expect_identical(paras[[1]], "TEMPLATE GUIDE")
    expect_identical(paras[[2]], y$label)

    props <- officer::doc_properties(officer::read_docx(file))
    expect_identical(props$value[props$tag == "title"], paste0("Template guide: ", y$label))
    expect_identical(props$value[props$tag == "subject"], id)
    expect_identical(props$value[props$tag == "creator"], "Example Pharma")

    headers <- tg_xml_texts(file, "^header[0-9]+\\.xml$")
    expect_gt(length(headers), 0)
    for (h in headers) {
      expect_match(h, "Example Pharma", fixed = TRUE)
      expect_match(h, "Biometrics", fixed = TRUE)
      expect_match(h, "Template guide: ", fixed = TRUE)
      expect_match(h, "Page\\s*PAGE\\s*of\\s*NUMPAGES")
    }
    footers <- tg_xml_texts(file, "^footer[0-9]+\\.xml$")
    expect_gt(length(footers), 0)
    for (f in footers) {
      expect_match(f, id, fixed = TRUE)
      expect_match(f, paste("Template version", y$version), fixed = TRUE)
    }
  }
})

test_that("the organisation comes from the library unless given, and '' shows none", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  lib <- withr::local_tempdir()
  tg_copy_lib(lib)
  writeLines(
    c("kind: dta_app_config", "organisation:", "  name: Library Org", "  unit: Library Unit"),
    file.path(lib, "dta-app.yaml")
  )
  headers_of <- function(...) {
    out <- withr::local_tempdir()
    got <- write_template_guide(lib, out, templates = "biomarker_gf", quiet = TRUE, ...)
    tg_xml_texts(got[[1]], "^header[0-9]+\\.xml$")
  }
  for (h in headers_of()) expect_match(h, "Library Org", fixed = TRUE)
  for (h in headers_of(organisation = "")) {
    expect_false(grepl("Library", h, fixed = TRUE))
    expect_match(h, "Template guide: ", fixed = TRUE)
  }
  for (h in headers_of(organisation = "Explicit Org")) {
    expect_match(h, "Explicit Org", fixed = TRUE)
    expect_false(grepl("Library", h, fixed = TRUE))
  }
})

# ---- layout: the user's complaints, pinned structurally ---------------------

test_that("no narrow or crowded tables, and every table fits its section", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  for (file in tg_written()) {
    tabs <- tg_tables(file)
    expect_gt(length(tabs), 5)
    for (t in tabs) {
      expect_lte(length(t$grid), 5)
      if (!identical(t$orient, "landscape")) expect_lte(length(t$grid), 4)
      expect_true(all(t$grid >= 864))
      expect_lte(sum(t$grid), t$text_w * 1.01)
    }
  }
})

test_that("datasets sit in one landscape section", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  file <- tg_written()[["biomarker_gf_acme"]]
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  doc <- readLines(file.path(ex, "word", "document.xml"), warn = FALSE)
  expect_match(paste(doc, collapse = ""), "w:orient=\"landscape\"", fixed = TRUE)
  tabs <- tg_tables(file)
  landscape <- Filter(function(t) identical(t$orient, "landscape"), tabs)
  expect_gt(length(landscape), 0)
  # a column table of 5 columns exists, and only landscape tables have that many
  expect_true(any(vapply(landscape, function(t) length(t$grid) == 5, logical(1))))
  expect_false(any(vapply(Filter(function(t) !identical(t$orient, "landscape"), tabs), function(t) length(t$grid) == 5, logical(1))))
  # exactly one landscape section: portrait, landscape, portrait
  orient <- xml2::xml_attr(xml2::xml_find_all(
    xml2::read_xml(file.path(ex, "word", "document.xml")), "//*[local-name()='sectPr']/*[local-name()='pgSz']"
  ), "orient")
  expect_identical(sum(orient %in% "landscape"), 1L)
})

test_that("every question and every built column is in the document", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  written <- tg_written()
  ds <- yaml::read_yaml(file.path(tg_lib(), "templates", "gf_smrnaseq.dta-dataset-template.yaml"))$dataset
  ids <- vapply(ds$columns, function(cl) cl$id, character(1))

  gf <- tg_text(written[["biomarker_gf"]])
  for (o in tg_yaml("biomarker_gf")$options) {
    expect_match(gf, o$label, fixed = TRUE)
  }
  for (id in ids) expect_match(gf, id, fixed = TRUE)

  acme <- tg_text(written[["biomarker_gf_acme"]])
  for (id in c(ids, "ACMEID")) expect_match(acme, id, fixed = TRUE)
})

test_that("changes against the dataset template are marked", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  acme <- tg_text(tg_written()[["biomarker_gf_acme"]])
  expect_match(acme, "Changed by this template", fixed = TRUE)
  expect_match(acme, "Added by this template", fixed = TRUE)
  gf <- tg_text(tg_written()[["biomarker_gf"]])
  expect_false(grepl("by this template", gf, fixed = TRUE))
})

test_that("sections that do not apply are left out, and the list on the cover matches", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  for (id in c("biomarker_gf", "biomarker_gf_acme")) {
    s <- officer::docx_summary(officer::read_docx(tg_written()[[id]]))
    h1 <- s$text[s$style_name %in% "heading 1"]
    paras <- s$text[s$content_type == "paragraph"]
    listed <- sub("^[0-9]+  ", "", paras[grepl("^[0-9]+  \\S", paras)])
    expect_identical(listed, h1)
  }
  gf <- officer::docx_summary(officer::read_docx(tg_written()[["biomarker_gf"]]))
  expect_false(any(grepl("^How it differs", gf$text[gf$style_name %in% "heading 1"])))
})

# ---- stubs and rendering of hand-made entries --------------------------------

test_that("a template that cannot be built still gets a document and a warning", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: broken", "version: '1.0'", "label: Broken one",
      "extends: missing_parent@1.0", "base:", "  metadata:", "    title: T"
    ),
    tg_path(dir, "templates", "broken.dta-template.yaml")
  )
  out <- withr::local_tempdir()
  expect_warning(
    got <- write_template_guide(dir, out, quiet = TRUE),
    class = "dta_template_guide_problems"
  )
  expect_identical(basename(got[["broken"]]), "broken_v1.0_template_guide.docx")
  s <- officer::docx_summary(officer::read_docx(got[["broken"]]))
  expect_true("Overview" %in% s$text[s$style_name %in% "heading 1"])
  expect_true("Technical details" %in% s$text[s$style_name %in% "heading 1"])
})

test_that("long choice lists are cut, and the cut says how many are left", {
  bullet <- paste0(intToUtf8(0x2022), " ")
  expect_identical(.tg_choices("a\nb"), paste0(bullet, c("a", "b"), collapse = "\n"))
  cut <- .tg_choices(paste(letters[1:14], collapse = "\n"))
  lines <- strsplit(cut, "\n", fixed = TRUE)[[1]]
  expect_length(lines, 11L)
  expect_identical(lines[1:10], paste0(bullet, letters[1:10]))
  expect_match(lines[[11]], "4 more", fixed = TRUE)
})

test_that("the file name is sanitised and drops an empty version", {
  expect_identical(.tg_file_name("a b/c", "1.0", "pdf"), "a_b_c_v1.0_template_guide.pdf")
  expect_identical(.tg_file_name("x", "", "docx"), "x_template_guide.docx")
})

test_that("a PDF is a real PDF when a backend exists", {
  skip_if(is.null(dta_pdf_backend()), "no PDF backend on this machine")
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  got <- suppressWarnings(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", format = "pdf", quiet = TRUE)
  )
  expect_identical(rawToChar(readBin(got[[1]], "raw", 4)), "%PDF")
})

# ---- review fixes: wording, versions, safe writing ----------------------------

# Bundled library plus extra files, in a scratch folder.
tg_scratch <- function(extra = list()) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  tg_copy_lib(dir)
  for (n in names(extra)) writeLines(extra[[n]], tg_path(dir, tg_folder_of(n), n))
  dir
}

tg_heading_texts <- function(file, style) {
  s <- officer::docx_summary(officer::read_docx(file))
  s$text[s$style_name %in% style]
}

# Cells (text per table) of every table in a docx.
tg_cell_texts <- function(file) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  lapply(xml2::xml_find_all(x, "//*[local-name()='tbl']"), function(t) {
    vapply(xml2::xml_find_all(t, ".//*[local-name()='tc']"), function(tc) {
      paste(xml2::xml_text(xml2::xml_find_all(tc, ".//*[local-name()='t']")), collapse = "")
    }, character(1))
  })
}

test_that("a building block parent gets no 'everything else is the same' promise", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  dir <- tg_scratch(list(
    "biomarker_gf.dta-template.yaml" = sub(
      "^id: biomarker_gf$", "id: biomarker_gf\nabstract: true",
      readLines(file.path(tg_lib(), "templates", "biomarker_gf.dta-template.yaml")),
      perl = TRUE
    )
  ))
  out <- withr::local_tempdir()
  got <- suppressWarnings(write_template_guide(dir, out, templates = "biomarker_gf_acme", quiet = TRUE))
  txt <- tg_text(got[[1]])
  expect_false(grepl("Everything else is the same", txt, fixed = TRUE))
  expect_match(txt, "is a building block, so this guide does not list differences", fixed = TRUE)
  # The bundled (buildable) parent keeps the promise.
  expect_match(tg_text(tg_written()[["biomarker_gf_acme"]]), "Everything else is the same as in", fixed = TRUE)
})

test_that("a term list is documented once, in the version the template pins", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  v1 <- readLines(file.path(tg_lib(), "vocabulary", "gf_tstdtl.dta-vocabulary.yaml"))
  v2 <- sub("^version: .*$", "version: \"2.0\"", v1)
  v2 <- sub("code: FRAGMENT COUNT", "code: NEWER ONLY", v2, fixed = TRUE)
  dir <- tg_scratch(list("gf_tstdtl_2.dta-vocabulary.yaml" = v2))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, templates = "biomarker_gf", quiet = TRUE)
  h2 <- tg_heading_texts(got[[1]], "heading 2")
  expect_identical(sum(h2 == "Genomic measurement detail"), 1L)
  txt <- tg_text(got[[1]])
  expect_match(txt, "FRAGMENT COUNT", fixed = TRUE)
  expect_false(grepl("NEWER ONLY", txt, fixed = TRUE))
})

test_that("a single template file documents that file's version, not the newest", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  old <- readLines(file.path(tg_lib(), "templates", "biomarker_gf.dta-template.yaml"))
  newer <- sub("^version: .*$", "version: \"2.0\"", old)
  dir <- tg_scratch(list("biomarker_gf_2.dta-template.yaml" = newer))
  out <- withr::local_tempdir()
  got <- write_template_guide(file.path(dir, "templates", "biomarker_gf.dta-template.yaml"), out, quiet = TRUE)
  expect_identical(basename(got[["biomarker_gf"]]), "biomarker_gf_v1.0_template_guide.docx")
  got2 <- write_template_guide(file.path(dir, "templates", "biomarker_gf_2.dta-template.yaml"), out, quiet = TRUE)
  expect_identical(basename(got2[["biomarker_gf"]]), "biomarker_gf_v2.0_template_guide.docx")
})

test_that("a failed write removes what this call created and keeps what was there", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  n <- 0L
  local_mocked_bindings(
    .dta_write_staged = function(target, writer) {
      n <<- n + 1L
      if (n == 2L) cli::cli_abort("disk full", class = "tg_test_disk")
      writeLines("x", target)
    }
  )
  expect_error(
    write_template_guide(tg_lib(), out, templates = c("biomarker_gf", "biomarker_gf_acme"), quiet = TRUE),
    class = "tg_test_disk"
  )
  expect_length(list.files(out), 0L)
})

test_that("a failed write leaves a file that already existed alone", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  keep <- file.path(out, "biomarker_gf_v1.0_template_guide.docx")
  writeLines("old", keep)
  local_mocked_bindings(.dta_write_staged = function(target, writer) cli::cli_abort("no", class = "tg_test_disk"))
  expect_error(
    write_template_guide(tg_lib(), out, templates = c("biomarker_gf", "biomarker_gf_acme"), overwrite = TRUE, quiet = TRUE),
    class = "tg_test_disk"
  )
  expect_identical(list.files(out), basename(keep))
  expect_identical(readLines(keep), "old")
})

test_that("file names that differ only in case collide", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  mk <- function(id) {
    c(
      "kind: dta_creation_template", paste0("id: ", id), "version: '1.0'", "label: Case",
      "datasets:", "  - id: tiny", "    name: Tiny", "    type: tabular",
      "    files: {filename: t.csv, type: csv}",
      "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
    )
  }
  dir <- withr::local_tempdir()
  writeLines(mk("Case_tpl"), tg_path(dir, "templates", "a.dta-template.yaml"))
  writeLines(mk("case_tpl"), tg_path(dir, "templates", "b.dta-template.yaml"))
  out <- withr::local_tempdir()
  expect_error(write_template_guide(dir, out, quiet = TRUE), class = "dta_template_guide_exists")
  expect_length(list.files(out), 0L)
})

test_that("organisation arguments are validated like write_dta, and '' still means none", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  expect_error(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", organisation = c("a", "b"), quiet = TRUE),
    "organisation"
  )
  expect_error(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", organisation_unit = 3, quiet = TRUE),
    "organisation_unit"
  )
  expect_length(list.files(out), 0L)
  got <- write_template_guide(tg_lib(), out, templates = "biomarker_gf", organisation = "", quiet = TRUE)
  props <- officer::doc_properties(officer::read_docx(got[[1]]))
  expect_identical(props$value[props$tag == "creator"], "DTAtools")
})

test_that("questions, filled-in fields and the glossary use the reviewed wording", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  gf <- tg_text(tg_written()[["biomarker_gf"]])
  # No question of biomarker_gf is required (checked in the YAML).
  expect_null(tg_yaml("biomarker_gf")$required)
  expect_match(gf, "All questions are optional; each starts with its pre-filled answer where one is shown.", fixed = TRUE)
  expect_match(gf, "Your answers to the questions in section", fixed = TRUE)
  expect_match(gf, "Version history (first entry)", fixed = TRUE)
  expect_false(grepl("Version history 1", gf, fixed = TRUE))
  expect_match(gf, "Supplier: organisation and contacts", fixed = TRUE)
  expect_match(gf, "Document header (organisation name)", fixed = TRUE)
  expect_false(grepl("can be changed by question", gf, fixed = TRUE))
  # Glossary entries for columns and rules; a cover pointer with quoted terms.
  expect_match(gf, "How the value is stored", fixed = TRUE)
  expect_match(gf, "A check the data must pass", fixed = TRUE)
  expect_match(gf, "Terms such as \"Pre-filled answer\" are explained in section", fixed = TRUE)
  # Suggestions and bullets.
  expect_match(gf, "Suggestions", fixed = TRUE)
  expect_match(gf, paste0(intToUtf8(0x2022), " "), fixed = TRUE)
})

test_that("a required question reads 'Must be answered' in the intro and the card", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  dir <- withr::local_tempdir()
  writeLines(c(
    "kind: dta_creation_template", "id: req_tpl", "version: '1.0'", "label: Required one",
    "base:", "  metadata:", "    title: T",
    "required: [base.metadata.header]",
    "options:",
    "  - {id: header, label: Header, type: text, target: metadata.header, default: Acme}",
    "datasets:", "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ), tg_path(dir, "templates", "req.dta-template.yaml"))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, quiet = TRUE)
  txt <- tg_text(got[[1]])
  expect_match(txt, "Questions marked \"Must be answered\" cannot be left empty; all others are optional.", fixed = TRUE)
  expect_match(txt, "Must be answered", fixed = TRUE)
  expect_false(grepl("All questions are optional", txt, fixed = TRUE))
})

test_that("small tables stay on one page, and table widths follow the review", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  file <- tg_written()[["biomarker_gf"]]
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  tbls <- xml2::xml_find_all(x, "//*[local-name()='tbl']")
  txt <- vapply(tbls, function(t) paste(xml2::xml_text(xml2::xml_find_all(t, ".//*[local-name()='t']")), collapse = " "), character(1))
  grid <- function(t) as.numeric(xml2::xml_attr(xml2::xml_find_all(t, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"), "w"))
  # Rules table (<= 15 rows): every row but the last keeps with the next one.
  rules <- tbls[[which(startsWith(txt, "Rule ID Description"))[[1]]]]
  rows <- xml2::xml_find_all(rules, "./*[local-name()='tr']")
  expect_lte(length(rows) - 1L, 15L)
  keep <- vapply(rows, function(r) length(xml2::xml_find_all(r, ".//*[local-name()='keepNext']")) > 0L, logical(1))
  expect_true(all(keep[-length(keep)]))
  expect_false(keep[[length(keep)]])
  g <- grid(rules)
  expect_equal(g[[1]] / g[[2]], 2.0 / 7.7, tolerance = 0.02)
  # Supplier and receiver: the repeated column is gone, three columns remain.
  par <- tbls[[which(grepl("Profiles you can choose", txt, fixed = TRUE))[[1]]]]
  expect_length(grid(par), 3L)
  # Facts of a dataset no longer repeat the counts the headings carry.
  cells <- unlist(tg_cell_texts(file))
  expect_false(any(cells %in% c("Columns", "Rules")))
})

test_that("contact widths follow the review", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  file <- tg_written()[["biomarker_gf"]]
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  tbls <- xml2::xml_find_all(x, "//*[local-name()='tbl']")
  txt <- vapply(tbls, function(t) paste(xml2::xml_text(xml2::xml_find_all(t, ".//*[local-name()='t']")), collapse = " "), character(1))
  ct <- tbls[[which(grepl("Signs the DTA", txt, fixed = TRUE))[[1]]]]
  g <- as.numeric(xml2::xml_attr(xml2::xml_find_all(ct, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"), "w"))
  expect_equal(g / sum(g), c(1.6, 1.6, 2.2, 0.9) / 6.3, tolerance = 0.02)
})

# ---- library layout ----------------------------------------------------------

# A scratch library ROOT: a copy of the bundled kind folders, plus an
# unrelated template and the two things the guide must leave alone.
tg_scratch_library <- function(.local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  tg_copy_lib(root)
  writeLines(
    c(
      "kind: dta_creation_template", "id: zeta_unrelated", "version: '1.0'",
      "label: Unrelated Zeta Template",
      "base:", "  metadata:", "    title: Zeta",
      "datasets:",
      "  - id: z", "    name: Zeta", "    type: tabular",
      "    files: {filename: z.csv, type: csv}",
      "    columns:", "      - {id: ZZ, type: SAS Char}", "    rules: []"
    ),
    tg_path(root, "templates", "zeta.dta-template.yaml")
  )
  writeLines("not a template", tg_path(root, "export-templates", "README.md"))
  writeLines("kind: dta_app_config", file.path(root, "dta-app.yaml"))
  root
}

test_that("a library root is guided across its kind folders", {
  skip_if(!nzchar(tg_lib()), "bundled library not installed")
  root <- tg_scratch_library()
  out <- withr::local_tempdir()
  got <- write_template_guide(root, out, quiet = TRUE)
  expect_true(all(c("biomarker_gf", "biomarker_gf_acme", "zeta_unrelated") %in% names(got)))
})

test_that("a single template file in its kind folder guides that template from the library root", {
  skip_if(!nzchar(tg_lib()), "bundled library not installed")
  root <- tg_scratch_library()
  out <- withr::local_tempdir()
  file <- file.path(root, "templates", "biomarker_gf_acme.dta-template.yaml")
  got <- write_template_guide(file, out, quiet = TRUE)
  expect_identical(names(got), "biomarker_gf_acme")
  # The parties and term lists it uses come from their own folders.
  expect_match(tg_text(got[[1]]), "ACME Labs", fixed = TRUE)
})

test_that("a single template file outside the folder of its kind aborts with dta_template_guide_no_templates", {
  skip_if(!nzchar(tg_lib()), "bundled library not installed")
  root <- tg_scratch_library()
  src <- file.path(root, "templates", "biomarker_gf_acme.dta-template.yaml")
  stray <- c(
    file.path(root, "biomarker_gf_acme.dta-template.yaml"),
    file.path(root, "parties", "biomarker_gf_acme.dta-template.yaml")
  )
  file.copy(src, stray)
  for (f in stray) {
    out <- tempfile("guides")
    expect_error(
      write_template_guide(f, out, quiet = TRUE),
      class = "dta_template_guide_no_templates", info = f
    )
    expect_false(dir.exists(out), info = f)
  }
})

test_that("a kind folder given as the library aborts with dta_template_guide_no_templates", {
  skip_if(!nzchar(tg_lib()), "bundled library not installed")
  root <- tg_scratch_library()
  out <- tempfile("guides")
  expect_error(
    write_template_guide(file.path(root, "templates"), out, quiet = TRUE),
    class = "dta_template_guide_no_templates"
  )
  expect_false(dir.exists(out))
})

# ---- rules in plain English ----------------------------------------------------

# Every table of a docx whose header row is "Rule ID | Description": per table
# the rows as list(id, description), the description with a Word line break
# written as "\n", plus the text and boldness of every run and the fonts used.
tg_rule_tables <- function(file) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  par_text <- function(p) {
    paste(vapply(
      xml2::xml_find_all(p, ".//*[local-name()='t' or local-name()='br']"),
      function(n) if (xml2::xml_name(n) == "br") "\n" else xml2::xml_text(n),
      character(1)
    ), collapse = "")
  }
  out <- list()
  for (t in xml2::xml_find_all(x, "//*[local-name()='tbl']")) {
    rows <- xml2::xml_find_all(t, "./*[local-name()='tr']")
    cells <- lapply(rows, function(r) xml2::xml_find_all(r, "./*[local-name()='tc']"))
    cell_text <- function(tc) paste(vapply(xml2::xml_find_all(tc, ".//*[local-name()='p']"), par_text, character(1)), collapse = "\n")
    if (length(cells[[1]]) == 2L && identical(cell_text(cells[[1]][[1]]), "Rule ID") &&
      identical(cell_text(cells[[1]][[2]]), "Description")) {
      body <- cells[-1]
      runs <- xml2::xml_find_all(t, ".//*[local-name()='r']")
      out[[length(out) + 1L]] <- list(
        run_text = vapply(runs, function(r) paste(xml2::xml_text(xml2::xml_find_all(r, "./*[local-name()='t']")), collapse = ""), character(1)),
        run_bold = vapply(runs, function(r) {
          b <- xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='b']")
          !inherits(b, "xml_missing") && !(xml2::xml_attr(b, "val") %in% c("false", "0"))
        }, logical(1)),
        fonts = unique(xml2::xml_attr(xml2::xml_find_all(t, ".//*[local-name()='rFonts']"), "ascii")),
        id = vapply(body, function(r) cell_text(r[[1]]), character(1)),
        description = vapply(body, function(r) cell_text(r[[2]]), character(1))
      )
    }
  }
  out
}

tg_rules_template <- function() {
  cols <- list(
    list(id = "AA", type = "SAS Num", format = "SAS 8."),
    list(id = "BB", type = "SAS Char", format = "SAS $5."),
    list(id = "CC", type = "SAS Char", format = "SAS $5.")
  )
  rules1 <- list(
    list(id = "r_range", description = "Keep AA small", type = "col_range", columns = "AA", min = 0, max = 10),
    list(id = "r_unique", type = "col_unique", columns = list("BB", "CC")),
    list(
      id = "r_cond", description = "If BB is x then AA is at least 3", type = "col_condition",
      condition = list(BB = list(equals = "x")), then = list(AA = list(min = 3))
    )
  )
  ds <- function(id, rules) {
    list(
      id = id, name = id, type = "tabular",
      files = list(filename = paste0(id, ".csv"), type = "csv"), columns = cols, rules = rules
    )
  }
  tpl <- list(
    kind = "dta_creation_template", id = "rules_tpl", version = "1.0", label = "Rules",
    base = list(metadata = list(title = "T")),
    datasets = list(ds("first_ds", rules1), ds("second_ds", rules1[1]))
  )
  list(tpl = tpl, cols = cols, rules = rules1)
}

test_that("the rules table reads Rule ID | Description and shows the full wording of each rule", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  x <- tg_rules_template()
  dir <- tg_scratch()
  writeLines(yaml::as.yaml(x$tpl), tg_path(dir, "templates", "rules_tpl.dta-template.yaml"))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, templates = "rules_tpl", quiet = TRUE)
  tbls <- tg_rule_tables(got[[1]])
  expect_length(tbls, 2L)
  expect_identical(tbls[[1]]$id, c("r_range", "r_unique", "r_cond"))
  expect_identical(tbls[[2]]$id, "r_range")

  # Wording from the specification (R/rulePhrasing.R), written by hand: the
  # author's text is its own "Purpose" line, the statement follows.
  d <- strsplit(tbls[[1]]$description, "\n", fixed = TRUE)
  expect_identical(d[[1]][1:2], c(
    "Purpose: Keep AA small",
    "Every value in column AA must be a number from 0 to 10, inclusive."
  ))
  expect_identical(
    d[[2]][[1]],
    "No two rows may have the same combination of values in columns BB and CC."
  )
  expect_identical(d[[3]][1:2], c(
    "Purpose: If BB is x then AA is at least 3",
    "When BB is equal to \u201cx\u201d, AA must be at least 3."
  ))

  # The cell carries exactly the shared rule text, for every rule.
  specs <- specs_from_list(x$cols, x$rules)
  for (i in seq_along(specs@rules)) {
    expect_identical(tbls[[1]]$description[[i]], .rule_text(specs@rules[[i]]))
  }
})

test_that("rule labels are bold, lists are bullets, and no rule is set in monospace", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  x <- tg_rules_template()
  dir <- tg_scratch()
  writeLines(yaml::as.yaml(x$tpl), tg_path(dir, "templates", "rules_tpl.dta-template.yaml"))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, templates = "rules_tpl", quiet = TRUE)
  t1 <- tg_rule_tables(got[[1]])[[1]]
  run_text <- t1$run_text
  is_bold <- t1$run_bold
  expect_true(any(run_text == "Purpose:"))
  expect_true(all(is_bold[run_text %in% c("Purpose:", "Missing values:")]))
  expect_false(any(is_bold[startsWith(run_text, "Every value in column")]))
  expect_true(any(startsWith(run_text, "\u2022 ")))
  fonts <- t1$fonts
  expect_false(FONTS$monospace %in% fonts)
})

test_that("the reading guide for rules appears once, before the first rules table", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  x <- tg_rules_template()
  dir <- tg_scratch()
  writeLines(yaml::as.yaml(x$tpl), tg_path(dir, "templates", "rules_tpl.dta-template.yaml"))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, templates = "rules_tpl", quiet = TRUE)
  lines <- strsplit(tg_text(got[[1]]), "\n", fixed = TRUE)[[1]]
  # One sentence of the reading guide, as written in the specification.
  first <- "Each rule is checked separately for every file of the dataset."
  hits <- which(grepl(first, lines, fixed = TRUE))
  expect_length(hits, 1L)
  header <- which(lines == "Rule ID")
  expect_length(header, 2L)
  expect_lt(hits, header[[1]])
  # Every paragraph of the guide is there once.
  for (s in .rule_reading_guide()) expect_equal(sum(grepl(s, lines, fixed = TRUE)), 1L)
})
