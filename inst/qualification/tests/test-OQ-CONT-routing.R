# How load_delivery() chooses the file entry for a file (tiers, misfits,
# ambiguity, route, datasets, duplicates), what a failure leaves behind, and how
# the specification and its exports describe a container. REQ-CONT-002,
# REQ-CONT-006, REQ-CONT-009, REQ-CONT-012, REQ-CONT-014, REQ-CONT-015 and
# REQ-CONT-020 to REQ-CONT-023.
#
# Every expected value is written down from the specification, from the
# literal data of the fixtures (helper-container.R, built by
# .github/scripts/build_container_fixtures.R) or from the bundled example
# specification, never captured from a run:
#
#   sdtm_csv.zip   ae.csv, dm.csv, define.xml, docs/readme.txt and two system files
#   encrypted.zip  ae.csv, flagged as encrypted
#   nested.zip     inner.zip
#   lib_in_zip.zip sdtm.xpt (a library of AE, DM and VS) and define.xml
#
# Archives that must differ from a fixture (a name in capitals, two folders
# holding the same file name) are built at run time.
#
# LIM-016 (files are matched by base name) is pinned in OQ-CONT-016.

# ---- local helpers --------------------------------------------------------------

# The reason a file gets when an entry takes it although it arrived loose and the
# entry requires it inside sdtm_csv.zip (a misfit: loaded, not conforming).
RT_MISFIT_LOOSE <- "loaded, but it arrived loose and the agreement requires it inside 'sdtm_csv.zip'"

# A dataset ae reading a SAS dataset (which the archive's text is not) and a
# dataset dm reading a CSV.
rt_sas_and_csv <- function() {
  cont_of(
    cont_tab("ae", list(DTAtools::DTAFileSAS7BDAT("ae.sas7bdat")), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("dm.csv")), cont_specs("dm"))
  )
}

# Lint rows of a one-dataset document whose files entry is `files`, with extra
# keys on the dataset itself.
rt_lint <- function(files, dataset = list()) {
  lint_dta(list(datasets = list(c(list(id = "ae", type = "tabular"), dataset, list(files = files)))))
}

# A one-dataset YAML document with a chosen `container:` line on its file entry.
rt_yaml <- function(container_line, env = parent.frame()) {
  p <- file.path(qa_tempdir(env), "doc.yaml")
  writeLines(
    c(
      "metadata:", "  title: T", "  version: '1'",
      "datasets:",
      "  - id: ae", "    type: tabular", "    files:",
      "      type: csv", "      filename: ae.csv",
      paste0("      ", container_line)
    ),
    p
  )
  p
}

# ---- containers -------------------------------------------------------------------

test_that("OQ-CONT-013 | a file that does not arrive inside its entry's container is still loaded, as a misfit, and a wildcard in the container is matched to the whole name | REQ-CONT-006", {
  declared <- cont_dta("csv", container = "sdtm_csv.zip")
  loose_ae <- cont_write("ae.csv", CONT_AE_CSV)

  # A loose file of the right name is taken by the entry that expects it inside
  # an archive, as nothing ranks higher: it is loaded, flagged as not conforming,
  # and the reason says where it arrived and what the agreement requires.
  l <- cont_run(declared, loose_ae, exdir = cont_exdir("OQ-CONT-013"))
  qa_check("a loose ae.csv is accepted", is.null(l$error), l$error)
  lrow <- cont_rows(l$plan, "loaded")
  qa_step(
    "it is loaded into ae, and the row is not conformant",
    list(rows = 1L, dataset = "ae", conformant = FALSE),
    list(rows = nrow(lrow), dataset = lrow$dataset, conformant = lrow$conformant)
  )
  qa_step("its reason names how it arrived and what is required", RT_MISFIT_LOOSE, lrow$reason)
  qa_step("its content is loaded all the same", c("HEADACHE", "NAUSEA"), cont_table(l$result, "ae", "ae")$AETERM)
  qa_step("and the packaging is reported as a finding", "outside", DTAtools::container_findings(l$result)$status)
  z <- cont_run(declared, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  zl <- cont_rows(z$plan, "loaded")
  qa_step(
    "the same file from inside the named archive is loaded and conforms",
    list(dataset = c("ae", "dm"), conformant = c(TRUE, TRUE)),
    list(dataset = sort(zl$dataset), conformant = zl$conformant[order(zl$dataset)])
  )

  # Both in one call, in either order: the copy that fits is kept, and the loose
  # copy is unused and names the copy that was kept.
  for (files in list(c(loose_ae, cont_fx("sdtm_csv.zip")), c(cont_fx("sdtm_csv.zip"), loose_ae))) {
    r <- cont_run(declared, files, exdir = cont_exdir("OQ-CONT-013"))
    loaded <- cont_rows(r$plan, "loaded")
    qa_step("one call: ae and dm are loaded", c("ae", "dm"), sort(loaded$dataset))
    qa_step("both recording the archive as their container", rep("sdtm_csv.zip", 2), loaded$container)
    loose_row <- r$plan[is.na(r$plan$container) & r$plan$name == "ae.csv", ]
    qa_step(
      "one call: the loose ae.csv is unused, naming the copy that was loaded",
      list(rows = 1L, status = "unused", reason = "another copy was loaded from sdtm_csv.zip > ae.csv"),
      list(rows = nrow(loose_row), status = loose_row$status, reason = loose_row$reason)
    )
  }

  # An entry without a container accepts the file loose or inside any container,
  # and no row is judged.
  plain <- cont_run(cont_dta("csv"), c(loose_ae, cont_write("dm.csv", CONT_DM_CSV)), exdir = cont_exdir("OQ-CONT-013"))
  qa_step(
    "an entry without a container takes loose files, with no container and no verdict on the row",
    list(dataset = c("ae", "dm"), container = c(NA_character_, NA_character_), conformant = c(NA, NA)),
    list(
      dataset = sort(cont_rows(plain$plan, "loaded")$dataset), container = cont_rows(plain$plan, "loaded")$container,
      conformant = cont_rows(plain$plan, "loaded")$conformant
    )
  )
  inside <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step("and the same files from inside an archive", c("ae", "dm"), sort(cont_rows(inside$plan, "loaded")$dataset))

  # A member of another container is taken as a misfit that says which container
  # it came in; a name differing only in case is explained.
  o <- cont_run(cont_dta("csv", container = "other.zip"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  ol <- cont_rows(o$plan, "loaded")
  qa_step(
    "a member of another archive is loaded, not conforming",
    list(dataset = c("ae", "dm"), conformant = c(FALSE, FALSE)),
    list(dataset = sort(ol$dataset), conformant = ol$conformant)
  )
  qa_step(
    "and the reason names the archive it came in and the one required",
    rep("loaded, but it arrived inside 'sdtm_csv.zip' and the agreement requires it inside 'other.zip'", 2),
    ol$reason
  )
  qa_check("with no mention of letter case", !any(grepl("letter case", ol$reason, fixed = TRUE)), ol$reason)
  shouting <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "SDTM_CSV.ZIP")
  s <- cont_run(cont_dta("csv", container = "sdtm_csv.zip"), shouting, exdir = cont_exdir("OQ-CONT-013"))
  sl <- cont_rows(s$plan, "loaded")
  qa_step(
    "an archive named in capitals does not fit a lower-case declaration, and is loaded as a misfit",
    list(rows = 1L, conformant = FALSE),
    list(rows = nrow(sl), conformant = sl$conformant)
  )
  qa_step("whose reason says letter case must match", TRUE, length(sl$reason) == 1L && endsWith(sl$reason, "(letter case must match)"))

  # * matches any text and ? one character, and the whole name must match.
  w <- cont_run(cont_dta("csv", container = "sdtm_*.zip"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step(
    "sdtm_*.zip matches sdtm_csv.zip: both members conform",
    list(dataset = c("ae", "dm"), conformant = c(TRUE, TRUE)),
    list(dataset = sort(cont_rows(w$plan, "loaded")$dataset), conformant = cont_rows(w$plan, "loaded")$conformant)
  )
  q <- cont_run(cont_dta("csv", container = "sdtm_????.zip"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step(
    "sdtm_????.zip, whose four characters cannot be 'csv', does not: both members are misfits",
    list(dataset = c("ae", "dm"), conformant = c(FALSE, FALSE)),
    list(dataset = sort(cont_rows(q$plan, "loaded")$dataset), conformant = cont_rows(q$plan, "loaded")$conformant)
  )

  # An outer container declaration reaches a library's members: the container
  # chain is matched, not only the innermost archive.
  n <- cont_run(cont_dta("xpt", container = "lib_in_zip.zip"), cont_fx("lib_in_zip.zip"), exdir = cont_exdir("OQ-CONT-013"))
  nl <- cont_rows(n$plan, "loaded")
  qa_step(
    "a declaration of the outer archive reaches the members of the library inside it",
    list(name = c("ae.xpt", "dm.xpt", "vs.xpt"), conformant = c(TRUE, TRUE, TRUE)),
    list(name = sort(nl$name), conformant = nl$conformant[order(nl$name)])
  )
})

test_that("OQ-CONT-014 | a literal entry wins over pattern entries, the first pattern entry over later ones, and a pattern judges a gzip file without its .gz | REQ-CONT-002", {
  # clinical_data matches clinical_data.csv by a pattern, raw_export names
  # clinical_data2.csv.gz literally (inst/extdata/clinical_dta_with_file_dataset.yaml).
  dta <- suppressWarnings(read_dta_from_yaml(
    system.file("extdata", "clinical_dta_with_file_dataset.yaml", package = "DTAtools")
  ))
  files <- system.file("extdata", c("clinical_data.csv", "clinical_data2.csv.gz"), package = "DTAtools")
  r <- cont_run(dta, files, exdir = cont_exdir("OQ-CONT-014"))
  qa_check("the delivery is accepted", is.null(r$error), r$error)
  qa_step("the pattern entry's dataset receives clinical_data.csv", "clinical_data", names(tables(datasets(r$result, "clinical_data"))))
  qa_step(
    "the literal entry of another dataset takes clinical_data2.csv.gz, which the pattern would also match",
    "clinical_data2.csv.gz",
    basename(datasets(r$result, "raw_export")@file_paths)
  )

  # Within a dataset: a literal entry before a pattern entry, and the first of
  # two pattern entries.
  lit <- cont_of(cont_tab("ae", list(
    DTAtools::DTAFileCSV("^a.*\\.csv$", pattern = TRUE), DTAtools::DTAFileCSV("ae.csv")
  )))
  a <- cont_run(lit, cont_write("ae.csv", CONT_AE_CSV), exdir = cont_exdir("OQ-CONT-014"))
  qa_step("the literal entry (the second) wins over a pattern entry of the same dataset", 2L, cont_rows(a$plan, "loaded")$handler)
  pat <- cont_of(cont_tab("ae", list(
    DTAtools::DTAFileCSV("^a.*\\.csv$", pattern = TRUE), DTAtools::DTAFileCSV("^ae.*\\.csv$", pattern = TRUE)
  )))
  b <- cont_run(pat, cont_write("ae.csv", CONT_AE_CSV), exdir = cont_exdir("OQ-CONT-014"))
  qa_step("the first of two matching pattern entries wins", 1L, cont_rows(b$plan, "loaded")$handler)

  # A pattern entry judges a gzip file without its compression suffix, loose
  # or inside an archive.
  gz_entry <- cont_of(cont_tab("ae", list(DTAtools::DTAFileCSV("^ae_.*\\.csv$", pattern = TRUE)), cont_empty_specs()))
  g <- cont_gz(CONT_AE_CSV, "ae_2025.csv.gz")
  loose <- cont_run(gz_entry, g, exdir = cont_exdir("OQ-CONT-014"))
  qa_step(
    "a loose ae_2025.csv.gz is taken by the pattern ^ae_.*\\.csv$",
    list(rows = 1L, name = "ae_2025.csv.gz", item = "ae_2025"),
    list(rows = nrow(cont_rows(loose$plan, "loaded")), name = cont_rows(loose$plan, "loaded")$name, item = cont_rows(loose$plan, "loaded")$item)
  )
  zipped <- cont_run(gz_entry, cont_zip(list(ae_2025.csv.gz = readBin(g, "raw", file.size(g)))), exdir = cont_exdir("OQ-CONT-014"))
  qa_step(
    "so is the same file inside an archive, which is then its container",
    list(rows = 1L, name = "ae_2025.csv.gz", item = "ae_2025", container = "test.zip"),
    list(
      rows = nrow(cont_rows(zipped$plan, "loaded")), name = cont_rows(zipped$plan, "loaded")$name,
      item = cont_rows(zipped$plan, "loaded")$item, container = cont_rows(zipped$plan, "loaded")$container
    )
  )
})

test_that("OQ-CONT-015 | abort leaves no trace, skip loads the rest, nothing loaded is an error | REQ-CONT-012", {
  # A member that is not a SAS dataset, and a good CSV beside it.
  z <- cont_zip(list(ae.sas7bdat = "not a SAS dataset\n", dm.csv = CONT_DM_CSV))
  dta0 <- rt_sas_and_csv()

  ex <- cont_exdir("OQ-CONT-015")
  a <- cont_run(dta0, z, exdir = ex)
  qa_step("abort mode (the default): the failure aborts as a load error", TRUE, cont_is(a$error, "dta_delivery_load_error"))
  qa_step(
    "naming the dataset and carrying the reader's own refusal",
    list(dataset = "ae", cause = TRUE),
    list(dataset = a$error$dataset, cause = cont_is(a$error$parent, "dta_sas7bdat_read_error"))
  )
  qa_step("no table is bound", 0L, length(tables(datasets(dta0, "dm"))))
  qa_step("and no extracted file is left behind", character(0), list.files(ex, recursive = TRUE))

  s <- cont_run(dta0, z, on_error = "skip", exdir = cont_exdir("OQ-CONT-015"))
  qa_check("skip mode: the delivery is accepted", is.null(s$error), s$error)
  qa_step("one warning of class dta_delivery_refused is signalled", 1L, s$n_warnings)
  qa_step("every other file is bound", c(34, 51), cont_table(s$result, "dm", "dm")$AGE, tolerance = 0)
  row <- s$plan[s$plan$name == "ae.sas7bdat", ]
  qa_step("the failure is listed as refused", "refused", row$status)
  paths <- c(tempdir(), normalizePath(tempdir(), winslash = "/"), normalizePath(tempdir(), winslash = "\\"))
  qa_step(
    "and its reason names no server path",
    rep(FALSE, 3),
    vapply(paths, function(p) grepl(p, row$reason, fixed = TRUE), logical(1), USE.NAMES = FALSE)
  )

  # Nothing that can be bound: refused in both modes.
  unrelated <- cont_write("unrelated.txt", "x")
  for (mode in c("abort", "skip")) {
    n <- cont_run(cont_dta("csv"), unrelated, on_error = mode, exdir = cont_exdir("OQ-CONT-015"))
    qa_step(
      sprintf("%s mode: a delivery from which nothing can be bound is refused", mode),
      list(refused = TRUE, reason = "matches no file entry of the specification"),
      list(refused = cont_is(n$error, "dta_delivery_nothing_loaded"), reason = n$plan$reason)
    )
  }

  # A container that fails to open: when an entry also binds it as a file the
  # file loads and the container is `opened` with the reason; nothing is
  # refused, and the files its entries declared are not reported missing.
  docs_ae <- cont_of(
    cont_filedata("docs", DTAtools::DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv")))
  )
  e <- cont_run(docs_ae, cont_fx("encrypted.zip"), on_error = "skip", exdir = cont_exdir("OQ-CONT-015"))
  qa_step("an encrypted archive that an entry binds as a file raises no warning", 0L, e$n_warnings)
  qa_step("it is loaded as a file and listed as opened", c("loaded", "opened"), sort(e$plan$status))
  qa_check(
    "the opened row says it could not be opened",
    startsWith(e$plan$reason[e$plan$status == "opened"], "could not be opened: ")
  )
  declared <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv", container = "encrypted.zip"))),
    cont_tab("dm", list(DTAtools::DTAFileCSV("dm.csv")))
  )
  files <- c(cont_fx("encrypted.zip"), cont_zip(list(dm.csv = CONT_DM_CSV), name = "other.zip"))
  m <- cont_run(declared, files, on_error = "skip", exdir = cont_exdir("OQ-CONT-015"))
  qa_step("a refused container leaves no declared file 'not in the delivery'", 0L, length(m$summary$missing))
  qa_step("and its refusal is listed", "refused", m$plan$status[m$plan$name == "encrypted.zip"])
})

test_that("OQ-CONT-016 | two files of one delivery that would be the same table are refused | REQ-CONT-009", {
  # LIM-016: folders are not part of matching, so ae.csv of two folders are two
  # files for the one entry that names ae.csv.
  folders <- cont_zip(list("a/ae.csv" = CONT_AE_CSV, "b/ae.csv" = CONT_AE_CSV))
  a <- cont_run(cont_dta("csv"), folders, exdir = cont_exdir("OQ-CONT-016"))
  qa_step(
    "LIM-016: the same file name in two folders is refused in abort mode",
    list(class = TRUE, dataset = "ae", item = "ae"),
    list(class = cont_is(a$error, "dta_delivery_duplicate_name"), dataset = a$error$dataset, item = a$error$item)
  )
  loose <- cont_write("ae.csv", CONT_AE_CSV)
  b <- cont_run(cont_dta("csv"), c(loose, cont_fx("sdtm_csv.zip")), exdir = cont_exdir("OQ-CONT-016"))
  qa_step("a loose ae.csv and the one inside an archive clash the same way", TRUE, cont_is(b$error, "dta_delivery_duplicate_name"))

  files <- c(folders, cont_write("dm.csv", CONT_DM_CSV))
  s <- cont_run(cont_dta("csv"), files, on_error = "skip", exdir = cont_exdir("OQ-CONT-016"))
  qa_check("skip mode: the delivery is accepted", is.null(s$error), s$error)
  qa_step("one warning of class dta_delivery_refused is signalled", 1L, s$n_warnings)
  qa_step("the rest is loaded: dm has its table", 1L, cont_ntab(s$result, "dm"))
  qa_step("and ae has none", 0L, cont_ntab(s$result, "ae"))
  refused <- cont_rows(s$plan, "refused")
  qa_step(
    "LIM-016: both clashing files are refused, naming the item and the dataset",
    list(
      names = c("ae.csv", "ae.csv"),
      reason = rep("is one of several files of the delivery that would be loaded as 'ae' into dataset 'ae'", 2)
    ),
    list(names = refused$name, reason = refused$reason)
  )

  # Equal names across the paths of one delivery are one file when they are the
  # same path: the same archive given twice is not a clash.
  twice <- cont_run(cont_dta("csv"), c(cont_fx("sdtm_csv.zip"), cont_fx("sdtm_csv.zip")), exdir = cont_exdir("OQ-CONT-016"))
  qa_step("the same archive given twice loads once", list(error = FALSE, loaded = c("ae", "dm")), list(
    error = !is.null(twice$error),
    loaded = sort(cont_rows(twice$plan, "loaded")$dataset)
  ))
})

# ---- the specification --------------------------------------------------------------

test_that("OQ-CONT-017 | lint and YAML reading refuse an invalid or misplaced container and an unknown file key | REQ-CONT-015", {
  bad <- rt_lint(list(type = "csv", filename = "ae.csv", container = "sdtm.tar"))
  qa_step(
    "a container that is not a .zip or .xpt file name is one error finding naming the entry",
    list(
      rows = 1L, code = "file_container_invalid", severity = "error", dataset = "ae",
      object = "file", id = "ae.csv",
      message = "The container 'sdtm.tar' must name a .zip archive or a .xpt SAS transport file."
    ),
    list(
      rows = nrow(bad), code = bad$code, severity = bad$severity, dataset = bad$dataset,
      object = bad$object, id = bad$id, message = bad$message
    )
  )
  ok <- rt_lint(list(
    list(type = "csv", filename = "ae.csv", container = "sdtm_*.zip"),
    list(type = "csv", filename = "dm.csv")
  ))
  qa_step("a wildcard container and an entry without one produce no file finding", FALSE, any(startsWith(ok$code, "file_")))

  typo <- rt_lint(list(type = "csv", filename = "ae.csv", contianer = "sdtm.zip"))
  qa_step(
    "a misspelt file key is one error finding that names the key",
    list(rows = 1L, code = "file_field_unknown", severity = "error", message = "Unknown file field(s): 'contianer'."),
    list(rows = nrow(typo), code = typo$code, severity = typo$severity, message = typo$message)
  )
  misplaced <- rt_lint(list(type = "csv", filename = "ae.csv"), dataset = list(container = "sdtm.zip"))
  qa_step(
    "a container written on the dataset is one error finding that points to the file entry",
    list(
      rows = 1L, code = "file_container_misplaced", severity = "error",
      message = "'container' belongs to a file entry: write it under 'files:'."
    ),
    list(rows = nrow(misplaced), code = misplaced$code, severity = misplaced$severity, message = misplaced$message)
  )

  # read_dta_from_yaml() refuses the same documents.
  p <- rt_yaml("container: deliveries/sdtm.zip")
  e <- cont_catch(read_dta_from_yaml(p))
  qa_step("a container with a folder in it is refused when the YAML is read", TRUE, cont_is(e, "dta_lint_error"))
  b <- cont_catch(read_dta_from_yaml(rt_yaml("container: \"{1+1}.tar\"")))
  qa_step(
    "a refused value is shown as written, with its braces",
    list(class = TRUE, braces = TRUE),
    list(class = cont_is(b, "dta_lint_error"), braces = !is.null(b) && grepl("{1+1}", conditionMessage(b), fixed = TRUE))
  )
  good <- read_dta_from_yaml(rt_yaml("container: sdtm.zip"))
  qa_step(
    "a valid container is carried into the file entry",
    "sdtm.zip",
    datasets(good, "ae")@files[[1]]@container
  )
})

test_that("OQ-CONT-018 | the exported Files table shows Delivered In only when declared | REQ-CONT-014", {
  fixed_cells <- function(line) trimws(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", line)), "|", fixed = TRUE)[[1]])
  export <- function(files) {
    ds <- DTAtools::DTADataSetTabular(id = "ae", specs = cont_specs("ae"), files = files)
    out <- file.path(qa_tempdir(), "ae.md")
    write_dataset_metadata(ds, out, format = "md", quiet = TRUE)
    readLines(out, warn = FALSE, encoding = "UTF-8")
  }
  header_of <- function(lines) lines[startsWith(lines, "| File Name / Pattern")]

  declared <- export(list(
    DTAtools::DTAFileCSV("ae.csv", container = "sdtm.zip"),
    DTAtools::DTAFileCSV("dm.csv")
  ))
  head <- header_of(declared)
  qa_step("with a declared container the Files table has one header line", 1L, length(head))
  qa_check("which ends in the column Delivered In", endsWith(head, "| Delivered In |"), head)
  cells <- fixed_cells(head)
  rows <- declared[startsWith(declared, "| ae.csv") | startsWith(declared, "| dm.csv")]
  qa_step(
    "the entry that declares a container shows it under that column",
    "sdtm.zip",
    fixed_cells(rows[startsWith(rows, "| ae.csv")][[1]])[length(cells)]
  )
  qa_step(
    "and an entry without one leaves the cell empty",
    "",
    fixed_cells(rows[startsWith(rows, "| dm.csv")][[1]])[length(cells)]
  )

  plain <- export(list(DTAtools::DTAFileCSV("ae.csv")))
  head_plain <- header_of(plain)
  qa_step("without any container the Files table has one header line", 1L, length(head_plain))
  qa_step("which has no Delivered In column", FALSE, grepl("Delivered In", head_plain, fixed = TRUE))
  qa_step(
    "and is the four-column table (no entry has a description) the export always had",
    "| File Name / Pattern | Match Type | Format | Expected Count |",
    head_plain
  )

  # Markdown leaves out a column that is empty throughout, so the Word export
  # is where the five-column table is told from the six-column one.
  word_files <- function(files) {
    ds <- DTAtools::DTADataSetTabular(id = "ae", specs = cont_specs("ae"), files = files)
    out <- file.path(qa_tempdir(), "ae.docx")
    write_dataset_metadata(ds, out, quiet = TRUE)
    tabs <- Filter(function(t) nrow(t) > 0 && identical(t$col1[[1]], "File Name / Pattern"), qa_docx_tables(out))
    if (length(tabs) == 0) NULL else tabs[[1]]
  }
  w_declared <- word_files(list(DTAtools::DTAFileCSV("ae.csv", container = "sdtm.zip"), DTAtools::DTAFileCSV("dm.csv")))
  qa_step(
    "Word: with a declared container the Files table has six columns, the last headed Delivered In",
    list(columns = 6L, last = "Delivered In"),
    list(columns = ncol(w_declared), last = unname(unlist(w_declared[1, ncol(w_declared)])))
  )
  qa_step("Word: and shows the container in the row of the entry that declares it", "sdtm.zip", unname(unlist(w_declared[2, 6])))
  w_plain <- word_files(list(DTAtools::DTAFileCSV("ae.csv"), DTAtools::DTAFileCSV("dm.csv")))
  qa_step(
    "Word: without any container the Files table keeps its five columns",
    list(columns = 5L, delivered_in = FALSE),
    list(columns = ncol(w_plain), delivered_in = "Delivered In" %in% unlist(w_plain[1, ]))
  )
})

# ---- a literal entry outranks pattern entries, container or not -----------------

test_that("OQ-CONT-028 | a loose file that a literal entry names, but expects inside its container, goes to that entry as a misfit and not to a pattern entry that needs no container | REQ-CONT-002 REQ-CONT-006", {
  # Dataset ae names ae.csv literally but only inside sdtm.zip; dataset listings
  # takes every .csv by pattern. A literal entry whose container the file missed
  # (tier 3) outranks a pattern entry that needs no container (tier 4), so the
  # loose ae.csv goes to ae, not conforming; lb.csv, named by no literal entry,
  # goes to listings.
  dta <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv", container = "sdtm.zip")), cont_empty_specs()),
    cont_tab("listings", list(DTAtools::DTAFileCSV("[.]csv$", pattern = TRUE)), cont_empty_specs())
  )
  loose <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("lb.csv", "A,B\n1,2\n"))
  r <- cont_run(dta, loose, exdir = cont_exdir("OQ-CONT-028"))
  qa_check("the delivery is accepted", is.null(r$error), r$error)
  loaded <- cont_rows(r$plan, "loaded")
  loaded <- loaded[order(loaded$name), ]
  qa_step(
    "ae.csv is loaded into ae, not conforming, and lb.csv into listings, with no verdict",
    list(name = c("ae.csv", "lb.csv"), dataset = c("ae", "listings"), conformant = c(FALSE, NA)),
    list(name = loaded$name, dataset = loaded$dataset, conformant = loaded$conformant)
  )
  qa_step(
    "the reason of ae.csv says it arrived loose and sdtm.zip is required",
    "loaded, but it arrived loose and the agreement requires it inside 'sdtm.zip'",
    loaded$reason[loaded$name == "ae.csv"]
  )
  qa_step("dataset listings holds the one table lb", "lb", names(DTAtools::tables(DTAtools::datasets(r$result, "listings"))))
  qa_step("dataset ae holds the table ae", "ae", names(DTAtools::tables(DTAtools::datasets(r$result, "ae"))))
  qa_step("no file is unused", 0L, nrow(cont_rows(r$plan, "unused")))

  # Control: the same name inside the archive it expects is taken by ae, and the
  # pattern entry still takes the member of another name.
  z <- cont_run(dta, cont_zip(list(ae.csv = CONT_AE_CSV, lb.csv = "A,B\n1,2\n"), name = "sdtm.zip"), exdir = cont_exdir("OQ-CONT-028"))
  zl <- cont_rows(z$plan, "loaded")
  qa_step(
    "inside sdtm.zip, ae.csv goes to ae and lb.csv to listings",
    list(ae = "ae.csv", listings = "lb.csv"),
    list(ae = zl$name[zl$dataset == "ae"], listings = zl$name[zl$dataset == "listings"])
  )

  # A pattern entry that declares the container the file did come from keeps
  # it: sdtm_docs names define.xml literally inside sdtm.zip, adam_docs takes
  # every .xml inside adam.zip, and each archive holds a define.xml.
  docs <- cont_of(
    cont_filedata("sdtm_docs", DTAtools::DTAFileAny("define.xml", container = "sdtm.zip")),
    cont_filedata("adam_docs", DTAtools::DTAFileAny("[.]xml$", pattern = TRUE, container = "adam.zip"))
  )
  both <- c(
    cont_zip(list(define.xml = "<sdtm/>"), name = "sdtm.zip"),
    cont_zip(list(define.xml = "<adam/>"), name = "adam.zip")
  )
  d <- cont_run(docs, both, exdir = cont_exdir("OQ-CONT-028"))
  dl <- cont_rows(d$plan, "loaded")
  qa_step(
    "each define.xml goes to the dataset whose entry names its archive",
    c("adam_docs", "sdtm_docs"),
    sort(dl$dataset[dl$name == "define.xml"], method = "radix")
  )
  qa_step("no file is left unused", 0L, nrow(cont_rows(d$plan, "unused")))
})

# ---- a refused file was delivered ------------------------------------------------

test_that("OQ-CONT-029 | a file that was delivered but refused is not reported missing and keeps its dataset and entry | REQ-CONT-006 REQ-CONT-012", {
  # Three entries each expect their file inside sdtm_csv.zip. The archive holds
  # ae.csv (53 bytes) and dm.csv (30 bytes) and no vs.csv. With a member limit of
  # 40 bytes ae.csv is refused and dm.csv loads.
  entries <- function(id) cont_tab(id, list(DTAtools::DTAFileCSV(paste0(id, ".csv"), container = "sdtm_csv.zip")), cont_specs(id))
  dta <- cont_of(entries("ae"), entries("dm"), entries("vs"))
  r <- withr::with_options(
    list(DTAtools.container_max_member_bytes = 40),
    cont_run(dta, cont_fx("sdtm_csv.zip"), on_error = "skip", exdir = cont_exdir("OQ-CONT-029"))
  )
  qa_check("skip mode: the delivery is accepted", is.null(r$error), r$error)
  ae <- r$plan[r$plan$name == "ae.csv", ]
  qa_step("ae.csv is listed as refused", "refused", ae$status)
  qa_step("with the dataset and the entry (the first) it was meant for", list(dataset = "ae", handler = 1L), list(dataset = ae$dataset, handler = ae$handler))
  qa_step("dm.csv is loaded", "dm", cont_rows(r$plan, "loaded")$dataset)
  qa_step(
    "only vs.csv, which the archive does not hold, is reported as not in the delivery",
    "vs.csv", r$summary$missing
  )
})

# ---- the five tiers, ambiguity, route, datasets and duplicates ----------------------

# A tabular dataset of no columns with the given file entries.
rt_ds <- function(id, ...) cont_tab(id, list(...), cont_empty_specs())

# "<file> -> <dataset>" for every loaded row of a plan, in a fixed order.
rt_who <- function(plan) {
  l <- cont_rows(plan, "loaded")
  l <- l[order(l$name, l$dataset, method = "radix"), ]
  paste(l$name, l$dataset, sep = " -> ")
}

# ae takes `^a.*[.]csv$`, dm takes `^.*e[.]csv$`: ae.csv is taken by both,
# de.csv by dm only.
rt_tie <- function() {
  cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("^a.*[.]csv$", pattern = TRUE)), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("^.*e[.]csv$", pattern = TRUE)), cont_empty_specs())
  )
}
rt_de_csv <- function(env = parent.frame()) cont_write("de.csv", CONT_AE_CSV, env = env)

test_that("OQ-CONT-033 | entries are ranked in five tiers, and only the datasets of the best tier take the file | REQ-CONT-002 REQ-CONT-020", {
  csv <- DTAtools::DTAFileCSV
  pat <- function(p, ...) csv(p, pattern = TRUE, ...)
  ae_loose <- cont_write("ae.csv", CONT_AE_CSV)

  # Tier 1 (a literal entry that needs no container) beats tier 2 (a pattern
  # entry the file fits), whatever the dataset order: dm's pattern also matches
  # ae.csv, but ae names it.
  t12 <- cont_run(
    cont_of(rt_ds("ae", csv("ae.csv")), rt_ds("dm", pat("[.]csv$", container = "sdtm_csv.zip"))),
    cont_fx("sdtm_csv.zip"),
    exdir = cont_exdir("OQ-CONT-033")
  )
  qa_step("tier 1 beats tier 2: ae.csv goes to ae, dm.csv to dm", c("ae.csv -> ae", "dm.csv -> dm"), rt_who(t12$plan))
  qa_step("and nothing is ambiguous", 0L, nrow(cont_rows(t12$plan, "ambiguous")))

  # Tier 2 (a pattern entry the file fits) beats tier 4 (a pattern entry that
  # needs no container): not a tie.
  t24 <- cont_run(
    cont_of(
      rt_ds("ae", pat("^ae.*[.]csv$", container = "sdtm_csv.zip")),
      rt_ds("dm", pat("^[a-d].*[.]csv$"))
    ),
    cont_fx("sdtm_csv.zip"),
    exdir = cont_exdir("OQ-CONT-033")
  )
  qa_step("tier 2 beats tier 4: ae.csv goes to ae alone, dm.csv to dm", c("ae.csv -> ae", "dm.csv -> dm"), rt_who(t24$plan))
  qa_step("and nothing is ambiguous", 0L, nrow(cont_rows(t24$plan, "ambiguous")))

  # Tier 1 beats tier 3 (a literal entry whose container the file missed) in
  # another dataset: dm gets neither the file nor a row.
  t13 <- cont_run(
    cont_of(rt_ds("ae", csv("ae.csv")), rt_ds("dm", csv("ae.csv", container = "other.zip"))),
    ae_loose,
    exdir = cont_exdir("OQ-CONT-033")
  )
  qa_step(
    "tier 1 beats tier 3: the loose ae.csv is taken by ae alone, with one row in the plan",
    list(who = "ae.csv -> ae", rows = 1L, dm_tables = 0L),
    list(who = rt_who(t13$plan), rows = nrow(t13$plan), dm_tables = cont_ntab(t13$result, "dm"))
  )

  # Literal entries of one tier in two datasets both take the file: that is
  # deliberate duplicate naming, not ambiguity.
  t33 <- cont_run(
    cont_of(
      rt_ds("ae", csv("ae.csv", container = "other.zip")),
      rt_ds("dm", csv("ae.csv", container = "other.zip"))
    ),
    ae_loose,
    exdir = cont_exdir("OQ-CONT-033")
  )
  qa_step(
    "two literal entries of the same tier both take the file",
    list(who = c("ae.csv -> ae", "ae.csv -> dm"), ambiguous = 0L),
    list(who = rt_who(t33$plan), ambiguous = nrow(cont_rows(t33$plan, "ambiguous")))
  )

  # Tier 4 (a pattern entry that needs no container) beats tier 5 (a pattern
  # entry whose container the file missed).
  t45 <- cont_run(
    cont_of(
      rt_ds("ae", pat("^ae.*[.]csv$", container = "other.zip")),
      rt_ds("dm", pat("^[a-z]+[.]csv$"))
    ),
    ae_loose,
    exdir = cont_exdir("OQ-CONT-033")
  )
  qa_step(
    "tier 4 beats tier 5: the loose ae.csv goes to dm, and ae holds no table",
    list(who = "ae.csv -> dm", ae_tables = 0L),
    list(who = rt_who(t45$plan), ae_tables = cont_ntab(t45$result, "ae"))
  )

  # Within one dataset a fitting entry comes before a free one of the same tier,
  # wherever it stands in the list.
  for (fit_first in c(FALSE, TRUE)) {
    entries <- list(csv("ae.csv"), csv("ae.csv", container = "sdtm_csv.zip"))
    if (fit_first) entries <- rev(entries)
    w <- cont_run(cont_of(cont_tab("ae", entries, cont_empty_specs())), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-033"))
    wl <- cont_rows(w$plan, "loaded")
    qa_step(
      sprintf("within a dataset the entry whose container holds the file wins (it stands %s)", if (fit_first) "first" else "second"),
      list(handler = if (fit_first) 1L else 2L, conformant = TRUE),
      list(handler = wl$handler, conformant = wl$conformant)
    )
  }
})

test_that("OQ-CONT-034 | a file that several datasets take by pattern in one tier is ambiguous, is not loaded, and never aborts the rest | REQ-CONT-020", {
  # ae.csv alone: nothing else is loaded, so the delivery is refused with the
  # ambiguous row in the plan.
  a <- cont_run(rt_tie(), cont_write("ae.csv", CONT_AE_CSV), exdir = cont_exdir("OQ-CONT-034"))
  qa_step("nothing but an ambiguous file is a delivery with nothing loaded", TRUE, cont_is(a$error, "dta_delivery_nothing_loaded"))
  row <- a$plan
  qa_step(
    "its one row is ambiguous, lists both datasets, and has no path",
    list(rows = 1L, status = "ambiguous", candidates = c("ae", "dm"), path = NA_character_),
    list(rows = nrow(row), status = row$status, candidates = row$candidates[[1]], path = row$path)
  )
  qa_step(
    "and its reason says how to choose",
    "matches the file entries of datasets 'ae' and 'dm'; pass route = list(\"ae.csv\" = \"<dataset>\") to choose",
    row$reason
  )

  # Beside a file that loads, the ambiguous one is only reported.
  b <- cont_run(rt_tie(), c(cont_write("ae.csv", CONT_AE_CSV), rt_de_csv()), exdir = cont_exdir("OQ-CONT-034"))
  qa_check("the delivery is accepted", is.null(b$error), b$error)
  qa_step(
    "ae.csv is ambiguous and de.csv is loaded",
    list(ambiguous = "ae.csv", loaded = "de.csv -> dm"),
    list(ambiguous = cont_rows(b$plan, "ambiguous")$name, loaded = rt_who(b$plan))
  )
  qa_step("neither dataset holds the table of the ambiguous file", list(ae = 0L, dm = "de"), list(ae = cont_ntab(b$result, "ae"), dm = names(DTAtools::tables(DTAtools::datasets(b$result, "dm")))))
  qa_step(
    "a row that is not ambiguous has no candidates",
    character(0),
    b$plan$candidates[b$plan$name == "de.csv"][[1]]
  )
  qa_step(
    "the summary names the ambiguous file",
    list(field = "ae.csv", text = TRUE),
    list(field = b$summary$ambiguous, text = grepl("Matches several datasets, not loaded: ae.csv", conditionMessage(b$summary), fixed = TRUE))
  )

  # The same tie inside a container both entries name.
  both <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("^a.*[.]csv$", pattern = TRUE, container = "sdtm_csv.zip")), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("^.*e[.]csv$", pattern = TRUE, container = "sdtm_csv.zip")), cont_empty_specs())
  )
  c2 <- cont_run(both, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-034"))
  qa_step(
    "two entries that both fit the archive tie in tier 2 and are ambiguous too",
    list(status = "ambiguous", candidates = c("ae", "dm"), container = "sdtm_csv.zip"),
    list(
      status = cont_rows(c2$plan, "ambiguous")$status, candidates = cont_rows(c2$plan, "ambiguous")$candidates[[1]],
      container = cont_rows(c2$plan, "ambiguous")$container
    )
  )
})

test_that("OQ-CONT-035 | route chooses the datasets of an ambiguous file, and is refused when it names anything else | REQ-CONT-021", {
  files <- c(cont_write("ae.csv", CONT_AE_CSV), rt_de_csv())
  tables_of <- function(d, id) sort(as.character(names(DTAtools::tables(DTAtools::datasets(d, id)))))

  r <- cont_run(rt_tie(), files, route = list("ae.csv" = "dm"), exdir = cont_exdir("OQ-CONT-035"))
  qa_step(
    "a list route sends ae.csv to dm, which then holds ae and de; ae holds nothing",
    list(dm = c("ae", "de"), ae = character(0), ambiguous = 0L),
    list(dm = tables_of(r$result, "dm"), ae = tables_of(r$result, "ae"), ambiguous = nrow(cont_rows(r$plan, "ambiguous")))
  )
  v <- cont_run(rt_tie(), files, route = c("ae.csv" = "ae"), exdir = cont_exdir("OQ-CONT-035"))
  qa_step("a named character vector is the same thing", c("ae", "de"), c(tables_of(v$result, "ae"), tables_of(v$result, "dm")))

  # A route to a dataset the file is not ambiguous between refuses the whole
  # file: skip mode lists one refused row, abort mode aborts.
  three <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("^a.*[.]csv$", pattern = TRUE)), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("^.*e[.]csv$", pattern = TRUE)), cont_empty_specs()),
    cont_tab("vs", list(DTAtools::DTAFileCSV("^v.*[.]csv$", pattern = TRUE)), cont_empty_specs())
  )
  for (route in list(list("ae.csv" = "vs"), list("ae.csv" = c("dm", "vs")))) {
    s <- cont_run(three, files, route = route, on_error = "skip", exdir = cont_exdir("OQ-CONT-035"))
    row <- s$plan[s$plan$name == "ae.csv", ]
    qa_step(
      "skip mode: the whole file is refused in one row, naming the dataset it is not ambiguous between",
      list(rows = 1L, status = "refused", reason = TRUE),
      list(
        rows = nrow(row), status = row$status,
        reason = grepl("route sends it to dataset 'vs', which is not one of the datasets it is ambiguous between ('ae' and 'dm')", row$reason, fixed = TRUE)
      )
    )
    qa_step("and nothing of it is bound", 0L, cont_ntab(s$result, "ae") + sum("ae" %in% names(DTAtools::tables(DTAtools::datasets(s$result, "dm")))))
    ab <- cont_run(three, files, route = route, exdir = cont_exdir("OQ-CONT-035"))
    qa_step("abort mode: the call is refused with dta_delivery_bad_input", TRUE, cont_is(ab$error, "dta_delivery_bad_input"))
  }

  # A route to no dataset leaves the file unused.
  n <- cont_run(rt_tie(), files, route = list("ae.csv" = character(0)), exdir = cont_exdir("OQ-CONT-035"))
  nrow_ae <- n$plan[n$plan$name == "ae.csv", ]
  qa_step(
    "a route to no dataset lists the file as unused with that reason",
    list(status = "unused", reason = "route sends it to no dataset", ae_tables = 0L),
    list(status = nrow_ae$status, reason = nrow_ae$reason, ae_tables = cont_ntab(n$result, "ae"))
  )

  # A route for a file that was not ambiguous changes nothing and is listed.
  q <- cont_run(cont_dta("csv"), cont_write("ae.csv", CONT_AE_CSV), route = list("ae.csv" = "dm"), exdir = cont_exdir("OQ-CONT-035"))
  qa_step(
    "a route for a file that was not ambiguous leaves it in its own dataset",
    list(ae = "ae", dm = 0L),
    list(ae = tables_of(q$result, "ae"), dm = cont_ntab(q$result, "dm"))
  )
  qa_check(
    "and the summary lists the file the route named",
    grepl("route names files that were not ambiguous: ae.csv", conditionMessage(q$summary), fixed = TRUE),
    conditionMessage(q$summary)
  )

  # A malformed route is refused before anything is read or extracted.
  ex <- cont_exdir("OQ-CONT-035-bad")
  f <- cont_write("ae.csv", CONT_AE_CSV)
  bad <- list(
    unnamed = list("dm"),
    duplicated = list(a = "ae", a = "dm"),
    unknown_dataset = list("ae.csv" = "nope")
  )
  for (nm in names(bad)) {
    e <- cont_run(cont_dta("csv"), f, route = bad[[nm]], exdir = ex)
    qa_step(sprintf("a route with %s is refused as bad input", nm), TRUE, cont_is(e$error, "dta_delivery_bad_input"))
  }
  qa_step("and nothing was extracted", character(0), list.files(ex, recursive = TRUE))
})

test_that("OQ-CONT-036 | datasets limits what a call binds and extracts, and is refused when it names no dataset of the document | REQ-CONT-022", {
  ex <- cont_exdir("OQ-CONT-036")
  r <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), datasets = "ae", exdir = ex)
  qa_check("the delivery is accepted", is.null(r$error), r$error)
  qa_step(
    "only ae is bound",
    list(ae = 1L, dm = 0L),
    list(ae = cont_ntab(r$result, "ae"), dm = cont_ntab(r$result, "dm"))
  )
  dm_row <- r$plan[r$plan$name == "dm.csv", ]
  qa_step(
    "dm.csv is unused, for a dataset this call does not load",
    list(status = "unused", reason = "is for dataset 'dm', which this call does not load"),
    list(status = dm_row$status, reason = dm_row$reason)
  )
  qa_step("only the file for ae is extracted", "ae.csv", basename(list.files(ex, recursive = TRUE)))
  qa_step("and the datasets left out are not reported as missing from the delivery", character(0), r$summary$missing)

  # An ambiguous file that only one of the chosen datasets takes is bound to it.
  files <- c(cont_write("ae.csv", CONT_AE_CSV), rt_de_csv())
  one <- cont_run(rt_tie(), files, datasets = "ae", exdir = cont_exdir("OQ-CONT-036"))
  qa_step(
    "datasets = ae binds the ambiguous ae.csv to ae, and nothing is ambiguous",
    list(ae = "ae", ambiguous = 0L),
    list(ae = names(DTAtools::tables(DTAtools::datasets(one$result, "ae"))), ambiguous = nrow(cont_rows(one$plan, "ambiguous")))
  )

  # Bad input is refused before anything is read.
  for (bad in list("nope", character(0))) {
    e <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), datasets = bad, exdir = cont_exdir("OQ-CONT-036"))
    qa_step(
      sprintf("datasets = %s is refused as bad input", if (length(bad)) "an unknown id" else "an empty vector"),
      TRUE, cont_is(e$error, "dta_delivery_bad_input")
    )
  }
})

test_that("OQ-CONT-037 | of several copies of a file the one that fits is kept, and when none fits none is loaded | REQ-CONT-023 REQ-CONT-009", {
  # Every copy outside the container: nothing is loaded and nothing is refused.
  declared <- cont_dta("csv", container = "sdtm_csv.zip")
  loose <- cont_write("ae.csv", CONT_AE_CSV)
  other <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "other.zip")
  a <- cont_run(declared, c(loose, other), exdir = cont_exdir("OQ-CONT-037"))
  qa_check("the delivery is accepted, not refused", is.null(a$error), a$error)
  ae_rows <- a$plan[a$plan$name == "ae.csv", ]
  qa_step(
    "both copies of ae.csv are unused, with the reason that none was loaded",
    list(status = c("unused", "unused"), reason = rep("several copies arrived outside 'sdtm_csv.zip'; none was loaded", 2)),
    list(status = ae_rows$status, reason = ae_rows$reason)
  )
  qa_step("ae holds no table", 0L, cont_ntab(a$result, "ae"))
  qa_step("dm.csv, which arrived once, is loaded", "dm", cont_rows(a$plan, "loaded")$dataset)

  # The copy that fits is kept; when it fails to load, the best other copy is
  # loaded and no reason claims the failed copy was loaded.
  sas_file <- file.path(qa_suite_root(), "fx", "sas", "iris.sas7bdat")
  sas <- readBin(sas_file, "raw", file.size(sas_file))
  dta <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileSAS7BDAT("ae.sas7bdat", container = "sdtm.zip")), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("dm.csv")), cont_empty_specs())
  )
  good <- cont_write("ae.sas7bdat", sas)
  bad_zip <- cont_zip(list(ae.sas7bdat = "not a SAS dataset", dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  f <- cont_run(dta, c(good, bad_zip), on_error = "skip", exdir = cont_exdir("OQ-CONT-037"))
  qa_check("skip mode: the delivery is accepted", is.null(f$error), f$error)
  loose_row <- f$plan[f$plan$name == "ae.sas7bdat" & is.na(f$plan$container), ]
  zip_row <- f$plan[f$plan$name == "ae.sas7bdat" & !is.na(f$plan$container), ]
  qa_step(
    "the copy that fits fails and is refused; the loose copy is loaded instead, not conforming",
    list(zip = "refused", loose = "loaded", conformant = FALSE, tables = 1L),
    list(zip = zip_row$status, loose = loose_row$status, conformant = loose_row$conformant, tables = cont_ntab(f$result, "ae"))
  )
  qa_check("and no reason says another copy was loaded from the archive that failed", !any(grepl("another copy was loaded from sdtm.zip", f$plan$reason, fixed = TRUE)), f$plan$reason)

  ok_zip <- cont_zip(list(ae.sas7bdat = sas, dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  g <- cont_run(dta, c(good, ok_zip), on_error = "skip", exdir = cont_exdir("OQ-CONT-037"))
  unused <- g$plan[g$plan$name == "ae.sas7bdat" & g$plan$status == "unused", ]
  qa_step(
    "when the copy that fits loads, the other says where the loaded one came from",
    "another copy was loaded from sdtm.zip > ae.sas7bdat",
    unused$reason
  )
})

test_that("OQ-CONT-038 | a default container, the word none and a container id decide what an entry requires | REQ-CONT-006", {
  main <- DTAtools::DTAContainer("main", "delivery.zip", default = TRUE)
  adam <- DTAtools::DTAContainer("adam", "adam.zip")
  doc <- cont_doc(
    cont_csv_ds("ae", specs = cont_empty_specs()),
    cont_csv_ds("dm", "none", cont_empty_specs()),
    cont_csv_ds("vs", "adam", cont_empty_specs()),
    containers = list(main, adam)
  )
  loose <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV), cont_write("vs.csv", "A\n1\n"))
  l <- cont_run(doc, loose, exdir = cont_exdir("OQ-CONT-038"))
  ll <- cont_rows(l$plan, "loaded")
  ll <- ll[order(ll$dataset), ]
  qa_step(
    "loose: ae (the default container) and vs (by id) are not conforming, dm (none) is exempt",
    list(dataset = c("ae", "dm", "vs"), conformant = c(FALSE, NA, FALSE)),
    list(dataset = ll$dataset, conformant = ll$conformant)
  )
  qa_step(
    "the reason of ae names the default container's file, that of vs the file of the container its id names",
    c(
      "loaded, but it arrived loose and the agreement requires it inside 'delivery.zip'",
      "loaded, but it arrived loose and the agreement requires it inside 'adam.zip'"
    ),
    ll$reason[ll$dataset %in% c("ae", "vs")]
  )

  inside <- c(
    cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip"),
    cont_zip(list(vs.csv = "A\n1\n"), name = "adam.zip")
  )
  i <- cont_run(doc, inside, exdir = cont_exdir("OQ-CONT-038"))
  il <- cont_rows(i$plan, "loaded")
  il <- il[order(il$dataset), ]
  qa_step(
    "inside the right archives: ae and vs conform, dm is still exempt",
    list(dataset = c("ae", "dm", "vs"), conformant = c(TRUE, NA, TRUE)),
    list(dataset = il$dataset, conformant = il$conformant)
  )
})

test_that("OQ-CONT-039 | an entry refers to a container by a valid id, a dotted file name or none, and refuses anything else | REQ-CONT-015", {
  ok <- c("sdtm", "adam-2_x", "none", "sdtm_*.zip", "STUDY.xpt")
  made <- vapply(ok, function(ref) {
    h <- tryCatch(DTAtools::DTAFileCSV("ae.csv", container = ref), error = function(e) NULL)
    !is.null(h) && identical(h@container, ref)
  }, logical(1))
  qa_step("an id, a hyphenated id, none, a wildcard file name and an .xpt name are all kept as written", rep(TRUE, length(ok)), unname(made))

  bad <- c("1bad", "bad id!", "a b", "_x", "sdtm.tar", "dir/sdtm.zip")
  refused <- vapply(bad, function(ref) {
    e <- cont_catch(DTAtools::DTAFileCSV("ae.csv", container = ref))
    cont_is(e, "dta_container_invalid")
  }, logical(1))
  qa_step("a value that is neither an id, none nor a .zip/.xpt name is refused with dta_container_invalid", rep(TRUE, length(bad)), unname(refused))

  lint_bad <- rt_lint(list(type = "csv", filename = "ae.csv", container = "1bad"))
  qa_step(
    "lint reports an undotted value that is no id as one error finding on the entry",
    list(rows = 1L, code = "file_container_invalid", severity = "error", dataset = "ae", object = "file"),
    list(rows = nrow(lint_bad), code = lint_bad$code, severity = lint_bad$severity, dataset = lint_bad$dataset, object = lint_bad$object)
  )
  qa_check("and its message names the container id rule", grepl("container id", lint_bad$message, fixed = TRUE), lint_bad$message)
  e <- cont_catch(read_dta_from_yaml(rt_yaml("container: 1bad")))
  qa_step("read_dta_from_yaml() refuses such a document with dta_lint_error", TRUE, cont_is(e, "dta_lint_error"))
  for (ref in c("none", "sdtm_*.zip", "x.xpt")) {
    out <- rt_lint(list(type = "csv", filename = "ae.csv", container = ref))
    qa_step(sprintf("'%s' gives no file_container_invalid finding", ref), 0L, sum(out$code == "file_container_invalid"))
  }
})
