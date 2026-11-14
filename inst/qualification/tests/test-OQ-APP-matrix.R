# The application and a matrix dataset. REQ-APP-033 .. REQ-APP-040.
#
# A matrix dataset keeps its row-label columns under `row_labels:` and its
# settings under `matrix:`, and it carries a recorded header for every loaded
# table. Each of those has to survive the app's own paths: saving, restoring a
# session, applying edited raw YAML, unloading. Every expected value below is
# taken from a requirement sentence or read off the bundled fixture file by
# hand-written code, never from what the application returned.
#
# The fixtures are the bundled matrix_dta.yaml (two matrix datasets: genes by
# samples as a TSV, samples by miRNAs as a CSV), matrix_gene_counts.tsv and
# matrix_mature_counts.csv. The raw header of the TSV is its first line.

mxa_header_fields <- function() {
  strsplit(readLines(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"), n = 1L), "\t", fixed = TRUE)[[1]]
}

# A specification as lines of YAML, written to a temp file and read back.
mxa_read <- function(lines, env = parent.frame()) {
  path <- file.path(qa_tempdir(env), "spec.yaml")
  writeLines(lines, path)
  suppressMessages(read_dta_from_yaml(path))
}

# One matrix dataset whose corner has no field above the first row label.
mxa_null_corner_lines <- c(
  "datasets:",
  "  - id: gene_counts",
  "    type: matrix",
  "    files: { filename: \"gene_counts.tsv\", type: tsv }",
  "    row_labels:",
  "      - { id: GENE_ID, type: SAS Char, nullable: No }",
  "      - { id: GENE_NAME, type: SAS Char, nullable: Yes }",
  "    matrix:",
  "      header:",
  "        corner: [~, gene_name]",
  "        pattern: \"^S[0-9]{4}$\"",
  "      rows:",
  "        key: GENE_ID",
  "      cells:",
  "        type: SAS Num",
  "        nullable: No",
  "        range: { min: 0 }"
)

# The bundled delivery spec with the genes file bound, held loose (no browser).
mxa_bound_gene_counts <- function() {
  dta <- suppressMessages(read_dta_from_yaml(system.file("extdata", "matrix_dta.yaml", package = "DTAtools")))
  dta@datasets[["gene_counts"]] <- load_file(
    dta@datasets[["gene_counts"]],
    file = system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"),
    stream = "never"
  )
  dta
}

mxa_status <- function(rv, id) unname(as.character(rv$status[[id]]))

# Load the bundled spec and bind both fixtures, as a user would, then check.
mxa_load_and_check <- function(session, genes = qapp_file_input("matrix_gene_counts.tsv")) {
  session$setInputs(dta_client_id = strrep("m", 32))
  session$setInputs(dta_file = qapp_file_input("matrix_dta.yaml"))
  session$setInputs(up_1_1 = genes)
  session$setInputs(up_2_1 = qapp_file_input("matrix_mature_counts.csv"))
  session$setInputs(check_all = 1)
}

test_that("OQ-APP-042 | saving a matrix dataset writes row_labels and matrix, and a null-first corner survives save, reload and save | REQ-APP-033", {
  qapp_skip_unless_app_installed()
  dta <- mxa_read(mxa_null_corner_lines)
  save_text <- qapp_fn("dta_to_yaml_text")

  first <- save_text(dta)
  qa_check("the document is saved", isTRUE(first$ok))
  saved <- yaml::yaml.load(first$value)$datasets[[1]]
  qa_step(
    "the saved dataset carries row_labels, matrix and no columns key",
    c(row_labels = TRUE, matrix = TRUE, columns = FALSE),
    c(
      row_labels = !is.null(saved$row_labels), matrix = !is.null(saved$matrix),
      columns = !is.null(saved$columns)
    )
  )
  qa_step(
    "the row labels are written in the order they were declared",
    c("GENE_ID", "GENE_NAME"), vapply(saved$row_labels, function(x) x$id, "")
  )
  corner <- saved$matrix$header$corner
  qa_step("the corner still has two entries", 2L, length(corner))
  qa_check("the first corner entry is written as the YAML null, not dropped", is.null(corner[[1]]))
  qa_step("and the second entry is the declared text", "gene_name", corner[[2]])

  again <- mxa_read(strsplit(first$value, "\n", fixed = TRUE)[[1]])
  ds <- again@datasets[["gene_counts"]]
  qa_step(
    "reading the saved text back yields the same row labels",
    c("GENE_ID", "GENE_NAME"), names(ds@specs@columns)
  )
  back_corner <- as.list(ds@matrix)$header$corner
  qa_step(
    "and a corner whose first entry is null and whose second is gene_name",
    c(TRUE, "gene_name"), c(is.null(back_corner[[1]]), back_corner[[2]])
  )
  qa_step("saving the reloaded document gives the identical text", first$value, save_text(again)$value)
})

test_that("OQ-APP-043 | a restored session keeps the recorded header of every loaded matrix table | REQ-APP-034 | tags: white-box", {
  qapp_skip_unless_app_installed()
  dta <- mxa_bound_gene_counts()
  qa_step(
    "before the dump the table's recorded header is the first line of the file",
    mxa_header_fields(), dta@datasets[["gene_counts"]]@heads[["matrix_gene_counts"]]$fields
  )

  # What the app does: the dump is written with saveRDS and read back later.
  file <- file.path(qa_tempdir(), "session.rds")
  saveRDS(qapp_fn("dta_dump_session")(dta), file)
  restored <- qapp_fn("dta_restore_session")(readRDS(file))
  ds <- restored@datasets[["gene_counts"]]

  qa_step("the restored dataset holds its table again", "matrix_gene_counts", names(ds@tables))
  qa_step(
    "and the recorded header of that table is unchanged",
    mxa_header_fields(), ds@heads[["matrix_gene_counts"]]$fields
  )
  res <- check(restored, quiet = TRUE, persist = FALSE)
  qa_check(
    "so the restored table checks valid, with no 'load it again' finding",
    isTRUE(validation_status(res@datasets[["gene_counts"]])$ok) && nrow(messages(res@datasets[["gene_counts"]])) == 0L
  )

  # Control: without a recorded header the same table is not valid (REQ-MATRIX-044),
  # so the pass above does depend on the header having travelled.
  ds@heads <- list()
  restored@datasets[["gene_counts"]] <- ds
  res2 <- check(restored, quiet = TRUE, persist = FALSE)
  qa_check(
    "control: the same table with its header cleared is not valid",
    isFALSE(validation_status(res2@datasets[["gene_counts"]])$ok)
  )
})

test_that("OQ-APP-044 | applying raw YAML that changes the matrix block re-validates the dataset | REQ-APP-035", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    mxa_load_and_check(session)
    qa_step("the genes delivery passes with the specification as shipped", "pass", mxa_status(rv, "gene_counts"))
    qapp_unlock_editing(session)

    # The fixture holds cells equal to 0 and to 1.5, so a minimum of 5 is broken.
    original <- rv$yaml_text
    stricter <- sub("(range:\\s+min: )0", "\\15", original)
    qa_check("the minimum was raised in the text", !identical(original, stricter))
    session$setInputs(raw_yaml_editor = stricter)
    session$setInputs(apply_yaml = 1)
    qa_check("the edited text applies", isTRUE(rv$yaml_msg$ok))
    qa_step(
      "the dataset is judged against the new minimum without pressing check, and fails",
      "fail", mxa_status(rv, "gene_counts")
    )
    found <- messages(datasets(rv$dta, "gene_counts"), as_tibble = FALSE)
    qa_check("with findings of keyword minimum", "minimum" %in% as.character(found$keyword))
    qa_step("while the other matrix dataset keeps its verdict", "pass", mxa_status(rv, "mature_counts"))

    session$setInputs(raw_yaml_editor = original)
    session$setInputs(apply_yaml = 2)
    qa_step("restoring the original text restores the pass", "pass", mxa_status(rv, "gene_counts"))
  })
})

test_that("OQ-APP-045 | applying raw YAML that changes one dataset's type drops that dataset's tables and uploads and no other's | REQ-APP-036", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    mxa_load_and_check(session)
    qa_step(
      "both deliveries are recorded before the edit",
      c(TRUE, TRUE), c("gene_counts||1", "mature_counts||1") %in% names(rv$uploads)
    )
    qapp_unlock_editing(session)

    # Turn the second dataset into a tabular one: its key, its type, no matrix block.
    y <- rv$yaml_text
    i <- regexpr("- id: mature_counts", y, fixed = TRUE)
    tail_txt <- substring(y, i)
    tail_txt <- sub("type: matrix", "type: tabular", tail_txt, fixed = TRUE)
    tail_txt <- sub("row_labels:", "columns:", tail_txt, fixed = TRUE)
    tail_txt <- sub("\n  matrix:.*$", "\n", tail_txt)
    session$setInputs(raw_yaml_editor = paste0(substring(y, 1, i - 1), tail_txt))
    session$setInputs(apply_yaml = 1)
    qa_check("the edited text applies", isTRUE(rv$yaml_msg$ok))

    qa_step(
      "the dataset types are now matrix and tabular",
      c("matrix", "tabular"), unname(vapply(rv$structure, function(s) s$type, ""))
    )
    qa_step(
      "the retyped dataset keeps no upload record",
      FALSE, any(startsWith(names(rv$uploads), "mature_counts||"))
    )
    qa_step("and no bound table", 0L, length(rv$dta@datasets[["mature_counts"]]@tables))
    qa_step("and is back to having no data", "nodata", mxa_status(rv, "mature_counts"))
    qa_check("the other dataset keeps its upload record", "gene_counts||1" %in% names(rv$uploads))
    qa_step("and its bound table", 1L, length(rv$dta@datasets[["gene_counts"]]@tables))
    qa_step("and its recorded header", 1L, length(rv$dta@datasets[["gene_counts"]]@heads))
    qa_step("and its verdict", "pass", mxa_status(rv, "gene_counts"))
  })
})

test_that("OQ-APP-046 | applying raw YAML that changes the corner of a loaded matrix reports the table as needing a reload, not a crash | REQ-APP-037", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("n", 32))
    session$setInputs(dta_file = qapp_file_input("matrix_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("matrix_gene_counts.tsv"))
    session$setInputs(check_all = 1)
    qa_step("the delivery passes with the corner as shipped", "pass", mxa_status(rv, "gene_counts"))
    qapp_unlock_editing(session)

    # The first corner entry becomes null: the row-label count under the header
    # changes, so the names the recorded header yields no longer match the table.
    y <- rv$yaml_text
    edited <- sub("corner:\n      - gene_id\n      - gene_name", "corner:\n      - ~\n      - gene_name", y, fixed = TRUE)
    qa_check("the corner was edited in the text", !identical(y, edited))
    session$setInputs(raw_yaml_editor = edited)
    session$setInputs(apply_yaml = 1)
    qa_check("the edited text applies", isTRUE(rv$yaml_msg$ok))
    qa_step("the loaded table is still held", 1L, length(rv$dta@datasets[["gene_counts"]]@tables))

    result <- tryCatch(
      {
        session$setInputs(check_all = 2)
        NULL
      },
      error = function(e) e
    )
    qa_check("checking afterwards does not raise", is.null(result))
    found <- qapp_fn("dta_dataset_messages")(rv$dta, "gene_counts")
    qa_check(
      "the dataset's findings tell the user to load the file again",
      any(grepl("load it again", found$message, fixed = TRUE))
    )
    qa_step("and its verdict is a failure", "fail", mxa_status(rv, "gene_counts"))
  })
})

test_that("OQ-APP-047 | unloading a matrix table drops its recorded header and keeps the others | REQ-APP-038 | tags: white-box", {
  qapp_skip_unless_app_installed()
  lines <- c(
    "datasets:", "  - id: gc", "    type: matrix", "    files:",
    "      - { filename: \"a.tsv\", type: tsv }",
    "      - { filename: \"b.tsv\", type: tsv }",
    "    row_labels:",
    "      - { id: GENE_ID, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, type: SAS Char, nullable: Yes }",
    "    matrix:", "      header:", "        corner: [gene_id, gene_name]",
    "      rows: { key: GENE_ID }", "      cells: { type: SAS Num }"
  )
  dta <- mxa_read(lines)
  dir <- qa_tempdir()
  file.copy(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"), file.path(dir, c("a.tsv", "b.tsv")))
  ds <- dta@datasets[["gc"]]
  ds <- load_file(ds, file = file.path(dir, "a.tsv"), handler_index = 1, stream = "never")
  ds <- load_file(ds, file = file.path(dir, "b.tsv"), handler_index = 2, stream = "never")
  dta@datasets[["gc"]] <- ds
  qa_step("both tables carry a recorded header to begin with", c("a", "b"), names(ds@heads))

  one <- qapp_fn("dta_unload_table")(dta, "gc", "a")
  qa_check("unloading one table succeeds", isTRUE(one$ok))
  qa_step("the unloaded table's header is gone", "b", names(one$value@datasets[["gc"]]@heads))
  qa_step("and the other table is still bound", "b", names(one$value@datasets[["gc"]]@tables))

  every <- qapp_fn("dta_unload_all")(dta, "gc")
  qa_check("unloading everything succeeds", isTRUE(every$ok))
  qa_step("no recorded header remains", 0L, length(every$value@datasets[["gc"]]@heads))
  qa_step("and no table", 0L, length(every$value@datasets[["gc"]]@tables))
})

test_that("OQ-APP-048 | the application lists matrix as a supported type and offers a matrix dataset csv and tsv file entries only | REQ-APP-039 | tags: white-box", {
  qapp_skip_unless_app_installed()
  qa_check("matrix is among the supported dataset types", "matrix" %in% qapp_fn("dta_supported_types")())
  qa_check("as are the two types that existed before", all(c("tabular", "file") %in% qapp_fn("dta_supported_types")()))
  handlers <- qapp_fn("dta_handler_types")
  qa_step("a matrix dataset is offered csv and tsv", c("csv", "tsv"), handlers("matrix"))
  qa_step("a tabular dataset is still offered csv, tsv, sas7bdat and xpt", c("csv", "tsv", "sas7bdat", "xpt"), handlers("tabular"))
  qa_step("a file dataset is still offered any type", "any", handlers("file"))
})

test_that("OQ-APP-049 | a matrix dataset loads, validates, and its findings can be inspected | REQ-APP-040", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  # One cell of the genes file made negative: row 1, column S0001, where the
  # specification says range.min is 0.
  lines <- readLines(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"))
  fields <- strsplit(lines[2], "\t", fixed = TRUE)[[1]]
  qa_check("the cell to change holds 118", identical(fields[[3]], "118"))
  fields[[3]] <- "-118"
  lines[2] <- paste(fields, collapse = "\t")
  negative <- file.path(qa_tempdir(), "matrix_gene_counts.tsv")
  writeLines(lines, negative)
  upload <- data.frame(
    name = "matrix_gene_counts.tsv", size = file.size(negative), type = "",
    datapath = negative, stringsAsFactors = FALSE
  )

  shiny::testServer(app, {
    mxa_load_and_check(session)
    qa_step(
      "a conforming delivery of each matrix dataset passes",
      c(gene_counts = "pass", mature_counts = "pass"),
      c(gene_counts = mxa_status(rv, "gene_counts"), mature_counts = mxa_status(rv, "mature_counts"))
    )
  })

  shiny::testServer(app, {
    mxa_load_and_check(session, genes = upload)
    qa_step("a delivery with a cell below the minimum fails", "fail", mxa_status(rv, "gene_counts"))
    qa_step("while the other dataset is unaffected", "pass", mxa_status(rv, "mature_counts"))

    found <- qapp_fn("dta_dataset_messages")(rv$dta, "gene_counts")
    hit <- found[found$keyword %in% "minimum", , drop = FALSE]
    qa_step(
      "the findings list one minimum finding, at row 1 of column S0001",
      list(n = 1L, row = 1L, column = "S0001"),
      list(n = nrow(hit), row = as.integer(hit$row), column = as.character(hit$column))
    )

    inspected <- qapp_fn("dta_inspect")(rv$dta, "gene_counts", hit$id[[1]])
    qa_check("the finding can be inspected", isTRUE(inspected$ok))
    qa_check(
      "and the inspected row shows the offending value",
      any(grepl("-118", unlist(inspected$value, use.names = FALSE), fixed = TRUE))
    )
  })
})
