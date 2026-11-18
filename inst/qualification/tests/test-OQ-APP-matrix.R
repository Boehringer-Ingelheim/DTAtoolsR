# The application and a matrix dataset. REQ-APP-033 .. REQ-APP-046.
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

# One matrix dataset whose first row label has no field in the header line
# (header: No), whose second has the ordinary text named, whose third has an
# empty text and whose fourth declares no header.
mxa_header_lines <- c(
  "datasets:",
  "  - id: gene_counts",
  "    type: matrix",
  "    files: { filename: \"gene_counts.tsv\", type: tsv }",
  "    row_labels:",
  "      - { id: GENE_ID, header: No, type: SAS Char, nullable: No }",
  "      - { id: GENE_NAME, header: named, type: SAS Char, nullable: Yes }",
  "      - { id: NOTE, header: '', type: SAS Char, nullable: Yes }",
  "      - { id: BIOTYPE, type: SAS Char, nullable: Yes }",
  "    matrix:",
  "      columns:",
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

test_that("OQ-APP-042 | saving a matrix dataset writes row_labels and matrix, and the header of each row label (No, a text, an empty text, none) survives save, reload and save | REQ-APP-033", {
  qapp_skip_unless_app_installed()
  dta <- mxa_read(mxa_header_lines)
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
    c("GENE_ID", "GENE_NAME", "NOTE", "BIOTYPE"), vapply(saved$row_labels, function(x) x$id, "")
  )
  qa_check("header: No is written as the plain boolean false", identical(saved$row_labels[[1]]$header, FALSE))
  qa_step("the text named is written as that text", "named", saved$row_labels[[2]]$header)
  qa_step("an empty text is written as an empty text, not dropped", "", saved$row_labels[[3]]$header)
  qa_check("a row label with no header declares none", is.null(saved$row_labels[[4]]$header))

  again <- mxa_read(strsplit(first$value, "\n", fixed = TRUE)[[1]])
  ds <- again@datasets[["gene_counts"]]
  qa_step(
    "reading the saved text back yields the same row labels",
    c("GENE_ID", "GENE_NAME", "NOTE", "BIOTYPE"), names(ds@specs@columns)
  )
  back <- lapply(ds@specs@columns, function(col) as.list(col)$header)
  qa_check("and the same header on each: No, named, an empty text, none", identical(
    unname(back), list(FALSE, "named", "", NULL)
  ))
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
    # A tabular column has no header key: drop the row label's (an empty text).
    tail_txt <- sub("\n\\s+header: (''|\"\")", "", tail_txt)
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

test_that("OQ-APP-046 | applying raw YAML that gives the first row label no header field in a loaded matrix reports the table as needing a reload, not a crash | REQ-APP-037", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("n", 32))
    session$setInputs(dta_file = qapp_file_input("matrix_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("matrix_gene_counts.tsv"))
    session$setInputs(check_all = 1)
    qa_step("the delivery passes with the header as shipped", "pass", mxa_status(rv, "gene_counts"))
    qapp_unlock_editing(session)

    # The first row label loses its field in the header line: the number of
    # fields above the data columns changes, so the names the recorded header
    # yields no longer match the table.
    y <- rv$yaml_text
    edited <- sub("header: gene_id", "header: No", y, fixed = TRUE)
    qa_check("the header was edited in the text", !identical(y, edited))
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
    "      - { id: GENE_ID, header: gene_id, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, header: gene_name, type: SAS Char, nullable: Yes }",
    "    matrix:",
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
  upload <- qapp_upload_value(negative, "matrix_gene_counts.tsv")

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

# ---- Editing a matrix dataset in the app (REQ-APP-041 .. REQ-APP-046) -------

# A document with the bundled matrix specification loaded and editing entered.
mxa_editing <- function(session, id = strrep("p", 32)) {
  session$setInputs(dta_client_id = id)
  session$setInputs(dta_file = qapp_file_input("matrix_dta.yaml"))
  qapp_enter_edit_mode(session)
}

# The inputs the row-label form carries, for one label.
mxa_label_form <- function(id, nullable = FALSE, pattern = "", header_mode = "id", header_text = "") {
  list(
    col_header_mode = header_mode, col_header_text = header_text,
    col_id = id, col_label = "", col_backend = "SAS", col_type = "Char",
    col_format = "", col_length = "", col_nullable = nullable, col_values = "",
    col_pattern = pattern, col_desc = ""
  )
}

# An upload as the browser reports it, for a file written by a test.
mxa_upload <- function(path) qapp_upload_value(path)

test_that("OQ-APP-050 | adding a matrix dataset needs a row-label ID, a header choice and a cell type and builds the declared matrix | REQ-APP-041", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    mxa_editing(session)
    n_before <- length(rv$dta@datasets)

    # Each of the three missing in turn (and another text left blank): the dataset is not added.
    session$setInputs(add_ds_id = "mx_a", add_ds_type = "matrix", add_ds_mx_label = "PID", add_ds_mx_cells = "SAS Num")
    session$setInputs(add_ds_save = 1)
    qa_step("without a header choice the dataset is not added", n_before, length(rv$dta@datasets))
    qa_check("and the refusal is reported", isFALSE(rv$add_ds_msg$ok))

    session$setInputs(add_ds_mx_header = "text", add_ds_mx_header_text = "  ", add_ds_mx_cells = "SAS Num")
    session$setInputs(add_ds_save = 11)
    qa_step("another text chosen and left blank: not added", n_before, length(rv$dta@datasets))

    session$setInputs(add_ds_mx_header = "empty", add_ds_mx_cells = "")
    session$setInputs(add_ds_save = 2)
    qa_step("without a cell type the dataset is not added", n_before, length(rv$dta@datasets))

    session$setInputs(add_ds_mx_cells = "SAS Num", add_ds_mx_label = "")
    session$setInputs(add_ds_save = 3)
    qa_step("without a row-label ID the dataset is not added", n_before, length(rv$dta@datasets))

    # All three given: an empty field above the row label.
    session$setInputs(add_ds_mx_label = "PID")
    session$setInputs(add_ds_save = 4)
    qa_step("with all three the dataset is added", n_before + 1L, length(rv$dta@datasets))
    ds <- rv$dta@datasets[["mx_a"]]
    qa_check("and is a matrix dataset", inherits(ds, "DTAtools::DTADataSetMatrix"))
    cols <- ds@specs@columns
    qa_step("it has the one row-label column given", "PID", names(cols))
    qa_step(
      "which is SAS Char and not nullable",
      list(type = "SAS Char", nullable = FALSE),
      list(type = as.list(cols[["PID"]])$type, nullable = cols[["PID"]]@nullable)
    )
    qa_step("and is the row key", "PID", ds@matrix@row_key)
    l <- as.list(ds@matrix)
    qa_step("the header is the one chosen, an empty field", "", as.list(cols[["PID"]])$header)
    qa_step("the cell type is the one chosen", "SAS Num", l$cells$type)
    qa_step("at least one data column is required", 1, ds@matrix@n_columns[[1L]])
    qa_step("and at least one data row", 1, ds@matrix@n_rows[[1L]])

    # The other three choices, each on a dataset of its own.
    chosen <- list(
      id = list(NULL, ""), text = list("pid", "pid"), none = list(FALSE, "")
    )
    for (mode in names(chosen)) {
      session$setInputs(
        add_ds_id = paste0("mx_", mode), add_ds_mx_label = "PID", add_ds_mx_header = mode,
        add_ds_mx_header_text = chosen[[mode]][[2]], add_ds_mx_cells = "SAS Num"
      )
      session$setInputs(add_ds_save = 20 + match(mode, names(chosen)))
      added <- rv$dta@datasets[[paste0("mx_", mode)]]
      qa_check(
        sprintf("the choice %s declares the header of the requirement sentence", mode),
        !is.null(added) && identical(as.list(added@specs@columns[["PID"]])$header, chosen[[mode]][[1]])
      )
    }
  })
})

test_that("OQ-APP-051 | row-label edits keep a matrix valid: rename follows into the key, adding a row label leaves the other headers unchanged, removing the key is refused | REQ-APP-042", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    mxa_editing(session, strrep("q", 32))
    session$setInputs(edit_cols = 1)
    pat <- "^ENSG[0-9]{11}(\\.[0-9]+)?(_PAR_Y)?$"

    # Rename the first row label (the row key).
    session$setInputs(col_edit_click = 1)
    do.call(session$setInputs, mxa_label_form("GENE_UID", pattern = pat, header_mode = "text", header_text = "gene_id"))
    session$setInputs(col_save = 1)
    ds <- rv$dta@datasets[["gene_counts"]]
    qa_step("the renamed label is in the specification", c("GENE_UID", "GENE_NAME"), names(ds@specs@columns))
    qa_step("and the row key follows it", "GENE_UID", ds@matrix@row_key)

    # Add a third row label: the headers of the other two stay as they were.
    k <- length(ds@specs@columns)
    header_of <- function(ds) lapply(ds@specs@columns, function(col) as.list(col)$header)
    headers_before <- header_of(ds)
    session$setInputs(col_add = 1)
    do.call(session$setInputs, mxa_label_form("NOTE", nullable = TRUE))
    session$setInputs(col_save = 2)
    ds <- rv$dta@datasets[["gene_counts"]]
    qa_step("the new label is added", k + 1L, length(ds@specs@columns))
    qa_step(
      "the header of each earlier row label is unchanged", unname(headers_before),
      unname(header_of(ds)[seq_len(k)])
    )
    qa_step("and the new row label's header is its id", "NOTE", names(ds@specs@columns)[[k + 1L]])
    qa_check("with no header declared", is.null(header_of(ds)[[k + 1L]]))

    # Remove the second row label, which is not the row key: the headers of
    # the two that remain stay as they were.
    kept_before <- header_of(ds)[c("GENE_UID", "NOTE")]
    session$setInputs(col_del_click = 2)
    ds <- rv$dta@datasets[["gene_counts"]]
    qa_step("the row label is removed", c("GENE_UID", "NOTE"), names(ds@specs@columns))
    qa_step(
      "the header of each remaining row label is unchanged", unname(kept_before),
      unname(header_of(ds))
    )

    # Remove the row key: refused, nothing changes.
    before <- rv$dta
    session$setInputs(col_del_click = 1)
    qa_check("removing the row-key column leaves the document unchanged", identical(rv$dta, before))
    qa_step(
      "and the key column is still there",
      TRUE, "GENE_UID" %in% names(rv$dta@datasets[["gene_counts"]]@specs@columns)
    )
  })
})

test_that("OQ-APP-052 | the matrix-layout editor saves into the specification and re-validates, an unchanged save changes nothing, minimum above maximum is refused | REQ-APP-043", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  rows <- length(readLines(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"))) - 1L

  shiny::testServer(app, {
    mxa_load_and_check(session)
    qa_step("the genes delivery passes before the edit", "pass", mxa_status(rv, "gene_counts"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_matrix = 1)
    session$setInputs(mx_row_key = "GENE_ID")

    # One more data row required than the file holds.
    session$setInputs(mx_n_rows_min = rows + 1)
    session$setInputs(mx_save = 1)
    ds <- datasets(rv$dta, "gene_counts")
    qa_step("the new row-count minimum is in the specification", rows + 1, ds@matrix@n_rows[[1L]])
    qa_step("and in the YAML text", TRUE, grepl(sprintf("min: %d", rows + 1L), rv$yaml_text, fixed = TRUE))
    qa_step("the dataset is re-validated against it, and fails", "fail", mxa_status(rv, "gene_counts"))

    # Saving again with nothing changed.
    session$setInputs(edit_matrix = 2)
    # Every field as a browser returns it, taken from the bundled YAML by
    # hand-written code: text as strings (empty ones as ""), a blank number
    # as NA, checkboxes as logicals, the radio as its value, the key as a
    # character vector. The row-count minimum is the one saved just above.
    y <- yaml::read_yaml(system.file("extdata", "matrix_dta.yaml", package = "DTAtools"))$datasets[[1]]
    labels <- vapply(y$row_labels, function(l) l$id, "")
    do.call(session$setInputs, c(
      list(
        mx_columns_label = y$matrix$columns$label,
        mx_columns_description = "",
        mx_columns_pattern = y$matrix$columns$pattern,
        mx_columns_values = "", mx_columns_length = "",
        mx_n_columns_min = y$matrix$columns$count$min,
        mx_n_columns_max = y$matrix$columns$count$max,
        mx_row_key = y$matrix$rows$key,
        mx_n_rows_min = rows + 1, mx_n_rows_max = NA_real_,
        mx_cell_type = y$matrix$cells$type,
        mx_cell_nullable = FALSE,
        mx_range_min = y$matrix$cells$range$min, mx_range_max = NA_real_,
        mx_infinite = FALSE,
        mx_cell_values = "", mx_cell_pattern = "", mx_cell_length = "",
        mx_cell_format = "", mx_cell_label = "", mx_cell_description = ""
      )
    ))
    before <- as.list(datasets(rv$dta, "gene_counts")@matrix)
    session$setInputs(mx_save = 2)
    qa_check(
      "an unchanged save leaves the matrix settings as they were",
      identical(before, as.list(datasets(rv$dta, "gene_counts")@matrix))
    )

    # A minimum above the maximum.
    session$setInputs(edit_matrix = 3)
    session$setInputs(mx_row_key = "GENE_ID", mx_n_rows_min = 10, mx_n_rows_max = 5)
    held <- rv$dta
    session$setInputs(mx_save = 3)
    qa_step("a minimum above the maximum is refused", FALSE, isTRUE(rv$mx_msg$ok))
    qa_check("and leaves the dataset unchanged", identical(rv$dta, held))
  })
})

test_that("OQ-APP-053 | a rule on a matrix row label is saved and a rule ID starting .matrix: is refused | REQ-APP-044", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  rule_ids <- function(dta) vapply(datasets(dta, "gene_counts")@specs@rules, function(r) r@id, character(1))

  shiny::testServer(app, {
    mxa_editing(session, strrep("r", 32))
    session$setInputs(edit_rules = 1)
    n_before <- length(rule_ids(rv$dta))

    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_unique", rule_id = "gene_name_unique", rule_desc = "", rule_cols = "GENE_NAME")
    session$setInputs(rule_save = 1)
    qa_step("the rule on a row-label column is added", n_before + 1L, length(rule_ids(rv$dta)))
    qa_step("under the ID given", TRUE, "gene_name_unique" %in% rule_ids(rv$dta))

    held <- rv$dta
    session$setInputs(rule_add = 2)
    session$setInputs(rule_type = "col_unique", rule_id = ".matrix:x", rule_desc = "", rule_cols = "GENE_NAME")
    session$setInputs(rule_save = 2)
    qa_step("a rule ID starting .matrix: is refused", FALSE, isTRUE(rv$rule_msg$ok))
    qa_check("and the rules are unchanged", identical(rule_ids(rv$dta), rule_ids(held)))
  })
})

test_that("OQ-APP-054 | inspecting a too-few-rows finding states the declared minimum and the actual number of data rows | REQ-APP-045 | tags: white-box", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  dir <- qa_tempdir()
  spec <- file.path(dir, "few_rows.yaml")
  writeLines(c(
    "datasets:",
    "  - id: few",
    "    type: matrix",
    "    files: { filename: \"few.tsv\", type: tsv }",
    "    row_labels:",
    "      - { id: ID, header: id, type: SAS Char, nullable: No }",
    "    matrix:",
    "      rows: { count: { min: 5 } }",
    "      cells: { type: SAS Int, nullable: No }"
  ), spec)
  data <- file.path(dir, "few.tsv")
  writeLines(c("id\tS1\tS2", "a\t1\t2", "b\t3\t4"), data) # two data rows

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("s", 32))
    session$setInputs(dta_file = mxa_upload(spec))
    session$setInputs(up_1_1 = mxa_upload(data))
    session$setInputs(check_all = 1)
    qa_step("the delivery fails", "fail", mxa_status(rv, "few"))

    found <- qapp_fn("dta_dataset_messages")(rv$dta, "few")
    hit <- found[found$keyword %in% "minItems", , drop = FALSE]
    qa_step("with one row-count finding", 1L, nrow(hit))

    inspected <- qapp_fn("dta_inspect")(rv$dta, "few", hit$id[[1]])
    qa_check("it can be inspected", isTRUE(inspected$ok))
    body <- as.character(render_inspect_body(inspected$value, "few"))
    qa_check("the panel states the declared minimum of 5 data rows", grepl("at least 5 data rows", body, fixed = TRUE))
    qa_check("and the actual 2 data rows", grepl("2 data rows", body, fixed = TRUE))
  })
})

test_that("OQ-APP-055 | a refused file is listed in the dataset's messages until a matching file loads, and removing a file entry keeps a lazy table valid | REQ-APP-046", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  qa_requires("arrow")

  wrong <- file.path(qa_tempdir(), "not_the_expected_name.tsv")
  file.copy(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"), wrong)

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("t", 32))
    session$setInputs(dta_file = qapp_file_input("matrix_dta.yaml"))
    session$setInputs(up_1_1 = mxa_upload(wrong))
    shown <- msgs_r()
    shown <- shown[shown$source %in% "load", , drop = FALSE]
    qa_step("the refused file is listed once, as a load message", 1L, nrow(shown))
    qa_check("naming the file", grepl("not_the_expected_name.tsv", shown$message, fixed = TRUE))
    qa_check("and giving the reason", grepl("does not match", shown$message, fixed = TRUE))

    session$setInputs(up_1_1 = qapp_file_input("matrix_gene_counts.tsv"))
    shown <- msgs_r()
    qa_step("a matching file loading in its place removes it", 0L, sum(shown$source %in% "load"))
  })

  # Removing the first of two file entries, with the table loaded lazily
  # through the second.
  lines <- c(
    "datasets:", "  - id: gc", "    type: matrix", "    files:",
    "      - { filename: \"a.tsv\", type: tsv }",
    "      - { filename: \"b.tsv\", type: tsv }",
    "    row_labels:",
    "      - { id: GENE_ID, header: gene_id, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, header: gene_name, type: SAS Char, nullable: Yes }",
    "    matrix:",
    "      rows: { key: GENE_ID }", "      cells: { type: SAS Num }"
  )
  dta <- mxa_read(lines)
  dir <- qa_tempdir()
  file.copy(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"), file.path(dir, "b.tsv"))
  ds <- load_file(dta@datasets[["gc"]], file = file.path(dir, "b.tsv"), handler_index = 2, stream = "always")
  dta@datasets[["gc"]] <- ds
  before <- check(dta, quiet = TRUE, persist = FALSE)
  qa_check("control: the lazily held table passes with both entries", isTRUE(validation_status(before@datasets[["gc"]])$ok))

  removed <- qapp_fn("dta_remove_handler")(dta, "gc", 1)
  qa_check("removing the first file entry succeeds", isTRUE(removed$ok))
  after <- check(removed$value, quiet = TRUE, persist = FALSE)
  d <- after@datasets[["gc"]]
  qa_step(
    "no finding says the file changed or must be loaded again", 0L,
    sum(grepl("load it again", as.data.frame(messages(d))$message, fixed = TRUE))
  )
  qa_check("and the table passes", isTRUE(validation_status(d)$ok))

  # Reordering the two file entries in raw YAML, through the application: the
  # table was loaded lazily through the second entry (an upload in slot 2).
  qa_requires("withr")
  withr::local_options(DTAtools.stream_threshold = 1)
  rdir <- qa_tempdir()
  spec <- file.path(rdir, "gc.yaml")
  writeLines(c("metadata:", "  title: Reorder", "  version: \"1.0\"", lines), spec)
  file.copy(system.file("extdata", "matrix_gene_counts.tsv", package = "DTAtools"), file.path(rdir, "b.tsv"))

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("u", 32))
    session$setInputs(dta_file = mxa_upload(spec))
    session$setInputs(up_1_2 = mxa_upload(file.path(rdir, "b.tsv")))
    session$setInputs(check_all = 1)
    qa_check("the lazily loaded table passes before the reorder", isTRUE(validation_status(rv$dta@datasets[["gc"]])$ok))
    qapp_unlock_editing(session)

    y <- rv$yaml_text
    swapped <- gsub("@@", "b.tsv", gsub("b.tsv", "a.tsv", gsub("a.tsv", "@@", y, fixed = TRUE), fixed = TRUE), fixed = TRUE)
    qa_check("the two file entries were swapped in the text", !identical(y, swapped))
    session$setInputs(raw_yaml_editor = swapped)
    session$setInputs(apply_yaml = 1)
    qa_check("the reordered text applies", isTRUE(rv$yaml_msg$ok))
    qa_step("the table is still held", 1L, length(rv$dta@datasets[["gc"]]@tables))
    session$setInputs(check_all = 2)
    found <- qapp_fn("dta_dataset_messages")(rv$dta, "gc")
    qa_step(
      "no finding says the file changed or must be loaded again", 0L,
      sum(grepl("load it again", found$message, fixed = TRUE))
    )
    qa_check("and the table passes", isTRUE(validation_status(rv$dta@datasets[["gc"]])$ok))
  })
})

# ---- the header of a row label in the application (REQ-APP-042, REQ-APP-047) --

test_that("OQ-APP-056 | no field above a row label that is not first, and moving the first row label away while it has no field, are refused and change nothing | REQ-APP-042 | tags: white-box", {
  qapp_skip_unless_app_installed()
  set_column <- qapp_fn("dta_set_column")
  move <- qapp_fn("dta_move_column")
  header_of <- function(dta) {
    lapply(datasets(dta, "gene_counts")@specs@columns, function(col) as.list(col)$header)
  }
  dta <- mxa_read(mxa_header_lines)

  # header: No on the second row label.
  r <- set_column(
    dta, "gene_counts",
    id = "GENE_NAME", backend = "SAS", type = "Char", nullable = TRUE,
    old_id = "GENE_NAME", header_mode = "none"
  )
  qa_step("no field on a row label that is not the first is refused", FALSE, r$ok)
  qa_check("and the dataset is not changed", is.null(r$value))

  # The first row label has no field: it must stay first.
  down <- move(dta, "gene_counts", "GENE_ID", "down")
  qa_step("moving the first row label down while it has no field is refused", FALSE, down$ok)
  up <- move(dta, "gene_counts", "GENE_NAME", "up")
  qa_step("and so is moving another row label into first place", FALSE, up$ok)
  qa_step(
    "the order of the row labels is unchanged",
    c("GENE_ID", "GENE_NAME", "NOTE", "BIOTYPE"), names(datasets(dta, "gene_counts")@specs@columns)
  )
  # Moving two later row labels is allowed and leaves every header as it was.
  ok <- move(dta, "gene_counts", "NOTE", "down")
  qa_step("moving a later row label is accepted", TRUE, ok$ok)
  qa_step(
    "and each row label keeps its own header",
    list(GENE_ID = FALSE, GENE_NAME = "named", NOTE = "", BIOTYPE = NULL)[c("GENE_ID", "GENE_NAME", "NOTE")],
    header_of(ok$value)[c("GENE_ID", "GENE_NAME", "NOTE")]
  )
})

test_that("OQ-APP-057 | the row-label form reports and saves the header of a row label as a choice and a text | REQ-APP-047 | tags: white-box", {
  qapp_skip_unless_app_installed()
  fields <- qapp_fn("dta_column_fields")
  set_column <- qapp_fn("dta_set_column")
  dta <- mxa_read(mxa_header_lines)
  got <- function(id) {
    f <- fields(dta, "gene_counts", id)
    c(f$header_mode, f$header_text)
  }
  qa_step("header: No is the choice none", c("none", ""), got("GENE_ID"))
  qa_step("a text is the choice text with that text", c("text", "named"), got("GENE_NAME"))
  qa_step("an empty text is the choice empty", c("empty", ""), got("NOTE"))
  qa_step("no header is the choice id", c("id", ""), got("BIOTYPE"))

  saved <- set_column(
    dta, "gene_counts",
    id = "BIOTYPE", backend = "SAS", type = "Char", nullable = TRUE,
    old_id = "BIOTYPE", header_mode = "text", header_text = "biotype"
  )
  qa_check("saving the form succeeds", isTRUE(saved$ok))
  col <- datasets(saved$value, "gene_counts")@specs@columns[["BIOTYPE"]]
  qa_step("it declares exactly that header on the row label", "biotype", as.list(col)$header)
  qa_step("and leaves the row label's other settings", c("SAS Char", TRUE), c(as.list(col)$type, col@nullable))
})

test_that("OQ-APP-058 | a matrix dataset shows a sketch of its file and the header text of each row label | REQ-APP-048 | tags: white-box", {
  qapp_skip_unless_app_installed()
  sketch <- qapp_fn("dta_matrix_sketch")
  overview <- qapp_fn("dta_columns_overview")
  # GENE_ID: no field; GENE_NAME: the text named; NOTE: an empty field;
  # BIOTYPE: no header declared.
  dta <- mxa_read(mxa_header_lines)
  lines <- sketch(dta, "gene_counts")
  qa_step("the sketch has two lines", 2L, length(lines))
  ids <- c("GENE_ID", "GENE_NAME", "NOTE", "BIOTYPE")
  at <- vapply(ids, function(id) as.integer(regexpr(id, lines[[2]], fixed = TRUE)), integer(1))
  qa_check("the second line holds the row-label IDs in file order", all(at > 0L) && !is.unsorted(at, strictly = TRUE))
  # What the first line holds above each ID (none of these texts is wider than its ID).
  above <- vapply(ids, function(id) trimws(substr(lines[[1]], at[[id]], at[[id]] + nchar(id) - 1L)), character(1))
  qa_step(
    "the first line holds nothing, the text, two double quotes and the ID above them",
    c("", "named", "\"\"", "BIOTYPE"), unname(above)
  )
  qa_check(
    "and ends with a note naming the row label that has no field above it",
    grepl("no field above GENE_ID", lines[[1]], fixed = TRUE)
  )
  qa_step(
    "the row-label list carries the header of each row label in a column named header",
    c("(no field)", "named", "(empty)", "BIOTYPE"), overview(dta, "gene_counts")$header
  )

  tab <- suppressMessages(read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools")))
  qa_step("a tabular dataset has no sketch", 0L, length(sketch(tab, "clinical_data")))
  qa_check(
    "and its column list has no header column",
    !("header" %in% names(overview(tab, "clinical_data")))
  )
})

test_that("OQ-APP-059 | an edit of the row labels asks for the file to be loaded again exactly when the loaded table goes stale | REQ-APP-049 | tags: white-box", {
  qapp_skip_unless_app_installed()
  needs <- qapp_fn("dta_matrix_needs_reload")
  set_column <- qapp_fn("dta_set_column")
  value <- function(r) {
    qa_check("the edit succeeds", isTRUE(r$ok))
    r$value
  }
  set <- function(dta, id, ...) value(set_column(dta, "gene_counts", id = id, backend = "SAS", type = "Char", ...))
  # The bundled specification with the genes file loaded: GENE_ID, GENE_NAME.
  bound <- mxa_bound_gene_counts()

  stale <- list(
    "a renamed row label" = set(bound, "GENE_UID", nullable = FALSE, old_id = "GENE_ID"),
    "an added row label" = set(bound, "NOTE", nullable = TRUE),
    "a removed row label" = value(qapp_fn("dta_remove_column")(bound, "gene_counts", "GENE_NAME")),
    "a moved row label" = value(qapp_fn("dta_move_column")(bound, "gene_counts", "GENE_NAME", "up")),
    "no header field above the first row label" = set(bound, "GENE_ID", nullable = FALSE, header_mode = "none")
  )
  for (what in names(stale)) {
    qa_step(sprintf("%s asks for a reload", what), TRUE, needs(bound, stale[[what]], "gene_counts"))
  }

  same <- list(
    "another header text" = set(bound, "GENE_ID", nullable = FALSE, header_mode = "text", header_text = "Gene"),
    "another constraint" = set(bound, "GENE_NAME", nullable = TRUE, pattern = "^[A-Z]"),
    "no edit" = bound
  )
  for (what in names(same)) {
    qa_step(sprintf("%s does not", what), FALSE, needs(bound, same[[what]], "gene_counts"))
  }

  unbound <- suppressMessages(read_dta_from_yaml(system.file("extdata", "matrix_dta.yaml", package = "DTAtools")))
  qa_step(
    "a rename with no table loaded does not",
    FALSE, needs(unbound, set(unbound, "GENE_UID", nullable = FALSE, old_id = "GENE_ID"), "gene_counts")
  )
  tab <- suppressMessages(read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools")))
  qa_step("a tabular dataset does not", FALSE, needs(tab, tab, "clinical_data"))
})
