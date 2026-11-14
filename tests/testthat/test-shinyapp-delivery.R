# Deliveries in the Shiny app: a drop holding a .zip / .xpt is routed across the
# whole document by load_delivery(), asks once before it replaces loaded tables,
# and every extracted file is owned by the upload record that names it.
#
# Every file a test drops is written BEFORE shiny::testServer() starts: a
# withr::local_*() call made inside the block defers to an environment that
# never exits, so its files would outlive the test.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# A fileInput() value for files this test wrote (or a fixture).
fi <- function(path, name) {
  data.frame(
    name = name, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

# Write the lines of a YAML document to a temp file owned by the calling test.
dv_yaml <- function(lines, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(lines, f)
  f
}

dv_header <- c(
  "metadata:",
  "  title: Delivery test",
  "  version: \"1\"",
  "datasets:"
)
dv_ae <- function(files = "{filename: ae.csv, type: csv}") {
  c(
    "  - id: ae",
    "    type: tabular",
    paste0("    files: ", files),
    "    columns:",
    "      - {id: USUBJID, type: SAS Char}",
    "      - {id: AETERM, type: SAS Char}",
    "      - {id: AESEQ, type: SAS Num}"
  )
}
dv_dm <- c(
  "  - id: dm",
  "    type: tabular",
  "    files: {filename: dm.csv, type: csv}",
  "    columns:",
  "      - {id: USUBJID, type: SAS Char}",
  "      - {id: AGE, type: SAS Num}"
)

# The plain specification (ae, dm), and the container one (ae declares
# container: sdtm_csv.zip).
dv_spec <- function(env = parent.frame()) dv_yaml(c(dv_header, dv_ae(), dv_dm), env)
dv_spec_container <- function(ae_container = "sdtm_csv.zip", env = parent.frame()) {
  dv_yaml(
    c(dv_header, dv_ae(sprintf("{filename: ae.csv, type: csv, container: %s}", ae_container)), dv_dm),
    env
  )
}

# A copy of a file at another path (testServer only fires an observer when the
# input value changes).
dv_copy <- function(path, name = basename(path), env = parent.frame()) {
  cont_write(name, readBin(path, "raw", file.size(path)), env = env)
}

# Capture every notification the app shows into `shown` of the calling test.
mock_notifications <- function(env = parent.frame()) {
  env$shown <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      env$shown <- c(env$shown, paste(as.character(ui), collapse = ""))
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
}

# Capture the HTML of every modal the app shows into `modals` of the calling
# test (whitespace collapsed: htmltools pretty-prints it).
mock_modals <- function(env = parent.frame()) {
  env$modals <- character(0)
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) {
      env$modals <- c(env$modals, flat_message(paste(as.character(ui), collapse = "")))
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
}

# The staged copies (not delivery directories) the session still owns.
staged_dirs <- function(session) {
  u <- session$env$owned$upload
  u[!startsWith(basename(u), "dta_upload_delivery_")]
}

# The delivery directories the session still owns and that exist.
delivery_dirs <- function(session) {
  u <- session$env$owned$upload
  u[startsWith(basename(u), "dta_upload_delivery_") & dir.exists(u)]
}

# The delivery directory a path lies under.
delivery_root <- function(path) {
  if (!length(path)) {
    return(character(0))
  }
  while (!startsWith(basename(path), "dta_upload_delivery_") && dirname(path) != path) path <- dirname(path)
  path
}

# A plan row with the columns of load_delivery()'s `last_delivery` table.
plan_row <- function(name, status, dataset = NA_character_, item = NA_character_,
                     reason = NA_character_, delivered = "sdtm.zip") {
  data.frame(
    delivered = delivered, container = NA_character_, member = NA_character_,
    name = name, dataset = dataset,
    handler = if (identical(status, "loaded")) 1L else NA_integer_,
    item = item, status = status, reason = reason, path = NA_character_,
    stringsAsFactors = FALSE
  )
}

# ---- unit tests through app_fn() ---------------------------------------------

test_that("the tabular type list offers xpt", {
  fn <- app_fn("dta_handler_types")
  expect_identical(fn("tabular"), c("csv", "tsv", "sas7bdat", "xpt"))
  expect_identical(fn("file"), "any")
})

test_that("xpt is recognised before sas7bdat", {
  fn <- app_fn("dta_handler_type")
  expect_identical(fn(DTAtools::DTAFileXPT("a.xpt")), "xpt")
  expect_identical(fn(DTAtools::DTAFileSAS7BDAT("a.sas7bdat")), "sas7bdat")
})

test_that("a container is written to YAML and read back", {
  l <- app_fn("dta_handler_to_list")(DTAtools::DTAFileCSV("ae.csv", container = "sdtm_*.zip"))
  expect_identical(l$container, "sdtm_*.zip")
  expect_identical(do.call(DTAtools::DTAFileFactory, l)@container, "sdtm_*.zip")
  expect_null(app_fn("dta_handler_to_list")(DTAtools::DTAFileCSV("ae.csv"))$container)
})

test_that("the editor stores, clears and refuses a container", {
  fn <- app_fn("dta_set_handler")
  handlers <- app_fn("dta_handlers")
  dta <- app_fixture_dta()
  first <- function(r) handlers(DTAtools::datasets(r$value, "clinical_data"))[[1]]

  r <- fn(dta, "clinical_data",
    index = 1, filename = "clinical_data.*.csv$", type = "csv", pattern = TRUE,
    container = "  delivery.zip "
  )
  expect_true(r$ok)
  expect_identical(first(r)@container, "delivery.zip")

  r <- fn(dta, "clinical_data",
    index = 1, filename = "clinical_data.*.csv$", type = "csv", pattern = TRUE,
    container = ""
  )
  expect_true(r$ok)
  expect_null(first(r)@container)

  r <- fn(dta, "clinical_data",
    index = 1, filename = "clinical_data.*.csv$", type = "csv", pattern = TRUE,
    container = "delivery.tar"
  )
  expect_false(r$ok)
  expect_true(grepl("must name a .zip archive", flat_message(r$error), fixed = TRUE))

  r <- fn(dta, "clinical_data",
    index = 1, filename = "clinical_data.csv", type = "csv", pattern = FALSE,
    container = "deliveries/x.zip"
  )
  expect_false(r$ok)
  expect_true(grepl("not a path", flat_message(r$error), fixed = TRUE))
})

test_that("the editor prefills and lists the container", {
  r <- app_fn("dta_set_handler")(app_fixture_dta(), "clinical_data",
    index = 1, filename = "clinical_data.*.csv$", type = "csv", pattern = TRUE,
    container = "  delivery.zip "
  )
  expect_true(r$ok)
  expect_identical(app_fn("dta_handler_fields")(r$value, "clinical_data", 1)$container, "delivery.zip")
  expect_identical(app_fn("dta_handlers_overview")(r$value, "clinical_data")$container, "delivery.zip")
})

test_that("container names are recognised like the package does", {
  expect_identical(
    app_fn("dta_is_container_name")(c("a.zip", "B.XPT", "c.xpt.gz", "d.csv", "e.zip.gz")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("the replaced rows and the note are computed from the plan", {
  ae_csv <- cont_write("ae.csv", CONT_AE_CSV)
  dta <- DTAtools::load_file(cont_dta("csv"), dataset = "ae", file = ae_csv)
  plan <- rbind(
    plan_row("ae.csv", "loaded", dataset = "ae", item = "ae"),
    plan_row("dm.csv", "loaded", dataset = "dm", item = "dm"),
    plan_row("define.xml", "unused", reason = "matches no file entry of the specification"),
    plan_row("vs.csv", "unused",
      reason = "matches the file entry of dataset 'vs', which expects it inside stdm.zip"
    )
  )

  replaced <- app_fn("dta_delivery_replaced")(dta, plan)
  expect_identical(names(replaced), c("dataset", "item"))
  expect_identical(replaced$dataset, "ae")
  expect_identical(replaced$item, "ae")

  note <- app_fn("dta_delivery_note")(plan, 1L, missing = "lb.csv")
  expect_identical(
    note$text,
    "sdtm.zip: loaded 2 file(s) into 2 dataset(s) (1 replaced). Not in the delivery: lb.csv. Not used: define.xml, vs.csv (matches the file entry of dataset 'vs', which expects it inside stdm.zip). Run Check to validate."
  )
  expect_identical(note$type, "message")

  # A file refused for its size, worded for the app user.
  refused <- rbind(plan, plan_row("lb.csv", "refused",
    reason = "is larger (53 bytes) than DTAtools.container_max_member_bytes (40)"
  ))
  note <- app_fn("dta_delivery_note")(refused, 1L, missing = "lb.csv")
  expect_true(grepl("Refused: lb.csv (is larger (53 bytes) than this application accepts (40 bytes)).", note$text, fixed = TRUE))
  expect_false(grepl("DTAtools.", note$text, fixed = TRUE))
  expect_identical(note$type, "warning")

  # An archive bound as a file whose contents could not be opened.
  failed <- rbind(
    plan_row("encrypted.zip", "loaded", dataset = "docs", item = "encrypted.zip", delivered = "encrypted.zip"),
    plan_row("encrypted.zip", "opened",
      reason = "could not be opened: 'encrypted.zip' is refused: 'ae.csv' is encrypted.",
      delivered = "encrypted.zip"
    )
  )
  note <- app_fn("dta_delivery_note")(failed)
  expect_identical(
    note$text,
    "encrypted.zip: loaded 1 file(s) into 1 dataset(s). Could not open: encrypted.zip ('encrypted.zip' is refused: 'ae.csv' is encrypted.). Run Check to validate."
  )
  expect_identical(note$type, "warning")

  # A deliverable archive kept closed.
  kept <- plan_row("sdtm.zip", "loaded",
    dataset = "archive", item = "sdtm.zip",
    reason = "kept as one file: a type: any entry takes it as a deliverable; to have its files checked, name it as the container of their entries"
  )
  note <- app_fn("dta_delivery_note")(kept)
  expect_identical(
    note$text,
    "sdtm.zip: loaded 1 file(s) into 1 dataset(s). Kept closed: sdtm.zip (a deliverable; to have its files checked, name it as the container of their entries). Run Check to validate."
  )
  expect_identical(note$type, "message")
})

test_that("extracted files no record names are deleted, and then their empty directories", {
  root <- withr::local_tempdir()
  put <- function(...) {
    p <- file.path(root, ...)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines("x", p)
    p
  }
  a <- file.path(root, "dta_upload_delivery_a")
  ae <- put("dta_upload_delivery_a", "1-x.zip", "e00001", "ae.csv")
  put("dta_upload_delivery_a", "1-x.zip", "e00002", "dm.csv")
  b <- file.path(root, "dta_upload_delivery_b")
  put("dta_upload_delivery_b", "vs.csv")
  o <- file.path(root, "dta_upload_other")
  put("dta_upload_other", "lb.csv")
  uploads <- list(
    "ae||1" = list(list(file = "ae.csv", table = "ae", path = ae)),
    "x||1" = list(list(file = "x.csv", table = "x"))
  )

  app_fn("dta_delivery_cleanup")(c(a, b, o), uploads)

  expect_identical(list.files(a, recursive = TRUE), "1-x.zip/e00001/ae.csv")
  expect_false(dir.exists(b))
  expect_true(file.exists(file.path(o, "lb.csv")))
})

test_that("the session budget reads its variable", {
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "5e9")
  budget <- app_fn("dta_session_delivery_budget")
  expect_identical(budget(), 5e9)
  for (bad in c("x", "0", "-1")) {
    withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = bad)
    expect_identical(budget(), 2e10)
  }

  root <- withr::local_tempdir()
  delivery <- file.path(root, "dta_upload_delivery_t")
  staged <- file.path(root, "dta_upload_t")
  dir.create(delivery)
  dir.create(staged)
  writeBin(raw(53), file.path(delivery, "x.bin"))
  writeBin(raw(53), file.path(staged, "x.bin"))
  expect_identical(app_fn("dta_session_delivery_bytes")(c(delivery, staged)), 53)
})

# ---- server tests -------------------------------------------------------------

test_that("a zip dropped on the Delivery zone loads every dataset it feeds", {
  clean_session_file()
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))

    expect_identical(sort(names(rv$uploads)), c("ae||1", "dm||1"))
    rec <- rv$uploads[["dm||1"]][[1]]
    expect_identical(rec$table, "dm")
    expect_identical(rec$container, "sdtm_csv.zip")
    dirs <- delivery_dirs(session)
    expect_length(dirs, 1)
    expect_true(startsWith(
      normalizePath(rec$path, winslash = "/"),
      normalizePath(dirs, winslash = "/")
    ))
    expect_true(file.exists(rec$path))
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "dm"))), "dm")

    owned <- session$env$owned$upload
    staged <- owned[!startsWith(basename(owned), "dta_upload_delivery_")]
    expect_gte(length(staged), 1)
    expect_false(any(file.exists(file.path(staged, "sdtm_csv.zip"))))
  })
})

test_that("a second delivery asks before replacing, and Cancel keeps the old data and deletes the new extraction", {
  clean_session_file()
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  zip2 <- dv_copy(zip1)
  mock_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))
    session$setInputs(delivery_files = fi(zip2, "sdtm_csv.zip"))

    # The dialog: both replaced tables with their dataset, the two buttons, and
    # no way to dismiss it other than them.
    dialog <- utils::tail(modals, 1)
    expect_true(grepl("Replace loaded files?", dialog, fixed = TRUE))
    expect_true(grepl("<code>ae</code> (dataset 'ae')", dialog, fixed = TRUE))
    expect_true(grepl("<code>dm</code> (dataset 'dm')", dialog, fixed = TRUE))
    expect_true(grepl("id=\"confirm_overwrite\"", dialog, fixed = TRUE))
    expect_true(grepl("id=\"cancel_overwrite\"", dialog, fixed = TRUE))
    expect_true(grepl("data-backdrop=\"static\"", dialog, fixed = TRUE))

    expect_false(is.null(rv$pending_upload$delivery))
    x <- rv$pending_upload$delivery$exdir
    copies <- rv$pending_upload$delivery$copies
    expect_true(dir.exists(x))
    expect_length(copies, 1)
    expect_true(file.exists(copies))

    session$setInputs(cancel_overwrite = 1)
    expect_null(rv$pending_upload)
    expect_false(dir.exists(x))
    expect_false(file.exists(copies))
    expect_length(rv$uploads[["ae||1"]], 1)
  })
})

test_that("Replace commits the delivery and deletes the superseded extraction", {
  clean_session_file()
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  zip2 <- dv_copy(zip1)
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))
    first <- delivery_dirs(session)
    old <- rv$uploads[["ae||1"]][[1]]$path
    expect_true(file.exists(old))

    session$setInputs(delivery_files = fi(zip2, "sdtm_csv.zip"))
    session$setInputs(confirm_overwrite = 1)

    expect_length(rv$uploads[["ae||1"]], 1)
    expect_false(file.exists(old))
    now <- delivery_dirs(session)
    expect_length(now, 1)
    expect_false(identical(now, first))
    expect_setequal(basename(list.files(now, recursive = TRUE)), c("ae.csv", "dm.csv"))
    expect_true(grepl("(2 replaced)", utils::tail(shown, 1), fixed = TRUE))
  })
})

test_that("a hostile archive binds nothing", {
  clean_session_file()
  spec <- dv_spec()
  hostile <- cont_fx("unsafe_dotdot.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(hostile, "unsafe_dotdot.zip"))

    expect_length(rv$uploads, 0)
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "ae")), 0)
    # The session did stage the archive, and the failure took the copy away.
    staged <- staged_dirs(session)
    expect_gte(length(staged), 1)
    expect_false(any(file.exists(file.path(staged, "unsafe_dotdot.zip"))))
  })
})

test_that("a loose file for an entry that declares a container loads on its own, and a second one asks before it replaces", {
  clean_session_file()
  spec <- dv_spec_container()
  ae1 <- cont_write("ae.csv", CONT_AE_CSV)
  ae2 <- cont_write("ae.csv", CONT_AE_CSV)
  mock_notifications()
  mock_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(ae1, "ae.csv"))

    # The old gate G3b refused this; the file is the individual check.
    expect_length(rv$uploads[["ae||1"]], 1)
    expect_true(grepl(
      "Loaded ae.csv on its own for checking. The agreement requires it inside sdtm_csv.zip, so this is not yet a conforming delivery.",
      utils::tail(shown, 1),
      fixed = TRUE
    ))

    # Back to the ordinary per-slot overwrite question.
    session$setInputs(up_1_1 = fi(ae2, "ae.csv"))
    expect_false(is.null(rv$pending_upload))
    expect_true(grepl("Overwrite existing file(s)?", utils::tail(modals, 1), fixed = TRUE))
  })
})

test_that("the slot card, the dataset hint and the loaded-files list name the container", {
  clean_session_file()
  spec <- dv_spec_container()
  zip1 <- cont_fx("sdtm_csv.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))
    # The Delivery view stands in for the slots until a dataset is clicked.
    expect_identical(rv$main_view, "delivery")
    do.call(session$setInputs, stats::setNames(list(1), paste0("selds_", rv$structure[["ae"]]$index)))
    expect_identical(rv$main_view, "dataset")

    detail <- ui_text(output$dataset_detail)
    expect_true(grepl("in sdtm_csv.zip", detail, fixed = TRUE))
    expect_true(grepl(
      "Must arrive inside sdtm_csv.zip. Drop that archive on Delivery (sidebar) to load every dataset, or drop ae.csv here to check it on its own.",
      detail,
      fixed = TRUE
    ))
    expect_true(grepl("Drop or choose a file", detail, fixed = TRUE))
    expect_true(grepl(
      "Drop this dataset's files below. To load a whole delivery for every dataset, drop it on Delivery in the sidebar.",
      detail,
      fixed = TRUE
    ))
    expect_true(grepl("from sdtm_csv.zip", ui_text(output$loaded_files), fixed = TRUE))
  })
})

test_that("an archive named by one entry and declared by another fills both", {
  clean_session_file()
  spec <- dv_yaml(c(
    dv_header,
    "  - id: archive",
    "    type: file",
    "    files: {filename: sdtm.zip, type: any}",
    "  - id: define",
    "    type: file",
    "    files: {filename: define.xml, type: any, container: sdtm.zip}"
  ))
  archive <- dv_copy(cont_fx("sdtm_csv.zip"), name = "sdtm.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(archive, "sdtm.zip"))

    expect_length(rv$uploads[["archive||1"]], 1)
    expect_length(rv$uploads[["define||1"]], 1)
  })
})

test_that("a mixed drop on a slot goes through one delivery for that dataset", {
  clean_session_file()
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  dm <- cont_write("dm.csv", CONT_DM_CSV)
  lb <- cont_write("lb.csv", "")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(c(zip1, dm, lb), c("sdtm_csv.zip", "dm.csv", "lb.csv")))

    expect_null(rv$pending_upload)
    # Only ae: dm's files, inside the archive and loose, are for another dataset.
    expect_identical(names(rv$uploads), "ae||1")
    expect_true(any(grepl("Not loaded (empty or not transferred completely): lb.csv", shown, fixed = TRUE)))
    # One delivery, so one record of it; the loose file travelled with it.
    expect_identical(unique(rv$last_delivery$plan$delivered), c("sdtm_csv.zip", "dm.csv"))
    expect_true(grepl(
      "Loaded only ae's files from sdtm_csv.zip (ae.csv).",
      utils::tail(shown, 1),
      fixed = TRUE
    ))
    expect_true(grepl("judged as delivered", utils::tail(shown, 1), fixed = TRUE))
    # A slot drop does not take over the main area.
    expect_identical(rv$main_view, "dataset")
  })
})

test_that("the editor form offers xpt and the container field after the type hint", {
  clean_session_file()
  spec <- dv_spec()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)

    html <- ui_text(output$file_modal_body)
    expect_match(html, 'id="file_container"', fixed = TRUE)
    expect_match(html, "Loose or in any container", fixed = TRUE)
    expect_match(html, 'value="xpt"', fixed = TRUE)
    expect_match(html, "SAS transport (.xpt)", fixed = TRUE)
    expect_match(html, "file_type != .{1,6}xpt")
    expect_match(html, "letter case must match", fixed = TRUE)
  })

  # A file dataset has one type only, and the container field comes after the
  # sentence that says so.
  withfile <- app_fixture_path("clinical_dta_with_file_dataset.yaml")
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(withfile, "clinical_dta_with_file_dataset.yaml"))
    unlock_editing(session)
    do.call(session$setInputs, stats::setNames(list(1), paste0("selds_", rv$structure[["raw_export"]]$index)))
    expect_identical(rv$active, "raw_export")
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)

    html <- ui_text(output$file_modal_body)
    expect_gt(regexpr("cannot be changed", html), 0)
    expect_lt(regexpr("cannot be changed", html), regexpr('id="file_container"', html))
  })
})

test_that("the editor shows a stored container and keeps it when the field did not report", {
  clean_session_file()
  spec <- dv_spec_container("sdtm.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    # A select now: the stored file name is an implied container, offered and selected.
    expect_true(grepl('<option value="sdtm.zip" selected>sdtm.zip</option>', ui_text(output$file_modal_body), fixed = TRUE))

    # Every input the form renders except the container field.
    session$setInputs(
      file_filename = "ae2.csv", file_type = "csv", file_pattern = FALSE,
      file_count_mode = "exact", file_number_of_files = 1,
      file_pattern_description = "", file_info = "", file_extensions = ""
    )
    session$setInputs(file_save = 1)
    expect_null(rv$file_msg)
    handler <- DTAtools::datasets(rv$dta, "ae")@files[[1]]
    expect_identical(handler@filename, "ae2.csv")
    expect_identical(handler@container, "sdtm.zip")

    # An explicitly emptied field clears it. A repeated value does not fire, so
    # the edit click goes through another value first.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "")
    session$setInputs(file_save = 2)
    expect_null(rv$file_msg)
    expect_null(DTAtools::datasets(rv$dta, "ae")@files[[1]]@container)
  })
})

# ---- limits, cleanup and budget -------------------------------------------------

test_that("dta_load_delivery() applies the app limits for its call only", {
  withr::local_options(
    DTAtools.container_max_bytes = NULL, DTAtools.container_max_member_bytes = NULL,
    DTAtools.container_max_members = NULL, DTAtools.container_max_ratio = NULL
  )
  exdir <- withr::local_tempdir()

  res <- app_fn("dta_load_delivery")(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir, bytes_left = 60)

  expect_false(isTRUE(res$ok))
  # 83 bytes against 60: bytes_left reached the call.
  expect_true(grepl("container_max_bytes", flat_message(res$error), fixed = TRUE))
  expect_null(getOption("DTAtools.container_max_bytes"))
  expect_null(getOption("DTAtools.container_max_member_bytes"))
})

test_that("limit refusals are worded for app users, and nothing else is lost", {
  reason <- app_fn("dta_app_reason")
  txt <- paste(
    "'b.zip' is refused: it exceeds `DTAtools.container_max_bytes`.",
    "x 83 bytes against a limit of 60 bytes.",
    "i Raise the limit with `options(DTAtools.container_max_bytes = )` in R if this delivery is genuine.",
    sep = "\n"
  )
  expect_identical(
    reason(txt),
    "'b.zip' is refused: it is larger than this application accepts; deliver it in smaller parts or ask the application's administrator to raise the limit. x 83 bytes against a limit of 60 bytes."
  )
  expect_identical(
    reason(txt, budget_limited = TRUE),
    "'b.zip' is refused: it is larger than the room this session has left for extracted deliveries; remove or discard files loaded from a delivery and drop again, or use Start over. x 83 bytes against a limit of 60 bytes."
  )
  # The session budget bounds only the total, and one file cannot be delivered
  # in parts.
  expect_true(startsWith(
    reason(gsub("max_bytes", "max_member_bytes", txt), budget_limited = TRUE),
    "'b.zip' is refused: it is larger than this application accepts for one file; load it in R with load_delivery()"
  ))
  expect_true(startsWith(
    reason(gsub("max_bytes", "max_ratio", txt)),
    "'b.zip' is refused: it is compressed far more than any genuine delivery and is refused as unsafe."
  ))
  expect_identical(reason("matches no file entry of the specification"), "matches no file entry of the specification")
  expect_identical(reason("line one\nline two"), "line one\nline two")

  # A real message, cut by cli across lines.
  withr::local_options(DTAtools.container_max_bytes = 47)
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  e <- suppressWarnings(tryCatch(
    load_delivery(cont_dta("csv"), zipf, on_error = "skip"),
    dta_delivery_nothing_loaded = identity
  ))
  expect_s3_class(e, "dta_delivery_nothing_loaded")
  out <- reason(conditionMessage(e), TRUE)
  expect_true(grepl("remove or discard", out, fixed = TRUE))
  expect_true(grepl("83 bytes against a limit of 47 bytes", out, fixed = TRUE))
  expect_false(grepl("DTAtools.", out, fixed = TRUE))
  expect_false(grepl("Raise the limit", out, fixed = TRUE))
})

test_that("the uploads of file datasets are picked out", {
  dta <- DTAtools::read_dta_from_yaml(app_fixture_path("clinical_dta_with_file_dataset.yaml"))
  uploads <- list(
    "clinical_data||1" = list(list(table = "a")),
    "raw_export||1" = list(list(table = "b"))
  )
  expect_identical(names(app_fn("dta_file_dataset_uploads")(dta, uploads)), "raw_export||1")
})

test_that("removing or discarding a delivery's files frees the session budget", {
  clean_session_file()
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "100")
  spec <- dv_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a1, "a.zip"))
    expect_length(delivery_dirs(session), 1)

    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    expect_length(delivery_dirs(session), 0)

    session$setInputs(up_1_1 = fi(a2, "a.zip"))
    expect_length(rv$uploads[["ae||1"]], 1)
    expect_length(delivery_dirs(session), 1)
  })
})

test_that("discarding one dataset a shared delivery filled frees exactly its files", {
  clean_session_file()
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "100")
  spec <- dv_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b1 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  b2 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a1, "a.zip"))
    # up_2_1 is dm's slot; in a browser it exists only while dm is the active
    # dataset, testServer() accepts it regardless.
    session$setInputs(delivery_files = fi(b1, "b.zip"))
    # 83 bytes against the 47 left.
    expect_identical(names(rv$uploads), "ae||1")
    expect_true(grepl("remove or discard", utils::tail(shown, 1), fixed = TRUE))

    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    session$setInputs(delivery_files = fi(b2, "b.zip"))
    expect_identical(sort(names(rv$uploads)), c("ae||1", "dm||1"))
    ae_file <- rv$uploads[["ae||1"]][[1]]$path
    dm_file <- rv$uploads[["dm||1"]][[1]]$path
    expect_identical(delivery_root(ae_file), delivery_root(dm_file))

    session$setInputs(discard_all = 2)
    session$setInputs(confirm_discard_all = 2)
    expect_false(file.exists(ae_file))
    expect_true(file.exists(dm_file))

    # 70 bytes are free once ae's 53 are.
    session$setInputs(up_1_1 = fi(a2, "a.zip"))
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))), "ae")
  })
})

test_that("session end keeps only the files a file dataset uses", {
  clean_session_file()
  spec <- dv_yaml(c(
    dv_header, dv_ae(),
    "  - id: define",
    "    type: file",
    "    files: {filename: define.xml, type: any}"
  ))
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, define.xml = "<define/>\n"), name = "d.zip")
  ae_path <- NULL
  define_path <- NULL
  withr::defer(unlink(delivery_root(define_path), recursive = TRUE))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "d.zip"))
    ae_path <<- rv$uploads[["ae||1"]][[1]]$path
    define_path <<- rv$uploads[["define||1"]][[1]]$path
    expect_true(file.exists(ae_path))
    expect_true(file.exists(define_path))
    session$close()
  })

  expect_false(file.exists(ae_path))
  expect_true(file.exists(define_path))
})

test_that("an extraction orphaned by removing its dataset is deleted before the next delivery is budgeted", {
  clean_session_file()
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "100")
  spec <- dv_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b <- cont_zip(
    list(dm.csv = paste0(CONT_DM_CSV, "S-003,40\nS-004,22\nS-005,63\n")),
    name = "b.zip"
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a, "a.zip"))
    unlock_editing(session)
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)
    expect_identical(names(rv$structure), "dm")

    # 57 bytes: more than the 47 left while ae's file still counts.
    session$setInputs(up_1_1 = fi(b, "b.zip"))
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "dm"))), "dm")
  })
})

test_that("removing the one file of a delivery deletes its extraction", {
  clean_session_file()
  spec <- dv_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a, "a.zip"))
    expect_length(delivery_dirs(session), 1)

    session$setInputs(rmfile_1 = 1)
    expect_length(rv$uploads[["ae||1"]], 0)
    expect_length(delivery_dirs(session), 0)
  })
})

test_that("a session at its budget refuses with the app sentence, never an R option", {
  clean_session_file()
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "53")
  spec <- dv_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b <- cont_zip(list(dm.csv = CONT_DM_CSV), name = "b.zip")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a, "a.zip"))
    session$setInputs(up_1_1 = fi(b, "b.zip"))

    expect_true(grepl("already holds the maximum", utils::tail(shown, 1), fixed = TRUE))
    expect_false(any(grepl("options(", shown, fixed = TRUE)))
  })
})

test_that("a delivery over validated datasets marks every dataset it fed not validated", {
  clean_session_file()
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  zip2 <- dv_copy(zip1)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))
    session$setInputs(check_all = 1)
    expect_identical(rv$status[["ae"]], "pass")
    expect_identical(rv$status[["dm"]], "pass")

    session$setInputs(delivery_files = fi(zip2, "sdtm_csv.zip"))
    session$setInputs(confirm_overwrite = 1)
    expect_identical(rv$status[["ae"]], "pending")
    expect_identical(rv$status[["dm"]], "pending")
  })
})

test_that("the app's limits are the ones inside the call", {
  withr::local_options(
    DTAtools.container_max_bytes = NULL, DTAtools.container_max_member_bytes = NULL,
    DTAtools.container_max_members = NULL, DTAtools.container_max_ratio = NULL
  )
  testthat::local_mocked_bindings(
    load_delivery = function(...) {
      list(getOption("DTAtools.container_max_bytes"), getOption("DTAtools.container_max_member_bytes"))
    },
    .package = "DTAtools"
  )
  fn <- app_fn("dta_load_delivery")

  expect_identical(fn(cont_dta("csv"), "x.zip", tempdir(), bytes_left = 5e9)$value, list(5e9, 2e9))
  expect_identical(fn(cont_dta("csv"), "x.zip", tempdir(), bytes_left = 3e10)$value, list(1e10, 2e9))
})

test_that("an administrator's member cap is the one inside the call", {
  withr::local_options(
    DTAtools.container_max_bytes = NULL, DTAtools.container_max_member_bytes = 5e9,
    DTAtools.container_max_members = NULL, DTAtools.container_max_ratio = NULL
  )
  testthat::local_mocked_bindings(
    load_delivery = function(...) {
      list(getOption("DTAtools.container_max_bytes"), getOption("DTAtools.container_max_member_bytes"))
    },
    .package = "DTAtools"
  )

  expect_identical(
    app_fn("dta_load_delivery")(cont_dta("csv"), "x.zip", tempdir(), bytes_left = 3e9)$value,
    list(3e9, 5e9)
  )
})

test_that("a delivery that fails after extraction began leaves no directory", {
  clean_session_file()
  spec <- dv_spec()
  lying <- cont_fx("lying_size.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(lying, "lying_size.zip"))

    expect_length(rv$uploads, 0)
    expect_length(delivery_dirs(session), 0)
    staged <- staged_dirs(session)
    expect_gte(length(staged), 1)
    expect_false(any(file.exists(file.path(staged, "lying_size.zip"))))
  })
})

# ---- review follow-ups: ownership, cleanup, wording, restore ---------------------

test_that("the session budget is named as the limit only when the session holds something", {
  withr::local_options(
    DTAtools.container_max_bytes = NULL, DTAtools.container_max_member_bytes = NULL,
    DTAtools.container_max_members = NULL, DTAtools.container_max_ratio = NULL
  )
  fn <- app_fn("dta_load_delivery")
  reason <- app_fn("dta_app_reason")
  zipf <- cont_fx("sdtm_csv.zip")

  # An empty session whose administrator set a small budget: nothing to remove,
  # so the refusal names the administrator's limit.
  empty <- fn(cont_dta("csv"), zipf, withr::local_tempdir(), bytes_left = 60)
  expect_false(isTRUE(empty$ok))
  expect_false(empty$budget_limited)
  said <- reason(empty$error, empty$budget_limited)
  expect_true(grepl("ask the application's administrator", said, fixed = TRUE))
  expect_false(grepl("remove or discard", said, fixed = TRUE))

  # A session holding extracted files: freeing them is the remedy.
  held <- fn(cont_dta("csv"), zipf, withr::local_tempdir(), bytes_left = 60, held = 40)
  expect_false(isTRUE(held$ok))
  expect_true(held$budget_limited)
  expect_true(grepl("remove or discard", reason(held$error, held$budget_limited), fixed = TRUE))

  # Room left, and the app's own cap is the smaller limit: not the budget.
  roomy <- fn(cont_dta("csv"), zipf, withr::local_tempdir(), bytes_left = 3e10, held = 40)
  expect_true(isTRUE(roomy$ok))
  expect_false(roomy$budget_limited)
})

test_that("a drop refused by a small budget in an empty session names the administrator", {
  clean_session_file()
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "60")
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))

    expect_length(rv$uploads, 0)
    last <- utils::tail(shown, 1)
    expect_true(grepl("Delivery not loaded", last, fixed = TRUE))
    expect_true(grepl("ask the application's administrator", last, fixed = TRUE))
    expect_false(grepl("remove or discard", last, fixed = TRUE))
  })
})

test_that("a refusal by the entry count says files, not size", {
  txt <- paste(
    "'b.zip' is refused: it exceeds `DTAtools.container_max_members`.",
    "x 7 entries or files against a limit of 1.",
    "i Raise the limit with `options(DTAtools.container_max_members = )` in R if this delivery is genuine.",
    sep = "\n"
  )
  expect_identical(
    app_fn("dta_app_reason")(txt),
    "'b.zip' is refused: it holds more files than this application accepts; deliver it in smaller parts or ask the application's administrator to raise the limit. x 7 entries or files against a limit of 1."
  )
})

test_that("the delivery directories of the upload records are found for a restored session", {
  base <- withr::local_tempdir()
  other <- withr::local_tempdir()
  put <- function(root, ...) {
    p <- file.path(root, ...)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines("x", p)
    p
  }
  uploads <- list(
    "ae||1" = list(list(file = "ae.csv", table = "ae", path = put(base, "dta_upload_delivery_a", "1-x.zip", "e00001", "ae.csv"))),
    "dm||1" = list(
      list(file = "dm.csv", table = "dm", path = put(base, "dta_upload_delivery_b", "dm.csv")),
      list(file = "x.csv", table = "x")
    ),
    "vs||1" = list(list(file = "vs.csv", table = "vs", path = put(base, "dta_upload_c", "vs.csv"))),
    "lb||1" = list(list(file = "lb.csv", table = "lb", path = put(other, "dta_upload_delivery_d", "lb.csv")))
  )
  roots <- app_fn("dta_delivery_roots")(uploads, base = base)

  # Delivery directories directly below `base` only: not a staged upload, not a
  # directory somewhere else.
  expect_setequal(basename(roots), c("dta_upload_delivery_a", "dta_upload_delivery_b"))
  expect_identical(app_fn("dta_delivery_roots")(list(), base = base), character(0))
})

test_that("a restored session owns the extracted files it re-reads", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  spec <- dv_yaml(c(
    dv_header, dv_ae(),
    "  - id: define",
    "    type: file",
    "    files: {filename: define.xml, type: any}"
  ))
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, define.xml = "<define/>\n"), name = "d.zip")
  id <- strrep("9", 32)
  define_path <- NULL
  withr::defer(unlink(delivery_root(define_path), recursive = TRUE))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "d.zip"))
    define_path <<- rv$uploads[["define||1"]][[1]]$path
    session$close()
  })
  # Kept at session end for the restore.
  expect_true(file.exists(define_path))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    expect_length(delivery_dirs(session), 0)
    session$setInputs(restore_session = 1)

    expect_true("define||1" %in% names(rv$uploads))
    owned_now <- delivery_dirs(session)
    expect_length(owned_now, 1)
    expect_identical(
      normalizePath(owned_now, winslash = "/"),
      normalizePath(delivery_root(define_path), winslash = "/")
    )
  })
})

test_that("a bound archive's staged copy survives the commit", {
  clean_session_file()
  spec <- dv_yaml(c(
    dv_header,
    "  - id: archive",
    "    type: file",
    "    files: {filename: sdtm.zip, type: any}",
    "  - id: define",
    "    type: file",
    "    files: {filename: define.xml, type: any, container: sdtm.zip}"
  ))
  archive <- dv_copy(cont_fx("sdtm_csv.zip"), name = "sdtm.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(archive, "sdtm.zip"))

    bound <- DTAtools::datasets(rv$dta, "archive")@file_paths
    expect_length(bound, 1)
    expect_identical(basename(bound), "sdtm.zip")
    expect_true(file.exists(bound))
  })
})

test_that("a file bound into two datasets survives discarding one of them", {
  clean_session_file()
  spec <- dv_yaml(c(
    dv_header, dv_ae(),
    sub("id: ae", "id: ae2", dv_ae(), fixed = TRUE)
  ))
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(a, "a.zip"))
    shared <- rv$uploads[["ae2||1"]][[1]]$path
    expect_identical(rv$uploads[["ae||1"]][[1]]$path, shared)
    expect_true(file.exists(shared))

    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    expect_length(rv$uploads[["ae||1"]], 0)
    expect_true(file.exists(shared))
    expect_length(rv$uploads[["ae2||1"]], 1)

    # Once the last record naming it is gone, so is the file.
    do.call(session$setInputs, stats::setNames(list(1), paste0("selds_", rv$structure[["ae2"]]$index)))
    expect_identical(rv$active, "ae2")
    session$setInputs(discard_all = 2)
    session$setInputs(confirm_discard_all = 2)
    expect_false(file.exists(shared))
  })
})

test_that("session end deletes a delivery that waits in the Replace dialog", {
  clean_session_file()
  spec <- dv_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  pending_dir <- NULL
  pending_copies <- NULL

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a1, "a.zip"))
    session$setInputs(up_1_1 = fi(a2, "a.zip"))
    expect_false(is.null(rv$pending_upload$delivery))
    pending_dir <<- rv$pending_upload$delivery$exdir
    pending_copies <<- rv$pending_upload$delivery$copies
    expect_true(dir.exists(pending_dir))
    expect_true(all(file.exists(pending_copies)))
    session$close()
  })

  expect_length(pending_copies, 1)
  expect_false(dir.exists(pending_dir))
  expect_false(any(file.exists(pending_copies)))
})

test_that("a committed delivery is in the autosave", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  spec <- dv_spec()
  zip1 <- cont_fx("sdtm_csv.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("9", 32))
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zip1, "sdtm_csv.zip"))

    f <- list.files(tempdir(), pattern = "^dtatools_app_session.*[.]rds$", full.names = TRUE)
    expect_length(f, 1)
    saved <- readRDS(f)$uploads
    expect_setequal(names(saved), c("ae||1", "dm||1"))
    expect_identical(saved[["dm||1"]][[1]]$container, "sdtm_csv.zip")
  })
})

test_that("a loose file loaded beside an archive has no container", {
  clean_session_file()
  spec <- dv_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  dm <- cont_write("dm.csv", CONT_DM_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(c(a, dm), c("a.zip", "dm.csv")))

    expect_setequal(names(rv$uploads), c("ae||1", "dm||1"))
    expect_identical(rv$uploads[["ae||1"]][[1]]$container, "a.zip")
    expect_null(rv$uploads[["dm||1"]][[1]]$container)

    do.call(session$setInputs, stats::setNames(list(1), paste0("selds_", rv$structure[["dm"]]$index)))
    expect_identical(rv$active, "dm")
    html <- ui_text(output$loaded_files)
    expect_true(grepl("dm.csv", html, fixed = TRUE))
    expect_false(grepl("from ", html, fixed = TRUE))
  })
})

test_that("the note lists at most five names and counts the rest", {
  plan <- rbind(
    plan_row("ae.csv", "loaded", dataset = "ae", item = "ae"),
    plan_row(paste0("u", 1:7, ".txt"), "unused", reason = "matches no file entry of the specification")
  )
  note <- app_fn("dta_delivery_note")(plan)
  expect_true(grepl("Not used: u1.txt, u2.txt, u3.txt, u4.txt, u5.txt, and 2 more.", note$text, fixed = TRUE))
  expect_false(grepl("u6.txt", note$text, fixed = TRUE))
})

test_that("the container check reads the slot's own entry, not the first one", {
  clean_session_file()
  spec <- dv_yaml(c(
    dv_header,
    "  - id: ae",
    "    type: tabular",
    "    files:",
    "      - {filename: ae.csv, type: csv}",
    "      - {filename: ae2.csv, type: csv, container: a.zip}",
    dv_ae()[4:7]
  ))
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ae2 <- cont_write("ae2.csv", CONT_AE_CSV)
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(ae, "ae.csv"))
    expect_length(rv$uploads[["ae||1"]], 1)
    # Entry 1 declares no container: the plain toast.
    expect_true(grepl("Loaded 1 file(s) into 'ae'", utils::tail(shown, 1), fixed = TRUE))

    # Entry 2 does: the loose file is its individual check, and the toast names
    # entry 2's container, not entry 1's (it has none).
    session$setInputs(up_1_2 = fi(ae2, "ae2.csv"))
    expect_length(rv$uploads[["ae||2"]], 1)
    expect_true(grepl("Loaded ae2.csv on its own for checking", utils::tail(shown, 1), fixed = TRUE))
    expect_true(grepl("requires it inside a.zip", utils::tail(shown, 1), fixed = TRUE))
  })
})

test_that("the note names what a declared container did not hold", {
  clean_session_file()
  dm <- dv_dm
  dm[3] <- "    files: {filename: dm.csv, type: csv, container: a.zip}"
  spec <- dv_yaml(c(dv_header, dv_ae("{filename: ae.csv, type: csv, container: a.zip}"), dm))
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(a, "a.zip"))

    expect_identical(names(rv$uploads), "ae||1")
    expect_true(grepl("Not in the delivery: dm.csv", utils::tail(shown, 1), fixed = TRUE))
  })
})

test_that("a loose upload that replaces a delivered file deletes the extraction", {
  clean_session_file()
  spec <- dv_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  ae <- cont_write("ae.csv", CONT_AE_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a, "a.zip"))
    old <- rv$uploads[["ae||1"]][[1]]$path
    expect_true(file.exists(old))

    # The per-slot flow asks first, and replaces the delivered record.
    session$setInputs(up_1_1 = fi(ae, "ae.csv"))
    session$setInputs(confirm_overwrite = 1)
    expect_null(rv$uploads[["ae||1"]][[1]]$container)
    expect_false(file.exists(old))
    expect_length(delivery_dirs(session), 0)
  })
})

test_that("the Replace dialog shows table names as text and names the dataset", {
  clean_session_file()
  spec <- dv_yaml(c(
    dv_header,
    "  - id: ae",
    "    type: tabular",
    "    files: {filename: \"^ae.*[.]csv$\", type: csv, pattern: true}",
    dv_ae()[4:7]
  ))
  # A table name that is itself an HTML entity: shown as written, never decoded.
  h1 <- cont_zip(list("ae&amp;1.csv" = CONT_AE_CSV), name = "h1.zip")
  h2 <- cont_zip(list("ae&amp;1.csv" = CONT_AE_CSV), name = "h2.zip")
  mock_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(h1, "h1.zip"))
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))), "ae&amp;1")
    session$setInputs(up_1_1 = fi(h2, "h2.zip"))

    dialog <- utils::tail(modals, 1)
    expect_true(grepl("<code>ae&amp;amp;1</code> (dataset 'ae')", dialog, fixed = TRUE))
    expect_false(grepl("<code>ae&amp;1</code>", dialog, fixed = TRUE))
  })
})

# ---- Two tabs of one browser (red-team D1) ------------------------------------
# 'Restore previous session' in a second tab makes both sessions own the same
# delivery directory; either one's cleanup used to delete files the other still
# reads. testServer() cannot nest, so tab B is the app's server function run in
# a second MockShinySession, exactly as testServer() itself drives one.
# `first` is the first tab's session. Each source of the app gets its own copy
# of the live-session registry, while real tabs share one R process and so one
# registry: the second tab is pointed at the first's.
second_tab <- function(client_id, first) {
  srv <- shiny::shinyAppDir(app_server_dir())$serverFuncSource()
  holder <- function(e) {
    while (!exists(".dta_live_session_paths", envir = e, inherits = FALSE)) e <- parent.env(e)
    e
  }
  assign(".dta_live_session_paths", get(".dta_live_session_paths", envir = holder(first$env)),
    envir = holder(environment(srv))
  )
  s <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s, srv(input = s$input, output = s$output, session = s))
  s$setInputs(dta_client_id = client_id)
  s$setInputs(restore_session = 1)
  s
}

test_that("a second tab that restores the session does not lose the first tab's streamed table", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  # Stand-in for a member over the 512 MB streaming threshold.
  withr::local_options(DTAtools.stream_threshold = 10)
  spec <- dv_spec()
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  id <- strrep("a", 32)
  root <- NULL
  withr::defer(unlink(root, recursive = TRUE))
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "sdtm.zip"))
    ae_path <- rv$uploads[["ae||1"]][[1]]$path
    root <<- delivery_root(ae_path)
    expect_true(file.exists(ae_path))

    tab_b <- second_tab(id, session)
    tab_b$close()
    expect_true(file.exists(ae_path))
    session$setInputs(check_all = 1)
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))
    expect_true(file.exists(ae_path))
  })
})

test_that("a second tab that restores the session does not lose the first tab's file dataset", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  spec <- dv_yaml(c(
    dv_header, dv_ae(), dv_dm,
    "  - id: define",
    "    type: file",
    "    files: {filename: define.xml, type: any}"
  ))
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV, define.xml = "<define/>\n"), name = "sdtm.zip")
  id <- strrep("b", 32)
  root <- NULL
  withr::defer(unlink(root, recursive = TRUE))
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "sdtm.zip"))
    def_path <- rv$uploads[["define||1"]][[1]]$path
    root <<- delivery_root(def_path)

    tab_b <- second_tab(id, session)
    tab_b$setInputs(check_all = 1)
    # Tab A drops its own define.xml; tab B still binds the same file.
    fid <- get_file_id("define", 1L, "define.xml")
    do.call(session$setInputs, stats::setNames(list(1), paste0("rmfile_", fid)))
    expect_true(file.exists(def_path))
    tab_b$close()
  })
})

test_that("without a second tab, removing the delivery's last file still deletes it", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  spec <- dv_yaml(c(
    dv_header, dv_ae(),
    "  - id: define",
    "    type: file",
    "    files: {filename: define.xml, type: any}"
  ))
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, define.xml = "<define/>\n"), name = "sdtm.zip")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("c", 32))
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "sdtm.zip"))
    def_path <- rv$uploads[["define||1"]][[1]]$path
    fid <- get_file_id("define", 1L, "define.xml")
    do.call(session$setInputs, stats::setNames(list(1), paste0("rmfile_", fid)))
    expect_false(file.exists(def_path))
  })
})

# ---- The slot maximum (red-team D3, D4) ---------------------------------------
dv_max1 <- function(env = parent.frame()) {
  dv_yaml(c(
    dv_header,
    dv_ae("{filename: 'ae_.*\\.csv$', type: csv, pattern: true, min_number_of_files: 1, max_number_of_files: 1}"),
    dv_dm
  ), env)
}

test_that("a stray file named like a bound table does not get around the slot maximum", {
  clean_session_file()
  spec <- dv_max1()
  a1 <- cont_write("ae_1.csv", CONT_AE_CSV)
  a2 <- cont_write("ae_2.csv", CONT_AE_CSV)
  stray <- cont_write("ae_1.tsv", CONT_AE_CSV)
  mock_notifications()
  mock_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(a1, "ae_1.csv"))
    session$setInputs(up_1_1 = rbind(fi(a2, "ae_2.csv"), fi(stray, "ae_1.tsv")))
    expect_length(modals, 0) # no "Overwrite ae_1?" for a file that cannot bind
    expect_true(grepl("accepts at most 1 file(s)", utils::tail(shown, 1), fixed = TRUE))
    expect_identical(vapply(rv$uploads[["ae||1"]], `[[`, "", "file"), "ae_1.csv")
  })
})

test_that("a delivery that overfills a slot says so in its note", {
  clean_session_file()
  spec <- dv_max1()
  z <- cont_zip(
    list("ae_1.csv" = CONT_AE_CSV, "ae_2.csv" = CONT_AE_CSV, "ae_3.csv" = CONT_AE_CSV),
    name = "ae.zip"
  )
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = fi(z, "ae.zip"))
    note <- utils::tail(shown, 1)
    expect_true(grepl("ae (3 files, at most 1)", note, fixed = TRUE))
  })

  # The helper, on a document whose slot accepts one file.
  dta <- DTAtools::DTA(
    datasets = list(DTAtools::DTADataSetTabular(
      id = "ae", specs = cont_specs("ae"),
      files = list(DTAtools::DTAFileCSV(filename = "ae_.*[.]csv$", pattern = TRUE, max_number_of_files = 1))
    )),
    metadata = DTAtools::create_example_DTAMetaData()
  )
  recs <- function(n) lapply(seq_len(n), function(i) list(file = paste0("ae_", i, ".csv"), table = paste0("ae_", i)))
  over <- app_fn("dta_slots_over_max")
  expect_identical(over(dta, list("ae||1" = recs(1))), character(0))
  expect_identical(over(dta, list("ae||1" = recs(3))), "ae (3 files, at most 1)")
  note <- app_fn("dta_delivery_note")(plan_row("ae.csv", "loaded", dataset = "ae", item = "ae"), over = "ae (3 files, at most 1)")
  expect_identical(note$type, "warning")
  expect_true(grepl("ae (3 files, at most 1)", note$text, fixed = TRUE))
})

# ---- Restoring a streamed delivery table ----------------------------------------
# A table held as a lazy Arrow dataset over the extracted CSV must survive the
# session dump: the extracted files are deleted when the session ends.
test_that("a streamed delivery table comes back after the session ends and is restored", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  withr::local_options(DTAtools.stream_threshold = 10)
  spec <- dv_spec()
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  id <- strrep("d", 32)
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "sdtm.zip"))
    session$setInputs(check_all = 1) # an autosave follows the delivery
    session$close()
  })

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    ae <- DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))[["ae"]]
    expect_false(is.null(ae))
    expect_identical(sort(as.data.frame(ae)$USUBJID), c("S-001", "S-002"))
  })
})

# ---- file_handlers stays in step with file_paths (C2) ---------------------------
test_that("removing one file of a file dataset keeps the other entry's recorded handler", {
  dir <- withr::local_tempdir()
  for (f in c("a.pdf", "b.txt")) writeLines("x", file.path(dir, f))
  ds <- DTAtools::DTADataSetFile(id = "docs", files = list(
    DTAtools::DTAFileAny("[.]pdf$", pattern = TRUE),
    DTAtools::DTAFileAny("[.]txt$", pattern = TRUE)
  ))
  ds <- DTAtools::load_file(ds, file = file.path(dir, "a.pdf"), handler_index = 1)
  ds <- DTAtools::load_file(ds, file = file.path(dir, "b.txt"), handler_index = 2)
  expect_identical(ds@file_handlers, c(1L, 2L))
  dta <- DTAtools::DTA(datasets = list(ds), metadata = DTAtools::create_example_DTAMetaData())

  out <- app_fn("dta_unload_table")(dta, "docs", "a.pdf")$value
  d2 <- DTAtools::datasets(out, "docs")
  expect_identical(basename(d2@file_paths), "b.txt")
  expect_identical(d2@file_handlers, 2L)

  # Out of step: the record is dropped, never misaligned.
  ds@file_handlers <- integer(0)
  dta@datasets[["docs"]] <- ds
  d3 <- DTAtools::datasets(app_fn("dta_unload_table")(dta, "docs", "a.pdf")$value, "docs")
  expect_identical(d3@file_handlers, integer(0))

  # In step again: unloading everything must reset the record, not keep it.
  in_step <- DTAtools::load_file(DTAtools::DTADataSetFile(id = "docs", files = list(
    DTAtools::DTAFileAny("[.]pdf$", pattern = TRUE)
  )), file = file.path(dir, "a.pdf"), handler_index = 1)
  expect_identical(in_step@file_handlers, 1L)
  dta@datasets[["docs"]] <- in_step
  d4 <- DTAtools::datasets(app_fn("dta_unload_all")(dta, "docs")$value, "docs")
  expect_identical(d4@file_handlers, integer(0))
})

# ---- file_handlers follows the handlers when they move (round 2) -----------------
# A pdf entry wanting exactly 2 files and a txt entry wanting exactly 1: if the
# recorded indices still point at the old positions, check() counts the files
# under the wrong entry and reports a count that is not there.
fh_fixture <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  for (f in c("a1.pdf", "a2.pdf", "b.txt")) writeLines("x", file.path(dir, f))
  pdf <- DTAtools::DTAFileAny("[.]pdf$", pattern = TRUE, min_number_of_files = 2, max_number_of_files = 2)
  txt <- DTAtools::DTAFileAny("[.]txt$", pattern = TRUE, min_number_of_files = 1, max_number_of_files = 1)
  list(dir = dir, pdf = pdf, txt = txt)
}
fh_load <- function(ds, dir, ...) {
  at <- c(...) # handler index for a1.pdf, a2.pdf, b.txt
  for (i in 1:3) {
    ds <- DTAtools::load_file(ds, file = file.path(dir, c("a1.pdf", "a2.pdf", "b.txt")[i]), handler_index = at[i])
  }
  ds
}
fh_dta <- function(ds) DTAtools::DTA(datasets = list(ds), metadata = DTAtools::create_example_DTAMetaData())
fh_messages <- function(dta) {
  x <- DTAtools::check(DTAtools::datasets(dta, "docs"), persist = FALSE, quiet = TRUE)
  m <- as.data.frame(DTAtools::messages(x))
  if (nrow(m)) unique(as.character(m[[intersect(c("message", "msg", "text"), names(m))[1]]])) else character(0)
}

test_that("moving a handler moves the recorded handler of its files with it", {
  fx <- fh_fixture()
  ds <- fh_load(DTAtools::DTADataSetFile(id = "docs", files = list(fx$pdf, fx$txt)), fx$dir, 1, 1, 2)
  dta <- fh_dta(ds)
  expect_identical(fh_messages(dta), character(0))

  moved <- app_fn("dta_move_handler")(dta, "docs", 1, "down")$value
  expect_identical(DTAtools::datasets(moved, "docs")@file_handlers, c(2L, 2L, 1L))
  expect_identical(fh_messages(moved), character(0))
})

test_that("removing an empty first handler shifts the recorded handlers of the rest", {
  fx <- fh_fixture()
  empty <- DTAtools::DTAFileAny("[.]doc$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 1)
  ds <- fh_load(DTAtools::DTADataSetFile(id = "docs", files = list(empty, fx$pdf, fx$txt)), fx$dir, 2, 2, 3)
  dta <- fh_dta(ds)
  expect_identical(fh_messages(dta), character(0))

  removed <- app_fn("dta_remove_handler")(dta, "docs", 1)$value
  expect_identical(DTAtools::datasets(removed, "docs")@file_handlers, c(1L, 1L, 2L))
  expect_identical(fh_messages(removed), character(0))
})

test_that("removing a handler that holds files leaves those files unknown, not misattributed", {
  fx <- fh_fixture()
  ds <- fh_load(DTAtools::DTADataSetFile(id = "docs", files = list(fx$pdf, fx$txt)), fx$dir, 1, 1, 2)
  removed <- app_fn("dta_remove_handler")(fh_dta(ds), "docs", 1)$value
  expect_identical(DTAtools::datasets(removed, "docs")@file_handlers, c(NA, NA, 1L))
})

test_that("applying a YAML that reorders `files:` re-keys the recorded handlers", {
  fx <- fh_fixture()
  old_ds <- fh_load(DTAtools::DTADataSetFile(id = "docs", files = list(fx$pdf, fx$txt)), fx$dir, 1, 1, 2)
  parsed <- fh_dta(DTAtools::DTADataSetFile(id = "docs", files = list(fx$txt, fx$pdf)))

  tr <- app_fn("dta_transfer_bound_data")(parsed, "docs", old_ds, keep_validation = FALSE)$value
  expect_identical(DTAtools::datasets(tr, "docs")@file_handlers, c(2L, 2L, 1L))
  expect_identical(fh_messages(tr), character(0))

  # A handler that no longer exists in the YAML leaves its files unknown.
  gone <- fh_dta(DTAtools::DTADataSetFile(id = "docs", files = list(fx$txt)))
  tr2 <- app_fn("dta_transfer_bound_data")(gone, "docs", old_ds, keep_validation = FALSE)$value
  expect_identical(DTAtools::datasets(tr2, "docs")@file_handlers, c(NA, NA, 1L))
})

# ---- Start over in one tab leaves the other tab's delivery alone ------------------
test_that("Start over in a second tab does not delete the files the first tab streams", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  withr::local_options(DTAtools.stream_threshold = 10)
  spec <- dv_spec()
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  id <- strrep("e", 32)
  root <- NULL
  withr::defer(unlink(root, recursive = TRUE))
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "sdtm.zip"))
    ae_path <- rv$uploads[["ae||1"]][[1]]$path
    root <<- delivery_root(ae_path)

    tab_b <- second_tab(id, session)
    tab_b$setInputs(confirm_reset = 1)
    expect_true(file.exists(ae_path))
    session$setInputs(check_all = 1)
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))
    tab_b$close()
  })
})

test_that("without a second tab, Start over still deletes the delivery", {
  clean_session_file()
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE))
  spec <- dv_spec()
  zipf <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  mock_notifications()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("f", 32))
    session$setInputs(dta_file = fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = fi(zipf, "sdtm.zip"))
    root <- delivery_root(rv$uploads[["ae||1"]][[1]]$path)
    expect_true(dir.exists(root))
    session$setInputs(confirm_reset = 1)
    expect_false(dir.exists(root))
  })
})
