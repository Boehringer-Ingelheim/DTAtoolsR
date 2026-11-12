# Deliveries in the interactive application. REQ-CONT-016 and REQ-CONT-017.
#
# Driven through shiny::testServer(), which runs the real server function
# without a browser, so every input name below is a contract with the
# application (maintaining.md, "You changed the Shiny app"). What is checked is
# what a user could observe: the records the workspace holds, the files that
# are on disk, and what the notifications say.
#
# Every expected value is written down from the specification and from the
# literal data of the fixtures (helper-container.R), never captured from a run:
# sdtm_csv.zip holds ae.csv (53 bytes) and dm.csv (30 bytes), so a delivery of
# it writes 83 bytes. The session budget is the environment variable
# DTATOOLS_SESSION_DELIVERY_BYTES; with 100 bytes, one 53-byte extraction fits
# and a second one of 83 bytes does not, until the first is deleted.
#
# Two notes on the harness. testServer() evaluates the test block against a
# copy of the server environment, so a plain local such as `owned` is frozen at
# the value it had before the first upload; it is read through session$env,
# the live server environment. And every file a case drops is written before
# testServer() starts: files written inside the block would be deleted with an
# environment that never exits, after the case. A "nothing is left on disk"
# step always names a path that existed before the action, so it fails if the
# application leaves the file behind.
#
# LIM-014 (extracted members stay on disk while they are used) is the reason
# the cleanup below is the application's job.

# ---- local helpers --------------------------------------------------------------

# The delivery directories the session still owns and that exist.
ap_deliveries <- function(session) {
  u <- session$env$owned$upload
  u[startsWith(basename(u), "dta_upload_delivery_") & dir.exists(u)]
}

# The delivery directory a path lies under.
ap_root <- function(path) {
  if (!length(path)) {
    return(character(0))
  }
  while (!startsWith(basename(path), "dta_upload_delivery_") && dirname(path) != path) path <- dirname(path)
  path
}

# The lines of a YAML document, written to a file of its own.
ap_yaml <- function(lines, env = parent.frame()) {
  f <- file.path(qa_tempdir(env), "spec.yaml")
  writeLines(lines, f)
  f
}

AP_HEADER <- c("metadata:", "  title: Delivery test", "  version: \"1\"", "datasets:")

ap_ae <- function(files = "{filename: ae.csv, type: csv}") {
  c(
    "  - id: ae", "    type: tabular", paste0("    files: ", files),
    "    columns:",
    "      - {id: USUBJID, type: SAS Char}", "      - {id: AETERM, type: SAS Char}", "      - {id: AESEQ, type: SAS Num}"
  )
}

AP_DM <- c(
  "  - id: dm", "    type: tabular", "    files: {filename: dm.csv, type: csv}",
  "    columns:",
  "      - {id: USUBJID, type: SAS Char}", "      - {id: AGE, type: SAS Num}"
)

# The plain specification of ae and dm, and the one in which ae declares
# `container: sdtm_csv.zip`.
ap_spec <- function(env = parent.frame()) ap_yaml(c(AP_HEADER, ap_ae(), AP_DM), env)
ap_spec_container <- function(env = parent.frame()) {
  ap_yaml(c(AP_HEADER, ap_ae("{filename: ae.csv, type: csv, container: sdtm_csv.zip}"), AP_DM), env)
}

# A copy of a fixture at another path: testServer() fires an observer only when
# the value of an input changes.
ap_copy <- function(fixture, env = parent.frame()) cont_write(fixture, cont_bytes(fixture), env = env)

# ---- cases --------------------------------------------------------------------------

test_that("OQ-CONT-025 | a zip dropped on one slot fills every dataset, asks before replacing, cleans up and frees the session budget | REQ-CONT-016", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  # One archive on the ae slot fills ae and dm, and the extraction is the
  # session's to delete.
  spec <- ap_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(zip1, "sdtm_csv.zip"))

    qa_step("a drop on the ae slot binds a file to ae and to dm", c("ae||1", "dm||1"), sort(names(rv$uploads)))
    rec <- rv$uploads[["dm||1"]][[1]]
    qa_step(
      "the dm record names its table and the archive it came from",
      list(table = "dm", container = "sdtm_csv.zip"),
      list(table = rec$table, container = rec$container)
    )
    dirs <- ap_deliveries(session)
    qa_step("one delivery directory is owned by the session", 1L, length(dirs))
    qa_check(
      "the dm file lies under it and exists",
      startsWith(normalizePath(rec$path, winslash = "/"), normalizePath(dirs, winslash = "/")) && file.exists(rec$path)
    )
    qa_step("dm holds its table", "dm", names(tables(datasets(rv$dta, "dm"))))
    owned <- session$env$owned$upload
    staged <- owned[!startsWith(basename(owned), "dta_upload_delivery_")]
    qa_check("the archive was staged for the delivery", length(staged) >= 1)
    qa_step("and the staged copy of the archive is gone", FALSE, any(file.exists(file.path(staged, "sdtm_csv.zip"))))
  })

  # A second delivery asks first. Cancel keeps the old data and deletes the
  # whole new extraction.
  spec <- ap_spec()
  zip2 <- ap_copy("sdtm_csv.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(zip1, "sdtm_csv.zip"))
    session$setInputs(up_1_1 = cont_fi(zip2, "sdtm_csv.zip"))

    qa_check("a second delivery over loaded tables is held pending the user's answer", !is.null(rv$pending_upload$delivery))
    x <- rv$pending_upload$delivery$exdir
    copies <- rv$pending_upload$delivery$copies
    qa_step(
      "the pending delivery's extraction and the staged copy exist",
      list(extraction = TRUE, copies = 1L, copy_exists = TRUE),
      list(extraction = dir.exists(x), copies = length(copies), copy_exists = all(file.exists(copies)))
    )
    session$setInputs(cancel_overwrite = 1)
    qa_step("Cancel: nothing is pending any more", TRUE, is.null(rv$pending_upload))
    qa_step("Cancel: the extraction that existed is deleted", FALSE, dir.exists(x))
    qa_step("Cancel: so is the staged copy that existed", FALSE, any(file.exists(copies)))
    qa_step("Cancel: the old data is kept", 1L, length(rv$uploads[["ae||1"]]))
  })

  # Replace commits the new delivery and deletes the superseded extraction.
  spec <- ap_spec()
  zip3 <- ap_copy("sdtm_csv.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(zip1, "sdtm_csv.zip"))
    first <- ap_deliveries(session)
    old <- rv$uploads[["ae||1"]][[1]]$path
    qa_check("the first delivery's ae file exists", file.exists(old))
    session$setInputs(up_1_1 = cont_fi(zip3, "sdtm_csv.zip"))
    session$setInputs(confirm_overwrite = 1)

    qa_step("Replace: ae has one record", 1L, length(rv$uploads[["ae||1"]]))
    qa_step("Replace: the superseded file that existed is deleted", FALSE, file.exists(old))
    now <- ap_deliveries(session)
    qa_step("Replace: one delivery directory remains", 1L, length(now))
    qa_step("and it is not the first one", FALSE, identical(now, first))
    qa_step(
      "and holds the new delivery's two files",
      c("ae.csv", "dm.csv"),
      sort(basename(list.files(now, recursive = TRUE)))
    )
  })

  # A loose file for an entry that declares a container is refused before any
  # question about replacing, also when the entry is already filled.
  spec_c <- ap_spec_container()
  ae1 <- cont_write("ae.csv", CONT_AE_CSV)
  ae2 <- cont_write("ae.csv", CONT_AE_CSV)
  shown <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown <<- c(shown, paste(as.character(ui), collapse = ""))
      invisible(NULL)
    },
    .package = "shiny"
  )
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec_c, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(ae1, "ae.csv"))
    qa_step("a loose ae.csv for an entry declared inside sdtm_csv.zip binds nothing", 0L, length(rv$uploads))
    qa_check(
      "and the notification names the container the file is expected in",
      grepl("'ae.csv' (expected inside sdtm_csv.zip; drop the archive)", utils::tail(shown, 1), fixed = TRUE)
    )
    session$setInputs(up_1_1 = cont_fi(zip1, "sdtm_csv.zip"))
    qa_step("the archive itself fills the entry", 1L, length(rv$uploads[["ae||1"]]))
    session$setInputs(up_1_1 = cont_fi(ae2, "ae.csv"))
    qa_step("a second loose ae.csv raises no question about replacing", TRUE, is.null(rv$pending_upload))
    qa_check(
      "and is refused with the same notification",
      grepl("expected inside sdtm_csv.zip", utils::tail(shown, 1), fixed = TRUE)
    )
  })

  # A hostile archive binds nothing.
  spec <- ap_spec()
  hostile <- cont_fx("unsafe_dotdot.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(hostile, "unsafe_dotdot.zip"))
    qa_step("a hostile archive binds no file", 0L, length(rv$uploads))
    qa_step("and ae holds no table", 0L, length(tables(datasets(rv$dta, "ae"))))
  })

  # A delivery that fails after extraction began leaves no directory.
  spec <- ap_spec()
  lying <- cont_fx("lying_size.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(lying, "lying_size.zip"))
    qa_step("an archive refused while it is extracted binds no file", 0L, length(rv$uploads))
    qa_step("and leaves no extraction directory behind", 0L, length(ap_deliveries(session)))
  })

  # The session budget: removing or discarding files frees it.
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "100")
  spec <- ap_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a1, "a.zip"))
    qa_step("a 53-byte extraction is held by the session", 1L, length(ap_deliveries(session)))
    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    qa_step("discarding the dataset's files deletes the extraction", 0L, length(ap_deliveries(session)))
    session$setInputs(up_1_1 = cont_fi(a2, "a.zip"))
    qa_step("so the same 53 bytes fit in a budget of 100 again", 1L, length(rv$uploads[["ae||1"]]))
    qa_step("in an extraction of its own", 1L, length(ap_deliveries(session)))
  })

  # One delivery that filled two datasets: discarding one frees exactly its files.
  spec <- ap_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b1 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  b2 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  shown <- character(0)
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a1, "a.zip"))
    # up_2_1 is dm's slot. A browser renders it only while dm is the active
    # dataset; testServer() accepts it regardless. That is this case's shortcut,
    # not a defect of the application.
    session$setInputs(up_2_1 = cont_fi(b1, "b.zip"))
    qa_step("83 bytes against the 47 that are left are refused: only ae holds a file", "ae||1", names(rv$uploads))
    qa_check(
      "and the notification says what to do",
      grepl("remove or discard", utils::tail(shown, 1), fixed = TRUE)
    )

    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    session$setInputs(up_2_1 = cont_fi(b2, "b.zip"))
    qa_step("after ae's files are discarded the delivery fits and fills both datasets", c("ae||1", "dm||1"), sort(names(rv$uploads)))
    ae_file <- rv$uploads[["ae||1"]][[1]]$path
    dm_file <- rv$uploads[["dm||1"]][[1]]$path
    qa_step("both files are in the delivery", TRUE, file.exists(ae_file) && file.exists(dm_file))

    session$setInputs(discard_all = 2)
    session$setInputs(confirm_discard_all = 2)
    qa_step("discarding ae again deletes ae's file", FALSE, file.exists(ae_file))
    qa_step("and keeps dm's, which a record still names", TRUE, file.exists(dm_file))
    session$setInputs(up_1_1 = cont_fi(a2, "a.zip"))
    qa_step("the 70 bytes then free hold ae's 53 again", "ae", names(tables(datasets(rv$dta, "ae"))))
  })

  # An extraction whose dataset was removed is deleted before the next delivery
  # is counted against the budget: 57 bytes are more than the 47 left while
  # ae's 53 still count.
  spec <- ap_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b <- cont_zip(list(dm.csv = paste0(CONT_DM_CSV, "S-003,40\nS-004,22\nS-005,63\n")), name = "b.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a, "a.zip"))
    held <- ap_deliveries(session)
    qa_step("ae's 53-byte extraction is held", 1L, length(held))
    qapp_unlock_editing(session)
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)
    qa_step("ae is removed from the specification", "dm", names(rv$structure))
    session$setInputs(up_1_1 = cont_fi(b, "b.zip"))
    qa_step(
      "a 57-byte delivery is accepted, because the removed dataset's extraction no longer counts",
      "dm",
      names(tables(datasets(rv$dta, "dm")))
    )
    qa_step("and the extraction of the removed dataset was deleted", FALSE, any(dir.exists(held)))
  })

  # Removing the one file of a delivery deletes its extraction.
  spec <- ap_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a, "a.zip"))
    held <- ap_deliveries(session)
    qa_step("the delivery is held", 1L, length(held))
    session$setInputs(rmfile_1 = 1)
    qa_step("removing its file empties the record", 0L, length(rv$uploads[["ae||1"]]))
    qa_step("and deletes the extraction that existed", FALSE, any(dir.exists(held)))
  })

  # Session end keeps only the files a file dataset uses: "Restore previous
  # session" re-reads file datasets from their files.
  spec <- ap_yaml(c(
    AP_HEADER, ap_ae(),
    "  - id: define", "    type: file", "    files: {filename: define.xml, type: any}"
  ))
  d_zip <- cont_zip(list(ae.csv = CONT_AE_CSV, define.xml = "<define/>\n"), name = "d.zip")
  ae_path <- NULL
  define_path <- NULL
  withr::defer(unlink(ap_root(define_path), recursive = TRUE))
  shiny::testServer(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(d_zip, "d.zip"))
    ae_path <<- rv$uploads[["ae||1"]][[1]]$path
    define_path <<- rv$uploads[["define||1"]][[1]]$path
    qa_step("one delivery filled both the table and the file dataset", TRUE, file.exists(ae_path) && file.exists(define_path))
    session$close()
  })
  qa_step("at session end the table's extracted file is deleted", FALSE, file.exists(ae_path))
  qa_step("and the file dataset's is kept", TRUE, file.exists(define_path))
})

test_that("OQ-CONT-026 | the file editor offers xpt and saves Delivered inside into the YAML | REQ-CONT-017", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    # Every input the form would render is set explicitly: testServer() has no
    # browser, and an absent input is not the same answer as an unset one.
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qa_step("the bundled specification's dataset is active", "clinical_data", as.character(rv$active))
    qapp_unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)
    session$setInputs(
      file_filename = "ae.xpt", file_type = "xpt", file_pattern = FALSE,
      file_count_mode = "exact", file_number_of_files = 1,
      file_min_number_of_files = 1, file_max_number_of_files = 1,
      file_pattern_description = "", file_info = "", file_extensions = "",
      file_container = "sdtm.xpt"
    )
    session$setInputs(file_save = 1)

    qa_step("the save is accepted", TRUE, is.null(rv$file_msg))
    qa_check("a tabular dataset is offered the type xpt", "xpt" %in% qapp_fn("dta_handler_types")("tabular"))
    handler <- datasets(rv$dta, "clinical_data")@files[[2]]
    qa_step(
      "the new entry is a transport-file handler that carries the container",
      list(xpt = TRUE, container = "sdtm.xpt"),
      list(xpt = inherits(handler, "DTAtools::DTAFileXPT"), container = handler@container)
    )
    yaml_text <- paste(qapp_fn("dta_to_yaml_text")(rv$dta)$value, collapse = "\n")
    qa_check("the exported YAML writes the container", grepl("container: sdtm.xpt", yaml_text, fixed = TRUE))
    qa_check("and the file type", grepl("type: xpt", yaml_text, fixed = TRUE))
  })
})
