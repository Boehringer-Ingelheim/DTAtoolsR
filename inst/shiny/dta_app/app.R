# =============================================================================
# DTAtools Shiny app
# Modern UI for the DTAtools R package.
# Launched via DTAtools::run_dta_app(). Helper code lives in ./R (auto-sourced).
# =============================================================================

library(shiny)
library(bslib)

# Allow large uploads (clinical data files can be big).
options(shiny.maxRequestSize = 1024 * 1024^2) # 1 GB

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
brandbar <- div(
  class = "app-brandbar",
  div(
    div(class = "brand-title", "DTAtools"),
    div(class = "brand-sub", "Data Transmission Agreement \u2014 validation & authoring")
  )
)

ui <- bslib::page_fluid(
  theme = bi_theme(),
  tags$head(tags$style(bi_css())),
  brandbar,
  div(style = "padding: 18px;", uiOutput("main"))
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- reactive state (single source of truth) ---------------------------
  rv <- reactiveValues(
    dta = NULL,          # the DTA S7 object
    yaml_text = NULL,    # original uploaded YAML text (for raw view)
    structure = NULL,    # stable per-dataset handler metadata (for slots)
    active = NULL,       # currently selected dataset name
    uploads = list(),    # key "dataset||handlerIdx" -> character vector of file names
    status = list(),     # dataset name -> "pass" | "fail" | "pending"
    md_token = 0,        # bump to re-render metadata editor
    contacts_token = 0   # bump to re-render contacts list
  )

  upload_registry <- new.env(parent = emptyenv())
  session_file <- file.path(tempdir(), "dtatools_app_session.rds")

  # --- helpers ------------------------------------------------------------
  build_structure <- function(dta) {
    names_ds <- dta_dataset_names(dta)
    if (length(names_ds) == 0) return(list())
    stats::setNames(lapply(seq_along(names_ds), function(i) {
      ds <- dta_get_dataset(dta, names_ds[i])
      handlers <- lapply(dta_handlers(ds), function(h) list(
        expected = handler_expected(h),
        hint     = handler_hint(h),
        count    = handler_count_label(h),
        min      = handler_min(h),
        max      = handler_max(h),
        pattern  = handler_is_pattern(h)
      ))
      list(
        index = i,
        name = names_ds[i],
        type = tryCatch(ds@type, error = function(e) NA_character_),
        handlers = handlers
      )
    }), names_ds)
  }

  autosave <- function() {
    try(saveRDS(
      list(
        dta = isolate(rv$dta),
        yaml_text = isolate(rv$yaml_text),
        structure = isolate(rv$structure),
        active = isolate(rv$active),
        uploads = isolate(rv$uploads),
        status = isolate(rv$status)
      ),
      session_file
    ), silent = TRUE)
  }

  apply_loaded <- function(dta, yaml_text) {
    names_ds <- dta_dataset_names(dta)
    rv$dta <- dta
    rv$yaml_text <- yaml_text
    rv$structure <- build_structure(dta)
    rv$uploads <- list()
    rv$status <- stats::setNames(rep("pending", length(names_ds)), names_ds)
    rv$active <- if (length(names_ds) > 0) names_ds[1] else NULL
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    autosave()
  }

  # --- landing: load a DTA YAML ------------------------------------------
  observeEvent(input$dta_file, {
    f <- input$dta_file
    req(f)
    txt <- tryCatch(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    res <- dta_read_yaml(f$datapath)
    if (!res$ok) {
      showNotification(
        paste("Could not load DTA YAML:", res$error),
        type = "error", duration = 10
      )
      return()
    }
    apply_loaded(res$value, txt)
    showNotification("DTA loaded.", type = "message")
  })

  observeEvent(input$load_example, {
    path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
    if (!nzchar(path)) {
      showNotification("Example file not found.", type = "error")
      return()
    }
    txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    res <- dta_read_yaml(path)
    if (!res$ok) {
      showNotification(paste("Could not load example:", res$error), type = "error")
      return()
    }
    apply_loaded(res$value, txt)
    showNotification("Example DTA loaded.", type = "message")
  })

  # --- dataset navigation -------------------------------------------------
  observeEvent(input$dataset_nav, {
    rv$active <- input$dataset_nav
  })

  # --- upload observers (registered once per handler) ---------------------
  handle_upload <- function(ds_idx, hi, fileinfo) {
    if (is.null(fileinfo)) return()
    names_ds <- dta_dataset_names(rv$dta)
    if (ds_idx < 1 || ds_idx > length(names_ds)) return()
    dsname <- names_ds[ds_idx]
    ds <- dta_get_dataset(rv$dta, dsname)
    handlers <- dta_handlers(ds)
    if (hi < 1 || hi > length(handlers)) return()
    h <- handlers[[hi]]

    # G2 -- count gate: never let a slot exceed its max (across repeated drops).
    mx <- handler_max(h)
    key <- paste0(dsname, "||", hi)
    already <- length(rv$uploads[[key]] %||% character(0))
    if (!is.na(mx) && (already + nrow(fileinfo)) > mx) {
      showNotification(
        sprintf("This slot accepts at most %d file(s); you already have %d and dropped %d.",
                as.integer(mx), already, nrow(fileinfo)),
        type = "error"
      )
      return()
    }

    loaded <- character(0)
    rejected <- character(0)
    for (r in seq_len(nrow(fileinfo))) {
      nm <- fileinfo$name[r]
      dp <- fileinfo$datapath[r]
      sz <- suppressWarnings(as.numeric(fileinfo$size[r]))

      # G1 -- transfer complete & non-empty (guards truncated/partial uploads).
      if (is.na(sz) || sz <= 0) {
        rejected <- c(rejected, sprintf("'%s' (empty or truncated)", nm))
        next
      }
      # G3 -- filename must match the handler (mirrors matches_filename()).
      if (!handler_matches(h, nm)) {
        rejected <- c(rejected,
                      sprintf("'%s' (name does not match %s)", nm, handler_expected(h)))
        next
      }
      # G4 -- bind via load_file(). Shiny stores the upload under a temp name
      # like "0.csv"; matches_filename()/read_file() key off basename(file), so
      # stage the bytes under the ORIGINAL name first, else a valid file is
      # rejected inside load_file() (failure mode F7).
      staged <- dta_stage_upload(dp, nm)
      before <- dta_dataset_content_count(dta_get_dataset(rv$dta, dsname))
      res <- dta_load_file(
        rv$dta, dataset = dsname, file = staged, handler_index = hi,
        name = tools::file_path_sans_ext(nm)
      )
      if (!res$ok) {
        rejected <- c(rejected, sprintf("'%s' (%s)", nm, res$error))
        next
      }
      # G5 -- VERIFY the file actually landed before claiming success (C2).
      after <- dta_dataset_content_count(dta_get_dataset(res$value, dsname))
      if (after <= before) {
        rejected <- c(rejected, sprintf("'%s' (could not be bound)", nm))
        next
      }
      rv$dta <- res$value
      loaded <- c(loaded, nm)
    }

    # Record only VERIFIED binds; changed data must be re-validated.
    if (length(loaded) > 0) {
      up <- rv$uploads
      up[[key]] <- unique(c(up[[key]], loaded))
      rv$uploads <- up
      st <- rv$status
      st[[dsname]] <- "pending"
      rv$status <- st
      autosave()
    }

    # ONE reconciled outcome (Contract C1) with honest counts (C8): never a
    # success message when nothing was actually bound.
    if (length(loaded) > 0 && length(rejected) == 0) {
      showNotification(
        sprintf("Loaded %d file(s) into '%s'. Run Check to validate.",
                length(loaded), dsname),
        type = "message"
      )
    } else if (length(loaded) > 0) {
      showNotification(
        sprintf("Loaded %d file(s) into '%s'; rejected %d: %s",
                length(loaded), dsname, length(rejected),
                paste(rejected, collapse = "; ")),
        type = "warning", duration = 10
      )
    } else {
      showNotification(
        sprintf("No files added to '%s'. Rejected %d: %s",
                dsname, length(rejected), paste(rejected, collapse = "; ")),
        type = "warning", duration = 10
      )
    }
  }

  observeEvent(rv$structure, {
    req(rv$structure)
    for (dsname in names(rv$structure)) {
      s <- rv$structure[[dsname]]
      for (hi in seq_along(s$handlers)) {
        upid <- sprintf("up_%d_%d", s$index, hi)
        if (is.null(upload_registry[[upid]])) {
          upload_registry[[upid]] <- TRUE
          local({
            UP <- upid; DSIDX <- s$index; HI <- hi
            observeEvent(input[[UP]], {
              handle_upload(DSIDX, HI, input[[UP]])
            }, ignoreInit = TRUE)
          })
        }
      }
    }
  })

  # --- validation ---------------------------------------------------------
  run_check <- function(dataset = NULL) {
    req(rv$dta)
    names_ds <- dta_dataset_names(rv$dta)
    targets <- if (is.null(dataset)) names_ds else intersect(dataset, names_ds)
    if (length(targets) == 0) return()

    # C7 -- pre-flight: only validate datasets that actually have data bound.
    ready <- Filter(function(nm) dta_dataset_readiness(rv$dta, nm)$has_data, targets)
    nodata <- setdiff(targets, ready)

    # Mark no-data datasets explicitly; they are never reported as passed (C5).
    if (length(nodata) > 0) {
      st <- rv$status
      for (nm in nodata) st[[nm]] <- "nodata"
      rv$status <- st
    }

    if (length(ready) == 0) {
      msg <- if (length(targets) == 1) {
        sprintf("'%s' has no data loaded \u2014 upload the required file(s) before validating.",
                targets[1])
      } else {
        "No selected dataset has data loaded yet \u2014 upload files before validating."
      }
      showNotification(msg, type = "warning", duration = 8)
      return()
    }

    ok <- FALSE
    err <- NULL
    withProgress(message = "Validating\u2026", value = 0.4, {
      res <- dta_check(rv$dta, dataset = ready)
      setProgress(0.8)
      if (res$ok) {
        rv$dta <- res$value
        rv$status <- dta_status_map(rv$dta)
        ok <- TRUE
      } else {
        err <- res$error
      }
      setProgress(1)
    })

    if (!ok) {
      showNotification(paste("Validation error:", err), type = "error", duration = 10)
      return()
    }
    autosave()

    # ONE reconciled summary (C1/C6) derived from the verified status map.
    st <- unlist(rv$status)
    n_pass <- sum(st[ready] == "pass", na.rm = TRUE)
    n_fail <- sum(st[ready] == "fail", na.rm = TRUE)
    parts <- character(0)
    if (n_pass > 0) parts <- c(parts, sprintf("%d passed", n_pass))
    if (n_fail > 0) parts <- c(parts, sprintf("%d failed", n_fail))
    if (length(nodata) > 0) parts <- c(parts, sprintf("%d skipped (no data)", length(nodata)))
    if (length(parts) == 0) parts <- "nothing to validate"
    showNotification(
      sprintf("Validation complete \u2014 %s.", paste(parts, collapse = ", ")),
      type = if (n_fail > 0) "warning" else "message"
    )
  }

  observeEvent(input$check_all, run_check(NULL))
  observeEvent(input$check_one, {
    req(rv$active)
    run_check(rv$active)
  })

  # --- messages + inspect -------------------------------------------------
  msgs_r <- reactive({
    req(rv$active)
    rv$status # depend on validation state
    dta_dataset_messages(rv$dta, rv$active)
  })

  output$msgs <- DT::renderDataTable({
    m <- msgs_r()
    if (is.null(m) || nrow(m) == 0) {
      return(DT::datatable(
        data.frame(Message = "No validation messages. Run Check to validate this dataset."),
        rownames = FALSE, options = list(dom = "t"), selection = "none"
      ))
    }
    cols <- intersect(c("id", "source", "row", "column", "rule_id", "message"), names(m))
    DT::datatable(
      m[, cols, drop = FALSE],
      rownames = FALSE, selection = "single",
      options = list(pageLength = 8, dom = "tp", scrollX = TRUE)
    )
  })

  df_to_kv <- function(d) {
    # single-row -> transposed Field/Value (drop NA/empty)
    row <- d[1, , drop = FALSE]
    keep <- vapply(names(row), function(k) {
      v <- row[[k]]
      !(is.null(v) || all(is.na(v)) || (is.character(v) && all(!nzchar(v))))
    }, logical(1))
    fields <- names(row)[keep]
    tags$table(
      class = "table table-sm",
      tags$tbody(
        lapply(fields, function(k) {
          tags$tr(
            tags$th(style = "white-space:nowrap; color:var(--bi-grey);", k),
            tags$td(as.character(row[[k]]))
          )
        })
      )
    )
  }

  df_to_html_table <- function(d) {
    # drop all-NA columns
    keep <- vapply(d, function(v) !all(is.na(v)), logical(1))
    d <- d[, keep, drop = FALSE]
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-sm table-striped",
        tags$thead(tags$tr(lapply(names(d), tags$th))),
        tags$tbody(
          lapply(seq_len(nrow(d)), function(i) {
            tags$tr(lapply(names(d), function(k) tags$td(as.character(d[[k]][i]))))
          })
        )
      )
    )
  }

  show_inspect_modal <- function(dataset, id) {
    res <- dta_inspect(rv$dta, dataset, id)
    if (!res$ok || is.null(res$value) || nrow(res$value) == 0) {
      showModal(modalDialog(
        title = paste("Message", id),
        tags$p("No detailed inspect report is available for this message."),
        if (!is.null(res$error)) tags$pre(res$error),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    d <- res$value
    body <- if (nrow(d) == 1) df_to_kv(d) else df_to_html_table(d)
    showModal(modalDialog(
      title = paste("Inspect \u2014 message", id),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      body
    ))
  }

  observeEvent(input$msgs_rows_selected, {
    sel <- input$msgs_rows_selected
    m <- msgs_r()
    if (is.null(sel) || is.null(m) || nrow(m) == 0) return()
    id <- if ("id" %in% names(m)) m$id[sel] else sel
    show_inspect_modal(rv$active, id)
  })

  # --- metadata editing ---------------------------------------------------
  output$metadata_editor <- renderUI({
    rv$md_token
    dta <- isolate(rv$dta)
    req(dta)
    md <- DTAtools::metadata(dta)
    getf <- function(field) {
      v <- tryCatch(S7::prop(md, field), error = function(e) NULL)
      if (is.null(v)) "" else as.character(v)[1]
    }
    date_val <- tryCatch(S7::prop(md, "date"), error = function(e) NULL)
    tagList(
      layout_columns(
        col_widths = c(6, 6),
        textInput("md_title", "Title", value = getf("title"), width = "100%"),
        textInput("md_version", "Version", value = getf("version"), width = "100%")
      ),
      layout_columns(
        col_widths = c(6, 6),
        dateInput("md_date", "Date",
                  value = if (inherits(date_val, "Date")) date_val else NULL,
                  width = "100%"),
        textInput("md_header", "Header / organization", value = getf("header"), width = "100%")
      ),
      div(class = "msg-hint",
          "Changes are saved automatically to the current session as you type.")
    )
  })

  # per-field debounced saves (incremental, non-destructive)
  save_md <- function(field, value) {
    req(rv$dta)
    r <- dta_set_metadata_field(rv$dta, field, value)
    if (r$ok) {
      rv$dta <- r$value
      autosave()
    } else {
      showNotification(paste("Could not update", field, "\u2014", r$error),
                       type = "error")
    }
  }
  title_d   <- debounce(reactive(input$md_title), 700)
  version_d <- debounce(reactive(input$md_version), 700)
  header_d  <- debounce(reactive(input$md_header), 700)
  observeEvent(title_d(),   save_md("title", title_d()),     ignoreInit = TRUE)
  observeEvent(version_d(), save_md("version", version_d()), ignoreInit = TRUE)
  observeEvent(header_d(),  save_md("header", header_d()),   ignoreInit = TRUE)
  observeEvent(input$md_date, {
    req(input$md_date)
    save_md("date", input$md_date)
  }, ignoreInit = TRUE)

  # --- people / contacts --------------------------------------------------
  render_contacts <- function(side) {
    cs <- dta_contacts(isolate(rv$dta), side)
    if (length(cs) == 0) {
      return(div(class = "msg-hint", "No contacts yet."))
    }
    tags$ul(
      class = "list-group",
      lapply(seq_along(cs), function(i) {
        tags$li(
          class = "list-group-item d-flex justify-content-between align-items-center",
          span(contact_display(cs[[i]])),
          actionButton(
            paste0("rm_", side, "_", i), "Remove",
            class = "btn btn-sm btn-outline-danger"
          )
        )
      })
    )
  }

  output$receiver_contacts <- renderUI({ rv$contacts_token; req(isolate(rv$dta)); render_contacts("receiver") })
  output$supplier_contacts <- renderUI({ rv$contacts_token; req(isolate(rv$dta)); render_contacts("supplier") })

  # --- affiliation (side-level: receiver / supplier) ----------------------
  render_affiliation <- function(side) {
    aff <- dta_affiliation(isolate(rv$dta), side)
    g <- function(k) aff[[k]] %||% ""
    tagList(
      textInput(paste0(side, "_aff_name"), "Organization",
                value = g("name"), width = "100%",
                placeholder = "e.g. Test Company"),
      layout_columns(
        col_widths = c(6, 6),
        textInput(paste0(side, "_aff_country"), "Country",
                  value = g("country"), width = "100%"),
        textInput(paste0(side, "_aff_address"), "Address",
                  value = g("address"), width = "100%")
      )
    )
  }
  output$receiver_affiliation <- renderUI({ rv$md_token; req(isolate(rv$dta)); render_affiliation("receiver") })
  output$supplier_affiliation <- renderUI({ rv$md_token; req(isolate(rv$dta)); render_affiliation("supplier") })

  save_affiliation <- function(side, field, value) {
    req(rv$dta)
    kv <- list(); kv[[field]] <- value %||% ""
    r <- do.call(dta_set_affiliation, c(list(rv$dta, side), kv))
    if (r$ok) {
      rv$dta <- r$value
      autosave()
    } else {
      showNotification(paste("Could not update affiliation \u2014", r$error), type = "error")
    }
  }
  for (.side in c("receiver", "supplier")) {
    for (.field in c("name", "country", "address")) {
      local({
        SIDE <- .side; FIELD <- .field
        inid <- paste0(SIDE, "_aff_", FIELD)
        d <- debounce(reactive(input[[inid]]), 700)
        observeEvent(d(), save_affiliation(SIDE, FIELD, d()), ignoreInit = TRUE)
      })
    }
  }

  # dynamic remove observers for contacts
  contact_rm_registry <- new.env(parent = emptyenv())
  observe({
    rv$contacts_token
    req(isolate(rv$dta))
    for (side in c("receiver", "supplier")) {
      cs <- dta_contacts(isolate(rv$dta), side)
      for (i in seq_along(cs)) {
        rmid <- paste0("rm_", side, "_", i)
        if (is.null(contact_rm_registry[[rmid]])) {
          contact_rm_registry[[rmid]] <- TRUE
          local({
            SIDE <- side; IDX <- i; ID <- rmid
            observeEvent(input[[ID]], {
              showModal(modalDialog(
                title = "Remove contact?",
                sprintf("Remove this %s contact?", SIDE),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(paste0("confirm_", ID), "Remove", class = "btn btn-danger")
                ),
                easyClose = TRUE
              ))
            }, ignoreInit = TRUE)
            observeEvent(input[[paste0("confirm_", ID)]], {
              r <- dta_remove_contact(rv$dta, SIDE, IDX)
              if (r$ok) {
                rv$dta <- r$value
                rv$contacts_token <- rv$contacts_token + 1
                autosave()
              } else {
                showNotification(r$error, type = "error")
              }
              removeModal()
            }, ignoreInit = TRUE)
          })
        }
      }
    }
  })

  add_contact_flow <- function(side) {
    showModal(modalDialog(
      title = paste("Add", side, "contact"),
      textInput("new_contact_name", "Name"),
      textInput("new_contact_roles", "Role(s)", placeholder = "e.g. Data Manager, Reviewer"),
      textInput("new_contact_email", "Email (optional)"),
      div(class = "msg-hint",
          "Separate multiple roles with commas. Affiliation is set once per side (above), not per person."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0("confirm_add_", side), "Add", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  observeEvent(input$add_receiver, add_contact_flow("receiver"))
  observeEvent(input$add_supplier, add_contact_flow("supplier"))

  confirm_add <- function(side) {
    nm <- input$new_contact_name %||% ""
    if (!nzchar(nm)) {
      showNotification("A name is required.", type = "warning")
      return()
    }
    roles <- trimws(strsplit(input$new_contact_roles %||% "", ",")[[1]])
    r <- dta_add_contact(
      rv$dta, side, name = nm, roles = roles,
      email = input$new_contact_email %||% ""
    )
    if (r$ok) {
      rv$dta <- r$value
      rv$contacts_token <- rv$contacts_token + 1
      autosave()
      removeModal()
    } else {
      showNotification(r$error, type = "error")
    }
  }
  observeEvent(input$confirm_add_receiver, confirm_add("receiver"))
  observeEvent(input$confirm_add_supplier, confirm_add("supplier"))

  # --- raw YAML (syntax-highlighted, read-only) ---------------------------
  output$raw_yaml <- renderUI({
    req(rv$yaml_text)
    HTML(paste0(
      "<pre class=\"yaml-view\"><code>",
      yaml_highlight_html(rv$yaml_text),
      "</code></pre>"
    ))
  })

  # --- export -------------------------------------------------------------
  output$dl_docx <- downloadHandler(
    filename = function() paste0("DTA_", Sys.Date(), ".docx"),
    content = function(file) {
      req(rv$dta)
      res <- dta_export(rv$dta, file, "docx")
      if (!res$ok) {
        showNotification(paste("Word export failed:", res$error), type = "error", duration = 10)
        stop(res$error)
      }
    }
  )
  output$dl_pdf <- downloadHandler(
    filename = function() paste0("DTA_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(rv$dta)
      res <- dta_export(rv$dta, file, "pdf")
      if (!res$ok) {
        showNotification(
          paste("PDF export failed (a DOCX converter such as LibreOffice may be required):",
                res$error),
          type = "error", duration = 12
        )
        stop(res$error)
      }
    }
  )

  # --- reset --------------------------------------------------------------
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Start over?",
      "This clears the loaded DTA and all uploads from this session.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Start over", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_reset, {
    rv$dta <- NULL; rv$yaml_text <- NULL; rv$structure <- NULL
    rv$active <- NULL; rv$uploads <- list(); rv$status <- list()
    try(unlink(session_file), silent = TRUE)
    removeModal()
  })

  # --- status / summary outputs ------------------------------------------
  output$dataset_status_line <- renderUI({
    req(rv$active)
    st <- rv$status[[rv$active]] %||% "pending"
    s <- rv$structure[[rv$active]]
    type_txt <- if (!is.null(s) && !is.na(s$type)) s$type else ""
    div(
      style = "display:flex; align-items:center; gap:12px; margin-bottom:10px;",
      tags$h4(rv$active, style = "margin:0;"),
      if (nzchar(type_txt)) tags$span(class = "status-chip status-pending", type_txt),
      status_chip(st)
    )
  })

  output$loaded_files <- renderUI({
    req(rv$active, rv$structure)
    s <- rv$structure[[rv$active]]
    tagList(lapply(seq_along(s$handlers), function(hi) {
      h <- s$handlers[[hi]]
      key <- paste0(rv$active, "||", hi)
      files <- rv$uploads[[key]] %||% character(0)
      n <- length(files)
      mn <- h$min
      state <- if (n == 0) "empty" else if (!is.na(mn) && n < mn) "warn" else "ok"
      detail <- if (n == 0) "no files yet" else paste0("bound: ", paste(files, collapse = ", "))
      div(
        style = "margin-bottom:6px;",
        tags$span(class = "slot-expected", h$expected),
        tags$span(class = "slot-meta", paste0("  (", h$count, ")  ")),
        slot_state_label(state, detail)
      )
    }))
  })

  output$workspace_header <- renderUI({
    req(rv$dta)
    md <- tryCatch(DTAtools::metadata(rv$dta), error = function(e) NULL)
    getf <- function(field) {
      v <- tryCatch(S7::prop(md, field), error = function(e) NULL)
      if (is.null(v) || length(v) == 0) "" else as.character(v)[1]
    }
    title <- getf("title"); if (!nzchar(title)) title <- "Untitled DTA"
    version <- getf("version")
    date <- getf("date")
    div(
      class = "workspace-header",
      div(class = "ws-title", title),
      if (nzchar(version) || nzchar(date)) div(
        class = "ws-meta",
        if (nzchar(version)) tags$span(class = "ws-pill", paste0("v", version)),
        if (nzchar(date)) tags$span(class = "ws-pill", date)
      )
    )
  })

  output$summary_metrics <- renderUI({
    req(rv$dta)
    st <- unlist(rv$status)
    n <- length(dta_dataset_names(rv$dta))
    np <- sum(st == "pass"); nf <- sum(st == "fail"); nd <- sum(st == "nodata")
    div(
      style = "display:flex; gap:18px; margin-bottom:8px; flex-wrap:wrap;",
      div(div(class = "metric", n), div(class = "slot-meta", "datasets")),
      div(div(class = "metric", np), div(class = "slot-meta", "passed")),
      div(div(class = "metric", nf), div(class = "slot-meta", "failed")),
      div(div(class = "metric", nd), div(class = "slot-meta", "no data"))
    )
  })

  # --- dataset detail (structure only -> stable file inputs) --------------
  output$dataset_detail <- renderUI({
    req(rv$active, rv$structure)
    s <- rv$structure[[rv$active]]
    req(!is.null(s))
    ds_idx <- s$index

    if (length(s$handlers) == 0) {
      slots <- div(class = "msg-hint", "This dataset has no file handlers.")
    } else {
      slot_cards <- lapply(seq_along(s$handlers), function(hi) {
        h <- s$handlers[[hi]]
        upid <- sprintf("up_%d_%d", ds_idx, hi)
        multiple <- isTRUE(!is.na(h$max) && h$max > 1)
        card(
          class = "slot-card",
          card_header(
            div(
              tags$strong("Expected: "),
              tags$span(class = "slot-expected", h$expected),
              tags$span(class = "slot-meta", paste0("  \u2022  ", h$count))
            )
          ),
          card_body(
            if (nzchar(h$hint)) div(class = "slot-meta", style = "margin-bottom:8px;", h$hint),
            div(class = "dropzone",
                fileInput(upid,
                          label = if (multiple) "Drop or choose file(s)" else "Drop or choose a file",
                          multiple = multiple, width = "100%"))
          )
        )
      })
      slots <- do.call(bslib::layout_columns, c(list(col_widths = 6), slot_cards))
    }

    tagList(
      uiOutput("dataset_status_line"),
      div(
        style = "margin-bottom:12px;",
        actionButton("check_one", "Check this dataset",
                     class = "btn btn-primary"),
        tags$span(class = "msg-hint", style = "margin-left:10px;",
                  "Uploads are validated against the file handler as you add them.")
      ),
      card(
        card_header("Loaded files"),
        card_body(uiOutput("loaded_files"))
      ),
      slots,
      tags$hr(),
      tags$h5("Validation messages"),
      DT::dataTableOutput("msgs"),
      div(class = "msg-hint",
          "Click a message row to open the detailed inspect report.")
    )
  })

  # --- main layout (landing vs workspace) --------------------------------
  output$main <- renderUI({
    # Depend ONLY on rv$structure (set once per load) so metadata edits and
    # uploads -- which mutate rv$dta -- do not rebuild the whole workspace or
    # reset the active tab / file inputs. Live bits live in their own outputs.
    if (is.null(rv$structure)) {
      # Landing
      restore_available <- file.exists(session_file)
      card(
        max_height = "620px",
        card_header(tags$h3("Load a DTA specification", style = "margin:0;")),
        card_body(
          p("Drag and drop a DTA ", tags$code(".yaml"),
            " file to begin, or load the bundled example."),
          div(class = "dropzone",
              fileInput("dta_file", "Drop or choose a .yaml / .yml file",
                        accept = c(".yaml", ".yml"), width = "100%")),
          div(
            style = "display:flex; gap:10px; margin-top:8px;",
            actionButton("load_example", "Load example DTA", class = "btn btn-outline-primary"),
            if (restore_available)
              actionButton("restore_session", "Restore previous session",
                           class = "btn btn-outline-secondary")
          )
        )
      )
    } else {
      # Workspace
      names_ds <- names(rv$structure)
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          uiOutput("workspace_header"),
          uiOutput("summary_metrics"),
          radioButtons("dataset_nav", "Datasets",
                       choices = names_ds,
                       selected = isolate(rv$active) %||% names_ds[1]),
          tags$hr(),
          actionButton("check_all", "Check all datasets", class = "btn btn-primary w-100"),
          div(style = "height:8px;"),
          downloadButton("dl_docx", "Export Word", class = "btn btn-outline-primary w-100"),
          div(style = "height:6px;"),
          downloadButton("dl_pdf", "Export PDF", class = "btn btn-outline-primary w-100"),
          tags$hr(),
          actionButton("reset_app", "Start over", class = "btn btn-outline-danger w-100")
        ),
        navset_card_tab(
          nav_panel("Datasets", uiOutput("dataset_detail")),
          nav_panel(
            "Metadata",
            uiOutput("metadata_editor"),
            tags$hr(),
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header(
                  div(style = "display:flex; justify-content:space-between; align-items:center;",
                      "Receiver",
                      actionButton("add_receiver", "Add person", class = "btn btn-sm btn-outline-primary"))
                ),
                card_body(
                  div(class = "section-label", "Affiliation"),
                  uiOutput("receiver_affiliation"),
                  tags$hr(),
                  div(class = "section-label", "Contacts"),
                  uiOutput("receiver_contacts")
                )
              ),
              card(
                card_header(
                  div(style = "display:flex; justify-content:space-between; align-items:center;",
                      "Supplier",
                      actionButton("add_supplier", "Add person", class = "btn btn-sm btn-outline-primary"))
                ),
                card_body(
                  div(class = "section-label", "Affiliation"),
                  uiOutput("supplier_affiliation"),
                  tags$hr(),
                  div(class = "section-label", "Contacts"),
                  uiOutput("supplier_contacts")
                )
              )
            )
          ),
          nav_panel(
            "Raw YAML",
            div(class = "msg-hint", style = "margin-bottom:6px;",
                "Original uploaded YAML (read-only, syntax-highlighted). Edits made in the app update the DTA object used for validation and export."),
            uiOutput("raw_yaml")
          )
        )
      )
    }
  })

  # --- restore previous session ------------------------------------------
  observeEvent(input$restore_session, {
    if (!file.exists(session_file)) return()
    saved <- tryCatch(readRDS(session_file), error = function(e) NULL)
    if (is.null(saved) || is.null(saved$dta)) {
      showNotification("Could not restore the previous session.", type = "error")
      return()
    }
    rv$dta <- saved$dta
    rv$yaml_text <- saved$yaml_text
    rv$structure <- saved$structure %||% build_structure(saved$dta)
    rv$uploads <- saved$uploads %||% list()
    rv$status <- saved$status %||% stats::setNames(
      rep("pending", length(dta_dataset_names(saved$dta))),
      dta_dataset_names(saved$dta)
    )
    rv$active <- saved$active %||% (dta_dataset_names(saved$dta)[1] %||% NULL)
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    showNotification("Previous session restored.", type = "message")
  })
}

shinyApp(ui, server)
