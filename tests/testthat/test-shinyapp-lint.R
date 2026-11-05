# Spec guards in the Shiny app: a column a rule uses cannot be deleted, a rename
# carries into the rules, a bad length is refused, examples are editable, and a
# document whose rules contradict its columns is refused or reported.
#
# Same method as test-shinyapp-spec-recheck.R: drive the server through inputs
# and read state. The linter itself (lint_dta() and its codes) is tested in
# test-lintDTA.R; here only the app's wiring around it is.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

lint_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

lint_clean_session <- function() {
  unlink(
    list.files(tempdir(),
      pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
    ),
    force = TRUE
  )
}

# Loaded clinical spec, editing unlocked, column editor open.
lint_setup <- quote({
  session$setInputs(dta_file = lint_input("clinical_dta.yaml"))
  unlock_editing(session)
  session$setInputs(edit_cols = 1)
})

# The inputs the column form carries for a column, with overrides.
lint_col_form <- function(dta, id, ...) {
  f <- app_fn("dta_column_fields")(dta, "clinical_data", id)
  base <- list(
    col_id = f$id, col_label = f$label, col_backend = f$backend,
    col_type = f$type, col_format = f$format, col_length = f$length,
    col_nullable = f$nullable, col_values = f$values,
    col_pattern = f$pattern, col_examples = f$examples,
    col_desc = f$description
  )
  utils::modifyList(base, list(...))
}

lint_rule_ids <- function(dta) {
  vapply(
    DTAtools::datasets(dta, "clinical_data")@specs@rules,
    function(r) r@id, character(1)
  )
}

lint_rule_list <- function(dta, id) {
  rl <- DTAtools::datasets(dta, "clinical_data")@specs@rules
  r <- rl[[match(id, lint_rule_ids(dta))]]
  app_fn("dta_rule_to_list")(r)
}

# ---- column delete ----------------------------------------------------------

test_that("dta_remove_column refuses a column rules use, naming the rules", {
  dta <- app_fixture_dta()

  res <- app_fn("dta_remove_column")(dta, "clinical_data", "AGE")

  expect_false(res$ok)
  expect_match(res$error, "Column AGE is used by rule(s) ", fixed = TRUE)
  expect_match(res$error, "rule_range_example", fixed = TRUE)
  expect_match(res$error, "check_col_condition_example", fixed = TRUE)
  expect_match(res$error, "remove or edit them first", fixed = TRUE)
})

test_that("dta_remove_column still removes a column no rule uses", {
  dta <- app_fixture_dta()

  res <- app_fn("dta_remove_column")(dta, "clinical_data", "BMI")

  expect_true(res$ok)
  expect_false("BMI" %in% app_fn("dta_column_ids")(res$value, "clinical_data"))
})

test_that("deleting a used column in the editor changes nothing", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    before <- app_fn("dta_column_ids")(rv$dta, "clinical_data")

    session$setInputs(col_del_click = match("SUBJECT_ID", before))

    expect_identical(app_fn("dta_column_ids")(rv$dta, "clinical_data"), before)
    expect_gt(length(lint_rule_ids(rv$dta)), 0)
  })
})

# ---- column rename ----------------------------------------------------------

test_that("renaming a column rewrites it in every kind of rule that uses it", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")

  res <- set_col(dta, "clinical_data", id = "SUBJ", type = "Char", nullable = FALSE, old_id = "SUBJECT_ID")
  expect_true(res$ok, info = res$error)
  out <- res$value

  ids <- app_fn("dta_column_ids")(out, "clinical_data")
  expect_true("SUBJ" %in% ids)
  expect_false("SUBJECT_ID" %in% ids)

  expect_identical(lint_rule_list(out, "rule_unique_example")$columns, c("SUBJ", "VISIT"))
  expect_identical(lint_rule_list(out, "group_condition_pass_example")$group_by, "SUBJ")
  # Nothing is left pointing at the old name.
  used <- unlist(lapply(lint_rule_ids(out), function(i) {
    app_fn("dta_rule_list_columns")(lint_rule_list(out, i))
  }))
  expect_false("SUBJECT_ID" %in% used)
})

test_that("renaming a middle column keeps its position", {
  dta <- app_fixture_dta()
  before <- app_fn("dta_column_ids")(dta, "clinical_data")
  at <- match("VISIT", before)
  # Neither first nor last, so appending or prepending would be caught.
  expect_true(at > 1L && at < length(before))

  res <- app_fn("dta_set_column")(
    dta, "clinical_data",
    id = "VIS", type = "Char", length = 12, nullable = TRUE, old_id = "VISIT"
  )
  expect_true(res$ok, info = res$error)
  after <- app_fn("dta_column_ids")(res$value, "clinical_data")
  expect_identical(after, replace(before, at, "VIS"))
})

test_that("renaming rewrites condition and then keys of a col_condition rule", {
  dta <- app_fixture_dta()

  res <- app_fn("dta_set_column")(
    dta, "clinical_data",
    id = "STAT", type = "Char", values = c("COMPLETED", "DROPPED", "PENDING"),
    nullable = TRUE, old_id = "STATUS"
  )
  expect_true(res$ok, info = res$error)

  r <- lint_rule_list(res$value, "rule_equal_example")
  expect_identical(names(r$condition), "VISIT")
  expect_identical(names(r$then), "STAT")
  expect_identical(r$then$STAT$equals, "COMPLETED")
  expect_identical(
    names(lint_rule_list(res$value, "group_condition_pass_example")$conditions$c_has_status),
    "STAT"
  )
})

test_that("saving a rename in the column editor keeps spec and rules consistent", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    ids <- app_fn("dta_column_ids")(rv$dta, "clinical_data")
    session$setInputs(col_edit_click = match("AGE", ids))
    do.call(session$setInputs, lint_col_form(rv$dta, "AGE", col_id = "PATIENT_AGE"))
    session$setInputs(col_save = 1)

    expect_null(rv$col_msg)
    expect_true("PATIENT_AGE" %in% app_fn("dta_column_ids")(rv$dta, "clinical_data"))
    expect_identical(lint_rule_list(rv$dta, "rule_range_example")$columns, "PATIENT_AGE")
    expect_equal(nrow(app_fn("dta_lint")(rv$dta)), 0L)
  })
})

# ---- column form ------------------------------------------------------------

test_that("a length that is not a whole number is refused, not dropped", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")

  for (bad in c("abc", "1.5", "-3", "8 bytes")) {
    res <- set_col(dta, "clinical_data", id = "BADLEN", type = "Char", length = bad)
    expect_false(res$ok, info = bad)
    expect_match(res$error, "Length must be a whole number", fixed = TRUE)
  }
  # Zero is a whole number but not a length.
  res0 <- set_col(dta, "clinical_data", id = "ZEROLEN", type = "Char", length = "0")
  expect_false(res0$ok)
  expect_match(res0$error, "at least 1", fixed = TRUE)
  # A blank length is still "no length", and a whole number is still stored.
  expect_true(set_col(dta, "clinical_data", id = "NOLEN", type = "Char", length = "")$ok)
  ok <- set_col(dta, "clinical_data", id = "LEN", type = "Char", length = " 12 ")
  expect_true(ok$ok, info = ok$error)
  st <- DTAtools::datasets(ok$value, "clinical_data")@specs@columns[["LEN"]]@structure
  expect_equal(as.numeric(st@length), 12)
})

test_that("a bad length in the column editor lands in col_msg and saves nothing", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    before <- app_fn("dta_column_ids")(rv$dta, "clinical_data")
    session$setInputs(col_add = 1)
    do.call(session$setInputs, list(
      col_id = "NEWCOL", col_label = "", col_backend = "SAS", col_type = "Char",
      col_format = "", col_length = "eight", col_nullable = TRUE, col_values = "",
      col_pattern = "", col_examples = "", col_desc = ""
    ))
    session$setInputs(col_save = 1)

    expect_false(isTRUE(rv$col_msg$ok))
    expect_match(rv$col_msg$error, "whole number", fixed = TRUE)
    expect_identical(app_fn("dta_column_ids")(rv$dta, "clinical_data"), before)
    # The message is rendered, not just stored.
    expect_match(output$col_editor_msg$html, "whole number", fixed = TRUE)
  })
})

test_that("column fields expose examples, and dta_set_column replaces or keeps them", {
  dta <- app_fixture_dta()
  set_col <- app_fn("dta_set_column")
  fields <- app_fn("dta_column_fields")

  made <- set_col(dta, "clinical_data", id = "TAG", type = "Char", examples = c("a", " b ", ""))
  expect_true(made$ok, info = made$error)
  expect_identical(fields(made$value, "clinical_data", "TAG")$examples, "a\nb")

  # NULL keeps them (a caller without the field), character(0) clears them.
  kept <- set_col(made$value, "clinical_data", id = "TAG", type = "Char")
  expect_identical(fields(kept$value, "clinical_data", "TAG")$examples, "a\nb")
  cleared <- set_col(made$value, "clinical_data", id = "TAG", type = "Char", examples = character(0))
  expect_identical(fields(cleared$value, "clinical_data", "TAG")$examples, "")
})

test_that("a type change that clashes with the old examples is fixed in the form", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    session$setInputs(col_add = 1)
    do.call(session$setInputs, list(
      col_id = "SCORE", col_label = "", col_backend = "SAS", col_type = "Char",
      col_format = "", col_length = "", col_nullable = TRUE, col_values = "",
      col_pattern = "", col_examples = "high\nlow", col_desc = ""
    ))
    session$setInputs(col_save = 1)
    expect_null(rv$col_msg)
    ids <- app_fn("dta_column_ids")(rv$dta, "clinical_data")
    expect_true("SCORE" %in% ids)

    # Edit it to Num while the old examples are still in the field.
    session$setInputs(col_edit_click = match("SCORE", ids))
    do.call(session$setInputs, lint_col_form(rv$dta, "SCORE", col_type = "Num"))
    session$setInputs(col_save = 1)
    expect_false(isTRUE(rv$col_msg$ok))
    expect_false(is.null(rv$col_msg$error))
    st <- DTAtools::datasets(rv$dta, "clinical_data")@specs@columns[["SCORE"]]@structure
    expect_identical(st@type, "Char") # nothing was saved

    # Fix the examples in the same form, and it saves.
    do.call(session$setInputs, lint_col_form(
      rv$dta, "SCORE",
      col_type = "Num", col_examples = "1.5\n2"
    ))
    session$setInputs(col_save = 1)
    expect_null(rv$col_msg)
    col <- DTAtools::datasets(rv$dta, "clinical_data")@specs@columns[["SCORE"]]
    expect_identical(col@structure@type, "Num")
    expect_equal(as.numeric(unlist(col@examples)), c(1.5, 2))
  })
})

# ---- rule editor ------------------------------------------------------------

test_that("dta_numeric_column_ids lists only Num and Int columns", {
  dta <- app_fixture_dta()

  expect_setequal(
    app_fn("dta_numeric_column_ids")(dta, "clinical_data"),
    c("AGE", "HEIGHT", "WEIGHT", "BMI")
  )
})

test_that("a range rule on a text column is refused with the reason in rv$rule_msg", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    session$setInputs(edit_rules = 1)
    n <- length(lint_rule_ids(rv$dta))
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_range")
    session$setInputs(
      rule_id = "range_on_text", rule_desc = "", rule_col_single = "GENDER",
      rule_min = "1", rule_max = "2"
    )
    session$setInputs(rule_save = 1)

    expect_false(isTRUE(rv$rule_msg$ok))
    expect_true(nzchar(rv$rule_msg$error))
    expect_length(lint_rule_ids(rv$dta), n)
  })
})

test_that("a duplicate rule id is refused", {
  res <- app_fn("dta_set_rule")(
    app_fixture_dta(), "clinical_data",
    id = "rule_range_example", type = "col_range", columns = "WEIGHT", min = 1, max = 2
  )

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- raw YAML ---------------------------------------------------------------

test_that("raw YAML naming an undeclared column is refused and lists the findings", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    before_ids <- lint_rule_ids(rv$dta)
    before_yaml <- rv$yaml_text
    bad <- sub("- SUBJECT_ID(\\s+)- VISIT", "- NOPE\\1- VISIT", before_yaml)
    expect_false(identical(bad, before_yaml))

    session$setInputs(raw_yaml_editor = bad)
    session$setInputs(apply_yaml = 1)

    expect_false(isTRUE(rv$yaml_msg$ok))
    expect_true(is.data.frame(rv$yaml_msg$findings))
    expect_true("rule_column_undeclared" %in% rv$yaml_msg$findings$code)
    # Nothing was applied.
    expect_identical(lint_rule_ids(rv$dta), before_ids)
    expect_identical(rv$yaml_text, before_yaml)
    expect_false("NOPE" %in% app_fn("dta_column_ids")(rv$dta, "clinical_data"))
    # Every finding is listed in the panel, one item each.
    html <- as.character(output$yaml_validation_msg$html)
    expect_match(html, "rule_column_undeclared", fixed = TRUE)
    expect_match(html, "<li", fixed = TRUE)
  })
})

test_that("a clean document renders no Spec check panel", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)

    expect_null(output$spec_check$html)
  })
})

test_that("the Spec check panel lists findings of the live document", {
  lint_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(lint_setup)
    # A warning-severity finding: rules but no declared column to check them
    # against. Built by hand because no editor can produce it.
    dta <- rv$dta
    ds <- DTAtools::datasets(dta, "clinical_data")
    specs <- ds@specs
    keep <- specs@rules[1]
    specs@rules <- NULL
    specs@columns <- list()
    specs@rules <- keep
    ds@specs <- specs
    dta@datasets[["clinical_data"]] <- ds
    rv$dta <- dta
    session$flushReact()

    html <- as.character(output$spec_check$html)
    expect_match(html, "rules_without_columns", fixed = TRUE)
    expect_match(html, "spec-finding warning", fixed = TRUE)
  })
})

# ---- a failed file load keeps its text -------------------------------------

lint_bad_file <- function() {
  txt <- paste(readLines(app_fixture_path("clinical_dta.yaml"), warn = FALSE), collapse = "\n")
  bad <- sub("- SUBJECT_ID(\\s+)- VISIT", "- NOPE\\1- VISIT", txt)
  expect_false(identical(bad, txt))
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
  writeLines(bad, path)
  list(
    txt = txt, bad = bad,
    input = data.frame(
      name = "bad.yaml", size = file.size(path), type = "", datapath = path,
      stringsAsFactors = FALSE
    )
  )
}

test_that("a refused file load changes nothing and opens a dialog that keeps its text", {
  lint_clean_session()
  f <- lint_bad_file()

  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_client_id = strrep("a", 32))
    session$setInputs(dta_file = lint_input("clinical_dta.yaml"))
    autosave_file <- list.files(tempdir(),
      pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
    )
    expect_length(autosave_file, 1L)
    saved_before <- readBin(autosave_file, "raw", file.size(autosave_file))
    dta_before <- rv$dta
    text_before <- rv$yaml_text
    locked_before <- rv$version_locked

    session$setInputs(dta_file = f$input)

    # The loaded document, its text, its locking state and the autosave are
    # exactly as they were.
    expect_identical(rv$dta, dta_before)
    expect_identical(rv$yaml_text, text_before)
    expect_identical(rv$version_locked, locked_before)
    expect_identical(readBin(autosave_file, "raw", file.size(autosave_file)), saved_before)

    # The dialog's state carries the findings and the file's text.
    expect_identical(rv$load_failure$name, "bad.yaml")
    expect_identical(rv$load_failure$text, f$bad)
    expect_true("rule_column_undeclared" %in% rv$load_failure$findings$code)
    html <- as.character(app_fn("dta_load_failure_modal")(rv$load_failure))
    expect_match(html, "Could not load bad.yaml", fixed = TRUE)
    expect_match(html, "rule_column_undeclared", fixed = TRUE)
    expect_match(html, paste0("load_failure_text_", rv$load_failure$nonce), fixed = TRUE)
    expect_match(html, "Load corrected YAML", fixed = TRUE)
  })
})

test_that("Load corrected YAML loads fixed text like a file load, and a still-broken text keeps the dialog", {
  lint_clean_session()
  f <- lint_bad_file()

  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = f$input)
    n1 <- rv$load_failure$nonce

    # Still broken: nothing loads, the dialog is re-opened with fresh findings
    # and a new text-box id (so no cached value is re-inserted).
    nid <- paste0("load_failure_text_", n1)
    do.call(session$setInputs, stats::setNames(list(paste0(f$bad, "\n# edited")), nid))
    session$setInputs(load_corrected_yaml = 1)
    expect_null(rv$dta)
    expect_true("rule_column_undeclared" %in% rv$load_failure$findings$code)
    expect_match(rv$load_failure$text, "# edited", fixed = TRUE)
    expect_gt(rv$load_failure$nonce, n1)

    # Fixed: loaded as a file load, i.e. versioned, with the file's own version.
    nid <- paste0("load_failure_text_", rv$load_failure$nonce)
    do.call(session$setInputs, stats::setNames(list(f$txt), nid))
    session$setInputs(load_corrected_yaml = 2)
    expect_null(rv$load_failure)
    expect_true("clinical_data" %in% app_fn("dta_dataset_names")(rv$dta))
    expect_identical(as.character(DTAtools::metadata(rv$dta)@version), "0.1")
    expect_true(isTRUE(rv$version_locked))
    expect_identical(rv$version_baseline_yaml, f$txt)
    expect_false(isTRUE(rv$editing))
  })
})

# ---- the linter wrapper ------------------------------------------------------

test_that("dta_lint never throws, is empty for clean input and reports a crash as lint_failed", {
  lint <- app_fn("dta_lint")

  expect_equal(nrow(lint(NULL)), 0L)
  expect_equal(nrow(lint(app_fixture_dta())), 0L)
  expect_named(
    lint(NULL),
    c("dataset", "object", "id", "severity", "code", "message")
  )

  # A linter that cannot run is not a clean document.
  out <- lint("no-such-file.yaml")
  expect_equal(nrow(out), 1L)
  expect_identical(out$severity, "error")
  expect_identical(out$code, "lint_failed")
  expect_match(out$message, "does not exist", fixed = TRUE)
  # ... and the Spec check panel can list it.
  expect_match(as.character(app_fn("dta_findings_ui")(out)), "lint_failed", fixed = TRUE)
})
