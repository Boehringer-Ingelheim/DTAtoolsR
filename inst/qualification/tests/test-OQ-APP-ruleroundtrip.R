# Opening a rule in the rule editor and saving it without a change.
# REQ-APP-063 and REQ-APP-064.
#
# The fixture is the worked laboratory example of DTA_YAML_GUIDE.md (section 5)
# with the operator rows its operator table describes. Every expected clause is
# what that guide says the clause means: `range: [lo, hi]` is `min: lo` with
# `max: hi`, `equal`/`not_equal` are `equals`/`not_equals`, several operators on
# one column are all required, and a quoted value is text. None is read back from
# the application.
#
# testServer() has no browser, so what the browser would have reported for the
# form the server rendered is read off that HTML (qapp_browser_inputs()); the
# test never names an operator or a value itself.

qrr_fixture <- c(
  "metadata:",
  "  title: Laboratory results specification",
  "  version: '1.0'",
  "datasets:",
  "  - id: lb",
  "    type: tabular",
  "    name: Laboratory results",
  "    files: {filename: lb.csv, type: csv}",
  "    columns:",
  "      - {id: VISIT, type: SAS Char, length: 20, nullable: false}",
  "      - {id: LBTESTCD, type: SAS Char, length: 8, nullable: false}",
  "      - {id: LBORRES, type: SAS Num, nullable: true}",
  "      - {id: LBORRESU, type: SAS Char, length: 10, nullable: true}",
  "      - {id: SITEID, type: SAS Char, length: 3, nullable: true}",
  "      - {id: LBNAM, type: SAS Char, length: 40, nullable: true}",
  "    rules:",
  "      - id: glucose_unit",
  "        type: col_condition",
  "        description: Glucose is reported in mmol/L.",
  "        condition:",
  "          LBTESTCD: { equals: GLUC }",
  "          LBORRES: { empty: false }",
  "        then:",
  "          LBORRESU: { equals: mmol/L }",
  "          LBORRES: { range: [0.5, 40] }",
  "      - id: hba1c_unit",
  "        type: col_condition",
  "        description: HbA1c is reported in percent within a plausible range.",
  "        condition:",
  "          LBTESTCD: { equals: HBA1C }",
  "          LBORRES: { empty: false }",
  "        then:",
  "          LBORRESU: { equals: \"%\" }",
  "          LBORRES: { range: [3, 20] }",
  "      - id: result_after_screening",
  "        type: col_condition",
  "        description: A result outside screening is reported (not_equal is not_equals).",
  "        condition:",
  "          VISIT: { not_equal: SCREENING }",
  "        then:",
  "          LBORRES: { empty: false }",
  "      - id: result_band",
  "        type: col_condition",
  "        description: Several operators on one column are all required.",
  "        condition:",
  "          LBTESTCD: { equal: GLUC }",
  "        then:",
  "          LBORRES: { greater_equal: 0.5, less: 40 }",
  "      - id: site_codes",
  "        type: col_condition",
  "        description: Site codes are text, and a name may hold a comma.",
  "        condition:",
  "          SITEID: { in: [\"001\", \"002\"] }",
  "        then:",
  "          LBNAM: { in: [\"Smith, John\", \"Doe\"] }",
  "      - id: site_seven",
  "        type: col_condition",
  "        description: A site code with leading zeros stays text.",
  "        condition:",
  "          SITEID: { equals: \"007\" }",
  "        then:",
  "          LBTESTCD: { not_equals: GLUC }",
  "      - id: glucose_per_site",
  "        type: group_condition",
  "        group_by: [SITEID]",
  "        conditions:",
  "          c_glucose:",
  "            LBTESTCD: { equals: GLUC }",
  "            LBORRES: { range: [0.5, 40] }",
  "          c_other:",
  "            LBTESTCD: { not_equal: GLUC }",
  "        constraints:",
  "          - id: glucose_excludes_other",
  "            type: mutually_exclusive",
  "            left: c_glucose",
  "            right: c_other",
  "      - id: result_limits",
  "        type: col_range",
  "        description: A plausible range for the result.",
  "        columns: LBORRES",
  "        min: 0.5",
  "        max: 100000",
  "      - id: band_twice",
  "        type: col_condition",
  "        description: Two spellings of the one between operator on a column.",
  "        condition:",
  "          VISIT: { equals: V03 }",
  "        then:",
  "          LBORRES: { range: [0, 100], min: 5 }"
)

# The position of each rule in the fixture.
qrr_index <- c(
  glucose_unit = 1L, hba1c_unit = 2L, result_after_screening = 3L, result_band = 4L,
  site_codes = 5L, site_seven = 6L, glucose_per_site = 7L, result_limits = 8L,
  band_twice = 9L
)

# Press Save. A mock session ignores a value it already holds, so every press
# carries a new one.
qrr_presses <- new.env()
qrr_presses$n <- 0L
qrr_save <- function(session) {
  qrr_presses$n <- qrr_presses$n + 1L
  session$setInputs(rule_save = qrr_presses$n)
}

qrr_rule <- function(rv, id) {
  rules <- DTAtools::datasets(rv$dta, "lb")@specs@rules
  hit <- Filter(function(r) identical(r@id, id), rules)
  qa_check(paste0("the rule ", id, " is in the document"), length(hit) == 1L)
  qapp_fn("dta_rule_to_list")(hit[[1]])
}

# Open a rule with its pencil and hand the server what the browser reports for
# the form it rendered.
qrr_open <- function(session, output, id) {
  session$setInputs(rule_edit_click = unname(qrr_index[[id]]))
  inputs <- qapp_browser_inputs(qapp_ui_text(output$rule_modal_body))
  do.call(session$setInputs, inputs)
  invisible(inputs)
}

qrr_start <- function(session) {
  path <- file.path(qa_tempdir(), "lb.yaml")
  writeLines(qrr_fixture, path)
  session$setInputs(dta_client_id = strrep("r", 32))
  session$setInputs(dta_file = qapp_upload_value(path, "lb.yaml"))
  qapp_enter_edit_mode(session)
  session$setInputs(edit_rules = 1)
}

test_that("OQ-APP-076 | a rule written with range opens in the rule editor and saves, unchanged, as the same band of min and max | REQ-APP-063", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)

    for (id in c("glucose_unit", "hba1c_unit")) {
      qrr_open(session, output, id)
      qrr_save(session)
      qa_check(paste0("saving ", id, " reports no error"), is.null(rv$rule_msg))
    }

    # Guide, operators table: `range` | [lo, hi] | inclusive band, the same band
    # `min` + `max` give.
    glucose <- qrr_rule(rv, "glucose_unit")
    qa_step(
      "the glucose range is saved as min 0.5 and max 40",
      list(LBORRESU = list(equals = "mmol/L"), LBORRES = list(min = 0.5, max = 40L)),
      glucose$then
    )
    qa_step(
      "and its IF clause is untouched",
      list(LBTESTCD = list(equals = "GLUC"), LBORRES = list(empty = FALSE)),
      glucose$condition
    )
    hba1c <- qrr_rule(rv, "hba1c_unit")
    qa_step(
      "the HbA1c range is saved as min 3 and max 20",
      list(LBORRESU = list(equals = "%"), LBORRES = list(min = 3L, max = 20L)),
      hba1c$then
    )
  })
})

test_that("OQ-APP-077 | several operators on one column and the equal and not_equal spellings are saved as the same tests | REQ-APP-063", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)

    for (id in c("result_after_screening", "result_band")) {
      qrr_open(session, output, id)
      qrr_save(session)
      qa_check(paste0("saving ", id, " reports no error"), is.null(rv$rule_msg))
    }

    # `not_equal` is the negation: it must not come back as `equals`.
    qa_step(
      "not_equal: SCREENING is saved as not_equals: SCREENING",
      list(VISIT = list(not_equals = "SCREENING")),
      qrr_rule(rv, "result_after_screening")$condition
    )
    qa_step(
      "equal: GLUC is saved as equals: GLUC",
      list(LBTESTCD = list(equals = "GLUC")),
      qrr_rule(rv, "result_band")$condition
    )
    # Guide: several operators on one column are ANDed; both must survive.
    qa_step(
      "greater_equal 0.5 and less 40 are both kept on the column",
      list(LBORRES = list(greater_equal = 0.5, less = 40L)),
      qrr_rule(rv, "result_band")$then
    )
  })
})

test_that("OQ-APP-078 | a text code and a list member holding a comma are saved as the text they were | REQ-APP-063", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)

    for (id in c("site_codes", "site_seven")) {
      qrr_open(session, output, id)
      qrr_save(session)
      qa_check(paste0("saving ", id, " reports no error"), is.null(rv$rule_msg))
    }

    # SITEID is a Char column: its codes are text, leading zeros included.
    codes <- qrr_rule(rv, "site_codes")
    qa_step(
      "the site codes 001 and 002 stay text",
      list(SITEID = list(`in` = c("001", "002"))),
      codes$condition
    )
    qa_step(
      "and the name with a comma is one member of the list",
      list(LBNAM = list(`in` = c("Smith, John", "Doe"))),
      codes$then
    )
    qa_step(
      "the site code 007 stays text",
      list(SITEID = list(equals = "007")),
      qrr_rule(rv, "site_seven")$condition
    )
  })
})

test_that("OQ-APP-079 | a grouped condition opened in the rule editor and saved without a change keeps every test of every condition | REQ-APP-063", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)
    qrr_open(session, output, "glucose_per_site")
    qrr_save(session)
    qa_check("saving the grouped rule reports no error", is.null(rv$rule_msg))

    saved <- qrr_rule(rv, "glucose_per_site")
    qa_step(
      "both conditions are kept, a range as min and max and not_equal as not_equals",
      list(
        c_glucose = list(
          LBTESTCD = list(equals = "GLUC"),
          LBORRES = list(min = 0.5, max = 40L)
        ),
        c_other = list(LBTESTCD = list(not_equals = "GLUC"))
      ),
      saved$conditions
    )
    qa_step("the grouping column is kept", "SITEID", saved$group_by)
    qa_step("and so is the constraint", "glucose_excludes_other", saved$constraints[[1]]$id)
  })
})

test_that("OQ-APP-080 | a value the rule editor cannot read is refused with the text named and the rule is left as it was | REQ-APP-064", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)
    flat <- function(x) gsub("\\s+", " ", x)

    # --- a conditional rule: a bound that is not a number -------------------------
    qrr_open(session, output, "result_band")
    before <- qrr_rule(rv, "result_band")

    # Row 2 of THEN is `less`; "18 - 65" is not a number.
    session$setInputs(then_val_2 = "18 - 65")
    qrr_save(session)
    qa_check("a bound that is not a number is refused", isFALSE(rv$rule_msg$ok))
    qa_check("and the message names the text typed", grepl("18 - 65", flat(rv$rule_msg$error), fixed = TRUE))
    qa_step("the rule is unchanged", before, qrr_rule(rv, "result_band"))

    # A decimal comma is not a decimal: "0,5" is not read as 0 and 5.
    session$setInputs(then_val_2 = "40", then_val_1 = "0,5")
    qrr_save(session)
    qa_check("a decimal comma is refused", isFALSE(rv$rule_msg$ok))
    qa_check("and the message names it", grepl("0,5", flat(rv$rule_msg$error), fixed = TRUE))
    qa_step("and the rule is still unchanged", before, qrr_rule(rv, "result_band"))

    # A blank value is not a value.
    session$setInputs(then_val_1 = "")
    qrr_save(session)
    qa_check("a row with a column and no value is refused", isFALSE(rv$rule_msg$ok))
    qa_step("and the rule is unchanged", before, qrr_rule(rv, "result_band"))

    # `less` a second time for the same column.
    session$setInputs(then_val_1 = "0.5")
    session$setInputs(then_add = 1)
    session$setInputs(then_col_3 = "LBORRES", then_op_3 = "less", then_val_3 = "30")
    qrr_save(session)
    qa_check("the same operator twice on one column is refused", isFALSE(rv$rule_msg$ok))
    qa_check("and the message names the column", grepl("LBORRES", rv$rule_msg$error, fixed = TRUE))
    qa_step("and the rule is unchanged", before, qrr_rule(rv, "result_band"))

    # --- a between row: more than two bounds -------------------------------------
    qrr_open(session, output, "glucose_unit")
    before_unit <- qrr_rule(rv, "glucose_unit")
    session$setInputs(then_val_2 = "0.5, 40, 50")
    qrr_save(session)
    qa_check("a between row with three bounds is refused", isFALSE(rv$rule_msg$ok))
    qa_step("and the rule is unchanged", before_unit, qrr_rule(rv, "glucose_unit"))

    # --- a list with a double quote that is never closed ---------------------------
    qrr_open(session, output, "site_codes")
    before_codes <- qrr_rule(rv, "site_codes")
    session$setInputs(then_val_1 = "\"Smith, John, Doe")
    qrr_save(session)
    qa_check("a list with an unclosed double quote is refused", isFALSE(rv$rule_msg$ok))
    qa_step("and the rule is unchanged", before_codes, qrr_rule(rv, "site_codes"))

    # --- a range rule: a bound with a decimal comma ------------------------------
    qrr_open(session, output, "result_limits")
    before_limits <- qrr_rule(rv, "result_limits")
    session$setInputs(rule_min = "0,5")
    qrr_save(session)
    qa_check("a range rule's minimum of 0,5 is refused", isFALSE(rv$rule_msg$ok))
    qa_check("and the message names it", grepl("0,5", flat(rv$rule_msg$error), fixed = TRUE))
    qa_step("and the rule keeps its minimum", before_limits, qrr_rule(rv, "result_limits"))
  })
})

test_that("OQ-APP-081 | a range rule opened in the rule editor and saved without a change keeps its bounds as they were | REQ-APP-063", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)
    qrr_open(session, output, "result_limits")
    qrr_save(session)
    qa_check("saving the range rule reports no error", is.null(rv$rule_msg))

    saved <- qrr_rule(rv, "result_limits")
    qa_step("the minimum is 0.5", 0.5, saved$min)
    # YAML reads 100000 as an integer; it must not come back as 1e+05.
    qa_step("the maximum is the integer 100000", 100000L, saved$max)
  })
})

test_that("OQ-APP-082 | a rule the rule editor cannot show faithfully is not opened, so nothing is saved as something else | REQ-APP-064", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)
    before <- rv$dta

    # `range: [0, 100]` and `min: 5` on one column are two keys of the one
    # between row; a form row per operator cannot hold both.
    session$setInputs(rule_edit_click = unname(qrr_index[["band_twice"]]))
    qa_step("the pencil opens no form", "list", rv$rule_view)
    qa_check("and no rule is being edited", is.null(rv$rule_edit_index))
    qa_step("and the document is untouched", before, rv$dta)
  })
})

test_that("OQ-APP-083 | a range rule saved with its maximum or its minimum box left blank has no bound on that side and reopens with that box blank | REQ-APP-065", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qrr_start(session)
    qrr_open(session, output, "result_limits")
    session$setInputs(rule_min = "5", rule_max = "")
    qrr_save(session)
    qa_check("saving the rule reports no error", is.null(rv$rule_msg))

    saved <- qrr_rule(rv, "result_limits")
    qa_step("the minimum is the 5 that was typed", 5, saved$min)
    qa_step("there is no upper bound", Inf, saved$max)

    # Opened again, the Maximum box is blank and the Minimum shows its number.
    session$setInputs(rule_edit_click = unname(qrr_index[["result_limits"]]))
    shown <- qapp_browser_inputs(qapp_ui_text(output$rule_modal_body))
    qa_step("the Maximum box is blank", "", shown$rule_max)
    qa_step("the Minimum box shows 5", "5", shown$rule_min)

    # The other side: a blank Minimum box leaves no lower bound.
    session$setInputs(rule_min = "", rule_max = "7")
    qrr_save(session)
    qa_check("saving with the Minimum box blank reports no error", is.null(rv$rule_msg))
    saved <- qrr_rule(rv, "result_limits")
    qa_step("there is no lower bound", -Inf, saved$min)
    qa_step("the maximum is the 7 that was typed", 7, saved$max)

    # Both boxes blank is refused and leaves the rule as it was.
    session$setInputs(rule_edit_click = unname(qrr_index[["result_limits"]]))
    session$setInputs(rule_min = "", rule_max = "")
    qrr_save(session)
    qa_check("a range with both boxes blank is refused", isFALSE(rv$rule_msg$ok))
    qa_step("the saved rule is unchanged", saved, qrr_rule(rv, "result_limits"))
  })
})
