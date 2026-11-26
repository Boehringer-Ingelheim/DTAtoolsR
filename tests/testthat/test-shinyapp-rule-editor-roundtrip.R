# The rule editor, driven through the real server: a rule opened with the pencil
# and saved without a change keeps its meaning.
#
# testServer() has no browser, so every control the browser would have reported
# is supplied by the test. app_browser_inputs() reads those values off the HTML
# the server rendered, the way a browser does (a <select> without a matching
# option reports its first option), instead of the test naming the operator and
# the value itself. Every expectation is written out by hand from the YAML guide.

rr_fixture <- c(
  "metadata:",
  "  title: Rule editor round trip",
  "  version: '0.1'",
  "datasets:",
  "  - id: lab",
  "    type: tabular",
  "    name: Laboratory results",
  "    files: {filename: lab.csv, type: csv}",
  "    columns:",
  "      - {id: LBORRES, type: SAS Num, nullable: true}",
  "      - {id: VISIT, type: SAS Char, length: 12, nullable: true}",
  "      - {id: AGE, type: SAS Num, nullable: true}",
  "      - {id: SITEID, type: SAS Char, length: 3, nullable: true}",
  "      - {id: LBNAM, type: SAS Char, length: 20, nullable: true}",
  "    rules:",
  "      - id: glucose_range",
  "        type: col_condition",
  "        condition: {VISIT: {equals: V03}}",
  "        then: {LBORRES: {range: [0.5, 40]}}",
  "      - id: not_screening",
  "        type: col_condition",
  "        condition: {VISIT: {not_equal: SCREENING}}",
  "        then: {LBORRES: {empty: false}}",
  "      - id: adults_only",
  "        type: col_condition",
  "        condition: {SITEID: {in: ['001', '002']}}",
  "        then: {AGE: {greater_equal: 18, less: 65}}",
  "      - id: text_codes",
  "        type: col_condition",
  "        condition: {SITEID: {equals: '007'}}",
  "        then: {LBNAM: {in: ['Smith, John', 'Doe']}}",
  "      - id: age_band",
  "        type: col_range",
  "        columns: AGE",
  "        min: 0.30000000000000004",
  "        max: 100000",
  "      - id: two_if_rows",
  "        type: col_condition",
  "        condition:",
  "          VISIT: {equals: V03}",
  "          SITEID: {in: ['001', '002']}",
  "        then: {LBORRES: {min: 0.5}}",
  "      - id: adult_group",
  "        type: group_condition",
  "        group_by: [SITEID]",
  "        conditions:",
  "          c_adult:",
  "            AGE: {greater_equal: 18, less: 65}",
  "          c_band:",
  "            LBORRES: {range: [0.5, 40]}",
  "            VISIT: {not_equal: SCREENING}",
  "        constraints:",
  "          - id: adult_needs_band",
  "            type: requires",
  "            if: c_adult",
  "            then: c_band",
  "            if_scope: any",
  "            then_scope: all"
)

# The position of each rule in the fixture.
rr_index <- c(
  glucose_range = 1L, not_screening = 2L, adults_only = 3L, text_codes = 4L,
  age_band = 5L, two_if_rows = 6L, adult_group = 7L
)

rr_rule <- function(rv, id) {
  rules <- DTAtools::datasets(rv$dta, "lab")@specs@rules
  hit <- Filter(function(r) identical(r@id, id), rules)
  expect_length(hit, 1L)
  app_fn("dta_rule_to_list")(hit[[1]])
}

# Open the rule `id` with its pencil and hand the server what the browser
# reports for the form it rendered.
rr_open <- function(session, rv, output, id) {
  session$setInputs(rule_edit_click = unname(rr_index[[id]]))
  html <- ui_text(output$rule_modal_body)
  inputs <- app_browser_inputs(html)
  inputs <- inputs[lengths(inputs) > 0L]
  do.call(session$setInputs, inputs)
  invisible(inputs)
}

# Press Save. Every press carries a new value: a mock session ignores a value it
# already holds, so a second press of `rule_save = 1` would not reach the server.
rr_presses <- new.env()
rr_presses$n <- 0L
rr_save <- function(session) {
  rr_presses$n <- rr_presses$n + 1L
  session$setInputs(rule_save = rr_presses$n)
}

rr_start <- function(session) {
  path <- tempfile(fileext = ".yaml")
  writeLines(rr_fixture, path)
  session$setInputs(dta_file = app_upload_value(path, "doc.yaml"))
  unlock_editing(session)
  session$setInputs(edit_rules = 1)
}

test_that("every rule of the fixture opens and saves without a change", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    before <- lapply(names(rr_index), function(id) rr_rule(rv, id))
    names(before) <- names(rr_index)

    # The clauses the guide means, written by hand: aliases in their normal form,
    # values with the type and spelling they had.
    expected <- list(
      glucose_range = list(
        condition = list(VISIT = list(equals = "V03")),
        then = list(LBORRES = list(min = 0.5, max = 40L))
      ),
      not_screening = list(
        condition = list(VISIT = list(not_equals = "SCREENING")),
        then = list(LBORRES = list(empty = FALSE))
      ),
      adults_only = list(
        condition = list(SITEID = list(`in` = c("001", "002"))),
        then = list(AGE = list(greater_equal = 18L, less = 65L))
      ),
      text_codes = list(
        condition = list(SITEID = list(equals = "007")),
        then = list(LBNAM = list(`in` = c("Smith, John", "Doe")))
      ),
      two_if_rows = list(
        condition = list(
          VISIT = list(equals = "V03"),
          SITEID = list(`in` = c("001", "002"))
        ),
        then = list(LBORRES = list(min = 0.5))
      )
    )

    for (id in names(expected)) {
      rr_open(session, rv, output, id)
      rr_save(session)
      expect_null(rv$rule_msg, info = id)
      expect_identical(rr_rule(rv, id)$condition, expected[[id]]$condition, info = id)
      expect_identical(rr_rule(rv, id)$then, expected[[id]]$then, info = id)
    }
  })
})

test_that("a grouped condition opened and saved without a change keeps every operator", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "adult_group")
    rr_save(session)
    expect_null(rv$rule_msg, info = rv$rule_msg$error)

    saved <- rr_rule(rv, "adult_group")
    expect_identical(
      saved$conditions,
      list(
        c_adult = list(AGE = list(greater_equal = 18L, less = 65L)),
        c_band = list(
          LBORRES = list(min = 0.5, max = 40L),
          VISIT = list(not_equals = "SCREENING")
        )
      )
    )
    expect_identical(saved$group_by, "SITEID")
    expect_identical(saved$constraints[[1]]$id, "adult_needs_band")
    expect_identical(saved$constraints[[1]]$`if`, "c_adult")
  })
})

test_that("a range rule opened and saved without a change keeps its bounds", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "age_band")
    rr_save(session)
    expect_null(rv$rule_msg)
    saved <- rr_rule(rv, "age_band")
    expect_identical(saved$min, 0.30000000000000004)
    expect_identical(saved$max, 100000L)
  })
})

test_that("a range bound written with a decimal comma is refused, not dropped", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "age_band")
    before <- rr_rule(rv, "age_band")
    session$setInputs(rule_min = "0,5")
    rr_save(session)

    expect_false(isTRUE(rv$rule_msg$ok))
    expect_match(rv$rule_msg$error, "0,5", fixed = TRUE)
    expect_identical(rr_rule(rv, "age_band"), before)
  })
})

test_that("an operator row added beside an untouched one is merged into the same column", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "adults_only")
    session$setInputs(then_add = 1)
    session$setInputs(then_col_3 = "AGE", then_op_3 = "not_equals", then_val_3 = "40")
    rr_save(session)

    expect_null(rv$rule_msg)
    # the two operators it already had are untouched (18L, not 18), the new one is
    # a number because AGE is a numeric column
    expect_identical(
      rr_rule(rv, "adults_only")$then,
      list(AGE = list(greater_equal = 18L, less = 65L, not_equals = 40))
    )
  })
})

test_that("the same operator twice on one column is refused on Save and nothing is saved", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "adults_only")
    before <- rr_rule(rv, "adults_only")
    session$setInputs(then_add = 1)
    session$setInputs(then_col_3 = "AGE", then_op_3 = "less", then_val_3 = "70")
    rr_save(session)

    expect_false(isTRUE(rv$rule_msg$ok))
    expect_match(rv$rule_msg$error, "AGE", fixed = TRUE)
    expect_identical(rr_rule(rv, "adults_only"), before)
  })
})

test_that("a number that cannot be read is an error naming the clause, and the rule is not changed", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "adults_only")
    before <- rr_rule(rv, "adults_only")
    session$setInputs(then_val_2 = "18 - 65")
    rr_save(session)

    expect_false(isTRUE(rv$rule_msg$ok))
    expect_match(rv$rule_msg$error, "THEN", fixed = TRUE)
    expect_match(rv$rule_msg$error, "18 - 65", fixed = TRUE)
    expect_identical(rr_rule(rv, "adults_only"), before)
  })
})

test_that("a text code that is edited stays text on a text column", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "text_codes")
    session$setInputs(cond_val_1 = "008")
    session$setInputs(then_val_1 = "\"O'Brien, Pat\", Doe")
    rr_save(session)

    expect_null(rv$rule_msg)
    saved <- rr_rule(rv, "text_codes")
    expect_identical(saved$condition, list(SITEID = list(equals = "008")))
    expect_identical(saved$then, list(LBNAM = list(`in` = c("O'Brien, Pat", "Doe"))))
  })
})

test_that("removing a row keeps the other rows' own original values", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "two_if_rows")
    session$setInputs(cond_remove_1 = 1)
    rr_save(session)

    expect_null(rv$rule_msg)
    expect_identical(
      rr_rule(rv, "two_if_rows")$condition,
      list(SITEID = list(`in` = c("001", "002")))
    )
  })
})

test_that("a row added to a new rule is read by the type of its column", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_condition")
    session$setInputs(
      rule_id = "typed_rows", rule_desc = "",
      cond_col_1 = "SITEID", cond_op_1 = "in", cond_val_1 = "001, 002",
      then_col_1 = "AGE", then_op_1 = "min_max", then_val_1 = "18, 65"
    )
    rr_save(session)

    expect_null(rv$rule_msg)
    saved <- rr_rule(rv, "typed_rows")
    expect_identical(saved$condition, list(SITEID = list(`in` = c("001", "002"))))
    expect_identical(saved$then, list(AGE = list(min = 18, max = 65)))
  })
})

test_that("the editor's hint says what it does with aliases, several operators and quoted lists", {
  skip_if_not_installed("xml2")
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    session$setInputs(rule_edit_click = 1L)
    doc <- xml2::read_html(paste0("<body>", ui_text(output$rule_modal_body), "</body>"))
    # only the hint paragraphs: the rule id and the operator options also hold
    # words such as "range" and "not_equals"
    hint <- paste(xml2::xml_text(xml2::xml_find_all(doc, "//div[contains(@class, 'cond-hint')]")), collapse = " ")
    expect_match(hint, "double quotes", fixed = TRUE)
    expect_match(hint, "range", fixed = TRUE)
    expect_match(hint, "not_equal", fixed = TRUE)
    expect_match(hint, "Several rows", fixed = TRUE)
    expect_match(hint, "decimals with a point", ignore.case = TRUE)
  })
})

test_that("a row with a value and no column is refused on Save, not dropped", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "adults_only")
    before <- rr_rule(rv, "adults_only")
    session$setInputs(cond_add = 1)
    session$setInputs(cond_col_2 = "", cond_op_2 = "equals", cond_val_2 = "V03")
    rr_save(session)

    expect_false(isTRUE(rv$rule_msg$ok))
    expect_match(rv$rule_msg$error, "V03", fixed = TRUE)
    expect_identical(rr_rule(rv, "adults_only"), before)
  })
})

test_that("a grouped condition row with a column and no name is refused on Save, not dropped", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "adult_group")
    before <- rr_rule(rv, "adult_group")
    session$setInputs(gcond_add = 1)
    session$setInputs(gcond_name_5 = "", gcond_col_5 = "VISIT", gcond_op_5 = "equals", gcond_val_5 = "V03")
    rr_save(session)

    expect_false(isTRUE(rv$rule_msg$ok))
    expect_match(rv$rule_msg$error, "VISIT", fixed = TRUE)
    expect_identical(rr_rule(rv, "adult_group"), before)
  })
})

test_that("a range rule saved with only a Maximum has an open lower side and reopens with a blank Minimum", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    rr_start(session)
    rr_open(session, rv, output, "age_band")
    session$setInputs(rule_min = "", rule_max = "10")
    rr_save(session)
    expect_null(rv$rule_msg, info = rv$rule_msg$error)

    rules <- DTAtools::datasets(rv$dta, "lab")@specs@rules
    rule <- Filter(function(r) identical(r@id, "age_band"), rules)[[1]]
    expect_identical(rule@min, -Inf)
    expect_identical(rule@max, 10)
    # No value above the maximum passes; a very negative value does
    result <- rule_check_range(rule, data.frame(AGE = c(-1e9, 10, 11)))
    expect_false(result$valid)
    expect_match(result$message, "violated: 1 rows")
    expect_true(rule_check_range(rule, data.frame(AGE = c(-1e9, 0, 10)))$valid)

    # Reopened: the Minimum box is blank, the Maximum shows its number
    session$setInputs(rule_edit_click = unname(rr_index[["age_band"]]))
    html <- ui_text(output$rule_modal_body)
    inputs <- app_browser_inputs(html)
    expect_identical(inputs$rule_min, "")
    expect_identical(inputs$rule_max, "10")

    # ... and saved unchanged keeps the open side
    rr_open(session, rv, output, "age_band")
    rr_save(session)
    expect_null(rv$rule_msg, info = rv$rule_msg$error)
    saved <- rr_rule(rv, "age_band")
    expect_identical(saved$min, -Inf)
    expect_identical(saved$max, 10)
  })
})
