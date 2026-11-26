# The rule editor's conversions between a condition clause and form rows
# (inst/shiny/dta_app/R/rule_conditions.R).
#
# The principle under test: opening a rule and saving it without a change keeps
# its meaning. Every expected clause below is WRITTEN OUT BY HAND from what the
# YAML guide says the clause means (an alias becomes its normal form, a value
# keeps its type and spelling); none is produced by running the code under test.
# The clauses go in as YAML text, read with yaml::yaml.load() as the app's own
# reader does, so the shapes are the ones the editor really receives: an integer
# stays an integer, `[0.5, 40]` is a list because its two members differ in type,
# `["001", "002"]` is a character vector.

yclause <- function(text) yaml::yaml.load(text)

# What the browser reports for rows that were opened and not touched: each row
# carries the row it was opened from.
untouched <- function(rows) {
  lapply(rows, function(r) list(col = r$col, op = r$op, val = r$val, orig = r))
}

# Open a clause in the editor and save it again without a change.
reopen <- function(clause, families = character(0)) {
  rows <- app_fn("condition_to_form_rows")(clause)
  app_fn("form_rows_to_condition")(untouched(rows), families)
}

# A row the user typed (no row it was opened from).
typed <- function(col, op, val) list(col = col, op = op, val = val)

# ---- open and save without a change ---------------------------------------------

test_that("opening a clause and saving it without a change keeps what it means", {
  # name = the clause as YAML; expected = what must come out, spelled out.
  corpus <- list(
    # `range` is the alias of `min` + `max` (guide, operators table)
    list(
      yaml = "LBORRES: {range: [0.5, 40]}",
      expected = list(LBORRES = list(min = 0.5, max = 40L))
    ),
    list(
      yaml = "AGE: {range: [18, 65]}",
      expected = list(AGE = list(min = 18L, max = 65L))
    ),
    # `not_equal` is `not_equals`: it must stay a negation
    list(
      yaml = "VISIT: {not_equal: SCREENING}",
      expected = list(VISIT = list(not_equals = "SCREENING"))
    ),
    list(
      yaml = "VISIT: {equal: V03}",
      expected = list(VISIT = list(equals = "V03"))
    ),
    # several operators on one column, as the guide prescribes
    list(
      yaml = "AGE: {greater_equal: 18, less: 65}",
      expected = list(AGE = list(greater_equal = 18L, less = 65L))
    ),
    # text codes keep their zeros and decimals
    list(
      yaml = "SITEID: {in: [\"001\", \"002\"]}",
      expected = list(SITEID = list(`in` = c("001", "002")))
    ),
    list(
      yaml = "SITEID: {equals: \"007\"}",
      expected = list(SITEID = list(equals = "007"))
    ),
    list(
      yaml = "RES: {equals: \"1.50\"}",
      expected = list(RES = list(equals = "1.50"))
    ),
    # a value that holds a comma stays one value
    list(
      yaml = "LBNAM: {in: [\"Smith, John\", \"Doe\"]}",
      expected = list(LBNAM = list(`in` = c("Smith, John", "Doe")))
    ),
    list(
      yaml = "LBNAM: {not_in: [\"Smith, John\", \"Doe\"]}",
      expected = list(LBNAM = list(not_in = c("Smith, John", "Doe")))
    ),
    list(
      yaml = "LBORRES: {empty: false}",
      expected = list(LBORRES = list(empty = FALSE))
    ),
    list(
      yaml = "USUBJID: {pattern: \"^A\"}",
      expected = list(USUBJID = list(pattern = "^A"))
    ),
    # min and max, alone and together
    list(
      yaml = "AGE: {min: 18}",
      expected = list(AGE = list(min = 18L))
    ),
    list(
      yaml = "AGE: {max: 65}",
      expected = list(AGE = list(max = 65L))
    ),
    list(
      yaml = "AGE: {min: 18, max: 65}",
      expected = list(AGE = list(min = 18L, max = 65L))
    ),
    # a number stays a number, however it prints
    list(
      yaml = "AGE: {equals: 18}",
      expected = list(AGE = list(equals = 18L))
    ),
    list(
      yaml = "LBORRES: {greater: 0.30000000000000004}",
      expected = list(LBORRES = list(greater = 0.30000000000000004))
    ),
    # a missing member of a list (`~`) is kept
    list(
      yaml = "VISIT: {in: [V03, ~]}",
      expected = list(VISIT = list(`in` = list("V03", NULL)))
    ),
    # several columns, in the order written
    list(
      yaml = "{VISIT: {equals: V03}, AGE: {greater: 18}, LBORRES: {empty: false}}",
      expected = list(
        VISIT = list(equals = "V03"), AGE = list(greater = 18L),
        LBORRES = list(empty = FALSE)
      )
    )
  )

  for (case in corpus) {
    expect_identical(reopen(yclause(case$yaml)), case$expected, info = case$yaml)
  }
})

test_that("a clause opened and saved without a change selects the same rows", {
  # Secondary check, independent of the literal expectations above: the engine
  # gives the same mask for the clause before and after. (`min`/`max` are only
  # evaluated by evaluate_conditions(), not by the per-operator function.)
  evaluate <- utils::getFromNamespace("evaluate_conditions", "DTAtools")
  df <- data.frame(
    LBORRES = c(0.4, 0.5, 20, 40, 41, NA, 3),
    VISIT = c("SCREENING", "V03", "V03", "EOT", "SCREENING", "V03", NA),
    AGE = c(17, 18, 64, 65, 80, NA, 30),
    SITEID = c("001", "002", "007", "1", "2", NA, "003"),
    RES = c("1.50", "1.5", "2.0", "1.50", "3", NA, "1.50"),
    LBNAM = c("Smith, John", "Doe", "Smith", "John", "Doe", NA, "Doe"),
    USUBJID = c("A1", "B2", "AB3", "C", "A", NA, "ZA"),
    stringsAsFactors = FALSE
  )
  clauses <- c(
    "LBORRES: {range: [0.5, 40]}", "VISIT: {not_equal: SCREENING}",
    "VISIT: {equal: V03}", "AGE: {greater_equal: 18, less: 65}",
    "SITEID: {in: [\"001\", \"002\"]}", "SITEID: {equals: \"007\"}",
    "RES: {equals: \"1.50\"}", "LBNAM: {in: [\"Smith, John\", \"Doe\"]}",
    "LBNAM: {not_in: [\"Smith, John\", \"Doe\"]}", "LBORRES: {empty: false}",
    "USUBJID: {pattern: \"^A\"}", "AGE: {min: 18}", "AGE: {max: 65}",
    "AGE: {min: 18, max: 65}", "VISIT: {in: [V03, ~]}",
    "{VISIT: {equals: V03}, AGE: {greater: 18}, LBORRES: {empty: false}}"
  )
  for (text in clauses) {
    before <- yclause(text)
    expect_identical(
      evaluate(reopen(before), df), evaluate(before, df),
      info = text
    )
  }
})

test_that("the clause of the guide's glucose rule opens as a between row and saves as min and max", {
  rows <- app_fn("condition_to_form_rows")(yclause("LBORRES: {range: [0.5, 40]}"))
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$col, "LBORRES")
  expect_identical(rows[[1]]$op, "min_max")
  expect_identical(rows[[1]]$val, "0.5, 40")
})

test_that("several operators on one column open as one row each", {
  rows <- app_fn("condition_to_form_rows")(yclause("AGE: {greater_equal: 18, less: 65}"))
  expect_length(rows, 2L)
  expect_identical(vapply(rows, function(r) r$col, ""), c("AGE", "AGE"))
  expect_identical(vapply(rows, function(r) r$op, ""), c("greater_equal", "less"))
  expect_identical(vapply(rows, function(r) r$val, ""), c("18", "65"))
})

# ---- the editor can show every operator the engine has ---------------------------

test_that("every operator the engine accepts can be shown by the editor", {
  engine <- utils::getFromNamespace("dta_condition_operators", "DTAtools")()
  offered <- unname(app_fn("dta_condition_operators")())

  # One legal clause for each engine operator, written by hand. A new engine
  # operator makes this list incomplete, which fails the first expectation.
  clauses <- list(
    equals = "A: {equals: x}", equal = "A: {equal: x}",
    not_equals = "A: {not_equals: x}", not_equal = "A: {not_equal: x}",
    `in` = "A: {in: [x, y]}", not_in = "A: {not_in: [x, y]}",
    greater = "A: {greater: 1}", less = "A: {less: 1}",
    greater_equal = "A: {greater_equal: 1}", less_equal = "A: {less_equal: 1}",
    min = "A: {min: 1}", max = "A: {max: 1}", range = "A: {range: [1, 2]}",
    pattern = "A: {pattern: x}", empty = "A: {empty: true}"
  )
  expect_setequal(names(clauses), engine)
  expect_setequal(names(app_fn("form_operator_aliases")()), engine)
  expect_true(all(app_fn("form_operator_aliases")() %in% offered))

  for (op in names(clauses)) {
    rows <- app_fn("condition_to_form_rows")(yclause(clauses[[op]]))
    expect_length(rows, 1L)
    # A <select> that has no option for its `selected` value reports its first
    # option, so a row whose operator is not offered silently becomes `equals`.
    expect_true(rows[[1]]$op %in% offered, info = op)
  }
})

test_that("a clause the editor cannot show is refused instead of being rewritten", {
  expect_error(
    app_fn("condition_to_form_rows")(yclause("AGE: {between: [1, 2]}")),
    class = "dta_form_unsupported", regexp = "between"
  )
  expect_error(
    app_fn("condition_to_form_rows")(yclause("AGE: 18")),
    class = "dta_form_unsupported", regexp = "AGE"
  )
})

# ---- saving rows of one column -----------------------------------------------------

test_that("rows of one column are merged, in the order of the rows", {
  out <- app_fn("form_rows_to_condition")(
    list(
      typed("AGE", "greater_equal", "18"), typed("VISIT", "equals", "V03"),
      typed("AGE", "less", "65")
    ),
    c(AGE = "number", VISIT = "text")
  )
  expect_identical(
    out,
    list(AGE = list(greater_equal = 18, less = 65), VISIT = list(equals = "V03"))
  )
})

test_that("the same operator twice on one column is an error, never an overwrite", {
  expect_error(
    app_fn("form_rows_to_condition")(
      list(typed("AGE", "greater", "18"), typed("AGE", "greater", "21")),
      c(AGE = "number")
    ),
    class = "dta_form_operator_twice", regexp = "AGE"
  )
  # `between` is one operator, whatever its bounds
  expect_error(
    app_fn("form_rows_to_condition")(
      list(typed("AGE", "min_max", "18, 30"), typed("AGE", "min_max", ", 65")),
      c(AGE = "number")
    ),
    class = "dta_form_operator_twice", regexp = "AGE"
  )
})

test_that("a clause with two spellings of one operator is not opened, so it never fails on an untouched Save", {
  # `range` and `min`, or `equals` and `equal`, are two keys of one form row;
  # merging them would change what the rule says.
  for (text in c("AGE: {range: [0, 100], min: 18}", "VISIT: {equals: A, equal: B}")) {
    expect_error(
      app_fn("condition_to_form_rows")(yclause(text)),
      class = "dta_form_unsupported", regexp = "one operator", info = text
    )
  }
  # `min` with `max` is one row by design and opens
  expect_length(app_fn("condition_to_form_rows")(yclause("AGE: {min: 18, max: 65}")), 1L)
})

test_that("a row with a value and no column is an error, an empty row is not", {
  expect_error(
    app_fn("form_rows_to_condition")(list(typed("", "equals", "V03")), clause = "IF"),
    class = "dta_form_value_error", regexp = "V03"
  )
  expect_identical(app_fn("form_rows_to_condition")(list(typed("", "equals", "  ")), clause = "IF"), list())
  expect_identical(app_fn("form_rows_to_condition")(list(), clause = "IF"), list())
})

test_that("an untouched row keeps its value while a row added beside it is read by the column's type", {
  rows <- app_fn("condition_to_form_rows")(yclause("AGE: {greater_equal: 18}"))
  submitted <- c(untouched(rows), list(typed("AGE", "less", "70")))
  expect_identical(
    app_fn("form_rows_to_condition")(submitted, c(AGE = "number")),
    list(AGE = list(greater_equal = 18L, less = 70))
  )
})

test_that("a row whose column, operator or text changed is read again, not kept", {
  rows <- app_fn("condition_to_form_rows")(yclause("SITEID: {equals: \"007\"}"))
  r <- rows[[1]]
  save <- function(...) {
    app_fn("form_rows_to_condition")(
      list(utils::modifyList(list(col = r$col, op = r$op, val = r$val, orig = r), list(...))),
      c(SITEID = "text", AGE = "number")
    )
  }
  expect_identical(save(), list(SITEID = list(equals = "007")))
  expect_identical(save(val = "008"), list(SITEID = list(equals = "008")))
  expect_identical(save(op = "not_equals"), list(SITEID = list(not_equals = "007")))
  # moved to a numeric column, the same text is a number
  expect_identical(save(col = "AGE"), list(AGE = list(equals = 7)))
})

test_that("a row without a column is left out", {
  expect_identical(
    app_fn("form_rows_to_condition")(
      list(typed("", "equals", ""), typed("VISIT", "equals", "V03")),
      c(VISIT = "text")
    ),
    list(VISIT = list(equals = "V03"))
  )
})

# ---- values typed by the user are read by the column's declared type -----------------

test_that("equality and list values follow the declared type of the column", {
  parse <- app_fn("parse_form_value")

  # text: nothing is turned into a number
  expect_identical(parse("equals", "007", "text"), "007")
  expect_identical(parse("equals", "1.50", "text"), "1.50")
  expect_identical(parse("not_equals", "1e5", "text"), "1e5")
  expect_identical(parse("in", "001, 002", "text"), c("001", "002"))
  # a column that is not declared (no family) is text
  expect_identical(parse("equals", "007", NA_character_), "007")
  expect_identical(parse("in", "001, 002", NA_character_), c("001", "002"))
  # logical and temporal columns: the text as typed
  expect_identical(parse("equals", "true", "logical"), "true")
  expect_identical(parse("equals", "2024-01-31", "temporal"), "2024-01-31")

  # numbers
  expect_identical(parse("equals", "007", "number"), 7)
  expect_identical(parse("equals", "1.50", "integer"), 1.5)
  expect_identical(parse("not_equals", " 18 ", "number"), 18)
  expect_identical(parse("in", "1, 2.5, -3e2", "number"), c(1, 2.5, -300))
})

test_that("comparison bounds are numbers, whatever the column, except on dates", {
  parse <- app_fn("parse_form_value")
  for (op in c("greater", "less", "greater_equal", "less_equal")) {
    expect_identical(parse(op, "18", "number"), 18, info = op)
    expect_identical(parse(op, "0.5", NA_character_), 0.5, info = op)
    expect_identical(parse(op, "2024-01-31", "temporal"), "2024-01-31", info = op)
  }
})

test_that("a list is read with a quoting rule, not split at every comma", {
  parse <- app_fn("parse_form_value")
  expect_identical(parse("in", "\"Smith, John\", Doe", "text"), c("Smith, John", "Doe"))
  expect_identical(parse("in", "Doe,\"Smith, John\"", "text"), c("Doe", "Smith, John"))
  expect_identical(parse("not_in", "  a ,b  ,  c", "text"), c("a", "b", "c"))
  expect_identical(parse("in", "\"say \"\"hi\"\"\", x", "text"), c("say \"hi\"", "x"))
  # a trailing or doubled comma holds no member
  expect_identical(parse("in", "a,,b,", "text"), c("a", "b"))
  # a quoted member stays text on a text column, a number on a numeric one
  expect_identical(parse("in", "\"5\", 6", "number"), c(5, 6))
  # `~` is a missing member, as in YAML; quoted it is the text
  expect_identical(parse("in", "A, ~", "text"), c("A", NA_character_))
  expect_identical(parse("in", "A, \"~\"", "text"), c("A", "~"))
  expect_identical(parse("in", "1, ~", "number"), c(1, NA_real_))
})

test_that("a list that cannot be read is an error, not a guess", {
  parse <- app_fn("parse_form_value")
  expect_error(parse("in", "\"unclosed, b", "text"), class = "dta_form_value_error")
  expect_error(parse("in", "\"a\" \"b\"", "text"), class = "dta_form_value_error")
  expect_error(parse("in", "\"a\"x, b", "text"), class = "dta_form_value_error")
  expect_error(parse("in", "", "text"), class = "dta_form_value_error")
  expect_error(parse("in", " , ", "text"), class = "dta_form_value_error")
  expect_error(parse("in", "1, two", "number"), class = "dta_form_value_error", regexp = "two")
})

test_that("a value that is not a number is an error where a number is needed", {
  parse <- app_fn("parse_form_value")
  for (txt in c("1,5", "abc", "", "  ", "0x10", "Inf", "NaN", "NA", "1 2", "5%", "1e", "--1")) {
    expect_error(
      parse("equals", txt, "number"),
      class = "dta_form_value_error", info = paste("equals", txt)
    )
    expect_error(
      parse("greater", txt, "number"),
      class = "dta_form_value_error", info = paste("greater", txt)
    )
  }
  # the text of the error names what was typed
  expect_error(parse("less", "1,5", "number"), regexp = "1,5")
})

test_that("a blank value is an error for every operator that needs one", {
  parse <- app_fn("parse_form_value")
  for (op in c("equals", "not_equals", "in", "not_in", "pattern", "greater", "less_equal")) {
    expect_error(parse(op, "", "text"), class = "dta_form_value_error", info = op)
  }
})

test_that("empty reads true and false and refuses anything else", {
  parse <- app_fn("parse_form_value")
  expect_identical(parse("empty", "true"), TRUE)
  expect_identical(parse("empty", "FALSE"), FALSE)
  expect_identical(parse("empty", " yes "), TRUE)
  expect_identical(parse("empty", "no"), FALSE)
  expect_identical(parse("empty", ""), TRUE) # the box shows the default, true
  expect_error(parse("empty", "maybe"), class = "dta_form_value_error", regexp = "maybe")
})

test_that("a pattern is kept exactly as typed", {
  expect_identical(app_fn("parse_form_value")("pattern", "^A[0-9]+, x$", "text"), "^A[0-9]+, x$")
})

test_that("between reads two positional bounds and refuses what it cannot read", {
  parse <- app_fn("parse_form_value")
  between <- app_fn("parse_form_between")
  expect_identical(between("0.5, 40", "number"), list(min = 0.5, max = 40))
  expect_identical(between("18", "number"), list(min = 18))
  expect_identical(between(", 65", "number"), list(max = 65))
  expect_identical(between("2024-01-01, 2024-12-31", "temporal"), list(min = "2024-01-01", max = "2024-12-31"))
  for (txt in c("", " , ", "18 - 65", "0,5, 10", "1, 2, 3", "a, 5")) {
    expect_error(between(txt, "number"), class = "dta_form_value_error", info = txt)
  }
  # `between` is not a single value
  expect_error(parse("min_max", "1, 2", "number"), class = "dta_form_value_error")
})

test_that("the row's column and operator are named in the error", {
  expect_error(
    app_fn("form_rows_to_condition")(list(typed("AGE", "greater", "1,5")), c(AGE = "number")),
    class = "dta_form_value_error", regexp = "AGE"
  )
  expect_error(
    app_fn("form_rows_to_condition")(
      list(typed("AGE", "greater", "1,5")), c(AGE = "number"),
      clause = "THEN"
    ),
    class = "dta_form_value_error", regexp = "THEN"
  )
})

# ---- the number a row shows is the number it was opened from --------------------------

test_that("numbers are shown so that reading them back gives the same number", {
  show <- function(x) {
    app_fn("condition_to_form_rows")(list(A = list(equals = x)))[[1]]$val
  }
  expect_identical(show(100000), "100000")
  expect_identical(show(1e-5), "1e-05")
  expect_identical(show(0.30000000000000004), "0.30000000000000004")
  expect_identical(show(0.5), "0.5")
  expect_identical(show(40L), "40")
  expect_identical(show(1e15), "1e+15")
  expect_identical(show(-2.5), "-2.5")

  # and an operator changed with the text left alone reads the shown text back
  for (x in c(100000, 1e-5, 0.30000000000000004, 0.5, 1e15, -2.5, 123456789 + 8285045 / 2^26)) {
    row <- app_fn("condition_to_form_rows")(list(A = list(equals = x)))[[1]]
    out <- app_fn("form_rows_to_condition")(
      list(list(col = "A", op = "greater", val = row$val, orig = row)),
      c(A = "number")
    )
    expect_identical(out, list(A = list(greater = x)), info = format(x, digits = 17))
  }
})

test_that("every spelling of a number the editor accepts reads as the number it spells", {
  # Expected values are written out by hand; none comes from as.numeric(), whose
  # rounding differs on macOS arm64 for long decimals (the reader is jsonlite's).
  read <- app_fn("form_read_number")
  spelled <- list(
    "5" = 5, "+5" = 5, "-5" = -5, "5." = 5, ".5" = 0.5, "-.5" = -0.5,
    "007" = 7, "000" = 0, "0.50" = 0.5, "1e5" = 1e5, "1E5" = 1e5,
    "1e+05" = 1e5, "1e-05" = 1e-5, "2.5e3" = 2500, "-0" = 0,
    " 12 " = 12, "0.1" = 0.1, "100000" = 1e5, "1e22" = 1e22
  )
  # 1e22 is the largest power of ten a double holds exactly, so even R's own
  # literal is right on every platform; 1e308 is not (arm64 reads it a ulp off)
  for (txt in names(spelled)) {
    expect_identical(read(txt, "A"), spelled[[txt]], info = txt)
  }
  # a text longer than 15 digits is read to the nearest double, whatever the
  # platform: 0.12345679 * 2^26 = 8285044.93, so the nearest double is built from
  # integers, two exact operations that involve no text parser
  expect_identical(read("123456789.12345679", "A"), 123456789 + 8285045 / 2^26)
  expect_error(read("1e400", "A"), class = "dta_form_value_error")
  expect_error(read("1,5", "A"), class = "dta_form_value_error")
})

test_that("a list is shown so that a value with a comma or a quote reads back as itself", {
  members <- c("Smith, John", "Doe", "say \"hi\"", " padded ", "~", "")
  row <- app_fn("condition_to_form_rows")(list(A = list(`in` = members)))[[1]]
  expect_identical(
    app_fn("parse_form_value")("in", row$val, "text"),
    members
  )
})

# ---- the range rule's two boxes -----------------------------------------------------------

test_that("the range rule reads its two boxes strictly", {
  bounds <- app_fn("parse_form_bounds")
  expect_identical(bounds("0.5", "40"), list(min = 0.5, max = 40))
  # a blank box is an open side
  expect_identical(bounds("", "10"), list(min = -Inf, max = 10))
  expect_identical(bounds("-3", " "), list(min = -3, max = Inf))
  expect_identical(bounds("2", "", orig = list(min = -Inf, max = Inf)), list(min = 2, max = Inf))
  # a box still showing the bound the rule was opened with keeps that bound
  # as it was (an integer stays an integer); an edited box is read again
  expect_identical(bounds("100000", "7", orig = list(min = 100000L, max = 7)), list(min = 100000L, max = 7))
  expect_identical(
    bounds("0.30000000000000004", "8", orig = list(min = 0.30000000000000004)),
    list(min = 0.30000000000000004, max = 8)
  )
  expect_identical(bounds("101", "7", orig = list(min = 100L)), list(min = 101, max = 7))
  expect_error(bounds("0,5", "10"), class = "dta_form_value_error", regexp = "0,5")
  expect_error(bounds("1", "ten"), class = "dta_form_value_error", regexp = "ten")
  expect_error(bounds("", ""), class = "dta_form_value_error")
})

test_that("the range rule's boxes show the bound the rule holds", {
  expect_identical(app_fn("form_number_text")(100000), "100000")
  expect_identical(app_fn("form_number_text")(0.30000000000000004), "0.30000000000000004")
  expect_identical(app_fn("form_number_text")(NULL), "")
})

test_that("an open range side shows as an empty box", {
  text <- app_fn("form_bound_text")
  for (open in list(-Inf, Inf, NA, NA_real_, NULL, numeric(0))) {
    expect_identical(text(open), "")
  }
  expect_identical(text(5L), "5")
  expect_identical(text(0.5), "0.5")
})
