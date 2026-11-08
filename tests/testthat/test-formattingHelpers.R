# R/formattingHelpers.R had no test file. These tests pin the display-formatting
# contract that every export format (DOCX tables, Markdown bullets) depends on.
#
# Note on locale: this suite runs on machines with non-English locales, so base
# R condition messages are translated. Assertions here match condition classes
# or package-authored text only, never translated prose.

test_that(".format_scalar_value renders the empty and typed cases", {
  expect_equal(.format_scalar_value(NULL), "(not specified)")
  expect_equal(.format_scalar_value(""), "(not specified)")
  expect_equal(.format_scalar_value(list()), "(not specified)")

  expect_equal(.format_scalar_value(TRUE), "Yes")
  expect_equal(.format_scalar_value(FALSE), "No")

  expect_equal(.format_scalar_value(as.Date("2026-01-15")), "2026-01-15")

  # A vector collapses to a comma-separated line ...
  expect_equal(.format_scalar_value(c(1, 2, 3)), "1, 2, 3")
  expect_equal(.format_scalar_value(c("a", "b")), "a, b")
  # ... while a named list degrades to its NAMES, discarding the values. This is
  # documented as a should-not-happen path (callers are expected to flatten
  # first, R/formattingHelpers.R:251-255).
  expect_equal(.format_scalar_value(list(a = 1, b = 2)), "a, b")
})

test_that(".format_scalar_value renders large numerics in scientific notation", {
  # as.character() decides the numeric rendering, so a value that a DTA author
  # typed as 1000000000000000 is shown as "1e+15" in the exported agreement.
  expect_equal(.format_scalar_value(1e15), "1e+15")
  expect_equal(.format_scalar_value(1e14), "1e+14")
  expect_equal(.format_scalar_value(1000), "1000")
})

test_that(".format_scalar_value truncates strings longer than 80 characters", {
  # R/formattingHelpers.R:266-268: >80 chars becomes the first 77 plus "...",
  # i.e. exactly 80 characters wide.
  short <- strrep("B", 80)
  expect_identical(.format_scalar_value(short), short)

  long <- strrep("A", 100)
  truncated <- .format_scalar_value(long)
  expect_equal(nchar(truncated), 80)
  expect_equal(truncated, paste0(strrep("A", 77), "..."))

  # The boundary: 81 characters is already truncated.
  expect_equal(nchar(.format_scalar_value(strrep("B", 81))), 80)
})

test_that(".format_scalar_value renders every flavour of NA as one display string", {
  # A missing value has exactly one representation, MISSING_VALUE_DISPLAY, and
  # the formatter always hands back a character scalar -- previously
  # nchar(NA_character_) fed NA to `if (nchar(val) > 80)` and aborted, while a
  # logical NA escaped through ifelse(NA, "Yes", "No") as a non-character NA.
  expect_identical(MISSING_VALUE_DISPLAY, "(not specified)")

  for (missing in list(NA_character_, NA, NA_integer_, NA_real_, NULL, "")) {
    out <- .format_scalar_value(missing)
    expect_type(out, "character")
    expect_length(out, 1L)
    expect_equal(out, MISSING_VALUE_DISPLAY)
  }

  pairs <- .format_metadata_pairs(list(x = NA_character_))
  expect_equal(pairs$key, "x")
  expect_equal(pairs$value, MISSING_VALUE_DISPLAY)

  # The bare text "NA" must never reach a rendered document.
  expect_false(grepl("NA", .format_scalar_value(NA), fixed = TRUE))
})

test_that("an NA metadata field no longer breaks any export format", {
  # A transmission field left as NA -- a state the Shiny editor and YAML
  # round-trips can produce -- used to take down both the Markdown and the DOCX
  # export via .format_scalar_value().
  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(transmission = list(type = NA_character_), version = "1.0")
  )

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  expect_no_error(
    write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE)
  )
  # Not merely "no error": the NA field must be rendered as the missing-value
  # placeholder, not as the literal "NA".
  md_lines <- readLines(out_md, warn = FALSE)
  expect_true("| Transfer method | (not specified) |" %in% md_lines)
  expect_false("| Transfer method | NA |" %in% md_lines)

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  expect_no_error(
    write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  )
  expect_true(grepl("(not specified)", .docx_text(out_docx), fixed = TRUE))
})

test_that(".format_document_date renders ISO 8601 regardless of LC_TIME", {
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)

  d <- as.Date("2026-01-15")
  expect_equal(.format_document_date(d), "2026-01-15")

  # Whichever non-English time locale this machine offers; the assertion is
  # locale-independent either way, so nothing is skipped.
  for (loc in c("de_DE.UTF-8", "German_Germany.1252", "fr_FR.UTF-8", "French_France.1252")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", loc)))) break
  }
  expect_equal(.format_document_date(d), "2026-01-15")
  expect_false(grepl("[A-Za-z]", .format_document_date(d)))

  Sys.setlocale("LC_TIME", "C")
  expect_equal(.format_document_date(d), "2026-01-15")

  # POSIXct is narrowed to the date, and non-date input passes through.
  expect_equal(.format_document_date(as.POSIXct("2026-01-15 13:45:00", tz = "UTC")), "2026-01-15")
  expect_equal(.format_document_date("2 weeks after approval"), "2 weeks after approval")
  expect_equal(.format_document_date(NULL), "")
  expect_equal(.format_document_date(NA), "")
})

test_that(".title_case_field normalizes snake and dotted field names", {
  expect_equal(.title_case_field("date_first_transfer"), "Date First Transfer")
  expect_equal(.title_case_field("name"), "Name")
})

test_that("a markdown table cell survives an embedded newline as one row", {
  # A GFM pipe table is line-based. A description authored as a YAML block
  # scalar carries real newlines, and each one used to split the row in two --
  # the renderer then read the tail as a fresh table row, misattributing its
  # text to the first column and dropping everything after the last pipe.
  df <- data.frame(
    id = "age_range",
    description = "Ages over 100 need:\nsupervisor sign-off | extra consent",
    stringsAsFactors = FALSE
  )

  lines <- .df_to_md_table(df)

  # Header, separator, and exactly one body line.
  expect_length(lines, 3)
  expect_false(any(grepl("\n", lines, fixed = TRUE)))

  body <- lines[3]
  expect_match(body, "supervisor sign-off", fixed = TRUE)
  expect_match(body, "extra consent", fixed = TRUE)
  # The pipe inside the text stays escaped, so it cannot open a new cell.
  expect_match(body, "\\|", fixed = TRUE)
  expect_match(body, "<br>", fixed = TRUE)
})

test_that("carriage returns are folded the same way as newlines", {
  df <- data.frame(x = "a\r\nb", stringsAsFactors = FALSE)
  lines <- .df_to_md_table(df)

  expect_length(lines, 3)
  expect_equal(lines[3], "| a<br>b |")
})

test_that("an empty table renders as nothing at all", {
  expect_equal(.df_to_md_table(NULL), character(0))
  expect_equal(.df_to_md_table(data.frame()), character(0))
})

# ---- translate_rule_to_human() ----------------------------------------------
# A thin wrapper around the shared wording in R/rulePhrasing.R. The expected
# strings below are written out by hand from the wording specification at the
# top of that file (sentence forms, operator table, notes), never copied from
# the function's output. U+201C/U+201D quote text values, U+2022 starts a list
# item.

.gc_rule <- function(...) {
  args <- list(...)
  do.call(DTAtools::DTARuleGroupCondition, args)
}

test_that("translate_rule_to_human is the shared plain-text wording for every rule type", {
  rules <- list(
    DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65),
    DTARuleColUnique(id = "unique_keys", columns = c("A", "B")),
    DTARuleColCondition(
      id = "adult_status",
      condition = list(AGE = list(greater = 17)),
      then = list(STATUS = list(equal = "adult"))
    ),
    .gc_rule(
      id = "g", group_by = "G",
      conditions = list(a = list(A = list(empty = FALSE)), b = list(B = list(empty = FALSE))),
      constraints = list(list(type = "requires", `if` = "a", `then` = "b", id = "a_needs_b"))
    )
  )
  for (rule in rules) {
    expect_identical(translate_rule_to_human(rule), .rule_text(rule), info = rule@type)
  }
})

test_that("a range rule reads as a sentence with its bounds and notes", {
  rule <- DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65)
  expect_identical(
    translate_rule_to_human(rule),
    paste(
      "Every value in column AGE must be a number from 18 to 65, inclusive.",
      "Missing values: Missing values are not checked.",
      "Non-numeric values: A value that is present but is not a number violates the rule.",
      sep = "\n"
    )
  )
})

test_that("a unique rule over two columns reads as a combination", {
  rule <- DTARuleColUnique(id = "unique_keys", columns = c("A", "B"))
  expect_identical(
    translate_rule_to_human(rule),
    paste(
      "No two rows may have the same combination of values in columns A and B.",
      "Missing values: Two missing values count as the same value.",
      sep = "\n"
    )
  )
})

test_that("a column-condition rule reads its operators, not their key names", {
  rule <- DTARuleColCondition(
    id = "adult_status",
    condition = list(AGE = list(greater = 17)),
    then = list(STATUS = list(equal = "adult"))
  )
  expected <- paste(
    "When AGE is greater than 17, STATUS must be equal to \u201cadult\u201d.",
    "Missing values:",
    "\u2022 If AGE is missing, the condition is not met and the row is not checked.",
    "\u2022 If STATUS is missing in a row that meets the condition, the rule is violated.",
    "Non-numeric values: A value in AGE that is not a number does not exempt the row from the check.",
    sep = "\n"
  )
  expect_identical(translate_rule_to_human(rule), expected)

  # The author's description is its own Purpose line on top; it never
  # replaces the condition. An empty description counts as none.
  described <- DTARuleColCondition(
    id = "adult_status",
    description = "Adults are marked as such",
    condition = list(AGE = list(greater = 17)),
    then = list(STATUS = list(equal = "adult"))
  )
  expect_identical(
    translate_rule_to_human(described),
    paste("Purpose: Adults are marked as such", expected, sep = "\n")
  )
  blank <- DTARuleColCondition(
    id = "adult_status",
    description = "",
    condition = list(AGE = list(greater = 17)),
    then = list(STATUS = list(equal = "adult"))
  )
  expect_identical(translate_rule_to_human(blank), expected)
})

test_that("translate_rule_to_human expands a group_condition into its parts", {
  rule <- .gc_rule(
    id = "visit_logic",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(
      c_failed = list(REASND = list(empty = FALSE)),
      c_reported = list(REASND = list(empty = TRUE), ORRES = list(empty = FALSE))
    ),
    constraints = list(
      list(
        type = "requires", `if` = "c_failed", `then` = "c_reported",
        id = "needs_report", message = "Missing report"
      )
    )
  )

  expect_identical(
    translate_rule_to_human(rule),
    paste(
      paste(
        "Rows are grouped by the combination of values in columns SUBJECT_ID and VISIT;",
        "within each group, the requirements below must hold."
      ),
      "Conditions:",
      "\u2022 c_failed: a row in which REASND is not empty",
      "\u2022 c_reported: a row in which REASND is empty and ORRES is not empty",
      "Requirements:",
      paste(
        "\u2022 needs_report: If at least one row of a group meets condition c_failed,",
        "then at least one row of that group must meet condition c_reported.",
        "If not, the message is \u201cMissing report\u201d."
      ),
      "Missing values:",
      paste(
        "\u2022 A missing value in a grouping column counts as a value of its own:",
        "rows missing it are grouped together."
      ),
      "\u2022 A row in which REASND is missing does not meet condition c_failed.",
      "\u2022 If REASND is missing, this part of condition c_reported counts as met.",
      "\u2022 A row in which ORRES is missing does not meet condition c_reported.",
      sep = "\n"
    )
  )
})

test_that("group_condition descriptions distinguish the any and all scopes", {
  # dta_group_scope_truth() reads "any" as at-least-one-row and "all" as
  # every-row; the prose must not blur the two.
  mk <- function(if_scope, then_scope) {
    translate_rule_to_human(.gc_rule(
      id = "scoped",
      group_by = "G",
      conditions = list(a = list(A = list(empty = FALSE)), b = list(B = list(empty = FALSE))),
      constraints = list(list(
        type = "requires", `if` = "a", `then` = "b", id = "r",
        if_scope = if_scope, then_scope = then_scope
      ))
    ))
  }
  expect_match(
    mk("any", "all"),
    "r: If at least one row of a group meets condition a, then every row of that group must meet condition b.",
    fixed = TRUE
  )
  expect_match(
    mk("all", "any"),
    "r: If every row of a group meets condition a, then at least one row of that group must meet condition b.",
    fixed = TRUE
  )
})

test_that("group_condition descriptions state mutual exclusivity, also under its alias", {
  mk <- function(type) {
    translate_rule_to_human(.gc_rule(
      id = "excl",
      group_by = "G",
      conditions = list(a = list(A = list(equals = "X")), b = list(B = list(equals = "Y"))),
      constraints = list(list(type = type, left = "a", right = "b", id = "x"))
    ))
  }
  expected <- paste(
    "x: Within a group, it must not happen that at least one row meets condition a",
    "and also at least one row meets condition b.",
    "If not, a message naming the group and its rows is reported."
  )
  expect_match(mk("mutually_exclusive"), expected, fixed = TRUE)
  # The alias not_both normalises to the same wording.
  expect_identical(mk("not_both"), mk("mutually_exclusive"))
})

test_that("translate_rule_to_human never renders a rule as code", {
  rules <- list(
    DTARuleColRange(id = "r", columns = "AGE", min = 0.123456789, max = 99999.999),
    DTARuleColCondition(
      id = "c",
      condition = list(CODE = list(pattern = "a```b")),
      then = list(NOTE = list(empty = TRUE))
    ),
    .gc_rule(
      id = "g", group_by = "S",
      conditions = list(a = list(X = list(empty = FALSE)), b = list(X = list(empty = TRUE))),
      constraints = list(list(type = "mutually_exclusive", left = "a", right = "b", id = "cx"))
    )
  )
  for (rule in rules) {
    txt <- translate_rule_to_human(rule)
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    expect_false(any(grepl("^\\s*(type|condition|then|columns|constraints):", lines)), info = rule@id)
    expect_false(grepl("```", gsub("\u201ca```b\u201d", "", txt, fixed = TRUE), fixed = TRUE), info = rule@id)
  }
  # Full precision, plain decimals.
  expect_match(translate_rule_to_human(rules[[1]]), "from 0.123456789 to 99999.999, inclusive.", fixed = TRUE)
})

test_that("translate_rule_to_human rejects something that is not a rule", {
  expect_error(translate_rule_to_human("not a rule"), class = "dta_rule_phrasing_error")
})
