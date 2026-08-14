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

  expect_equal(.kv_bullets_md(list(x = NA_character_)), "- **x:** (not specified)")
  expect_equal(.kv_bullets_md(list(x = NA)), "- **x:** (not specified)")

  # The bare text "NA" must never reach a rendered document.
  expect_false(grepl("NA", .kv_bullets_md(list(x = NA)), fixed = TRUE))
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
  expect_true(any(grepl("- **type:** (not specified)", md_lines, fixed = TRUE)))

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  expect_no_error(
    write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  )
  expect_true(grepl("(not specified)", .docx_text(out_docx), fixed = TRUE))
})

test_that(".format_value_list drops values beyond max_items", {
  expect_equal(.format_value_list(NULL), "(not specified)")
  expect_equal(.format_value_list(c("a", "b")), "a, b")

  # Only a count survives; values 11..25 are gone from the rendered spec, so a
  # column's allowed-value list is not fully documented by the export.
  out <- .format_value_list(1:25, max_items = 10)
  expect_equal(out, "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ... and 15 more")
  expect_false(grepl("11", out, fixed = TRUE))
  expect_true(grepl("... and 15 more", out, fixed = TRUE))

  # The default cut-off is 5.
  expect_equal(.format_value_list(1:25), "1, 2, 3, 4, 5, ... and 20 more")

  # Lists are unlisted before truncation.
  expect_equal(.format_value_list(list("a", "b", "c"), max_items = 2), "a, b, ... and 1 more")
})

test_that(".format_value_list switches to line breaks past max_width", {
  wide <- c(strrep("x", 30), strrep("y", 40))
  expect_equal(.format_value_list(wide), paste(wide, collapse = "\n"))

  narrow <- c("x", "y")
  expect_equal(.format_value_list(narrow), "x, y")
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

# ---- group_condition rule descriptions --------------------------------------
# A group_condition rule used to fall through to the default formatter, which
# printed either the bare author description or "no description available" --
# none of the grouping columns, named conditions or constraints reached the
# exported document.

.gc_rule <- function(...) {
  args <- list(...)
  do.call(DTAtools::DTARuleGroupCondition, args)
}

test_that("translate_rule_to_human expands a group_condition into its parts", {
  rule <- .gc_rule(
    id = "visit_logic",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(
      c_failed = list(REASND = list(empty = FALSE)),
      c_reported = list(REASND = list(empty = TRUE), ORRES = list(empty = FALSE))
    ),
    constraints = list(
      list(type = "requires", `if` = "c_failed", `then` = "c_reported")
    )
  )

  txt <- translate_rule_to_human(rule)

  # Never the default fallback.
  expect_false(grepl("no description available", txt, fixed = TRUE))
  expect_false(grepl("Rule type", txt, fixed = TRUE))

  # Grouping columns, named as such.
  expect_match(txt, "grouped by 'SUBJECT_ID' and 'VISIT'", fixed = TRUE)

  # Each named condition is spelled out in terms of its columns, not just named.
  expect_match(txt, "\"c_failed\": a row where 'REASND' is present", fixed = TRUE)
  expect_match(txt, "\"c_reported\": a row where 'REASND' is empty/absent AND 'ORRES' is present", fixed = TRUE)

  # The constraint is stated as a requirement.
  expect_match(txt, "If \"c_failed\" holds", fixed = TRUE)
  expect_match(txt, "then \"c_reported\" must hold", fixed = TRUE)
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
        type = "requires", `if` = "a", `then` = "b",
        if_scope = if_scope, then_scope = then_scope
      ))
    ))
  }

  any_any <- mk("any", "any")
  expect_match(any_any, "If \"a\" holds for at least one row in the group", fixed = TRUE)
  expect_match(any_any, "then \"b\" must hold for at least one row in the group", fixed = TRUE)

  any_all <- mk("any", "all")
  expect_match(any_all, "then \"b\" must hold for every row in the group", fixed = TRUE)
  expect_false(grepl("then \"b\" must hold for at least one row", any_all, fixed = TRUE))

  all_any <- mk("all", "any")
  expect_match(all_any, "If \"a\" holds for every row in the group", fixed = TRUE)
})

test_that("group_condition descriptions state mutual exclusivity", {
  txt <- translate_rule_to_human(.gc_rule(
    id = "excl",
    group_by = "G",
    conditions = list(a = list(A = list(equals = "X")), b = list(B = list(equals = "Y"))),
    constraints = list(list(type = "mutually_exclusive", left = "a", right = "b"))
  ))

  expect_match(txt, "must not both occur in the same group", fixed = TRUE)
  expect_match(txt, "\"a\" (at least one row) and \"b\" (at least one row)", fixed = TRUE)
  # The alias not_both normalises to the same wording.
  txt_alias <- translate_rule_to_human(.gc_rule(
    id = "excl2",
    group_by = "G",
    conditions = list(a = list(A = list(equals = "X")), b = list(B = list(equals = "Y"))),
    constraints = list(list(type = "not_both", left = "a", right = "b"))
  ))
  expect_match(txt_alias, "must not both occur in the same group", fixed = TRUE)
})

test_that("group_condition descriptions keep the author summary and add the detail", {
  rule <- .gc_rule(
    id = "described",
    description = "Consented records need a consent date",
    group_by = "SUBJECT_ID",
    conditions = list(
      yes = list(CONSENT = list(equals = "YES")),
      dated = list(CONSENT_DATE = list(empty = FALSE))
    ),
    constraints = list(list(type = "requires", `if` = "yes", `then` = "dated"))
  )

  txt <- translate_rule_to_human(rule)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

  # The author's own wording leads; the expansion follows it.
  expect_identical(lines[[1]], "Consented records need a consent date")
  expect_gt(length(lines), 1)
  expect_match(txt, "grouped by 'SUBJECT_ID'", fixed = TRUE)
  expect_match(txt, "'CONSENT' = 'YES'", fixed = TRUE)
})

test_that("all constraints of a multi-constraint group rule are described", {
  txt <- translate_rule_to_human(.gc_rule(
    id = "multi",
    group_by = "G",
    conditions = list(
      a = list(A = list(empty = FALSE)),
      b = list(B = list(empty = FALSE)),
      c = list(C = list(equals = "NOT DONE"))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "a", right = "b"),
      list(type = "requires", `if` = "a", `then` = "c")
    )
  ))

  expect_match(txt, "Requirements:", fixed = TRUE)
  expect_match(txt, "must not both occur", fixed = TRUE)
  expect_match(txt, "If \"a\" holds", fixed = TRUE)
  # Every declared condition is defined, including the one only the second
  # constraint refers to.
  for (nm in c("a", "b", "c")) {
    expect_match(txt, paste0("\"", nm, "\": a row where"), fixed = TRUE)
  }
})
