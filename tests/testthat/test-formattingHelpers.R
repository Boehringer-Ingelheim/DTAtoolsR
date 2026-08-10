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

test_that(".format_scalar_value aborts on NA_character_ (DEFERRED defect)", {
  # KNOWN DEFECT, pinned rather than endorsed: nchar(NA_character_) is
  # NA_integer_, so the `if (nchar(val) > 80)` guard receives NA and R aborts
  # with "missing value where TRUE/FALSE needed". Every export format crashes
  # when any metadata field holds NA. When R/formattingHelpers.R:266 is fixed
  # this SHOULD fail -- change it to
  # expect_equal(.format_scalar_value(NA_character_), "(not specified)").
  #
  # Matched by condition class, not message: base R messages are translated on
  # non-English machines.
  expect_error(.format_scalar_value(NA_character_), class = "simpleError")
  expect_error(.format_metadata_pairs(list(x = NA_character_)), class = "simpleError")
  expect_error(.kv_bullets_md(list(x = NA_character_)), class = "simpleError")

  # KNOWN DEFECT, pinned rather than endorsed: a logical NA takes the
  # is.logical() branch first and escapes the crash, but ifelse(NA, "Yes", "No")
  # returns logical NA -- a formatter documented to return a display string
  # hands back a non-character NA, which then reaches the rendered document as
  # the bare text "NA". When R/formattingHelpers.R:256-258 is fixed this SHOULD
  # fail -- change it to
  # expect_equal(.format_scalar_value(NA), "(not specified)").
  expect_true(is.na(.format_scalar_value(NA)))
  expect_type(.format_scalar_value(NA), "logical")
  expect_equal(.kv_bullets_md(list(x = NA)), "- **x:** NA")
})

test_that("an NA metadata field crashes the whole export (DEFERRED defect)", {
  # KNOWN DEFECT, pinned rather than endorsed: the .format_scalar_value(NA)
  # abort above is reachable end to end -- a transmission field left as NA (a
  # state the Shiny editor and YAML round-trips can produce) takes down both the
  # Markdown and the DOCX export. When R/formattingHelpers.R:266 is fixed this
  # SHOULD fail -- change it to expect_no_error() for both formats.
  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2)),
    metadata = DTAMetaData(transmission = list(type = NA_character_), version = "1.0")
  )

  out_md <- tempfile(fileext = ".md")
  on.exit(unlink(out_md, force = TRUE), add = TRUE)
  expect_error(
    write_dta(dta, file = out_md, format = "md", overwrite = TRUE, quiet = TRUE),
    class = "simpleError"
  )

  out_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(out_docx, force = TRUE), add = TRUE)
  expect_error(
    write_dta(dta, file = out_docx, format = "docx", overwrite = TRUE, quiet = TRUE),
    class = "simpleError"
  )
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

test_that(".title_case_field normalizes snake and dotted field names", {
  expect_equal(.title_case_field("date_first_transfer"), "Date First Transfer")
  expect_equal(.title_case_field("name"), "Name")
})
