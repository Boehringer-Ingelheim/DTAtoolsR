# R/theme.R lives in inst/shiny/dta_app/R/ and is auto-sourced by Shiny at
# launch, so it is invisible to the package namespace and to the normal test
# suite. These tests reach it through the app_env()/app_fn() harness in
# helper-shinyapp.R.
#
# yaml_highlight_html() in particular renders arbitrary, user-editable YAML
# text straight into the page (the "raw YAML" view / editor), so its escaping
# behaviour is a security property, not just a formatting nicety.

test_that("yaml_highlight_html renders a plain key: value line", {
  f <- app_fn("yaml_highlight_html")
  out <- f("title: Clinical Data Specification")
  expect_match(out, '<span class="yml-key">title</span>', fixed = TRUE)
  expect_match(out, '<span class="yml-punct">:</span>', fixed = TRUE)
  expect_match(out, '<span class="yml-str">Clinical Data Specification</span>', fixed = TRUE)
})

test_that("yaml_highlight_html renders a quoted string value", {
  f <- app_fn("yaml_highlight_html")
  out <- f('version: "0.1"')
  expect_match(out, '<span class="yml-str">"0.1"</span>', fixed = TRUE)
})

test_that("yaml_highlight_html renders a whole-line comment", {
  f <- app_fn("yaml_highlight_html")
  out <- f("# this is a comment")
  expect_equal(out, '<span class="yml-comment"># this is a comment</span>')
})

test_that("yaml_highlight_html renders a list item with a dash marker", {
  f <- app_fn("yaml_highlight_html")
  out <- f("- V01")
  expect_match(out, '<span class="yml-dash">- </span>', fixed = TRUE)
  expect_match(out, '<span class="yml-str">V01</span>', fixed = TRUE)
})

test_that("yaml_highlight_html keeps nested indentation on child lines", {
  f <- app_fn("yaml_highlight_html")
  out <- f("receiver:\n  affiliation:\n    name: Test Company")
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_length(lines, 3)
  expect_match(lines[[2]], "^  <span class=\"yml-key\">affiliation")
  expect_match(lines[[3]], "^    <span class=\"yml-key\">name")
})

test_that("yaml_highlight_html returns an empty string for empty input", {
  f <- app_fn("yaml_highlight_html")
  expect_identical(f(""), "")
})

test_that("yaml_highlight_html HTML-escapes dangerous characters instead of emitting live markup", {
  # The rendered output is injected into the page's raw-YAML view, so any
  # unescaped '<' would let user-supplied YAML text execute as HTML/JS. Read
  # the source (inst/shiny/dta_app/R/theme.R esc()) first: it does escape.
  f <- app_fn("yaml_highlight_html")
  out <- f("desc: <script>alert(1)</script> & co")

  expect_false(grepl("<script>", out, fixed = TRUE))
  expect_match(out, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_match(out, "&amp; co", fixed = TRUE)
})

test_that("status_chip renders the label and CSS class for each known status", {
  f <- app_fn("status_chip")

  expect_match(as.character(f("pass")), 'class="status-chip status-pass"', fixed = TRUE)
  expect_match(as.character(f("pass")), "Passed", fixed = TRUE)

  expect_match(as.character(f("fail")), 'class="status-chip status-fail"', fixed = TRUE)
  expect_match(as.character(f("fail")), "Failed", fixed = TRUE)

  expect_match(as.character(f("pending")), 'class="status-chip status-pending"', fixed = TRUE)
  expect_match(as.character(f("pending")), "Not validated", fixed = TRUE)

  expect_match(as.character(f("nodata")), 'class="status-chip status-nodata"', fixed = TRUE)
  expect_match(as.character(f("nodata")), "No data", fixed = TRUE)
})

test_that("status_chip rejects an unrecognised status instead of silently rendering", {
  # status_chip() validates its input with match.arg() against the four known
  # statuses, so an unknown status is a hard error, not a silently-degraded
  # chip. (The error text itself is match.arg()'s own, base-R, translated on
  # this locale, so only the fact of the error is asserted, not its wording.)
  f <- app_fn("status_chip")
  expect_error(f("bogus"))
})

test_that("slot_state_label renders the icon and class for each known state", {
  f <- app_fn("slot_state_label")

  ok <- as.character(f("ok"))
  expect_match(ok, 'class="slot-ok"', fixed = TRUE)
  expect_match(ok, "✔", fixed = TRUE)

  warn <- as.character(f("warn"))
  expect_match(warn, 'class="slot-warn"', fixed = TRUE)
  expect_match(warn, "⚠", fixed = TRUE)

  empty <- as.character(f("empty"))
  expect_match(empty, 'class="slot-meta"', fixed = TRUE)
  expect_match(empty, "—", fixed = TRUE)
})

test_that("slot_state_label includes detail when supplied and omits it when not", {
  f <- app_fn("slot_state_label")

  with_detail <- as.character(f("warn", "3 rows failed"))
  expect_match(with_detail, "3 rows failed", fixed = TRUE)

  without_detail <- as.character(f("warn"))
  expect_false(grepl("3 rows failed", without_detail, fixed = TRUE))
  # Only the icon (plus a trailing space from paste0(icon, " ", detail %||% ""))
  # remains when no detail is given.
  expect_match(without_detail, "⚠", fixed = TRUE)
})

test_that("slot_state_label does not throw on an unrecognised state", {
  # state's icon/class come from switch() with no default branch, so an
  # unmatched state yields NULL icon/class rather than an error: the <span>
  # is rendered with no class and just the detail text.
  f <- app_fn("slot_state_label")
  out <- as.character(f("unknown_state", "some detail"))
  expect_match(out, "some detail", fixed = TRUE)
  expect_false(grepl('class="', out, fixed = TRUE))
})

test_that("bi_theme returns a Bootstrap 5 bslib theme", {
  skip_if_not_installed("bslib")
  f <- app_fn("bi_theme")
  theme <- f()
  expect_s3_class(theme, "bs_theme")
  expect_equal(as.character(bslib::theme_version(theme)), "5")
})

test_that("bi_css returns html-classed CSS defining the custom properties the markup depends on", {
  skip_if_not_installed("bslib")
  f <- app_fn("bi_css")
  css <- f()
  expect_s3_class(css, "html")

  txt <- as.character(css)
  # Picked because the app's own markup reads these custom properties
  # directly (status chips / status-tinted dataset tiles, theme.R above).
  expect_match(txt, "--bi-pass:", fixed = TRUE)
  expect_match(txt, "--bi-fail:", fixed = TRUE)
  expect_match(txt, "--bi-green-dark:", fixed = TRUE)
})

test_that("bi_css hides an empty edit_gate slot, after the rule that would otherwise lay it out", {
  skip_if_not_installed("bslib")
  txt <- as.character(app_fn("bi_css")())

  base_rule <- ".app-actions > .shiny-html-output { display: flex;"
  empty_rule <- ".app-actions > .shiny-html-output:empty { display: none; }"
  expect_match(txt, base_rule, fixed = TRUE)
  expect_match(txt, empty_rule, fixed = TRUE)

  # Order, not just presence. On the landing page edit_gate renders NULL and
  # Shiny empties the span without removing it, so the base rule above would
  # leave a generated zero-width flex item still taking a share of
  # .app-actions' gap: 8px. :empty outranks it on specificity, but the
  # override is only safe to rely on while it stays later in the sheet.
  expect_lt(
    regexpr(base_rule, txt, fixed = TRUE)[[1]],
    regexpr(empty_rule, txt, fixed = TRUE)[[1]]
  )
})
