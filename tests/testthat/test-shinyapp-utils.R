# Unit tests for the PURE / conversion / serialization helpers of the Shiny
# app's inst/shiny/dta_app/R/utils_dta.R (reached via helper-shinyapp.R, since
# the app's helper files are auto-sourced at launch and are not part of the
# package namespace). DTA-mutation and status-map helpers are covered
# elsewhere; this file sticks to the read-only / conversion / serialization
# surface.

# ---- %||% and dta_try ------------------------------------------------------

test_that("%||% returns the left side unless it is NULL or empty", {
  `%||%` <- app_fn("%||%")

  expect_equal(1 %||% 2, 1)
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal(character(0) %||% "fallback", "fallback")
  expect_equal(FALSE %||% "fallback", FALSE)
})

test_that("dta_try captures a successful expression as ok/value/error", {
  dta_try <- app_fn("dta_try")

  res <- dta_try(1 + 1)

  expect_true(res$ok)
  expect_equal(res$value, 2)
  expect_null(res$error)
})

test_that("dta_try captures an error as ok = FALSE without throwing", {
  dta_try <- app_fn("dta_try")

  expect_no_error(res <- dta_try(stop("boom")))

  expect_false(res$ok)
  expect_null(res$value)
  expect_equal(res$error, "boom")
})

# ---- .dta_compact / .dta_stringify_dates -----------------------------------

test_that(".dta_compact drops NULL and zero-length elements recursively", {
  compact <- app_fn(".dta_compact")

  out <- compact(list(a = 1, b = NULL, c = list(), d = list(x = NULL, y = 2)))

  expect_equal(out, list(a = 1, d = list(y = 2)))
})

test_that(".dta_compact collapses a list of all-empty elements to NULL", {
  compact <- app_fn(".dta_compact")

  expect_null(compact(list(a = NULL, b = character(0))))
})

test_that(".dta_stringify_dates converts Date objects to YYYY-MM-DD strings", {
  stringify <- app_fn(".dta_stringify_dates")

  expect_identical(stringify(as.Date("2024-06-19")), "2024-06-19")
  expect_identical(
    stringify(list(d = as.Date("2024-01-01"), x = "keep")),
    list(d = "2024-01-01", x = "keep")
  )
})

# ---- dta_flag_to_choice / dta_choice_to_flag -------------------------------

test_that("dta_flag_to_choice maps logical, string and unset flags to the tri-state", {
  flag_to_choice <- app_fn("dta_flag_to_choice")

  expect_equal(flag_to_choice(TRUE), "yes")
  expect_equal(flag_to_choice(FALSE), "no")
  expect_equal(flag_to_choice(NULL), "undefined")
  expect_equal(flag_to_choice(NA), "undefined")
  expect_equal(flag_to_choice("yes"), "yes")
  expect_equal(flag_to_choice("no"), "no")
  expect_equal(flag_to_choice("TRUE"), "yes")
  expect_equal(flag_to_choice("0"), "no")
  expect_equal(flag_to_choice("banana"), "undefined")
})

test_that("dta_choice_to_flag inverts the tri-state to TRUE/FALSE/NULL", {
  choice_to_flag <- app_fn("dta_choice_to_flag")

  expect_true(choice_to_flag("yes"))
  expect_false(choice_to_flag("no"))
  expect_null(choice_to_flag("undefined"))
  expect_null(choice_to_flag("anything else"))
})

test_that("dta_flag_to_choice and dta_choice_to_flag round-trip TRUE/FALSE/NULL", {
  flag_to_choice <- app_fn("dta_flag_to_choice")
  choice_to_flag <- app_fn("dta_choice_to_flag")

  expect_identical(choice_to_flag(flag_to_choice(TRUE)), TRUE)
  expect_identical(choice_to_flag(flag_to_choice(FALSE)), FALSE)
  expect_null(choice_to_flag(flag_to_choice(NULL)))
})

# ---- condition operators / rule labels / condition text --------------------

test_that("dta_condition_operators exposes the friendly-label -> engine-key map", {
  ops <- app_fn("dta_condition_operators")()

  expect_equal(unname(ops["equals"]), "equals")
  expect_equal(unname(ops["between (min/max)"]), "min_max")
  expect_equal(unname(ops["matches pattern"]), "pattern")
  expect_true("empty" %in% names(ops))
})

test_that("dta_rule_type_label maps known type tokens and falls back for unknown/blank", {
  label <- app_fn("dta_rule_type_label")

  expect_equal(label("col_condition"), "Conditional (IF/THEN)")
  expect_equal(label("col_range"), "Range")
  expect_equal(label("col_unique"), "Unique")
  expect_equal(label("group_condition"), "Grouped condition")
  expect_equal(label("some_future_type"), "some_future_type")
  expect_equal(label(""), "—")
  expect_equal(label(NULL), "—")
})

test_that(".dta_cond_to_text renders a condition list as a short readable string", {
  cond_to_text <- app_fn(".dta_cond_to_text")

  expect_equal(cond_to_text(list(VISIT = list(equals = "V03"))), "VISIT equals V03")
  expect_equal(
    cond_to_text(list(AGE = list(min = 10, max = 100), WEIGHT = list(greater_equal = 5))),
    "AGE min 10; WEIGHT greater_equal 5"
  )
  expect_equal(cond_to_text(NULL), "")
  expect_equal(cond_to_text(list()), "")
})

# ---- supported types / backends --------------------------------------------

test_that("dta_supported_types / dta_supported_backends / dta_sas_types return the real package sets", {
  expect_equal(app_fn("dta_supported_types")(), c("tabular", "file"))
  expect_equal(app_fn("dta_supported_backends")(), "SAS")
  expect_equal(
    app_fn("dta_sas_types")(),
    c("Char", "Num", "Int", "Date", "Time", "DateTime")
  )
})

# ---- handler_* / dta_handler_type -------------------------------------------

test_that("handler_* helpers read the real pattern handler of the clinical fixture", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")
  h <- app_fn("dta_handlers")(ds)[[1]]

  expect_equal(app_fn("handler_min")(h), 1)
  expect_equal(app_fn("handler_max")(h), 1)
  expect_true(app_fn("handler_is_pattern")(h))
  expect_equal(app_fn("handler_expected")(h), "clinical_data.*.csv$")
  expect_equal(app_fn("handler_count_label")(h), "1 file")
  expect_equal(app_fn("dta_handler_type")(h), "csv")
  expect_true(app_fn("handler_matches")(h, "clinical_data.2024.csv"))
  expect_false(app_fn("handler_matches")(h, "unrelated_file.csv"))
})

test_that("handler_* helpers read the non-pattern TSV handler of the standalone GF dataset", {
  ds <- read_dataset_from_yaml(app_fixture_path("gf_dataset.yaml"))
  h <- app_fn("dta_handlers")(ds)[[1]]

  expect_equal(app_fn("handler_min")(h), 1)
  expect_equal(app_fn("handler_max")(h), 1)
  expect_false(app_fn("handler_is_pattern")(h))
  expect_equal(app_fn("handler_expected")(h), "gf_data_small_smirna.tsv")
  expect_equal(app_fn("dta_handler_type")(h), "tsv")
})

test_that("handler_* helpers fall back to their documented default on a non-handler object", {
  fake <- structure(list(), class = "not_a_dta_handler")

  expect_true(is.na(app_fn("handler_min")(fake)))
  expect_true(is.na(app_fn("handler_max")(fake)))
  expect_false(app_fn("handler_is_pattern")(fake))
  expect_equal(app_fn("handler_hint")(fake), "")
  expect_equal(app_fn("handler_count_label")(fake), "")
  expect_false(app_fn("handler_matches")(fake, "anything.csv"))
  # dta_handler_type has no error-guarded fallback branch of its own: an
  # object that is neither DTAFileTSV nor DTAFileCSV falls through to "csv".
  expect_equal(app_fn("dta_handler_type")(fake), "csv")
})

# ---- dataset / metadata introspection ---------------------------------------

test_that("dta_dataset_names lists the datasets of a DTA and is empty for a dataset-less DTA", {
  expect_equal(app_fn("dta_dataset_names")(app_fixture_dta()), "clinical_data")
  expect_equal(app_fn("dta_dataset_names")(DTA(datasets = list())), character(0))
})

test_that("dta_get_dataset returns the named dataset or NULL when absent", {
  dta <- app_fixture_dta()

  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")
  expect_equal(ds@name, "clinical_data")
  expect_null(app_fn("dta_get_dataset")(dta, "no_such_dataset"))
})

test_that("dta_handlers exposes the dataset's file handler list", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")

  h <- app_fn("dta_handlers")(ds)

  expect_length(h, 1)
  expect_true(inherits(h[[1]], "DTAtools::DTAFileCSV"))
})

test_that("dta_dataset_table_names and dta_dataset_content_count reflect actual bound data", {
  dta_empty <- app_fixture_dta()
  ds_empty <- app_fn("dta_get_dataset")(dta_empty, "clinical_data")

  expect_equal(app_fn("dta_dataset_table_names")(ds_empty), character(0))
  expect_equal(app_fn("dta_dataset_content_count")(ds_empty), 0L)
  expect_equal(app_fn("dta_dataset_content_count")(NULL), 0L)

  dta_loaded <- app_fixture_dta_with_data()
  ds_loaded <- app_fn("dta_get_dataset")(dta_loaded, "clinical_data")

  expect_equal(app_fn("dta_dataset_table_names")(ds_loaded), "clinical_data")
  expect_equal(app_fn("dta_dataset_content_count")(ds_loaded), 1L)
})

test_that("dta_dataset_content_count with NULL after unloading detects the vacuous case", {
  # A tabular dataset with zero tables (never loaded) must count 0, not error.
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")

  expect_equal(app_fn("dta_dataset_content_count")(ds), 0L)
})

test_that("contact_display formats a name/role pair and falls back for missing parts", {
  contact_display <- app_fn("contact_display")

  expect_equal(contact_display(list(name = "Alice", role = "Lead")), "Alice — Lead")
  expect_equal(contact_display(list(name = "NoRole")), "NoRole")
  expect_equal(contact_display(list()), "(unnamed)")
})

test_that("dta_contacts reads the receiver/supplier contact lists from metadata", {
  dta <- app_fixture_dta()

  receiver <- app_fn("dta_contacts")(dta, "receiver")
  expect_length(receiver, 4)
  expect_equal(receiver[[1]]$name, "Alice Smith")
  expect_equal(receiver[[1]]$role, "Lead Data Manager")

  supplier <- app_fn("dta_contacts")(dta, "supplier")
  expect_length(supplier, 2)
  expect_equal(supplier[[1]]$name, "Emily Turner")
})

test_that("dta_contact_at returns one contact by 1-based index, NULL when out of range", {
  dta <- app_fixture_dta()
  contact_at <- app_fn("dta_contact_at")

  expect_equal(contact_at(dta, "receiver", 1)$name, "Alice Smith")
  expect_null(contact_at(dta, "receiver", 99))
  expect_null(contact_at(dta, "receiver", 0))
})

test_that("dta_affiliation reads the side-level affiliation fields", {
  dta <- app_fixture_dta()
  affiliation <- app_fn("dta_affiliation")

  expect_equal(affiliation(dta, "receiver")$name, "Test Company")
  supplier_aff <- affiliation(dta, "supplier")
  expect_equal(supplier_aff$name, "Test Company 2")
  expect_equal(supplier_aff$country, "Test Country")
  expect_equal(supplier_aff$address, "Test Address 2")
})

test_that("dta_transmission reads the metadata transmission block", {
  dta <- app_fixture_dta()

  tr <- app_fn("dta_transmission")(dta)

  expect_equal(tr$type, "secure S3 bucket")
  expect_equal(tr$frequency, "one-time")
  expect_false(tr$test_upload)
  expect_false(tr$blinded_transfer)
})

# ---- <thing>_to_list serializers -------------------------------------------

test_that("dta_handler_to_list mirrors the handler's filename/type/pattern/count", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")
  h <- app_fn("dta_handlers")(ds)[[1]]

  l <- app_fn("dta_handler_to_list")(h)

  expect_equal(l$filename, "clinical_data.*.csv$")
  expect_equal(l$type, "csv")
  expect_true(l$pattern)
  expect_equal(l$number_of_files, 1L)
})

test_that("dta_column_to_list mirrors a real column's type/length/values/description", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")
  col <- ds@specs@columns[["STUDYID"]]

  l <- app_fn("dta_column_to_list")(col)

  expect_equal(l$id, "STUDYID")
  expect_equal(l$type, "SAS Char")
  expect_equal(l$length, 10L)
  expect_false(l$nullable)
  expect_equal(l$values, "1234-5678")
  expect_equal(l$description, "Unique study ID")
})

test_that("dta_rule_to_list normalizes a col_condition rule's condition/then", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")
  rule <- ds@specs@rules[[1]]

  l <- app_fn("dta_rule_to_list")(rule)

  expect_equal(l$id, "rule_equal_example")
  expect_equal(l$type, "col_condition")
  expect_equal(l$condition, list(VISIT = list(equals = "V03")))
  expect_equal(l$then, list(STATUS = list(equals = "COMPLETED")))
})

test_that("dta_rule_to_list normalizes a col_range rule's columns/min/max", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")
  rule <- ds@specs@rules[[3]]

  l <- app_fn("dta_rule_to_list")(rule)

  expect_equal(l$id, "rule_range_example")
  expect_equal(l$type, "col_range")
  expect_equal(l$columns, "AGE")
  expect_equal(l$min, 18)
  expect_equal(l$max, 65)
})

test_that("dta_rule_to_list normalizes a group_condition rule", {
  rule <- DTARuleGroupCondition(
    id = "g_rule",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(
      c1 = list(STATUS = list(equals = "FAILED")),
      c2 = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "requires", `if` = "c1", then = "c2"))
  )

  l <- app_fn("dta_rule_to_list")(rule)

  expect_equal(l$id, "g_rule")
  expect_equal(l$type, "group_condition")
  expect_equal(l$group_by, c("SUBJECT_ID", "VISIT"))
  expect_equal(names(l$conditions), c("c1", "c2"))
  expect_equal(l$constraints[[1]]$type, "requires")
})

test_that("dta_dataset_to_list builds the standalone-dataset shape with all columns and rules", {
  dta <- app_fixture_dta()
  ds <- app_fn("dta_get_dataset")(dta, "clinical_data")

  l <- app_fn("dta_dataset_to_list")(ds)

  expect_equal(l$name, "clinical_data")
  expect_equal(l$type, "tabular")
  expect_length(l$columns, 14)
  expect_length(l$rules, 8)
})

test_that("dta_to_list builds metadata + datasets top-level keys", {
  dta <- app_fixture_dta()

  l <- app_fn("dta_to_list")(dta)

  expect_setequal(names(l), c("metadata", "datasets"))
  expect_equal(l$metadata$title, "Clinical Data Specification")
  expect_length(l$datasets, 1)
  expect_equal(l$datasets[[1]]$name, "clinical_data")
})

# ---- YAML blank-line layout -------------------------------------------------

# The lines of a YAML string, so a layout expectation can be written as the
# document it is meant to produce.
yaml_lines <- function(x) strsplit(x, "\n", fixed = TRUE)[[1]]

# A document with a nested metadata block, a dataset, and a column list: one of
# each level the formatter has to tell apart.
nested_yaml_fixture <- function() {
  paste(
    "metadata:",
    "  title: T",
    "  receiver:",
    "    name: N",
    "datasets:",
    "- name: d",
    "  type: tabular",
    "  columns:",
    "  - id: A",
    "    label: a",
    "  - id: B",
    "    label: b",
    sep = "\n"
  )
}

test_that("dta_yaml_blank_lines spaces blocks down to max_depth and leaves deeper ones tight", {
  out <- app_fn("dta_yaml_blank_lines")(nested_yaml_fixture(), max_depth = 3L)

  expect_equal(
    yaml_lines(out),
    c(
      "metadata:",
      "  title: T",
      "",
      "  receiver:",
      "    name: N",
      "",
      "datasets:",
      "- name: d",
      "  type: tabular",
      "",
      "  columns:",
      "  - id: A",
      "    label: a",
      "  - id: B",
      "    label: b"
    )
  )
})

test_that("dta_yaml_blank_lines honours max_depth: at 1 only the top-level blocks separate", {
  out <- app_fn("dta_yaml_blank_lines")(nested_yaml_fixture(), max_depth = 1L)
  lines <- yaml_lines(out)

  # The one blank closes `metadata:` before `datasets:`; `receiver:` (depth 2)
  # and `columns:` (depth 3) are now below the threshold and stay tight.
  expect_equal(which(!nzchar(lines)), 5L)
  expect_equal(lines[[6]], "datasets:")
})

test_that("dta_yaml_blank_lines lays a standalone dataset out at max_depth 1", {
  text <- paste(
    "name: d",
    "type: tabular",
    "files:",
    "  filename: f.csv",
    "columns:",
    "- id: A",
    "  label: a",
    "- id: B",
    sep = "\n"
  )

  expect_equal(
    yaml_lines(app_fn("dta_yaml_blank_lines")(text, max_depth = 1L)),
    c(
      "name: d",
      "type: tabular",
      "",
      "files:",
      "  filename: f.csv",
      "",
      "columns:",
      "- id: A",
      "  label: a",
      "- id: B"
    )
  )
})

test_that("dta_yaml_blank_lines keeps a block flush against the parent that opens it", {
  # `datasets:` is immediately followed by its first entry, and `receiver:` by
  # its first key: a blank there would orphan the parent line.
  lines <- yaml_lines(
    app_fn("dta_yaml_blank_lines")(nested_yaml_fixture(), max_depth = 3L)
  )

  expect_equal(lines[[which(lines == "datasets:") + 1L]], "- name: d")
  expect_equal(lines[[which(lines == "  receiver:") + 1L]], "    name: N")
})

test_that("dta_yaml_blank_lines preserves the trailing newline and adds none of its own", {
  out <- app_fn("dta_yaml_blank_lines")(
    paste0(nested_yaml_fixture(), "\n"),
    max_depth = 3L
  )

  expect_true(endsWith(out, "b\n"))
  expect_false(endsWith(out, "\n\n"))
})

test_that("dta_yaml_blank_lines carries a block scalar body through untouched", {
  # The body contains a blank line and a line that looks like a key. Reading
  # either as structure would insert a blank INSIDE the scalar, changing the
  # value rather than the layout.
  text <- paste(
    "metadata:",
    "  title: T",
    "  note: |-",
    "    line one",
    "    key: not a key",
    "",
    "    after blank",
    "  other: x",
    sep = "\n"
  )

  out <- app_fn("dta_yaml_blank_lines")(text, max_depth = 3L)
  lines <- yaml_lines(out)
  start <- which(lines == "  note: |-")

  expect_equal(
    lines[start + seq_len(4L)],
    c("    line one", "    key: not a key", "", "    after blank")
  )
  expect_identical(yaml::yaml.load(out), yaml::yaml.load(text))
})

test_that("dta_yaml_blank_lines survives the block scalar header as.yaml really writes", {
  # A value whose first line starts with a space forces an explicit indentation
  # indicator, and yaml writes it BEFORE the chomping one: `|2-`, not `|-`. A
  # header regex that only allowed chomp-then-digit parsed the scalar's body as
  # structure and spaced blank lines into the middle of a user's text. The
  # fixture is emitted rather than hand-typed so it cannot drift from reality.
  text <- yaml::as.yaml(
    list(metadata = list(note = " indented first line\nsecond\n  deeper\nfourth")),
    indent = 2, line.sep = "\n"
  )
  expect_true(grepl("|2-", text, fixed = TRUE))

  out <- app_fn("dta_yaml_blank_lines")(text, max_depth = 3L)

  expect_identical(yaml::yaml.load(out), yaml::yaml.load(text))
  expect_identical(
    yaml::yaml.load(out)$metadata$note,
    " indented first line\nsecond\n  deeper\nfourth"
  )
})

test_that("dta_yaml_blank_lines does not treat a folded long value as a nested block", {
  # as.yaml() wraps a long plain scalar across several lines, indenting the
  # continuations exactly as it would a child key. They are the value, not
  # structure, and must not earn the field blank lines of its own.
  long <- paste(rep("wordy", 40), collapse = " ")
  # `template_source` follows `description` in a real dataset, and being a plain
  # scalar it is what makes this discriminate: if the folded value counted as a
  # block, closing it would push a blank line in front of the scalar after it.
  text <- yaml::as.yaml(
    list(datasets = list(list(
      name = "d", description = long, template_source = "t.xlsx",
      columns = list(list(id = "A"))
    ))),
    indent = 2, line.sep = "\n"
  )
  expect_gt(length(grep("^ +wordy", yaml_lines(text))), 0L)

  lines <- yaml_lines(app_fn("dta_yaml_blank_lines")(text, max_depth = 3L))

  expect_match(
    lines[[which(lines == "  template_source: t.xlsx") - 1L]], "^ +wordy"
  )
  expect_true(all(nzchar(lines[grep("^ +wordy", lines) - 1L])))
  expect_identical(
    yaml::yaml.load(paste(lines, collapse = "\n"))$datasets[[1]]$description,
    long
  )
})

test_that("dta_yaml_blank_lines does not split folded prose that contains a colon", {
  # `... and then Note: something ...` wrapped onto a continuation line is
  # indistinguishable from a mapping entry by shape alone. What settles it is
  # the OWNING line: `description:` already carries its value inline, so in
  # block style it cannot have children at all.
  prose <- paste(
    paste(rep("filler", 14), collapse = " "),
    "and then Note: something follows, e.g.: a second one",
    paste(rep("filler", 14), collapse = " ")
  )
  text <- yaml::as.yaml(
    list(datasets = list(list(
      name = "d", description = prose, template_source = "t.xlsx",
      columns = list(list(id = "A"))
    ))),
    indent = 2, line.sep = "\n"
  )
  # Pin the premise: a continuation line really does carry a `key: value` shape.
  expect_gt(length(grep("^ +filler.*: ", yaml_lines(text))), 0L)

  out <- app_fn("dta_yaml_blank_lines")(text, max_depth = 3L)
  lines <- yaml_lines(out)

  # The value is untouched, the scalar after it gains no blank line, and the
  # document keeps its layout -- the guard bailing out here would silently cost
  # the whole document its formatting.
  expect_identical(yaml::yaml.load(out)$datasets[[1]]$description, prose)
  expect_true(nzchar(lines[[which(lines == "  template_source: t.xlsx") - 1L]]))
  expect_equal(lines[[which(lines == "  columns:") - 1L]], "")
})

test_that("dta_yaml_blank_lines leaves text it cannot own alone", {
  text <- nested_yaml_fixture()
  blank_lines <- app_fn("dta_yaml_blank_lines")

  # max_depth below the top level: nothing is spaced, so nothing changes.
  expect_identical(blank_lines(text, max_depth = 0L), text)
  # Already laid out -- running twice must not double the blanks.
  once <- blank_lines(text, max_depth = 3L)
  expect_identical(blank_lines(once, max_depth = 3L), once)
  # Degenerate input.
  expect_identical(blank_lines("", max_depth = 3L), "")
  expect_identical(blank_lines("{}\n", max_depth = 3L), "{}\n")
  expect_null(blank_lines(NULL, max_depth = 3L))
})

# ---- YAML text serialization ------------------------------------------------

test_that("dta_to_yaml_text serializes the whole DTA including metadata and dataset content", {
  res <- app_fn("dta_to_yaml_text")(app_fixture_dta())

  expect_true(res$ok)
  expect_true(is.character(res$value) && nzchar(res$value))
  expect_true(grepl("title: Clinical Data Specification", res$value, fixed = TRUE))
  expect_true(grepl("clinical_data", res$value, fixed = TRUE))
  expect_true(grepl("STUDYID", res$value, fixed = TRUE))
})

test_that("dta_to_yaml_text lays the document out in blank-line separated sections", {
  res <- app_fn("dta_to_yaml_text")(app_fixture_dta())
  expect_true(res$ok)
  lines <- yaml_lines(res$value)

  # `    contacts:` occurs under both receiver and supplier, so every
  # occurrence of a key is checked rather than an assumed single one.
  expect_blank_before <- function(key) {
    at <- which(lines == key)
    expect_gt(length(at), 0L)
    expect_equal(lines[at - 1L], rep("", length(at)), info = key)
  }

  # Top-level paragraphs, and the depth-2/3 blocks inside them.
  expect_blank_before("datasets:")
  expect_blank_before("  supplier:")
  expect_blank_before("    contacts:")
  # A dataset's own sections.
  expect_blank_before("  columns:")
  expect_blank_before("  rules:")
  # ... but not its list entries: two adjacent columns stay tight.
  ids <- which(grepl("^  - id: ", lines))
  expect_gt(length(ids), 1L)
  expect_true(all(nzchar(lines[ids[-1] - 1L])))

  # A block never opens with a blank, and the document never ends with one.
  expect_equal(lines[[which(lines == "datasets:") + 1L]], "- name: clinical_data")
  expect_true(nzchar(lines[[length(lines)]]))
  expect_false(any(!nzchar(utils::head(lines, -1L)) & !nzchar(lines[-1])))
})

test_that("dta_dataset_to_yaml_text serializes one dataset and errors for an unknown name", {
  dta <- app_fixture_dta()

  ok_res <- app_fn("dta_dataset_to_yaml_text")(dta, "clinical_data")
  expect_true(ok_res$ok)
  expect_true(grepl("name: clinical_data", ok_res$value, fixed = TRUE))

  bad_res <- app_fn("dta_dataset_to_yaml_text")(dta, "no_such_dataset")
  expect_false(bad_res$ok)
  expect_null(bad_res$value)
})

test_that("dta_dataset_to_yaml_text separates the dataset's own sections at its own root", {
  res <- app_fn("dta_dataset_to_yaml_text")(app_fixture_dta(), "clinical_data")
  expect_true(res$ok)
  lines <- yaml_lines(res$value)

  # `files:`/`columns:`/`rules:` are top-level here rather than two levels down,
  # and must still be the things that separate.
  expect_equal(lines[[which(lines == "files:") - 1L]], "")
  expect_equal(lines[[which(lines == "columns:") - 1L]], "")
  expect_equal(lines[[which(lines == "rules:") - 1L]], "")

  ids <- which(grepl("^- id: ", lines))
  expect_gt(length(ids), 1L)
  expect_true(all(nzchar(lines[ids[-1] - 1L])))
})

# ---- dta_read_yaml / dta_read_yaml_text ------------------------------------

test_that("dta_read_yaml loads a real DTA YAML and reports has_metadata / not wrapped", {
  res <- app_fn("dta_read_yaml")(app_fixture_path("clinical_dta.yaml"))

  expect_true(res$ok)
  expect_false(res$dataset_only)
  expect_true(res$has_metadata)
  expect_false(res$wrapped_dataset)
  expect_equal(app_fn("dta_dataset_names")(res$value), "clinical_data")
})

test_that("dta_read_yaml wraps a standalone dataset YAML into a new DTA (no metadata)", {
  res <- app_fn("dta_read_yaml")(app_fixture_path("gf_dataset.yaml"))

  expect_true(res$ok)
  expect_false(res$dataset_only)
  expect_false(res$has_metadata)
  expect_true(res$wrapped_dataset)
  expect_equal(app_fn("dta_dataset_names")(res$value), "gf_data_specs_pattern")
})

test_that("dta_read_yaml reports ok = FALSE with an error for a nonexistent path", {
  # file() emits a base R warning (translated under a non-English locale) when
  # it cannot open the path; suppress it here since we only assert on the
  # dta_try() ok/error shape, never on that message text.
  res <- suppressWarnings(
    app_fn("dta_read_yaml")(file.path(tempdir(), "does_not_exist_12345.yaml"))
  )

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(is.character(res$error) && nzchar(res$error))
  expect_false(res$has_metadata)
  expect_false(res$wrapped_dataset)
})

test_that("dta_read_yaml_text reports ok = FALSE for syntactically malformed YAML", {
  res <- app_fn("dta_read_yaml_text")("key: [unterminated\n  - broken: yaml: : :")

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(is.character(res$error) && nzchar(res$error))
})

test_that("dta_read_yaml_text accepts a real DTA document passed as text", {
  yaml_text <- paste(readLines(app_fixture_path("clinical_dta.yaml")), collapse = "\n")

  res <- app_fn("dta_read_yaml_text")(yaml_text)

  expect_true(res$ok)
  expect_true(res$has_metadata)
  expect_equal(app_fn("dta_dataset_names")(res$value), "clinical_data")
})

test_that("YAML round-trip: to_yaml_text -> read_yaml_text preserves dataset/column/rule identity", {
  dta <- app_fixture_dta()

  serialized <- app_fn("dta_to_yaml_text")(dta)
  expect_true(serialized$ok)

  round <- app_fn("dta_read_yaml_text")(serialized$value)
  expect_true(round$ok)
  dta2 <- round$value

  expect_equal(
    app_fn("dta_dataset_names")(dta2),
    app_fn("dta_dataset_names")(dta)
  )
  expect_equal(
    app_fn("dta_column_ids")(dta2, "clinical_data"),
    app_fn("dta_column_ids")(dta, "clinical_data")
  )
  expect_equal(
    app_fn("dta_rules_overview")(dta2, "clinical_data")$id,
    app_fn("dta_rules_overview")(dta, "clinical_data")$id
  )
})

# ---- column editor read-only helpers ----------------------------------------

test_that("dta_column_ids lists column ids in spec order", {
  ids <- app_fn("dta_column_ids")(app_fixture_dta(), "clinical_data")

  expect_equal(ids[1:3], c("STUDYID", "VISIT", "AGE"))
  expect_length(ids, 14)
})

test_that("dta_columns_overview builds one row per column with id/type/constraint", {
  overview <- app_fn("dta_columns_overview")(app_fixture_dta(), "clinical_data")

  expect_equal(nrow(overview), 14)
  expect_equal(overview$id[1], "STUDYID")
  expect_equal(overview$type[1], "SAS Char")
  expect_equal(overview$constraint[1], "1234-5678")
  expect_equal(overview$constraint[2], "V01, V02, V03, EOT")
})

test_that("dta_column_fields splits backend/type/format for one column, NULL when absent", {
  fields <- app_fn("dta_column_fields")(app_fixture_dta(), "clinical_data", "STUDYID")

  expect_equal(fields$backend, "SAS")
  expect_equal(fields$type, "Char")
  expect_equal(fields$length, "10")
  expect_false(fields$nullable)

  expect_null(app_fn("dta_column_fields")(app_fixture_dta(), "clinical_data", "NO_SUCH_COL"))
})

test_that("dta_rules_overview builds one row per rule with a human-readable detail", {
  overview <- app_fn("dta_rules_overview")(app_fixture_dta(), "clinical_data")

  expect_equal(nrow(overview), 8)
  expect_equal(overview$id[3], "rule_range_example")
  expect_equal(overview$detail[3], "AGE in [18, 65]")
  expect_equal(overview$id[5], "rule_unique_example")
  expect_equal(overview$detail[5], "unique(SUBJECT_ID, VISIT)")
})

test_that("dta_rule_fields returns the rule at a 1-based index, NULL out of range", {
  fields <- app_fn("dta_rule_fields")(app_fixture_dta(), "clinical_data", 3)

  expect_equal(fields$id, "rule_range_example")
  expect_equal(fields$columns, "AGE")
  expect_equal(fields$min, 18)
  expect_equal(fields$max, 65)

  expect_null(app_fn("dta_rule_fields")(app_fixture_dta(), "clinical_data", 99))
  expect_null(app_fn("dta_rule_fields")(app_fixture_dta(), "clinical_data", 0))
})

# ---- signatures --------------------------------------------------------------

test_that("dta_handlers_signature is stable for the same dataset and differs across datasets", {
  dta <- app_fixture_dta()
  ds_clinical <- app_fn("dta_get_dataset")(dta, "clinical_data")
  ds_gf <- read_dataset_from_yaml(app_fixture_path("gf_dataset.yaml"))

  sig <- app_fn("dta_handlers_signature")

  expect_identical(sig(ds_clinical), sig(ds_clinical))
  expect_false(identical(sig(ds_clinical), sig(ds_gf)))
})

test_that("dta_specs_signature is stable for the same dataset and differs across datasets", {
  dta <- app_fixture_dta()
  ds_clinical <- app_fn("dta_get_dataset")(dta, "clinical_data")
  ds_gf <- read_dataset_from_yaml(app_fixture_path("gf_dataset.yaml"))

  sig <- app_fn("dta_specs_signature")

  expect_identical(sig(ds_clinical), sig(ds_clinical))
  expect_false(identical(sig(ds_clinical), sig(ds_gf)))
})

# ---- validation report --------------------------------------------------------

test_that("dta_build_validation_report includes dataset names and reflects nodata status", {
  dta <- app_fixture_dta()
  status <- c(clinical_data = "nodata")

  html <- app_fn("dta_build_validation_report")(dta, status)

  expect_true(grepl("clinical_data", html, fixed = TRUE))
  expect_true(grepl("No data", html, fixed = TRUE))
  expect_true(grepl("VALIDATION INCOMPLETE", html, fixed = TRUE))
  expect_true(grepl("<!doctype html>", html, fixed = TRUE))
})

test_that("dta_build_validation_report reports a passed banner when every dataset passed", {
  dta <- app_fixture_dta()
  status <- c(clinical_data = "pass")

  html <- app_fn("dta_build_validation_report")(dta, status)

  expect_true(grepl("VALIDATION PASSED", html, fixed = TRUE))
  expect_true(grepl("1 passed, 0 failed, 0 without data", html, fixed = TRUE))
})

test_that("dta_export_stem puts the version between the title and the date", {
  stem_fn <- app_fn("dta_export_stem")
  dta <- app_fixture_dta()
  ver <- as.character(S7::prop(dta@metadata, "version"))[1]
  expect_true(nzchar(ver))

  stem <- stem_fn(dta, when = as.POSIXct("2024-03-07 14:07:00", tz = "UTC"))

  # Title first, then "-v<version>-", then the date and time.
  expect_true(endsWith(stem, paste0("-v", ver, "-2024-03-07_14-07")))
  expect_gt(regexpr("-v", stem, fixed = TRUE), 1L)
  # The version reads as authored: dots are not mangled into underscores.
  expect_true(grepl(paste0("v", ver), stem, fixed = TRUE))
})

test_that("dta_export_stem omits the version segment when no version is set", {
  stem_fn <- app_fn("dta_export_stem")
  dta <- app_fixture_dta()

  meta <- dta@metadata
  S7::prop(meta, "version") <- NULL
  dta@metadata <- meta

  stem <- stem_fn(dta, when = as.POSIXct("2024-03-07 14:07:00", tz = "UTC"))
  expect_match(stem, "_2024-03-07_14-07$")
  expect_false(grepl("-v", stem, fixed = TRUE))
  # No doubled separator left where the version segment would have been.
  expect_false(grepl("__", stem, fixed = TRUE))
})

test_that("dta_export_stem falls back to DTA when there is no title", {
  stem_fn <- app_fn("dta_export_stem")
  dta <- app_fixture_dta()
  meta <- dta@metadata
  S7::prop(meta, "title") <- NULL
  dta@metadata <- meta

  expect_match(stem_fn(dta, when = as.POSIXct("2024-03-07 14:07:00", tz = "UTC")), "^DTA-v")
})

test_that("dta_export_stem renders a bare Date as midnight rather than now", {
  stem_fn <- app_fn("dta_export_stem")
  dta <- app_fixture_dta()

  expect_true(endsWith(stem_fn(dta, when = as.Date("2024-03-07")), "-2024-03-07_00-00"))
})
