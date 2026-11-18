# ${study.id} / ${study.protocol_id} in dataset bodies (template_core.R:
# resolve_dataset_tokens(), create_dta_from_template()). A template reuses the
# study identity entered once at creation in a file name pattern and in a
# column's allowed values. Reached through the app_fn() harness
# (helper-shinyapp.R).

# One dataset template using every token context: a pattern filename (regex),
# its pattern_description (text), column values (text), a column pattern and
# its example (regex / text), and a rule's equals (text) and pattern (regex).
write_token_dataset_template <- function(root) {
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: tok_dt",
      'version: "1.0"',
      "label: Token dataset",
      "dataset:",
      "  id: tok_data",
      "  type: tabular",
      "  name: Token data",
      "  files:",
      "    filename: ^s${study.id}_[0-9]{12}\\.csv$",
      "    pattern: true",
      "    pattern_description: s${study.id}_YYYYMMDDHHMM.csv",
      "    type: csv",
      "  columns:",
      "    - id: STUDYID",
      "      label: Study ID",
      "      type: SAS Char",
      "      format: SAS $20.",
      "      values: ${study.id}",
      "    - id: PROTID",
      "      label: Protocol ID",
      "      type: SAS Char",
      "      format: SAS $20.",
      "      values: ${study.protocol_id}",
      "    - id: SUBJID",
      "      label: Subject",
      "      type: SAS Char",
      "      format: SAS $30.",
      "      pattern: ^${study.id}-[0-9]{4}$",
      "      examples: ${study.id}-0001",
      "  rules:",
      "    - id: subject_in_study",
      "      type: col_condition",
      "      condition:",
      "        STUDYID:",
      "          equals: ${study.id}",
      "      then:",
      "        SUBJID:",
      "          pattern: ^${study.id}-"
    ),
    lib_path(root, "tok_dt.dta-dataset-template.yaml")
  )
}

# The creation template asks for the study identity the ordinary way: options
# targeting metadata.study.
write_token_creation_template <- function(root) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: tok_ct",
      'version: "1.0"',
      "label: Token CT",
      "base:",
      "  metadata:",
      "    title: Token DTS",
      '    version: "1.0"',
      "datasets:",
      "  - template: tok_dt@1.0",
      "options:",
      "  - id: study_id",
      "    label: Study ID",
      "    type: text",
      "    target: metadata.study.id",
      "  - id: protocol_id",
      "    label: Protocol ID",
      "    type: text",
      "    target: metadata.study.protocol_id"
    ),
    lib_path(root, "tok_ct.dta-template.yaml")
  )
}

token_index <- function(root, .local_envir = parent.frame()) {
  local_clean_template_env(.local_envir = .local_envir)
  app_fn("dta_template_index_invalidate")()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root), .local_envir = .local_envir)
  app_fn("build_template_index")()
}

token_fixture <- function(.local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  write_token_dataset_template(root)
  write_token_creation_template(root)
  list(root = root, index = token_index(root, .local_envir = .local_envir))
}

# Create from tok_ct@1.0 with these option answers; returns the dta_try() result.
create_token_dta <- function(fx, selections = list(), ...) {
  loaded <- app_fn("load_template_definition")("tok_ct@1.0", index = fx$index)
  expect_true(loaded$ok, info = loaded$error)
  app_fn("create_dta_from_template")(
    loaded$value$def, loaded$value$path,
    selections = selections, index = fx$index, ...
  )
}

token_dataset <- function(dta) DTAtools::datasets(dta)[["tok_data"]]

token_rule <- function(ds) {
  app_fn("dta_rule_to_list")(ds@specs@rules[[1]])
}

file_accepts <- function(f, name) any(DTAtools::matches_filename(f, name))

test_that("a dataset's study tokens become the study identity the document was created with", {
  fx <- token_fixture()
  res <- create_token_dta(fx, list(study_id = "1001_0190", protocol_id = "BI 1001-0190"))
  expect_true(res$ok, info = res$error)

  expect_identical(DTAtools::metadata(res$value)@study$id, "1001_0190")
  ds <- token_dataset(res$value)
  f <- ds@files[[1]]
  expect_identical(f@filename, "^s1001_0190_[0-9]{12}\\.csv$")
  expect_identical(f@pattern_description, "s1001_0190_YYYYMMDDHHMM.csv")
  expect_true(file_accepts(f, "s1001_0190_202611181200.csv"))
  expect_false(file_accepts(f, "s1001_0191_202611181200.csv"))
  expect_false(file_accepts(f, "s1001_0190_2026111812.csv"))

  cols <- ds@specs@columns
  expect_identical(unlist(cols[["STUDYID"]]@values), "1001_0190")
  expect_identical(unlist(cols[["PROTID"]]@values), "BI 1001-0190")
})

test_that("a study id with regex metacharacters is matched literally where it sits in a pattern", {
  fx <- token_fixture()
  res <- create_token_dta(fx, list(study_id = "1199.187(a)", protocol_id = "P-1"))
  expect_true(res$ok, info = res$error)
  ds <- token_dataset(res$value)

  # Filename: the "." and the "(a)" are literal, not "any character" and a group.
  f <- ds@files[[1]]
  expect_identical(f@filename, "^s1199\\.187\\(a\\)_[0-9]{12}\\.csv$")
  expect_true(file_accepts(f, "s1199.187(a)_202611181200.csv"))
  expect_false(file_accepts(f, "s1199x187(a)_202611181200.csv"))
  expect_false(file_accepts(f, "s1199.187a_202611181200.csv"))
  # The description is text, so it keeps the id as entered.
  expect_identical(f@pattern_description, "s1199.187(a)_YYYYMMDDHHMM.csv")

  cols <- ds@specs@columns
  expect_identical(unlist(cols[["STUDYID"]]@values), "1199.187(a)")
  expect_identical(cols[["SUBJID"]]@pattern, "^1199\\.187\\(a\\)-[0-9]{4}$")
  # The example is text (the column only built because it matches the pattern).
  expect_identical(unlist(cols[["SUBJID"]]@examples), "1199.187(a)-0001")
  expect_true(grepl(cols[["SUBJID"]]@pattern, "1199.187(a)-0042", perl = TRUE))
  expect_false(grepl(cols[["SUBJID"]]@pattern, "1199x187(a)-0042", perl = TRUE))

  rule <- token_rule(ds)
  expect_identical(unlist(rule$condition$STUDYID$equals), "1199.187(a)")
  expect_identical(unlist(rule$then$SUBJID$pattern), "^1199\\.187\\(a\\)-")
})

test_that("the study id an option sets beats base.metadata, and is what the tokens resolve to", {
  fx <- token_fixture()
  loaded <- app_fn("load_template_definition")("tok_ct@1.0", index = fx$index)
  def <- loaded$value$def
  def$base$metadata$study <- list(id = "BASE-1", protocol_id = "BASE-P")
  create <- app_fn("create_dta_from_template")

  from_base <- create(def, loaded$value$path, selections = list(), index = fx$index)
  expect_true(from_base$ok, info = from_base$error)
  expect_identical(unlist(token_dataset(from_base$value)@specs@columns[["STUDYID"]]@values), "BASE-1")

  from_option <- create(def, loaded$value$path, selections = list(study_id = "OPT-2"), index = fx$index)
  expect_true(from_option$ok, info = from_option$error)
  expect_identical(DTAtools::metadata(from_option$value)@study$id, "OPT-2")
  cols <- token_dataset(from_option$value)@specs@columns
  expect_identical(unlist(cols[["STUDYID"]]@values), "OPT-2")
  expect_identical(unlist(cols[["PROTID"]]@values), "BASE-P")
})

test_that("creating a document whose dataset uses a study token it has no value for fails, naming the dataset", {
  fx <- token_fixture()
  unanswered <- create_token_dta(fx, list())
  expect_false(unanswered$ok)
  expect_match(unanswered$error, "tok_data", fixed = TRUE)
  expect_match(unanswered$error, "${study.id}", fixed = TRUE)

  # A blank answer is "not supplied", not an id of "".
  blank <- create_token_dta(fx, list(study_id = "   ", protocol_id = "P-1"))
  expect_false(blank$ok)
  expect_match(blank$error, "${study.id}", fixed = TRUE)

  # The protocol id is checked on its own.
  no_protocol <- create_token_dta(fx, list(study_id = "1001_0190"))
  expect_false(no_protocol$ok)
  expect_match(no_protocol$error, "${study.protocol_id}", fixed = TRUE)
})

test_that("resolve_dataset_tokens() raises a classed error for a token with no value", {
  env <- app_fn("dta_template_dataset_env")()
  expect_error(
    app_fn("resolve_dataset_tokens")(list(id = "x", columns = list(list(values = "${study.id}"))), env, "x"),
    class = "dta_template_token_missing"
  )
  # No token in the body: nothing to answer, nothing raised.
  expect_identical(
    app_fn("resolve_dataset_tokens")(list(id = "x", note = "plain"), env, "x"),
    list(id = "x", note = "plain")
  )
})

test_that("a study_fallback answers the tokens without being written to the metadata", {
  fx <- token_fixture()
  res <- create_token_dta(fx, list(), study_fallback = list(id = "FB-1", protocol_id = "FB-P"))
  expect_true(res$ok, info = res$error)
  expect_null(DTAtools::metadata(res$value)@study$id)
  cols <- token_dataset(res$value)@specs@columns
  expect_identical(unlist(cols[["STUDYID"]]@values), "FB-1")
  expect_identical(unlist(cols[["PROTID"]]@values), "FB-P")

  # The document's own answer still wins over the fallback.
  answered <- create_token_dta(fx, list(study_id = "OWN-1"), study_fallback = list(id = "FB-1", protocol_id = "FB-P"))
  expect_true(answered$ok, info = answered$error)
  expect_identical(unlist(token_dataset(answered$value)@specs@columns[["STUDYID"]]@values), "OWN-1")
})

test_that("an inline dataset of a creation template resolves the tokens, with or without an index", {
  inline <- list(
    id = "inline_ds", type = "tabular",
    files = list(filename = "^${study.id}_[0-9]+\\.csv$", pattern = TRUE, type = "csv"),
    columns = list(list(id = "STUDYID", type = "SAS Char", values = "${study.id}"))
  )
  def <- list(
    base = list(metadata = list(title = "T", version = "1.0", study = list(id = "1199.187"))),
    datasets = list(inline)
  )
  create <- app_fn("create_dta_from_template")
  root <- withr::local_tempdir()

  without_index <- create(def, file.path(root, "t.yaml"))
  expect_true(without_index$ok, info = without_index$error)
  with_index <- create(def, file.path(root, "t.yaml"), index = app_fn("dta_template_index_empty")())
  expect_true(with_index$ok, info = with_index$error)

  for (res in list(without_index, with_index)) {
    ds <- DTAtools::datasets(res$value)[["inline_ds"]]
    expect_identical(unlist(ds@specs@columns[["STUDYID"]]@values), "1199.187")
    expect_true(file_accepts(ds@files[[1]], "1199.187_1.csv"))
    expect_false(file_accepts(ds@files[[1]], "1199x187_1.csv"))
  }
})

test_that("a template without study tokens builds unchanged with no study identity at all", {
  def <- list(
    base = list(metadata = list(title = "T", version = "1.0")),
    datasets = list(list(id = "mini_ds", type = "file", files = list(filename = "mini.csv", type = "csv")))
  )
  root <- withr::local_tempdir()
  res <- app_fn("create_dta_from_template")(def, file.path(root, "t.yaml"))
  expect_true(res$ok, info = res$error)
  expect_identical(DTAtools::datasets(res$value)[["mini_ds"]]@files[[1]]@filename, "mini.csv")
  expect_null(DTAtools::metadata(res$value)@study$id)
})

test_that("the study tokens are filled in the document metadata as entered, and fail there when unanswered", {
  fx <- token_fixture()
  loaded <- app_fn("load_template_definition")("tok_ct@1.0", index = fx$index)
  def <- loaded$value$def
  def$base$metadata$title <- "Transfer for ${study.id} (${study.protocol_id})"
  create <- app_fn("create_dta_from_template")

  res <- create(def, loaded$value$path, selections = list(study_id = "1199.187", protocol_id = "P-1"), index = fx$index)
  expect_true(res$ok, info = res$error)
  # Metadata is text, never a regex: no escaping.
  expect_identical(DTAtools::metadata(res$value)@title, "Transfer for 1199.187 (P-1)")

  # Answered for the datasets by a fallback, the metadata gets the same value.
  fb <- create(def, loaded$value$path, index = fx$index, study_fallback = list(id = "FB-1", protocol_id = "FB-P"))
  expect_true(fb$ok, info = fb$error)
  expect_identical(DTAtools::metadata(fb$value)@title, "Transfer for FB-1 (FB-P)")

  def$datasets <- list(list(id = "mini_ds", type = "file", files = list(filename = "mini.csv", type = "csv")))
  unanswered <- create(def, loaded$value$path, index = fx$index)
  expect_false(unanswered$ok)
  expect_match(unanswered$error, "title", fixed = TRUE)
  expect_match(unanswered$error, "${study.id}", fixed = TRUE)
})

test_that("a rule condition on a column named `pattern` compares the study id as entered", {
  env <- app_fn("dta_template_dataset_env")(study_fallback = list(id = "1199.187", protocol_id = "P"))
  rule <- list(
    id = "r", type = "col_condition",
    condition = list(pattern = list(equals = "${study.id}")),
    then = list(B = list(pattern = "^${study.id}$"))
  )
  out <- app_fn("resolve_dataset_tokens")(list(id = "x", rules = list(rule)), env, "x")
  # The column called "pattern" holds a comparison, not a regex...
  expect_identical(out$rules[[1]]$condition$pattern$equals, "1199.187")
  # ...while the `pattern` operator under B is one.
  expect_identical(out$rules[[1]]$then$B$pattern, "^1199\\.187$")
})

test_that("dry builds stand in the study option's first suggestion, else the generic stand-in", {
  dry <- app_fn("dta_template_dry_study")
  expect_identical(dry(), list(id = "STUDY-ID", protocol_id = "PROT-ID"))
  def <- list(options = list(
    list(id = "s", type = "text", target = "metadata.study.id", choices = c("1001_0190", "1001_0191")),
    list(id = "p", type = "text", target = "metadata.study.protocol_id"),
    list(id = "t", type = "text", target = "metadata.title", choices = "Not a study id")
  ))
  expect_identical(dry(def), list(id = "1001_0190", protocol_id = "PROT-ID"))

  # A select whose effects write both fields: read the way creation reads it.
  picked <- list(options = list(list(
    id = "study", type = "select", choices = c("A-1", "B-2"),
    effects = list(
      "A-1" = list(list(path = "metadata.study.id", value = "1001_0190"), list(path = "metadata.study.protocol_id", value = "P-A")),
      "B-2" = list(list(path = "metadata.study.id", value = "1001_0191"))
    )
  )))
  expect_identical(dry(picked), list(id = "1001_0190", protocol_id = "P-A"))
  set_map <- list(options = list(list(
    id = "study", type = "select", choices = "1199187",
    effects = list("__selection__" = list(list(set = list("metadata.study.id" = "__selection__"))))
  )))
  expect_identical(dry(set_map)$id, "1199187")
})

test_that("template history builds a token template with the stand-in study identity", {
  fx <- token_fixture()
  built <- app_fn("template_history_build")("tok_ct@1.0", fx$index)
  expect_true(built$ok, info = built$error)
  stand_in <- app_fn("dta_template_dry_study")()
  cols <- token_dataset(built$value$dta)@specs@columns
  expect_identical(unlist(cols[["STUDYID"]]@values), stand_in$id)
  expect_identical(unlist(cols[["PROTID"]]@values), stand_in$protocol_id)
})
