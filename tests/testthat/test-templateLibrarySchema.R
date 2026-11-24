# Coverage for the JSON Schemas create_template_repo() ships in every scaffold
# (inst/extdata/schemas/): dta-template-library.schema.json for the four
# template file kinds and dta-app.schema.json for dta-app.yaml.
#
# The schemas are hand-written from the readers in inst/shiny/dta_app/R, so
# every expectation below comes from the grammar those readers accept, never
# from what a schema happened to print. A rejection is asserted by WHERE and
# WHY (instance path and ajv keyword), not by message text.
#
# What the file does not test: that an editor loads the schemas. That needs the
# Red Hat YAML extension and is checked by hand.

# ---- Harness ----------------------------------------------------------------

# jsonvalidate is a Suggests; see the CI guard below for why a missing one must
# not pass silently where CI runs this file.
skip_without_jsonvalidate <- function() {
  testthat::skip_if_not_installed("jsonvalidate")
}

schema_file <- function(name) {
  file <- system.file("extdata", "schemas", name, package = "DTAtools")
  stopifnot(nzchar(file), file.exists(file))
  file
}

# `engine = "ajv"` in EVERY call. The default engine reads draft-04 and ignores
# `if`/`then`, which would wave through the kind dispatch and every "required
# unless" rule without checking either.
schema_validator <- function(name) {
  jsonvalidate::json_validator(schema_file(name), engine = "ajv")
}

# YAML text to the JSON an editor would see. `seq = identity` keeps a
# one-element sequence an array: yaml::yaml.load() otherwise collapses
# `mixins: [x]` to the scalar "x", and the test would check something other
# than what the editor checks.
yaml_to_json <- function(text) {
  value <- yaml::yaml.load(text, handlers = list(seq = function(x) x))
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA))
}

# NULL when the schema accepts `text`; otherwise one row per ajv error with the
# instance path, the keyword that failed and the property it names (missing or
# unexpected), if any.
schema_problems <- function(validator, text) {
  ok <- validator(yaml_to_json(text), verbose = TRUE, greedy = TRUE)
  if (isTRUE(ok)) {
    return(NULL)
  }
  errors <- attr(ok, "errors")
  param <- function(column) {
    if (column %in% names(errors$params)) errors$params[[column]] else rep(NA_character_, nrow(errors))
  }
  data.frame(
    path = errors$instancePath,
    keyword = errors$keyword,
    what = ifelse(is.na(param("missingProperty")), param("additionalProperty"), param("missingProperty")),
    stringsAsFactors = FALSE
  )
}

expect_accepted <- function(validator, text, info = "") {
  problems <- schema_problems(validator, text)
  expect_null(problems, info = paste(info, paste(utils::capture.output(print(problems)), collapse = "\n")))
}

# The schema rejects `text`, and one of its errors is `keyword` at `path`
# (naming `what`, when given).
expect_flagged <- function(validator, text, path, keyword, what = NULL, info = "") {
  problems <- schema_problems(validator, text)
  expect_false(is.null(problems), info = paste(info, "- the schema accepted it"))
  if (is.null(problems)) {
    return(invisible())
  }
  hit <- problems$path == path & problems$keyword == keyword
  if (!is.null(what)) hit <- hit & problems$what %in% what
  expect_true(
    any(hit),
    info = paste0(
      info, " - expected ", keyword, " at '", path, "'", if (!is.null(what)) paste0(" naming ", what),
      "; got:\n", paste(utils::capture.output(print(problems)), collapse = "\n")
    )
  )
}

# A minimal creation template, with the parts a case varies spelled out.
creation <- function(base = "base: {}\n", datasets = "datasets: [a.yaml]\n", extra = "") {
  paste0("kind: dta_creation_template\nid: t\nversion: '1.0'\n", base, datasets, extra)
}
party <- function(extra = "") paste0("kind: dta_party_profile\nid: p\nversion: '1.0'\n", extra)
vocabulary <- function(extra = "terms: [a, b]\n") paste0("kind: dta_vocabulary\nid: v\nversion: '1.0'\n", extra)
dataset_template <- function(extra = "dataset:\n  id: d\n") {
  paste0("kind: dta_dataset_template\nid: d\nversion: '1.0'\n", extra)
}

# ---- The schemas themselves -------------------------------------------------

test_that("both schemas are valid JSON and name the draft they are written in", {
  for (name in c("dta-template-library.schema.json", "dta-app.schema.json")) {
    text <- paste(readLines(schema_file(name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_true(jsonlite::validate(text), info = name)
    expect_identical(
      jsonlite::fromJSON(text, simplifyVector = FALSE)[["$schema"]],
      "http://json-schema.org/draft-07/schema#",
      info = name
    )
  }
})

test_that("jsonvalidate is installed wherever CI runs these tests", {
  # COMPANION GUARD to everything below, which skips without jsonvalidate. A
  # permanently skipped schema suite is invisible dead coverage, so on CI its
  # absence must fail. This test itself never skips, and it can only fail on CI.
  expect_true(
    requireNamespace("jsonvalidate", quietly = TRUE) || !identical(Sys.getenv("CI"), "true"),
    info = "CI must install jsonvalidate (a Suggests, which setup-r-dependencies installs); without it the schema tests skip."
  )
})

test_that("both schemas are valid draft-07 schemas and the engine really applies if/then", {
  skip_without_jsonvalidate()
  # Building a validator only proves a schema valid if the engine checks it
  # against the metaschema first. ajv does, which this pins: an invalid schema
  # must fail to build, so a schema that builds below is a valid one.
  # Judged by the failure itself, not by ajv's wording of it.
  built <- tryCatch(
    jsonvalidate::json_validator('{"type": "nonsense"}', engine = "ajv"),
    error = identity
  )
  expect_s3_class(built, "error")
  for (name in c("dta-template-library.schema.json", "dta-app.schema.json")) {
    expect_no_error(jsonvalidate::json_validator(schema_file(name), engine = "ajv"))
  }

  # A template with neither `base`, `extends` nor `mixins` is rejected by a
  # conditional alone: if the engine ignored `if`/`then`, this would pass.
  v <- schema_validator("dta-template-library.schema.json")
  expect_flagged(
    v, creation(base = ""),
    path = "", keyword = "required", what = "base",
    info = "conditional"
  )
})

# ---- Real files are never flagged ---------------------------------------------

test_that("every YAML file of the packaged library and the skeleton validates", {
  skip_without_jsonvalidate()
  lib <- schema_validator("dta-template-library.schema.json")
  app <- schema_validator("dta-app.schema.json")

  roots <- c(
    system.file("extdata", "library", package = "DTAtools"),
    system.file("extdata", "template-repo-skeleton", package = "DTAtools")
  )
  expect_true(all(nzchar(roots)))
  files <- unlist(lapply(roots, list.files, pattern = "[.]ya?ml$", recursive = TRUE, full.names = TRUE))
  # A directory that quietly lost its files would make the loop below vacuous.
  expect_gte(length(files), 10)

  for (file in files) {
    text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    validator <- if (basename(file) == "dta-app.yaml") app else lib
    expect_accepted(validator, text, info = basename(file))
  }
})

test_that("a freshly created repository validates file by file", {
  skip_without_jsonvalidate()
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE, agent_skill = FALSE)
  lib <- schema_validator("dta-template-library.schema.json")
  app <- schema_validator("dta-app.schema.json")

  files <- list.files(dir, pattern = "[.]ya?ml$", recursive = TRUE, full.names = TRUE)
  expect_gte(length(files), 5)
  for (file in files) {
    text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_accepted(if (basename(file) == "dta-app.yaml") app else lib, text, info = basename(file))
  }
})

# ---- What the engine accepts, the schema accepts ------------------------------

test_that("the section states, list shapes and inheritance verbs the engine reads are accepted", {
  skip_without_jsonvalidate()
  v <- schema_validator("dta-template-library.schema.json")

  # Absent, null, {} and [] are four different instructions to the inheritance
  # merge, so none may be flagged.
  for (section in c("options", "datasets", "party_slots", "vocabulary_slots")) {
    for (value in c("~", "{}", "[]")) {
      # `creation()` writes a `datasets:` of its own, which would be a second
      # key when the section under test is `datasets`.
      expect_accepted(
        v,
        creation(
          datasets = if (section == "datasets") "" else "datasets: [a.yaml]\n",
          extra = paste0(section, ": ", value, "\n")
        ),
        info = paste(section, value)
      )
    }
  }
  expect_accepted(v, creation(base = "base: ~\n"), info = "base: null")

  # One-element lists stay lists, and `mixins:` may be a bare string.
  expect_accepted(v, creation(base = "", datasets = "", extra = "extends: p@latest\nmixins: [m]\nsealed: [metadata.title]\n"))
  expect_accepted(v, creation(base = "", datasets = "", extra = "mixins: m\n"))

  # Every verb, on every collection, with a list and with a single entry.
  expect_accepted(v, creation(
    base = "", datasets = "datasets:\n  inherit: none\n  remove: [a]\n  add:\n    template: ds@1.0\n    as: x\n  modify:\n    - as: x\n  order: [x]\n",
    extra = "extends: p\noptions:\n  inherit: [o1]\n  add:\n    - id: o2\n  modify:\n    - id: o1\n      label: L\n  order: [o2, o1]\n"
  ))

  # Every dataset-entry shape.
  expect_accepted(v, creation(datasets = "datasets:\n  - file.yaml\n  - source: file.yaml\n  - template: ds@latest\n    options:\n      x: 1\n    patch:\n      set:\n        dataset.id: z\n  - id: inline\n    type: tabular\n    columns:\n      - id: A\n"))

  # An unquoted date is a Date to R and a plain scalar to an editor.
  expect_accepted(v, creation(extra = "date: 2024-03-01\n"))
  expect_accepted(v, creation(extra = "date: '2024-03-01'\n"))

  # R reads yes/no as booleans, an editor as strings; both must pass, also
  # where the flag decides whether `datasets:` may be left out.
  expect_accepted(v, creation(extra = "abstract: true\n"))
  expect_accepted(v, creation(extra = "abstract: 'yes'\n"))
  expect_accepted(v, creation(datasets = "", extra = "abstract: 'yes'\n"))

  # R reads `x` and `[x]` alike, so a list of one may be written as the value.
  expect_accepted(v, creation(extra = "sealed: metadata.title\nrequired: metadata.title\norder: o1\n"))
  expect_accepted(v, creation(
    base = "", datasets = "", extra = "extends: p\noptions:\n  inherit: o1\n  remove: o2\n  order: o1\n"
  ))
  expect_accepted(v, creation(extra = "party_slots:\n  - id: s\n    target: metadata.supplier\n    profiles: starter_supplier\n"))
  expect_accepted(v, vocabulary("extends: other\nremove_terms: a\n"))

  # `add:` and `modify:` hold a single entry as readily as a list (the engine
  # accepts a bare mapping for both), on every collection.
  expect_accepted(v, creation(
    base = "", datasets = "",
    extra = "extends: p\noptions:\n  modify:\n    id: o1\n    label: L\nparty_slots:\n  modify:\n    id: s\n    label: S\n"
  ))

  # A dataset entry keyed by `as`, one that drops an inherited dataset, and a
  # sourced one that is renamed: all merge forms of the inheritance.
  expect_accepted(v, creation(
    base = "", extra = "extends: p\n",
    datasets = paste0(
      "datasets:\n  - template: ds@1.0\n    remove: true\n  - source: f.yaml\n    as: z\n    remove: true\n",
      # Not `as: y`: R reads a bare y as the boolean TRUE (YAML 1.1), an editor
      # as a string, so a one-letter id would test the parser, not the schema.
      "  - as: x\n    options:\n      a: 1\n  - as: other\n    remove: true\n"
    )
  ))

  # A variant's `options:` merges like a template's, so it takes the verbs.
  expect_accepted(v, creation(
    datasets = "",
    extra = "variants:\n  id: v\n  choices:\n    - value: a\n      datasets: [x.yaml]\n      options:\n        remove: [o1]\n"
  ))

  # Null where the engine reads "absent" or the default.
  expect_accepted(v, creation(extra = "mixins: ~\nextends: ~\n"))
  expect_accepted(v, party("role: ~\n"))
  expect_accepted(v, creation(
    base = "", datasets = "", extra = "extends: p\noptions:\n  inherit: ~\n  add: ~\n  modify: ~\n"
  ))

  # A vocabulary: bare codes, mappings, a single `columns:` mapping, and a child
  # that has no terms of its own.
  expect_accepted(v, vocabulary("terms:\n  - a\n  - code: b\n    label: B\n  - 3\ncolumns:\n  id: COL\n  field: label\n"))
  expect_accepted(v, vocabulary("extends: other@1.0\nadd_terms: [c]\nremove_terms: [a]\n"))
})

test_that("the app schema accepts the documented forms", {
  skip_without_jsonvalidate()
  v <- schema_validator("dta-app.schema.json")
  expect_accepted(v, "kind: dta_app_config\n")
  expect_accepted(v, "# only a comment\n")
  expect_accepted(v, "document:\n  name: [Data Transfer Agreement, Data Transmission Specification]\n  abbreviation: [DTA, DTS]\n")
  expect_accepted(v, "colors:\n  primary: '#00625B'\n  accent: '#0a8'\n")
  expect_accepted(v, "links:\n  documentation: false\n  about: https://example.org/about\n  report_issue: mailto:me@example.org\n")
  expect_accepted(v, "features:\n  start:\n    load_example: true\n  editing: false\nexports:\n  document: [word]\n  word_templates:\n    - file: export-templates/a.docx\n      label: A\n")
  # A list of one may be written as the value, as R reads both alike.
  expect_accepted(v, "exports:\n  document: word\n  validation_messages: csv\n")
  # A null link is hidden, like false; the URL scheme is case-insensitive.
  expect_accepted(v, "links:\n  about: ~\n  repository: HTTPS://example.org\n")
  # data_types.default takes either type system in any letter case, with blanks.
  expect_accepted(v, "data_types:\n  default: R\n")
  expect_accepted(v, "data_types:\n  default: sas\n")
  expect_accepted(v, "data_types:\n  default: ' r '\n")
})

# ---- What the engine rejects, the schema flags --------------------------------

test_that("a file the readers reject is flagged where the author wrote it", {
  skip_without_jsonvalidate()
  v <- schema_validator("dta-template-library.schema.json")

  cases <- list(
    list("no kind", "id: a\nversion: '1.0'\n", "", "required", "kind"),
    list("unknown kind", "kind: dta_other\nid: a\nversion: '1.0'\n", "/kind", "enum", NULL),
    list("no id", "kind: dta_party_profile\nversion: '1.0'\n", "", "required", "id"),
    list("no version", "kind: dta_party_profile\nid: a\n", "", "required", "version"),
    list("unquoted version", sub("'1.0'", "1.0", party(), fixed = TRUE), "/version", "type", NULL),
    list("version that is not a version", sub("'1.0'", "latest", party(), fixed = TRUE), "/version", "pattern", NULL),
    list("invalid role", party("role: owner\n"), "/role", "enum", NULL),
    # `kind` selects the grammar: `base:` belongs to a creation template.
    list("a key of another kind", party("base: {}\n"), "", "additionalProperties", "base"),
    list("misspelt root key", creation(extra = "option: []\n"), "", "additionalProperties", "option"),
    list("creation without base, extends or mixins", creation(base = ""), "", "required", "base"),
    list("creation without datasets", creation(datasets = ""), "", "required", NULL),
    list("misspelt metadata field", creation(base = "base:\n  metadata:\n    titel: x\n"), "/base/metadata", "additionalProperties", "titel"),
    list("option without id", creation(extra = "options:\n  - label: x\n"), "/options/0", "required", "id"),
    list("misspelt collection verb", creation(extra = "options:\n  inhert: all\n"), "/options", "additionalProperties", "inhert"),
    list("misspelt dataset-entry key", creation(datasets = "datasets:\n  - tempate: ds@1.0\n    as: x\n"), "/datasets/0", "additionalProperties", "tempate"),
    list("inline dataset without an id", creation(datasets = "datasets:\n  - type: tabular\n"), "/datasets/0", "required", "id"),
    list("prior_versions entry without changes", creation(extra = "prior_versions:\n  - version: '0.9'\n"), "/prior_versions/0", "required", "changes"),
    list("a flag that is not a boolean", creation(extra = "abstract: maybe\n"), "/abstract", "enum", NULL),
    list("unknown key in an imported dataset", creation(datasets = "datasets:\n  - template: ds@1.0\n    as: x\n    asd: 1\n"), "/datasets/0", "additionalProperties", "asd"),
    list("party slot with a bad target", creation(extra = "party_slots:\n  - id: s\n    target: metadata.sponsor\n"), "/party_slots/0/target", "enum", NULL),
    list("variant choice without datasets", creation(datasets = "", extra = "variants:\n  id: v\n  choices:\n    - value: a\n"), "/variants/choices/0", "required", "datasets"),
    list("unknown key in prior_versions", creation(extra = "prior_versions:\n  - version: '0.9'\n    note: x\n"), "/prior_versions/0", "additionalProperties", "note"),
    list("dataset template without dataset", dataset_template(""), "", "required", "dataset"),
    list("dataset with an unknown type", dataset_template("dataset:\n  id: d\n  type: tabelar\n"), "/dataset/type", "enum", NULL),
    list("vocabulary without terms or extends", vocabulary(""), "", "required", "terms"),
    list("vocabulary with a bad type", vocabulary("type: date\nterms: [a]\n"), "/type", "enum", NULL),
    list("vocabulary term without code", vocabulary("terms:\n  - label: x\n"), "/terms/0", "required", "code"),
    list("vocabulary column with an extra key", vocabulary("terms: [a]\ncolumns:\n  - id: A\n    feild: label\n"), "/columns/0", "additionalProperties", "feild"),
    list("vocabulary column with a bad field", vocabulary("terms: [a]\ncolumns:\n  - id: A\n    field: labl\n"), "/columns/0/field", "enum", NULL)
  )

  for (case in cases) {
    expect_flagged(v, case[[2]], path = case[[3]], keyword = case[[4]], what = case[[5]], info = case[[1]])
  }
})

test_that("a creation template inherits or states its base and datasets, and only an abstract one may omit datasets", {
  skip_without_jsonvalidate()
  v <- schema_validator("dta-template-library.schema.json")

  # Each of these lifts one conditional; they pair with the rejections above.
  expect_accepted(v, creation(base = "", datasets = "", extra = "extends: parent\n"), "extends")
  expect_accepted(v, creation(base = "", datasets = "", extra = "mixins: [m]\n"), "mixins")
  expect_accepted(v, creation(datasets = "", extra = "abstract: true\n"), "abstract")
  expect_accepted(v, creation(datasets = "", extra = "variants:\n  id: v\n  choices:\n    - value: a\n      datasets: [x.yaml]\n"), "variants")
  expect_accepted(v, creation(base = "base:\n  datasets: [x.yaml]\n", datasets = ""), "legacy base.datasets")
  expect_accepted(v, vocabulary("extends: other\n"), "vocabulary extends")
})

test_that("the app schema flags what parse_dta_app_config() rejects", {
  skip_without_jsonvalidate()
  v <- schema_validator("dta-app.schema.json")

  cases <- list(
    list("wrong kind", "kind: dta_creation_template\n", "/kind", "const", NULL),
    list("misspelt section", "brandin:\n  app_title: x\n", "", "additionalProperties", "brandin"),
    list("misspelt key in a section", "branding:\n  app_titel: x\n", "/branding", "additionalProperties", "app_titel"),
    # An unquoted #hex is a YAML comment, so the value arrives as null.
    list("unquoted colour", "colors:\n  primary: #00625B\n", "/colors/primary", "type", NULL),
    list("colour that is not hex", "colors:\n  primary: red\n", "/colors/primary", "pattern", NULL),
    list("abbreviation over 12 characters", "document:\n  abbreviation: ABCDEFGHIJKLM\n", "/document/abbreviation", "maxLength", NULL),
    list("link that is not http or mailto", "links:\n  about: ftp://example.org\n", "/links/about", "anyOf", NULL),
    list("unknown export format", "exports:\n  document: [pdf]\n", "/exports/document/0", "enum", NULL),
    list("word template that is not a docx", "exports:\n  word_templates:\n    - file: a.txt\n", "/exports/word_templates/0/file", "pattern", NULL),
    list("word template without a file", "exports:\n  word_templates:\n    - label: A\n", "/exports/word_templates/0", "required", "file"),
    list("a feature flag that is not a boolean", "features:\n  editing: maybe\n", "/features/editing", "enum", NULL),
    list("a type system that does not exist", "data_types:\n  default: Python\n", "/data_types/default", "pattern", NULL),
    # An empty `default:` arrives as null, which the app reports as invalid.
    list("an empty default type system", "data_types:\n  default:\n", "/data_types/default", "type", NULL),
    list("misspelt key under data_types", "data_types:\n  defualt: R\n", "/data_types", "additionalProperties", "defualt")
  )

  for (case in cases) {
    expect_flagged(v, case[[2]], path = case[[3]], keyword = case[[4]], what = case[[5]], info = case[[1]])
  }
})

# ---- Schema and validator --------------------------------------------------------

test_that("a defect validate_template() reports as an error is also flagged by the schema", {
  skip_without_jsonvalidate()
  lib <- schema_validator("dta-template-library.schema.json")

  # Each mutation is a defect the readers reject, applied to one file of a
  # scaffold that otherwise passes. Both the validator and the schema must see it.
  mutations <- list(
    list(
      "unquoted version", "parties/starter_supplier.dta-party.yaml",
      function(x) sub("^version: .*$", "version: 1.0", x)
    ),
    list(
      "invalid role", "parties/starter_supplier.dta-party.yaml",
      function(x) sub("^role: .*$", "role: owner", x)
    ),
    list(
      "invalid vocabulary type", "vocabulary/starter_terms.dta-vocabulary.yaml",
      function(x) sub("^type: .*$", "type: date", x)
    )
  )

  for (m in mutations) {
    dir <- withr::local_tempdir()
    create_template_repo(dir, ci = FALSE, agent_skill = FALSE)
    file <- file.path(dir, m[[2]])
    before <- readLines(file, warn = FALSE, encoding = "UTF-8")
    after <- m[[3]](before)
    expect_false(identical(before, after), info = paste(m[[1]], "- the mutation changed nothing"))
    writeLines(after, file)

    # An invalid role also raises an R warning, because the party reader skips
    # the profile; that warning is the validator reporting it, not a defect here.
    issues <- suppressWarnings(validate_template(dir))
    errors <- issues[issues$severity == "error", , drop = FALSE]
    expect_gt(nrow(errors), 0, label = paste(m[[1]], "- validator errors"))
    expect_false(
      is.null(schema_problems(lib, paste(after, collapse = "\n"))),
      info = paste(m[[1]], "- the schema accepted what the validator rejects")
    )
  }

  # An unknown `kind:` is only a WARNING in the validator (`kind_unknown`), but
  # the schema flags it: with no known kind it cannot say what the file should
  # look like, and the author needs to see that while typing.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE, agent_skill = FALSE)
  file <- file.path(dir, "templates", "starter.dta-template.yaml")
  lines <- sub("^kind: .*$", "kind: dta_other", readLines(file, warn = FALSE, encoding = "UTF-8"))
  writeLines(lines, file)
  issues <- suppressWarnings(validate_template(dir))
  expect_true("kind_unknown" %in% issues$code)
  expect_identical(unique(issues$severity[issues$code == "kind_unknown"]), "warning")
  expect_flagged(
    lib, paste(lines, collapse = "\n"),
    path = "/kind", keyword = "enum"
  )
})

test_that("KNOWN DIFFERENCE, pinned rather than endorsed: the schema closes the root, the engine does not", {
  skip_without_jsonvalidate()
  # The engine reads template keys by name and never rejects an unknown one, so
  # a misspelt root key is silent there. The schema flags it on purpose: that is
  # the typo an author most needs to see. If the validator ever starts to flag
  # an unknown root key, delete the last expectation and the README's "Stricter"
  # bullet that says so.
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE, agent_skill = FALSE)
  file <- file.path(dir, "templates", "starter.dta-template.yaml")
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  writeLines(c(lines, "option: []"), file)

  expect_flagged(
    schema_validator("dta-template-library.schema.json"), paste(c(lines, "option: []"), collapse = "\n"),
    path = "", keyword = "additionalProperties", what = "option"
  )
  issues <- validate_template(dir)
  expect_equal(sum(issues$severity == "error"), 0)
})
