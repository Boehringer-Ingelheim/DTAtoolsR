test_that("OQ-META-019 | study identity is structured and independent of presentation | REQ-META-019", {
  md <- DTAMetaData(header = "Presentation", study = list(id = "DEMO-001", protocol_id = "P-01"))
  qa_step("study identity is retained", list(id = "DEMO-001", protocol_id = "P-01"), md@study)
  qa_step("header is not rewritten", "Presentation", md@header)
  invalid <- list(
    list(id = 1), list(id = NA_character_), list(id = ""),
    list(id = " "), list(id = c("a", "b")), list("unnamed"),
    list(unknown = "value"), stats::setNames(list("a", "b"), c("id", "id"))
  )
  for (i in seq_along(invalid)) {
    outcome <- tryCatch(DTAMetaData(study = invalid[[i]]), error = function(e) e)
    qa_check(paste("invalid study shape rejected", i), inherits(outcome, "error"))
  }
})

test_that("OQ-META-020 | agreement sections validate structure and retain order and paragraphs | REQ-META-020", {
  sections <- list(
    scope = list(title = "Scope", text = "First paragraph.\n\nSecond paragraph."),
    responsibilities = list(title = "Responsibilities", text = "")
  )
  md <- DTAMetaData(sections = sections)
  qa_step("section order is declared order", c("scope", "responsibilities"), names(md@sections))
  qa_step("paragraph breaks remain literal", "First paragraph.\n\nSecond paragraph.", md@sections$scope$text)
  qa_step("unfinished draft text is allowed", "", md@sections$responsibilities$text)
  invalid <- list(
    list(list(title = "Missing key", text = "")),
    stats::setNames(list(sections$scope, sections$scope), c("scope", "scope")),
    list("bad.key" = sections$scope),
    list(scope = list(title = "", text = "")),
    list(scope = list(title = "Title")),
    list(scope = list(title = "Title", text = NA_character_)),
    list(scope = list(title = "Title", text = c("a", "b"))),
    list(scope = list(title = "Title", text = "", unknown = "value"))
  )
  for (i in seq_along(invalid)) {
    outcome <- tryCatch(DTAMetaData(sections = invalid[[i]]), error = function(e) e)
    qa_check(paste("invalid section shape rejected", i), inherits(outcome, "error"))
  }
})

test_that("OQ-META-021 | extended transmission clauses are text and preserve extensions | REQ-META-021", {
  fields <- c("extent", "scope", "packaging", "security", "checksum", "test_requirements", "blinding_requirements")
  transmission <- stats::setNames(rep(list("Agreed procedure"), length(fields)), fields)
  transmission$custom <- list(owner = "Sender")
  md <- DTAMetaData(transmission = transmission)
  qa_step("all agreed text is retained", transmission, md@transmission)
  qa_step("empty draft text is accepted", "", DTAMetaData(transmission = list(scope = ""))@transmission$scope)
  for (field in fields) {
    for (value in list(TRUE, 1, NA_character_, c("a", "b"))) {
      outcome <- tryCatch(
        DTAMetaData(transmission = stats::setNames(list(value), field)),
        error = function(e) e
      )
      qa_check(paste("invalid transmission text rejected", field, typeof(value), length(value)), inherits(outcome, "error"))
    }
  }
})

test_that("OQ-META-022 | new metadata defaults and YAML round trips preserve legacy contracts | REQ-META-022", {
  legacy <- DTAMetaData(
    "Legacy title", "1.0", NULL, "Header", list(), list(), list(), list(),
    "Correction procedure", "Reviewer", list(id = "legacy", version = "1.0")
  )
  qa_step("existing positional provenance retains its meaning", list(id = "legacy", version = "1.0"), legacy@template)
  qa_step("empty study defaults to a list", list(), legacy@study)
  qa_step("empty sections defaults to a list", list(), legacy@sections)
  qa_step("new unset properties are omitted", character(), intersect(c("study", "sections"), names(as.list(legacy))))
  legacy@study <- list(id = "DEMO-002")
  legacy@sections <- list(
    second = list(title = "Declared first", text = "A\n\nB"),
    first = list(title = "Declared second", text = "C")
  )
  path <- file.path(qa_tempdir(), "agreement.yaml")
  yaml::write_yaml(list(metadata = as.list(legacy)), path)
  restored <- read_dta_from_yaml(path)@metadata
  qa_step("study round trips", list(id = "DEMO-002"), restored@study)
  qa_step("declared order is not sorted alphabetically", c("second", "first"), names(restored@sections))
  qa_step("paragraphs round trip", "A\n\nB", restored@sections$second$text)
})
