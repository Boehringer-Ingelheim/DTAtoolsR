test_that("OQ-TMPL-067 | structured metadata inherits and carries over without replacing new clauses by default | REQ-TMPL-032 | tags: white-box", {
  dir <- qa_tempdir()
  dir.create(file.path(dir, "templates"))
  parent <- list(
    kind = "dta_creation_template", id = "agreement_base", version = "1.0",
    abstract = TRUE,
    base = list(metadata = list(
      title = "Agreement",
      study = list(id = "T-001"),
      sections = list(
        scope = list(title = "Scope", text = "Parent scope"),
        review = list(title = "Review", text = "Review delivered files.")
      )
    ))
  )
  child <- list(
    kind = "dta_creation_template", id = "agreement_child", version = "1.0",
    extends = "agreement_base@1.0",
    base = list(metadata = list(
      sections = list(scope = list(text = "Child scope"))
    )),
    options = list(list(
      id = "protocol", type = "text",
      target = "metadata.study.protocol_id", default = "P-001"
    )),
    datasets = list(list(
      id = "records", type = "tabular",
      files = list(filename = "records.tsv", type = "tsv"),
      columns = list(list(id = "SUBJECT", type = "SAS Char"))
    ))
  )
  yaml::write_yaml(parent, file.path(dir, "templates", "base.dta-template.yaml"))
  yaml::write_yaml(child, file.path(dir, "templates", "child.dta-template.yaml"))
  qa_step("structured metadata templates pass strict validation", 0L, nrow(validate_template(dir, strict = TRUE)))
  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("qualification=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  ))
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  index <- engine$build_template_index()
  loaded <- engine$load_template_definition("agreement_child@1.0", index)
  qa_check("child resolves", isTRUE(loaded$ok))
  built <- engine$create_dta_from_template(
    loaded$value$def, loaded$value$path,
    index = index,
    selections = list(protocol = "P-002")
  )
  qa_check("child builds", isTRUE(built$ok))
  qa_step("study option targets a structured field", list(id = "T-001", protocol_id = "P-002"), built$value@metadata@study)
  qa_step("clause order survives inheritance", c("scope", "review"), names(built$value@metadata@sections))
  qa_step("one clause is overridden", "Child scope", built$value@metadata@sections$scope$text)
  qa_step("unrelated clause survives", "Review delivered files.", built$value@metadata@sections$review$text)
  leaves <- engine$dta_metadata_leaves(built$value@metadata)
  qa_step(
    "rebase treats the ordered section collection as one atomic value",
    list(sections = built$value@metadata@sections),
    leaves[startsWith(names(leaves), "sections")]
  )

  ancestor <- DTAMetaData(
    study = list(id = "OLD-STUDY"),
    sections = list(scope = list(title = "Old scope", text = "Old wording")),
    template = list(id = "old", version = "1.0")
  )
  defaults <- engine$carry_over_default_fields()
  qa_check("study is default-on and clauses are opt-in", "study" %in% defaults && !"sections" %in% defaults)
  carried <- engine$apply_metadata_carry_over(built$value, ancestor, defaults)
  qa_step("study carries over", list(id = "OLD-STUDY"), carried@metadata@study)
  qa_step("new clause remains by default", "Child scope", carried@metadata@sections$scope$text)
  explicit <- engine$apply_metadata_carry_over(built$value, ancestor, c("sections", "template"))
  qa_step("explicit clause carry-over works", "Old wording", explicit@metadata@sections$scope$text)
  qa_check("old machine provenance never carries over", !identical(explicit@metadata@template$id, "old"))
})

test_that("OQ-TMPL-136 | study tokens in a dataset body take the document's study identity, escaped in a regular expression, and creation fails when none is known | REQ-TMPL-058 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  inline <- list(
    id = "tok", type = "tabular",
    files = list(filename = "^s${study.id}_[0-9]{12}\\.csv$", pattern = TRUE, type = "csv"),
    columns = list(
      list(id = "STUDYID", type = "SAS Char", values = "${study.id}"),
      list(id = "PROTID", type = "SAS Char", values = "${study.protocol_id}"),
      list(id = "SUBJID", type = "SAS Char", pattern = "^${study.id}-[0-9]{4}$")
    )
  )
  def_with <- function(study = NULL) {
    md <- list(title = "T", version = "1.0")
    if (!is.null(study)) md$study <- study
    list(base = list(metadata = md), datasets = list(inline))
  }
  path <- file.path(qa_tempdir(), "t.yaml")

  built <- engine$create_dta_from_template(def_with(list(id = "1199.187(a)", protocol_id = "BI 1-2")), path)
  qa_check("creation with a study identity succeeds", isTRUE(built$ok))
  ds <- built$value@datasets[["tok"]]
  file <- ds@files[[1]]
  qa_step("the id is escaped in the file name pattern", "^s1199\\.187\\(a\\)_[0-9]{12}\\.csv$", file@filename)
  qa_step("the pattern accepts the literal id", TRUE, any(matches_filename(file, "s1199.187(a)_202611181200.csv")))
  qa_step("the pattern refuses a lookalike", FALSE, any(matches_filename(file, "s1199x187(a)_202611181200.csv")))
  cols <- ds@specs@columns
  qa_step("allowed values carry the id as entered", "1199.187(a)", unlist(cols[["STUDYID"]]@values))
  qa_step("allowed values carry the protocol id", "BI 1-2", unlist(cols[["PROTID"]]@values))
  qa_step("the id is escaped in a column pattern", "^1199\\.187\\(a\\)-[0-9]{4}$", cols[["SUBJID"]]@pattern)

  unanswered <- engine$create_dta_from_template(def_with(NULL), path)
  qa_step("creation without any study identity fails", FALSE, unanswered$ok)
  qa_check(
    "the error names the dataset and the placeholder",
    grepl("tok", unanswered$error, fixed = TRUE) && grepl("${study.id}", unanswered$error, fixed = TRUE)
  )
  # Blank through the study id QUESTION: base.metadata.study rejects a blank id
  # on its own, so only an option answer reaches the placeholder check.
  def_asked <- def_with(list(protocol_id = "P"))
  def_asked$options <- list(list(id = "study_id", type = "text", target = "metadata.study.id"))
  blank <- engine$create_dta_from_template(def_asked, path, selections = list(study_id = "   "))
  qa_step("a blank answer to the study id question counts as not supplied", FALSE, blank$ok)
  qa_check("and is reported as the missing placeholder", grepl("${study.id}", blank$error, fixed = TRUE))
  answered <- engine$create_dta_from_template(def_asked, path, selections = list(study_id = "OPT-1"))
  qa_step(
    "an answer to the study id question reaches the dataset", "OPT-1",
    unlist(answered$value@datasets[["tok"]]@specs@columns[["STUDYID"]]@values)
  )

  titled <- def_with(list(id = "1199.187(a)", protocol_id = "P"))
  titled$base$metadata$title <- "Transfer for ${study.id}"
  titled_built <- engine$create_dta_from_template(titled, path)
  qa_step(
    "the placeholder is filled in the document metadata too, as entered",
    "Transfer for 1199.187(a)", titled_built$value@metadata@title
  )

  fallback <- engine$create_dta_from_template(
    def_with(NULL), path,
    study_fallback = list(id = "FB-1", protocol_id = "FB-P")
  )
  qa_check("a supplied fallback answers the placeholders", isTRUE(fallback$ok))
  qa_step(
    "and is not written to the document's metadata", NULL,
    fallback$value@metadata@study$id
  )
  qa_step(
    "the dataset carries the fallback value", "FB-1",
    unlist(fallback$value@datasets[["tok"]]@specs@columns[["STUDYID"]]@values)
  )
})

test_that("OQ-TMPL-151 | the _underscore study tokens give the study identity with every hyphen written as an underscore, escaped in a regular expression, and creation fails when none is known | REQ-TMPL-058 | tags: white-box", {
  engine <- getFromNamespace(".dta_template_engine", "DTAtools")()
  inline <- list(
    id = "tok", type = "tabular",
    files = list(filename = "^s${study.id_underscore}_[0-9]{12}\\.csv$", pattern = TRUE, type = "csv"),
    columns = list(
      list(id = "STUDYID", type = "SAS Char", values = "${study.id}"),
      list(id = "PROTID", type = "SAS Char", values = "${study.protocol_id_underscore}")
    )
  )
  def_with <- function(study = NULL) {
    md <- list(title = "Transfer for ${study.id_underscore}", version = "1.0")
    if (!is.null(study)) md$study <- study
    list(base = list(metadata = md), datasets = list(inline))
  }
  path <- file.path(qa_tempdir(), "t.yaml")

  # Expected values are derived from the requirement: the id 1199.187-a
  # becomes 1199.187_a, the protocol id BI 1-2 becomes BI 1_2 (only the hyphen
  # changes, not the blank), and in a pattern the "." is escaped.
  built <- engine$create_dta_from_template(def_with(list(id = "1199.187-a", protocol_id = "BI 1-2")), path)
  qa_check("creation with a study identity succeeds", isTRUE(built$ok))
  ds <- built$value@datasets[["tok"]]
  file <- ds@files[[1]]
  qa_step(
    "the underscored id is escaped in the file name pattern",
    "^s1199\\.187_a_[0-9]{12}\\.csv$", file@filename
  )
  qa_step("the pattern accepts the underscored id", TRUE, any(matches_filename(file, "s1199.187_a_202611181200.csv")))
  qa_step("the pattern refuses the hyphenated id", FALSE, any(matches_filename(file, "s1199.187-a_202611181200.csv")))
  qa_step("the pattern refuses a lookalike", FALSE, any(matches_filename(file, "s1199x187_a_202611181200.csv")))
  cols <- ds@specs@columns
  qa_step("the plain token beside it keeps the id as entered", "1199.187-a", unlist(cols[["STUDYID"]]@values))
  qa_step("the protocol id has its hyphen written as an underscore", "BI 1_2", unlist(cols[["PROTID"]]@values))
  qa_step(
    "the placeholder is filled in the document metadata too",
    "Transfer for 1199.187_a", built$value@metadata@title
  )
  qa_step("and the study identity itself stays as entered", "1199.187-a", built$value@metadata@study$id)

  unanswered <- engine$create_dta_from_template(def_with(NULL), path)
  qa_step("creation without any study identity fails", FALSE, unanswered$ok)
  qa_check(
    "the error names the dataset and the placeholder",
    grepl("tok", unanswered$error, fixed = TRUE) && grepl("${study.id_underscore}", unanswered$error, fixed = TRUE)
  )
  # The title holds the placeholder too; with the dataset body not using one, the
  # error names the metadata field.
  no_datasets <- def_with(NULL)
  no_datasets$datasets <- list(list(id = "mini", type = "file", files = list(filename = "mini.csv", type = "csv")))
  unanswered_title <- engine$create_dta_from_template(no_datasets, path)
  qa_step("creation fails for a placeholder in the metadata alone", FALSE, unanswered_title$ok)
  qa_check(
    "and names the metadata field and the placeholder",
    grepl("title", unanswered_title$error, fixed = TRUE) &&
      grepl("${study.id_underscore}", unanswered_title$error, fixed = TRUE)
  )

  fallback <- engine$create_dta_from_template(
    def_with(NULL), path,
    study_fallback = list(id = "FB-1", protocol_id = "FB-P")
  )
  qa_check("a supplied fallback answers the placeholders", isTRUE(fallback$ok))
  qa_step(
    "the dataset carries the underscored fallback", "FB_P",
    unlist(fallback$value@datasets[["tok"]]@specs@columns[["PROTID"]]@values)
  )
})
