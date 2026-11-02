test_that("OQ-TMPL-067 | structured metadata inherits and carries over without replacing new clauses by default | REQ-TMPL-032 | tags: white-box", {
  dir <- qa_tempdir()
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
      name = "records", type = "tabular",
      columns = list(list(id = "SUBJECT", type = "SAS Char"))
    ))
  )
  yaml::write_yaml(parent, file.path(dir, "base.dta-template.yaml"))
  yaml::write_yaml(child, file.path(dir, "child.dta-template.yaml"))
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
