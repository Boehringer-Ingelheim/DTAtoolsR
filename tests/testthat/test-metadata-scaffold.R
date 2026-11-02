test_that("the starter scaffold demonstrates structured agreement metadata", {
  path <- withr::local_tempdir()
  create_template_repo(path, ci = FALSE)
  definition <- yaml::read_yaml(file.path(
    path, "templates", "starter.dta-template.yaml"
  ))
  expect_identical(definition$base$metadata$study$id, "DEMO-001")
  expect_identical(names(definition$base$metadata$sections), "responsibilities")
  expect_identical(
    definition$base$metadata$sections$responsibilities$title, "Responsibilities"
  )
  targets <- vapply(definition$options, function(x) x$target, character(1))
  expect_true(all(c("metadata.study.id", "metadata.study.protocol_id") %in% targets))

  engine <- .dta_template_engine()
  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("starter=dir:", path),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  ))
  index <- engine$build_template_index()
  loaded <- engine$load_template_definition("starter@1.0", index = index)
  expect_true(loaded$ok, info = loaded$error)
  built <- engine$create_dta_from_template(
    loaded$value$def, loaded$value$path,
    index = index,
    selections = list(study_id = "OTHER-002", protocol_id = "P-02")
  )
  expect_true(built$ok, info = built$error)
  expect_identical(built$value@metadata@study, list(id = "OTHER-002", protocol_id = "P-02"))
  expect_identical(names(built$value@metadata@sections), "responsibilities")
  expect_identical(nrow(validate_template(path)), 0L)
})
