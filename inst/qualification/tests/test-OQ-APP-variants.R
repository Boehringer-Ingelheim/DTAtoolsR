test_that("OQ-APP-032 | structural choice selects one schema and records its source | REQ-APP-025", {
  qapp_skip_unless_app_installed()
  entry <- function(id, type) {
    list(
      name = "subjects", type = "tabular",
      files = list(filename = "subjects.csv", type = "csv"),
      columns = list(list(id = id, type = type))
    )
  }
  def <- list(
    id = "qualification_platform", version = "1.0",
    base = list(metadata = list(title = "Qualification platform", version = "1.0")),
    options = list(list(
      id = "medium", target = "metadata.transmission.method", choices = list("EDT")
    )),
    variants = list(
      id = "trial_platform", label = "Trial platform",
      choices = list(
        list(
          value = "brave", datasets = list(entry("SUBJIDN", "SAS Num")),
          source = list(document = "Qualification source", version = "5.0")
        ),
        list(
          value = "veeva", datasets = list(entry("SUBJECT", "SAS Char")),
          source = list(document = "Qualification source", version = "5.0"),
          options = list(list(id = "medium", choices = list("EDT", "CDB")))
        )
      )
    )
  )
  create <- qapp_fn("create_dta_from_template")
  resolve <- qapp_fn("resolve_template_variant")
  for (platform in c("brave", "veeva")) {
    selection <- list(trial_platform = platform, medium = "Custom delivery")
    built <- create(def, tempfile(), selection, provenance = list(id = def$id, version = "1.0"))
    qa_step(paste("creation succeeds for", platform), TRUE, built$ok)
    testthat::expect_true(built$ok, info = built$error)
    if (!isTRUE(built$ok)) next
    column <- if (platform == "brave") "SUBJIDN" else "SUBJECT"
    type <- if (platform == "brave") "SAS Num" else "SAS Char"
    qa_step("exactly the selected identifier is built", column, names(built$value@datasets[[1]]@specs@columns))
    qa_step("identifier storage type is preserved", type, as.list(built$value@datasets[[1]]@specs@columns[[1]])$type)
    qa_step("custom agreement text remains editable", "Custom delivery", built$value@metadata@transmission$method)
    qa_step(
      "only the applicable suggestions are offered",
      if (platform == "brave") list("EDT") else list("EDT", "CDB"),
      resolve(def, selection)$options[[1]]$choices
    )
    expected <- list(
      id = "trial_platform", value = platform,
      source = list(document = "Qualification source", version = "5.0")
    )
    qa_step("provenance records the selected source", expected, built$value@metadata@template$variant)
    serialized <- qapp_fn("dta_to_yaml_text")(built$value)
    testthat::expect_true(serialized$ok, info = serialized$error)
    restored <- qapp_fn("dta_read_yaml_text")(serialized$value)
    testthat::expect_true(restored$ok, info = restored$error)
    qa_step("YAML preserves the source and choice", expected, restored$value@metadata@template$variant)
  }
  for (selection in list(list(), list(trial_platform = ""), list(trial_platform = "unsupported"))) {
    result <- create(def, tempfile(), selection)
    qa_step("an unresolved platform never creates a document", FALSE, result$ok)
    qa_check("the error identifies the required selection", grepl("trial_platform", result$error, fixed = TRUE))
  }
})
