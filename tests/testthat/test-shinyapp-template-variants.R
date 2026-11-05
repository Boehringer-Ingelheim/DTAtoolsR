variant_creation_fixture <- function() {
  dataset <- function(id, type) {
    list(
      id = "fa", name = "Adjudication findings", type = "tabular",
      files = list(filename = "fa.csv", type = "csv"),
      columns = list(list(id = id, type = type)), rules = list()
    )
  }
  list(
    kind = "dta_creation_template", id = "adjudication", version = "1.0",
    base = list(metadata = list(title = "Adjudication", version = "1.0")),
    datasets = list(dataset("SUBJIDN", "SAS Num")),
    options = list(list(
      id = "medium", type = "text", allow_custom = TRUE,
      target = "metadata.transmission.method", choices = list("EDT")
    )),
    variants = list(
      id = "trial_platform", label = "Trial platform",
      choices = list(
        list(
          value = "brave", label = "BRAVE",
          source = list(document = "Adjudication DTS", version = "5.0"),
          base = list(metadata = list(version = "5.0")),
          datasets = list(dataset("SUBJIDN", "SAS Num"))
        ),
        list(
          value = "veeva", label = "Veeva",
          source = list(document = "Adjudication DTS", version = "5.0"),
          base = list(metadata = list(version = "5.0")),
          options = list(list(id = "medium", choices = list("EDT", "CDB"))),
          datasets = list(dataset("SUBJECT", "SAS Char"))
        )
      )
    )
  )
}

variant_creation_resolver <- function(defs) {
  function(ref) {
    def <- defs[[ref]]
    if (is.null(def)) {
      return(NULL)
    }
    list(def = def, id = def$id, version = def$version)
  }
}

test_that("platform selection replaces the schema before creation", {
  def <- variant_creation_fixture()
  create <- app_fn("create_dta_from_template")
  path <- tempfile(fileext = ".yaml")
  for (platform in c("brave", "veeva")) {
    built <- create(def, path, list(trial_platform = platform, medium = "custom"))
    expect_true(built$ok, info = built$error)
    if (!isTRUE(built$ok)) next
    cols <- built$value@datasets[[1]]@specs@columns
    expect_identical(
      names(cols), if (platform == "brave") "SUBJIDN" else "SUBJECT"
    )
    expect_identical(
      as.list(cols[[1]])$type, if (platform == "brave") "SAS Num" else "SAS Char"
    )
    expect_identical(metadata(built$value)@version, "5.0")
    expect_identical(metadata(built$value)@transmission$method, "custom")
    expect_identical(built$value@datasets[[1]]@id, "fa")
    expect_identical(built$value@datasets[[1]]@name, "Adjudication findings")
  }
})

test_that("an unresolved or unsupported platform never falls back to a schema", {
  create <- app_fn("create_dta_from_template")
  def <- variant_creation_fixture()
  for (value in list(NULL, "", "__blank__", "custom", c("brave", "veeva"))) {
    result <- create(def, tempfile(), list(trial_platform = value))
    expect_false(result$ok)
    expect_match(result$error, "trial_platform")
  }
})

test_that("variant choices use inherited options without leaking other platforms", {
  resolve <- app_fn("resolve_template_variant")
  def <- variant_creation_fixture()
  brave <- resolve(def, list(trial_platform = "brave"))
  veeva <- resolve(def, list(trial_platform = "veeva"))
  expect_identical(brave$options[[1]]$choices, list("EDT"))
  expect_identical(veeva$options[[1]]$choices, list("EDT", "CDB"))
  expect_identical(veeva$options[[1]]$target, "metadata.transmission.method")
  expect_true(veeva$options[[1]]$allow_custom)
  expect_identical(def$options[[1]]$choices, list("EDT"))
  expect_identical(resolve(veeva, list(trial_platform = "brave"))$datasets, brave$datasets)
})

test_that("child variant metadata cannot override an ancestor's seal", {
  parent <- variant_creation_fixture()
  parent$variants <- NULL
  parent$sealed <- "base.metadata.title"
  child <- list(
    id = "child", version = "1.0", extends = parent$id,
    variants = variant_creation_fixture()$variants
  )
  child$variants$choices[[1]]$base$metadata$title <- "Overridden"
  expect_error(
    app_fn("resolve_template_inheritance")(child, variant_creation_resolver(list(adjudication = parent))),
    class = "dta_template_sealed_violation"
  )
})

test_that("child variant option effects cannot write sealed metadata", {
  parent <- variant_creation_fixture()
  parent$variants <- NULL
  parent$sealed <- "base.metadata.title"
  for (effect in list(
    list(target = "metadata.title"),
    list(effects = list(list(path = "metadata.title", value = "Overridden"))),
    list(effects_all = list(list(set = list(metadata.title = "Overridden"))))
  )) {
    child <- list(
      id = "child", version = "1.0", extends = parent$id,
      variants = variant_creation_fixture()$variants
    )
    child$variants$choices[[1]]$options <- list(c(
      list(id = "title_override", type = "text", default = "Overridden"), effect
    ))
    expect_error(
      app_fn("resolve_template_inheritance")(child, variant_creation_resolver(list(adjudication = parent))),
      class = "dta_template_sealed_violation"
    )
  }
})

test_that("child variant dataset replacement cannot bypass dataset seals", {
  parent <- variant_creation_fixture()
  parent$variants <- NULL
  parent$sealed <- "datasets"
  child <- list(
    id = "child", version = "1.0", extends = parent$id,
    variants = variant_creation_fixture()$variants
  )
  expect_error(
    app_fn("resolve_template_inheritance")(child, variant_creation_resolver(list(adjudication = parent))),
    class = "dta_template_sealed_violation"
  )
})

test_that("a sealing ancestor's own variants and unchanged inherited variants remain allowed", {
  parent <- variant_creation_fixture()
  parent$sealed <- c("base.metadata.version", "base.metadata.title", "datasets")
  parent$variants$choices[[2]]$options <- list(list(
    id = "title", type = "text", target = "metadata.title", default = "Variant title"
  ))
  resolver <- variant_creation_resolver(list(adjudication = parent))
  resolve <- app_fn("resolve_template_inheritance")
  expect_identical(resolve(parent, resolver)$def, parent)
  child <- list(id = "child", version = "1.0", extends = parent$id)
  inherited <- resolve(child, resolver)$def
  expect_identical(inherited$variants, parent$variants)
  for (value in c("brave", "veeva")) {
    effective <- app_fn("resolve_template_variant")(inherited, list(trial_platform = value))
    expected <- app_fn("resolve_template_variant")(parent, list(trial_platform = value))
    expect_identical(effective$base, expected$base)
    expect_identical(effective$options, expected$options)
    expect_identical(effective$datasets, expected$datasets)
  }
  grandchild <- list(id = "grandchild", version = "1.0", extends = "child")
  expect_identical(
    resolve(grandchild, variant_creation_resolver(list(adjudication = parent, child = child)))$def$variants,
    parent$variants
  )
})

test_that("shared child options cannot change an inherited sealed effective variant", {
  parent <- variant_creation_fixture()
  parent$sealed <- "base.metadata.title"
  parent$options[[1]]$default <- "EDT"
  parent$variants$choices[[1]]$options <- list(list(
    id = "medium", target = "metadata.title"
  ))
  child <- list(
    id = "child", version = "1.0", extends = parent$id,
    options = list(list(id = "medium", default = "Overridden"))
  )
  expect_error(
    app_fn("resolve_template_inheritance")(child, variant_creation_resolver(list(adjudication = parent))),
    class = "dta_template_sealed_violation"
  )
})

test_that("child replacements compare matching parent choices, not just shared defaults", {
  parent <- variant_creation_fixture()
  parent$sealed <- "base.metadata.title"
  parent$variants$choices[[1]]$base$metadata$title <- "Protected variant"
  child <- list(
    id = "child", version = "1.0", extends = parent$id,
    variants = parent$variants
  )
  resolve <- app_fn("resolve_template_inheritance")
  resolver <- variant_creation_resolver(list(adjudication = parent))
  expect_identical(resolve(child, resolver)$def$variants, parent$variants)
  child$variants$choices[[1]]$base$metadata$title <- parent$base$metadata$title
  expect_error(resolve(child, resolver), class = "dta_template_sealed_violation")
  child$variants <- NULL
  child$base <- list(metadata = list(title = "Overridden shared base"))
  expect_error(resolve(child, resolver), class = "dta_template_sealed_violation")
})

test_that("new child selector or choice values do not inherit an ancestor's exemption", {
  parent <- variant_creation_fixture()
  parent$sealed <- "base.metadata.title"
  parent$variants$choices[[1]]$base$metadata$title <- "Ancestor choice"
  resolve <- app_fn("resolve_template_inheritance")
  resolver <- variant_creation_resolver(list(adjudication = parent))
  for (change in c("selector", "value")) {
    child <- list(
      id = "child", version = "1.0", extends = parent$id,
      variants = parent$variants
    )
    if (change == "selector") {
      child$variants$id <- "other_platform"
    } else {
      child$variants$choices[[1]]$value <- "other_value"
    }
    expect_error(resolve(child, resolver), class = "dta_template_sealed_violation")
  }
})

test_that("variants-only definitions load and invalid variants are rejected", {
  read <- app_fn("read_dta_creation_template")
  finalize <- app_fn("dta_template_finalize_def")
  def <- variant_creation_fixture()
  def$datasets <- NULL
  path <- tempfile(fileext = ".dta-template.yaml")
  withr::defer(unlink(path))
  yaml::write_yaml(def, path)
  loaded <- read(path)
  expect_true(loaded$ok, info = loaded$error)
  if (isTRUE(loaded$ok)) expect_length(finalize(loaded$value)$variants$choices, 2)
  for (bad in list(
    list(id = "medium", label = "Platform", choices = def$variants$choices),
    list(id = "trial_platform", choices = list(def$variants$choices[[1]], def$variants$choices[[1]])),
    list(id = "trial_platform", choices = list(list(value = "brave", datasets = list()))),
    list(id = "trial_platform", choices = list(list(value = "brave", unexpected = TRUE)))
  )) {
    def$variants <- bad
    expect_error(finalize(def), class = "rlang_error")
  }
})

test_that("variant provenance records platform and source and survives YAML", {
  def <- variant_creation_fixture()
  built <- app_fn("create_dta_from_template")(
    def, tempfile(), list(trial_platform = "veeva", medium = ""),
    provenance = list(id = "adjudication", version = "1.0")
  )
  expect_true(built$ok, info = built$error)
  if (!isTRUE(built$ok)) {
    return()
  }
  expect_identical(metadata(built$value)@template$variant$value, "veeva")
  expect_identical(metadata(built$value)@template$variant$source$version, "5.0")
  expect_identical(metadata(built$value)@transmission$method, "")
  path <- tempfile(fileext = ".yaml")
  withr::defer(unlink(path))
  serialized <- app_fn("dta_to_yaml_text")(built$value)
  expect_true(serialized$ok, info = serialized$error)
  writeLines(serialized$value, path)
  restored <- read_dta_from_yaml(path)
  expect_identical(metadata(restored)@template$variant, metadata(built$value)@template$variant)
  expect_identical(names(restored@datasets[[1]]@specs@columns), "SUBJECT")
})

test_that("platform control cannot accept custom values or choose implicitly", {
  html <- as.character(app_fn("template_variant_input")(variant_creation_fixture()))
  expect_match(html, 'id="tmpl_variant_trial_platform"', fixed = TRUE)
  expect_match(html, 'value="" selected', fixed = TRUE)
  expect_match(html, "Choose Trial platform", fixed = TRUE)
  expect_match(html, 'value="brave"', fixed = TRUE)
  expect_match(html, 'value="veeva"', fixed = TRUE)
})

test_that("the server requires platform selection before collecting its options", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false"
  ))
  app_fn("dta_template_index_invalidate")()
  def <- variant_creation_fixture()
  yaml::write_yaml(def, file.path(root, "adjudication.dta-template.yaml"))
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(create_from_template = 1)
    output$template_picker_ui
    session$setInputs(template_select_name = "adjudication", template_select_version = "1.0")
    session$setInputs(template_select_next = 1)
    session$setInputs(template_create_confirm = 1, tmpl_carry_source = "none")
    expect_null(rv$dta)
    session$setInputs(tmpl_variant_trial_platform = "veeva")
    session$setInputs(template_variant_confirm = 1)
    expect_identical(rv$template_variant_selections, list(trial_platform = "veeva"))
    session$setInputs(tmpl_opt_medium = "CDB")
    session$setInputs(template_create_confirm = 2)
    expect_s3_class(rv$dta, "DTAtools::DTA")
    if (!is.null(rv$dta)) {
      expect_identical(names(rv$dta@datasets[[1]]@specs@columns), "SUBJECT")
      expect_identical(rv$dta@metadata@transmission$method, "CDB")
      expect_identical(rv$dta@metadata@template$variant$value, "veeva")
      expect_null(rv$dta@metadata@template$selections)
    }
  })
})

test_that("strict validation dry-runs every platform rather than an arbitrary default", {
  root <- withr::local_tempdir()
  def <- variant_creation_fixture()
  path <- file.path(root, "adjudication.dta-template.yaml")
  yaml::write_yaml(def, path)
  expect_identical(nrow(validate_template(root, strict = TRUE)), 0L)
  def$variants$choices[[2]]$datasets <- list(list(template = "missing_records@1.0"))
  yaml::write_yaml(def, path)
  expect_error(validate_template(root, strict = TRUE), class = "rlang_error")
  diagnostics <- validate_template(root)
  expect_true(any(diagnostics$code == "instantiate_failed"))
  expect_true(any(grepl("veeva", diagnostics$message, fixed = TRUE)))
})
