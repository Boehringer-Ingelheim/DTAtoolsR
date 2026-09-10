# The Word template placeholder vocabulary. REQ-EXPORT-030.
#
# A template author writes {DTA_TITLE} into a Word document and expects the
# transfer's title to appear there. The list of names that works is the whole
# contract, and it is only useful if the documented set and the substituted set
# are the same set -- a name that is documented but never filled in, or filled
# in but never documented, leaves the author guessing at exactly the thing this
# mechanism exists to make explicit.

test_that("OQ-EXPORT-040 | the placeholder vocabulary is reported with a description of each | REQ-EXPORT-030", {
  described <- dta_template_placeholders()

  qa_check("a vocabulary is reported", length(described) > 0)
  qa_step(
    "every entry is named for the token a template writes",
    TRUE,
    all(grepl("^\\{[A-Z0-9_]+\\}$", names(described)))
  )
  qa_step(
    "and every entry carries a description rather than an empty string",
    TRUE,
    all(nzchar(as.character(described)))
  )

  # The names a template author is most likely to reach for first.
  qa_step(
    "the transfer's own identity is addressable",
    c("{DTA_TITLE}", "{DTA_VERSION}", "{DTA_DATE}"),
    intersect(c("{DTA_TITLE}", "{DTA_VERSION}", "{DTA_DATE}"), names(described))
  )
  qa_step(
    "and so are both parties to it",
    c("{SUPPLIER_NAME}", "{RECEIVER_NAME}"),
    intersect(c("{SUPPLIER_NAME}", "{RECEIVER_NAME}"), names(described))
  )
})

test_that("OQ-EXPORT-041 | the same vocabulary carries a transfer's values | REQ-EXPORT-030", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  if (!nzchar(path)) {
    testthat::skip("the bundled example specification is not installed")
  }
  dta <- read_dta_from_yaml(path)

  described <- dta_template_placeholders()
  filled <- dta_template_placeholders(dta)

  # The two calls must offer the same names. If the populated set were smaller,
  # a template using a documented placeholder would render it as literal text
  # in a signed document; if it were larger, a substitution would happen that
  # no author could have known to expect.
  qa_step(
    "describing and filling report the same placeholder names",
    names(described), names(filled)
  )

  qa_step(
    "and the values are the transfer's own",
    c(
      title = metadata(dta)@title,
      version = as.character(metadata(dta)@version)
    ),
    c(
      title = unname(filled[["{DTA_TITLE}"]]),
      version = unname(filled[["{DTA_VERSION}"]])
    )
  )
  qa_step(
    "the date is rendered as an ISO date rather than as a number",
    TRUE,
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", filled[["{DTA_DATE}"]])
  )
})
