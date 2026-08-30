# Coverage for inst/shiny/dta_app/R/party_profiles.R, reached via the
# app_env()/app_fn() harness in helper-shinyapp.R (see that file for why this
# is necessary: the app's helper files are auto-sourced by Shiny at launch and
# are not part of the package namespace).

Sys.setenv(NOT_CRAN = "true")

# cli wraps a long condition message across lines at the console width, which
# breaks a naive substring/regexp match the moment the wrap lands inside the
# very text being asserted on -- see the identical helper (and identical
# rationale) in test-shinyapp-template-inherit.R. Duplicated here rather than
# shared: helper-shinyapp.R is the one file every test-shinyapp-*.R may rely
# on, and this task adds no second one.
expect_error_message_contains <- function(expr, fixed_text) {
  err <- tryCatch(
    {
      expr
      NULL
    },
    error = function(e) e
  )
  testthat::expect_false(is.null(err), info = "expected an error, got none")
  flat <- gsub("\\s+", " ", conditionMessage(err))
  testthat::expect_match(flat, fixed_text, fixed = TRUE)
}

# A minimal but complete `dta_party_profile` YAML, matching the shape
# documented at the top of party_profiles.R. Arguments are parameterised so
# individual tests can swap in the pieces they are actually exercising
# (a bad kind, a missing id, an invalid role, ...) without repeating the rest.
write_party_profile_fixture <- function(
  dir,
  id = "supplier_acme",
  version_line = 'version: "1.0"',
  role_line = "role: supplier",
  extra_lines = character(0),
  filename = "profile.yaml"
) {
  path <- file.path(dir, filename)
  writeLines(
    c(
      "kind: dta_party_profile",
      paste0("id: ", id),
      version_line,
      role_line,
      "label: ACME Labs",
      "description: Standard ACME sequencing supplier block",
      "affiliation:",
      "  name: ACME Labs",
      "  address: 1 Example Way",
      "  country: DE",
      "contacts:",
      "  - name: Jane Doe",
      "    role: Data Manager",
      "    email: jane.doe@example.com",
      "    signature: true",
      extra_lines
    ),
    path
  )
  path
}

# ---- read_party_profile -----------------------------------------------------

test_that("read_party_profile() accepts a valid file", {
  dir <- withr::local_tempdir()
  path <- write_party_profile_fixture(dir)

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_true(res$ok)
  expect_null(res$error)
  expect_equal(res$value$id, "supplier_acme")
  expect_equal(res$value$version, "1.0")
  expect_equal(res$value$role, "supplier")
  expect_equal(res$value$label, "ACME Labs")
  expect_equal(res$value$description, "Standard ACME sequencing supplier block")
  expect_equal(res$value$affiliation$name, "ACME Labs")
  expect_length(res$value$contacts, 1)
  expect_equal(res$value$contacts[[1]]$name, "Jane Doe")
})

test_that("read_party_profile() rejects a wrong 'kind'", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.yaml")
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: supplier_acme",
      'version: "1.0"'
    ),
    path
  )

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_false(res$ok)
  expect_null(res$value)
  # App's own hardcoded string, not a base R/yaml message -- fine to assert.
  expect_equal(res$error, "Party profile 'kind' must be 'dta_party_profile'.")
})

test_that("read_party_profile() rejects a missing 'id'", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.yaml")
  writeLines(
    c(
      "kind: dta_party_profile",
      'version: "1.0"'
    ),
    path
  )

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_false(res$ok)
  expect_equal(res$error, "Party profile must define a non-empty 'id'.")
})

test_that("read_party_profile() rejects a missing 'version'", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.yaml")
  writeLines(
    c(
      "kind: dta_party_profile",
      "id: supplier_acme"
    ),
    path
  )

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_false(res$ok)
  expect_equal(res$error, "Party profile must define a non-empty 'version'.")
})

test_that("read_party_profile() rejects an invalid 'role'", {
  dir <- withr::local_tempdir()
  path <- write_party_profile_fixture(dir, role_line = "role: courier")

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_false(res$ok)
  expect_match(res$error, "invalid role 'courier'", fixed = TRUE)
})

test_that("read_party_profile() defaults 'role' to 'any' when absent", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.yaml")
  writeLines(
    c(
      "kind: dta_party_profile",
      "id: generic_contact",
      'version: "1.0"'
    ),
    path
  )

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_true(res$ok)
  expect_equal(res$value$role, "any")
})

test_that("read_party_profile() keeps a quoted version exact", {
  dir <- withr::local_tempdir()
  path <- write_party_profile_fixture(dir, version_line = 'version: "1.10"')

  fn <- app_fn("read_party_profile")
  res <- fn(path)

  expect_true(res$ok)
  expect_equal(res$value$version, "1.10")
})

test_that("read_party_profile() drops a malformed contact with a warning, keeping the good ones", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.yaml")
  writeLines(
    c(
      "kind: dta_party_profile",
      "id: supplier_acme",
      'version: "1.0"',
      "contacts:",
      "  - name: Jane Doe",
      "    role: Data Manager",
      "  - just a bare string, not a mapping",
      "  - name: John Roe",
      "    role: Backup Contact"
    ),
    path
  )

  fn <- app_fn("read_party_profile")
  expect_warning(res <- fn(path), regexp = "malformed contact")

  expect_true(res$ok)
  expect_length(res$value$contacts, 2)
  expect_equal(
    vapply(res$value$contacts, function(c) c$name, character(1)),
    c("Jane Doe", "John Roe")
  )
})

# ---- party_slot_target_valid ------------------------------------------------

test_that("party_slot_target_valid() accepts exactly the two supported targets", {
  fn <- app_fn("party_slot_target_valid")

  expect_true(fn("metadata.supplier"))
  expect_true(fn("metadata.receiver"))
})

test_that("party_slot_target_valid() rejects anything else without throwing", {
  fn <- app_fn("party_slot_target_valid")

  expect_false(fn("metadata.title"))
  expect_false(fn("metadata"))
  expect_false(fn(""))
  expect_false(fn(NULL))
})

# ---- normalise_party_slots --------------------------------------------------

test_that("normalise_party_slots() fills in label/role/profiles defaults", {
  fn <- app_fn("normalise_party_slots")

  result <- fn(list(list(id = "supplier_choice", target = "metadata.supplier")))

  expect_length(result, 1)
  expect_equal(result[[1]]$id, "supplier_choice")
  expect_equal(result[[1]]$target, "metadata.supplier")
  expect_equal(result[[1]]$label, "supplier_choice")
  expect_equal(result[[1]]$role, "supplier")
  expect_equal(result[[1]]$profiles, character(0))
})

test_that("normalise_party_slots() derives 'role' from 'target' for both directions", {
  fn <- app_fn("normalise_party_slots")

  result <- fn(list(
    list(id = "s1", target = "metadata.supplier"),
    list(id = "s2", target = "metadata.receiver")
  ))

  expect_equal(result[[1]]$role, "supplier")
  expect_equal(result[[2]]$role, "receiver")
})

test_that("normalise_party_slots() keeps an explicitly stated role instead of deriving it", {
  fn <- app_fn("normalise_party_slots")

  result <- fn(list(list(id = "s1", target = "metadata.supplier", role = "any")))

  expect_equal(result[[1]]$role, "any")
})

test_that("normalise_party_slots() drops a slot with a missing or empty id, with a warning", {
  fn <- app_fn("normalise_party_slots")

  expect_warning(
    result <- fn(list(
      list(target = "metadata.supplier"),
      list(id = "", target = "metadata.receiver"),
      list(id = "kept", target = "metadata.supplier")
    )),
    regexp = "missing or empty"
  )

  expect_length(result, 1)
  expect_equal(result[[1]]$id, "kept")
})

test_that("normalise_party_slots() aborts naming the slot and the invalid target", {
  fn <- app_fn("normalise_party_slots")

  expect_error_message_contains(
    fn(list(list(id = "bad_slot", target = "metadata.title"))),
    "bad_slot"
  )
  expect_error_message_contains(
    fn(list(list(id = "bad_slot", target = "metadata.title"))),
    "metadata.title"
  )
})

# ---- party_profiles_for_slot -------------------------------------------------

party_profile_stub <- function(id, role, label = id) {
  list(id = id, role = role, label = label)
}

test_that("party_profiles_for_slot() filters by role match", {
  fn <- app_fn("party_profiles_for_slot")
  slot <- list(id = "s1", target = "metadata.supplier", role = "supplier", profiles = character(0))
  profiles <- list(
    party_profile_stub("supplier_a", "supplier"),
    party_profile_stub("receiver_a", "receiver")
  )

  result <- fn(profiles, slot)

  expect_equal(vapply(result, function(p) p$id, character(1)), "supplier_a")
})

test_that("party_profiles_for_slot() always includes 'any'-role profiles", {
  fn <- app_fn("party_profiles_for_slot")
  supplier_slot <- list(id = "s1", target = "metadata.supplier", role = "supplier", profiles = character(0))
  receiver_slot <- list(id = "s2", target = "metadata.receiver", role = "receiver", profiles = character(0))
  profiles <- list(party_profile_stub("generic", "any"))

  expect_equal(vapply(fn(profiles, supplier_slot), function(p) p$id, character(1)), "generic")
  expect_equal(vapply(fn(profiles, receiver_slot), function(p) p$id, character(1)), "generic")
})

test_that("party_profiles_for_slot() restricts to an explicit allow-list", {
  fn <- app_fn("party_profiles_for_slot")
  slot <- list(
    id = "s1", target = "metadata.supplier", role = "supplier",
    profiles = c("supplier_a")
  )
  profiles <- list(
    party_profile_stub("supplier_a", "supplier"),
    party_profile_stub("supplier_b", "supplier")
  )

  result <- fn(profiles, slot)

  expect_equal(vapply(result, function(p) p$id, character(1)), "supplier_a")
})

test_that("party_profiles_for_slot() orders eligible profiles by label with radix collation", {
  fn <- app_fn("party_profiles_for_slot")
  slot <- list(id = "s1", target = "metadata.supplier", role = "supplier", profiles = character(0))
  profiles <- list(
    party_profile_stub("z", "supplier", label = "Zeta Labs"),
    party_profile_stub("a", "supplier", label = "Alpha Labs"),
    party_profile_stub("m", "supplier", label = "Mu Labs")
  )

  result <- fn(profiles, slot)

  expect_equal(
    vapply(result, function(p) p$label, character(1)),
    sort(c("Zeta Labs", "Alpha Labs", "Mu Labs"), method = "radix")
  )
})

# ---- apply_party_profile -----------------------------------------------------

test_that("apply_party_profile() replaces the whole supplier block instead of merging with what was already there", {
  dta <- app_fixture_dta()
  before <- DTAtools::metadata(dta)@supplier
  # The bundled fixture is exactly the "already populated" starting point the
  # spec asks for: two contacts plus an affiliation.
  expect_length(before$contacts, 2)
  expect_equal(before$affiliation$name, "Test Company 2")

  profile <- list(
    affiliation = list(name = "ACME Labs", address = "1 Example Way", country = "DE"),
    contacts = list(list(
      name = "Jane Doe", role = "Data Manager",
      email = "jane.doe@example.com", signature = TRUE
    ))
  )

  fn <- app_fn("apply_party_profile")
  dta2 <- fn(dta, "metadata.supplier", profile)

  after <- DTAtools::metadata(dta2)@supplier
  expect_length(after$contacts, 1)
  expect_equal(after$contacts[[1]]$name, "Jane Doe")
  expect_equal(after$affiliation, profile$affiliation)

  # Nothing from the OLD block survives: neither the old contacts' names...
  old_names <- vapply(before$contacts, function(c) c$name, character(1))
  new_names <- vapply(after$contacts, function(c) c$name, character(1))
  expect_false(any(old_names %in% new_names))
  # ...nor the old affiliation.
  expect_false(identical(after$affiliation$name, before$affiliation$name))
})

test_that("apply_party_profile() rejects a target other than metadata.supplier/metadata.receiver", {
  dta <- app_fixture_dta()
  fn <- app_fn("apply_party_profile")

  expect_error_message_contains(
    fn(dta, "metadata.title", list(affiliation = list(name = "x"))),
    "metadata.supplier"
  )
})

# ---- apply_party_selections --------------------------------------------------

full_party_slots <- function() {
  app_fn("normalise_party_slots")(list(
    list(id = "supplier_choice", target = "metadata.supplier"),
    list(id = "receiver_choice", target = "metadata.receiver")
  ))
}

full_party_profiles <- function() {
  list(
    list(
      id = "supplier_acme", role = "supplier", label = "ACME Labs",
      affiliation = list(name = "ACME Labs", country = "DE"),
      contacts = list(list(name = "Jane Doe", role = "Data Manager"))
    ),
    list(
      id = "receiver_gencorp", role = "receiver", label = "GenCorp",
      affiliation = list(name = "GenCorp", country = "US"),
      contacts = list(list(name = "Sam Receiver", role = "Data Manager"))
    )
  )
}

test_that("apply_party_selections() writes a selected profile to the right target", {
  dta <- app_fixture_dta()
  fn <- app_fn("apply_party_selections")

  dta2 <- fn(dta, full_party_slots(), list(supplier_choice = "supplier_acme"), full_party_profiles())

  supplier <- DTAtools::metadata(dta2)@supplier
  expect_equal(supplier$affiliation$name, "ACME Labs")
  expect_equal(supplier$contacts[[1]]$name, "Jane Doe")
})

test_that("apply_party_selections() leaves an unselected slot untouched", {
  dta <- app_fixture_dta()
  before_receiver <- DTAtools::metadata(dta)@receiver
  fn <- app_fn("apply_party_selections")

  # Only the supplier slot has a selection; the receiver slot's entry is
  # missing entirely from `selections`.
  dta2 <- fn(dta, full_party_slots(), list(supplier_choice = "supplier_acme"), full_party_profiles())

  expect_equal(DTAtools::metadata(dta2)@receiver, before_receiver)
})

test_that("apply_party_selections() aborts naming the slot on an unknown profile id", {
  dta <- app_fixture_dta()
  fn <- app_fn("apply_party_selections")

  expect_error_message_contains(
    fn(dta, full_party_slots(), list(supplier_choice = "no_such_profile"), full_party_profiles()),
    "supplier_choice"
  )
  expect_error_message_contains(
    fn(dta, full_party_slots(), list(supplier_choice = "no_such_profile"), full_party_profiles()),
    "no_such_profile"
  )
})

test_that("apply_party_selections() applies a receiver and a supplier profile in one call without cross-contamination", {
  dta <- app_fixture_dta()
  fn <- app_fn("apply_party_selections")

  dta2 <- fn(
    dta,
    full_party_slots(),
    list(supplier_choice = "supplier_acme", receiver_choice = "receiver_gencorp"),
    full_party_profiles()
  )

  supplier <- DTAtools::metadata(dta2)@supplier
  receiver <- DTAtools::metadata(dta2)@receiver

  expect_equal(supplier$affiliation$name, "ACME Labs")
  expect_equal(supplier$contacts[[1]]$name, "Jane Doe")
  expect_equal(receiver$affiliation$name, "GenCorp")
  expect_equal(receiver$contacts[[1]]$name, "Sam Receiver")

  # Each field wrote to its own slot's target only -- neither picked up the
  # other's affiliation or contact.
  expect_false(identical(supplier$affiliation$name, receiver$affiliation$name))
  expect_false(identical(supplier$contacts[[1]]$name, receiver$contacts[[1]]$name))
})

test_that("apply_party_selections() refuses a profile whose role does not fit the slot", {
  dta <- app_fixture_dta()
  fn <- app_fn("apply_party_selections")

  # The UI builds its dropdown from party_profiles_for_slot(), so this cannot
  # happen from the app. It CAN happen from a restored session, a saved
  # selection made against a template whose slots have since changed, or any
  # direct caller. The result would be a document naming the data RECEIVER as
  # the data SUPPLIER -- entirely plausible-looking, and wrong. Existence of
  # the id is not the same check as eligibility for the slot.
  expect_error(
    fn(
      dta, full_party_slots(),
      list(supplier_choice = "receiver_gencorp"),
      full_party_profiles()
    ),
    regexp = "cannot take profile"
  )
})

test_that("apply_party_selections() still accepts an 'any' role in either slot", {
  dta <- app_fixture_dta()
  fn <- app_fn("apply_party_selections")
  profiles <- c(full_party_profiles(), list(
    list(
      id = "shared_bureau", role = "any", label = "Shared Bureau",
      affiliation = list(name = "Shared Bureau"),
      contacts = list(list(name = "Alex Both"))
    )
  ))

  # The eligibility tightening must not break the deliberate "any" escape
  # hatch, which exists so one profile can serve both sides of a transfer.
  dta2 <- fn(
    dta, full_party_slots(),
    list(supplier_choice = "shared_bureau", receiver_choice = "shared_bureau"),
    profiles
  )
  md <- DTAtools::metadata(dta2)
  expect_equal(md@supplier$affiliation$name, "Shared Bureau")
  expect_equal(md@receiver$affiliation$name, "Shared Bureau")
})
