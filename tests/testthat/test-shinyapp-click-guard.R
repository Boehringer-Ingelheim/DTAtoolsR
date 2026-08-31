# The click guard -- click_guard_script() in R/ui_components.R, and the CSS it
# drives in bi_css() (R/theme.R).
#
# These are SOURCE-LEVEL assertions, which this suite otherwise avoids, and the
# reason is specific: the behaviour under test is JavaScript running against a
# DOM, and there is no JS engine anywhere in the package's dependency set (no
# V8, no chromote, no shinytest2), so the guard cannot be executed here at all.
#
# What is pinned instead is the small set of structural properties the guard's
# SAFETY rests on -- each one a thing that, if it silently regressed, would
# either break the export download or leave a button permanently dead on
# screen, and neither of which any other test in this suite would notice. Each
# test below names that failure rather than checking that a word is present.

# The rendered <script> tag, via the app helper environment.
click_guard_tag <- function() app_fn("click_guard_script")()

# The script BODY, not the rendered <script>...</script> -- the closing tag the
# test below looks for is otherwise always present, and the assertion vacuous.
guard_js <- function() {
  paste(as.character(click_guard_tag()$children[[1]]), collapse = "\n")
}

app_css <- function() paste(as.character(app_fn("bi_css")()), collapse = "\n")

# Body of a top-level `function name(args) { ... }` in the guard source, so the
# tests below can assert about ONE branch rather than about the whole blob.
# Returns NA when the function is not found, which every caller asserts on: a
# renamed helper must fail loudly here, not silently pass a vacuous match.
guard_fn_body <- function(signature) {
  js <- guard_js()
  start <- regexpr(paste0(signature, " {"), js, fixed = TRUE)[[1]]
  if (start < 0) {
    return(NA_character_)
  }
  rest <- substring(js, start)
  end <- regexpr("\n  }\n", rest, fixed = TRUE)[[1]]
  if (end < 0) {
    return(NA_character_)
  }
  substring(rest, nchar(signature) + 3L, end)
}

test_that("the guard renders as a script tag the page can actually carry", {
  tag <- click_guard_tag()

  expect_s3_class(tag, "shiny.tag")
  expect_identical(tag$name, "script")

  js <- guard_js()
  # An unescaped "</script>" anywhere in the body would close the tag early and
  # spill the rest of the guard into the document as text -- silently, since
  # the page would still render.
  expect_no_match(js, "</script", fixed = TRUE)
  # Capture phase (the trailing `true`) is the whole mechanism: it is what puts
  # this listener ahead of the element's own bubble-phase Shiny binding, so a
  # swallowed click never reaches it.
  expect_match(js, "addEventListener('click', function(ev){", fixed = TRUE)
  expect_match(js, "}, true);", fixed = TRUE)
})

test_that("the guard is registered in the app's head", {
  # In tags$head() rather than in the body: the listener has to be installed
  # before any button exists, so a click landing during the first render is
  # guarded like any other.
  src <- app_source("app.R")
  head_block <- sub("(?s)^.*?tags\\$head\\((.*?)\\n  \\),.*$", "\\1", src, perl = TRUE)

  expect_false(identical(head_block, src)) # the head block was actually found
  expect_match(head_block, "click_guard_script()", fixed = TRUE)
})

test_that("untrusted clicks are let through before anything can swallow them", {
  # THE failure this pins: app.R starts the export download by calling native
  # .click() on the hidden downloadButton "export_trigger_download" (see
  # download_trigger_js). That synthetic event matches .shiny-download-link, so
  # a guard that swallowed it would break the export outright -- and nothing
  # else in this suite exercises that path, because it only exists in a
  # browser.
  #
  # Asserted as an ORDERING, not as the presence of a word: the bail-out only
  # protects anything if it runs before the first preventDefault().
  js <- guard_js()

  trusted_at <- regexpr("!ev.isTrusted", js, fixed = TRUE)[[1]]
  prevent_at <- regexpr("preventDefault", js, fixed = TRUE)[[1]]

  expect_gt(trusted_at, 0)
  expect_gt(prevent_at, 0)
  expect_lt(trusted_at, prevent_at)

  # ...and the other half of the same guarantee: the export really is started
  # by a synthetic click, which is what isTrusted distinguishes it by.
  expect_match(app_source("app.R"), "el.click();", fixed = TRUE)
})

test_that("both kinds of button are guarded", {
  # actionButton and downloadButton fail differently -- one queues duplicate
  # inputs, the other issues duplicate HTTP downloads -- and only the download
  # case is invisible to the server. Dropping either class from the selector
  # leaves half the problem unfixed with no other symptom.
  js <- guard_js()

  expect_match(js, ".action-button", fixed = TRUE)
  expect_match(js, ".shiny-download-link", fixed = TRUE)
})

test_that("a download link is released by its own cooldown, not by shiny:idle", {
  # A file download is a plain HTTP GET: it never touches the websocket, so no
  # busy/idle pair is ever emitted for it. A download link parked in `pending`
  # to await shiny:idle would be released only by accident -- whenever some
  # unrelated observer next finished -- or not at all.
  hold_body <- guard_fn_body("function hold(el, isDownload)")
  expect_false(is.na(hold_body))

  download_branch <- sub(
    "(?s)^.*if \\(isDownload\\) \\{(.*?)\\n      return;.*$", "\\1",
    hold_body,
    perl = TRUE
  )
  expect_false(identical(download_branch, hold_body))
  expect_match(download_branch, "DOWNLOAD_HOLD", fixed = TRUE)
  # The download branch returns before pending.push(), so it never waits on an
  # idle that is not coming.
  expect_no_match(download_branch, "pending.push", fixed = TRUE)
  expect_match(hold_body, "pending.push(el)", fixed = TRUE)
})

test_that("every hold arms a release that does not depend on the server", {
  # A guard that can stick is worse than no guard. Whichever branch hold()
  # takes, a timer must be armed, so a lost shiny:idle -- a dropped socket, an
  # error escaping shiny's handler, or a button with no observer bound to it at
  # all -- degrades to a delayed release rather than a dead button.
  hold_body <- guard_fn_body("function hold(el, isDownload)")
  expect_false(is.na(hold_body))

  # Download branch: the fixed cooldown. Action branch: the sanity release for
  # a server that never went busy, plus the ceiling for a lost idle.
  expect_match(hold_body, "DOWNLOAD_HOLD", fixed = TRUE)
  expect_match(hold_body, "SANITY_HOLD", fixed = TRUE)
  expect_match(hold_body, "armCeiling(el, 0)", fixed = TRUE)

  # The busy state is entered in exactly one place, so there is no second,
  # unguarded path that could set it without arming any of the above.
  expect_equal(
    length(gregexpr("classList.add('dta-busy')", guard_js(), fixed = TRUE)[[1]]),
    1L
  )
})

test_that("the ceiling re-arms while the server works, but only so often", {
  # The ceiling exists for a LOST idle, not for a slow one. Firing it blind
  # would release a button mid-export and re-invite the second click the guard
  # was added to prevent, so it defers while the app is demonstrably busy.
  #
  # But the deferral must be BOUNDED, and this is the subtle half: one way to
  # lose an idle is a socket dropped after the busy message arrived, which
  # leaves `shiny-busy` itself stuck true. An unbounded re-arm would then
  # defer forever -- in precisely the scenario the ceiling exists to cover.
  ceiling_body <- guard_fn_body("function armCeiling(el, tries)")
  expect_false(is.na(ceiling_body))
  expect_match(
    ceiling_body,
    "if (serverBusy() && tries < CEILING_TRIES) { armCeiling(el, tries + 1); return; }",
    fixed = TRUE
  )
  expect_match(guard_js(), "var CEILING_TRIES", fixed = TRUE)
})

test_that("a lost connection releases everything still held", {
  # No idle is coming once the socket is gone, so without this the held
  # buttons wait out the full ceiling instead.
  expect_match(guard_js(), "onShiny('shiny:disconnected', releaseAll)", fixed = TRUE)
})

test_that("the sanity release cannot fire on a click still in flight", {
  # The failure this pins: sampling `shiny-busy` at a fixed deadline asks "is
  # the server busy NOW", which on a slow link is answered before the click
  # has even arrived. It would then conclude no observer exists, release the
  # button, and re-open the double-click window -- on exactly the slow
  # connections this feature is for.
  #
  # Comparing a busy-transition COUNTER instead asks "did the server ever
  # start work since this click", which no amount of latency can turn into a
  # false negative before the deadline.
  hold_body <- guard_fn_body("function hold(el, isDownload)")
  expect_false(is.na(hold_body))

  expect_match(hold_body, "el.dtaBusyTick = busyTicks", fixed = TRUE)
  expect_match(hold_body, "busyTicks === el.dtaBusyTick && !serverBusy()", fixed = TRUE)
  expect_match(guard_js(), "onShiny('shiny:busy', function(){ busyTicks++; })", fixed = TRUE)

  # ...and the deadline outlasts a plausible round trip rather than racing it.
  sanity_ms <- as.integer(sub(
    "(?s)^.*var SANITY_HOLD\\s*=\\s*(\\d+).*$", "\\1", guard_js(),
    perl = TRUE
  ))
  expect_gte(sanity_ms, 1000L)
})

test_that("releasing a button clears every timer it armed", {
  # Timers outlive the element: an output$main re-render replaces the whole
  # button DOM, and a timer still holding a detached node would fire against it
  # later. release() is the single teardown, so it has to clear all three.
  release_body <- guard_fn_body("function release(el)")
  expect_false(is.na(release_body))

  for (timer in c("dtaBusyShow", "dtaBusySanity", "dtaBusyCeiling")) {
    expect_match(release_body, sprintf("clearTimer(el, '%s')", timer), fixed = TRUE)
  }
  # ...and drops the element from the idle queue, so a detached node is not
  # retained for the life of the session.
  expect_match(release_body, "pending.splice(i, 1)", fixed = TRUE)
})

test_that("the busy styling is defined outside any media query", {
  # A rule whose ONLY definition sits inside @media (prefers-reduced-motion)
  # would leave the spinner invisible for everyone else -- the guard would then
  # swallow clicks while giving no sign it had done so, which reads as a broken
  # button rather than a busy one.
  css <- app_css()

  # Strip every @media block, then assert the rules survive.
  outside <- gsub("(?s)@media[^{]*\\{.*?\\}\\s*\\}", "", css, perl = TRUE)

  expect_match(outside, ".dta-busy { cursor: progress; }", fixed = TRUE)
  expect_match(outside, ".dta-busy-shown::after", fixed = TRUE)
  expect_match(outside, "@keyframes dta-busy-spin", fixed = TRUE)
})

test_that("a busy button stays in hit-testing", {
  # `pointer-events: none` is the obvious way to write a disabled-looking
  # button and is wrong here: it removes the element from hit-testing, so the
  # repeat click does not land on the button to be cancelled -- it lands on
  # whatever is behind it. Confirmed in a browser before this assertion was
  # written: with pointer-events set, the second click of a double-click
  # arrived retargeted rather than swallowed.
  # Comments first: the WHY note on these rules says the words
  # "pointer-events: none" in order to warn against them, and a selector
  # search that ran over it would match its own explanation.
  css <- gsub("(?s)/\\*.*?\\*/", "", app_css(), perl = TRUE)
  busy_rules <- regmatches(
    css,
    gregexpr("\\.dta-busy[^{}]*\\{[^}]*\\}", css, perl = TRUE)
  )[[1]]

  expect_gt(length(busy_rules), 0)
  expect_false(any(grepl("pointer-events", busy_rules, fixed = TRUE)))
})

test_that("reduced motion stops the spinner without hiding it", {
  # The spinner is the only feedback a guarded button gives. Hiding it under
  # prefers-reduced-motion would take that feedback away from exactly the users
  # least able to infer it from a missing animation.
  css <- app_css()
  block <- sub(
    "(?s)^.*@media \\(prefers-reduced-motion: reduce\\) \\{(.*?)\\n    \\}.*$", "\\1",
    css,
    perl = TRUE
  )
  expect_false(identical(block, css))
  expect_match(block, "animation: none", fixed = TRUE)
  expect_no_match(block, "display: none", fixed = TRUE)
  expect_no_match(block, "visibility: hidden", fixed = TRUE)
})
