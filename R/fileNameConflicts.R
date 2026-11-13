# ---- file-name conflicts -------------------------------------------------------
# Two pattern entries of different datasets that could take one and the same
# correctly packaged file make an agreement ambiguous. Detection is sound, not
# complete: example names are generated from every pattern, and a conflict is
# reported only with a real example that both entries take.

# ---- example names of a regular expression -------------------------------------

# Up to `limit` example names that `pattern` matches, shortest first; character(0)
# when the pattern uses a construct outside the supported subset (look-arounds,
# inline flags, back-references, possessive quantifiers, \p{..}, ...). Every
# example is verified with stringr::str_detect(), the engine matches_filename()
# uses, so an expansion mistake can only lose examples, never invent one.
dta_pattern_examples <- function(pattern, limit = 40L) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
    return(character(0))
  }
  cap <- max(as.integer(limit) * 4L, 40L)
  chars <- strsplit(pattern, "", fixed = TRUE)[[1]]
  n <- length(chars)
  pos <- 1L
  # Internal control flow only: this condition is always caught below and turns
  # into `character(0)`; it never reaches a user, so it is not a cli_abort().
  unsupported <- function() stop(structure(class = c("dta_pattern_unsupported", "error", "condition"), list(message = "unsupported", call = NULL)))
  peek <- function(k = 0L) if (pos + k <= n) chars[[pos + k]] else ""
  trim <- function(v) {
    v <- unique(v)
    if (length(v) > cap) {
      v <- v[order(nchar(v), v, method = "radix")][seq_len(cap)]
    }
    v
  }
  # Capped cartesian product of two example sets; a name longer than a file
  # name can be (255) is dropped, which also stops nested repeats from growing.
  # Operands are cut to their 40 shortest first: the shortest products are
  # products of the shortest operands, and 1600 pairs stay cheap.
  shortest <- function(v) {
    if (length(v) > 40L) v[order(nchar(v), v, method = "radix")][1:40] else v
  }
  product <- function(a, b) {
    v <- as.vector(outer(shortest(a), shortest(b), paste0))
    trim(v[nchar(v) <= 255L])
  }
  repeat_set <- function(e, k) {
    out <- ""
    for (i in seq_len(k)) {
      out <- product(out, e)
      if (length(out) == 0L) break
    }
    out
  }

  class_items <- function() {
    # After "[": returns list(items = list of integer c(lo, hi), negated).
    negated <- FALSE
    if (peek() == "^") {
      negated <- TRUE
      pos <<- pos + 1L
    }
    items <- list()
    first <- TRUE
    repeat {
      ch <- peek()
      if (ch == "") unsupported()
      if (ch == "]" && !first) {
        pos <<- pos + 1L
        break
      }
      first <- FALSE
      if (ch == "[") {
        if (peek(1L) != ":") unsupported()
        close <- NULL
        if (pos + 2L > n) unsupported()
        for (k in seq(pos + 2L, n)) {
          if (k + 1L <= n && chars[[k]] == ":" && chars[[k + 1L]] == "]") {
            close <- k
            break
          }
        }
        if (is.null(close)) unsupported()
        nm <- paste(chars[(pos + 2L):(close - 1L)], collapse = "")
        rng <- switch(nm,
          alpha = list(c(97L, 122L), c(65L, 90L)),
          digit = list(c(48L, 57L)),
          alnum = list(c(97L, 122L), c(65L, 90L), c(48L, 57L)),
          space = list(c(32L, 32L)),
          upper = list(c(65L, 90L)),
          lower = list(c(97L, 122L)),
          unsupported()
        )
        items <- c(items, rng)
        pos <<- close + 2L
        next
      }
      if (ch == "\\") {
        e <- peek(1L)
        if (e == "") unsupported()
        pos <<- pos + 2L
        rng <- switch(e,
          d = list(c(48L, 57L)),
          w = list(c(97L, 122L), c(65L, 90L), c(48L, 57L), c(95L, 95L)),
          s = list(c(32L, 32L)),
          {
            if (grepl("[[:alnum:]]", e)) unsupported()
            list(rep(utf8ToInt(e), 2L))
          }
        )
        items <- c(items, rng)
        next
      }
      lo <- utf8ToInt(ch)
      pos <<- pos + 1L
      if (peek() == "-" && peek(1L) != "]" && peek(1L) != "") {
        hi_ch <- peek(1L)
        if (hi_ch == "\\" || hi_ch == "[") unsupported()
        pos <<- pos + 2L
        items <- c(items, list(c(lo, utf8ToInt(hi_ch))))
      } else {
        items <- c(items, list(c(lo, lo)))
      }
    }
    if (length(items) == 0L) unsupported()
    list(items = items, negated = negated)
  }

  parse_atom <- function() {
    ch <- peek()
    pos <<- pos + 1L
    switch(ch,
      "(" = {
        if (peek() == "?") {
          if (peek(1L) != ":") unsupported()
          pos <<- pos + 2L
        }
        e <- parse_alt()
        if (peek() != ")") unsupported()
        pos <<- pos + 1L
        e
      },
      "[" = {
        cl <- class_items()
        if (cl$negated) {
          pick <- NULL
          for (cand in c("a", "x", "0", "_", "-")) {
            cp <- utf8ToInt(cand)
            if (!any(vapply(cl$items, function(r) cp >= r[[1]] && cp <= r[[2]], logical(1)))) {
              pick <- cand
              break
            }
          }
          if (is.null(pick)) unsupported()
          pick
        } else {
          intToUtf8(cl$items[[1]][[1]])
        }
      },
      "." = "a",
      "^" = "",
      "$" = "",
      "\\" = {
        e <- peek()
        if (e == "") unsupported()
        pos <<- pos + 1L
        switch(e,
          d = "0",
          w = "a",
          s = " ",
          D = "a",
          W = "-",
          b = "",
          {
            if (grepl("[[:alnum:]]", e)) unsupported()
            e
          }
        )
      },
      "*" = ,
      "+" = ,
      "?" = ,
      "{" = ,
      ")" = ,
      "|" = unsupported(),
      ch
    )
  }

  # The repeat counts a quantifier allows, or NULL when there is none.
  parse_quantifier <- function() {
    ch <- peek()
    counts <- NULL
    if (ch == "*") {
      counts <- c(0L, 1L)
      pos <<- pos + 1L
    } else if (ch == "+") {
      counts <- c(1L, 2L)
      pos <<- pos + 1L
    } else if (ch == "?") {
      counts <- c(0L, 1L)
      pos <<- pos + 1L
    } else if (ch == "{") {
      close <- match("}", chars[pos:n])
      if (is.na(close)) unsupported()
      body <- paste(chars[(pos + 1L):(pos + close - 2L)], collapse = "")
      m <- regmatches(body, regexec("^([0-9]+)(,([0-9]*))?$", body))[[1]]
      if (length(m) == 0L) unsupported()
      lo <- as.integer(m[[2]])
      if (is.na(lo) || lo > 255L) unsupported()
      counts <- if (!nzchar(m[[3]])) {
        lo
      } else if (!nzchar(m[[4]])) {
        c(lo, lo + 1L)
      } else {
        hi <- as.integer(m[[4]])
        if (is.na(hi)) unsupported()
        unique(c(lo, min(hi, lo + 1L)))
      }
      pos <<- pos + close
    }
    if (!is.null(counts)) {
      if (peek() == "?") {
        pos <<- pos + 1L # lazy
      } else if (peek() == "+") {
        unsupported() # possessive
      }
    }
    counts
  }

  parse_seq <- function() {
    out <- ""
    while (pos <= n && !(peek() %in% c("|", ")"))) {
      e <- parse_atom()
      counts <- parse_quantifier()
      if (!is.null(counts)) {
        e <- trim(unlist(lapply(counts, function(k) repeat_set(e, k))))
      }
      out <- product(out, e)
    }
    out
  }

  parse_alt <- function() {
    out <- parse_seq()
    while (peek() == "|") {
      pos <<- pos + 1L
      out <- trim(c(out, parse_seq()))
    }
    out
  }

  ex <- tryCatch(
    {
      e <- parse_alt()
      if (pos <= n) unsupported() # a stray ")"
      e
    },
    dta_pattern_unsupported = function(e) character(0)
  )
  ex <- unique(ex[nzchar(ex)])
  if (length(ex) == 0L) {
    return(character(0))
  }
  ok <- tryCatch(stringr::str_detect(ex, pattern), error = function(e) rep(FALSE, length(ex)))
  ex <- ex[!is.na(ok) & ok]
  ex <- ex[order(nchar(ex), ex, method = "radix")]
  utils::head(ex, as.integer(limit))
}

# ---- the check -------------------------------------------------------------------

# Can `g1` and `g2` (container globs; NULL = free) name the same archive?
# Both free counts as equal; free against containered never does.
dta_globs_overlap <- function(g1, g2, memo = new.env(parent = emptyenv())) {
  if (is.null(g1) || is.null(g2)) {
    return(is.null(g1) && is.null(g2))
  }
  if (identical(g1, g2)) {
    return(TRUE)
  }
  key <- paste(sort(c(g1, g2), method = "radix"), collapse = "\r")
  hit <- memo[[key]]
  if (is.null(hit)) {
    r1 <- dta_container_regex(g1)
    r2 <- dta_container_regex(g2)
    hit <- any(stringr::str_detect(dta_pattern_examples(r2), r1)) ||
      any(stringr::str_detect(dta_pattern_examples(r1), r2))
    assign(key, hit, envir = memo)
  }
  hit
}

dta_handler_expected <- function(h) {
  expected <- if (isTRUE(h@pattern) && !is.null(h@pattern_description)) {
    h@pattern_description
  } else {
    h@filename
  }
  paste(expected, collapse = ", ")
}

# The slots of a DTA: one per file entry, with its effective container glob.
dta_conflict_slots <- function(x) {
  containers <- dta_containers(x)
  slots <- list()
  for (ds in x@datasets) {
    for (i in seq_along(ds@files)) {
      h <- ds@files[[i]]
      info <- dta_effective_container(containers, h)
      slots[[length(slots) + 1L]] <- list(
        dataset = ds@id, handler = i, h = h, literal = !isTRUE(h@pattern),
        glob = if (is.null(info)) NULL else info$filename
      )
    }
  }
  slots
}

dta_file_name_conflicts_empty <- function() {
  data.frame(
    dataset_a = character(), entry_a = integer(), expected_a = character(),
    dataset_b = character(), entry_b = integer(), expected_b = character(),
    example = character(), stringsAsFactors = FALSE
  )
}

# `slots`: list(dataset, handler, h, literal, glob) per file entry (glob NULL =
# free). See the algorithm in the plan, section 4.14.
dta_file_name_conflicts_slots <- function(slots) {
  empty <- dta_file_name_conflicts_empty()
  memo <- new.env(parent = emptyenv())
  overlap <- function(g1, g2) dta_globs_overlap(g1, g2, memo)
  pat <- Filter(function(s) !s$literal, slots)
  lit <- Filter(function(s) s$literal, slots)
  if (length(pat) < 2L) {
    return(empty)
  }

  # 1. Scopes: free is one; containered globs merge when they can name one archive.
  globs <- unique(unlist(lapply(pat, function(s) s$glob)))
  group <- seq_along(globs)
  for (i in seq_along(globs)) {
    for (j in seq_len(i - 1L)) {
      if (group[[i]] != group[[j]] && overlap(globs[[i]], globs[[j]])) {
        group[group == group[[i]]] <- group[[j]]
      }
    }
  }
  scope_of <- function(s) if (is.null(s$glob)) 0L else group[[match(s$glob, globs)]]
  scopes <- vapply(pat, scope_of, integer(1))

  parts <- list()
  for (sc in unique(scopes)) {
    members <- pat[scopes == sc]
    if (length(members) < 2L) next
    if (length(unique(vapply(members, function(s) s$dataset, character(1)))) < 2L) next

    # 2. Examples of the scope.
    ex <- character(0)
    for (s in members) {
      per <- unique(unlist(lapply(s$h@filename, dta_pattern_examples)))
      ext <- tryCatch(s$h@extensions, error = function(e) NULL)
      if (!is.null(ext)) per <- c(per, as.vector(outer(per, paste0(".", ext), paste0)))
      ex <- c(ex, per)
    }
    ex <- unique(ex)
    if (length(ex) == 0L) next

    # 3. Literal shadowing.
    scope_globs <- if (sc == 0L) NULL else globs[group == sc]
    for (l in lit) {
      shadows <- if (sc == 0L) {
        TRUE
      } else {
        is.null(l$glob) || any(vapply(scope_globs, function(g) overlap(l$glob, g), logical(1)))
      }
      if (!shadows) next
      taken <- dta_delivery_slot_hits(list(h = l$h, literal = TRUE), ex) &
        dta_entry_takes_kind(l$h, dta_container_kind(ex))
      ex <- ex[!taken]
      if (length(ex) == 0L) break
    }
    if (length(ex) == 0L) next

    # 4. Confirmed matrix: each member's real matcher (extensions included) over
    # the rows its fast hit test passes, once per member rather than per pair.
    ex <- ex[order(nchar(ex), ex, method = "radix")]
    kind <- dta_container_kind(ex)
    conf <- vapply(members, function(s) {
      hit <- dta_delivery_slot_hits(list(h = s$h, literal = FALSE), ex) & dta_entry_takes_kind(s$h, kind)
      hit[hit] <- vapply(ex[hit], function(e) isTRUE(any(matches_filename(s$h, e))), logical(1))
      hit
    }, logical(length(ex)))
    conf <- matrix(conf, nrow = length(ex))
    shared <- crossprod(conf * 1L)
    cand <- which(shared > 0L & upper.tri(shared), arr.ind = TRUE)
    if (nrow(cand) == 0L) next
    ia <- cand[, 1]
    ib <- cand[, 2]
    ds <- vapply(members, function(s) s$dataset, character(1))
    key <- vapply(members, function(s) if (is.null(s$glob)) "\001" else s$glob, character(1))
    keep <- ds[ia] != ds[ib]
    # Equal scopes need no glob test; only distinct glob pairs go to the memo.
    for (k in which(keep & key[ia] != key[ib])) {
      keep[[k]] <- overlap(members[[ia[[k]]]]$glob, members[[ib[[k]]]]$glob)
    }
    ia <- ia[keep]
    ib <- ib[keep]
    if (length(ia) == 0L) next
    # The first confirmed example (ex is ordered shortest first) of each pair.
    first <- integer(length(ia))
    for (a in unique(ia)) {
      sel <- which(ia == a)
      rows_a <- which(conf[, a])
      first[sel] <- rows_a[apply(conf[rows_a, ib[sel], drop = FALSE], 2L, which.max)]
    }
    handler <- vapply(members, function(s) as.integer(s$handler), integer(1))
    expected <- vapply(members, function(s) dta_handler_expected(s$h), character(1))
    rank <- order(order(ds, handler, method = "radix"))
    swap <- rank[ia] > rank[ib]
    lo <- ifelse(swap, ib, ia)
    hi <- ifelse(swap, ia, ib)
    parts[[length(parts) + 1L]] <- data.frame(
      dataset_a = ds[lo], entry_a = handler[lo], expected_a = expected[lo],
      dataset_b = ds[hi], entry_b = handler[hi], expected_b = expected[hi],
      example = ex[first], stringsAsFactors = FALSE
    )
  }
  if (length(parts) == 0L) {
    return(empty)
  }
  out <- do.call(rbind, parts)
  out <- out[order(out$dataset_a, out$entry_a, out$dataset_b, out$entry_b, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The sentence of one conflicting pair; built with sprintf() and handed to cli
# as data by every caller.
dta_file_name_conflict_message <- function(a, expected_a, b, expected_b, example) {
  sprintf(
    "Datasets '%s' (%s) and '%s' (%s) could both take a file named '%s'. Make their file names distinct so every file belongs to one dataset.",
    a, expected_a, b, expected_b, example
  )
}

#' @title File-Name Conflicts of a DTA
#' @description
#' Finds file entries of different datasets that could take one and the same
#' file. An agreement in which a file name could belong to two datasets is
#' ambiguous, so \code{lint_dta()} reports such a pair as
#' \code{file_name_conflict} and \code{validate_template()} refuses it.
#'
#' Two pattern entries conflict when a file name exists that both take, and
#' that is not routed to a literal entry first, while both are free of a
#' container or both belong to containers whose names can denote the same
#' archive. The search is sound but not complete: it works from example names
#' generated out of each pattern, so every reported conflict comes with a real
#' file name, but overlaps that no generated example shows (look-arounds,
#' back-references, inline flags) are not found. Two literal entries of the
#' same name are not a conflict: the file is routed to both on purpose.
#' @param x A \code{\link{DTA}} object.
#' @return A data frame with one row per conflicting pair and the columns
#'   \code{dataset_a}, \code{entry_a}, \code{expected_a}, \code{dataset_b},
#'   \code{entry_b}, \code{expected_b} (the datasets' ids, the entries' indices
#'   and their file names or pattern descriptions) and \code{example}, a file
#'   name both entries take. Zero rows when there is no conflict.
#' @examples
#' ds <- function(id, pattern) {
#'   DTADataSetTabular(
#'     id = id,
#'     specs = create_example_DTAColumnSpecCollection(1),
#'     files = list(DTAFileCSV(pattern, pattern = TRUE))
#'   )
#' }
#' dta <- DTA(
#'   datasets = list(ds("ae", "^a.*[.]csv$"), ds("dm", "^.*e[.]csv$")),
#'   metadata = create_example_DTAMetaData()
#' )
#' file_name_conflicts(dta)
#' @export
file_name_conflicts <- function(x) {
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("{.arg x} must be a {.cls DTA} object.")
  }
  if (dta_is_stale(x)) x <- dta_upgrade(x)
  dta_file_name_conflicts_slots(dta_conflict_slots(x))
}

# The file_name_conflict lint rows of a slot list (zero rows when none).
dta_lint_file_name_conflicts <- function(slots) {
  conflicts <- dta_file_name_conflicts_slots(slots)
  if (nrow(conflicts) == 0L) {
    return(dta_lint_empty())
  }
  msg <- mapply(
    dta_file_name_conflict_message, conflicts$dataset_a, conflicts$expected_a,
    conflicts$dataset_b, conflicts$expected_b, conflicts$example,
    USE.NAMES = FALSE
  )
  data.frame(
    dataset = conflicts$dataset_a,
    object = "file",
    id = sprintf("%s[%d] / %s[%d]", conflicts$dataset_a, conflicts$entry_a, conflicts$dataset_b, conflicts$entry_b),
    severity = "warning", code = "file_name_conflict", message = msg,
    stringsAsFactors = FALSE
  )
}
