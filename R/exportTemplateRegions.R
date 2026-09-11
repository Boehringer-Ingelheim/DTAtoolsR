#' Brace-delimited markers that open and close a repeating region
#' @keywords internal
.tv_region_markers <- function() {
  c(open = "{#DATASETS}", close = "{/DATASETS}")
}

#' Is this brace token a region marker (open or close)?
#' @keywords internal
.tv_is_region_marker <- function(token) token %in% .tv_region_markers()

#' Region markers, with a one-line description, for the placeholder catalogue
#' @keywords internal
.tv_region_catalog <- function() {
  c(
    "{#DATASETS}" = "Start of a region repeated once per dataset",
    "{/DATASETS}" = "End of a {#DATASETS} region"
  )
}

#' The six per-dataset placeholders usable inside a repeating region, with a
#' one-line description
#'
#' The single source of truth for which per-dataset placeholders exist.
#' [.tv_dataset_variables()] must produce exactly these names, and a test
#' asserts that it does -- otherwise the documented set and the implemented
#' set drift apart silently, which is the failure this catalogue exists to
#' prevent.
#' @keywords internal
.tv_dataset_catalog <- function() {
  c(
    "{DATASET_NAME}" = "Name of the current dataset",
    "{DATASET_TYPE}" = "Type of the current dataset",
    "{DATASET_DESCRIPTION}" = "Description of the current dataset",
    "{DATASET_FILE_COUNT}" = "Number of expected files of the current dataset",
    "{DATASET_COLUMN_COUNT}" = "Number of columns of the current dataset (0 unless tabular)",
    "{DATASET_RULE_COUNT}" = "Number of validation rules of the current dataset (0 unless tabular)"
  )
}

#' Resolve the six per-dataset placeholders for one dataset
#'
#' @param ds A `DTADataSet` (or subclass) object.
#' @param name Character. The dataset's name -- its key in `dta@datasets`.
#' @return A named list keyed exactly like [.tv_dataset_catalog()].
#' @keywords internal
.tv_dataset_variables <- function(ds, name) {
  is_tabular <- inherits(ds, "DTAtools::DTADataSetTabular")
  column_count <- if (is_tabular) length(ds@specs@columns) else 0L
  rule_count <- if (is_tabular) {
    rules <- ds@specs@rules
    if (is.null(rules)) 0L else length(rules)
  } else {
    0L
  }

  list(
    "{DATASET_NAME}" = name,
    "{DATASET_TYPE}" = .tv_scalar(ds@type),
    "{DATASET_DESCRIPTION}" = .tv_scalar(ds@description),
    "{DATASET_FILE_COUNT}" = as.character(length(ds@files)),
    "{DATASET_COLUMN_COUNT}" = as.character(column_count),
    "{DATASET_RULE_COUNT}" = as.character(rule_count)
  )
}

#' Bind a bare dataset-block paragraph inside a region copy to one dataset
#'
#' A dataset block (`{COLUMN_SPECS}`, `{VALIDATION_RULES}`, `{FILE_SPECS}`,
#' `{DATASETS}`/`{DATASETS_DETAIL}`) sitting alone in a paragraph of a region
#' copy is rewritten here to name the current dataset explicitly -- e.g.
#' `{COLUMN_SPECS}` becomes `{COLUMN_SPECS:demographics}` -- so
#' [.tv_render_blocks()] renders that one dataset instead of every dataset in
#' the DTA. An explicit `{COLUMN_SPECS:vitals}` is left untouched: the caller
#' already named a dataset, and rebinding it would silently override that
#' choice. A token the caller supplied a `variables` value for is left alone
#' too, for the same reason [.tv_mark_block_paragraphs()] leaves one alone: a
#' user-supplied value wins over any block.
#'
#' Uses its own sentinel family (`.tv_block_sentinel(nonce, paste0("R", k))`)
#' so a region-bound block can never collide with a sentinel
#' [.tv_mark_block_paragraphs()] stamps later, in the same document.
#'
#' @param p An `xml_node` for a body paragraph -- already a copy, inside one
#'   dataset's repetition of the region.
#' @param name Character. The current dataset's name.
#' @param nonce Character. This export's nonce, from [.tv_block_nonce()].
#' @param k Integer. This region's running sentinel counter.
#' @param variables Named list of caller-supplied placeholder values.
#' @return A named character vector of length 0 or 1: sentinel to
#'   `"{NAME:dataset}"`.
#' @keywords internal
.tv_bind_region_block <- function(p, name, nonce, k, variables) {
  if (!identical(xml2::xml_name(p), "p")) {
    return(character(0))
  }
  token <- trimws(xml2::xml_text(p))
  spec <- .tv_parse_block_token(token)
  if (
    is.null(spec) ||
      !is.null(spec$arg) ||
      !spec$name %in% .tv_block_takes_arg() ||
      token %in% names(variables)
  ) {
    return(character(0))
  }

  sentinel <- .tv_block_sentinel(nonce, paste0("R", k))
  if (!.tv_stamp_paragraph(p, sentinel)) {
    return(character(0))
  }
  stats::setNames(
    paste0(sub("\\}$", "", spec$name), ":", name, "}"),
    sentinel
  )
}

#' Give every drawing inside a region copy a fresh docPr id
#'
#' Word rejects a document with duplicate `wp:docPr/@id` values. A region copy
#' made with [xml2::xml_add_sibling()] duplicates whatever ids the original
#' drawing (typically an inline image) carried, so each copy needs ids that do
#' not collide with any other `docPr` already in the document -- including
#' ids handed out to an earlier copy of the same region.
#'
#' Only drawings are renumbered. A bookmark (`w:bookmarkStart` /
#' `w:bookmarkEnd`), footnote reference or comment reference inside a region
#' is copied verbatim, id included, in every repetition; renumbering those --
#' bookmarks in matched pairs, with unique names -- is a follow-up for the day
#' a template needs it.
#'
#' @param node The just-created copy (an `xml_node`), already attached to `doc`.
#' @param doc The whole document (`xml_document`), searched for the current
#'   maximum id.
#' @return Invisibly, `node`.
#' @keywords internal
.tv_renumber_drawings <- function(node, doc) {
  targets <- xml2::xml_find_all(node, ".//*[local-name()='docPr']")
  if (length(targets) == 0) {
    return(invisible(node))
  }

  all_ids <- suppressWarnings(as.integer(
    xml2::xml_attr(xml2::xml_find_all(doc, ".//*[local-name()='docPr']"), "id")
  ))
  next_id <- max(c(0L, all_ids[!is.na(all_ids)])) + 1L
  for (t in targets) {
    xml2::xml_set_attr(t, "id", as.character(next_id))
    next_id <- next_id + 1L
  }
  invisible(node)
}

#' Expand `{#DATASETS}` ... `{/DATASETS}` repeating regions in a document part
#'
#' Runs as "pass 0", before [.tv_mark_block_paragraphs()], directly on the
#' template's `word/document.xml`. A region is opened by a top-level body
#' paragraph whose whole (trimmed) text is `{#DATASETS}` and closed by the
#' next top-level body paragraph whose whole text is `{/DATASETS}`; every body
#' child strictly between the two -- paragraph or table -- is copied once per
#' dataset in `names(dta@datasets)`, in document order, with the six
#' per-dataset placeholders ([.tv_dataset_variables()]) resolved inside each
#' copy and a bare dataset block bound to that dataset
#' ([.tv_bind_region_block()]). The two marker paragraphs and the original
#' region content are then removed, so zero datasets makes the whole region
#' vanish.
#'
#' This must run before block-paragraph stamping: a bound dataset block
#' inside a region needs its own paragraph -- freshly copied per dataset --
#' stamped with its own sentinel; stamping the template's un-copied paragraph
#' first would apply to a paragraph this pass is about to multiply or delete.
#'
#' Regions do not nest. An inner `{#DATASETS}` paragraph is ordinary region
#' content: it is copied, left as literal text, and reported by the "block
#' placeholders were left unchanged" warning like any other malformed marker
#' (an unclosed open, an unopened close, or a marker not alone in a
#' paragraph) -- this function only ever pairs an open marker with the FIRST
#' close marker after it.
#'
#' @param xml_path Character. Path to the template's `word/document.xml`.
#' @param dta A [DTA] object.
#' @param nonce Character. This export's nonce, from [.tv_block_nonce()].
#' @param variables Named list of caller-supplied placeholder values.
#' @return A named character vector, sentinel to `"{NAME:dataset}"`, for every
#'   dataset block a region copy bound -- to be added to the `blocks`
#'   argument of [.tv_render_blocks()]. Empty when nothing was expanded.
#' @keywords internal
.tv_expand_regions <- function(xml_path, dta, nonce, variables = list()) {
  doc <- tryCatch(xml2::read_xml(xml_path), error = function(e) NULL)
  if (is.null(doc)) {
    return(character(0))
  }
  body <- xml2::xml_find_first(
    doc,
    "/*[local-name()='document']/*[local-name()='body']"
  )
  if (inherits(body, "xml_missing")) {
    return(character(0))
  }

  markers <- .tv_region_markers()
  marker_of <- function(node) {
    if (identical(xml2::xml_name(node), "p")) trimws(xml2::xml_text(node)) else ""
  }

  found <- character(0)
  changed <- FALSE
  from <- 1L
  repeat {
    kids <- xml2::xml_children(body)
    texts <- vapply(seq_along(kids), function(j) marker_of(kids[[j]]), character(1))

    open_idx <- which(texts == markers[["open"]])
    open_idx <- open_idx[open_idx >= from]
    if (length(open_idx) == 0) {
      break
    }
    i <- open_idx[[1]]

    close_idx <- which(texts == markers[["close"]])
    close_idx <- close_idx[close_idx > i]
    if (length(close_idx) == 0) {
      break
    }
    j <- close_idx[[1]]

    region <- if (j > i + 1L) kids[(i + 1L):(j - 1L)] else NULL
    close <- kids[[j]]
    ds_names <- names(dta@datasets)

    for (nm in ds_names) {
      ds_vars <- .tv_dataset_variables(dta@datasets[[nm]], nm)
      ds_vars <- ds_vars[!names(ds_vars) %in% names(variables)]
      if (is.null(region)) {
        next
      }
      for (node in region) {
        xml2::xml_add_sibling(close, node, .where = "before")
        copy <- xml2::xml_find_first(close, "preceding-sibling::*[1]")
        found <- c(found, .tv_bind_region_block(copy, nm, nonce, length(found) + 1L, variables))
        .tv_substitute_paragraphs(
          xml2::xml_find_all(copy, "descendant-or-self::*[local-name()='p']"),
          ds_vars
        )
        .tv_renumber_drawings(copy, doc)
      }
    }

    xml2::xml_remove(kids[[i]])
    if (!is.null(region)) {
      xml2::xml_remove(region)
    }
    xml2::xml_remove(close)

    from <- i + length(region) * length(ds_names)
    changed <- TRUE
  }

  if (changed) {
    xml2::write_xml(doc, xml_path)
  }
  found
}
