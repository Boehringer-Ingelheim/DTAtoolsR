#!/usr/bin/env Rscript
# Replacement for precommit's `deps-in-desc` hook.
#
# What it checks: every `pkg::fun()` / `pkg:::fun()` call under R/ names a
# package declared in DESCRIPTION (Depends, Imports, or Suggests). That is
# the direction that has actually bitten this project -- code calling a
# namespaced function from a package DESCRIPTION doesn't know about, which
# breaks a fresh install or R CMD check on a machine that doesn't already
# happen to have that package on its library path.
#
# What it deliberately does NOT check (unlike the original hook):
#   - packages declared in DESCRIPTION but never referenced (dead
#     dependencies). Harmless to miss; at worst it costs an unnecessary
#     install.
#   - non-`::` uses, e.g. a bare `filter()` after `library(dplyr)`, or
#     re-exported/imported-into-NAMESPACE functions used unqualified.
#     This project's convention (CLAUDE.md) is to always call dependencies
#     namespaced, so that gap is not expected to matter in practice.
#   - whether Imports vs Suggests is the *correct* field for a given call
#     (e.g. a Suggests package used unconditionally, without
#     requireNamespace()). Only "is it declared somewhere" is verified.
#
# Uses getParseData() rather than a text regex so that `pkg::fun` occurring
# inside a comment (roxygen markdown links like [shiny::runApp()]) or inside
# a string literal (S7 class-name strings such as "DTAtools::DTARule") is
# never mistaken for a real namespaced call -- R's own tokenizer marks only
# the actual `::`/`:::` operator's left-hand side as SYMBOL_PACKAGE.

r_dir <- "R"
desc_path <- "DESCRIPTION"

read_dcf_field_packages <- function(path, fields) {
  dcf <- read.dcf(path, fields = fields)[1, ]
  dcf <- dcf[!is.na(dcf)]
  if (length(dcf) == 0) {
    return(character())
  }
  raw <- unlist(strsplit(dcf, ","))
  raw <- trimws(raw)
  raw <- gsub("\\s*\\(.*\\)\\s*$", "", raw) # drop version constraints, e.g. "R (>= 4.1.0)"
  raw[nzchar(raw)]
}

self_pkg <- read.dcf(desc_path, fields = "Package")[1, 1]
declared <- unique(read_dcf_field_packages(
  desc_path,
  c("Depends", "Imports", "Suggests", "LinkingTo")
))
# A package never declares itself as its own dependency, but S7 methods and
# helpers in this codebase legitimately call `DTAtools::fun()` (needed for S7
# dispatch / class-name qualification). Treat self-reference as always fine.
declared <- union(declared, self_pkg)
# Base and its always-attached friends ship with every R install and are
# never listed in DESCRIPTION.
base_pkgs <- rownames(installed.packages(priority = "base"))
declared <- union(declared, base_pkgs)

r_files <- sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))

undeclared <- data.frame(file = character(), line = integer(), package = character())

for (f in r_files) {
  parsed <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) {
    # Reported by check_r_sanity.R; do not double-report here.
    next
  }
  pd <- utils::getParseData(parsed)
  pkg_rows <- pd[pd$token == "SYMBOL_PACKAGE", c("text", "line1")]
  for (i in seq_len(nrow(pkg_rows))) {
    pkg <- pkg_rows$text[i]
    if (!pkg %in% declared) {
      undeclared <- rbind(undeclared, data.frame(
        file = f, line = pkg_rows$line1[i], package = pkg
      ))
    }
  }
}

if (nrow(undeclared) > 0) {
  undeclared <- unique(undeclared)
  cat("Namespaced calls to packages not declared in DESCRIPTION:\n\n")
  for (i in seq_len(nrow(undeclared))) {
    cat(sprintf(
      "  %s:%d  %s::\n", undeclared$file[i], undeclared$line[i], undeclared$package[i]
    ))
  }
  cat("\nDeclare the package under Imports, Depends, or Suggests in DESCRIPTION.\n")
  quit(status = 1)
}

cat("check_deps_in_desc.R: OK -- every R/ namespaced call is declared in DESCRIPTION.\n")
