# Fixture corpus for the validation-engine rewrite (P0).
#
# Purpose: pin the behaviour of the CURRENT validation engine so the Arrow
# rewrite can be proved equivalent rather than assumed equivalent. Each case
# isolates one check so a drift in the replacement names itself instead of
# showing up as one opaque failure in a combined fixture.
#
# The cases are deliberately tiny. Volume is the benchmark's job
# (benchmarks/bench_validation.R); this corpus is about coverage of *kinds*.

# ---- construction helpers ---------------------------------------------------

vc_specs <- function(cols, rules = list()) {
  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = rules
  )
}

vc_case <- function(label, axis, specs, table) {
  list(label = label, axis = axis, specs = specs, table = table)
}

# ---- the corpus -------------------------------------------------------------

# Each entry: one construct under test. `axis` records which axis the case is
# aimed at, so a test can report drift per axis rather than per case.
vc_corpus <- function() {
  list(
    clean = vc_case(
      "all constraints satisfied",
      "none",
      vc_specs(list(
        DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
        DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
      )),
      data.frame(
        ID = c("A001", "A002"),
        AGE = c(30, 40),
        stringsAsFactors = FALSE
      )
    ),

    # --- column spec axis ---------------------------------------------------------

    columnspec_required = vc_case(
      "declared column absent from the table",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
        DTAColumnSpec(id = "MISSING", type = "SAS Char", length = 4, nullable = FALSE)
      )),
      data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)
    ),
    columnspec_type = vc_case(
      "text where the spec declares a number",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = FALSE)
      )),
      data.frame(AGE = c("30", "not-a-number"), stringsAsFactors = FALSE)
    ),
    columnspec_maxlength = vc_case(
      "string longer than the declared length",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
      )),
      data.frame(ID = c("A001", "TOO-LONG"), stringsAsFactors = FALSE)
    ),

    # Pins the "maxLength counts characters, not bytes" decision. "aeiou" with
    # combining marks is 5 characters but more than 5 bytes in UTF-8, so a
    # byte-counting implementation would wrongly flag row 2.
    columnspec_maxlength_unicode = vc_case(
      "multi-byte characters counted as characters, not bytes",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "NAME", type = "SAS Char", length = 5, nullable = FALSE)
      )),
      data.frame(
        NAME = c("abcde", "äöüßé", "äöüßéx"),
        stringsAsFactors = FALSE
      )
    ),
    columnspec_enum = vc_case(
      "value outside the declared codelist",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(
          id = "SEX", type = "SAS Char", length = 1,
          nullable = FALSE, values = c("M", "F")
        )
      )),
      data.frame(SEX = c("M", "X"), stringsAsFactors = FALSE)
    ),
    columnspec_const = vc_case(
      "value differs from the single permitted value",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(
          id = "DOMAIN", type = "SAS Char", length = 2,
          nullable = FALSE, values = "GF"
        )
      )),
      data.frame(DOMAIN = c("GF", "ZZ"), stringsAsFactors = FALSE)
    ),
    columnspec_pattern = vc_case(
      "value not matching the declared pattern",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(
          id = "CODE", type = "SAS Char", length = 6,
          nullable = FALSE, pattern = "^[A-Z]{3}[0-9]{3}$"
        )
      )),
      data.frame(CODE = c("ABC123", "bad!!!"), stringsAsFactors = FALSE)
    ),
    # The second column is load-bearing, not padding. With ID alone, the row
    # whose only value is NA serialises to a blank line, which every CSV parser
    # skips - so the round-trip layer would silently test blank-line handling
    # rather than nullability. SITE keeps the line populated.
    columnspec_nullable = vc_case(
      "missing value in a non-nullable column",
      "columnspec",
      vc_specs(list(
        DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
        DTAColumnSpec(id = "SITE", type = "SAS Char", length = 4, nullable = FALSE)
      )),
      data.frame(
        ID = c("A001", NA_character_),
        SITE = c("S01", "S02"),
        stringsAsFactors = FALSE
      )
    ),

    # --- rules axis ----------------------------------------------------------

    rule_range = vc_case(
      "values outside an inclusive numeric range",
      "rule",
      vc_specs(
        list(DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)),
        list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
      ),
      data.frame(AGE = c(18, 70, 17, 71), stringsAsFactors = FALSE)
    ),
    rule_unique = vc_case(
      "duplicate rows on a composite key",
      "rule",
      vc_specs(
        list(
          DTAColumnSpec(id = "SUBJ", type = "SAS Char", length = 8, nullable = FALSE),
          DTAColumnSpec(id = "VISIT", type = "SAS Char", length = 8, nullable = FALSE)
        ),
        list(DTARuleColUnique(id = "subj_visit", columns = c("SUBJ", "VISIT")))
      ),
      data.frame(
        SUBJ = c("A", "A", "B"),
        VISIT = c("V1", "V1", "V1"),
        stringsAsFactors = FALSE
      )
    ),

    # Pins duplicated()'s treatment of repeated NAs as duplicates. Arrow's
    # distinct counting does not agree by default, so this case is the tripwire.
    # SITE exists for the same reason as in columnspec_nullable: without it the two
    # NA rows are blank lines and never survive the round trip. The uniqueness
    # rule reads K only, so SITE does not affect the key.
    rule_unique_na = vc_case(
      "repeated missing values in the uniqueness key",
      "rule",
      vc_specs(
        list(
          DTAColumnSpec(id = "K", type = "SAS Char", length = 8, nullable = TRUE),
          DTAColumnSpec(id = "SITE", type = "SAS Char", length = 4, nullable = FALSE)
        ),
        list(DTARuleColUnique(id = "k_unique", columns = "K"))
      ),
      data.frame(
        K = c("a", NA_character_, NA_character_),
        SITE = c("S01", "S02", "S03"),
        stringsAsFactors = FALSE
      )
    ),
    rule_condition = vc_case(
      "IF matches but THEN fails",
      "rule",
      vc_specs(
        list(
          DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = FALSE),
          DTAColumnSpec(id = "STATUS", type = "SAS Char", length = 8, nullable = FALSE)
        ),
        list(DTARuleColCondition(
          id = "adult_status",
          condition = list(AGE = list(greater_equal = 18)),
          then = list(STATUS = list(equals = "OK"))
        ))
      ),
      data.frame(
        AGE = c(20, 20, 10),
        STATUS = c("OK", "BAD", "BAD"),
        stringsAsFactors = FALSE
      )
    ),
    rule_group_exclusive = vc_case(
      "two mutually exclusive conditions both hold in one group",
      "rule",
      vc_specs(
        list(
          DTAColumnSpec(id = "SUBJ", type = "SAS Char", length = 8, nullable = FALSE),
          DTAColumnSpec(id = "REASND", type = "SAS Char", length = 12, nullable = TRUE),
          DTAColumnSpec(id = "ORRES", type = "SAS Char", length = 12, nullable = TRUE)
        ),
        list(DTARuleGroupCondition(
          id = "grp_exclusive",
          group_by = "SUBJ",
          conditions = list(
            failed = list(REASND = list(empty = FALSE)),
            reported = list(REASND = list(empty = TRUE), ORRES = list(empty = FALSE))
          ),
          constraints = list(
            list(type = "mutually_exclusive", left = "failed", right = "reported")
          )
        ))
      ),
      data.frame(
        SUBJ = c("A", "A", "B"),
        REASND = c("BROKEN", NA_character_, NA_character_),
        ORRES = c(NA_character_, "12", "13"),
        stringsAsFactors = FALSE
      )
    ),
    rule_group_requires = vc_case(
      "an implication whose consequent never holds in the group",
      "rule",
      vc_specs(
        list(
          DTAColumnSpec(id = "SUBJ", type = "SAS Char", length = 8, nullable = FALSE),
          DTAColumnSpec(id = "REASND", type = "SAS Char", length = 12, nullable = TRUE),
          DTAColumnSpec(id = "STAT", type = "SAS Char", length = 12, nullable = TRUE)
        ),
        list(DTARuleGroupCondition(
          id = "grp_requires",
          group_by = "SUBJ",
          conditions = list(
            failed = list(REASND = list(empty = FALSE)),
            not_done = list(STAT = list(equals = "NOT DONE"))
          ),
          constraints = list(
            list(type = "requires", `if` = "failed", then = "not_done")
          )
        ))
      ),
      data.frame(
        SUBJ = c("A", "B"),
        REASND = c("BROKEN", NA_character_),
        STAT = c("DONE", "DONE"),
        stringsAsFactors = FALSE
      )
    ),

    # --- import axis ---------------------------------------------------------

    import_unconvertible = vc_case(
      "unrepresentable values in a column a numeric rule reads",
      "import",
      vc_specs(
        list(DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)),
        list(DTARuleColRange(id = "val_range", columns = "VAL", range = c(0, 100)))
      ),
      data.frame(VAL = c("10", "abc", "", NA_character_), stringsAsFactors = FALSE)
    ),

    # --- combined ------------------------------------------------------------

    all_axes = vc_case(
      "column spec, rule and import failures in one table",
      "all",
      vc_specs(
        list(
          DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
          DTAColumnSpec(
            id = "SEX", type = "SAS Char", length = 1,
            nullable = FALSE, values = c("M", "F")
          ),
          DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
        ),
        list(
          DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)),
          DTARuleColUnique(id = "id_unique", columns = "ID")
        )
      ),
      data.frame(
        ID = c("A001", "A001", "TOO-LONG"),
        SEX = c("M", "X", "F"),
        AGE = c("30", "abc", "99"),
        stringsAsFactors = FALSE
      )
    )
  )
}

# ---- strict-numeric semantics matrix ----------------------------------------

# The highest drift risk in the rewrite. `dta_as_numeric_strict()` draws a
# three-way distinction between missing, unconvertible and valid, and several
# of these outcomes are surprising enough that an Arrow reimplementation would
# plausibly get them wrong without noticing. Each row states the CURRENT
# behaviour; where that behaviour is arguably wrong the comment says so rather
# than quietly blessing it.
vc_numeric_edges <- function() {
  data.frame(
    input = c(
      "42",
      " 42 ",
      "-3.5",
      "1e3",
      "0x10",
      "Inf",
      "NaN",
      "",
      "   ",
      NA_character_,
      "abc",
      "1,5",
      "TRUE"
    ),
    # expected value after coercion (NA where not representable)
    value = c(42, 42, -3.5, 1000, 16, Inf, NaN, NA, NA, NA, NA, NA, NA),
    missing = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE,
      FALSE, FALSE, FALSE
    ),
    unconvertible = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      # "NaN" parses to NaN, and is.na(NaN) is TRUE, so the current
      # implementation classifies it as unconvertible. Questionable, but it is
      # the behaviour on record and the rewrite must not change it silently.
      TRUE,
      FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE
    ),
    stringsAsFactors = FALSE
  )
}
