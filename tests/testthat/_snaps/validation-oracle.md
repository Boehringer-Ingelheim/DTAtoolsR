# axis verdicts and error counts are stable across the corpus

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["case", "ok", "schema_valid", "rules_valid", "import_valid", "n_schema_errors", "n_rule_errors", "n_import_errors"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["clean", "schema_required", "schema_type", "schema_maxlength", "schema_maxlength_unicode", "schema_enum", "schema_const", "schema_pattern", "schema_nullable", "rule_range", "rule_unique", "rule_unique_na", "rule_condition", "rule_group_exclusive", "rule_group_requires", "import_unconvertible", "all_axes"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, false, false, false, false, false, false, false, false, true, true, true, true, true, true, false, false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 3, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1]
        }
      ]
    }

# each error is attributed to a stable source, row and column

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["case", "source", "rule_id", "row", "column", "keyword"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["schema_required", "schema_required", "schema_type", "schema_type", "schema_maxlength", "schema_maxlength_unicode", "schema_enum", "schema_const", "schema_pattern", "schema_nullable", "rule_range", "rule_unique", "rule_unique_na", "rule_condition", "rule_group_exclusive", "rule_group_requires", "import_unconvertible", "import_unconvertible", "import_unconvertible", "import_unconvertible", "import_unconvertible", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["schema", "schema", "schema", "schema", "schema", "schema", "schema", "schema", "schema", "schema", "rule", "rule", "rule", "rule", "rule", "rule", "schema", "schema", "schema", "rule", "import", "schema", "schema", "schema", "schema", "schema", "rule", "rule", "import"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, null, null, null, null, null, null, null, null, "age_range", "subj_visit", "k_unique", "adult_status", "grp_exclusive", "grp_requires", null, null, null, "val_range", null, null, null, null, null, null, "age_range", "id_unique", null]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 1, 2, 2, 3, 2, 2, 2, 2, "NA", "NA", "NA", "NA", "NA", "NA", 1, 2, 3, "NA", 2, 1, 2, 2, 3, 3, "NA", "NA", 2]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "AGE", "AGE", "ID", "NAME", "SEX", "DOMAIN", "CODE", "ID", null, null, null, null, null, null, "VAL", "VAL", "VAL", null, "VAL", "AGE", "SEX", "AGE", "ID", "AGE", null, null, "AGE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["required", "required", "type", "type", "maxLength", "maxLength", "enum", "const", "pattern", "type", null, null, null, null, null, null, "type", "type", "type", null, "not_convertible", "type", "enum", "type", "maxLength", "type", null, null, "not_convertible"]
        }
      ]
    }

# the flattened validation report is unchanged

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["case", "source", "rule_id", "row", "column", "keyword", "message"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["schema_required", "schema_required", "schema_type", "schema_type", "schema_maxlength", "schema_maxlength_unicode", "schema_enum", "schema_const", "schema_pattern", "schema_nullable", "rule_range", "rule_unique", "rule_unique_na", "rule_condition", "rule_group_exclusive", "rule_group_requires", "import_unconvertible", "import_unconvertible", "import_unconvertible", "import_unconvertible", "import_unconvertible", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes", "all_axes"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["schema", "schema", "schema", "schema", "schema", "schema", "schema", "schema", "schema", "schema", "rule", "rule", "rule", "rule", "rule", "rule", "schema", "schema", "schema", "rule", "import", "schema", "schema", "schema", "schema", "schema", "rule", "rule", "import"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, null, null, null, null, null, null, null, null, "age_range", "subj_visit", "k_unique", "adult_status", "grp_exclusive", "grp_requires", null, null, null, "val_range", null, null, null, null, null, null, "age_range", "id_unique", null]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 1, 2, 2, 3, 2, 2, 2, 2, "NA", "NA", "NA", "NA", "NA", "NA", 1, 2, 3, "NA", 2, 1, 2, 2, 3, 3, "NA", "NA", 2]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, "AGE", "AGE", "ID", "NAME", "SEX", "DOMAIN", "CODE", "ID", null, null, null, null, null, null, "VAL", "VAL", "VAL", null, "VAL", "AGE", "SEX", "AGE", "ID", "AGE", null, null, "AGE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["required", "required", "type", "type", "maxLength", "maxLength", "enum", "const", "pattern", "type", null, null, null, null, null, null, "type", "type", "type", null, "not_convertible", "type", "enum", "type", "maxLength", "type", null, null, "not_convertible"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["must have required property 'MISSING'", "must have required property 'MISSING'", "must be number", "must be number", "must NOT have more than 4 characters", "must NOT have more than 5 characters", "must be equal to one of the allowed values", "must be equal to constant", "must match pattern \"^[A-Z]{3}[0-9]{3}$\"", "must be string", "Rule 'age_range' violated: 2 rows where AGE not in range [18, 70]", "Rule 'subj_visit' violated: 1 duplicate row found when selecting column(s): SUBJ, VISIT", "Rule 'k_unique' violated: 1 duplicate row found when selecting column(s): K", "Rule 'adult_status' violated: 1 rows failed the THEN conditions after meeting the IF conditions.", "Rule 'grp_exclusive' failed: 1 grouped constraint violation detected. Constraint 'constraint_1' failed: 'failed' (scope=any; rows=1) and 'reported' (scope=any; rows=2) are both TRUE, but mutually_exclusive requires they cannot both hold. [SUBJ=A]", "Rule 'grp_requires' failed: 1 grouped constraint violation detected. Constraint 'constraint_1' failed: IF condition 'failed' (scope=any; rows=1) is TRUE, but THEN condition 'not_done' (scope=any) is not satisfied (no row in the group satisfied 'not_done' (rows with TRUE=none)). [SUBJ=A]", "must be number,null", "must be number,null", "must be number,null", "Rule 'val_range' violated: 1 rows where VAL not in range [0, 100]", "value 'abc' in column 'VAL' cannot be represented as declared type 'SAS Num' (not_convertible); imported as NA", "must be number,null", "must be equal to one of the allowed values", "must be number,null", "must NOT have more than 4 characters", "must be number,null", "Rule 'age_range' violated: 2 rows where AGE not in range [18, 70]", "Rule 'id_unique' violated: 1 duplicate row found when selecting column(s): ID", "value 'abc' in column 'AGE' cannot be represented as declared type 'SAS Num' (not_convertible); imported as NA"]
        }
      ]
    }

# the read and import-typing path produces stable verdicts

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["case", "read_ok", "ok", "schema_valid", "rules_valid", "import_valid", "n_schema_errors", "n_rule_errors", "n_import_errors"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["clean", "schema_required", "schema_type", "schema_maxlength", "schema_maxlength_unicode", "schema_enum", "schema_const", "schema_pattern", "schema_nullable", "rule_range", "rule_unique", "rule_unique_na", "rule_condition", "rule_group_exclusive", "rule_group_requires", "import_unconvertible", "all_axes"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, false, false, false, false, false, false, false, true, false, false, true, false, false, false, false, false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, true, true, true, true, true, true, true, true, false, false, true, false, false, false, true, false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, false, false]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 2]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1]
        }
      ]
    }

