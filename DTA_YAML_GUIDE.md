# Writing a DTAtools Data Transfer Specification (YAML)

Self-contained authoring reference (DTAtools, `dev` branch, Nov 2026). A
specification is **one YAML file** that is both the human-readable Data
Transfer Agreement (DTA) document and the machine contract that every
delivered file is validated against. You will not have the package, its docs or
its website: everything you need is here. Follow it literally — the parser
rejects unknown keys (an unknown top-level key is flagged by the linter).

**Your goal is a spec that would catch real data errors and that a reviewer
could sign.** That means: every column precisely typed and constrained, rules
that encode the actual domain logic, and metadata filled in as completely as
the information you have allows.

---

## 1. File skeleton

```yaml
metadata:   # agreement-level information (document header, parties, transfer)
  ...
containers: # optional: the .zip / .xpt archives the files arrive in
  - id: ...
    ...
datasets:   # one or more deliverables; each is validated independently
  - id: ...
    ...
```

Only these three top-level keys exist. `metadata` is technically optional but if
present `title` and `version` are **required** — always include it.
`containers` is needed only when files arrive in archives (see "`containers`"
at the end of section 2).

---

## 2. `metadata`

The key set is **closed**: any key not listed here is a hard error.

| Key | Type | Notes |
| --- | --- | --- |
| `title` | string, **required**, non-empty | Full agreement title. |
| `version` | string, **required**, non-empty | **Quote it**: `"1.0"` (unquoted `1.0` becomes a number). |
| `date` | ISO date `YYYY-MM-DD` | Date of this version. Anything else (a phrase, a date + words) is flagged as an import error. |
| `header` | string | Organisation / header text printed on the document. |
| `study` | mapping | Only `id` and/or `protocol_id`, each a non-blank string. No other keys. |
| `version_history` | list of records | Each record **must** have `version` (quoted string), `date` (ISO), `changes` (non-empty text). Oldest first; last entry = current `version`. |
| `receiver` | mapping | Party receiving the data. See parties below. |
| `supplier` | mapping | Party delivering the data. |
| `transmission` | mapping | How/when data moves. See below. |
| `error_handling` | string | Procedure when delivered data fails validation (who is notified, deadlines, re-delivery). |
| `authorized_for_corrections` | string or list of strings | Names/roles allowed to request corrections. |
| `sections` | mapping | Ordered free-text agreement sections. See below. |
| `dtatools_version` | string, machine-written | The DTAtools release that last saved the file, e.g. `"0.29.0"`. The app overwrites it on every save; leave it out when writing a file by hand. |

Never write `template:` — it is machine-owned provenance. `dtatools_version:` is
machine-written too; if you do write it, it must be a bare version number.

### Parties (`receiver`, `supplier`)

```yaml
receiver:
  affiliation:
    name: Example Pharma GmbH        # name, address, country
    address: Example Str. 1, 55216 Ingelheim
    country: Germany
  contacts:
    - name: Jane Doe
      role: Lead Data Manager
      email: jane.doe@example.com
      phone: "+49 6132 000000"       # quote phone numbers
      department: Data Management
      address: ...                   # optional
      company: ...                   # optional, if differs from affiliation
      signature: true                # signs the agreement
      reviewer: true                 # reviews the agreement
      backup: false                  # deputy contact
```

Give every party at least one contact with `signature: true`, and name a
backup contact on each side when known.

### `transmission`

| Key | Value |
| --- | --- |
| `type` | channel, e.g. `secure SFTP`, `S3 bucket` |
| `frequency` | e.g. `one-time`, `monthly`, `after each database lock` |
| `notification` | how the receiver is told a delivery arrived |
| `test_upload` | `true`/`false` — a test transfer precedes production |
| `blinded_transfer` | `true`/`false` |
| `date_first_transfer`, `date_last_transfer` | ISO date **or** a free phrase (`"2 weeks after approval"`). Never a date followed by words (`"2026-12-31 at the earliest"`) — that is flagged as an error; write a pure phrase or a pure date. |
| `extent`, `scope`, `packaging`, `security`, `checksum`, `test_requirements`, `blinding_requirements` | single strings (blank `""` allowed only as a draft placeholder) |

### `sections` (agreement prose)

A mapping of `identifier → {title, text}`, kept in the written order.
Identifiers match `^[A-Za-z][A-Za-z0-9_]*$` and are unique. Each record has
**exactly** `title` (non-blank) and `text` (may be blank in drafts). Use a
block scalar for multi-line text:

```yaml
sections:
  purpose:
    title: Purpose
    text: |
      Defines the transfer of central-lab biomarker results for study X.
  data_protection:
    title: Data Protection
    text: Data are pseudonymised; no direct identifiers are transferred.
```

Good sections to write when you have the facts: purpose, scope, data
protection/pseudonymisation, quality control, change management, archiving.

### `containers` (top level, optional)

Names the delivery archives — a `.zip` or a SAS transport library (`.xpt`) —
once, so that file entries refer to them (`container:` on a file handler,
section 3) instead of repeating the file name, and so that renaming an archive
is one edit. A list of mappings, written between `metadata` and `datasets`:

```yaml
containers:
  - id: sdtm
    filename: "STUDY_SDTM_*.zip"
    description: >-
      All SDTM domains as SAS transport files.
    default: true
  - id: adam
    filename: adam.zip
```

| Key | Notes |
| --- | --- |
| `id` | **required**. A letter, then letters, digits, `_` or `-` (`sdtm`, `adam-2`); no dot. Unique. `none` is reserved (in any letter case). File entries refer to the container by it. |
| `filename` | **required**. The archive's file name, a `.zip` or `.xpt` name, no folder. `*` and `?` are wildcards (`STUDY_SDTM_*.zip`, at most three `*`); the name is matched as a whole and letter case must match. Quote a value that starts with `*` or contains ` #`. Unique. |
| `description` | Free text; printed in the Containers section of the exported agreement. |
| `default` | `true` on at most one container: every file entry **without** a `container` of its own must then arrive inside it, including datasets added later. Must be `true` or `false`. |

No other keys. An entry's `container: none` opts that one entry out of the
default container (e.g. a `define.pdf` that is sent separately). A container
that no entry refers to and that is not the default is reported by the lint as
`container_unused`.

---

## 3. `datasets`

A list. Each dataset:

| Key | Notes |
| --- | --- |
| `id` | **required**, unique, no spaces (`lab_results`). The key every validation message, `load_file()` call and `{COLUMN_SPECS:<id>}` placeholder uses. |
| `type` | **required**: `tabular` (a table whose columns are listed: parsed and validated column by column), `matrix` (a table whose data columns are described by a rule, such as gene counts with one column per sample: section 6) or `file` (only checked to exist, be readable and non-empty: PDFs, archives, images). |
| `name` | Short human-readable name (`Laboratory results`), shown in the app and as the heading in exported documents. Always write it. |
| `description` | Longer free text: what the deliverable contains and why. Write it whenever there is more to say than the name. |
| `files` | One file handler (mapping) or a list of handlers. |
| `columns` | `tabular` only (section 4). |
| `row_labels`, `matrix` | `matrix` only (section 6). |
| `rules` | `tabular` (section 5) and `matrix` (on its row label columns). |

No other keys (`template_source/version/date` are machine-written).

A file written before `id` existed carried the identifier in `name` and the
short label in `description`. Such an entry (`name` but no `id`) is refused
with "A dataset needs an id", not reinterpreted: rename the identifier key to
`id:`.

### `files` — handler keys

| Key | Notes |
| --- | --- |
| `type` | **required**: `csv`, `tsv`, `sas7bdat`, `xpt` (for `tabular`), `csv`, `tsv` (for `matrix`), `any` (for `file`). |
| `filename` | **required**. Literal name, or a regex if `pattern: true`. |
| `pattern` | `true` → `filename` is a regex. Anchor it: `"^LAB_[0-9]{8}\\.csv$"`. |
| `pattern_description` | Human wording of the regex (`LAB_<YYYYMMDD>.csv`). Always add it when `pattern: true`. |
| `number_of_files` | Exact count (default 1). **Or** use `min_number_of_files` / `max_number_of_files` — never both styles. |
| `info` | One-line description of the file. |
| `encoding` | default `UTF-8` (e.g. `latin1`). |
| `has_header` | default `true`. |
| `quote` | default `"`. |
| `missing_values` | Extra token meaning "missing" besides empty, e.g. `"."` or `"NA"`. |
| `newlines_in_values` | `true` if quoted cells may contain line breaks. |
| `extensions` | `any` only: allowed endings, e.g. `[pdf, zip]`. A `.zip` or `.xpt` that such an entry accepts is kept as one file, not opened; to have its contents checked, name it as the `container` of their entries. |
| `container` | Optional. Where the file must arrive: the **id** of a container of the top-level `containers` section (`sdtm`, no dot); or a container **file name** (it has a dot): `sdtm.zip`, with `*` and `?` as wildcards (`sdtm_*.zip`, at most three `*`) and letter case significant (`sdtm.zip` does not match `SDTM.ZIP`); or `none`, which exempts the entry from the default container. Quote a value that starts with `*` or contains ` #`: `container: "*_SDTM.zip"`. Leave it out when the file may come loose or in any container (or, if a container is the `default`, must arrive inside that one). |

CSV is comma-separated, TSV tab-separated; `.gz` files are read transparently.

`sas7bdat` and `xpt` read a SAS dataset and a SAS transport file (the haven
package does the decoding). They take none of `encoding`, `has_header`, `quote`,
`missing_values` and `newlines_in_values` — passing one to the factory fails
with an unused-argument error, and the lint reports it as an unknown file
field — and the file is always read into memory, never streamed. A `sas7bdat`
or `xpt` handler declared inside a `type: file` dataset is coerced to a
reader-less `any` handler, since that dataset never parses its files.

An `xpt` entry reads a transport file that holds exactly one dataset. A
transport library that holds several is split by `load_delivery()` (below), so a
specification declares one `type: xpt` entry per dataset, named after the
dataset in lower case (`ae.xpt`).

### Deliveries in a `.zip` or a SAS transport library

A delivery often arrives as one `.zip`, or as one SAS transport library
(`.xpt`) holding several datasets. Declare the datasets as if their files came
loose: `load_delivery()` takes the archive or the library, looks at every file
inside by name, and gives each one to the dataset whose file entry matches it.
A delivery is loaded with `load_delivery()` or by dropping it in the app.

One zip of CSV files, two datasets (columns omitted). Define the container
once and refer to it by id:

```yaml
containers:
  - id: sdtm
    filename: sdtm.zip
datasets:
  - id: ae
    type: tabular
    name: Adverse events
    files:
      filename: ae.csv
      type: csv
      container: sdtm
  - id: dm
    type: tabular
    name: Demographics
    files:
      filename: dm.csv
      type: csv
      container: sdtm
```

Everything arrives in the zip, and every new dataset too: mark it `default`
instead of repeating the reference. An entry that is the exception says
`container: none`:

```yaml
containers:
  - id: sdtm
    filename: sdtm.zip
    description: The whole SDTM delivery.
    default: true
datasets:
  - id: ae
    type: tabular
    files:
      filename: ae.csv        # no container: belongs to the default, sdtm
      type: csv
  - id: define
    type: file
    files:
      filename: define.pdf
      type: any
      container: none         # sent separately
```

One transport library `sdtm.xpt` holding the datasets AE, DM and VS: one entry
per dataset (repeat it for `dm.xpt` and `vs.xpt`):

```yaml
containers:
  - id: sdtm
    filename: sdtm.xpt
datasets:
  - id: ae
    type: tabular
    files:
      filename: ae.xpt        # member AE of sdtm.xpt
      type: xpt
      container: sdtm
```

Sometimes a zip, sometimes loose files: leave `container` out (and define no
default).

```yaml
    files:
      filename: ae.csv
      type: csv
```

Dated archives (`STUDY123_SDTM_20251101.zip`, `STUDY123_SDTM_20251201.zip`):

```yaml
containers:
  - id: sdtm
    filename: STUDY123_SDTM_*.zip
```

**Id or file name.** A reference without a dot is an id and must name a
container of the document. A reference with a dot is a file name:
`container: sdtm.zip` works without any `containers` section (the entry then
declares an implied container of that name), and when it equals the `filename`
of a defined container it means that container. Prefer the id: a reference by
file name does not follow the container when its `filename` is edited later, and
`lint_dta()` suggests the id (`container_reference_by_filename`). Templates keep
using file names, because a template has no `containers` section.

How a delivery is routed:

- A library is split into one file per member, named after the member in lower
  case: member `AE` is offered as `ae.xpt`, which an entry `filename: ae.xpt`
  with `type: xpt` takes. A `.xpt` that holds a single dataset is a file like
  any other and is matched by its own name.
- A container never needs declaring; declaring one says where the file must
  arrive. A specification that declares `ae.csv` already takes `ae.csv` from
  inside any zip given to `load_delivery()`. With a container the entry ranks
  an `ae.csv` that came out of a matching container (or out of a container
  around that one) above one that did not. A loose `ae.csv`, or one from
  another container, is **still loaded and its content validated** unless a
  better entry takes it, and is reported as not conforming: it is `outside` in
  `container_findings()` and `check()` says "Validation FAILED". A file loaded
  on its own with `load_file()` is `unverified` ("Validation INCOMPLETE"). The
  name is matched as a whole and apart from `pattern`, which governs `filename`
  only.
- Entries of different datasets may match the same file. A literal name that
  two datasets declare gives the file to both. When pattern entries of several
  datasets fit a file equally well, `load_delivery()` does not guess: the file
  is `ambiguous` and is not loaded, until `route = list("lb_supp.csv" =
  "lb_supp")` (R) or the app's question chooses.
- A `.zip` or `.xpt` that a `type: any` entry takes as a deliverable (by its
  name, or because `extensions` lists `zip` or `xpt`) stays one file and is not
  opened, unless an entry declares it as its `container`; the load summary and
  the app say "kept as one file" for it.
- In a zip that holds single-dataset `.xpt` files named by the specification, a
  further `.xpt` library is opened only when an entry declares it as its
  `container`; it is listed as not opened, with that remedy.
- A file in the archive whose name cannot be written on the server (a `?` or a
  colon in the name, a control character, a device name such as `CON`) is
  skipped and listed when no entry asks for it, and refuses the archive when one
  does; a name that would escape its folder (an absolute path, `..`) refuses the
  archive always. macOS system files (`__MACOSX/`, `.DS_Store`, the Finder's
  `Icon` file) are ignored.
- Extracting needs free disk space for every file taken out of an archive, at
  its full size, also when the table is then streamed: a 60 GB member needs
  60 GB in R's temporary directory (or under `exdir`).
- Validation results name the dataset and table. For a table read into memory
  the validation report does not name the zip it came from; `load_delivery()`'s
  summary and its `last_delivery` table do, and the app lists the archive under
  Loaded files.

`load_delivery()` routes by `container`; `load_file()` binds the file it is
given to the entry it is told to, whatever its container (and refuses a `.zip`
or an `.xpt` the entry does not read). The limits that bound one call, and the
rest of the rules, are in `?load_delivery`.

### File names must not overlap across datasets

Write the agreement so that no file name could belong to two datasets. Two
pattern entries of different datasets conflict when one file name matches both
(and both are free of a container, or sit in containers that can name the same
archive):

```yaml
  - id: lb
    files:
      filename: "^lb.*[.]csv$"       # takes lb_supp.csv too
      type: csv
      pattern: true
  - id: lb_supp
    files:
      filename: "^lb_supp.*[.]csv$"
      type: csv
      pattern: true
```

`lint_dta()` reports this as the warning `file_name_conflict`, naming a file
that both take; `validate_template()` makes it an error; the app refuses an
edit that introduces one. Fix it with distinct prefixes (`^lb_main.*[.]csv$`
and `^lb_supp.*[.]csv$`) or make one pattern exclude the other with a
character class (`^lb([^_].*)?[.]csv$`). A look-ahead (`^lb(?!_supp).*[.]csv$`)
also routes correctly, but the check does not analyse look-arounds,
back-references or inline flags, so it neither confirms nor reports them.
Two literal entries with the same name are not a conflict (both datasets get
the file), nor is a literal name against a pattern (the literal wins).

---

## 4. `columns` (tabular datasets)

**The delivered table must contain exactly the declared columns** — a missing
column and an undeclared extra column are both errors. Declare every column.

| Key | Notes |
| --- | --- |
| `id` | **required**, exact header text, unique. Spaces and punctuation are allowed between characters (`Subject ID`, `Visit Date (UTC)`, `Dose mg/kg`); no space at the start or end, and no whitespace other than spaces between characters (tabs, line breaks and other whitespace such as an ideographic space are refused). See *Column ids* below. |
| `label` | Short human label (≤ 40 chars, SAS-label style). |
| `description` | What the value means, its unit, origin, derivation, and when it may be blank. |
| `type` | Always prefixed with its type system, `SAS ` or `R `. **`SAS`**: `SAS Char`, `SAS Num` (decimal), `SAS Int` (whole number), `SAS Date`, `SAS Time`, `SAS DateTime`. **`R`**: `R character`, `R numeric` (`R double` is accepted), `R integer`, `R logical`, `R Date`, `R POSIXct`, `R hms` — see *The `R` type system* below. A bare `Char` or `character` is refused. A dataset may mix the two. |
| `format` | Optional, prefixed like the type. **SAS**: prefixed `SAS `, then `$w.` (Char), `w.` / `w.d` / `BESTw.` (Num), `w.` / `BESTw.` (Int), `DATEw.`, `TIMEw.`, `DATETIMEw.`. **R**: prefixed `R `, then a date/time format such as `R %d/%m/%Y`, only on `R Date`, `R POSIXct` and `R hms`. Must fit the type. A declared format **is** checked: a value that cannot be displayed in it (too wide, too many decimals, not date/time display text) or, for R, does not match it, is reported. |
| `length` | Max length in characters (applies to text values; of the R types only `R character` takes it). Together with a declared `format`, this is what enforces width. |
| `nullable` | `true` / `false`. **Always set it explicitly** — if omitted, missing values fail. |
| `values` | Closed list of permitted values (controlled terminology). |
| `pattern` | Regex every non-missing value must match (Perl syntax). **Unanchored by default — always wrap in `^…$`.** |
| `examples` | Example values; must match `pattern` if one is given. |
| `colclass` | Optional category: `patient_info`, `measurement_patient`, `measurement`, `visit_related`, `date_related`, `study_info`, `wide_format`, `long_format`, `wide_and_long_format`. |

**Column ids.** An `id` is compared with the header of the delivered file
character for character. The match is case-sensitive and codepoint-exact:
`Subject  ID` (two spaces) is not `Subject ID`, and a no-break space is not a
space. Write the id exactly as the header reads; a tabular dataset has no
separate key for the header text. Before the comparison the reader trims the
whitespace around a header and removes one pair of double quotes wrapped around it
(`"Subject ID"` becomes `Subject ID`); a quote at only one edge belongs to the
name, so `Length 5"` stays `Length 5"`. (A matrix row-label `id` may not
contain whitespace, section 6, and neither may a rule `id`: a space in one is
written as an underscore when the file is read.)

A plain id needs no quotes: `id: Subject ID`. Quote an id (`id: "Y"`) when YAML
would read it as something other than text. Unquoted, these fail or change:

- **Words and numbers.** A YAML 1.1 reader turns plain `Y`, `N`, `yes`, `no`,
  `true`, `false`, `on`, `off`, `null` and `~`, and number-like text (`007`,
  `1.10`, `0.5`, `0x1F`, `.inf`), into something that is not a string.
  Numeric headers such as the time points of a PK table are realistic, so quote
  them (`id: "0.5"`). The specification is then refused with `column_spec_invalid`.
  So is `{id}`, which is read as a mapping, and `[a, b]`, which is read as a list.
- **A first character YAML reserves.** An id that starts with `%`, `|`, `>`, `'`,
  `"`, `@`, a backtick, `- ` or `? `, or contains `: `, makes the file unreadable
  (`yaml_unreadable`). One that starts with `&` or `!` or `#` ends up with no
  id and is refused as well.
- **Changed without a word.** An id that starts with `*` becomes the placeholder
  `_yaml.bad-anchor_`, ` #` cuts an id at the `#`, and `[a]` is read as `a`.
  Nothing reports these, so quote any id that holds one of those characters.

Inside an inline list such as `columns: [A, B]`, quote an id that contains `,`,
`[`, `]`, `{` or `}` as well. The same holds wherever a rule names a column:
`columns:` lists, the keys under `condition:` and `then:`, and `group_by:`.

Constraints between keys:

- `values` excludes both `pattern` and `examples` (pick one: closed list *or* pattern).
- `format`/`length` require `type`. An R `format` goes only on `R Date`, `R POSIXct` and `R hms`, an R `length` only on `R character`, and `pattern` is refused on `R logical`.
- `nullable: true` admits empty cells; with `values`, empty is allowed in addition to the list.

What is enforced per value: type (non-numeric text in a Num/Int column, or an
`R numeric`/`R integer` one, is an import error; a decimal in an Int/`R integer`
column fails), a declared `format`, `length`, `values`, `pattern`, and
missingness via `nullable`.

**`SAS Date`/`Time`/`DateTime` are stored as text and their shape is not checked
by the type** — add a pattern, e.g. ISO 8601 date
`"^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"`. An `R Date`,
`R POSIXct` or `R hms` column is the opposite: its format, the default one
included, is always enforced, so it needs no pattern.

For a `sas7bdat` or `xpt` delivery, a SAS date, datetime or time column arrives as the
display text of its declared format (`15AUG2025` under `DATE9.`) and as ISO 8601
text when no format is declared. Do **not** declare `length` on such a column:
it is checked against that text, so the `length: 8` that PROC CONTENTS reports
for the 8-byte numeric storage would fail every row. Declared `R Date`,
`R POSIXct` or `R hms`, the column arrives as text in the R format instead
(`2025-08-15` under `%Y-%m-%d`, a datetime in UTC; the value is rendered under
the declared format, so anything finer than it is cut), but only when the declared
type is the kind SAS stored: `R Date` for a date, `R POSIXct` for a datetime,
`R hms` for a time. Any other pairing arrives as ISO 8601 text, which then fails
the format. Declared `R numeric`, `R integer`, `R character` or `R logical`,
the column stays the SAS number (days since 1960-01-01 for a date, seconds for
a datetime or time).

### The `R` type system

`R` is the second type system next to `SAS`; both are always available. Write
`R` in capitals and then the type; the type is not case-sensitive (`R date` is
`R Date`) and is stored in the spelling below. A dataset or a matrix may mix
the two.

| Type | Validated as | What is checked | `format` | `length` |
| --- | --- | --- | --- | --- |
| `R character` | string | Any text; `length` is the maximum number of **characters**. | no | optional |
| `R numeric` (`R double`) | number | The same strict parse as `SAS Num`: text that is not a number is an import error. | no | no |
| `R integer` | integer | A whole number within ±2147483647, as `SAS Int`. | no | no |
| `R logical` | boolean | One of `TRUE`, `T`, `true`, `True` (true) or `FALSE`, `F`, `false`, `False` (false). A blank is missing. Anything else is an import error, including `yes`, `no`, `1`, `0`, a padded `" T"` and a wrong case such as `tRuE`. The text `NA` is a missing marker in a CSV or TSV file (no finding); only from a `sas7bdat` or `xpt` file is it an import error. | no | no |
| `R Date` | string | Text that fits the format. Default `%Y-%m-%d`. | optional | no |
| `R POSIXct` | string | Text that fits the format. Default `%Y-%m-%d %H:%M:%S`. | optional | no |
| `R hms` | string | A time of day that fits the format. Default `%H:%M:%S`. | optional | no |

`R logical` text is converted to logical at import (the same in memory and when
a large file is read in batches). `R Date`, `R POSIXct` and `R hms` are
validated as **text** and stay text after import, like the `SAS` date types.
Text that fails shows as `must fit format "<format>"` (the validation report
says "Must fit the R date/time format").

**Date/time format.** A format is built from these directives, which do not
depend on the locale:

| Directive | Fits |
| --- | --- |
| `%Y` | exactly four digits, `0000`–`9999` |
| `%m` | month, `01`–`12` |
| `%d` | day, `01`–`31`, and no later than the month's last day (Gregorian leap years: `2024-02-29` fits, `2023-02-29` and `2024-04-31` do not) |
| `%H` | hour, `00`–`23` |
| `%M`, `%S` | minute, second, `00`–`59` |
| `%OS1` … `%OS6` | seconds `00`–`59` with an optional fraction of one to n digits: `%OS3` fits `10:30:00`, `10:30:00.5`, `10:30:00.123`, not `10:30:00.1234` or `10:30:00.` |
| `%F`, `%T` | shorthands for `%Y-%m-%d` and `%H:%M:%S` |

Besides directives, only a space, `-`, `/`, `.`, `,`, `:`, `;`, `_` and the
capital letters `T` and `Z` may appear, and they stand for themselves
(case-sensitive): ISO 8601 is `%Y-%m-%dT%H:%M:%SZ`. Digits, other letters, `%%`,
quotes and braces are refused.

- Every field has a fixed width, so `2024-1-5` does not fit `%Y-%m-%d`, and
  `%Y%m%d` (`20240105`) is unambiguous.
- Year, month, day, hour and minute may each appear **once**, and only one
  seconds directive (`%F %Y` and `%T %OS3` are refused).
- A `Date` format needs year, month and day and no time part. A `POSIXct`
  format also needs hour and minute. An `hms` format needs hour and minute and
  no date part, and is a **time of day**: `24:00:00` and `100:00:00` fail
  (`SAS Time` accepts longer times).
- A value from a text source (CSV, TSV) is matched **exactly**:
  it is not trimmed and the letters are case-sensitive (a SAS display value is
  trimmed), so `" 2024-01-05"` fails `%Y-%m-%d` and `2024-01-05t10:30:00z`
  fails `%Y-%m-%dT%H:%M:%SZ`. A `sas7bdat` or `xpt` value is a typed number, so
  it is rendered under the declared format first: anything finer than the
  format is cut (`14:30:45` under `R %H:%M` reads `14:30`, `.1234` under
  `%OS3` reads `.123`) and then passes, as it does for `SAS TIME5.`. The format
  string itself is trimmed when the specification is read.
- **Not supported**: month and day names (`%b`, `%B`, `%a`, `%A`), AM/PM (`%p`,
  `%I`), two-digit years (`%y`) and time zones and offsets (`%z`, `%Z`) are
  refused when the specification is read. Leap seconds have no directive: a
  seconds field of `60` is simply a value that does not fit (`%S` stops at
  `59`), reported as a `format` finding. For `15AUG2025` use `SAS Date` with
  `SAS DATE9.`.
- **The default is written into the specification.** `type: R Date` with no
  format becomes `R Date` with `R %Y-%m-%d`, so a supplier who reads the
  specification sees what is enforced. A `SAS Date` with no format accepts any
  text.
- A format that holds a colon followed by a space (`%H: %M`) must be quoted:
  `format: 'R %H: %M'`.

```yaml
columns:
  - id: SUBJECT
    type: R character
    length: 12
    nullable: false
  - id: WEIGHT
    type: R numeric
    nullable: true
  - id: VISITNUM
    type: R integer
    nullable: false
  - id: ITT
    type: R logical
    nullable: false
    values: ["TRUE", "FALSE"]     # quoted: YAML reads a bare true as a boolean
  - id: VISITDT
    type: R Date                  # no format: R %Y-%m-%d is written in
    nullable: false
  - id: BIRTHDT
    type: R Date
    format: R %d/%m/%Y
    nullable: true
  - id: STARTDTM
    type: R POSIXct
    format: R %Y-%m-%dT%H:%M:%SZ  # ISO 8601 with a T and a Z
    nullable: false
  - id: DOSETM
    type: R hms
    format: R %H:%M:%OS3          # 10:30:00 or 10:30:00.123
    nullable: true
```

**`R logical` columns.**

- `values` and `examples` must be **quoted** (`["TRUE", "FALSE"]`): YAML reads
  an unquoted `true`, `yes` or `Y` as a boolean and the specification is
  refused (`column_spec_invalid`, with a hint).
- `pattern` is refused: nothing would enforce it.
- In a rule, `equals`, `equal`, `not_equals`, `not_equal`, `in` and `not_in`
  take `true` or `false`, or any of the eight tokens, so `equals: T` matches
  `TRUE`. Any other operand (`yes`, `1`) is reported as the error
  `rule_condition_value_unfit`. The rule compares what the column holds after
  import, which is logical, so a `T` in the file and `equals: true` agree.
- A rule condition with the `pattern` operator on an `R logical` column is
  refused when the specification is checked: it could never match a logical
  value.
- In a `col_unique` rule on a logical column, `T`, `true` and `TRUE` are one
  key.

**Limits.** A typed `Date`, `POSIXct` or `hms` vector (R, in memory) is not
class-checked against its declared type and skips the R format check; only text
is matched against a format. In a CSV or TSV file the text `NA` is already a
missing marker, so an `R logical` column raises no finding for it; read from a
`sas7bdat` or `xpt` file it is not one of the tokens and is an import error.
There are no time zones, leap seconds or month names, and no `factor` or
`integer64` type.

### Making column specs meaningful

Every column should constrain *something* beyond its type. Pick the strongest
true statement:

1. **Finite code list** (sex, visit, test code, unit, flag) → `values`.
2. **Structured identifier or date** → anchored `pattern` (+ `examples`); an
   `R Date`/`R POSIXct`/`R hms` column carries its format instead.
3. **Free text** → `type: SAS Char` (or `R character`) + realistic `length`.
4. **Measurement** → `SAS Num`/`SAS Int` (or `R numeric`/`R integer`), plus a `col_range` rule for plausible bounds.
5. `nullable: false` for keys, identifiers and anything mandatory; `true` only
   when blank is legitimate — and then say *when* in the `description`.

Write descriptions that a data manager at the supplier can act on: unit, source
system, derivation, CDISC/SDTM variable it maps to, blank-when conditions.

---

## 5. `rules` (tabular datasets)

Cross-column and cross-row logic. Every rule has `id` (unique, no whitespace),
`type`, and should have a `description` stating the business rule in plain
words. Rules must only reference declared columns.

### `col_unique` — key uniqueness

```yaml
- id: unique_subject_visit_test
  type: col_unique
  description: One result per subject, visit and test.
  columns: [USUBJID, VISIT, LBTESTCD]   # the combination must be unique
```

### `col_range` — numeric plausibility (one column, inclusive bounds)

```yaml
- id: age_adult_range
  type: col_range
  description: Only adults up to 85 were enrolled.
  columns: AGE        # exactly one column
  min: 18             # min < max, both required
  max: 85
```

(`range: [18, 85]` is equivalent to `min`/`max`.)

### `col_condition` — row-level IF → THEN

For each row where **all** `condition` clauses hold, **all** `then` clauses must
hold. Missing values in THEN count as a violation.

```yaml
- id: not_done_requires_reason
  type: col_condition
  description: A test that was not done must state why and carry no result.
  condition:
    LBSTAT:
      equals: NOT DONE
  then:
    LBREASND:
      empty: false
    LBORRES:
      empty: true
```

Operators (several on one column are ANDed):

| Operator | Value | Meaning |
| --- | --- | --- |
| `equals` / `not_equals` | scalar | equality (numeric compare if both sides are numbers) |
| `in` / `not_in` | list | set membership |
| `greater`, `less`, `greater_equal`, `less_equal` | number | numeric comparison |
| `min` + `max` | numbers | one inclusive band (either alone also works) |
| `range` | `[lo, hi]` | inclusive band |
| `pattern` | regex | value matches (anchor it); a missing value and an empty string never match |
| `empty` | `true` / `false` | value is / is not missing or blank |

Each column may appear only once per clause; merge its operators.

**What `pattern` does with missing, empty and numeric values.** A missing value
and an empty string match no pattern, not even `^$` or `.*`. In a `then` clause
that makes them violations, exactly as a missing value is for every operator; in
a `condition` clause it leaves their rows out of scope. Text of spaces only is
ordinary text for `pattern` (it is `empty` that counts blanks), so use `empty`
to ask whether a value is missing or empty. A number is matched as its plain
decimal text, never in scientific notation: `100000` is matched as `100000`, not
`1e+05`, and `0.00001` as `0.00001` (a fraction keeps at most 15 significant
digits). A whole number is therefore matched the same way whether the column is
declared as an integer or as a number. A column's own
`pattern` (in `columns:`) is a different check, whether a value may stand in the
column, and judges an empty string like any other text; a file never delivers
one, because an empty field is read as missing.

On an `R logical` column, `equals`, `equal`, `not_equals`, `not_equal`, `in` and
`not_in` take `true` or `false`, or one of `TRUE`, `T`, `true`, `True`, `FALSE`,
`F`, `false`, `False` (`equals: T` matches `TRUE`); any other operand is an
error.

**The app's rule editor** (Edit rules) shows a clause as one row per column and
operator. Several operators on one column are several rows, all of which must
hold, with each operator at most once per column; `min` and `max` together, and
`range`, are one *between* row. A rule opened and saved without a change keeps
its meaning and its values, but the aliases are written in their normal form:
`range: [lo, hi]` as `min: lo` with `max: hi`, `equal` as `equals`, `not_equal`
as `not_equals`. A value you type is read by the declared type of its column: a
number for a numeric column, text for any other, so `007` stays text on a
`SAS Char` column; the comparison operators and *between* always take numbers
(a date column keeps its text). Decimals are written with a point. In an `in` or
`not_in` list a value that holds a comma goes in double quotes (`"Smith, John",
Doe`, a quote inside it doubled), and a missing value is written `~`. A value
the editor cannot read, or the same operator twice on one column, is refused on
Save with a message, and the rule is left as it was. A rule it cannot show
faithfully, because an operator is unknown to it or two spellings of one
operator stand on one column (`range` with `min`), is not opened; edit it in the
YAML text.

### `group_condition` — logic across the rows of a group

Define named row conditions, then constrain how they co-occur inside each group.

```yaml
- id: subject_consent_logic
  type: group_condition
  description: A subject with any consented record must have a consent date on every record.
  group_by: [USUBJID]
  conditions:
    c_consented:
      CONSENT: { equals: "YES" }
    c_has_date:
      CONSENT_DATE: { empty: false }
  constraints:
    - id: consent_needs_date
      type: requires            # alias: implies
      if: c_consented
      then: c_has_date
      if_scope: any             # any row in the group (default)
      then_scope: all           # every row in the group
      message: Consented subject has records without a consent date.   # optional
    # - id: ...
    #   type: mutually_exclusive   # alias: not_both
    #   left: c_a
    #   right: c_b
    #   left_scope: any
    #   right_scope: any
```

- `requires`: if (`if` holds per `if_scope`) then (`then` must hold per `then_scope`).
- `mutually_exclusive`: `left` and `right` must not both hold (per their scopes).
- Scopes: `any` (≥ 1 row in the group) or `all` (every row). Default `any`.
- Constraint `id`s unique within the rule; names in `if/then/left/right` must be defined under `conditions`.

### Writing meaningful rules

Encode what a reviewer would check by hand. Typical, high-value rules:

- **Primary key** uniqueness (`col_unique`) — every tabular dataset should have one.
- **Plausibility ranges** for every numeric measurement (`col_range`), from the protocol or physiology.
- **Status consistency**: result present ⇔ status done; not-done ⇒ reason given (`col_condition`).
- **Units**: result present ⇒ unit present; specific test code ⇒ specific unit (`in`).
- **Date logic** expressible as patterns/presence (end date present ⇒ start date present).
- **Per-subject/per-visit consistency** (`group_condition`): baseline exists for every subject; a sample cannot be both failed and reported.

Don't write rules that restate a column spec (a `values` list already enforces
membership) and don't write rules that are always true. Each rule's
`description` states the business reason.

---

## 6. Matrix datasets (`type: matrix`)

Use `type: matrix` for a delivery whose columns cannot be listed in advance:
gene counts with one column per sample, expression or protein values, genotype
calls. Every line starts with one or a few **row label columns** that identify
and describe the row; **every further column is a data column**, and all data
columns hold the same kind of value. The specification does not list the data
columns: it says what their labels must look like, how many there may be and
what a cell may hold.

```
header line →   gene_id   gene_name   S0001   S0002   S0003  …
                ───────   ─────────   ─────   ─────   ─────
data rows   →   ENSG…003  TSPAN6      118     0       7.25
                ENSG…005  TNMD        0       1.5     0
                └ row label columns ┘ └──── data columns ────┘
                  (row_labels)          (matrix.columns, matrix.cells)
```

A table whose columns are known and differ in meaning (`SUBJID`, `VISIT`,
`AGE`) is `type: tabular`, not a matrix.

```yaml
datasets:
  - id: gene_counts
    type: matrix
    name: Gene counts
    files: { filename: "gene_counts.tsv", type: tsv }
    row_labels:                  # the leading columns, in file order
      - id: GENE_ID
        header: gene_id          # the header line's text above this column
        type: SAS Char
        nullable: false
        pattern: "^ENSG[0-9]{11}$"
      - id: GENE_NAME
        header: gene_name
        type: SAS Char
        nullable: true
    matrix:
      columns:                   # the data columns
        label: Sample ID
        pattern: "^S[0-9]{4}$"   # what every column label must look like
        count: { min: 1, max: 384 }
      rows:
        key: GENE_ID             # unique, never missing
        count: { min: 1 }
      cells:
        type: SAS Num
        nullable: false
        range: { min: 0 }
        infinite: false
```

A matrix dataset has `row_labels` and `matrix` **instead of** `columns`;
writing `columns` on a matrix, or `row_labels`/`matrix` on another type, is an
error. `files` takes `csv` or `tsv` handlers with a header line (`has_header`
must stay `true`); the file may arrive loose or in a container like any other.

### `row_labels` — the leading columns

A list with at least one entry. Entry *i* is column *i* of the file: row labels
are matched **by position**, not by name. An entry takes every key of a
tabular column (section 4: `id`, `label`, `description`, `type`, `format`,
`length`, `nullable`, `values`, `pattern`, `examples`) and one more. There is
one exception: a row-label `id` **may not contain whitespace**, unlike a column
`id`. Put the exact header text under `header` instead (`id: SAMPLE_ID` with
`header: "Sample ID"`); `lint_dta()` reports the whitespace as
`matrix_row_label_id_invalid`, and the dataset is refused.

| Key | Notes |
| --- | --- |
| `header` | What the header line holds above this column. **Left out** (or written with no value, `header:`): the `id`, as for a tabular column. **A text**: that text (quote it; `""` is an empty field). **`false`** (or `No`): the header line has no field above this column at all — what R `write.table()` writes above the row names; allowed on the first row label only. |

The first row label is the row key unless `matrix.rows.key` names another.
Further row labels describe the row (a gene name, a chromosome); constrain
them like any column.

Which `header` to write depends on what wrote the file:

| File written by | The header line starts | Write |
| --- | --- | --- |
| R `write.csv(x)` (row names) | `"",S1,S2` | `header: ""` |
| pandas `to_csv()`, index without a name | `,S1,S2` | `header: ""` |
| R `write.table(x, sep = "\t")` (row names) | `S1<TAB>S2` — one field fewer than the data rows | `header: false` |
| pandas `to_csv()`, index named `protein_id` | `protein_id,S1,S2` | `id: protein_id`, or another `id` and `header: protein_id` |
| any writer that names the column like your `id` | `GENE_ID<TAB>S1` | nothing |

A header text is compared after the whitespace around it and one pair of double
quotes wrapped around it are removed (a quote at only one edge stays part of the
text), and is case-sensitive.

### `matrix.columns` — the data columns

Optional; left out, any label is accepted and at least one data column is
required. Whatever is written, **a column label must always be present, unique,
and different from every row label `id`**.

| Key | Notes |
| --- | --- |
| `label` | What a data column is (`Sample ID`). Documentation, shown in the exported agreement. |
| `description` | Longer free text. |
| `pattern` | Regex every column label must match. Anchor it. |
| `values` | Closed list of permitted column labels, **quoted** (`["S01", "S02"]`). `pattern` or `values`, not both. With `count` equal to the length of the list, the file must hold exactly these columns, in any order. |
| `length` | Longest permitted label, in characters. |
| `count` | Number of data columns: a whole number for exactly that many (`count: 96`), or `{ min: …, max: … }` with either side optional. Default `{ min: 1 }`; zero data columns is never valid. |

### `matrix.rows`

| Key | Notes |
| --- | --- |
| `key` | The row label `id` that identifies a row, or a list of ids that do so together. Default: the first row label. A key column must be unique across the file and must not be `nullable: true`. |
| `count` | Number of data rows, written like `matrix.columns.count`. Default `{ min: 1 }`: a file with a header and no rows fails. Write `count: { min: 0 }` to accept one. |

### `matrix.cells` — what every cell may hold

**Required**, with `type`.

| Key | Notes |
| --- | --- |
| `type` | **required**: `SAS Num` or `R numeric` (decimal), `SAS Int` or `R integer` (whole number), `SAS Char` or `R character` (text). A date, time or logical type (`SAS Date`, `R Date`, `R logical`, …) is refused. The row labels may use either type system, independently of the cells. |
| `nullable` | `true` / `false`. Whether a missing cell is allowed. **Set it explicitly** — if omitted, missing cells fail. |
| `range` | `{ min: …, max: … }`, either side optional, both inclusive. `SAS Num`, `SAS Int`, `R numeric` and `R integer` only. Write plain numbers (`1000`, `1.0e+3`; YAML reads `1e3` as text). |
| `infinite` | `true` admits `Inf` and `-Inf`. `SAS Num` and `R numeric` only; default `false`. |
| `values` | Closed list of permitted values (`[0, 1, 2]`). |
| `pattern`, `length`, `format` | As for a tabular column. |
| `label`, `description` | Documentation (`Normalised count`). |

How a raw cell is read — exactly as a tabular column of the same type:

| Raw cell | Result |
| --- | --- |
| empty, `NA`, quoted `""` / `"NA"`, a token declared in `files.missing_values` | **missing**: allowed only with `nullable: true` |
| only spaces | missing for `SAS Num` / `SAS Int` / `R numeric` / `R integer`; text for `SAS Char` / `R character` |
| `12`, `+12`, `012`, `1e3` | the number |
| `1.5` in a `SAS Int` or `R integer` matrix | error (not a whole number) |
| `NaN` | error — never a value. To accept it as missing, declare `missing_values: "NaN"` on the file |
| `Inf`, `-Inf` | error unless `infinite: true` |
| `N/A`, `NULL`, `abc`, `1,5` | error — not missing unless declared in `files.missing_values` |

Example: `files: { filename: "geno.tsv", type: tsv, missing_values: "./." }`
with `cells: { type: SAS Int, nullable: true, values: [0, 1, 2] }` (or
`type: R integer`) describes a genotype matrix in which `./.` is a call that
was not made.

### `rules` on a matrix

Optional. The rule types of section 5, on **row label columns only** (a rule
cannot name a data column). The uniqueness of the row key is checked without a
rule, so no `col_unique` rule is needed for it. Rule ids must not start with
`.matrix:`.

### What a delivery is checked for

- The header line: the text above each row label; every column label present,
  unique and satisfying `matrix.columns`; the number of data columns.
- The rows: every row label column like a tabular column; the row key unique
  (a repeated key is reported under the rule id `.matrix:row_key`);
  the number of data rows.
- Every cell: type, missingness, `range`, `infinite`, `values`, `pattern`.
- A file that cannot be a matrix of the declared shape is **refused before it
  is read**, with the reason: the first data row has more or fewer fields than
  the header implies, the file starts with a `#` line, or it is separated by
  another character than the declared one.

### Not supported

Lines before the header (featureCounts, GCT); semicolon-separated files and
decimal commas; annotation columns after the data columns; files without a
header line or without a row label column; sparse and single-cell formats;
`sas7bdat` / `xpt` matrices.

---

## 7. YAML pitfalls

- **Quote anything that must stay text**: `"1.0"`, `"007"`, `"Y"`, `"N"`,
  `"YES"`, `"NO"`, `"ON"`, `"OFF"`, `"NA"`, phone numbers, codes with leading
  zeros. Unquoted `yes/no/on/off/y/n` become booleans, `1.10` becomes `1.1`.
- `values` for a text column: quote them all. For a numeric column, unquoted numbers.
  For an `R logical` column quote `"TRUE"` and `"FALSE"` as well.
- An R date/time `format` that holds a colon followed by a space (`%H: %M`) must
  be quoted: `format: 'R %H: %M'`. Unquoted, the file is unreadable.
- Regexes: in double quotes, backslashes are doubled (`"\\."`); in single
  quotes they are not (`'^\d+$'`). Prefer character classes (`[0-9]`) to avoid both.
- Dates: write `2026-03-01` (ISO). No other date format is understood.
- A row label's `header` is a text or `false`. Quote the text: `header: "1"`,
  `header: "Y"`. Unquoted, `1` is a number and `Y` is `true`, and both are refused;
  unquoted `N`, `no`, `off` and `False` are `false`, which on the first row label
  is accepted and means "no field".
- 2-space indentation, no tabs; lists with `- `; inline `[a, b]` is fine.
- A value that starts with `*` is a YAML alias and stops the file from being
  read; one that contains ` #` is cut at the `#`. Quote both:
  `container: "*_SDTM.zip"`.
- A column `id` is the header text and may hold spaces (`id: Subject ID`), but
  quote one that YAML would read as something else: `"Y"`, `"No"`, `"null"`,
  `"007"`, `"0.5"` (a number-like or boolean-like header), `"%Change"`,
  `"{id}"`, `"*x"`. Three groups, and which of them fail loudly, are under
  *Column ids* in section 4.

---

## 8. Complete example

```yaml
metadata:
  title: Central Laboratory Data Transfer Agreement – Study EX-101
  version: "1.1"
  date: 2026-03-01
  header: Example Pharma GmbH
  study:
    id: EX-101
    protocol_id: EX-101-PROT-02
  version_history:
    - version: "1.0"
      date: 2026-01-15
      changes: Initial version.
    - version: "1.1"
      date: 2026-03-01
      changes: Added LBREASND and the not-done rule.
  receiver:
    affiliation:
      name: Example Pharma GmbH
      address: Example Str. 1, 55216 Ingelheim
      country: Germany
    contacts:
      - name: Jane Doe
        role: Lead Data Manager
        email: jane.doe@example.com
        department: Data Management
        signature: true
        reviewer: true
      - name: Max Muster
        role: Backup Data Manager
        email: max.muster@example.com
        backup: true
  supplier:
    affiliation:
      name: CentralLab Ltd.
      country: United Kingdom
    contacts:
      - name: John Smith
        role: Data Transfer Lead
        email: john.smith@centrallab.example
        phone: "+44 20 0000 0000"
        signature: true
  transmission:
    type: secure SFTP
    frequency: monthly, plus final transfer after database lock
    notification: email to the receiver data manager
    test_upload: true
    blinded_transfer: false
    date_first_transfer: 2026-04-01
    date_last_transfer: 4 weeks after database lock
    packaging: One CSV per transfer, gzip-compressed
    security: Pseudonymised data only; SFTP with key authentication
    checksum: SHA-256 file delivered alongside each transfer
    test_requirements: Test file with dummy data must pass validation before the first production transfer
  error_handling: >-
    Files failing validation are rejected within 2 business days; the supplier
    re-delivers a corrected file within 5 business days.
  authorized_for_corrections: [Jane Doe, Max Muster]
  sections:
    purpose:
      title: Purpose
      text: Transfer of central laboratory results for study EX-101.
    data_protection:
      title: Data Protection
      text: Subjects are identified by USUBJID only; no direct identifiers are transferred.

datasets:
  - id: lab_results
    type: tabular
    name: Laboratory results
    description: Central laboratory results, one row per subject, visit and test.
    files:
      type: csv
      filename: "^EX101_LB_[0-9]{8}\\.csv$"
      pattern: true
      pattern_description: EX101_LB_<YYYYMMDD>.csv
      number_of_files: 1
      info: Cumulative lab results as of the transfer date.
    columns:
      - id: STUDYID
        label: Study Identifier
        type: SAS Char
        length: 6
        nullable: false
        values: ["EX-101"]
        description: Study identifier; constant.
      - id: USUBJID
        label: Unique Subject Identifier
        type: SAS Char
        length: 14
        nullable: false
        pattern: "^EX-101-[0-9]{3}-[0-9]{3}$"
        examples: ["EX-101-001-004"]
        description: STUDYID-SITE-SUBJECT, as assigned in the EDC.
      - id: VISIT
        label: Visit Name
        type: SAS Char
        nullable: false
        values: ["SCREENING", "WEEK 4", "WEEK 12", "END OF TREATMENT"]
        description: Protocol visit at which the sample was taken.
      - id: LBDTC
        label: Date of Specimen Collection
        type: SAS Date
        nullable: false
        pattern: "^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"
        examples: ["2026-04-14"]
        description: Collection date, ISO 8601 (YYYY-MM-DD).
      - id: LBTESTCD
        label: Lab Test Short Name
        type: SAS Char
        length: 8
        nullable: false
        values: ["GLUC", "HBA1C"]
        description: SDTM LBTESTCD controlled terminology.
      - id: LBSTAT
        label: Completion Status
        type: SAS Char
        nullable: true
        values: ["NOT DONE"]
        description: '"NOT DONE" if no result could be obtained, otherwise blank.'
      - id: LBREASND
        label: Reason Not Done
        type: SAS Char
        length: 200
        nullable: true
        description: Why the test was not done; required when LBSTAT is "NOT DONE".
      - id: LBORRES
        label: Result in Original Units
        type: SAS Num
        nullable: true
        description: Numeric result; blank when LBSTAT is "NOT DONE".
      - id: LBORRESU
        label: Original Units
        type: SAS Char
        nullable: true
        values: ["mmol/L", "%"]
        description: Unit of LBORRES; blank when there is no result.
    rules:
      - id: unique_subject_visit_test
        type: col_unique
        description: One result per subject, visit and test.
        columns: [USUBJID, VISIT, LBTESTCD]
      - id: glucose_unit
        type: col_condition
        description: Glucose is reported in mmol/L.
        condition:
          LBTESTCD: { equals: GLUC }
          LBORRES: { empty: false }
        then:
          LBORRESU: { equals: mmol/L }
          LBORRES: { range: [0.5, 40] }
      - id: hba1c_unit
        type: col_condition
        description: HbA1c is reported in percent within a plausible range.
        condition:
          LBTESTCD: { equals: HBA1C }
          LBORRES: { empty: false }
        then:
          LBORRESU: { equals: "%" }
          LBORRES: { range: [3, 20] }
      - id: not_done_requires_reason
        type: col_condition
        description: A test not done carries a reason and no result.
        condition:
          LBSTAT: { equals: NOT DONE }
        then:
          LBREASND: { empty: false }
          LBORRES: { empty: true }
      - id: screening_glucose_per_subject
        type: group_condition
        description: Every subject has a glucose record at screening.
        group_by: [USUBJID]
        conditions:
          c_any_row:
            USUBJID: { empty: false }
          c_screening_glucose:
            VISIT: { equals: SCREENING }
            LBTESTCD: { equals: GLUC }
        constraints:
          - id: subject_needs_screening_glucose
            type: requires
            if: c_any_row
            then: c_screening_glucose
            if_scope: any
            then_scope: any

  - id: lab_manual
    type: file
    name: Laboratory manual
    description: Laboratory manual describing methods and reference ranges.
    files:
      type: any
      filename: "^EX101_LabManual_v[0-9]+\\.pdf$"
      pattern: true
      pattern_description: EX101_LabManual_v<N>.pdf
      number_of_files: 1
      extensions: [pdf]
```

---

## 9. Final checklist

- [ ] Only allowed keys; `metadata.title` and quoted `metadata.version` present.
- [ ] `date`, `study`, `version_history`, both parties with signing contacts,
      `transmission`, `error_handling`, `authorized_for_corrections` and
      `sections` filled from the available information. Do not invent facts —
      if something is unknown, leave it out (or use a blank text section as a
      visible draft placeholder) rather than guessing.
- [ ] Every dataset has an `id` and a `name` (and a `description` where there is more to say); every file pattern is anchored and has a `pattern_description`.
- [ ] Every delivered column is declared, with `label`, `type`, explicit `nullable`, `description`.
- [ ] Every column carries `values`, an anchored `pattern`, or a `length`; `SAS` dates have a pattern, `R` dates a `format`.
- [ ] Every `type` and `format` starts with `SAS ` or `R `; an `R logical` column's `values` are quoted.
- [ ] `values` and `pattern` never on the same column; text codes quoted.
- [ ] Each tabular dataset has a key `col_unique` rule, ranges for measurements,
      and conditional rules for status/unit/result dependencies — each with a `description`.
- [ ] Rule and constraint ids unique, no whitespace; rules reference declared columns only.
- [ ] A `container:` value is the id of a defined container, a `.zip` or `.xpt` file name (no folder) or `none`, on the file entry; container ids and file names are unique, at most one container is `default`.
- [ ] No two pattern entries of different datasets can take the same file name.
- [ ] Each matrix dataset: row labels in file order, each with the `header` text the file really has (`""` for an empty field, `false` for none); `matrix.columns` with a `pattern` or `values` and a `count`; `matrix.cells` with `type`, explicit `nullable` and a `range` where one exists; no `columns` key.
