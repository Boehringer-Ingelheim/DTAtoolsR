# Biomarker GF DTS Metadata Overview

Source template analyzed:
- .tmp_dts_templates/BIOMARKER GF TRANSFER SPECIFICATIONS (DTS).docx

Goal:
- map the GF DTS document to the metadata the `DTAtools` package actually models
- give each field a type and example value
- keep the `biomarker_gf.dta-template.yaml` creation template coherent with the
  canonical `DTAMetaData` class (so every value the template sets is visible and
  editable in the Shiny app metadata editor)

> Only the fields in the "Canonical `DTAMetaData` fields" sections below are
> real object properties. The template's `base.metadata` and every option
> `effect` write to those fields only. GF-specific concepts that `DTAMetaData`
> does not (yet) model are collected in the appendix as free text / future work,
> not as invented `transmission.*` keys.

## Canonical `DTAMetaData` fields — Document
| Field | Type | Example |
|---|---|---|
| metadata.title | string | BIOMARKER GF DATA TRANSFER SPECIFICATIONS (DTS) |
| metadata.version | string | 1.0 |
| metadata.date | date (YYYY-MM-DD) | 2026-07-29 |
| metadata.header | string | Example Pharma Inc. |
| metadata.version_history[] | array<object> | [{version: "1.0", date: "2026-07-29", changes: "Initial trial version"}] |
| metadata.error_handling | string | Format or content errors are reported by email and corrected in a follow-up transfer |
| metadata.authorized_for_corrections | string \| array<string> | TDM |

## Canonical `DTAMetaData` fields — Parties
| Field | Type | Example |
|---|---|---|
| metadata.supplier.affiliation.name | string | External Genomics Supplier |
| metadata.supplier.contacts[] | array<object> | [{name: "Jane Doe", role: "Primary Contact", phone: "+49...", email: "jane@supplier.com"}] |
| metadata.receiver.affiliation.name | string | Our Company |
| metadata.receiver.contacts[] | array<object> | [{name: "Max Mustermann", role: "TDM", phone: "+49...", email: "max@example.com", backup: "..."}] |

Contact records support `name`, `role`, `email`, `phone`, `department`,
`address`, `signature`, `reviewer`, and `backup` (see `inst/extdata/clinical_dta.yaml`).

## Canonical `DTAMetaData` fields — Transmission
These are the only `metadata.transmission.*` keys the app editor renders.
| Field | Type | Example |
|---|---|---|
| metadata.transmission.type | string | Secure EDT server (SSH sFTP) |
| metadata.transmission.frequency | string | one-time |
| metadata.transmission.notification | string | email |
| metadata.transmission.date_first_transfer | string \| date | 3 weeks after first batch of sample delivery |
| metadata.transmission.date_last_transfer | string \| date | 2 weeks after last batch of samples arrive at laboratory |
| metadata.transmission.test_upload | boolean (yes/no) | true |
| metadata.transmission.blinded_transfer | boolean (yes/no) | true |

## Template option → field mapping (`biomarker_gf.dta-template.yaml`)
| Option id | Control | Target field |
|---|---|---|
| title | text combo | metadata.title |
| version | text | metadata.version |
| header | text combo | metadata.header |
| transmission_type | text combo | metadata.transmission.type |
| transmission_frequency | text combo | metadata.transmission.frequency |
| transmission_notification | text combo | metadata.transmission.notification |
| test_upload | boolean (Yes/No) | metadata.transmission.test_upload |
| blinded_transfer | boolean (Yes/No) | metadata.transmission.blinded_transfer |
| date_first_transfer | text combo | metadata.transmission.date_first_transfer |
| date_last_transfer | text combo | metadata.transmission.date_last_transfer |
| error_handling | text combo | metadata.error_handling |
| authorized_for_corrections | text combo | metadata.authorized_for_corrections |

"text combo" options show suggested values in a dropdown that also includes a
"(leave blank)" entry and a "Custom..." entry. Choosing "Custom..." reveals a
text field next to the dropdown for a free-typed value.

## Appendix — GF concepts not modeled in `DTAMetaData`
These appeared in the source `.docx` but have no first-class `DTAMetaData`
property. They are intentionally NOT written as `transmission.*` keys (doing so
would make them invisible in the app editor and document exports). Capture them
in `metadata.error_handling`, contact roles, or dataset column specs, or treat
as future schema work:

- study number (belongs to the `STUDYID` column, not metadata)
- recipient role (model as a contact `role`, e.g. TDM / SciMo)
- data format, file-name specification, supplier / data-type codes, allowed extensions
- transfer extent (e.g. cumulative vs. incremental)
- missing-response handling
- test-upload / blinded-transfer free-text descriptions
- unidentified / unexpected sample handling
