# DTS Metadata Coverage Overview (from 2026-07-28T2221 archive)

## Scope
This overview is based on 28 DTS template documents from:
- C:/Users/schwarz8/Downloads/2026-07-28T2221 - BI OneMed - Clinical.zip
- extracted to C:/Users/schwarz8/workspace/DTAtoolsR_github/.tmp_dts_templates
- plain-text conversions in C:/Users/schwarz8/workspace/DTAtoolsR_github/.tmp_dts_templates/_txt

Goal: define what metadata model must exist so all template cases can be represented.

## Coverage Strategy
Use a two-layer model:
1. Core metadata (required for almost all templates)
2. Case modules (conditionally required, but needed for full portfolio coverage)

## Layer 1: Core Metadata (must implement)
These are the minimum fields needed across virtually all DTS templates.

### A) Document identity and governance
- title
  - type: string
  - example: "Data Transfer Specifications - ECG"
- study_number
  - type: string
  - example: "1234-5678"
- study_numbers
  - type: array<string> (use when one DTS covers multiple studies)
  - example: ["1234-5678", "1234-5679"]
- version
  - type: string
  - example: "6.0"
- date
  - type: date (ISO 8601 string in serialization)
  - example: "2026-07-28"
- header
  - type: string
  - example: "Boehringer Ingelheim (BI)"
- document_version_control
  - type: array<object>
  - example:
    - date: "2026-07-28"
      version: "6.0"
      change_reference: "Updated upload specs and naming conventions"

### B) Parties and contacts
- supplier
  - type: object
  - example:
    - name: "IQVIA"
      role: "Data Supplier"
      contacts:
        - name: "Jane Doe"
          phone: "+49 123 456789"
          email: "jane.doe@supplier.com"
          department: "Clinical Data Operations"
      approval:
        signer_name: "Jane Doe"
        approval_date: "2026-07-15"
- receiver
  - type: object
  - example:
    - name: "Boehringer Ingelheim (BI)"
      role: "Trial Data Manager"
      contacts:
        - name: "Max Mustermann"
          phone: "+49 7351 54 12345"
          email: "max.mustermann@boehringer-ingelheim.com"
      approval:
        signer_name: "Max Mustermann"
        approval_date: "2026-07-16"
- contact_backup
  - type: object
  - example:
    - supplier_backups:
        - name: "Backup Supplier Contact"
          phone: "+49 123 000000"
          email: "backup@supplier.com"
      sponsor_backups:
        - name: "Backup TDM"
          phone: "+49 7351 54 99999"
          email: "backup.tdm@boehringer-ingelheim.com"

### C) Transfer specification
- transfer.media_type
  - type: string
  - example: "BI secure EDT server (sFTP)"
- transfer.data_format
  - type: string
  - example: ".zip containing .csv + manifest.json"
- transfer.frequency
  - type: string
  - example: "Monthly, plus on BI request"
- transfer.extent
  - type: string
  - example: "Cumulative transfer including corrected historical records"
- transfer.test_upload
  - type: object
  - example:
    - required: true
      timing: "When first 2 subjects are available"
      data_kind: "real"
      turnaround_days_if_error: 3
- transfer.error_handling
  - type: string
  - example: "BI rejects full file on structural failure; supplier resubmits within 5 business days"
- transfer.naming_conventions
  - type: string
  - example: "s<study>_<yyyymmddhhmm>_<supplier_code>_<datatype_code>"
- transfer.missing_responses
  - type: string
  - example: "Keep required columns; leave values empty when not available"
- transfer.unscheduled_tests
  - type: string
  - example: "Assign all unscheduled records to VISIT=UNSCHEDULED"
- transfer.notification
  - type: object
  - example:
    - email_required: true
      notes: "Notify BI TDM after each upload"

### D) File package and naming
- file_naming.transfer_pattern
  - type: string
  - example: "s1234_5678_202405031513_abc_oth.zip"
- file_naming.source_dataset_names
  - type: array<object>
  - example:
    - package_member: "eg.csv"
      source_dataset_name: "eg"
    - package_member: "manifest.json"
      source_dataset_name: "manifest"
- file_naming.file_extension_rules
  - type: array<string>
  - example: [".sas7bdat", ".xpt", ".txt", ".csv", ".zip"]
- data_type_code
  - type: string
  - example: "oth"
- supplier_code
  - type: string
  - example: "abc"

## Layer 2: Case Modules (must support to cover all cases)
These are not universal, but at least one template requires each module.

### 1) Veeva/manifest package module
Needed in multiple Veeva templates.
- manifest.required
  - type: boolean
  - example: true
- manifest.filename
  - type: string
  - example: "manifest.json"
- manifest.source_rules
  - type: string
  - example: "source field must match supplier system and DTS file map"
- manifest.trial_env_adaptations
  - type: object
  - example:
    - uat_study_suffix: "_TST5"
      prod_uses_real_studyid: true

### 2) Biomarker blinding module
Needed for GF/YC/MI/CP/IS/MB-like templates.
- blinded_transfer.required
  - type: boolean
  - example: true
- blinded_transfer.file_spec
  - type: string
  - example: "Blinded transfer file includes all received samples"
- blinded_transfer.content_scope
  - type: string
  - example: "Include sample status and identifiers without unblinding payload"
- blinded_transfer.timing
  - type: string
  - example: "Weekly during blinded conduct phase"
- unidentified_samples.handling
  - type: string
  - example: "Provide in separate listing and notify TDM"

### 3) Laboratory extensions module
Needed for LB templates.
- loinc.version
  - type: string
  - example: "LOINC 2.73"
- dili.measurement_rules
  - type: string
  - example: "Include triggered DILI panel measurements per protocol"
- non_protocol_tests.handling
  - type: string
  - example: "Exclude tests not specified in protocol"
- discontinued_subjects.handling
  - type: string
  - example: "Transmit data collected up to discontinuation only"

### 4) Adjudication module
Needed for adjudication template.
- re_adjudications.rule
  - type: string
  - example: "Latest adjudication overwrites prior adjudication result"
- event_overwrite_logic
  - type: string
  - example: "Resend full cumulative file with corrected adjudication rows"
- adjudication_scope
  - type: string
  - example: "FA-domain adjudication committee outcomes only"

### 5) Narrative outbound package module
Needed for patient narrative template.
- purpose_of_data_transmission
  - type: string
  - example: "Provide SDTM package for patient narrative writing"
- subjects_included_rule
  - type: string
  - example: "Include all subjects, including screening failures"
- sdtm_domain_inclusion
  - type: array<string>
  - example: ["dm", "ae", "lb", "mh", "cm", "pr", "ec", "ds", "se", "co", "suppdm"]
- zip_content_spec
  - type: object
  - example:
    - zip_name_pattern: "s<study>_<yyyymmdd>_bi_pn.zip"
      includes: ["*.xpt", "acrf.pdf"]
- post_final_transfer_update_rule
  - type: string
  - example: "Any SDTM update after final transfer requires CRO alignment before resend"

### 6) aECG waveform module
Needed for aECG waveform template.
- waveform.format
  - type: enum<string>
  - example: "XML_aECG" (allowed: "XML_aECG", "PDF_scanned")
- zip_file_naming_convention
  - type: string
  - example: "s1234_5678_202412031513_xml_aeg.zip"
- individual_waveform_file_naming
  - type: string
  - example: "1234-5678-USA10-001-113015169.xml"
- waveform_content_spec
  - type: object
  - example:
    - trial_name: "1234-5678"
      subject_id: "USA10-001"
      egrefid: "113015169"
      ecg_datetime: "2026-01-12T09:42:00"
      timepoint_event: "VISIT02_PRE_DOSE_1"

### 7) Oncology imaging module
Needed for oncology image template.
- analysis_criterion
  - type: enum<string>
  - example: "RECIST 1.1" (allowed examples: "RECIST 1.1", "iRECIST", "RANO-BM")
- image_transfer_type
  - type: string
  - example: "Cumulative TU/TR/RS transfer"
- oncology_domain_file_prefixes
  - type: object
  - example:
    - tu_prefix: "tuimg"
      tr_prefix: "trimg"
      rs_prefix: "rsimg"
- lesion_assessment_rules
  - type: string
  - example: "Handle split/merged lesions with stable TULNKID/TRLNKID references"

### 8) Re-randomization/unblinded transfer module
Needed for IRT re-randomization template.
- unblinded_transfer.trigger
  - type: string
  - example: "After TDM confirms Treatment Information Release approval"
- unblinded_transfer.purpose
  - type: string
  - example: "Provide re-randomization numbers only"
- unblinded_transfer.timeline
  - type: string
  - example: "Immediate transfer after EDC lock and unblinding"
- rerandomization_identifier_rules
  - type: object
  - example:
    - domain: "XI"
      identifier_field: "XIREFID"
      term_value: "RANDOMIZED TO TREATMENT"

## Recommended canonical metadata structure
A practical top-level layout that can represent all templates:

- document:
  - title, study_number, version, date, header, version_control[]
- parties:
  - supplier, receiver, contact_backup
- transfer:
  - universal transfer fields (media_type, data_format, frequency, extent, test_upload, error_handling, naming_conventions, ...)
- file_spec:
  - transfer filename patterns and contained file mapping
- modules:
  - veeva_manifest
  - biomarker_blinding
  - laboratory_extensions
  - adjudication
  - narrative_package
  - aecg_waveform
  - oncology_imaging
  - rerandomization_unblinded

## Mapping to current DTAtools metadata
Current DTAMetaData already covers part of Layer 1:
- existing: title, version, date, header, version_history, receiver, supplier, transmission, error_handling, authorized_for_corrections
- gaps for full template coverage are mostly:
  - explicit study_number
  - richer transfer sub-structure (naming/frequency/extent/test upload as structured fields)
  - module containers for case-specific requirements listed above

## Implementation minimum to claim full coverage
To say all 28 templates are covered, implement:
- all Layer 1 fields
- all eight Layer 2 modules (optional at runtime, but available in schema)
- validation that required module fields are present only when that module is enabled

## Notes
- Some templates use DOCUMENT CONTROL, others DOCUMENT VERSION CONTROL; model these as one normalized concept.
- UPLOAD SPECIFICATIONS and Transmission Specifications are semantically the same section and should be normalized into one transfer object.
