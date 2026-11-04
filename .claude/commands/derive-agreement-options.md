---
description: Derive source-backed transfer-medium choices and a DTAtools template YAML fragment from a DTS or agreement.
argument-hint: "[DTS/agreement file, template family, or 'apply <template file>']"
---

Derive applicable transfer-medium options for the agreement or DTS named by
the user or `$ARGUMENTS`. Use the source agreement as the authority; the
repository's `dts_transfer_medium_options.md`, when present, is a curated
catalog and evidence index, not a universal policy or a substitute for the
source.

1. Identify the source agreement and, when provided, the target creation
   template. If either is ambiguous or unavailable, ask for the missing input
   rather than guessing. Read the source's media/transfer clauses and any
   relevant restrictions, then check the corresponding entries in
   `dts_transfer_medium_options.md` and the target template's existing option
   conventions.
2. Derive only choices supported by the applicable source. Preserve service
   names and protocol distinctions (for example, FTPS is not sFTP), and carry
   forward restrictions, preferences, approval conditions, and exclusions.
   Do not add a medium merely because it appears in the general catalog.
3. Return:
   - a concise list of the applicable choices and the evidence supporting each;
   - a paste-ready YAML option for `metadata.transmission.type`, using the
     template's established option conventions;
   - when source-backed security or checksum wording is available and should
     vary by choice, an `effects` mapping that sets
     `metadata.transmission.type`, `metadata.transmission.security`, and
     `metadata.transmission.checksum` in every branch, preventing wording from
     a prior selection from being retained; and
   - unresolved details the agreement author must supply, such as a concrete
     endpoint, access arrangements, approvals, or checksum procedure.
4. Cite source file paths and page/section/line references when available.
   Separate source requirements from recommendations and assumptions. State
   when a cited SOP, KMED asset, or DTS Catalog was unavailable instead of
   inferring its contents.
5. Keep notification, frequency, file format, packaging, test uploads, and
   blinding separate from the transfer-medium choice. Use only existing
   `metadata.transmission` fields; do not invent schema fields. Choice labels
   and help text describe requirements but do not prove or enforce compliance.
6. Present the result for review without editing a template. If the user
   explicitly asks to apply it or uses `apply <template file>`, make the
   smallest edit to that file, preserve unrelated content, then validate the
   YAML and the template with the repository's existing validation path. Show
   the diff and report validation results.

When the source does not establish a restriction or control, say so plainly
instead of turning absence of evidence into an exemption.
