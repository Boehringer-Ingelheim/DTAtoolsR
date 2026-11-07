# export-templates

Word templates (`.docx`) for exporting a DTA as a Word document. A Word
template is an ordinary Word file containing `{PLACEHOLDER}` markers (for
example `{DTA_TITLE}`) that the export fills with the DTA. It is not a
styles-only reference document. PDF export does not use these files: it goes
through Markdown and pandoc.

- Every `.docx` file placed directly in this folder that contains at least one
  `{PLACEHOLDER}` marker is offered automatically in the app's export dialog.
  The label is the file name without its extension. A `.docx` without any
  marker would export a Word file without your DTA in it, so the app skips it
  and logs a warning.
- To change a label or the order, list the file under `exports.word_templates`
  in `../dta-app.yaml`, with a path such as `export-templates/report.docx`. A
  file listed there is offered whatever it contains.
- The placeholders a Word template can contain are documented in the package
  vignette.
- Only `.docx` files are used. Anything else in this folder is ignored (and
  reported as a warning by `validate_template()`), apart from this README.
