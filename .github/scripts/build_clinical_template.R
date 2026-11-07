# Rebuild inst/extdata/word-templates/clinical_dta_template.docx from source.
#
#   Rscript .github/scripts/build_clinical_template.R
#
# The template is written as raw WordprocessingML rather than edited in Word,
# so it can be reviewed as a diff and every style it defines is deliberate.
# Every placeholder is typed into a single run on purpose: Word splits text it
# edits across runs, and a split placeholder loses its run formatting when
# export_with_template() joins it back together.
#
# Colours and fonts follow THEME_COLORS / FONTS in R/formattingHelpers.R, so the
# template's own text agrees with the flextables the block placeholders render.
#
# After rebuilding, re-run .github/scripts/sync_qualification_manifest.R: the
# file is under inst/, so the qualification manifest pins its checksum.

out <- file.path("inst", "extdata", "word-templates", "clinical_dta_template.docx")

# ---- Palette (R/formattingHelpers.R) ---------------------------------------
green <- "00625B"
accent <- "00A886"
tint <- "E6F1EF"
ink <- "1A2B2A"
muted <- "5B6B6A"
paper <- "F4F7F6"
hair <- "CDD5D4"

# A4, 2 cm side margins: 11906 - 2 * 1134 = 9638 twips of text width.
text_w <- 9638L

# ---- WordprocessingML helpers ----------------------------------------------
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# rpr: raw <w:rPr> children.
r <- function(text, rpr = "") {
  sprintf(
    '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>',
    if (nzchar(rpr)) paste0("<w:rPr>", rpr, "</w:rPr>") else "",
    esc(text)
  )
}

field <- function(instr, rpr = "") {
  rp <- if (nzchar(rpr)) paste0("<w:rPr>", rpr, "</w:rPr>") else ""
  paste0(
    "<w:r>", rp, '<w:fldChar w:fldCharType="begin"/></w:r>',
    "<w:r>", rp, '<w:instrText xml:space="preserve"> ', instr, " </w:instrText></w:r>",
    "<w:r>", rp, '<w:fldChar w:fldCharType="separate"/></w:r>',
    "<w:r>", rp, "<w:t>1</w:t></w:r>",
    "<w:r>", rp, '<w:fldChar w:fldCharType="end"/></w:r>'
  )
}

# runs: character vector of run XML, or a plain string wrapped in one run.
p <- function(runs = "", style = NULL, ppr = "") {
  if (length(runs) == 1L && !startsWith(runs, "<w:r>")) runs <- r(runs)
  st <- if (is.null(style)) "" else sprintf('<w:pStyle w:val="%s"/>', style)
  pp <- paste0(st, ppr)
  paste0("<w:p>", if (nzchar(pp)) paste0("<w:pPr>", pp, "</w:pPr>"), paste(runs, collapse = ""), "</w:p>")
}

border <- function(side, color, sz = 4, space = 0) {
  sprintf('<w:%s w:val="single" w:sz="%d" w:space="%d" w:color="%s"/>', side, sz, space, color)
}
no_border <- function(side) sprintf('<w:%s w:val="nil"/>', side)

cell <- function(content, width, tcpr = "") {
  sprintf(
    '<w:tc><w:tcPr><w:tcW w:w="%d" w:type="dxa"/>%s</w:tcPr>%s</w:tc>',
    width, tcpr, paste(content, collapse = "")
  )
}

# A borderless layout table; borders are added per cell where wanted.
tbl <- function(rows, widths, margins = c(top = 60, left = 100, bottom = 60, right = 100), tblpr = "") {
  mar <- paste0(
    "<w:tblCellMar>",
    sprintf('<w:top w:w="%d" w:type="dxa"/>', margins[["top"]]),
    sprintf('<w:left w:w="%d" w:type="dxa"/>', margins[["left"]]),
    sprintf('<w:bottom w:w="%d" w:type="dxa"/>', margins[["bottom"]]),
    sprintf('<w:right w:w="%d" w:type="dxa"/>', margins[["right"]]),
    "</w:tblCellMar>"
  )
  bords <- paste0(
    "<w:tblBorders>",
    paste(vapply(c("top", "left", "bottom", "right", "insideH", "insideV"), no_border, ""), collapse = ""),
    "</w:tblBorders>"
  )
  grid <- paste0("<w:tblGrid>", paste(sprintf('<w:gridCol w:w="%d"/>', widths), collapse = ""), "</w:tblGrid>")
  paste0(
    "<w:tbl><w:tblPr>",
    sprintf('<w:tblW w:w="%d" w:type="dxa"/>', sum(widths)),
    '<w:tblLayout w:type="fixed"/>', bords, mar, tblpr,
    '<w:tblLook w:val="0000" w:firstRow="0" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="1" w:noVBand="1"/>',
    "</w:tblPr>", grid,
    paste(vapply(rows, function(cells) paste0("<w:tr><w:trPr><w:cantSplit/></w:trPr>", paste(cells, collapse = ""), "</w:tr>"), ""), collapse = ""),
    "</w:tbl>"
  )
}

shade <- function(fill) sprintf('<w:shd w:val="clear" w:color="auto" w:fill="%s"/>', fill)

# ---- Body ------------------------------------------------------------------

# Cover: eyebrow, title, subtitle -- no full-page cover, the agreement starts
# on page one.
cover <- c(
  p(r("{DTA_HEADER}"), "Eyebrow"),
  p(r("{DTA_TITLE}"), "Title"),
  p("Data Transfer Agreement  ·  Data Transmission Specification", "Subtitle")
)

# Key-facts strip: label over value, a heavy accent rule on top.
fact <- function(label, value, width) {
  cell(
    c(p(label, "Label"), p(r(value), "Fact")),
    width,
    paste0("<w:tcBorders>", border("top", accent, 18, 0), border("bottom", hair, 4, 0), "</w:tcBorders>")
  )
}
fw <- c(1700L, 2100L, 1946L, 1946L, 1946L)
facts <- tbl(
  list(c(
    fact("VERSION", "{DTA_VERSION}", fw[1]),
    fact("EFFECTIVE DATE", "{DTA_DATE}", fw[2]),
    fact("DATASETS", "{DATASET_COUNT}", fw[3]),
    fact("COLUMNS", "{TOTAL_COLUMNS}", fw[4]),
    fact("VALIDATION RULES", "{TOTAL_RULES}", fw[5])
  )),
  fw,
  margins = c(top = 90, left = 0, bottom = 90, right = 120)
)

# Party cards: tinted panel with an accent bar on the left.
party <- function(label, prefix, width) {
  cell(
    c(
      p(label, "Label"),
      p(r(sprintf("{%s_NAME}", prefix)), "PartyName"),
      p(r(sprintf("{%s_ADDRESS}", prefix)), "Small"),
      p(r(sprintf("{%s_COUNTRY}", prefix)), "Small")
    ),
    width,
    paste0(
      shade(paper),
      "<w:tcBorders>", border("left", accent, 24, 0), "</w:tcBorders>"
    )
  )
}
gap <- 240L
pw <- (text_w - gap) %/% 2L
parties <- tbl(
  list(c(
    party("SUPPLIER  ·  DATA PROVIDER", "SUPPLIER", pw),
    cell(p(""), gap),
    party("RECEIVER  ·  DATA RECIPIENT", "RECEIVER", pw)
  )),
  c(pw, gap, pw),
  margins = c(top = 120, left = 180, bottom = 120, right = 180)
)

# Transmission: two label/value pairs per row, hairline row rules.
kv_w <- c(1800L, 3019L, 1800L, 3019L)
kv_row <- function(l1, v1, l2, v2, last = FALSE) {
  b <- paste0("<w:tcBorders>", border("bottom", hair, 4, 0), "</w:tcBorders>")
  c(
    cell(p(l1, "Label", '<w:spacing w:before="20" w:after="0"/>'), kv_w[1], b),
    cell(p(r(v1), "Value"), kv_w[2], b),
    cell(p(l2, "Label", '<w:spacing w:before="20" w:after="0"/>'), kv_w[3], b),
    cell(p(r(v2), "Value"), kv_w[4], b)
  )
}
transmission <- tbl(
  list(
    kv_row("METHOD", "{TRANSMISSION_TYPE}", "FREQUENCY", "{TRANSMISSION_FREQUENCY}"),
    kv_row("FIRST TRANSFER", "{TRANSMISSION_FIRST_TRANSFER}", "LAST TRANSFER", "{TRANSMISSION_LAST_TRANSFER}"),
    kv_row("NOTIFICATION", "{TRANSMISSION_NOTIFICATION}", "TEST UPLOAD", "{TEST_UPLOAD}"),
    kv_row("BLINDED", "{BLINDED_TRANSFER}", "GENERATED", "{GENERATED_DATE}")
  ),
  kv_w,
  margins = c(top = 70, left = 0, bottom = 70, right = 120)
)

# The free-text transmission fields can run to a sentence or more, so each
# gets a full-width row.
wide_w <- c(kv_w[1], text_w - kv_w[1])
wide_row <- function(label, value) {
  b <- paste0("<w:tcBorders>", border("bottom", hair, 4, 0), "</w:tcBorders>")
  c(
    cell(p(label, "Label", '<w:spacing w:before="20" w:after="0"/>'), wide_w[1], b),
    cell(p(r(value), "Value"), wide_w[2], b)
  )
}
transmission_text <- tbl(
  list(
    wide_row("EXTENT", "{TRANSMISSION_EXTENT}"),
    wide_row("SCOPE", "{TRANSMISSION_SCOPE}"),
    wide_row("PACKAGING", "{TRANSMISSION_PACKAGING}"),
    wide_row("SECURITY", "{TRANSMISSION_SECURITY}"),
    wide_row("CHECKSUM", "{TRANSMISSION_CHECKSUM}"),
    wide_row("TEST REQUIREMENTS", "{TRANSMISSION_TEST_REQUIREMENTS}"),
    wide_row("BLINDING", "{TRANSMISSION_BLINDING_REQUIREMENTS}")
  ),
  wide_w,
  margins = c(top = 70, left = 0, bottom = 70, right = 120)
)

# Per-dataset meta line inside the region.
mono <- '<w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/>'
dot <- r("   ·   ", sprintf('<w:color w:val="%s"/>', hair))
ds_meta <- p(
  c(
    r("{DATASET_ID}", paste0(mono, sprintf('<w:color w:val="%s"/>', green))),
    dot, r("{DATASET_FILE_COUNT}"), r(" file pattern(s)"),
    dot, r("{DATASET_COLUMN_COUNT}"), r(" columns"),
    dot, r("{DATASET_RULE_COUNT}"), r(" rules")
  ),
  "Meta"
)

# Section 1 (one column, title page) ends at the paragraph carrying its sectPr;
# the appendix section after it sets the YAML in two newspaper columns.
sect <- function(cols = 1L, first = FALSE, type = NULL) {
  paste0(
    "<w:sectPr>",
    '<w:headerReference w:type="default" r:id="rId4"/>',
    if (first) '<w:headerReference w:type="first" r:id="rId5"/>',
    '<w:footerReference w:type="default" r:id="rId6"/>',
    if (first) '<w:footerReference w:type="first" r:id="rId6"/>',
    if (!is.null(type)) sprintf('<w:type w:val="%s"/>', type),
    '<w:pgSz w:w="11906" w:h="16838"/>',
    '<w:pgMar w:top="1247" w:right="1134" w:bottom="1134" w:left="1134" w:header="567" w:footer="454" w:gutter="0"/>',
    sprintf('<w:cols w:num="%d" w:space="397"/>', cols),
    if (first) "<w:titlePg/>",
    "</w:sectPr>"
  )
}

body <- c(
  cover,
  facts,
  p("", "Spacer"),
  parties,

  # 1 ------------------------------------------------------------------------
  p("Agreement & Parties", "Heading1"),
  p(r("{STUDY_TABLE}")),
  p(r("{AGREEMENT_SECTIONS}")),
  p(r("{SUPPLIER_CONTACTS_TABLE}")),
  p(r("{RECEIVER_CONTACTS_TABLE}")),

  # 2 ------------------------------------------------------------------------
  p("Data Transmission", "Heading1"),
  transmission,
  transmission_text,
  p("Error Handling", "Heading2"),
  p(r("{ERROR_HANDLING}")),
  p(r("{AUTHORIZED_CORRECTIONS_LIST}")),

  # 3 ------------------------------------------------------------------------
  p("Datasets", "Heading1"),
  p(c(
    r("This agreement covers "), r("{DATASET_COUNT}", "<w:b/>"), r(" dataset(s): "),
    r("{DATASET_NAMES}"), r(". Each section below gives the expected files, the column specification and the validation rules the transfer is checked against.")
  ), "Lead"),
  p(r("{#DATASETS}")),
  p(r("{DATASET_NAME}"), "Heading2"),
  ds_meta,
  p(r("{DATASETS}")),
  p(r("{/DATASETS}")),

  # 4 ------------------------------------------------------------------------
  p("Governance", "Heading1"),
  p(r("{VERSION_HISTORY_TABLE}")),
  p(r("{SIGNATURES_TABLE}")),

  # Appendix -----------------------------------------------------------------
  p("Appendix · Machine-Readable Specification", "Appendix"),
  p(c(
    r("The complete YAML specification this document was generated from. It is the authoritative, "),
    r("machine-readable form of the agreement and can be re-imported with "),
    r("read_dta_from_yaml()", mono), r(".")
  ), "Lead", sect(1L, first = TRUE)),
  p(r("{YAML_EMBEDDED}"), "Code"),
  sect(2L, type = "continuous")
)

ns <- paste(
  'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"',
  'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"'
)

document_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:document ", ns, "><w:body>",
  # The final sectPr is a direct child of w:body, not a paragraph.
  paste(head(body, -1L), collapse = ""), tail(body, 1L),
  "</w:body></w:document>"
)

# ---- Header / footer -------------------------------------------------------
right_tab <- sprintf('<w:tabs><w:tab w:val="right" w:pos="%d"/></w:tabs>', text_w)
tab <- "<w:r><w:tab/></w:r>"

header_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:hdr ", ns, ">",
  p(
    c(
      r("{DTA_TITLE}", sprintf('<w:b/><w:color w:val="%s"/>', green)),
      tab,
      r("Version "), r("{DTA_VERSION}"), r("   ·   "), r("{DTA_DATE}")
    ),
    "Header",
    right_tab
  ),
  "</w:hdr>"
)

header_first_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:hdr ", ns, ">", p("", "HeaderFirst"), "</w:hdr>"
)

footer_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:ftr ", ns, ">",
  p(
    c(
      r("{DTA_HEADER}"), r("   ·   Generated "), r("{GENERATED_DATE}"),
      tab,
      r("Page "), field("PAGE", sprintf('<w:b/><w:color w:val="%s"/>', green)),
      r(" of "), field("NUMPAGES")
    ),
    "Footer",
    right_tab
  ),
  "</w:ftr>"
)

# ---- Styles ----------------------------------------------------------------
font <- function(f) sprintf('<w:rFonts w:ascii="%1$s" w:hAnsi="%1$s" w:eastAsia="%1$s" w:cs="%1$s"/>', f)
sz <- function(pt) sprintf('<w:sz w:val="%1$d"/><w:szCs w:val="%1$d"/>', as.integer(round(pt * 2)))
col <- function(x) sprintf('<w:color w:val="%s"/>', x)
spacing <- function(before = 0, after = 0, line = 264) {
  sprintf('<w:spacing w:before="%d" w:after="%d" w:line="%d" w:lineRule="auto"/>', before, after, line)
}

pstyle <- function(id, name, ppr = "", rpr = "", based = "Normal", nxt = NULL, extra = "") {
  sprintf(
    '<w:style w:type="paragraph" w:styleId="%s"><w:name w:val="%s"/>%s%s%s<w:qFormat/><w:pPr>%s</w:pPr><w:rPr>%s</w:rPr></w:style>',
    id, name,
    if (is.null(based)) "" else sprintf('<w:basedOn w:val="%s"/>', based),
    if (is.null(nxt)) "" else sprintf('<w:next w:val="%s"/>', nxt),
    extra, ppr, rpr
  )
}

styles_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:styles ", ns, ">",
  "<w:docDefaults><w:rPrDefault><w:rPr>", font("Calibri"), sz(10), col(ink),
  '<w:lang w:val="en-GB" w:eastAsia="en-US" w:bidi="ar-SA"/></w:rPr></w:rPrDefault>',
  "<w:pPrDefault><w:pPr>", spacing(0, 100), "</w:pPr></w:pPrDefault></w:docDefaults>",
  # officer resolves an unstyled paragraph through the default style, and
  # errors when no paragraph style is flagged as the default.
  sub('w:styleId="Normal"', 'w:default="1" w:styleId="Normal"', pstyle("Normal", "Normal", based = NULL), fixed = TRUE),
  pstyle("Title", "Title", spacing(0, 40, 240), paste0("<w:b/>", sz(26), col(green)), nxt = "Subtitle"),
  pstyle("Subtitle", "Subtitle", spacing(0, 260), paste0(sz(11), col(muted))),
  pstyle("Eyebrow", "Eyebrow", spacing(0, 60), paste0("<w:b/><w:caps/>", '<w:spacing w:val="30"/>', sz(8), col(accent))),
  pstyle(
    "Heading1", "heading 1",
    paste0(
      "<w:keepNext/><w:keepLines/>", spacing(380, 140, 240),
      '<w:numPr><w:ilvl w:val="0"/><w:numId w:val="1"/></w:numPr>',
      "<w:pBdr>", border("bottom", accent, 8, 4), "</w:pBdr>",
      '<w:outlineLvl w:val="0"/>'
    ),
    paste0("<w:b/>", sz(15), col(green)),
    nxt = "Normal"
  ),
  pstyle(
    "Heading2", "heading 2",
    paste0(
      "<w:keepNext/><w:keepLines/>", spacing(200, 80, 240),
      '<w:numPr><w:ilvl w:val="1"/><w:numId w:val="1"/></w:numPr>',
      '<w:outlineLvl w:val="1"/>'
    ),
    paste0("<w:b/>", sz(12), col(ink)),
    nxt = "Normal"
  ),
  pstyle(
    "Heading3", "heading 3",
    paste0("<w:keepNext/>", spacing(180, 60, 240), '<w:outlineLvl w:val="2"/>'),
    paste0("<w:b/><w:caps/>", '<w:spacing w:val="20"/>', sz(8.5), col(muted)),
    nxt = "Normal"
  ),
  pstyle(
    "Appendix", "Appendix Heading",
    paste0("<w:keepNext/><w:pageBreakBefore/>", spacing(0, 140, 240), "<w:pBdr>", border("bottom", accent, 8, 4), "</w:pBdr>", '<w:outlineLvl w:val="0"/>'),
    paste0("<w:b/>", sz(15), col(green)),
    nxt = "Normal"
  ),
  pstyle("Lead", "Lead", spacing(0, 160, 276), paste0(sz(10), col(muted))),
  pstyle("Label", "Label", spacing(0, 20, 240), paste0("<w:b/><w:caps/>", '<w:spacing w:val="16"/>', sz(7), col(muted))),
  pstyle("Fact", "Fact", spacing(0, 0, 240), paste0("<w:b/>", sz(13), col(green))),
  pstyle("Value", "Value", spacing(0, 0, 252), paste0(sz(9.5), col(ink))),
  pstyle("PartyName", "Party Name", spacing(40, 40, 240), paste0("<w:b/>", sz(12), col(ink))),
  pstyle("Small", "Small", spacing(0, 0, 252), paste0(sz(8.5), col(muted))),
  pstyle("Meta", "Dataset Meta", spacing(0, 120, 240), paste0(sz(8.5), col(muted))),
  pstyle("Spacer", "Spacer", spacing(0, 0, 240), sz(6)),
  pstyle(
    "Code", "YAML Code",
    paste0(
      spacing(0, 0, 228), shade(paper),
      "<w:pBdr>", border("left", accent, 12, 6), "</w:pBdr>",
      '<w:ind w:left="120"/>'
    ),
    paste0(font("Consolas"), sz(6), col(ink))
  ),
  pstyle("Header", "header", paste0(spacing(0, 0, 240), "<w:pBdr>", border("bottom", hair, 4, 4), "</w:pBdr>"), paste0(sz(8), col(muted))),
  pstyle("HeaderFirst", "Header First", spacing(0, 0, 240), sz(8)),
  pstyle("Footer", "footer", spacing(0, 0, 240), paste0(sz(7.5), col(muted))),
  # officer renders the agreement-section clauses and generic headings through
  # these names; keep them defined so a block never falls back to plain text.
  '<w:style w:type="character" w:default="1" w:styleId="DefaultParagraphFont"><w:name w:val="Default Paragraph Font"/><w:uiPriority w:val="1"/><w:semiHidden/></w:style>',
  '<w:style w:type="table" w:default="1" w:styleId="TableNormal"><w:name w:val="Normal Table"/><w:semiHidden/><w:tblPr><w:tblInd w:w="0" w:type="dxa"/>',
  '<w:tblCellMar><w:top w:w="0" w:type="dxa"/><w:left w:w="108" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:right w:w="108" w:type="dxa"/></w:tblCellMar></w:tblPr></w:style>',
  '<w:style w:type="numbering" w:default="1" w:styleId="NoList"><w:name w:val="No List"/><w:semiHidden/></w:style>',
  "</w:styles>"
)

# ---- Heading numbering: "1" and "1.1" in the accent colour ----------------
lvl <- function(i, text, style) {
  sprintf(
    paste0(
      '<w:lvl w:ilvl="%d"><w:start w:val="1"/><w:numFmt w:val="decimal"/><w:pStyle w:val="%s"/>',
      '<w:lvlText w:val="%s"/><w:lvlJc w:val="left"/><w:pPr><w:ind w:left="567" w:hanging="567"/></w:pPr>',
      "<w:rPr>%s</w:rPr></w:lvl>"
    ),
    i, style, text, col(accent)
  )
}
numbering_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:numbering ", ns, ">",
  '<w:abstractNum w:abstractNumId="0"><w:multiLevelType w:val="multilevel"/>',
  lvl(0, "%1", "Heading1"), lvl(1, "%1.%2", "Heading2"),
  "</w:abstractNum>",
  '<w:num w:numId="1"><w:abstractNumId w:val="0"/></w:num>',
  "</w:numbering>"
)

settings_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  "<w:settings ", ns, ">",
  '<w:zoom w:percent="110"/><w:defaultTabStop w:val="709"/>',
  '<w:characterSpacingControl w:val="doNotCompress"/>',
  '<w:compat><w:compatSetting w:name="compatibilityMode" w:uri="http://schemas.microsoft.com/office/word" w:val="15"/></w:compat>',
  "</w:settings>"
)

content_types <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
  '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
  '<Default Extension="xml" ContentType="application/xml"/>',
  '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
  '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>',
  '<Override PartName="/word/settings.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/>',
  '<Override PartName="/word/numbering.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/>',
  '<Override PartName="/word/header1.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>',
  '<Override PartName="/word/header2.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>',
  '<Override PartName="/word/footer1.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"/>',
  '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>',
  "</Types>"
)

root_rels <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
  '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>',
  "</Relationships>"
)

rel <- function(id, type, target) {
  sprintf(
    '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/%s" Target="%s"/>',
    id, type, target
  )
}
doc_rels <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  rel("rId1", "styles", "styles.xml"),
  rel("rId2", "settings", "settings.xml"),
  rel("rId3", "numbering", "numbering.xml"),
  rel("rId4", "header", "header1.xml"),
  rel("rId5", "header", "header2.xml"),
  rel("rId6", "footer", "footer1.xml"),
  "</Relationships>"
)

core_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" ',
  'xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
  "<dc:title>Data Transfer Agreement</dc:title><dc:creator>DTAtools</dc:creator>",
  "</cp:coreProperties>"
)

# ---- Package ---------------------------------------------------------------
parts <- list(
  "[Content_Types].xml" = content_types,
  "_rels/.rels" = root_rels,
  "docProps/core.xml" = core_xml,
  "word/document.xml" = document_xml,
  "word/styles.xml" = styles_xml,
  "word/numbering.xml" = numbering_xml,
  "word/settings.xml" = settings_xml,
  "word/header1.xml" = header_xml,
  "word/header2.xml" = header_first_xml,
  "word/footer1.xml" = footer_xml,
  "word/_rels/document.xml.rels" = doc_rels
)

stage <- tempfile("clinical_template_")
for (nm in names(parts)) {
  path <- file.path(stage, nm)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Every part must parse; a malformed one makes Word refuse the whole file.
  xml2::read_xml(parts[[nm]])
  con <- file(path, open = "wb")
  writeBin(charToRaw(enc2utf8(parts[[nm]])), con)
  close(con)
}
# Fixed timestamps make the archive byte-identical across rebuilds, so an
# unchanged template does not churn its qualification-manifest checksum.
Sys.setFileTime(file.path(stage, names(parts)), as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
if (file.exists(out)) unlink(out)
zip::zip(normalizePath(out, mustWork = FALSE), files = names(parts), root = stage, include_directories = FALSE)
unlink(stage, recursive = TRUE)
cat("Wrote", out, "\n")
