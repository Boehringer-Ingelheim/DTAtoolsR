pkgname <- "DTAtools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "DTAtools-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('DTAtools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("DTA")
### * DTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTA
### Title: DTA Class
### Aliases: DTA

### ** Examples


# Create sample tables
table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))

# List of tables
tables <- list(table1 = table1, table2 = table2)

# Create the DTADataSet object
data_obj <- DTADataSetTabular(
  name = "example",
  specs = create_example_DTAColumnSpecCollection(1),
  tables = tables
)

DTA(
  datasets = list(data = data_obj),
  metadata = create_example_DTAMetaData()
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAColumnSpec")
### * DTAColumnSpec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAColumnSpec
### Title: DTA Column Format Class
### Aliases: DTAColumnSpec

### ** Examples

col_format <- DTAColumnSpec(
  id = "STUDYID", type = "SAS Char", nullable = FALSE, values = "1234-1234"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAColumnSpec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAColumnSpecCollection")
### * DTAColumnSpecCollection

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAColumnSpecCollection
### Title: DTAColumnSpecCollection Class
### Aliases: DTAColumnSpecCollection

### ** Examples

col1 <- DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = TRUE)
col2 <- DTAColumnSpec(id = "VISIT", type = "SAS Char", nullable = FALSE)
collection <- DTAColumnSpecCollection(columns = list(STUDYID = col1, VISIT = col2))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAColumnSpecCollection", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAColumnSpecStructure")
### * DTAColumnSpecStructure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAColumnSpecStructure
### Title: DTA Column Spec Structure
### Aliases: DTAColumnSpecStructure

### ** Examples

 DTAColumnSpecStructure(type = "Char", format = "$12.", length = 12, backend = "SAS")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAColumnSpecStructure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAColumnSpecStructureFactory")
### * DTAColumnSpecStructureFactory

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAColumnSpecStructureFactory
### Title: Create a DTAColumnSpecStructure Object
### Aliases: DTAColumnSpecStructureFactory

### ** Examples

library(DTAtools)
DTAColumnSpecStructureFactory(type = "SAS Char", format = "SAS $10.", length = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAColumnSpecStructureFactory", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAColumnSpecStructureSAS")
### * DTAColumnSpecStructureSAS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAColumnSpecStructureSAS
### Title: DTA Column Spec Structure SAS
### Aliases: DTAColumnSpecStructureSAS

### ** Examples

 DTAColumnSpecStructureSAS(type = "Char", format = "$12.", length = 12)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAColumnSpecStructureSAS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTADataSet")
### * DTADataSet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTADataSet
### Title: DTADataSet Class
### Aliases: DTADataSet

### ** Examples

ds <- DTADataSet(
  name = "example_dataset",
  type = "file",
  files = list(create_example_DTAFileCSV())
)
ds



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTADataSet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTADataSetFactory")
### * DTADataSetFactory

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTADataSetFactory
### Title: Create a DTADataSetFactory Object
### Aliases: DTADataSetFactory

### ** Examples

library(DTAtools)
DTADataSetFactory(
  type = "file",
  name = "mydataset",
  files = list(type = "csv", filename = "clinical_data.csv")
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTADataSetFactory", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTADataSetTabular")
### * DTADataSetTabular

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTADataSetTabular
### Title: DTADataSetTabular Class
### Aliases: DTADataSetTabular

### ** Examples

# Create sample tables
table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))

# List of tables
tables <- list(table1 = table1, table2 = table2)

# Create the DTADataSetTabular object
data_obj <- DTADataSetTabular(
  name = "example",
  specs = create_example_DTAColumnSpecCollection(1),
  tables = tables
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTADataSetTabular", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAFile-class")
### * DTAFile-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAFile-class
### Title: DTAFile Class
### Aliases: DTAFile-class DTAFile
### Keywords: internal

### ** Examples

  file_info <- DTAFile("file.txt")
  file_info_pattern <- DTAFile("file\\d+\\.txt", pattern = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAFile-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAFileFactory")
### * DTAFileFactory

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAFileFactory
### Title: Create a DTAFile Object
### Aliases: DTAFileFactory

### ** Examples

library(DTAtools)
DTAFileFactory(type = "csv", filename = "clinical_data.csv")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAFileFactory", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTAMetaData")
### * DTAMetaData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTAMetaData
### Title: DTAMetaData Class
### Aliases: DTAMetaData

### ** Examples


DTAMetaData(title = "Clinical Data Transfer", version = "1.0")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTAMetaData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTARule")
### * DTARule

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTARule
### Title: DTARule Class
### Aliases: DTARule

### ** Examples

rule <- DTARuleFactory("rule1", "col_unique", columns = "SUBJID")
rule



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTARule", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTARuleColCondition")
### * DTARuleColCondition

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTARuleColCondition
### Title: DTARuleColCondition Class
### Aliases: DTARuleColCondition

### ** Examples

# Create a check_range rule
DTAtools::DTARuleColCondition(
 id = "check_gfreasnd2",
 condition = list(
   GFREASND = list(
     empty = FALSE
   )
 ),
 then = list(
   GFSTAT = list(
     empty = FALSE
   ),
   GFORRES = list(
     empty = TRUE
   )
 )
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTARuleColCondition", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTARuleColRange")
### * DTARuleColRange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTARuleColRange
### Title: DTARuleColRange Class
### Aliases: DTARuleColRange

### ** Examples

# Create a check_range rule
rule1 <- DTAtools::DTARuleColRange(
  id = "rule1",
  columns = "age",
  range = c(18, 65)
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTARuleColRange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTARuleColUnique")
### * DTARuleColUnique

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTARuleColUnique
### Title: DTARuleColUnique Class
### Aliases: DTARuleColUnique

### ** Examples

# Create a check_unique rule
rule2 <- DTAtools::DTARuleColUnique(
  id = "rule2",
  columns = "id"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTARuleColUnique", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DTARuleFactory")
### * DTARuleFactory

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DTARuleFactory
### Title: Create a DTARule Object
### Aliases: DTARuleFactory

### ** Examples

DTARuleFactory(
  "rule1",
  "col_condition",
  condition = list(age = list(equals = 18)),
  then = list(status = list(equals = "adult"))
)
DTARuleFactory("rule2", "col_range", columns = "score", min = 0, max = 100)
DTARuleFactory("rule3", "col_unique", columns = "id")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DTARuleFactory", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.list")
### * as.list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.list
### Title: as.list method for DTAColumnSpec
### Aliases: as.list

### ** Examples

x <- create_example_DTAColumnSpecCollection()
as.list(x)
library(DTAtools)
md <- create_example_DTAMetaData(2)
md_list <- as.list(md)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as_json_schema")
### * as_json_schema

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as_json_schema
### Title: as_json_schema
### Aliases: as_json_schema

### ** Examples

library(DTAtools)
specs <- create_example_DTAColumnSpecCollection()
as_json_schema(specs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as_json_schema", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check")
### * check

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check
### Title: Check Generic
### Aliases: check

### ** Examples

  dta <- create_example_DTA()
  # Check all datasets
  check(dta)
  # Check specific dataset by name
  check(dta, datasets = "demographics")
  # Check by index
  check(dta, datasets = 1)
  ds <- create_example_DTADataSetTabular(2)
  # Check all tables
  ds <- check(ds)
  # Check specific table
  ds <- check(ds, tables = "tab1")
 # do not manually create DTARule objects, use derived classes instead



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("colspec")
### * colspec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: colspec
### Title: Get Column by ID Method
### Aliases: colspec

### ** Examples

collection <- create_example_DTAColumnSpecCollection()
colspec(collection, "STUDYID")
ds <- create_example_DTADataSetTabular(2)
colspec(ds, "STUDYID")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("colspec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("column_preview")
### * column_preview

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: column_preview
### Title: Preview Column IDs in a DTAColumnSpecCollection
### Aliases: column_preview

### ** Examples

library(DTAtools)
x <- create_example_DTAColumnSpecCollection()
column_preview(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("column_preview", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("columns")
### * columns

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: columns
### Title: Get column specs from DTADataSetTabular Object
### Aliases: columns

### ** Examples

library(DTAtools)
ds <- create_example_DTADataSetTabular()
columns(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("columns", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("columns_specs_from_word")
### * columns_specs_from_word

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: columns_specs_from_word
### Title: Create DTAColumnSpecCollection from DTA Word Document
### Aliases: columns_specs_from_word

### ** Examples

# No runnable example yet.
# Word import examples are intentionally skipped until the API is reworked.



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("columns_specs_from_word", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTA")
### * create_example_DTA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTA
### Title: Create Example DTA Object
### Aliases: create_example_DTA

### ** Examples

  example_dta <- create_example_DTA()
  print(example_dta)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTAColumnSpec")
### * create_example_DTAColumnSpec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTAColumnSpec
### Title: Create Example DTAColumnSpec
### Aliases: create_example_DTAColumnSpec

### ** Examples

library(DTAtools)
create_example_DTAColumnSpec()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTAColumnSpec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTAColumnSpecCollection")
### * create_example_DTAColumnSpecCollection

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTAColumnSpecCollection
### Title: Create Example DTAColumnSpecCollection
### Aliases: create_example_DTAColumnSpecCollection

### ** Examples

library(DTAtools)
create_example_DTAColumnSpecCollection()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTAColumnSpecCollection", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTADataSetTabular")
### * create_example_DTADataSetTabular

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTADataSetTabular
### Title: Create Example DTADataSetTabular
### Aliases: create_example_DTADataSetTabular

### ** Examples

library(DTAtools)
create_example_DTADataSetTabular()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTADataSetTabular", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTAFileCSV")
### * create_example_DTAFileCSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTAFileCSV
### Title: Create Example DTAFileCSV Object
### Aliases: create_example_DTAFileCSV

### ** Examples

library(DTAtools)
create_example_DTAFileCSV()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTAFileCSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTAFileTSV")
### * create_example_DTAFileTSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTAFileTSV
### Title: Create Example DTAFileTSV Object
### Aliases: create_example_DTAFileTSV

### ** Examples

library(DTAtools)
create_example_DTAFileTSV()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTAFileTSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTAMetaData")
### * create_example_DTAMetaData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTAMetaData
### Title: Create Example DTAMetaData Object
### Aliases: create_example_DTAMetaData

### ** Examples

library(DTAtools)
example_metadata <- create_example_DTAMetaData()
print(example_metadata)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTAMetaData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTARuleColCondition")
### * create_example_DTARuleColCondition

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTARuleColCondition
### Title: create_example_DTARuleColCondition
### Aliases: create_example_DTARuleColCondition

### ** Examples

 library(DTAtools)
 create_example_DTARuleColCondition()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTARuleColCondition", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTARuleColRange")
### * create_example_DTARuleColRange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTARuleColRange
### Title: create_example_DTARuleColRange
### Aliases: create_example_DTARuleColRange

### ** Examples

 library(DTAtools)
 create_example_DTARuleColRange()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTARuleColRange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_example_DTARuleColUnique")
### * create_example_DTARuleColUnique

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_example_DTARuleColUnique
### Title: create_example_DTARuleColUnique
### Aliases: create_example_DTARuleColUnique

### ** Examples

 library(DTAtools)
 create_example_DTARuleColUnique()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_example_DTARuleColUnique", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("datasets")
### * datasets

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: datasets
### Title: Get datasets
### Aliases: datasets

### ** Examples

library(DTAtools)
x <- create_example_DTA()
datasets(x)
datasets(x, "vitals")
datasets(x, 1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("datasets", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("double-bracket")
### * double-bracket

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: double-bracket
### Title: Extract single dataset with [[
### Aliases: double-bracket [[

### ** Examples

  dta <- create_example_DTA()
  dta[[1]]
  dta[["demographics"]]



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("double-bracket", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dta_dataset_from_list")
### * dta_dataset_from_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dta_dataset_from_list
### Title: DTADataSet from list
### Aliases: dta_dataset_from_list

### ** Examples

require(DTAtools)
file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
yaml_dataset <- yaml::read_yaml(file)
dataset <- dta_dataset_from_list(yaml_dataset)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dta_dataset_from_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dta_from_list")
### * dta_from_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dta_from_list
### Title: Read DTA from List
### Aliases: dta_from_list

### ** Examples

require(DTAtools)
file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
yaml_data <- yaml::read_yaml(file)
dta <- dta_from_list(yaml_data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dta_from_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_column_value_table")
### * export_column_value_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_column_value_table
### Title: Generate table containing all potential values of a column
### Aliases: export_column_value_table

### ** Examples

# No runnable example yet.
# Word export examples are intentionally skipped until the API is reworked.



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_column_value_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_specs_table")
### * export_specs_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_specs_table
### Title: Generate table containing all DTA column specs
### Aliases: export_specs_table

### ** Examples

# No runnable example yet.
# Word export examples are intentionally skipped until the API is reworked.



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_specs_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_with_template")
### * export_with_template

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_with_template
### Title: Export a DTA Using a User-Provided Word Template
### Aliases: export_with_template

### ** Examples

## Not run: 
##D # Build a minimal template with officer, then fill it from a DTA.
##D library(officer)
##D template <- tempfile(fileext = ".docx")
##D doc <- read_docx()
##D doc <- body_add_par(doc, "Title: {DTA_TITLE}")
##D doc <- body_add_par(doc, "Version: {DTA_VERSION}")
##D doc <- body_add_par(doc, "Supplier: {SUPPLIER_NAME}")
##D print(doc, target = template)
##D 
##D dta <- read_dta_from_yaml(
##D   system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
##D )
##D export_with_template(dta, template, tempfile(fileext = ".docx"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_with_template", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("files")
### * files

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: files
### Title: Get files from DTADataSet Object
### Aliases: files

### ** Examples

library(DTAtools)
ds <- create_example_DTADataSetTabular()
files(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("files", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_arrow_schema_type")
### * get_arrow_schema_type

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_arrow_schema_type
### Title: Get Arrow Schema Type
### Aliases: get_arrow_schema_type

### ** Examples

col <- DTAColumnSpec(id = "AGE", type = "SAS Char")
get_arrow_schema_type(col)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_arrow_schema_type", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_authorized_for_corrections")
### * get_authorized_for_corrections

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_authorized_for_corrections
### Title: Get Authorized Contacts for Corrections
### Aliases: get_authorized_for_corrections

### ** Examples

library(DTAtools)
md <- create_example_DTAMetaData(2)
get_authorized_for_corrections(md)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_authorized_for_corrections", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_receiver_reviewers")
### * get_receiver_reviewers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_receiver_reviewers
### Title: Get Receiver Contacts with Reviewer Role
### Aliases: get_receiver_reviewers

### ** Examples

library(DTAtools)
md <- create_example_DTAMetaData(2)
get_receiver_reviewers(md)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_receiver_reviewers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_table-DTADataSetTabular")
### * get_table-DTADataSetTabular

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_table-DTADataSetTabular
### Title: Get table from DTADataSetTabular Object
### Aliases: get_table-DTADataSetTabular get_table

### ** Examples

ds <- create_example_DTADataSetTabular(2)
get_table(ds, 1)
get_table(ds, "tab1")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_table-DTADataSetTabular", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_transmission_dates")
### * get_transmission_dates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_transmission_dates
### Title: Get Transmission Start and End Dates
### Aliases: get_transmission_dates

### ** Examples

library(DTAtools)
md <- create_example_DTAMetaData(2)
get_transmission_dates(md)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_transmission_dates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_version_history_df")
### * get_version_history_df

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_version_history_df
### Title: Get Version History as Data Frame
### Aliases: get_version_history_df

### ** Examples

library(DTAtools)
md <- create_example_DTAMetaData(2)
get_version_history_df(md)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_version_history_df", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_specs_from_yaml")
### * import_specs_from_yaml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_specs_from_yaml
### Title: Create DTAColumnSpecCollection from YAML File
### Aliases: import_specs_from_yaml

### ** Examples

# Sample YAML file content
yaml_content <- "
columns:
  - id: STUDYID
    label: Study Identifier
    type: SAS Char
    nullable: false
    values: '1234'
  - id: VISIT
    label: Visit
    type: SAS Char
    nullable: true
    values:
      - 'V03'
      - 'EOT'
      - 'V05'
"

# Write the YAML content to a file
yaml_file <- tempfile(fileext = ".yaml")
writeLines(yaml_content, yaml_file)

# Create the DTAColumnSpecCollection object
DTAColumnSpecCollection <- import_specs_from_yaml(yaml_file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_specs_from_yaml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("labels-DTADataSetTabular")
### * labels-DTADataSetTabular

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: labels-DTADataSetTabular
### Title: List of tables labels within DTADataSetTabular Object
### Aliases: labels-DTADataSetTabular labels

### ** Examples

ds <- create_example_DTADataSetTabular(2)
labels(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("labels-DTADataSetTabular", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("load_file")
### * load_file

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: load_file
### Title: Load a file into a DTA or DTADataSet object
### Aliases: load_file load_file,DTAtools::DTADataSetTabular-method

### ** Examples

file_handler <- DTAFileCSV(filename = "clinical_data.csv")
ds <- DTADataSetTabular(
  name = "demo",
  specs = create_example_DTAColumnSpecCollection(1),
  files = list(file_handler)
)
file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
ds <- DTAtools:::load_file(ds, file = file, handler_index = 1)
names(tables(ds))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("load_file", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("matches_filename")
### * matches_filename

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: matches_filename
### Title: Matches Filename
### Aliases: matches_filename

### ** Examples

  file_info <- DTAFile("file.txt")
  matches_filename(file_info, "file.txt")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("matches_filename", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("max_number_of_files")
### * max_number_of_files

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: max_number_of_files
### Title: Get max number of files
### Aliases: max_number_of_files

### ** Examples

ds <- DTADataSet(
  name = "example_dataset",
  type = "file",
  files = list(create_example_DTAFileCSV())
)
max_number_of_files(ds)
  file_info <- DTAFile("file.txt", number_of_files = 1)
  max_number_of_files(file_info)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("max_number_of_files", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("messages")
### * messages

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: messages
### Title: Retrieve Check Messages
### Aliases: messages

### ** Examples

ds <- create_example_DTADataSetTabular(2)
ds <- check(ds, quiet = TRUE)
messages(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("messages", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("metadata")
### * metadata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: metadata
### Title: Get Metadata
### Aliases: metadata

### ** Examples

library(DTAtools)
dta_obj <- create_example_DTA()
metadata(dta_obj)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("metadata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("min_number_of_files")
### * min_number_of_files

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: min_number_of_files
### Title: Get min number of files
### Aliases: min_number_of_files

### ** Examples

ds <- DTADataSet(
  name = "example_dataset",
  type = "file",
  files = list(create_example_DTAFileCSV())
)
min_number_of_files(ds)
  file_info <- DTAFile("file.txt", number_of_files = 1)
  min_number_of_files(file_info)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("min_number_of_files", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("names")
### * names

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: names
### Title: Get Names Method
### Aliases: names

### ** Examples

collection <- create_example_DTAColumnSpecCollection()
names(collection)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("names", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print
### Title: Print DTA Object
### Aliases: print

### ** Examples

  dta_obj <- create_example_DTA()
  print(dta_obj)
x <- create_example_DTAColumnSpecCollection()
print(x)
library(DTAtools)
print(create_example_DTADataSetTabular())
library(DTAtools)
print(create_example_DTADataSetTabular())
 # do not use this, use derived classes instead, e.g.
 # DTAFileCSV or DTAFileTSV
 print(DTAFileCSV("example.csv"))
library(DTAtools)
print(create_example_DTAFileCSV())

library(DTAtools)
print(DTAFileDelim("example.tsv"))

library(DTAtools)
print(create_example_DTAFileTSV())

library(DTAtools)
print(create_example_DTAMetaData())

rule <- DTARuleFactory("rule1", "col_unique", columns = "SUBJID")
print(rule)
rule <- create_example_DTARuleColCondition()
print(rule)
rule <- create_example_DTARuleColRange()
print(rule)
rule <- create_example_DTARuleColUnique()
print(rule)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print_info")
### * print_info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print_info
### Title: print info
### Aliases: print_info

### ** Examples

ds <- create_example_DTADataSetTabular(2)
print_info(ds)
dta_file <- DTAFileCSV(filename = "data.csv")
print_info(dta_file)

dta_file <- DTAFileCSV(filename = "data.csv")
print_info(dta_file)

library(DTAtools)
print(create_example_DTAMetaData())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print_info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print_short_info")
### * print_short_info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print_short_info
### Title: Print Short Information for DTADataset
### Aliases: print_short_info

### ** Examples

library(DTAtools)
ds <- create_example_DTADataSetTabular()
print_short_info(ds)
library(DTAtools)
ds <- create_example_DTADataSetTabular()
print_short_info(ds)
dta_file <- DTAFileCSV(filename = "data.csv")
print_short_info(dta_file)

library(DTAtools)
print_short_info(create_example_DTAMetaData())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print_short_info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_dataset_from_yaml")
### * read_dataset_from_yaml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_dataset_from_yaml
### Title: Read DTADataSet from YAML
### Aliases: read_dataset_from_yaml

### ** Examples

require(DTAtools)
file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
dta <- read_dataset_from_yaml(file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_dataset_from_yaml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_dta_from_yaml")
### * read_dta_from_yaml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_dta_from_yaml
### Title: Read DTA from YAML
### Aliases: read_dta_from_yaml

### ** Examples

require(DTAtools)
file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
dta <- read_dta_from_yaml(file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_dta_from_yaml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("results")
### * results

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: results
### Title: Retrieve Check Results
### Aliases: results

### ** Examples

dta <- create_example_DTA()
dta <- check(dta, quiet = TRUE)
results(dta)

ds <- dta[["demographics"]]
results(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("results", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rule_check_col_condition")
### * rule_check_col_condition

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rule_check_col_condition
### Title: Rule: check_col_condition
### Aliases: rule_check_col_condition

### ** Examples

# Example: If species == "setosa", then petal_length in [1.0, 1.9]
# rule_check_col_condition(rule, iris)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rule_check_col_condition", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rule_check_range")
### * rule_check_range

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rule_check_range
### Title: Rule: check_range
### Aliases: rule_check_range

### ** Examples

# Suppose `rule` is a DTARule with column="age", range=c(18, 65)
# rule_check_range(rule, df)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rule_check_range", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rule_check_unique")
### * rule_check_unique

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rule_check_unique
### Title: Rule: check_unique
### Aliases: rule_check_unique

### ** Examples

# rule_check_unique(rule, df)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rule_check_unique", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rule_preview")
### * rule_preview

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rule_preview
### Title: Preview Rules in a DTAColumnSpecCollection
### Aliases: rule_preview

### ** Examples

library(DTAtools)
x <- create_example_DTAColumnSpecCollection()
rule_preview(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rule_preview", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rules")
### * rules

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rules
### Title: Get Rules
### Aliases: rules

### ** Examples

collection <- create_example_DTAColumnSpecCollection()
rules(collection)
ds <- create_example_DTADataSetTabular(2)
rules(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rules", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_dta_app")
### * run_dta_app

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_dta_app
### Title: Launch the DTAtools Shiny Application
### Aliases: run_dta_app

### ** Examples

## Not run: 
##D run_dta_app()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_dta_app", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("single-bracket")
### * single-bracket

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: single-bracket
### Title: Extract multiple datasets with [
### Aliases: single-bracket [

### ** Examples

  dta <- create_example_DTA()
  dta[c(1, 2)]
  dta[c("demographics", "vitals")]



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("single-bracket", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("specs-DTADataSetTabular")
### * specs-DTADataSetTabular

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: specs-DTADataSetTabular
### Title: Get DTAColumnSpecCollection (specs) from DTADataSetTabular
###   Object
### Aliases: specs-DTADataSetTabular specs

### ** Examples

ds <- create_example_DTADataSetTabular(2)
specs(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("specs-DTADataSetTabular", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("specs_from_list")
### * specs_from_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: specs_from_list
### Title: Create DTAColumnSpecCollection from Components
### Aliases: specs_from_list

### ** Examples

library(DTAtools)
# Load example YAML file from package extdata
yaml_file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
input_list <- yaml::read_yaml(yaml_file)
specs <- specs_from_list(input_list$columns, input_list$rules)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("specs_from_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tables")
### * tables

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tables
### Title: Get tables from DTADataSet Object
### Aliases: tables

### ** Examples

library(DTAtools)
ds <- create_example_DTADataSetTabular()
tables(ds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tables", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("validate_transmission_dates")
### * validate_transmission_dates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: validate_transmission_dates
### Title: Validate Transmission Dates
### Aliases: validate_transmission_dates

### ** Examples

library(DTAtools)
md <- create_example_DTAMetaData(2)
validate_transmission_dates(md)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("validate_transmission_dates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_columns_to_json")
### * write_columns_to_json

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_columns_to_json
### Title: Write DTAColumnSpecCollection to JSON File
### Aliases: write_columns_to_json

### ** Examples

columns <- create_example_DTAColumnSpecCollection()
out_file <- tempfile(fileext = ".json")
write_columns_to_json(columns, out_file)
unlink(out_file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_columns_to_json", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_columns_to_yaml")
### * write_columns_to_yaml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_columns_to_yaml
### Title: Write DTAColumnSpecCollection to YAML File
### Aliases: write_columns_to_yaml

### ** Examples

columns <- create_example_DTAColumnSpecCollection()
out_file <- tempfile(fileext = ".yaml")
write_columns_to_yaml(columns, out_file)
unlink(out_file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_columns_to_yaml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_dataset_metadata")
### * write_dataset_metadata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_dataset_metadata
### Title: Export Dataset Metadata as Professional Document
### Aliases: write_dataset_metadata

### ** Examples

## Not run: 
##D ds <- DTADataSetTabular(
##D   name = "example_dataset",
##D   specs = create_example_DTAColumnSpecCollection(1)
##D )
##D write_dataset_metadata(ds, file = "dataset_spec.docx")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_dataset_metadata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_dta")
### * write_dta

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_dta
### Title: Export DTA Metadata as Professional Document
### Aliases: write_dta

### ** Examples

## Not run: 
##D dta <- DTA(
##D   title = "Clinical Data Transfer",
##D   version = "1.0",
##D   date = Sys.Date()
##D )
##D write_dta(dta, file = "dta_metadata.docx")
##D 
##D # Fill a user-authored Word template instead of the built-in layout
##D write_dta(dta, file = "dta_from_template.docx", template = "my_template.docx")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_dta", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_table_to_file")
### * write_table_to_file

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_table_to_file
### Title: Write DTA Table to File
### Aliases: write_table_to_file

### ** Examples

ds <- create_example_DTADataSetTabular(2)
out_file <- tempfile(fileext = ".tsv")
write_table_to_file(
  ds,
  table = "tab1",
  filename = out_file,
  sep = "\t",
  arrange_by = c("STUDYID", "VISIT")
)
unlink(out_file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_table_to_file", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
