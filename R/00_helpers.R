class_null <- S7::new_S3_class("NULL")
class_DTAColumnSpec <- S7::new_class("DTAColumnSpec")
class_DTAColumnSpecCollection <- S7::new_class(
  "DTAColumnSpecCollection"
)
class_DTAContainer <- S7::new_class("DTAContainer")
class_DTAMetadata <- S7::new_class("DTAMetaData")
class_character_or_null <- class_character | class_null
class_numeric_or_null <- class_numeric | class_null
class_character_or_numeric_or_null <- class_character |
  class_numeric |
  class_null
class_logical_or_null <- class_logical | class_null
class_character_or_list <- class_character |
  class_list
class_character_or_numeric_or_null_or_list <- class_character |
  class_numeric |
  class_null |
  class_list
#class_vector_or_null <- class_vector | class_null
