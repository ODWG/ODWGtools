flag.descriptions = readxl::read_excel("flag_descriptions.xlsx")
usethis::use_data(flag.descriptions, internal = TRUE,
  overwrite = TRUE)
