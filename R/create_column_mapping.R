# see DIADPATH_habitat data/column_mapping.xlsx
#
library(tibble)

column_mapping <- dplyr::bind_rows(
  readxl::read_xlsx("data-raw/column_mapping.xlsx", sheet = "sudoang"),
  readxl::read_xlsx("data-raw/column_mapping.xlsx", sheet = "amber_salmon"),
  readxl::read_xlsx("data-raw/column_mapping.xlsx", sheet = "hydrosheds")
)

usethis::use_data(column_mapping, overwrite = TRUE)


# TODO add other sheets in long format
