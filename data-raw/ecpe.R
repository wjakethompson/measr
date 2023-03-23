library(tidyverse)
library(here)
library(glue)

# original data from CDM package
ecpe_data <- read_csv(here("data-raw", "csv", "ecpe-data.csv"),
                      col_types = cols(.default =  col_integer())) %>%
  rename(resp_id = id)
ecpe_qmatrix <- read_csv(here("data-raw", "csv", "ecpe-qmatrix.csv"),
                         col_types = cols(.default = col_integer())) %>%
  mutate(item_id = paste0("E", seq_len(n()))) %>%
  select(item_id, everything()) %>%
  rename(morphosyntactic = skill1, cohesive = skill2, lexical = skill3)

use_data(ecpe_data, ecpe_qmatrix, overwrite = TRUE)
