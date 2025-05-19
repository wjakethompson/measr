library(tidyverse)
library(here)
library(glue)

# create random respondent ids
create_id <- function(length, num_prop = 0.2) {
  char <- sample(x = letters, size = round(length * (1 - num_prop)))
  numr <- sample(x = 0:9, size = round(length * num_prop))

  sample(x = c(char, numr), size = length) |>
    paste(collapse = "")
}
many_id <- function(number, length = 5, num_prop = 0.2) {
  all_id <- replicate(n = number * 2,
                      create_id(length = length, num_prop = num_prop))

  sample(unique(all_id), size = number)
}

mdm_data <- read_csv(here("data-raw", "csv", "mdm-data.csv"),
                     col_types = cols(.default =  col_integer()),
                     col_names = FALSE) |>
  rename(mdm1 = X1, mdm2 = X2, mdm3 = X3, mdm4 = X4) |>
  mutate(respondent = many_id(n(), length = 5, num_prop = 0.2),
         .before = 1)

mdm_qmatrix <- tibble(item = paste0("mdm", 1:4),
                      multiplication = 1L)

use_data(mdm_data, mdm_qmatrix, overwrite = TRUE)
