library(tidyverse)
devtools::load_all()

gqs_script(full_data = FALSE) %>%
  pluck("stancode") %>%
  write_lines("inst/stan/gqs_probs.stan")

gqs_script(full_data = TRUE) %>%
  pluck("stancode") %>%
  write_lines("inst/stan/gqs_ppmc.stan")

loglik_script() %>%
  pluck("stancode") %>%
  write_lines("inst/stan/gqs_loglik.stan")

rstn_mdm_lcdm$stancode %>%
  write_lines("inst/stan/test_lcdm.stan")

rstn_dina$stancode %>%
  write_lines("inst/stan/test_dina.stan")

rstn_dino$stancode %>%
  write_lines("inst/stan/test_dino.stan")
