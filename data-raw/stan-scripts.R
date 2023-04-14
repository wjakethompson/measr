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

dina_script(qmatrix = q_matrix[, -1]) %>%
  pluck("stancode") %>%
  write_lines("inst/stan/test_dina.stan")
