library(tidyverse)
library(dcmstan)
devtools::load_all()

<<<<<<< HEAD
stan_code(generated_quantities(probabilities = TRUE)) |>
  write_lines("inst/stan/gqs_probs.stan")

stan_code(generated_quantities(ppmc = TRUE)) |>
  write_lines("inst/stan/gqs_ppmc.stan")

stan_code(generated_quantities(ppmc = TRUE, probabilities = TRUE)) |>
  write_lines("inst/stan/gqs_ppmc_probs.stan")

stan_code(generated_quantities(loglik = TRUE)) |>
  write_lines("inst/stan/gqs_loglik.stan")


dina_spec <- dcm_specify(qmatrix = q_matrix[, -1],
                         measurement_model = dina())
stan_code(dina_spec) |>
=======
gqs_script(full_data = FALSE) |>
  pluck("stancode") |>
  write_lines("inst/stan/gqs_probs.stan")

gqs_script(full_data = TRUE) |>
  pluck("stancode") |>
  write_lines("inst/stan/gqs_ppmc.stan")

loglik_script() |>
  pluck("stancode") |>
  write_lines("inst/stan/gqs_loglik.stan")

dina_script(qmatrix = q_matrix[, -1]) |>
  pluck("stancode") |>
>>>>>>> b561f7d (switch to native pipe)
  write_lines("inst/stan/test_dina.stan")
