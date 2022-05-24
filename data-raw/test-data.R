# example data derived from: https://mc-stan.org/documentation/case-studies/dina_independent.html#stan_nostructure

library(tidyverse)
library(measr)
library(rstan)
library(glue)
library(here)

num_item <- 35
num_resp <- 1000
num_attr <- 5

set.seed(9416)

# create Q-matrix --------------------------------------------------------------
q_matrix <- create_profiles(num_attr) %>%
  rowwise() %>%
  mutate(total = sum(c_across(everything()))) %>%
  ungroup() %>%
  filter(between(total, 1, 3)) %>%
  select(-total) %>%
  slice_sample(n = num_item, replace = TRUE) %>%
  mutate(item = paste0("item_", seq_len(num_item)), .before = 1)


# mastery profiles -------------------------------------------------------------
alpha_patt <- create_profiles(num_attr)

eta <- sample(seq(0.1, 0.9, by = 0.1), size = num_attr, replace = FALSE)

profile_prob <- function(x, prob) {
  prod((prob ^ x) * ((1 - prob) ^ (1 - x)))
}

alpha_patt <- alpha_patt %>%
  rowwise() %>%
  mutate(prob = profile_prob(c_across(everything()), prob = eta),
         .before = 1) %>%
  ungroup()

true_profiles <- alpha_patt %>%
  slice_sample(n = num_resp, weight_by = prob, replace = TRUE) %>%
  select(-prob)


# item parameters --------------------------------------------------------------
## dina and dino
dinao_params <- tibble(item = glue("item_{seq_len(num_item)}"),
                       slip = runif(num_item, min = 0.05, max = 0.30),
                       guess = runif(num_item, min = 0.05, max = 0.30))

true_dinoa <- dinao_params %>%
  pivot_longer(-item, names_to = "parm1", values_to = "true") %>%
  separate(item, c(NA, "item"), sep = "_") %>%
  mutate(param = paste0(parm1, "[", item, "]")) %>%
  select(param, true) %>%
  bind_rows(alpha_patt %>%
              rowid_to_column(var = "parm1") %>%
              mutate(param = paste0("nu[", parm1, "]")) %>%
              select(param, true = prob))

## lcdm



# generate data ----------------------------------------------------------------
# Calculate an indicator whether respondents have all attributes needed for
# each item
stu_xi_dina <- matrix(0, num_resp, num_item)
stu_xi_dino <- matrix(0, num_resp, num_item)
for (j in seq_len(num_resp)) {
  for (i in seq_len(num_item)) {
    stu_xi_dina[j, i] <- prod(true_profiles[j, ] ^ q_matrix[i, -1])
    stu_xi_dino[j, i] <- 1 - prod((1 - true_profiles[j, ]) ^ q_matrix[i, -1])
  }
}

dina_xi <- matrix(0, num_item, 2 ^ num_attr)
dino_xi <- matrix(0, num_item, 2 ^ num_attr)
for (i in seq_len(num_item)) {
  for (c in seq_len(2 ^ num_attr)) {
    dina_xi[i, c] <- prod(alpha_patt[c, -1] ^ q_matrix[i, -1])
    dino_xi[i, c] <- 1 - prod((1 - alpha_patt[c, -1]) ^ q_matrix[i, -1])
  }
}

# generate data
dina_data <- stu_xi_dina %>%
  as_tibble(.name_repair = ~glue("item_{seq_len(num_item)}")) %>%
  rowid_to_column("resp_id") %>%
  pivot_longer(-resp_id, names_to = "item", values_to = "high_prob") %>%
  left_join(dinao_params, by = "item") %>%
  mutate(prob_correct = ((1 - slip) ^ high_prob) * (guess ^ (1 - high_prob)),
         score = as.integer(runif(n()) < prob_correct)) %>%
  select(resp_id, item, score) %>%
  pivot_wider(names_from = item, values_from = score)

dino_data <- stu_xi_dino %>%
  as_tibble(.name_repair = ~glue("item_{seq_len(num_item)}")) %>%
  rowid_to_column("resp_id") %>%
  pivot_longer(-resp_id, names_to = "item", values_to = "high_prob") %>%
  left_join(dinao_params, by = "item") %>%
  mutate(prob_correct = ((1 - slip) ^ high_prob) * (guess ^ (1 - high_prob)),
         score = as.integer(runif(n()) < prob_correct)) %>%
  select(resp_id, item, score) %>%
  pivot_wider(names_from = item, values_from = score)


# confirm that we can recover parameters using known stan script ---------------
dina_stan <- list(I = num_item, J = num_resp, K = num_attr, C = 2 ^ num_attr,
                  y = as.matrix(select(dina_data, -resp_id)),
                  alpha = as.matrix(select(alpha_patt, -prob)), xi = dina_xi)
dina_mod <- stan(here("data-raw", "stan", "no-strc-dinao.stan"),
                 data = dina_stan, chains = 4, cores = 4, iter = 500)

dino_stan <- list(I = num_item, J = num_resp, K = num_attr, C = 2 ^ num_attr,
                  y = as.matrix(select(dino_data, -resp_id)),
                  alpha = as.matrix(select(alpha_patt, -prob)), xi = dino_xi)
dino_mod <- stan(here("data-raw", "stan", "no-strc-dinao.stan"),
                 data = dino_stan, chains = 4, cores = 4, iter = 500)


dina_sum <- summary(dina_mod, pars = c("slip", "guess", "nu"))$summary %>%
  as_tibble(rownames = "param") %>%
  select(param, mean_dina = mean, n_eff_dina = n_eff, Rhat_dina = Rhat)

dino_sum <- summary(dino_mod, pars = c("slip", "guess", "nu"))$summary %>%
  as_tibble(rownames = "param") %>%
  select(param, mean_dino = mean, n_eff_dino = n_eff, Rhat_dino = Rhat)



param_compare <- full_join(dina_sum, dino_sum, by = "param") %>%
  left_join(true_dinoa, by = "param") %>%
  separate(param, c("class", "num", NA), sep = "\\[|\\]")

ggplot(param_compare, aes(x = true, y = mean_dina)) +
  facet_wrap(~class, nrow = 1) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

ggplot(param_compare, aes(x = true, y = mean_dino)) +
  facet_wrap(~class, nrow = 1) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")


# save data --------------------------------------------------------------------
use_data(q_matrix, true_dinoa, true_profiles, dina_data, dino_data,
         internal = TRUE, overwrite = TRUE)
