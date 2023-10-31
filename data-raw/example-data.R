library(tidyverse)
library(measr)
library(brms)
library(here)

set.seed(121389)

# Functions --------------------------------------------------------------------
generate_items <- function(q_matrix, generate) {
  all_param <- get_parameters(q_matrix, item_id = "item_id", type = "lcdm")
  intercepts <- all_param %>%
    filter(class == "intercept") %>%
    mutate(value = runif(n(), min = -3.00, max = -0.1))
  maineffects <- all_param %>%
    filter(class == "maineffect") %>%
    mutate(value = runif(n(), min = 1.00, max = 5.00))

  interactions <- if (generate == "lcdm") {
    all_param %>%
      filter(class == "interaction") %>%
      left_join(maineffects %>%
                  slice_min(value, n = 1, by = item_id) %>%
                  mutate(min_value = -1 * value) %>%
                  select(item_id, min_value),
                by = "item_id") %>%
      mutate(value = map_dbl(min_value, ~runif(1, min = .x, max = 2.00))) %>%
      select(-min_value)
  } else if (generate == "dina") {
    all_param %>%
      filter(class == "interaction") %>%
      mutate(value = runif(n(), min = 0.00, max = 5.00))
  }
  item_params <- full_join(all_param,
                           bind_rows(intercepts, maineffects, interactions),
                           by = c("item_id", "class", "attributes", "coef"))

  if (generate == "dina") {
    item_params <- item_params %>%
      mutate(num_att = case_when(is.na(attributes) ~ 0L,
                                 TRUE ~ str_count(attributes, "__") + 1)) %>%
      mutate(max_att = max(num_att), .by = item_id) %>%
      filter((num_att == 0) | (num_att == max_att)) %>%
      select(-c(num_att, max_att))
  }

  return(item_params)
}


# Generate data ----------------------------------------------------------------
qmatrix <- create_profiles(4) %>%
  rowwise() %>%
  mutate(total = sum(c_across(everything()))) %>%
  ungroup() %>%
  filter(between(total, 1, 2)) %>%
  slice_sample(n = 20, replace = TRUE) %>%
  select(-total) %>%
  rowid_to_column(var = "item_id") %>%
  mutate(item_id = paste0("A", item_id))

profiles <- create_profiles(4) %>%
  rowid_to_column(var = "class_id")

strc_params <- sample(5:10, size = 16, replace = TRUE)
strc_params <- strc_params / sum(strc_params)
persons <- profiles %>%
  mutate(class_prob = strc_params) %>%
  slice_sample(n = 2000, replace = TRUE, weight_by = class_prob) %>%
  rowid_to_column(var = "resp_id")

item_params <- generate_items(q_matrix = qmatrix, generate = "lcdm") %>%
  filter(class != "structural") %>%
  mutate(item_id = paste0("A", item_id))

pi <- get_parameters(qmatrix = create_profiles(4)) %>%
  rename(class_id = item_id) %>%
  select(-coef) %>%
  left_join(item_params, join_by(class, attributes),
            relationship = "many-to-many") %>%
  filter(!is.na(item_id)) %>%
  group_by(class_id, item_id) %>%
  summarize(log_odds = sum(value), .groups = "drop") %>%
  mutate(prob = exp(log_odds) / (exp(log_odds) + 1)) %>%
  select(item_id, class_id, log_odds, prob)

dat <- persons %>%
  select(resp_id, class_id) %>%
  mutate(item_id = "A1") %>%
  expand(nesting(resp_id, class_id), item_id = qmatrix$item_id) %>%
  left_join(pi, join_by(item_id, class_id)) %>%
  mutate(rand = runif(n(), min = 0, max = 1),
         score = case_when(rand < prob ~ 1L,
                           rand >= prob ~ 0L)) %>%
  select(resp_id, item_id, score) %>%
  mutate(item_id = factor(item_id, levels = paste0("A", 1:20))) %>%
  arrange(resp_id, item_id) %>%
  pivot_wider(names_from = item_id, values_from = score)


ret_list <- list(
  data = dat,
  q_matrix = qmatrix,
  true_person = persons,
  true_items = item_params,
  true_strc = strc_params,
  true_pi = pi,
  profiles = profiles
)

write_rds(ret_list,
          here::here("vignettes", "articles", "data", "simulated-data.rds"))
