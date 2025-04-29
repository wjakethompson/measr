write_measrfit <- function(model, file) {
  if (S7::S7_inherits(model@backend, cmdstanr)) {
    cur_files <-  model@model$output_files()
    new_files <- paste0(fs::path_wd(), "/",
                        fs::path_ext_remove(file), "-", seq_along(cur_files),
                        ".csv")
    if (!identical(cur_files, new_files)) {
      model@model$save_output_files(
        fs::path_dir(file),
        basename = fs::path_file(fs::path_ext_remove(file)),
        timestamp = FALSE, random = FALSE
      )
    }
  }
  saveRDS(model, file = file)
}

read_measrfit <- function(file) {
  file <- check_file(file)
  mod <- readRDS(file)

  mod
}

check_previous_fit <- function(file, dcm_spec, clean_data, stan_mthd,
                               stan_bknd) {
  prev <- read_measrfit(file)

  check <- all(
    identical(prev@model_spec@qmatrix, dcm_spec@qmatrix),
    S7::S7_inherits(dcm_spec@measurement_model,
                    S7::S7_class(prev@model_spec@measurement_model)),
    S7::S7_inherits(dcm_spec@structural_model,
                    S7::S7_class(prev@model_spec@structural_model)),
    identical(dcmstan::prior_tibble(prev@model_spec@priors),
              dcmstan::prior_tibble(dcm_spec@priors)),
    identical(prev@data, clean_data),
    S7::S7_inherits(stan_mthd, S7::S7_class(prev@method)),
    S7::S7_inherits(stan_bknd, S7::S7_class(prev@backend))
  )

  if (check) {
    return(prev)
  } else {
    return(NULL)
  }
}
