#' Extract components of a `measrfit` object.
#'
#' @param model The estimated to extract information from.
#' @param ... Additional arguments passed to specific methods.
#'
#' @export
measr_extract <- function(model, ...) {
  UseMethod("measr_extract")
}

#' Extract components of an estimated diagnostic classification model
#'
#' @param what Character string. The information to be extracted. See details
#'   for available options.
#'
#' @details
#' For diagnostic classification models, we can extract the following
#' information:
#'   * `item_param`: The estimated item parameters. This shows the name of the
#'     parameter, the class of the parameter, and the estimated value.
#'   * `strc_param`: The estimated structural parameters. This is base rate of
#'     membership in each class. This shows the class pattern and the estimated
#'     proportion of respondents in each class.
#'   * `prior`: The priors used when estimating the model.
#'
#' @return The extracted information. The specific structure will vary depending
#'   on what is being extracted, but usually the returned object is a
#'   [tibble][tibble::tibble-package] with the requested information.
#'
#' @describeIn measr_extract Extract components of an estimated diagnostic
#'   classification model.
#' @export
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' extract(rstn_mdm_lcdm, "strc_param")
measr_extract.measrdcm <- function(model, what, ...) {
  out <- switch(
    what,
    item_param = {
      items <- model$data$qmatrix %>%
        dplyr::select(item = "item_id") %>%
        dplyr::mutate(item_id = as.integer(.data$item))
      params <- get_parameters(dplyr::select(model$data$qmatrix, -"item_id"),
                               type = model$type) %>%
        dplyr::left_join(items, by = "item_id", multiple = "all") %>%
        dplyr::select("item", dplyr::everything(), -"item_id")
      draws <- as_draws(model) %>%
        posterior::subset_draws(variable = dplyr::pull(params, "coef")) %>%
        posterior::as_draws_rvars() %>%
        tibble::as_tibble()


      if (model$type %in% c("lcdm")) {
        draws <- draws %>%
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "coef", values_to = "estimate")
        dplyr::left_join(params, draws, by = c("coef")) %>%
          dplyr::rename(!!model$dat$item_id := "item")
      } else if (model$type %in% c("dina", "dino")) {
        draws <- draws %>%
          dplyr::mutate(item = items$item) %>%
          tidyr::pivot_longer(cols = -"item",
                              names_to = "coef", values_to = "estimate")

        dplyr::left_join(params, draws, by = c("item", "class" = "coef"),
                         relationship = "one-to-one") %>%
          dplyr::rename(!!model$dat$item_id := "item")
      }
    },
    strc_param = {
      profiles <- profile_labels(ncol(model$data$qmatrix) - 1)

      as_draws(model) %>%
        posterior::subset_draws(variable = "Vc") %>%
        posterior::as_draws_rvars() %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "coef", values_to = "estimate") %>%
        tibble::rowid_to_column(var = "class_id") %>%
        dplyr::left_join(profiles, by = "class_id") %>%
        dplyr::select("class", "estimate")
    },
    prior = model$prior,
    rlang::abort(message = glue::glue("Cannot extract element `{what}`"))
  )

  return(out)
}
