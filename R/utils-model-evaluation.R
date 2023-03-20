existing_ppmc_check <- function(model, method, dots, overwrite) {
  run_ppmc <- if (!"ppmc" %in% method) {
    list(run = FALSE, args = NULL)
  } else if (!("ppmc" %in% names(model$model_fit))) {
    dots$model <- model
    list(run = TRUE, args = dots)
  } else if (overwrite) {
    dots$model <- model
    list(run = TRUE, args = dots)
  } else {
    existing_criteria <- lapply(names(model$model_fit$ppmc),
                                function(.x) names(model$model_fit$ppmc[[.x]]))
    names(existing_criteria) <- names(model$model_fit$ppmc)

    new_dots <- lapply(names(dots),
                       function(x, new, old) {
                         if (!x %in% names(old)) return(new[[x]])

                         setdiff(new[[x]], old[[x]])
                       },
                       new = dots, old = existing_criteria)
    names(new_dots) <- names(dots)

    new_dots <- modifyList(list(model_fit = NULL, item_fit = NULL),
                           new_dots)

    new_dots$model <- model
    list(run = TRUE, args = new_dots)
  }

  return(run_ppmc)
}

existing_m2_check <- function(model, method, overwrite) {
  ("m2" %in% method && overwrite) ||
    ("m2" %in% method && !("m2" %in% names(model$model_fit)))
}

add_ppmc <- function(model, run_ppmc) {
  if (run_ppmc$run) {
    new_ppmc <- do.call(fit_ppmc, run_ppmc$args)

    if ("ppmc" %in% names(model$model_fit)) {
      model$model_fit$ppmc <- modifyList(model$model_fit$ppmc, new_ppmc)
    } else {
      model$model_fit$ppmc <- new_ppmc
    }
  }

  return(model)
}
