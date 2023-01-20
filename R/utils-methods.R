get_optim_draws <- function(x) {
  draw_matrix <- if (x$backend == "rstan") {
    t(as.matrix(model$model$par))
  } else if (x$backend == "cmdstanr") {
    as.matrix(model$model$draws())
  }

  all_vars <- colnames(draw_matrix)
  keep_vars <- all_vars[c(grep("^Vc", all_vars), # structural parameters
                          grep("_0", all_vars),
                          grep("_1", all_vars),
                          grep("l.*_raw", all_vars))]

  final_matrix <- draw_matrix[, keep_vars]
  final_matrix <- t(as.matrix(final_matrix))
  return(final_matrix)
}
