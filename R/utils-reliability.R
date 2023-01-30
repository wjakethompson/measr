clean_predicted_probs <- function(x, resp_id) {
  x %>%
    dplyr::select(-c(".chain", ".iteration", ".draw")) %>%
    dplyr::summarize(dplyr::across(where(is.double),
                                   \(x) mean(x, na.rm = TRUE)),
                     .by = !!resp_id) %>%
    dplyr::arrange(!!resp_id) %>%
    dplyr::select(-!!resp_id)
}

create_reli_list <- function(N, K, M, A, pAhatA, attr_probs, class_probs,
                             ahat, amean, pAx, pxi, att_names) {
  ### Accuracy and Consistency -----
  acc <- double(4 * K)
  consist <- double(4 * K)
  gamma <- double(K)
  prev <- double(K)

  for (i in (seq_len(M) - 1)) {
    for (k in (seq_len(K) - 1)) {
      if (A[(i + k * M) + 1] == 1) {
        prev[k + 1] <- prev[k + 1] + pxi[i + 1]
      }
    }
  }

  for (i in (seq_len(N) - 1)) {
    for (k in (seq_len(K) - 1)) {
      acc[(k * 4 + ahat[(i + k * N) + 1]) + 1] <-
        acc[(k * 4 + ahat[(i + k * N) + 1]) + 1] + (1 - amean[(i + k * N) + 1])
      acc[(k * 4 + 2 + ahat[(i + k * N) + 1]) + 1] <-
        acc[(k * 4 + 2 + ahat[(i + k * N) + 1]) + 1] + (amean[(i + k * N) + 1])
      gamma[k + 1] <- gamma[k + 1] +
        (amean[(i + k * N) + 1] * amean[(i + k * N) + 1] +
           (1 - amean[(i + k * N) + 1]) * (1 - amean[(i + k * N) + 1]))

      for (m in (seq_len(M) - 1)) {
        if (ahat[(i + k * N) + 1] == 1) {
          pAhatA[(m + k * M) + 1] <- pAhatA[(m + k * M) + 1] +
            pAx[(m + i * M) + 1]
        }
      }
    }
  }
  new_acc <- array(acc, c(2, 2, K),
                   dimnames = list(c("ahat.0", "ahat.1"),
                                   c("a.0", "a.1"),
                                   att_names))

  for (k in (seq_len(K) - 1)) {
    gamma[k + 1] <- gamma[k + 1] / (N)
  }
  for (m in (seq_len(M) - 1)) {
    for (k  in (seq_len(K) - 1)) {
      pAhatA[(m + k * M) + 1] <- pAhatA[(m + k * M) + 1] /
        ((N + 0.0) * pxi[m + 1])
      consist[(4 * k) + 1] <- consist[(4 * k) + 1] +
        ((1.0 - pAhatA[(m + k * M) + 1]) * (1.0 - pAhatA[(m + k * M) + 1]) *
           pxi[m + 1])
      consist[(4 * k + 1) + 1] <- consist[(4 * k + 1) + 1] +
        ((1.0 - pAhatA[(m + k * M) + 1]) * (pAhatA[(m + k * M) + 1]) *
           pxi[m + 1])
      consist[(4 * k + 2) + 1] <- consist[(4 * k + 1) + 1]
      consist[(4 * k + 3) + 1] <- consist[(4 * k + 3) + 1] +
        ((pAhatA[(m + k * M) + 1]) * (pAhatA[(m + k * M) + 1]) * pxi[m + 1])
    }
  }
  new_consist <- array(consist, c(2, 2, K),
                       dimnames = list(c("ahat1.0", "ahat1.1"),
                                       c("ahat2.0", "ahat2.1"),
                                       att_names))

  ### Final List -----
  ret_list <- list(
    posterior = class_probs %>%
      as.matrix() %>%
      unname(),
    eap = attr_probs %>%
      as.matrix() %>%
      unname(),
    acc = new_acc,
    consist = new_consist,
    gamma = gamma,
    prev = prev,
    pxi = pxi
  )
  return(ret_list)
}

reli_list <- function(model) {
  N <- dplyr::n_distinct(model$data$data$resp_id)
  K <- ncol(model$data$qmatrix) - 1L
  M  <- 2 ^ K
  A <- create_profiles(K) %>%
    as.matrix() %>%
    t() %>%
    as.vector()
  pAhatA <- double(M * K)

  probs <- lapply(predict(model, summary = FALSE),
                  clean_predicted_probs, resp_id = model$data$resp_id)
  class_probs <- probs$class_probabilities
  attr_probs <- probs$attribute_probabilities

  # map estimates
  ahat <- attr_probs %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~dplyr::case_when(.x >= 0.5 ~ 1L,
                                                  TRUE ~ 0L))) %>%
    as.matrix() %>%
    unname() %>%
    as.vector()

  # eap estimates
  amean <- attr_probs %>%
    as.matrix() %>%
    unname() %>%
    as.vector()

  # class probs
  pAx <- class_probs %>%
    as.matrix() %>%
    unname() %>%
    t() %>%
    as.vector()

  # structural parameters
  pxi <- if (model$backend == "rstan" && model$method == "optim") {
    model$model$par %>%
      tibble::enframe() %>%
      dplyr::filter(stringr::str_detect(.data$name, "^Vc")) %>%
      dplyr::pull(.data$value)
  } else {
    posterior::as_draws_df(model$model) %>%
      tibble::as_tibble() %>%
      dplyr::select(dplyr::matches("^Vc")) %>%
      dplyr::summarize(dplyr::across(dplyr::everything(),
                                     \(x) mean(x, na.rm = TRUE))) %>%
      as.numeric()
  }

  att_names <- model$data$qmatrix %>%
    dplyr::select(-"item_id") %>%
    colnames()

  res <- create_reli_list(N = N, K = K, M = M, A = A, pAhatA = pAhatA,
                          attr_probs = attr_probs, class_probs = class_probs,
                          ahat = ahat, amean = amean, pAx = pAx, pxi = pxi,
                          att_names = att_names)

  return(res)
}
