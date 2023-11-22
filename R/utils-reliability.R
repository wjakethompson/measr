clean_predicted_probs <- function(x, resp_id) {
  x %>%
    dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar), E)) %>%
    dplyr::arrange(!!resp_id) %>%
    dplyr::select(-!!resp_id)
}

create_reli_prev <- function(n_att, n_class, profile_vec, strc) {
  prev <- double(n_att)

  for (i in (seq_len(n_class) - 1)) {
    for (k in (seq_len(n_att) - 1)) {
      if (profile_vec[(i + k * n_class) + 1] == 1) {
        prev[k + 1] <- prev[k + 1] + strc[i + 1]
      }
    }
  }

  return(prev)
}

create_reli_acc <- function(n_att, n_resp, binary_att, prob_att) {
  acc <- double(4 * n_att)

  for (i in (seq_len(n_resp) - 1)) {
    for (k in (seq_len(n_att) - 1)) {
      acc[(k * 4 + binary_att[(i + k * n_resp) + 1]) + 1] <-
        acc[(k * 4 + binary_att[(i + k * n_resp) + 1]) + 1] +
        (1 - prob_att[(i + k * n_resp) + 1])
      acc[(k * 4 + 2 + binary_att[(i + k * n_resp) + 1]) + 1] <-
        acc[(k * 4 + 2 + binary_att[(i + k * n_resp) + 1]) + 1] +
        (prob_att[(i + k * n_resp) + 1])
    }
  }

  return(acc)
}

create_reli_gamma <- function(n_att, n_resp, prob_att) {
  gamma <- double(n_att)

  for (i in (seq_len(n_resp) - 1)) {
    for (k in (seq_len(n_att) - 1)) {
      gamma[k + 1] <- gamma[k + 1] +
        (prob_att[(i + k * n_resp) + 1] * prob_att[(i + k * n_resp) + 1] +
           (1 - prob_att[(i + k * n_resp) + 1]) *
           (1 - prob_att[(i + k * n_resp) + 1]))
    }
  }

  for (k in (seq_len(n_att) - 1)) {
    gamma[k + 1] <- gamma[k + 1] / (n_resp)
  }

  return(gamma)
}

create_reli_class_att <- function(class_att, n_resp, n_att, n_class,
                                  binary_att, prob_class) {
  for (i in (seq_len(n_resp) - 1)) {
    for (k in (seq_len(n_att) - 1)) {
      for (m in (seq_len(n_class) - 1)) {
        if (binary_att[(i + k * n_resp) + 1] == 1) {
          class_att[(m + k * n_class) + 1] <- class_att[(m + k * n_class) + 1] +
            prob_class[(m + i * n_class) + 1]
        }
      }
    }
  }

  return(class_att)
}

create_reli_consist <- function(n_att, n_resp, n_class, binary_att, class_att,
                                prob_class, strc) {
  consist <- double(4 * n_att)

  class_att <- create_reli_class_att(class_att = class_att, n_resp = n_resp,
                                     n_att = n_att, n_class = n_class,
                                     binary_att = binary_att,
                                     prob_class = prob_class)

  for (m in (seq_len(n_class) - 1)) {
    for (k  in (seq_len(n_att) - 1)) {
      class_att[(m + k * n_class) + 1] <- class_att[(m + k * n_class) + 1] /
        ((n_resp + 0.0) * strc[m + 1])
      consist[(4 * k) + 1] <- consist[(4 * k) + 1] +
        ((1.0 - class_att[(m + k * n_class) + 1]) *
           (1.0 - class_att[(m + k * n_class) + 1]) * strc[m + 1])
      consist[(4 * k + 1) + 1] <- consist[(4 * k + 1) + 1] +
        ((1.0 - class_att[(m + k * n_class) + 1]) *
           (class_att[(m + k * n_class) + 1]) * strc[m + 1])
      consist[(4 * k + 2) + 1] <- consist[(4 * k + 1) + 1]
      consist[(4 * k + 3) + 1] <- consist[(4 * k + 3) + 1] +
        ((class_att[(m + k * n_class) + 1]) *
           (class_att[(m + k * n_class) + 1]) * strc[m + 1])
    }
  }

  return(consist)
}

create_reli_list <- function(n_resp, n_att, n_class, profile_vec, class_att,
                             attr_probs, class_probs, binary_att, prob_att,
                             prob_class, strc, att_names) {
  prev <- create_reli_prev(n_att = n_att, n_class = n_class,
                           profile_vec = profile_vec, strc = strc)

  acc <- create_reli_acc(n_att = n_att, n_resp = n_resp,
                         binary_att = binary_att, prob_att = prob_att)
  new_acc <- array(acc, c(2, 2, n_att),
                   dimnames = list(c("att_pred_0", "att_pred_1"),
                                   c("att_true_0", "att_true_1"),
                                   att_names))

  gamma <- create_reli_gamma(n_att = n_att, n_resp = n_resp,
                             prob_att = prob_att)

  consist <- create_reli_consist(n_att = n_att, n_resp = n_resp,
                                 n_class = n_class, binary_att = binary_att,
                                 class_att = class_att, prob_class = prob_class,
                                 strc = strc)
  new_consist <- array(consist, c(2, 2, n_att),
                       dimnames = list(c("att_pred_1_0", "att_pred_1_1"),
                                       c("att_pred_2_0", "att_pred_2_1"),
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
    strc = strc
  )
  return(ret_list)
}

reli_list <- function(model) {
  n_resp <- dplyr::n_distinct(model$data$data$resp_id)
  n_att <- ncol(model$data$qmatrix) - 1L
  n_class  <- 2 ^ n_att
  profile_vec <- create_profiles(n_att) %>%
    as.matrix() %>%
    t() %>%
    as.vector()
  class_att <- double(n_class * n_att)

  probs <- lapply(stats::predict(model, summary = FALSE, force = TRUE),
                  clean_predicted_probs, resp_id = model$data$resp_id)
  class_probs <- probs$class_probabilities
  attr_probs <- probs$attribute_probabilities

  # map estimates
  binary_att <- attr_probs %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~dplyr::case_when(.x >= 0.5 ~ 1L,
                                                  TRUE ~ 0L))) %>%
    as.matrix() %>%
    unname() %>%
    as.vector()

  # eap estimates
  prob_att <- attr_probs %>%
    as.matrix() %>%
    unname() %>%
    as.vector()

  # class probs
  prob_class <- class_probs %>%
    as.matrix() %>%
    unname() %>%
    t() %>%
    as.vector()

  # structural parameters
  strc <- if (model$backend == "rstan" && model$method == "optim") {
    model$model$par %>%
      tibble::enframe() %>%
      dplyr::filter(grepl("^Vc", .data$name)) %>%
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

  res <- create_reli_list(n_resp = n_resp, n_att = n_att, n_class = n_class,
                          profile_vec = profile_vec, class_att = class_att,
                          attr_probs = attr_probs, class_probs = class_probs,
                          binary_att = binary_att, prob_att = prob_att,
                          prob_class = prob_class, strc = strc,
                          att_names = att_names)

  return(res)
}
