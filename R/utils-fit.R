backend_choices <- function() {
  c("rstan", "cmdstanr")
}

dcm_choices <- function() {
  c("lcdm", "dina", "dino")
}

install_backend <- function(pkg, ...) {
  if (pkg == "cmdstanr") pkg <- "stan-dev/cmdstanr"

  if (rlang::is_installed("pak")) {
    pkg_install <- rlang::env_get(rlang::ns_env("pak"),
                                  "pkg_install")
    pkg_install(pkg, ask = FALSE)
  } else {
    utils::install.packages(pkg)
  }

  if (pkg == "stan-dev/cmdstanr") {
    if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
      cnd <- rlang::error_cnd(class = "measr_error_CmdStan_not_found",
                              message = glue::glue("CmdStan is required for ",
                                                   "`backend = \"cmdstanr\"`"))

      header <- rlang::cnd_header(cnd)
      question <- "Would you like to install it?"

      cat(paste_line(
        paste0(info(), " ", header),
        paste0(cross(), " ", question),
        .trailing = TRUE
      ))

      if (utils::menu(c("Yes", "No")) != 1) {
        # Pass condition in case caller sets up an `abort` restart
        invokeRestart("abort", cnd)
      }

      cmdstanr::install_cmdstan()
    }
  }
}

calc_xi <- function(alpha, qmatrix, type) {
  xi <- matrix(0, nrow = nrow(qmatrix), ncol = nrow(alpha))

  if (type == "lcdm") {
    xi <- xi
  } else if (type == "dina") {
    for (i in 1:nrow(qmatrix)) {
      for (c in 1:nrow(alpha)) {
        xi[i, c] <- prod(alpha[c, ] ^ qmatrix[i, ])
      }
    }
  } else if (type == "dino") {
    for (i in 1:nrow(qmatrix)) {
      for (c in 1:nrow(alpha)) {
        xi[i, c] <- 1 - prod((1 - alpha[c, ]) ^ qmatrix[i, ])
      }
    }
  }

  return(xi)
}

#https://github.com/stan-dev/cmdstanr/issues/447
fix_cmdstanr_names <- function(obj) {
  obj@sim$samples <- lapply(obj@sim$samples,
                            function(x, obj) {
                              names(x) <- obj@sim$fnames_oi
                              return(x)
                            },
                            obj = obj)
  return(obj)
}
