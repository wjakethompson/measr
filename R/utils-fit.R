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
    for (i in seq_len(nrow(qmatrix))) {
      for (c in seq_len(nrow(alpha))) {
        xi[i, c] <- prod(alpha[c, ] ^ qmatrix[i, ])
      }
    }
  } else if (type == "dino") {
    for (i in seq_len(nrow(qmatrix))) {
      for (c in seq_len(nrow(alpha))) {
        xi[i, c] <- 1 - prod((1 - alpha[c, ]) ^ qmatrix[i, ])
      }
    }
  }

  return(xi)
}

check_file_exists <- function(file, refit, dat, qmat, code, method) {
  if (!is.null(file)) {
    if (fs::file_exists(file)) {
      prev <- readRDS(file)
      if (prev$backend == "cmdstanr") {
        prev$model <- readRDS(gsub("\\.rds", "-cmdstanr.rds", file))
      }
    } else {
      return(list(return = FALSE))
    }

    if (refit == "never") {
      return(list(return = TRUE, obj = prev))
    } else if (refit == "on_change") {
      # if fitted model matches current args and "on_change", return prev fit
      if (all(identical(prev$data, list(data = dat, qmatrix = qmat)),
              identical(prev$prior, code$prior),
              identical(prev$method, method)) &&
          refit == "on_change") {
        return(list(return = TRUE, obj = prev))
      } else {
        return(list(return = FALSE))
      }
    }
  } else {
    return(list(return = FALSE))
  }
}

extract_algorithm <- function(model, pars, method) {
  algorithm <- if (method == "optim") {
    pars$algorithm
  } else if ("stanfit" %in% class(model)) {
    model@stan_args[[1]]$algorithm
  } else if ("CmdStanFit" %in% class(model)) {
    model$metadata()$algorithm
  }

  return(algorithm)
}

get_version_info <- function(cmdstanr) {
  version_info <- list(R = base::getRversion(),
                       measr = utils::packageVersion("measr"),
                       rstan = utils::packageVersion("rstan"),
                       StanHeaders = utils::packageVersion("StanHeaders"))
  if (cmdstanr) {
    version_info$cmdstanr <- utils::packageVersion("cmdstanr")
    version_info$cmdstan <- as.package_version(cmdstanr::cmdstan_version())
  }

  return(version_info)
}
