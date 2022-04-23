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
