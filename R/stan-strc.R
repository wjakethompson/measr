strc_choices <- function() {
  c("unconstrained", "independent")
}

strc_script <- function(strc) {
  strc <- rlang::arg_match(strc, values = strc_choices())

  parameters_block <- if (strc == "unconstrained") {
    glue::glue(
      "  simplex[C] Vc;                  // base rates of class membership",
      .trim = FALSE
    )
  } else if (strc == "independent") {
    glue::glue("  array[A] real<lower=0,upper=1> eta;", .trim = FALSE)
  }

  transformed_parameters_block <- if (strc == "unconstrained") {
    glue::glue("  vector[C] log_Vc = log(Vc);", .trim = FALSE)
  } else if (strc == "independent") {
    glue::glue(
      "  simplex[C] Vc;",
      "  vector[C] log_Vc;",
      "  for (c in 1:C) {{",
      "    Vc[c] = 1;",
      "    for (a in 1:A) {{",
      "      Vc[c] = Vc[c] * eta[a]^Alpha[c,a] * ",
      "              (1 - eta[a]) ^ (1 - Alpha[c,a]);",
      "    }}",
      "  }}",
      "  log_Vc = log(Vc);", .sep = "\n", .trim = FALSE
    )
  }

  return(list(parameters = parameters_block,
              transformed = transformed_parameters_block))
}
