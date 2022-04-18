paste_line <- function(..., .trailing = FALSE) {
  text <- rlang::chr(...)

  if (.trailing) {
    paste0(text, "\n", collapse = "")
  } else {
    paste(text, collapse = "\n")
  }
}

has_crayon <- function() {
  rlang::is_installed("crayon") && crayon::has_color()
}

red  <- function(x) if (has_crayon()) crayon::red(x)  else x
blue <- function(x) if (has_crayon()) crayon::blue(x) else x

info <- function() {
  i <- if (rlang::is_installed("cli")) cli::symbol$info else "i"
  blue(i)
}
cross <- function() {
  x <- if (rlang::is_installed("cli")) cli::symbol$cross else "x"
  red(x)
}
