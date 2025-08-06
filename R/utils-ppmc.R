pw_or <- function(dat) {
  dat <- as.matrix(dat)
  p <- ncol(dat)
  ind <- t(utils::combn(p, 2))
  nind <- nrow(ind)
  or <- numeric(nind)

  for (i in 1:nind) {
    xtab <- table(dat[, ind[i, 1]], dat[, ind[i, 2]])
    n00 <- xtab[1, 1]
    n01 <- xtab[1, 2]
    n10 <- xtab[2, 1]
    n11 <- xtab[2, 2]
    or[i] <- (n00 * n11) / (n01 * n10)
  }
  pwor <- tibble::as_tibble(ind, .name_repair = ~ c("item_1", "item_2")) |>
    dplyr::mutate(or = or)
  return(pwor)
}
