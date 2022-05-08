

hfr <- function(x, y, .hyper_grid) {

  if (is.null(.hyper_grid)) {
    kappa_grid <- seq(0, 1, by = 0.1)
  } else {
    if (is.null(.hyper_grid$kappa)) {
      kappa_grid <- seq(0, 1, by = 0.1)
    } else {
      kappa_grid <- .hyper_grid$kappa
    }
  }
  m <- hfr::cv.hfr(x, y, kappa_grid = kappa_grid, nfolds = 1)
  return(coef(m))

}
