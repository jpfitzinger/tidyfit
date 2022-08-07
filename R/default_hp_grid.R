
#' @importFrom dials grid_regular penalty mixture

.default_hp_grid <- function(model_method, control, nvars) {

  args <- names(control)
  grid <- list()

  if (model_method %in% c("lasso", "ridge", "enet", "adalasso")) {
    if (!"lambda" %in% args) {
      grid$lambda <- dials::grid_regular(dials::penalty(), levels = 100)$penalty
    }
  }
  if (model_method == "enet") {
    if (!"alpha" %in% args) {
      grid$alpha <- dials::grid_regular(dials::mixture(), levels = 5)$mixture
    }
  }
  if (model_method %in% c("pcr", "plsr")) {
    if (!"ncomp" %in% args) {
      grid$ncomp <- unique(seq(1, nvars, length.out = 20))
    }
  }
  if (model_method == "hfr") {
    if (!"kappa" %in% args) {
      grid$kappa <- seq(0, 1, by = 0.05)
    }
  }
  if (model_method == "boost") {
    if (!"mstop" %in% args) {
      grid$mstop <- c(100, 500, 1000, 5000)
    }
    if (!"nu" %in% args) {
      grid$nu <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25)
    }
  }

  return(grid)

}
