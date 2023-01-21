
#' @importFrom dials grid_regular penalty mixture
#' @importFrom stats terms

.default_hp_grid <- function(model_method, control, formula, data) {

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
    #nvars <- length(labels(stats::terms(formula, data = data)))
    if (!any(c("ncomp_pct", "ncomp") %in% args)) {
      grid$ncomp_pct <- seq(0, 1, length.out = 20)
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
  if (model_method == "svm") {
    if (!"cost" %in% args) {
      grid$cost <- dials::grid_regular(dials::cost(), levels = 10)$cost
    }
  }

  return(grid)

}
