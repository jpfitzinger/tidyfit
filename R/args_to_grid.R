
#' @importFrom purrr cross

.args_to_grid <- function(model_method, control) {

  # Exceptions for vectors that are handled at a lower level
  if (!is.null(control$weights)) {
    control$weights <- list(control$weights)
  }
  if (!is.null(control$lambda) & model_method %in% c("lasso", "enet", "ridge", "adalasso")) {
    control$lambda <- list(control$lambda)
  }
  if (!is.null(control$kappa) & model_method == "hfr") {
    control$kappa <- list(control$kappa)
  }

  control <- func_to_list(control)
  grid <- purrr::cross(control)
  return(grid)

}
