# Internal helper functions

#' @importFrom purrr cross safely quietly
#' @importFrom dplyr rename mutate select relocate any_of
#' @importFrom tidyr unnest nest
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data

# Wrap vector/array arguments in list and return a grid of settings
.args_to_grid <- function(model_method, control) {

  # Exceptions for vectors that are handled at a lower level
  if (!is.null(control$weights)) {
    control$weights <- list(control$weights)
  }
  if (!is.null(control$index_col)) {
    control$index_col <- list(control$index_col)
  }
  if (!is.null(control$lambda) & model_method %in% c("lasso", "enet", "ridge", "adalasso")) {
    control$lambda <- list(control$lambda)
  }
  if (!is.null(control$kappa) & model_method == "hfr") {
    control$kappa <- list(control$kappa)
  }

  control <- .func_to_list(control)
  grid <- purrr::cross(control)
  return(grid)

}

# Wrap functions in list
.func_to_list <- function(control) {

  control <- lapply(control, function(arg) {
    if (inherits(arg, c("function", "family"))) {
      list(arg)
    } else {
      arg
    }
  })

  return(control)

}

# Combine grid_id in settings col and model frame
.unnest_settings <- function(model_frame) {

  if ("settings" %in% colnames(model_frame)) {
    model_frame <- model_frame %>%
      dplyr::rename(grid_id_ = .data$grid_id) %>%
      tidyr::unnest(.data$settings)
    if ("grid_id" %in% colnames(model_frame)) {
      model_frame <- model_frame %>%
        dplyr::mutate(grid_id = paste(substring(.data$grid_id_,1, 4), .data$grid_id, sep = ".")) %>%
        dplyr::select(-.data$grid_id_)
    } else {
      model_frame <- model_frame %>%
        dplyr::rename(grid_id = .data$grid_id_)
    }
    model_frame <- model_frame %>%
      dplyr::relocate(.data$grid_id) %>%
      tidyr::nest(settings = -any_of(c("grid_id", "estimator", "size", "handler", "warnings", "messages"))) %>%
      dplyr::relocate(any_of(c("warnings", "messages")), .after = .data$settings)
  }

  return(model_frame)

}

.names_map <- function(names_vec) {
  names_chk <- make.names(names_vec)
  names(names_vec) <- names_chk
  names_vec["(Intercept)"] <- "(Intercept)"
  return(names_vec)
}
