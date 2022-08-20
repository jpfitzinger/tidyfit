# Internal helper functions

#' @importFrom purrr cross map
#' @importFrom dplyr pull select filter
#' @importFrom tidyr spread
#' @importFrom rlang .data

# Wrap vector/array arguments in list and return a grid of settings
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

# Create a matrix of fitted values using coefficients and data
.fit_from_frame <- function(coefs_df, x) {

  f <- coefs_df %>%
    dplyr::pull(.data$family) %>%
    unique %>%
    .[[1]]

  has_class <- "class" %in% colnames(coefs_df)

  if (!has_class) {
    beta <- coefs_df %>%
      dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
      tidyr::spread(.data$grid_id, .data$beta)

    x <- x[, beta$variable]
    beta <- beta[, -1]
    fitted_values <- x %*% data.matrix(beta)
    fitted_values <- f$linkinv(fitted_values)
    fitted_values <- list(fitted_values)
  } else {
    class_vals <- unique(coefs_df$class)
    fitted_values_list <- class_vals %>%
      purrr::map(function(cls) {
        beta <- coefs_df %>%
          dplyr::filter(class == cls) %>%
          dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
          tidyr::spread(.data$grid_id, .data$beta)

        x <- x[, beta$variable]
        beta <- beta[, -1]
        fitted_values <- exp(x %*% data.matrix(beta))
        return(fitted_values)
      })
    fitted_values_sum <- fitted_values_list[[1]]
    for (i in 2:length(class_vals)) {
      fitted_values_sum <- fitted_values_sum + fitted_values_list[[i]]
    }
    fitted_values <- list()
    for (i in 1:length(class_vals)) {
      fitted_values[[i]] <- fitted_values_list[[i]] / fitted_values_sum
    }
    names(fitted_values) <- class_vals
  }

  return(fitted_values)

}
