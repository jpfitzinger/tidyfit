# Internal helper functions

#' @importFrom purrr cross safely quietly map_dfr transpose
#' @importFrom dplyr rename mutate select relocate any_of summarise ungroup pull group_nest row_number across
#' @importFrom tibble enframe
#' @importFrom tidyr unnest nest pivot_wider
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom utils object.size
#' @importFrom rlang .data

# Wrap vector/array arguments in list and return a grid of settings
.args_to_grid <- function(model_method, control) {

  vector_args <- METHOD_REGISTER[[model_method]]$vector_args
  list_args <- METHOD_REGISTER[[model_method]]$list_args
  no_grid_args <- METHOD_REGISTER[[model_method]]$no_grid_args

  for (arg_name in vector_args) {
    arg = control[[arg_name]]
    if (!is.null(arg)) {
      if ((arg_name %in% no_grid_args) | !inherits(arg, "list")) {
        control[[arg_name]] <- list(arg)
      }
    }
  }

  for (arg_name in list_args) {
    arg = control[[arg_name]]
    if (!is.null(arg)) {
      if ((arg_name %in% no_grid_args) | !inherits(arg[[1]], "list")) {
        control[[arg_name]] <- list(arg)
      }
    }
  }

  # Exceptions for vectors that are handled at a lower level
  if (!is.null(control[["weights"]])) {
    control$weights <- list(control[["weights"]])
  }

  control <- .func_to_list(control)
  grid <- tidyr::expand_grid(!!! control) |>
    purrr::transpose()
  if (length(grid)==0) grid <- list(grid)
  return(grid)

}

# Wrap functions in list
.func_to_list <- function(control) {

  control <- lapply(control, function(arg) {
    if (inherits(arg, c("function", "family", "R6"))) {
      list(arg)
    } else {
      arg
    }
  })

  return(control)

}

.args_to_frame <- function(mod) {
  if (length(mod$args) > 0) {
    args <- .func_to_list(mod$args)
    settings <- tibble::enframe(args) |>
      tidyr::pivot_wider() |>
      dplyr::summarise(across(.cols = dplyr::everything(), .fns = ~ if(length(unlist(.)) == 1) unlist(.) else .))
  } else {
    settings <- NULL
  }
  return(settings)
}

.make_model_cols <- function(df) {
  make_vector <- function(lst) {
    lst[sapply(lst, is.null)] <- NA
    if (all(is.na(lst))) return(NULL)
    unlist(lst)
  }
  df <- df |>
    dplyr::mutate(estimator_fct = make_vector(purrr::map(.data$model_object, function(mod) mod$estimator))) |>
    dplyr::mutate(`size (MB)` = make_vector(purrr::map(.data$model_object, function(mod) utils::object.size(mod$object)))/1e6) |>
    dplyr::mutate(errors = make_vector(purrr::map(.data$model_object, function(mod) mod$error))) |>
    dplyr::mutate(warnings = make_vector(purrr::map(.data$model_object, function(mod) mod$warnings))) |>
    dplyr::mutate(messages = make_vector(purrr::map(.data$model_object, function(mod) mod$messages)))
  df
}

.reassign_model_info <- function(df) {
  df |>
    dplyr::ungroup() |>
    dplyr::group_nest(row_number()) |>
    dplyr::pull(.data$data) |>
    purrr::map_dfr(function(row) {
      row$model_object[[1]] <- row$model_object[[1]]$clone()
      row$model_object[[1]]$grid_id <- row$grid_id
      row$model_object[[1]]$args <- unlist(purrr::transpose(row$settings[[1]]), recursive = FALSE)
      row
    })
}

appr_in <- function(a, b) {
  round(a, 12) %in% round(b, 12)
}

.coef_rescaler <- function(coefs,
                           x_mean = NULL,
                           x_sd = NULL,
                           y_mean = NULL,
                           y_sd = NULL) {

  rescaled_coefs <- coefs
  includes_intercept <- "(Intercept)" %in% names(coefs)
  var_names <- names(coefs)
  var_names <- var_names[var_names != "(Intercept)"]

  if (!is.null(x_sd)) {
    rescaled_coefs[var_names] = coefs[var_names] / x_sd[var_names]
  }
  if (!is.null(x_mean) & includes_intercept) {
    rescaled_coefs["(Intercept)"] <- coefs["(Intercept)"] - crossprod(rescaled_coefs[var_names], x_mean[var_names])
  }
  if (!is.null(y_sd)) {
    rescaled_coefs <- rescaled_coefs * y_sd
  }
  if (!is.null(y_mean) & includes_intercept) {
    rescaled_coefs["(Intercept)"] <- rescaled_coefs["(Intercept)"] + y_mean
  }

  return(rescaled_coefs)

}

.warn_and_remove_errors <- function(object) {
  object <- object |>
    dplyr::filter(as.logical(map(.data$model_object, function(obj) {
      if (is.null(obj$object))
        warning(paste0("No model fitted for '", obj$method, "'. Check errors.", call. = FALSE), call. = FALSE)
      return(!is.null(obj$object))
    })))
  return(object)
}

.nest_settings <- function(object) {
  if (!"settings" %in% colnames(object)) {
    valid_cols <- c("model", "estimator_fct", "size (MB)", "grid_id", "grid_id_",
                    "model_object", "errors", "warnings", "messages")
    object <- object |>
      tidyr::nest(settings = -any_of(c(valid_cols, attr(object, "structure")$groups)))
  }
  return(object)
}
