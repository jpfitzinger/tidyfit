
#' @importFrom dplyr select filter right_join ungroup distinct any_of all_of pull
#' @importFrom tidyr drop_na unnest
#' @importFrom purrr map_dfr
#' @importFrom stats binomial gaussian poisson model.frame model.matrix model.response
#' @importFrom rlang .data

.predict <- function(.data, fit, gr_vars) {

  eval_list <- fit %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$model, .data$grid_id) %>%
    dplyr::distinct()

  # Evaluate methods
  result <-
    purrr::map2_dfr(eval_list$model, eval_list$grid_id, function(mod, grd) {

      mask <- attr(fit, "structure")$mask
      weights <- attr(fit, "structure")$weights

      .data_core <- .data %>%
        dplyr::select(-dplyr::any_of(mask), -dplyr::all_of(gr_vars), -dplyr::any_of(weights))

      fit_model <- fit %>%
        dplyr::filter(.data$model == mod, .data$grid_id == grd)

      f <- fit_model %>%
        tidyr::unnest(.data$model_info) %>%
        dplyr::pull(.data$family) %>%
        .[1] %>%
        .[[1]]

      fit_core <- fit_model %>%
        dplyr::right_join(.data %>% select(!!gr_vars), by = c(gr_vars)) %>%
        dplyr::ungroup()

      formula <- attr(fit, "formula")
      m <- stats::model.frame(formula, data = .data_core)
      x <- stats::model.matrix(formula, data = .data_core)
      y <- stats::model.response(m)

      beta <- fit_core %>%
        dplyr::select(.data$variable, .data$beta) %>%
        dplyr::distinct() %>%
        tidyr::drop_na()

      x <- x[, beta$variable]
      beta <- beta %>% dplyr::select(-.data$variable)
      fit <- data.matrix(x) %*% data.matrix(beta)
      if (!is.null(f)) fit <- f$linkinv(fit)

      result <- .data %>%
        dplyr::mutate(prediction = as.numeric(fit)) %>%
        dplyr::select(-dplyr::any_of(colnames(m[,-1])), -dplyr::all_of(gr_vars), -dplyr::any_of(weights)) %>%
        dplyr::mutate(model = mod, grid_id = grd)

      return(result)

    })

  return(result)

}
