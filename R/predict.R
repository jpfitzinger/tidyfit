
#' @importFrom dplyr select filter right_join ungroup distinct any_of all_of pull
#' @importFrom tidyr drop_na unnest
#' @importFrom purrr map_dfr
#' @importFrom stats binomial gaussian poisson model.frame model.matrix model.response
#' @importFrom rlang .data

.predict <- function(.data, fit, gr_vars) {

  model_list <- unique(fit$model)

  # Evaluate methods
  result <-
    purrr::map_dfr(model_list, function(mod) {

      mask <- attr(fit, "structure")$mask
      weights <- attr(fit, "structure")$weights

      .data_core <- .data %>%
        dplyr::select(-dplyr::any_of(mask), -dplyr::all_of(gr_vars), -dplyr::any_of(weights))

      fit_model <- fit %>%
        dplyr::filter(.data$model == mod)

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
        dplyr::mutate(pred = as.numeric(fit)) %>%
        dplyr::select(-dplyr::any_of(colnames(m[,-1])), -dplyr::all_of(gr_vars), -dplyr::any_of(weights)) %>%
        dplyr::mutate(model = mod)

      return(result)

    })

  return(result)

}
