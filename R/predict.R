
#' @importFrom dplyr select filter right_join ungroup distinct any_of all_of
#' @importFrom tidyr drop_na
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
      family <- attr(fit, "family")
      if (!is.null(family)) {
        if (family == "binomial") f <- stats::binomial()
        if (family == "gaussian") f <- stats::gaussian()
        if (family == "poisson") f <- stats::poisson()
      }

      .data_core <- .data %>%
        dplyr::select(-dplyr::any_of(mask), -dplyr::all_of(gr_vars), -dplyr::any_of(weights))

      fit_core <- fit %>%
        dplyr::filter(.data$model == mod) %>%
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
      if (!is.null(family)) fit <- f$linkinv(fit)

      result <- .data %>%
        dplyr::mutate(pred = as.numeric(fit)) %>%
        dplyr::select(-dplyr::any_of(colnames(m[,-1])), -dplyr::all_of(gr_vars), -dplyr::any_of(weights)) %>%
        dplyr::mutate(model = mod)

      return(result)

    })

  return(result)

}
