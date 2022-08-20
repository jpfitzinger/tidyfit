
#' @importFrom dplyr select filter right_join ungroup distinct any_of all_of pull row_number
#' @importFrom tidyr drop_na unnest
#' @importFrom purrr map_dfr
#' @importFrom stats binomial gaussian poisson model.frame model.matrix model.response
#' @importFrom rlang .data

.predict <- function(.data, fit, gr_vars) {

  mask <- attr(fit, "structure")$mask
  weights <- attr(fit, "structure")$weights
  formula <- attr(fit, "formula")

  has_class <- "class" %in% colnames(fit)

  .data_core <- .data %>%
    dplyr::select(-dplyr::any_of(mask), -dplyr::all_of(gr_vars), -dplyr::any_of(weights))

  m <- stats::model.frame(formula, data = .data_core)
  x <- stats::model.matrix(formula, data = .data_core)
  y <- stats::model.response(m)

  out <- fit %>%
    dplyr::group_split(.data$model) %>%
    purrr::map_dfr(function(fit_mod) {
      fitted <- .fit_from_frame(fit_mod, x)
      fitted <- purrr::map_dfr(fitted, function(fitted_) {
        fitted_ <- fitted_ %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(row_number = dplyr::row_number()) %>%
          tidyr::gather("grid_id", "prediction", -.data$row_number) %>%
          dplyr::left_join(dplyr::mutate(.data, row_number = dplyr::row_number()), by = "row_number") %>%
          dplyr::select(-dplyr::any_of(colnames(m[,-1])), -dplyr::all_of(gr_vars), -dplyr::any_of(weights)) %>%
          dplyr::mutate(model = fit_mod$model[1])
        return(fitted_)
      }, .id = "class")
    })

  out <- out %>%
    dplyr::relocate(.data$model, .data$grid_id, .data$class, .data$row_number, .data$prediction)
  if (!has_class) out <- dplyr::select(out, -.data$class)

  return(out)

}
