
#' @importFrom dplyr select filter right_join ungroup distinct any_of all_of pull row_number
#' @importFrom tidyr drop_na unnest
#' @importFrom purrr map_dfr
#' @importFrom stats binomial gaussian poisson model.frame model.matrix model.response
#' @importFrom rlang .data

.predict <- function(.data, fit, gr_vars) {

  mask <- attr(fit, "structure")$mask
  weights <- attr(fit, "structure")$weights
  formula <- attr(fit, "formula")
  index_cols <- attr(fit, "structure")$index

  has_class <- "class" %in% colnames(fit)

  idx <- .get_index_from_df(.data, index_cols, formula)

  .data_core <- .data %>%
    dplyr::select(-dplyr::any_of(mask),
                  -dplyr::all_of(gr_vars),
                  -dplyr::any_of(weights),
                  -dplyr::any_of(index_cols))

  fit_core <- fit %>%
    dplyr::right_join(.data %>% dplyr::select(!!gr_vars) %>% dplyr::distinct(), by = c(gr_vars)) %>%
    dplyr::ungroup()

  m <- .model_frame(formula, data = .data_core)
  x <- .model_matrix(formula, data = .data_core)
  y <- .model_response(m)

  out <- fit_core %>%
    tidyr::unnest(.data$model_info) %>%
    dplyr::group_by(.data$model) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(fit_mod) {
      fitted <- .fit_from_frame(fit_mod, x, idx, index_cols)
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
