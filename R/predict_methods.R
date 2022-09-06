.predict.glmnet <- function(object, data, formula = NULL, lambda = NULL, inner_grid = NULL, family = NULL, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  x <- stats::model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  pred_mat <- stats::predict(object, x, type = "response", s = lambda)

  dimnames(pred_mat)[[length(dim(pred_mat))]] <- inner_grid$grid_id[appr_in(inner_grid$lambda, lambda)]
  if (length(dim(pred_mat))==3) {
    class_vals <- dimnames(pred_mat)[[2]]
  } else {
    class_vals <- NULL
  }
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(row_n = dplyr::row_number())
  if (!is.null(truth)) {
    pred <- dplyr::mutate(pred, truth = truth)
  }
  if (family == "multinomial") {
    pred <- pred %>%
      tidyr::pivot_longer(-dplyr::any_of(c("truth", "row_n")),
                          names_to = c("class", "grid_id"),
                          values_to = "prediction",
                          names_sep = "\\.")
  } else {
    pred <- pred %>%
      tidyr::gather("grid_id", "prediction", -dplyr::any_of(c("truth", "row_n")))
  }
  pred <- pred %>%
    dplyr::select(-.data$row_n)
  if (length(class_vals)==2) {
    pred <- pred %>%
      dplyr::filter(.data$class == sort(class_vals)[2]) %>%
      dplyr::select(-.data$class)
  }
  return(pred)
}
