.coef.glmnet <- function(object, lambda = NULL, inner_grid = NULL, ...) {
  estimates <- broom::tidy(object)
  lambdaSel <- lambda
  estimates <- estimates %>%
    dplyr::mutate(grid_id = inner_grid[.data$step, "grid_id"]) %>%
    dplyr::select(-.data$step) %>%
    dplyr::mutate(term = ifelse(.data$term == "", "(Intercept)", .data$term)) %>%
    dplyr::filter(appr_in(.data$lambda, lambdaSel))
  if ("class" %in% colnames(estimates)) {
    class_vals <- unique(estimates$class)
    if (length(class_vals) == 2) {
      estimates <- estimates %>%
        dplyr::mutate(estimate = .data$estimate * 2) %>%
        dplyr::filter(.data$class == sort(class_vals)[2]) %>%
        dplyr::select(-.data$class)
    }
  }
  return(estimates)
}
