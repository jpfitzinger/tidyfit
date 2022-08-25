#' @importFrom dplyr mutate select tibble group_by row_number any_of
#' @importFrom tidyr spread
#' @importFrom rlang .data
#' @importFrom yardstick rmse mn_log_loss

.eval_metrics <- function(pred, family) {

  if ("grid_id" %in% colnames(pred)) {
    pred <- dplyr::group_by(pred, .data$grid_id)
  }
  if(!"weights" %in% colnames(pred)) {
    pred <- dplyr::mutate(pred, weights = 1)
  }
  if (family$family == "gaussian") {
    metrics <- pred %>%
      yardstick::rmse(truth, prediction, case_weights = weights) %>%
      dplyr::mutate(metric = .data$.estimate^2)
  } else {
    is_multinomial <- "class" %in% colnames(pred)
    if (is_multinomial) {
      level_names <- levels(pred$truth)
      metrics <- pred %>%
        dplyr::group_by(.data$class, .add = TRUE) %>%
        dplyr::mutate(row_n = dplyr::row_number()) %>%
        tidyr::spread(.data$class, .data$prediction) %>%
        yardstick::mn_log_loss(truth = .data$truth, any_of(level_names), case_weights = weights) %>%
        dplyr::mutate(metric = -.data$.estimate)
    } else {
      metrics <- pred %>%
        yardstick::mn_log_loss(truth, prediction, case_weights = weights) %>%
        dplyr::mutate(metric = -.data$.estimate)
    }
  }

  metrics <- metrics %>%
    dplyr::select(-.data$.metric, -.data$.estimator, -.data$.estimate)

  return(metrics)

}
