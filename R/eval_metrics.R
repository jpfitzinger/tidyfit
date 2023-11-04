#' @importFrom dplyr mutate select tibble group_by row_number any_of across
#' @importFrom tidyr spread
#' @importFrom rlang .data
#' @importFrom yardstick rmse mn_log_loss

.eval_metrics <- function(pred, mode, weights = NULL) {

  if (!"truth" %in% colnames(pred)) {
    metrics <- tibble(
      grid_id = unique(pred$grid_id),
      metric = NA
    )
    return(metrics)
  }

  pred <- dplyr::group_by(pred, across(any_of(c("grid_id", "class"))))
  if(is.null(weights)) {
    if (!"weights" %in% colnames(pred))
      pred <- dplyr::mutate(pred, weights = 1)
  } else {
    pred <- dplyr::mutate(pred, weights = weights)
  }
  if (mode == "regression") {
    metrics <- pred %>%
      yardstick::rmse(.data$truth, .data$prediction, case_weights = .data$weights) %>%
      dplyr::mutate(metric = .data$.estimate^2)
  } else {
    is_multinomial <- "class" %in% colnames(pred)
    if (is_multinomial) {
      level_names <- levels(pred$truth)
      metrics <- pred %>%
        dplyr::mutate(row_n = dplyr::row_number()) %>%
        tidyr::spread(.data$class, .data$prediction) %>%
        yardstick::mn_log_loss(truth = .data$truth, any_of(level_names), case_weights = .data$weights) %>%
        dplyr::mutate(metric = .data$.estimate)
    } else {
      metrics <- pred %>%
        yardstick::mn_log_loss(.data$truth, .data$prediction,
                               case_weights = .data$weights,
                               event_level = "second") %>%
        dplyr::mutate(metric = .data$.estimate)
    }
  }

  metrics <- metrics %>%
    dplyr::select(-".metric", -".estimator", -".estimate")

  return(metrics)

}
