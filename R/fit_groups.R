#' @importFrom furrr future_pmap_dfr furrr_options

.fit_groups <- function(row) {
  mod <- row$model_object
  data <- row$data$mf
  wts <- row$data$wts
  cv <- row$data$cv

  if (!is.null(cv) & row$model_object$cv) {
    cv_res <- furrr::future_pmap_dfr(cv, function(splits, id) {
      res_row <- dplyr::tibble(
        model = row$model,
        grid_id = row$grid_id,
        model_object = list(row$model_object$clone())
      )
      df_train <- rsample::training(splits)
      df_test <- rsample::testing(splits)
      train_samples <- splits$in_id
      test_samples <- rsample::complement(splits)
      res_row$model_object[[1]]$set_args(weights = wts[train_samples])
      res_row$model_object[[1]]$fit(df_train)
      if (nrow(df_test) > 0 & !row$return_slices) {
        pred <- predict.tidyfit.models(res_row, df_test, .keep_grid_id = TRUE)
        metrics <- .eval_metrics(pred, res_row$model_object[[1]]$mode, weights = wts[test_samples])
      } else {
        metrics <- dplyr::tibble(metric = NA, grid_id = NA)
      }
      res_row <- .unnest_args(res_row)
      res_row <- res_row %>%
        dplyr::mutate(slice_id = id) %>%
        dplyr::left_join(metrics, by = "grid_id")
      return(res_row)
    }, .options = furrr::furrr_options(seed = TRUE))
  } else {
    cv_res <- NULL
  }

  out_row <- dplyr::tibble(
    model = row$model,
    grid_id = row$grid_id,
    model_object = list(mod$clone()$set_args(weights = wts)$fit(data))
  )
  out_row <- .unnest_args(out_row)
  out_row <- out_row %>%
    dplyr::mutate(slice_id = "FULL")
  # Ensure identical grids as in CV (can differ if some slices have errors)
  if (!is.null(cv_res)) {
    cv_res <- cv_res %>%
      dplyr::filter(.data$grid_id %in% out_row$grid_id)
    result <- dplyr::bind_rows(out_row, cv_res)
  } else {
    result <- out_row
  }
  result <- dplyr::bind_cols(row$data$grps, result)
  return(result)
}
