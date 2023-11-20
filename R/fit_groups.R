#' @importFrom furrr future_pmap_dfr furrr_options
#' @importFrom dplyr left_join

.fit_groups <- function(row) {
  mod <- row$model_object$clone()
  data <- row$data$mf
  wts <- row$data$wts
  cv <- row$data$cv
  mask <- row$data$mask

  data <- data %>%
    dplyr::select(-!!mask)

  # Fit on full sample
  out_row <- dplyr::tibble(
    model = row$model,
    grid_id = row$grid_id,
    model_object = list(mod$set_args(weights = wts)$fit(data))
  )

  # Get model parameters as dataframe
  model_args <- .args_to_frame(mod)

  if (!is.null(cv) & row$model_object$cv) {

    # Cross-validation mapper
    cv_res <- furrr::future_pmap_dfr(cv, function(splits, id) {

      # Model for current slice
      res_row <- dplyr::tibble(
        model = row$model,
        grid_id = row$grid_id,
        model_object = list(mod$clone()$clear())
      )

      # Define train and test data for slice
      df_train <- rsample::training(splits) %>%
        dplyr::select(-!!mask)
      df_test <- rsample::testing(splits) %>%
        dplyr::select(-!!mask)
      train_samples <- splits$in_id
      test_samples <- rsample::complement(splits)

      # Train model
      res_row$model_object[[1]]$set_args(weights = wts[train_samples])
      res_row$model_object[[1]]$fit(df_train)

      res_row <- res_row %>%
        dplyr::mutate(slice_id = id)

      # Store test data
      if (nrow(df_test) > 0) {
        res_row <- res_row %>%
          dplyr::bind_cols(tidyr::nest(df_test, df_test = everything()))
      } else {
        res_row <- res_row %>%
          dplyr::mutate(df_test = NA)
      }

      # Store weights
      if (!is.null(wts)) {
        test_weights <- tibble(weights = wts[test_samples])
        res_row <- res_row %>%
          dplyr::bind_cols(tidyr::nest(test_weights, weights = everything()))
      }

      return(res_row)

    }, .options = furrr::furrr_options(seed = TRUE))

    # Only generate predictions, if CV slices are not returned and the method has a predict method
    if (!row$return_grid & row$model_object$has_predict_method) {

      # Out-of-sample predictions
      pred <- cv_res %>%
        purrr::transpose() %>%
        purrr::map_dfr(function(row) {
          if (is.null(row$df_test) | is.null(row$model_object$object))
            return(NULL)
          out <- row$model_object$predict(as.data.frame(row$df_test), training_context = TRUE)
          if (is.null(out))
            return(NULL)
          if (!"grid_id" %in% colnames(out)) {
            out <- out %>%
              dplyr::mutate(grid_id = row[["grid_id"]])
          }
          out <- out %>%
            dplyr::group_by(.data$grid_id) %>%
            dplyr::mutate(weights = row$weights$weights)
          return(out)
        })
      metrics <- .eval_metrics(pred, mod$mode)
    } else {
      metrics <- dplyr::tibble(metric = NA, grid_id = NA)
    }

    # Unnest hyperparameters and join metrics
    cv_res <- cv_res %>%
      dplyr::select(-any_of(c("weights", "df_test"))) %>%
      dplyr::group_nest(dplyr::row_number()) %>%
      dplyr::pull(.data$data) %>%
      purrr::map_dfr(.unnest_args, model_args) %>%
      dplyr::left_join(metrics, by = c("grid_id"))

  } else {
    cv_res <- NULL
  }

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
