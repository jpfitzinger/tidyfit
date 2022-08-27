#' @importFrom purrr map_dfr
#' @importFrom stats binomial gaussian poisson
#' @importFrom rsample vfold_cv loo_cv rolling_origin complement
#' @importFrom furrr future_pmap_dfr furrr_options
#' @importFrom dplyr select tibble mutate left_join bind_rows
#' @importFrom rlang .data

.fit <- function(
    .data,
    formula,
    model_list,
    .cv,
    .cv_args,
    .weights,
    gr_vars,
    .mask,
    family,
    .force_cv,
    .remove_dependent_features
    ) {

  .data <- .data %>%
    dplyr::select(-!!gr_vars, -!!.mask)

  if (!is.null(.weights)) {
    wts <- .data %>%
      dplyr::pull(!!.weights)
    .data <- .data %>%
      dplyr::select(-dplyr::all_of(.weights))
  } else {
    wts <- NULL
  }

  mf <- data.frame(.data, check.names = FALSE)

  # Prepare CV
  if (!is.null(.cv_args$index)) {
    if (!.cv_args$index %in% colnames(mf))
      stop("'index_col' not found in the data")
  }

  cv_func <- switch(
    .cv,
    initial_split = rsample::initial_split,
    initial_time_split = rsample::initial_time_split,
    vfold = rsample::vfold_cv,
    loo = rsample::loo_cv,
    rolling_origin = rsample::rolling_origin,
    sliding_index = rsample::sliding_index,
    sliding_period = rsample::sliding_period,
    sliding_window = rsample::sliding_window,
    bootstraps = rsample::bootstraps
    )

  if (.cv == "none") {
    cv <- dplyr::tibble(splits = list(mf))
  } else {
    cv <- do.call(cv_func, append(list(data = mf), .cv_args))
    if (inherits(cv, "rsplit"))
      cv <- dplyr::tibble(splits = list(cv), id = .cv)
    if (!is.null(.cv_args$index)) {
      adj_id <- seq(1L, nrow(mf), by = ifelse(is.null(.cv_args$step), 1, .cv_args$step))
      adj_id <- adj_id[(length(adj_id) - nrow(cv) + 1):length(adj_id)]
      cv$id <- as.character(mf[adj_id, .cv_args$index])
    }
  }

  # Evaluate methods
  result <- model_list %>%
    purrr::map_dfr(function(model) {

      model_name <- model(.return_method_name = TRUE)

      if (.force_cv) {
        do_cv <- TRUE
      } else {
        do_cv <- .check_method(model_name, "cv")
      }
      if (!.check_method(model_name, "uses_index")) idx <- NULL

      if (.cv != "none" & do_cv) {

        result <- cv %>%
          #furrr::future_pmap_dfr(function(splits, id) {
          purrr::pmap_dfr(function(splits, id) {

            df_train <- rsample::training(splits)
            df_test <- rsample::testing(splits)
            train_samples <- splits$in_id
            test_samples <- rsample::complement(splits)

            model_args <- list(formula = formula, data = df_train,
                               .remove_dependent_features = .remove_dependent_features)
            if (!model(.check_family = TRUE)) {
              model_args <- append(model_args, list(family = family))
            }
            if (!is.null(wts)) {
              model_args <- append(model_args, list(weights = wts[train_samples]))
            }

            result_raw <- do.call(model, model_args)
            result <- .unnest_settings(result_raw)
            result <- dplyr::mutate(result, slice_id = id)
            pred <- predict.tidyfit.models(result_raw, df_test, weights = wts[test_samples], .keep_grid_id = TRUE)
            if (nrow(pred) == 0) return(dplyr::mutate(result, metric = 0))
            metrics <- .eval_metrics(pred, family)
            result <- result %>%
              dplyr::mutate(slice_id = id) %>%
              dplyr::left_join(metrics, by = "grid_id")

            return(result)

          })#,
          #.options = furrr::furrr_options(seed = TRUE))

      } else {
        result <- NULL
      }

      model_args <- list(formula = formula, data = mf,
                         .remove_dependent_features = .remove_dependent_features)
      if (!model(.check_family = TRUE)) {
        model_args <- append(model_args, list(family = family))
      }
      if (!is.null(wts)) {
        model_args <- append(model_args, list(weights = wts))
      }

      result_all <- do.call(model, model_args) %>%
        .unnest_settings() %>%
        dplyr::mutate(slice_id = "FULL")

      # Ensure identical grids as in CV (can differ if some slices have errors)
      if (!is.null(result)) {
        result <- result %>%
          dplyr::filter(.data$grid_id %in% result_all$grid_id)
      }

      result <- dplyr::bind_rows(result_all, result)

      return(result)

    }, .id = "model")

  return(result)

}
