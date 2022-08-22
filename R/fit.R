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
    .index,
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

  idx <- .get_index_from_df(.data, .index, formula)
  .data <- .data %>%
    dplyr::select(-dplyr::any_of(.index))

  m <- .model_frame(formula = formula, data = .data)
  x <- .model_matrix(formula, m)
  y <- .model_response(m)

  # Prepare CV
  if (.cv == "none")
    cv <- dplyr::tibble(splits = list(m))
  if (.cv == "initial_split")
    cv <- dplyr::tibble(
      splits = list(do.call(rsample::initial_split, append(list(data = m), .cv_args))),
      id = "Initial_Split"
    )
  if (.cv == "initial_time_split")
    cv <- dplyr::tibble(
      splits = list(do.call(rsample::initial_time_split, append(list(data = m), .cv_args))),
      id = "Initial_Split"
    )
  if (.cv == "vfold")
    cv <- do.call(rsample::vfold_cv, append(list(data = m), .cv_args))
  if (.cv == "loo")
    cv <- do.call(rsample::loo_cv, append(list(data = m), .cv_args))
  if (.cv == "rolling_origin")
    cv <- do.call(rsample::rolling_origin, append(list(data = m), .cv_args))

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

            df_train_x <- .model_matrix(formula, df_train)
            df_train_y <- .model_response(df_train)
            df_test_x <- .model_matrix(formula, df_test)
            df_test_y <- .model_response(df_test)

            model_args <- list(x = df_train_x, y = df_train_y,
                               .remove_dependent_features = .remove_dependent_features)
            if (!model(.check_family = TRUE)) {
              model_args <- append(model_args, list(family = family))
            }
            if (!is.null(wts)) {
              model_args <- append(model_args, list(weights = wts[train_samples]))
            }
            if (!is.null(idx)) {
              model_args <- append(model_args, list(index_col = idx[train_samples]))
              idx_test <- idx[test_samples]
            } else {
              idx_test <- NULL
            }
            model_args$model_formula <- list(formula)

            result <- do.call(model, model_args)
            crit <- .eval_metrics(result, df_test_x, df_test_y, idx_test, .index)

            result <- result %>%
              dplyr::mutate(slice_id = id) %>%
              dplyr::left_join(crit, by = "grid_id")

            return(result)

          })#,
          #.options = furrr::furrr_options(seed = TRUE))

      } else {
        result <- NULL
      }

      model_args <- list(x = x, y = y,
                         .remove_dependent_features = .remove_dependent_features)
      if (!model(.check_family = TRUE)) {
        model_args <- append(model_args, list(family = family))
      }
      if (!is.null(wts)) {
        model_args <- append(model_args, list(weights = wts))
      }
      if (!is.null(idx)) {
        model_args <- append(model_args, list(index_col = idx))
      }
      model_args$model_formula <- list(formula)

      result_all <- do.call(model, model_args) %>%
        mutate(slice_id = "FULL")

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
