

.fit <- function(.data, formula, model_list, .cv, .cv_args, .hyper_grid, .weights, gr_vars, .mask, .return_slices) {

  .data <- .data %>%
    select(-!!gr_vars, -!!.mask)

  m <- model.frame(formula = formula, data = .data)

  # Prepare CV
  if (.cv == "none")
    cv <- tibble(splits = list(m))
  if (.cv == "vfold")
    cv <- do.call(rsample::vfold_cv, append(list(data = m), .cv_args))
  if (.cv == "loo")
    cv <- cv <- do.call(rsample::loo_cv, append(list(data = m), .cv_args))
  if (.cv == "ts")
    cv <- cv <- do.call(rsample::rolling_origin, append(list(data = m), .cv_args))

  # Evaluate methods
  result <-
    map2_dfr(model_list, names(model_list), function(model, nam) {

      if (.cv != "none") {

        result <- cv$splits %>%
          furrr::future_map(function(split) {

            df_train <- rsample::training(split)
            df_test <- rsample::testing(split)

            result <- do.call(model,
                            append(list(x = df_train[,-1], y = df_train[,1]),
                                   .hyper_grid))
            beta <- result %>%
              select(grid_id, beta, variable) %>%
              spread(grid_id, beta)

            x_test <- data.frame(`(Intercept)` = 1, data.matrix(df_test[, -1]), check.names = FALSE)
            x_test <- x_test[, beta$variable]
            beta <- beta[, -1]
            fit <- data.matrix(x_test) %*% data.matrix(beta)
            mse <- colMeans((df_test[, 1] - fit)^2)

            return(list(mse = mse, result = result))

          })

        mse <- result %>% map_dfr(function(res) res$mse)
        result <- result %>% map_dfr(function(res) res$result)
        best_grid <- colnames(mse)[which.min(colMeans(mse))]

      } else {

        best_grid <- NULL

      }

      if (!.return_slices) {
        result <- do.call(model,
                          append(list(x = m[,-1], y = m[,1]),
                                 .hyper_grid))
      }

      if (!is.null(best_grid)) {
        result <- result %>%
          filter(as.character(grid_id) == as.character(best_grid))
      }

      result <- result %>%
        mutate(model = nam) %>%
        select(-grid_id)

      return(result)

    })

  return(result)

}
