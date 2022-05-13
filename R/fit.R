

.fit <- function(.data, formula, model_list, .cv, .cv_args, .hyper_grid, .weights, gr_vars, .mask) {

  .data <- .data %>%
    select(-!!gr_vars, -!!.mask)

  m <- model.frame(formula = formula, data = .data)

  # Prepare CV
  if (.cv == "none")
    cv <- tibble(splits = list(m))
  if (.cv == "vfold")
    cv <- rsample::vfold_cv(m, v = .cv_args$v)
  if (.cv == "loo")
    cv <- rsample::loo_cv(m)

  # Evaluate methods
  result <-
    map2_dfr(model_list, names(model_list), function(model, nam) {

      if (.cv != "none") {

        result <- cv$splits %>%
          furrr::future_map_dfr(function(split) {

            df_train <- rsample::training(split)
            df_test <- rsample::testing(split)

            beta <- do.call(model,
                            append(list(x = df_train[,-1], y = df_train[,1]),
                                   .hyper_grid))
            beta <- beta %>%
              select(grid_id, beta, variable) %>%
              spread(grid_id, beta)

            x_test <- data.frame(`(Intercept)` = 1, data.matrix(df_test[, -1]), check.names = FALSE)
            x_test <- x_test[, beta$variable]
            beta <- beta[, -1]
            fit <- data.matrix(x_test) %*% data.matrix(beta)
            mse <- colMeans((df_test[, 1] - fit)^2)

          })

        best_grid <- colnames(result)[which.min(colMeans(result))]

      } else {

        best_grid <- NULL

      }

      result <- do.call(model,
                      append(list(x = m[,-1], y = m[,1]),
                             .hyper_grid))
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
