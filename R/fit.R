

.fit <- function(.data, formula, model_list, .cv, .cv_args, .hyper_grid, .weights) {

  ff = as.formula("Growth ~ .")
  m <- model.frame(formula = ff, data = .data)

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

      result <- cv$splits %>%
        furrr::future_map_dfr(function(split) {

          if ("rsplit" %in% class(split)) {
            df_train <- rsample::training(split)
            df_test <- rsample::testing(split)
          } else {
            df_train <- split
            df_test <- NULL
          }

          beta <- model(df_train[, -1], df_train[, 1], .hyper_grid)
          if (!is.null(df_test)) {
            df_test <- cbind(1, data.matrix(df_test))
            mse <- mean((df_test[, 1] - df_test[, -1] %*% beta)^2)
          } else {
            mse <- 0
          }

          out <- tibble(
            variable = names(beta),
            beta = beta,
            mse = mse
          )

        })

      result <- result %>%
        filter(mse == min(mse, na.rm = TRUE)) %>%
        select(-mse) %>%
        mutate(model = nam)

      return(result)

    })

  return(result)

}
