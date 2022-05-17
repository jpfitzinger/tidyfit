

.fit <- function(.data, formula, model_list, .cv, .cv_args, .control, .weights, gr_vars, .mask) {

  .data <- .data %>%
    select(-!!gr_vars, -!!.mask)

  m <- model.frame(formula = formula, data = .data)

  family <- .control$family
  if (!is.null(family)) {
    if (!family %in% c("binomial", "gaussian"))
      stop("'family' must be binomial or gaussian.")
    if (family == "binomial") f <- binomial()
    if (family == "gaussian") f <- gaussian()
    # Needs to be integrated
    if (family == "poisson") f <- poisson()
  }

  # Prepare CV
  if (.cv == "none")
    cv <- tibble(splits = list(m))
  if (.cv == "vfold")
    cv <- do.call(rsample::vfold_cv, append(list(data = m), .cv_args))
  if (.cv == "loo")
    cv <- do.call(rsample::loo_cv, append(list(data = m), .cv_args))
  if (.cv == "ts")
    cv <- do.call(rsample::rolling_origin, append(list(data = m), .cv_args))

  # Evaluate methods
  result <-
    map2_dfr(model_list, names(model_list), function(model, nam) {

      if (.cv != "none") {

        result <- cv %>%
          furrr::future_pmap_dfr(function(splits, id) {

            df_train <- rsample::training(splits)
            df_test <- rsample::testing(splits)

            result <- do.call(model,
                            append(list(x = df_train[,-1], y = df_train[,1]),
                                   .control))
            beta <- result %>%
              select(grid_id, beta, variable) %>%
              spread(grid_id, beta)

            x_test <- data.frame(`(Intercept)` = 1, data.matrix(df_test[, -1]), check.names = FALSE)
            x_test <- x_test[, beta$variable]
            beta <- beta[, -1]
            fit <- data.matrix(x_test) %*% data.matrix(beta)

            if (is.null(family)) {
              # Calculate MSE
              crit <- colMeans((df_test[, 1] - fit)^2)
            } else {
              if (family == "binomial") {
                fit <- f$linkinv(fit)
                # Calculate accuracy
                fit <- (fit > 0.5) * 1
                crit <- apply(fit, 2, function(x) mean(x == df_test[,1]))
              } else if (family == "gaussian") {
                crit <- colMeans((df_test[, 1] - fit)^2)
              }
            }

            crit <- tibble(grid_id = names(crit), crit = crit)

            result <- result %>%
              mutate(slice_id = id) %>%
              left_join(crit, by = "grid_id")

            return(result)

          })

      }

      result_all <- do.call(model,
                            append(list(x = m[,-1], y = m[,1]),
                                   .control)) %>%
        mutate(slice_id = "FULL")

      result <- bind_rows(result, result_all)

      result <- result %>%
        mutate(model = nam)

      return(result)

    })

  return(result)

}
