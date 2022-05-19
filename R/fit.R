#' @importFrom purrr map2_dfr
#' @importFrom stats model.frame model.matrix model.response binomial gaussian poisson
#' @importFrom rsample vfold_cv loo_cv rolling_origin
#' @importFrom furrr future_pmap_dfr
#' @importFrom dplyr select tibble mutate left_join bind_rows
#' @importFrom tidyr spread
#' @importFrom rlang .data

.fit <- function(.data, formula, model_list, .cv, .cv_args, .control, .weights, gr_vars, .mask) {

  .data <- .data %>%
    dplyr::select(-!!gr_vars, -!!.mask)

  m <- stats::model.frame(formula = formula, data = .data)
  x <- stats::model.matrix(formula, m)[, -1]
  y <- stats::model.response(m)

  family <- .control$family
  if (!is.null(family)) {
    if (!family %in% c("binomial", "gaussian"))
      stop("'family' must be binomial or gaussian.")
    if (family == "binomial") f <- stats::binomial()
    if (family == "gaussian") f <- stats::gaussian()
    # Needs to be integrated
    if (family == "poisson") f <- stats::poisson()
  }

  # Prepare CV
  if (.cv == "none")
    cv <- dplyr::tibble(splits = list(m))
  if (.cv == "vfold")
    cv <- do.call(rsample::vfold_cv, append(list(data = m), .cv_args))
  if (.cv == "loo")
    cv <- do.call(rsample::loo_cv, append(list(data = m), .cv_args))
  if (.cv == "ts")
    cv <- do.call(rsample::rolling_origin, append(list(data = m), .cv_args))

  # Evaluate methods
  result <-
    purrr::map2_dfr(model_list, names(model_list), function(model, nam) {

      if (.cv != "none") {

        result <- cv %>%
          furrr::future_pmap_dfr(function(splits, id) {

            df_train <- rsample::training(splits)
            df_test <- rsample::testing(splits)

            df_train_x <- stats::model.matrix(formula, df_train)[, -1]
            df_train_y <- stats::model.response(df_train)
            df_test_x <- stats::model.matrix(formula, df_test)
            df_test_y <- stats::model.response(df_test)

            result <- do.call(model,
                            append(list(x = df_train_x, y = df_train_y),
                                   .control))
            beta <- result %>%
              dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
              tidyr::spread(.data$grid_id, .data$beta)

            df_test_x <- df_test_x[, beta$variable]
            beta <- beta[, -1]
            fit <- df_test_x %*% data.matrix(beta)

            if (is.null(family)) {
              # Calculate MSE
              crit <- colMeans((df_test_y - fit)^2)
            } else {
              if (family == "binomial") {
                fit <- f$linkinv(fit)
                # Calculate accuracy
                fit <- (fit > 0.5) * 1
                crit <- apply(fit, 2, function(x) mean(x == df_test_y))
              } else if (family == "gaussian") {
                crit <- colMeans((df_test_y - fit)^2)
              }
            }

            crit <- dplyr::tibble(grid_id = names(crit), crit = crit)

            result <- result %>%
              dplyr::mutate(slice_id = id) %>%
              dplyr::left_join(crit, by = "grid_id")

            return(result)

          })

      } else {
        result <- NULL
      }

      result_all <- do.call(model,
                            append(list(x = x, y = y),
                                   .control)) %>%
        mutate(slice_id = "FULL")

      result <- dplyr::bind_rows(result, result_all)

      result <- result %>%
        dplyr::mutate(model = nam)

      return(result)

    })

  return(result)

}
