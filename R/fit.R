#' @importFrom purrr map2_dfr
#' @importFrom stats model.frame model.matrix model.response binomial gaussian poisson
#' @importFrom rsample vfold_cv loo_cv rolling_origin
#' @importFrom furrr future_pmap_dfr furrr_options
#' @importFrom dplyr select tibble mutate left_join bind_rows
#' @importFrom tidyr spread
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
    family
    ) {

  .data <- .data %>%
    dplyr::select(-!!gr_vars, -!!.mask)

  if (!is.null(.weights)) {
    wts <- .data %>%
      dplyr::pull(!!.weights)
    .data <- .data %>%
      dplyr::select(-!!.weights)
  } else {
    wts = NULL
  }

  m <- stats::model.frame(formula = formula, data = .data)
  x <- stats::model.matrix(formula, m)[, -1]
  y <- stats::model.response(m)

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
  result <-
    purrr::map2_dfr(model_list, names(model_list), function(model, nam) {

      do_cv <- .check_method(model(.return_method_name = TRUE), "cv")
      if (.cv != "none" & do_cv) {

        result <- cv %>%
          furrr::future_pmap_dfr(function(splits, id) {

            df_train <- rsample::training(splits)
            df_test <- rsample::testing(splits)

            df_train_x <- stats::model.matrix(formula, df_train)[, -1]
            df_train_y <- stats::model.response(df_train)
            df_test_x <- stats::model.matrix(formula, df_test)
            df_test_y <- stats::model.response(df_test)

            model_args <- list(x = df_train_x, y = df_train_y)
            if (!model(.check_family = TRUE)) {
              model_args <- append(model_args, list(family = family))
            }
            if (!is.null(wts)) model_args <- append(model_args, list(weights = wts[splits$in_id]))

            result <- do.call(model, model_args)

            beta <- result %>%
              dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
              tidyr::spread(.data$grid_id, .data$beta)

            df_test_x <- df_test_x[, beta$variable]
            beta <- beta[, -1]
            fit <- df_test_x %*% data.matrix(beta)

            f <- result %>%
              dplyr::pull(family) %>%
              unique %>%
              .[[1]]
            fit <- f$linkinv(fit)

            if (f$family == "binomial") {
              # Calculate cross-entropy loss
              crit <- apply(fit, 2, function(x) -mean(df_test_y * log(x) + (1 - df_test_y) * log(1 - x)))
            } else if (f$family == "gaussian") {
              crit <- colMeans((df_test_y - fit)^2)
            }

            crit <- dplyr::tibble(grid_id = names(crit), crit = crit)

            result <- result %>%
              dplyr::mutate(slice_id = id) %>%
              dplyr::left_join(crit, by = "grid_id")

            return(result)

          },
          .options = furrr::furrr_options(seed = TRUE))

      } else {
        result <- NULL
      }

      model_args <- list(x = x, y = y)
      if (!model(.check_family = TRUE)) {
        model_args <- append(model_args, list(family = family))
      }
      if (!is.null(wts)) model_args <- append(model_args, list(weights = wts))

      result_all <- do.call(model, model_args) %>%
        mutate(slice_id = "FULL")

      result <- dplyr::bind_rows(result, result_all)

      result <- result %>%
        dplyr::mutate(model = nam)

      return(result)

    })

  return(result)

}
