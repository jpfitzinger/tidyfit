.predict.glmnet <- function(object, data, formula = NULL, lambda = NULL, inner_grid = NULL, family = NULL, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  x <- stats::model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  pred_mat <- stats::predict(object, x, type = "response", s = lambda)

  dimnames(pred_mat)[[length(dim(pred_mat))]] <- inner_grid$grid_id[appr_in(inner_grid$lambda, lambda)]
  if (length(dim(pred_mat))==3) {
    class_vals <- dimnames(pred_mat)[[2]]
  } else {
    class_vals <- NULL
  }
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(row_n = dplyr::row_number())
  if (!is.null(truth)) {
    pred <- dplyr::mutate(pred, truth = truth)
  }
  if (family == "multinomial") {
    pred <- pred %>%
      tidyr::pivot_longer(-dplyr::any_of(c("truth", "row_n")),
                          names_to = c("class", "grid_id"),
                          values_to = "prediction",
                          names_sep = "\\.")
  } else {
    pred <- pred %>%
      tidyr::gather("grid_id", "prediction", -dplyr::any_of(c("truth", "row_n")))
  }
  pred <- pred %>%
    dplyr::select(-.data$row_n)
  if (length(class_vals)==2) {
    pred <- pred %>%
      dplyr::filter(.data$class == sort(class_vals)[2]) %>%
      dplyr::select(-.data$class)
  }
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.lm <- function(object, data, formula, names_map = NULL, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  if (!is.null(names_map)) data <- data.frame(stats::model.matrix(formula, data))
  pred <- dplyr::tibble(
    prediction = stats::predict(object, data, type = "response"),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.glm <- function(object, data, formula, names_map = NULL, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  if (!is.null(names_map)) data <- data.frame(stats::model.matrix(formula, data))
  pred <- dplyr::tibble(
    prediction = stats::predict(object, data, type = "response"),
    truth = truth
  )
  return(pred)
}

#' @importFrom stats model.frame model.matrix model.response
#' @importFrom dplyr tibble
.predict.rlm <- function(object, data, formula, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, data)
  y <- stats::model.response(mf)
  pred <- dplyr::tibble(
    prediction = drop(crossprod(t(x), object$coefficients)),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.mvr <- function(object, data, formula, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred <- dplyr::tibble(
    prediction = drop(stats::predict(object, data, ncomp = object$ncomp)),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.rq <- function(object, data, formula, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred <- dplyr::tibble(
    prediction = stats::predict(object, data),
    truth = truth
  )
  return(pred)
}

#' @importFrom stats model.frame model.matrix model.response
.predict.glmboost <- function(object, data, formula, standard_mean = NULL, standard_sd = NULL, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))
  xs <- data.frame(`(Intercept)` = 1, xs, check.names = FALSE)
  pred <- dplyr::tibble(
    prediction = drop(stats::predict(object, data.matrix(xs), type = "response")),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.merMod <- function(object, data, formula, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred <- dplyr::tibble(
    prediction = stats::predict(object, data),
    truth = truth
  )
  return(pred)
}

.predict.shrinkTVP <- function(object, data, formula, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred <- stats::predict(object)
  pred <- dplyr::tibble(
    prediction = colMeans(pred),
    truth = truth
  )
  return(pred)
}

.predict.MSM.lm <- function(object, data, formula, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred <- object@Fit@CondMean * object@Fit@smoProb[-1,]
  pred <- dplyr::tibble(
    prediction = rowSums(pred),
    truth = truth
  )
  return(pred)
}

.predict.cv.hfr <- function(object, data, formula, inner_grid = NULL, kappa_grid = NULL, ...) {
  response_var <- all.vars(formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  pred_mat <- sapply(kappa_grid, function(kap) stats::predict(object, x, kappa = kap))
  colnames(pred_mat) <- inner_grid$grid_id[appr_in(inner_grid$kappa_grid, kappa_grid)]
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(truth = truth) %>%
    tidyr::pivot_longer(-any_of("truth"), names_to = "grid_id", values_to = "prediction")
  return(pred)
}
