.predict.glmnet <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  x <- stats::model.matrix(self$formula, data)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  pred_mat <- stats::predict(object, x, type = "response", s = self$args$lambda)

  dimnames(pred_mat)[[length(dim(pred_mat))]] <- self$inner_grid$grid_id[appr_in(self$inner_grid$lambda, self$args$lambda)]
  if (length(dim(pred_mat))==3) {
    class_vals <- dimnames(pred_mat)[[2]]
    dimnames(pred_mat)[[2]]  <- class_vals <- self$fit_info$class_names_map[class_vals]
  } else {
    class_vals <- NULL
  }
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(row_n = dplyr::row_number())
  if (!is.null(truth)) {
    pred <- dplyr::mutate(pred, truth = truth)
  }
  if (self$args$family == "multinomial") {
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
    dplyr::select(-"row_n")
  if (length(class_vals)==2) {
    pred <- pred %>%
      dplyr::filter(.data$class == sort(class_vals)[2]) %>%
      dplyr::select(-"class")
  }
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.lm <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 0
    truth <- NULL
  }
  if (!is.null(self$fit_info$names_map)) data <- data.frame(stats::model.matrix(self$formula, data))
  pred <- dplyr::tibble(
    prediction = stats::predict(object, data, type = "response"),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.glm <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- NA
    truth <- NULL
  }
  if (!is.null(self$fit_info$names_map)) data <- data.frame(stats::model.matrix(self$formula, data))
  pred <- dplyr::tibble(
    prediction = stats::predict(object, data, type = "response"),
    truth = truth
  )
  return(pred)
}

#' @importFrom stats model.frame model.matrix model.response
#' @importFrom dplyr tibble
.predict.rlm <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, data)
  y <- stats::model.response(mf)
  pred <- dplyr::tibble(
    prediction = drop(crossprod(t(x), object$coefficients)),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.mvr <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred_mat <- sapply(self$args$ncomp, function(nc) drop(stats::predict(object, data, ncomp = nc)))
  colnames(pred_mat) <- self$inner_grid$grid_id[appr_in(self$inner_grid$ncomp, self$args$ncomp)]
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(truth = truth) %>%
    tidyr::pivot_longer(-any_of("truth"), names_to = "grid_id", values_to = "prediction")
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.rq <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
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
.predict.glmboost <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  xs <- as.matrix(scale(x, center = self$fit_info$standard_mean, scale = self$fit_info$standard_sd))
  xs <- data.frame(`(Intercept)` = 1, xs, check.names = FALSE)
  pred <- dplyr::tibble(
    prediction = drop(stats::predict(object, data.matrix(xs), type = "response")),
    truth = truth
  )
  return(pred)
}

#' @importFrom dplyr tibble
#' @importFrom stats predict
.predict.merMod <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
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

.predict.shrinkTVP <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
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

.predict.MSM.lm <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
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

.predict.bma <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 0
    truth <- NULL
  }
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  pred <- dplyr::tibble(
    prediction = stats::predict(object, x),
    truth = truth
  )
  return(pred)
}

.predict.svm <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  x <- stats::model.frame(self$formula, data)
  pred_mat <- stats::predict(object, newdata = x, probability = TRUE)

  if (is.factor(pred_mat)) {
    pred_mat <- attr(pred_mat, "probabilities")
    if (ncol(pred_mat) > 2) {
      pred <- pred_mat %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(row_n = dplyr::row_number())
      if (!is.null(truth)) {
        pred <- dplyr::mutate(pred, truth = truth)
      }
      pred <- pred %>%
        tidyr::pivot_longer(-dplyr::any_of(c("truth", "row_n")),
                            names_to = "class",
                            values_to = "prediction") %>%
        dplyr::select(-dplyr::any_of("row_n"))

      return(pred)
    } else {
      pred_mat <- pred_mat[, 2]
    }
  }

  pred <- dplyr::tibble(
    prediction = pred_mat,
    truth = truth
  )

  return(pred)
}
