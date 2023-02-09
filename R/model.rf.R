#' @name .model.rf
#' @title Random Forest regression or classification for \code{tidyfit}
#' @description Fits a random forest on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details  **Hyperparameters:**
#'
#' - ntree (*number of trees*)
#' - mtry (*number of variables randomly sampled at each split*)
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{randomForest::randomForest}. See \code{?randomForest} for more details.
#'
#' **Implementation**
#'
#' The random forest is always fit with \code{importance = TRUE}. The feature importance values are extracted using \code{coef()}.
#'
#' @author Johann Pfitzinger
#'
#' @references
#' Liaw, A. and Wiener, M. (2002).
#' _Classification and Regression by randomForest._
#' R News 2(3), 18--22.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Date, -Industry)
#'
#' # Stand-alone function
#' fit <- m("rf", Return ~ ., data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("rf"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.model.svm}}, \code{\link{.model.boost}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.model.rf <- function(
    self,
    data = NULL
) {
  valid_args <- c("ntree", "mtry", "weights", "replace", "classwt", "cutoff",
                  "strata", "sampsize", "nodesize", "maxnodes", "importance",
                  "localImp", "nPerm", "proximity", "oob.prox", "norm.votes",
                  "do.trace", "keep.forest", "corr.bias", "keep.inbag")
  ctr <- self$args[names(self$args) %in% valid_args]
  ctr$importance <- TRUE
  var_names_map <- .names_map(colnames(data))
  data <- data.frame(data)
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(randomForest::randomForest, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(names_map = var_names_map)
  self$estimator <- "randomForest::randomForest"
  invisible(self)
}

.coef.randomForest <- function(object, self = NULL, ...) {
  if (self$mode == "regression") {
    imp <- object$importance
    estimates <- dplyr::as_tibble(imp) %>%
      dplyr::mutate(term = rownames(imp), estimate = NA) %>%
      dplyr::mutate(importanceSD = object$importanceSD[.data$term]) %>%
      dplyr::mutate(term = self$fit_info$names_map[.data$term])
  } else {
    imp <- object$importance
    imp_MDacc <- imp[, -(ncol(imp)-1):-ncol(imp)]
    imp_Other <- imp[, (ncol(imp)-1):ncol(imp)]
    estimates <- dplyr::as_tibble(imp_MDacc) %>%
      dplyr::mutate(term = rownames(imp)) %>%
      tidyr::pivot_longer(-.data$term, names_to = "class", values_to = "Class_MeanDecreaseAccuracy")
    estimates_other <- dplyr::as_tibble(imp_Other) %>%
      dplyr::mutate(term = rownames(imp))
    estimates <- dplyr::left_join(estimates, estimates_other, by = "term")
    impSD <- object$importanceSD
    impSD_MDacc <- impSD[, -ncol(impSD)]
    impSD_Other <- impSD[, ncol(impSD)]
    estimatesSD <- dplyr::as_tibble(impSD_MDacc) %>%
      dplyr::mutate(term = rownames(impSD)) %>%
      tidyr::pivot_longer(-.data$term, names_to = "class", values_to = "Class_MeanDecreaseAccuracySD")
    estimatesSD_other <- dplyr::tibble(MeanDecreaseAccuracySD = impSD_Other) %>%
      dplyr::mutate(term = rownames(impSD))
    estimatesSD <- dplyr::left_join(estimatesSD, estimatesSD_other, by = "term")
    estimates <- estimates %>%
      dplyr::left_join(estimatesSD, by = c("term", "class")) %>%
      dplyr::mutate(term = self$fit_info$names_map[.data$term], estimate = NA)
  }

  return(estimates)
}

.predict.randomForest <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  if (self$mode == "regression") {
    pred_mat <- stats::predict(object, newdata = data.frame(data))
  } else {
    pred_mat <- stats::predict(object, newdata = data.frame(data), type = "prob")
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
        dplyr::select(-dplyr::any_of("row_n")) %>%
        dplyr::mutate(prediction = as.numeric(.data$prediction))

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

.fitted.randomForest <- function(object, self = NULL, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(predict(object))
  )
  return(fitted)
}

.resid.randomForest <- function(object, self = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = object$y - drop(predict(object))
  )
  return(residuals)
}
