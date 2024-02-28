#' @name .fit.rf
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
#' explain(fit)
#'
#' @seealso \code{\link{.fit.svm}}, \code{\link{.fit.boost}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.rf <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  valid_args <- c("ntree", "mtry", "weights", "replace", "classwt", "cutoff",
                  "strata", "sampsize", "nodesize", "maxnodes", "importance",
                  "localImp", "nPerm", "proximity", "oob.prox", "norm.votes",
                  "do.trace", "keep.forest", "corr.bias", "keep.inbag")
  ctr <- self$args[names(self$args) %in% valid_args]
  ctr$importance <- TRUE
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(randomForest::randomForest, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x, y = y), ctr))
  .store_on_self(self, res)
  self$estimator <- "randomForest::randomForest"
  invisible(self)
}

.predict.randomForest <- function(object, data, self = NULL, ...) {
  augmented_data <- dplyr::bind_rows(data, .prepare_data(self, self$data))
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  mf <- stats::model.frame(self$formula, augmented_data)
  x <- stats::model.matrix(self$formula, mf)
  x <- x[1:nrow(data),]
  if (self$mode == "regression") {
    pred_mat <- stats::predict(object, newdata = x)
  } else {
    pred_mat <- stats::predict(object, newdata = x, type = "prob")
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
