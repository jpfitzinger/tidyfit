#' @name .fit.quantile_rf
#' @title Quantile regression forest for \code{tidyfit}
#' @description Fits a nonlinear quantile regression forest on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#'  - \code{tau} (the quantile(s) to be estimated)
#'
#' The function provides a wrapper for \code{quantregForest::quantregForest}. See \code{?quantregForest} for more details.
#' The argument \code{tau} is the chosen quantile (default \code{tau = 0.5}).
#' \code{tau} is passed directly to \code{m('quantile_rf', tau = c(0.1, 0.5, 0.9)} and is not passed to \code{predict} as in the \code{quantregForest::quantregForest} package. This is done to ensure a consistent interface with the quantile regression from \code{quantreg}.
#'
#' **Implementation**
#'
#' *No implementation notes*
#'
#' @author Johann Pfitzinger
#' @references
#' Meinshausen N (2017). _quantregForest: Quantile Regression Forests_. R package version 1.3-7, <https://CRAN.R-project.org/package=quantregForest>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Date, -Industry)
#'
#' # Stand-alone function
#' fit <- m("quantile_rf", Return ~ ., data, tau = 0.5, ntree = 50)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ .,
#'                m("quantile_rf", tau = c(0.1, 0.5, 0.9), ntree = 50))
#' explain(fit)
#'
#' @seealso \code{\link{.fit.quantile}}, \code{\link{.fit.rf}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.quantile_rf <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  self$set_args(tau = 0.5, overwrite = FALSE)
  valid_args <- c("ntree", "mtry", "weights", "replace", "classwt", "cutoff",
                  "strata", "sampsize", "nodesize", "maxnodes", "importance",
                  "localImp", "nPerm", "proximity", "oob.prox", "norm.votes",
                  "do.trace", "keep.forest", "corr.bias", "keep.inbag", "nthreads")
  ctr <- self$args[names(self$args) %in% valid_args]
  ctr$importance <- TRUE
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(quantregForest::quantregForest, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x, y = y), ctr))
  .store_on_self(self, res)
  self$estimator <- "quantregForest::quantregForest"
  invisible(self)
}

.predict.quantregForest <- function(object, data, self = NULL, training_context = FALSE, ...) {
  augmented_data <- dplyr::bind_rows(data, self$data)
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 1
    truth <- NULL
  }
  if (self$mode == "regression") {
    if (!training_context) {
      tau <- self$args$tau
    } else {
      tau <- 0.5
    }
    mf <- stats::model.frame(self$formula, augmented_data)
    x <- stats::model.matrix(self$formula, mf)
    x <- x[1:nrow(data),]
    pred_mat <- predict(object, newdata = x, what = tau)
    if (length(tau) == 1) {
      pred <- dplyr::tibble(prediction = pred_mat, truth = truth, tau = tau)
    } else {
      pred_mat <- data.frame(pred_mat)
      colnames(pred_mat) <- sort(tau)
      pred <- dplyr::tibble(pred_mat) %>%
        dplyr::mutate(truth = truth) %>%
        tidyr::gather("tau", "prediction", -any_of(c("truth")))
    }
    pred <- dplyr::mutate(pred, tau = as.numeric(tau))
  } else {

  }

  return(pred)
}

.fitted.quantregForest <- function(object, self = NULL, ...) {
  .predict.quantregForest(object, data = self$data, self = self, ...) %>%
    dplyr::rename(fitted = "prediction") %>%
    dplyr::select(-any_of(c("truth")))
}

.resid.quantregForest <- function(object, self = NULL, ...) {
  .predict.quantregForest(object, data = self$data, self = self, ...) %>%
    dplyr::mutate(residual = .data$truth - .data$prediction) %>%
    dplyr::select(-any_of(c("truth", "prediction")))
}
