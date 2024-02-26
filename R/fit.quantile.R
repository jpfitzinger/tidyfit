#' @name .fit.quantile
#' @title Quantile regression for \code{tidyfit}
#' @description Fits a linear quantile regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#'  - \code{tau} (the quantile(s) to be estimated)
#'
#' The function provides a wrapper for \code{quantreg::rq}. See \code{?rq} for more details. The argument \code{tau} is the chosen quantile (default \code{tau = 0.5}).
#'
#' **Implementation**
#'
#' *No implementation notes*
#'
#' @author Johann Pfitzinger
#' @references
#' Koenker R (2022). _quantreg: Quantile Regression_. R package version 5.94, <https://CRAN.R-project.org/package=quantreg>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' fit <- regress(data, Return ~ .,
#'                m("quantile", tau = c(0.1, 0.5, 0.9)),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lm}}, \code{\link{.fit.bayes}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.quantile <- function(
    self,
    data = NULL
) {
  self$set_args(tau = 0.5, overwrite = FALSE)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(quantreg::rq)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(quantreg::rq, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "quantreg::rq"
  invisible(self)
}

.coef.rq <- function(object, ...) {
  estimates <- broom::tidy(object)
  return(estimates)
}

.coef.rqs <- function(object, ...) {
  estimates <- broom::tidy(object)
  return(estimates)
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
    truth = truth,
    tau = self$args$tau
  )
  return(pred)
}

.predict.rqs <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }
  pred_mat <- stats::predict(object, data)
  tau <- self$args$tau
  if (length(tau) == 1) {
    pred <- dplyr::tibble(prediction = pred_mat, truth = truth, tau = tau)
  } else {
    pred_mat <- data.frame(pred_mat)
    colnames(pred_mat) <- sort(tau)
    pred <- dplyr::tibble(pred_mat) %>%
      dplyr::mutate(truth = truth) %>%
      tidyr::gather("tau", "prediction", -.data$truth) %>%
      dplyr::select(.data$prediction, .data$truth, .data$tau)
  }
  return(pred)
}

.fitted.rq <- function(object, self = NULL, ...) {
  .predict.rq(object, data = data.frame(self$data), self = self, ...) %>%
    dplyr::rename(fitted = "prediction") %>%
    dplyr::select(-any_of(c("truth")))
}

.fitted.rqs <- function(object, self = NULL, ...) {
  .predict.rqs(object, data = data.frame(self$data), self = self, ...) %>%
    dplyr::rename(fitted = "prediction") %>%
    dplyr::select(-any_of(c("truth")))
}

.resid.rq <- function(object, self = NULL, ...) {
  .predict.rq(object, data = data.frame(self$data), self = self, ...) %>%
    dplyr::mutate(residual = .data$truth - .data$prediction) %>%
    dplyr::select(-any_of(c("truth", "prediction")))
}

.resid.rqs <- function(object, self = NULL, ...) {
  .predict.rqs(object, data = data.frame(self$data), self = self, ...) %>%
    dplyr::mutate(residual = .data$truth - .data$prediction) %>%
    dplyr::select(-any_of(c("truth", "prediction")))
}
