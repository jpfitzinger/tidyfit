#' @name .model.bridge
#' @title Bayesian ridge regression for \code{tidyfit}
#' @description Fits a Bayesian ridge regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L2 penalty)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The ridge regression is estimated using \code{glmnet::glmnet} with \code{alpha = 0}. See \code{?glmnet} for more details. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
#'
#' **Implementation**
#'
#' If the response variable contains more than 2 classes, a multinomial response is used automatically.
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param self a tidyFit R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted tidyFit class model.
#' @author Johann Pfitzinger
#' @references
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("ridge", Return ~ ., data, lambda = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("bridge", lambda = c(0.1, 0.5)),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lasso}}, \code{\link{.model.adalasso}}, \code{\link{.model.enet}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.model.bridge <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  ctr <- self$args[names(self$args) %in% methods::formalArgs(monomvn::bridge)]
  ctr$verb <- 0

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(monomvn::bridge, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(X = x, y = y, icept = incl_intercept), ctr))
  .store_on_self(self, res)
  self$estimator <- "monomvn::bridge"
  self$fit_info <- list(var_names = colnames(x))
  invisible(self)

}
.coef.blasso <- function(object, self = NULL, ...) {
  burnin <- c(1:round(T/2))
  beta_int <- apply(object$beta[-burnin,], 2, quantile, c(0.05, 0.95))
  beta_mean <- apply(object$beta[-burnin,], 2, mean)
  mu_int <- quantile(object$mu[-burnin], c(0.05, 0.95))
  mu_mean <- mean(object$mu[-burnin])
  estimates <- tidyr::tibble(
    term = c("(Intercept)", self$fit_info$var_names),
    estimate = c(mu_mean, beta_mean),
    upper = c(mu_int[2], beta_int[2,]),
    lower = c(mu_int[1], beta_int[1,])
  )
  return(estimates)
}
.predict.blasso <- function(object, data, self, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 0
    truth <- NULL
  }
  burnin <- c(1:round(T/2))
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  prediction <- x %*% t(cbind(object$mu[-burnin], object$beta[-burnin,]))
  pred <- dplyr::tibble(
    prediction = rowMeans(prediction),
    truth = truth
  )
  return(pred)
}
.fitted.blasso <- function(object, self, ...) {
  burnin <- c(1:round(T/2))
  response_var <- all.vars(self$formula)[1]
  data <- as.data.frame(object$X)
  data[, response_var] <- object$y
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  prediction <- x %*% t(cbind(object$mu[-burnin], object$beta[-burnin,]))
  fit <- dplyr::tibble(
    fitted = rowMeans(prediction)
  )
  return(fit)
}
.resid.blasso <- function(object, self, ...) {
  burnin <- c(1:round(T/2))
  response_var <- all.vars(self$formula)[1]
  data <- as.data.frame(object$X)
  data[, response_var] <- object$y
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  prediction <- x %*% t(cbind(object$mu[-burnin], object$beta[-burnin,]))
  residual <- dplyr::tibble(
    residual = y - rowMeans(prediction)
  )
  return(residual)
}
