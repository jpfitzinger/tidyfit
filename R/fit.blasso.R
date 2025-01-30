#' @name .fit.blasso
#' @title Bayesian Lasso regression for \code{tidyfit}
#' @description Fits a Bayesian Lasso regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{monomvn::blasso}. See \code{?blasso} for more details.
#'
#' **Implementation**
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' @param self a tidyFit R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted tidyFit class model.
#' @author Johann Pfitzinger
#' @references
#' Gramacy RB, (qpgen2/quadprog) wFcfCMaubBAT (2023). _monomvn: Estimation for MVN and Student-t Data with Monotone Missingness_. R package version 1.9-17, <https://CRAN.R-project.org/package=monomvn>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("blasso", Return ~ ., data, T = 100)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("blasso", T = 100),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lasso}}, \code{\link{.fit.bridge}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.blasso <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("blasso cannot handle weights, weights are ignored", call. = FALSE)
  }

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  ctr <- self$args[names(self$args) %in% methods::formalArgs(monomvn::blasso)]
  ctr$verb <- 0

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(monomvn::blasso, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(X = x, y = y, icept = incl_intercept), ctr))
  .store_on_self(self, res)
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
  response_var <- self$get_syntactic_response_var_name()
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
  response_var <- self$get_syntactic_response_var_name()
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
  response_var <- self$get_syntactic_response_var_name()
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
