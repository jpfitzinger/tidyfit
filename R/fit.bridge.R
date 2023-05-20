#' @name .fit.bridge
#' @title Bayesian ridge regression for \code{tidyfit}
#' @description Fits a Bayesian ridge regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{monomvn::bridge}. See \code{?bridge} for more details.
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
#' fit <- m("bridge", Return ~ ., data, T = 100)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("bridge", T = 100),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.ridge}}, \code{\link{.fit.blasso}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.bridge <- function(
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
