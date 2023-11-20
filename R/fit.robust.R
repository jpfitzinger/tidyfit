#' @name .fit.robust
#' @title Robust regression for \code{tidyfit}
#' @description Fits a robust linear regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#'  - \code{method} (estimation algorithm, e.g. 'M', 'MM')
#'
#' The function provides a wrapper for \code{MASS::rlm}. See \code{?rlm} for more details.
#'
#' **Implementation**``
#'
#' An argument \code{vcov.} can be passed in control or to \code{...} in \code{\link{m}} to estimate the model with robust standard errors. \code{vcov.} can be one of "BS", "HAC", "HC" and "OPG" and is passed to the \code{sandwich} package.
#'
#' @author Johann Pfitzinger
#' @references
#' W. N. Venables and B. D. Ripley (2002).
#' \emph{Modern Applied Statistics with S. 4th ed., Springer, New York.}
#' URL https://www.stats.ox.ac.uk/pub/MASS4/.\cr
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' fit <- regress(data, Return ~ ., m("robust"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' # With robust standard errors
#' fit <- m("robust", Return ~ `Mkt-RF` + HML + SMB, data, vcov. = "HAC")
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.fit.lm}} and \code{\link{m}} methods
#'
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom MASS rlm

.fit.robust <- function(
    self,
    data = NULL
) {

  # Available args are not accessible using formalArgs without importing MASS:::rlm.formula
  rlm_args <- c("x", "y", "weights", "w", "init", "psi", "scale.est", "k2", "method",
                "wt.method", "maxit", "acc", "test.vec", "lqs.control")
  ctr <- self$args[names(self$args) %in% rlm_args]
  if (is.null(ctr$weights)) ctr <- ctr[names(ctr)!="weights"]
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, data)
  y <- stats::model.response(mf)
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(MASS::rlm, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x, y = y), ctr))
  .store_on_self(self, res)
  self$estimator <- "MASS::rlm"
  invisible(self)

}
