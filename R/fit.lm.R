#' @name .fit.lm
#' @title Linear regression for \code{tidyfit}
#' @description Fits a linear regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#' The function provides a wrapper for \code{stats::lm}. See \code{?lm} for more details.
#'
#' **Implementation**
#'
#' An argument \code{vcov.} can be passed in control or to \code{...} in \code{\link{m}} to estimate the model with robust standard errors. \code{vcov.} can be one of "BS", "HAC", "HC" and "OPG" and is passed to the \code{sandwich} package.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("lm", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' # With robust standard errors
#' fit <- m("lm", Return ~ `Mkt-RF` + HML + SMB, data, vcov. = "HAC")
#' fit
#'
#' @seealso \code{\link{.fit.robust}}, \code{\link{.fit.glm}} and \code{\link{m}} methods
#'
#' @importFrom stats lm
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.lm <- function(
    self,
    data = NULL
) {
  ctr <- self$args[names(self$args) %in% methods::formalArgs(stats::lm)]
  ctr$model <- TRUE
  ctr$x <- FALSE
  ctr$y <- FALSE
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(stats::lm, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "stats::lm"
  invisible(self)
}
