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
#' # Stand-alone function
#' fit <- m("quantile", Return ~ ., data, tau = 0.5)
#' fit
#'
#' # Within 'regress' function
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
  ctr <- self$args[names(self$args) %in% methods::formalArgs(quantreg::rq)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(quantreg::rq, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "quantile::rq"
  invisible(self)
}
