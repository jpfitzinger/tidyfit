#' @name .model.quantile
#' @title Quantile regression for \code{tidyfit}
#' @description Fits a linear quantile regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{quantreg::rq}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{quantreg::rq}. The argument \code{tau} is the chosen quantile (default \code{tau = 0.5}).
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("quantile", x, y, tau = c(0.1, 0.5, 0.9))
#' fit
#'
#' @seealso \code{\link{.model.lm}}, \code{\link{.model.bayes}} and \code{\link{m}} methods
#'
#' @importFrom stats coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom quantreg rq
#' @importFrom methods formalArgs

.model.quantile <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
) {

  f <- control$family
  control <- control[names(control) %in% methods::formalArgs(quantreg::rq)]

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(quantreg::rq, append(list(formula = y~., data = dat), control))
  coefs <- stats::coef(m)
  coef_stats <- summary(m)$coefficients

  out <- dplyr::tibble(
    variable = c("(Intercept)", colnames(dat)[-1]),
    beta = coefs,
    family = list(f),
    `lower bd` = coef_stats[, 2],
    `upper bd` = coef_stats[, 3]
  )
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, as_tibble(func_to_list(control)))
  }

  return(out)

}
