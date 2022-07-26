#' @name .model.bayes
#' @title Bayesian generalized linear regression for \code{tidyfit}
#' @description Fits a Bayesian regression and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{stats::glm}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{bayesglm}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rbinom(100, 1, 0.5)
#' fit <- m("bayes", x, y, family = binomial(link = probit))
#' fit
#'
#'
#' @seealso \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom stats coef
#' @importFrom dplyr tibble
#' @importFrom arm bayesglm

.model.bayes <- function(x = NULL, y = NULL, control = NULL, ...) {

  control <- control[names(control) %in% names(formals(arm::bayesglm))]

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(arm::bayesglm, append(list(formula = y~., data = dat), control))
  coefs <- stats::coef(m)

  out <- dplyr::tibble(
    variable = c("(Intercept)", colnames(dat)[-1]),
    grid_id = "default",
    beta = coefs,
    family = list(control$family),
    `s.e.` = summary(m)$coefficients[, 2],
    `t value` = summary(m)$coefficients[, 3],
    `p value` = summary(m)$coefficients[, 4],
    R.squared = summary(m)$adj.r.squared
  )

  return(out)

}
