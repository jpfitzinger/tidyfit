#' @name .model.lm
#' @title Linear regression for \code{tidyfit}
#' @description Fits a linear regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{lm}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{stats::lm}.
#'
#' An argument \code{vcov.} can be passed in control or to \code{...} in \code{\link{m}} to estimate the model with robust standard errors. \code{vcov.} can be one of "BS", "HAC", "HC" and "OPG" and is passed to the \code{sandwich} package.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("lm", x, y)
#' fit
#'
#' # With robust standard errors
#' fit <- m("lm", x, y, vcov. = "HAC")
#' fit
#'
#' @seealso \code{\link{.model.robust}}, \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom stats lm coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom purrr partial
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovBS vcovHAC vcovHC vcovOPG
#' @importFrom methods formalArgs

.model.lm <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
    ) {

  vcov. <- control$vcov.
  f <- control$family
  control <- control[names(control) %in% methods::formalArgs(stats::lm)]

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(stats::lm, append(list(formula = y~., data = dat), control))
  coefs <- stats::coef(m)

  if (is.null(vcov.)) {
    coef_stats <- summary(m)$coefficients
  } else if (vcov. == "BS") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovBS(m))
  } else if (vcov. == "HAC") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovHAC(m))
  } else if (vcov. == "HC") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m))
  } else if (vcov. == "OPG") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovOPG(m))
  }

  out <- dplyr::tibble(
    variable = c("(Intercept)", colnames(dat)[-1]),
    beta = coefs,
    family = list(f),
    `s.e.` = coef_stats[, 2],
    `t value` = coef_stats[, 3],
    `p value` = coef_stats[, 4],
    `Adj. R-squared` = summary(m)$adj.r.squared
    )
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, as_tibble(func_to_list(control)))
  }

  return(out)

}
