#' @name .model.robust
#' @title Robust regression for \code{tidyfit}
#' @description Fits a robust linear regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{MASS::rlm}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{MASS::rlm}.
#'
#' An argument \code{vcov.} can be passed in control or to \code{...} in \code{\link{m}} to estimate the model with robust standard errors. \code{vcov.} can be one of "BS", "HAC", "HC" and "OPG" and is passed to the \code{sandwich} package.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("robust", x, y, method = "MM")
#' fit
#'
#' # With robust standard errors
#' fit <- m("robust", x, y, vcov. = "HAC")
#' fit
#'
#' @seealso \code{\link{.model.lm}} and \code{\link{m}} methods
#'
#' @importFrom stats coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom purrr partial
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovBS vcovHAC vcovHC vcovOPG
#' @importFrom MASS rlm

.model.robust <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
) {

  vcov. <- control$vcov.
  f <- control$family
  # Available args are not accessible using formalArgs without importing MASS:::rlm.default
  rlm.default_args <- c("x", "y", "weights", "w", "init", "psi", "scale.est", "k2", "method",
                        "wt.method", "maxit", "acc", "test.vec", "lqs.control")
  control <- control[names(control) %in% rlm.default_args]

  x <- data.frame(`(Intercept)` = 1, x, check.names = FALSE)

  m <- do.call(MASS::rlm, append(list(x = x, y = y), control))
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
    variable = colnames(x),
    beta = coefs,
    family = list(f),
    `s.e.` = coef_stats[, 2],
    `t value` = coef_stats[, 3]
  )
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, as_tibble(func_to_list(control)))
  }

  return(out)

}
