#' @name m.lm
#' @title Linear regression for tidyfit
#' @description Fits a linear regression and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ...  Additional arguments passed to \code{lm}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = hfr(x, y, kappa = 0.5)
#' coef(fit)
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom stats lm coef

# x = matrix(rnorm(100 * 20), 100, 20)
# y = rnorm(100)
# m.lm(x, y, .ctr = list())

m.glm <- function(x, y, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::glm))]

  family <- args$family
  if (!is.null(family)) {
    if (family == "binomial") f <- binomial
    if (family == "gaussian") f <- gaussian
    if (family == "poisson") f <- poisson
    args$family <- f
  }

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(stats::glm, append(list(formula = y~., data = dat), args))
  coefs <- stats::coef(m)

  out <- tibble(
    variable = c("(Intercept)", colnames(dat)[-1]),
    grid_id = "X",
    beta = coefs,
    `s.e.` = summary(m)$coefficients[, 2],
    `t value` = summary(m)$coefficients[, 3],
    `p value` = summary(m)$coefficients[, 4],
    R.squared = summary(m)$adj.r.squared
  )

  return(out)

}
