#' @name m.lm
#' @title Linear regression for \code{tidyfit}
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
#' fit = m.lm(x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{m.lasso¸} method
#'
#' @importFrom stats lm coef
#' @importFrom dplyr tibble

m.lm <- function(x, y, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::lm))]

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(stats::lm, append(list(formula = y~., data = dat), args))
  coefs <- stats::coef(m)

  out <- dplyr::tibble(
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
