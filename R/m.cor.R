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

m.cor <- function(x, y, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::cor))]

  coefs <- drop(stats::cor(x, y))
  coefs <- c(0, coefs)
  names(coefs)[1] <- "(Intercept)"

  out <- tibble(
    variable = str_replace_all(names(coefs), "`", ""),
    grid_id = "X",
    beta = coefs
  )

  return(out)

}
