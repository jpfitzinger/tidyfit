#' @name m.cor
#' @title Pearson correlation coefficients for tidyfit
#' @description Calculates Pearson correlation coefficients and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ...  Additional arguments passed to \code{cor}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = m.cor(x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom stats cor
#' @importFrom dplyr tibble
#' @importFrom stringr str_replace_all

m.cor <- function(x, y, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::cor))]

  coefs <- drop(stats::cor(x, y))
  coefs <- c(0, coefs)
  names(coefs)[1] <- "(Intercept)"

  out <- dplyr::tibble(
    variable = stringr::str_replace_all(names(coefs), "`", ""),
    grid_id = "X",
    beta = coefs
  )

  return(out)

}
