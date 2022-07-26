#' @name .model.cor
#' @title Pearson correlation coefficients for \code{tidyfit}
#' @description Calculates Pearson correlation coefficients and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{stats::cor}.
#'
#' Note that this method is not compatible with \code{\link{cross_prod}} and the results will be nonsensical.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{cor}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("cor", x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom stats cor
#' @importFrom dplyr tibble

.model.cor <- function(x = NULL, y = NULL, control = NULL, ...) {

  f <- control$family
  control <- control[names(control) %in% names(formals(stats::cor))]

  x <- data.frame(x, check.names = FALSE)

  coefs <- drop(stats::cor(x, y))

  out <- dplyr::tibble(
    variable = c("(Intercept)", names(coefs)),
    grid_id = "default",
    beta = c(0, coefs),
    family = list(f)
  )

  return(out)

}
