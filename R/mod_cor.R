#' @name mod_cor
#' @title Pearson correlation coefficients for \code{tidyfit}
#' @description Calculates Pearson correlation coefficients and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ...  Additional arguments passed to \code{cor}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- mod_cor(x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom stats cor
#' @importFrom dplyr tibble
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty mixture

mod_cor <- function(x = NULL, y = NULL, ...) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_cor))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::cor))]

  x <- data.frame(x, check.names = FALSE)

  coefs <- drop(stats::cor(x, y))

  out <- dplyr::tibble(
    variable = c("(Intercept)", names(coefs)),
    grid_id = "default",
    beta = c(0, coefs)
  )

  return(out)

}
