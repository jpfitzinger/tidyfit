#' @name m.hfr
#' @title Hierarchical feature regression for tidyfit
#' @description Fits a hierarchical feature regression and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param kappa shrinkage penalty or sequence of shrinkage penalties.
#' @param ...  Additional arguments passed to \code{lm}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Pfitzinger J (2022).
#' _hfr: Estimate Hierarchical Feature Regression Models_.
#' R package version 0.5.0, <https://CRAN.R-project.org/package=hfr>.
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = m.hfr(x, y, kappa = 0.5)
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom hfr cv.hfr
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data

m.hfr <- function(x, y, kappa = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(hfr::cv.hfr))]

  if (is.null(kappa)) kappa <- seq(0, 1, by = 0.1)

  m <- do.call(hfr::cv.hfr, append(list(x = x, y = y, nfolds = 1,
                                        kappa_grid = kappa), args))

  coefs <- stats::coef(m)

  out <- coefs %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = rownames(coefs)) %>%
    dplyr::mutate(variable = ifelse(.data$variable == "intercept", "(Intercept)", .data$variable)) %>%
    tidyr::gather("kappa", "beta", -.data$variable) %>%
    dplyr::mutate(grid_id = kappa)

  return(out)

}
