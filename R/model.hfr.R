#' @name .model.hfr
#' @title Hierarchical feature regression for \code{tidyfit}
#' @description Fits a hierarchical feature regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - kappa (*proportional size of regression graph*)
#'
#' The hierarchical feature regression is estimated using the \code{hfr::cv.hfr} function. An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is provided (\code{is.null(control$kappa)}), the default is \code{seq(0, 1, by = 0.1)}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{hfr::cv.hfr}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Pfitzinger J (2022).
#' _hfr: Estimate Hierarchical Feature Regression Models_.
#' R package version 0.5.0, <https://CRAN.R-project.org/package=hfr>.
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("hfr", x, y, kappa = 0.5)
#' fit
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom hfr cv.hfr
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data

.model.hfr <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
    ) {

  f <- control$family
  names(control)[names(control)=="kappa"] <- "kappa_grid"
  control <- control[names(control) %in% names(formals(hfr::cv.hfr))]

  if (is.null(control$kappa_grid)) control$kappa_grid <- seq(0, 1, by = 0.1)

  m <- do.call(hfr::cv.hfr, append(list(x = x, y = y, nfolds = 1), control))

  coefs <- stats::coef(m)

  grid_ids <- paste0("s", seq_along(control$kappa_grid))
  names(grid_ids) <- control$kappa_grid

  out <- coefs %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = rownames(coefs)) %>%
    tidyr::gather("kappa", "beta", -.data$variable) %>%
    dplyr::mutate(grid_id = grid_ids[.data$kappa]) %>%
    mutate(family = list(f))

  return(out)

}
