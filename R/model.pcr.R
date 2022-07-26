#' @name .model.pcr
#' @title Principal Components Regression for \code{tidyfit}
#' @description Fits a principal components regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{ncomp} *(number of components)*
#'
#' The principal components regression is fitted using \code{pls} package. Covariates are standardized, with coefficients back-transformed to the original scale. An intercept is always included.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$ncomp)}), the default is \code{seq(1, nvars)}, where nvars is the number of features.
#'
#' Note that at present \code{pls} does not offer weighted implementations or non-gaussian response. The method can therefore only be used with \code{\link{regress}}
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{pls::pcr}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Kristian Hovde Liland, Bjørn-Helge Mevik, Ron Wehrens (2022).
#' pls: Partial Least Squares and Principal Component Regression.
#' R package version 2.8-1. URL https://CRAN.R-project.org/package=pls.
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("pcr", x, y, ncomp = 4)
#' fit
#'
#' @seealso \code{\link{.model.plsr}} and \code{\link{m}} methods
#'
#' @importFrom pls pcr
#' @importFrom stats coef sd
#' @importFrom dplyr as_tibble mutate
#' @importFrom tidyr gather

.model.pcr <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
    ) {

  if ("weights" %in% names(control)) {
    warning("pcr cannot handle weights, weights are ignored")
  }
  f <- control$family
  control <- control[names(control) %in% names(formals(pls::pcr))]

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  if (is.null(control$ncomp)) {
    control$ncomp <- 1:ncol(xs)
  }

  ncomps <- control$ncomp
  control <- control[names(control) != "ncomp"]

  coefs <- sapply(ncomps, function(ncomp) {
    m <- do.call(pls::pcr, append(list(formula = y~xs, scale=F, center=T, ncomp=ncomp), control))
    beta <- drop(stats::coef(m, intercept = T))
    beta[-1] <- beta[-1] / standard_sd
    beta[1] <- beta[1] - crossprod(beta[-1], standard_mean)
    return(beta)
  })
  colnames(coefs) <- as.character(ncomps)

  var_names <- rownames(coefs)

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(ncomp = ncomps[match(.data$grid_id, colnames(coefs))]) %>%
    mutate(family = list(f))

  return(out)

}
