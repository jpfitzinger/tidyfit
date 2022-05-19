#' @name m.plsr
#' @title Partial Least Squares Regression for tidyfit
#' @description Fits a partial least squares regression and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details The partial least squares regression is fitted using \code{pls} package. Covariates are standardized, with coefficients back-transformed to the original scale.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ncomp number of components or sequence of components.
#' @param ...  Additional arguments passed to \code{lm}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = m.plsr(x, y, ncomp = 4)
#' fit
#'
#' @export
#'
#' @seealso \code{m.pcr} method
#'
#' @importFrom pls plsr
#' @importFrom stats coef sd
#' @importFrom dplyr as_tibble mutate
#' @importFrom tidyr gather
#' @importFrom rlang .data


m.plsr <- function(x, y, ncomp = NULL, ...) {

  args <- list(...)
  if (!is.null(args$family)) {
    if (args$family == "binomial")
      stop("plsr cannot be used for classification")
  }
  args <- args[names(args) %in% names(formals(pls::plsr))]

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  if (is.null(ncomp)) {
    ncomps <- 1:ncol(xs)
  } else {
    ncomps <- ncomp
  }

  coefs <- sapply(ncomps, function(ncomp) {
    m <- do.call(pls::plsr, append(list(formula = y~xs, scale=F, center=T, ncomp=ncomp), args))
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
    dplyr::mutate(ncomp = ncomps[match(.data$grid_id, colnames(coefs))])

  return(out)

}
