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
#' @importFrom pls pcr

# x = matrix(rnorm(100 * 20), 100, 20)
# y = rnorm(100)
# m.lm(x, y, .ctr = list())

m.pcr <- function(x, y, ncomp = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(pls::pcr))]

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  if (is.null(ncomp)) {
    ncomps <- 1:ncol(xs)
  } else {
    ncomps <- ncomp
  }

  coefs <- sapply(ncomps, function(ncomp) {
    m <- do.call(pls::pcr, append(list(formula = y~xs, scale=F, center=T, ncomp=ncomp), args))
    beta <- drop(coef(m, intercept = T))
    beta[-1] <- beta[-1] / standard_sd
    beta[1] <- beta[1] - crossprod(beta[-1], standard_mean)
    return(beta)
  })
  colnames(coefs) <- as.character(ncomps)

  var_names <- rownames(coefs)

  out <- coefs %>%
    data.matrix %>%
    as_tibble %>%
    mutate(variable = var_names) %>%
    gather("grid_id", "beta", -variable) %>%
    mutate(ncomp = ncomps[match(grid_id, colnames(coefs))])

  return(out)

}
