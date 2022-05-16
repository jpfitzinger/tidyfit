#' @name m.boost
#' @title Gradient boosting regression for tidyfit
#' @description Fits a gradient boosting regression and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details The gradient boosting regression is performed using 'mboost::glmboost'.
#'
#' The function can be used for classification or regression, covariates are standardized and an intercept is always included.
#'
#' @param x input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y response variable.
#' @param mstop an integer giving the number of initial boosting iterations. If mstop = 0, the offset model is returned.
#' @param ...  Additional arguments passed to \code{mboost::glmboost}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' T. Hothorn, P. Buehlmann, T. Kneib, M. Schmid, and B. Hofner (2022).
#' mboost: Model-Based Boosting, R package version 2.9-7, https://CRAN.R-project.org/package=mboost.
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rbinom(100, 1, 0.5)
#' fit = m.boost(x, y, mstop = 100, family = "binomial")
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom stats coef
#' @importFrom mboost glmboost Gaussian Binomial

m.boost <- function(x, y, mstop = NULL, ...) {

  args <- list(...)
  if (!is.null(args$family)) {
    if (args$family == "gaussian") {
      family <- mboost::Gaussian()
    } else if (args$family == "binomial") {
      family <- mboost::Binomial()
      y <- as.factor(y)
    }
  } else {
    family <- mboost::Gaussian()
  }
  args <- args[names(args) %in% names(formals(mboost::glmboost))]
  args <- args[!names(args) %in% c("control", "center", "family")]

  if (is.null(mstop)) mstop <- c(100, 500, 1000, 2000)

  x <- data.frame(intercept = 1, x, check.names = FALSE)

  coefs <- sapply(mstop, function(mst) {
    ctr <- mboost::boost_control(mstop = mst, nu = 0.1)
    m <- do.call(mboost::glmboost, append(list(x = data.matrix(x), y = y,
                                               control = ctr, center = T,
                                               family = family), args))
    beta <- map_dbl(colnames(x), function(x) stats::coef(m, which = x, off2int = F)[x])
    beta[1] <- beta[1] + m$offset
    names(beta) <- colnames(x)

    return(beta)
  })
  colnames(coefs) <- as.character(mstop)

  var_names <- rownames(coefs)

  out <- coefs %>%
    data.matrix %>%
    as_tibble %>%
    mutate(variable = var_names) %>%
    gather("grid_id", "beta", -variable) %>%
    mutate(variable = ifelse(variable == "intercept", "(Intercept)", variable)) %>%
    mutate(mstop = mstop[match(grid_id, colnames(coefs))])

  return(out)

}
