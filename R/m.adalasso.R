#' @name m.adalasso
#' @title Adaptive Lasso for tidyfit
#' @description Fits an adaptive Lasso regression and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details The adaptive Lasso is a weighted implementation of the Lasso algorithm, with covariate-specific weights obtained using an initial regression fit (in this case, a ridge regression with \code{lambda = 0.01}). The adaptive Lasso is computed using the 'glmnet' package.
#'
#' The function can be used for classification or regression, covariates are standardized and an intercept is always included.
#'
#' @param x input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y response variable.
#' @param lambda shrinkage parameter or vector of shrinkage parameters.
#' @param ...  Additional arguments passed to \code{glmnet::glmnet}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Zou, H. (2006).
#' The Adaptive Lasso and Its Oracle Properties.
#' Journal of the American Statistical Association, 101(476), 1418-1429.
#'
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#' Regularization Paths for Generalized Linear Models via Coordinate Descent.
#' Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = m.adalasso(x, y, lambda = 0.1)
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom glmnet glmnet
#' @importFrom stats coef

m.adalasso <- function(x, y, lambda = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  penalty_mod <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 0,
                                                     lambda = 0.01), args))
  penalty_factor <- abs(stats::coef(penalty_mod)[-1])^(-1)
  penalty_factor[is.infinite(penalty_factor) | is.na(penalty_factor)] <- 0

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 1,
                                           lambda = lambda,
                                           penalty.factor = penalty_factor), args))

  coefs <- coef(m)
  var_names <- rownames(coefs)

  if (is.null(lambda)) lambda <- m$lambda

  out <- coefs %>%
    data.matrix %>%
    as_tibble %>%
    mutate(variable = var_names) %>%
    gather("grid_id", "beta", -variable) %>%
    mutate(lambda = lambda[match(grid_id, colnames(coefs))])

  return(out)

}
