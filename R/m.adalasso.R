#' @name m.lasso
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
#' @importFrom glmnet::glmnet

# x = matrix(rnorm(100 * 20), 100, 20)
# y = rnorm(100)
# m.lm(x, y, .ctr = list())

m.adalasso <- function(x, y, lambda = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  penalty_mod <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 0,
                                                     lambda = 0.01), args))
  penalty_factor <- abs(coef(penalty_mod)[-1])^(-1)
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
