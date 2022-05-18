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
#' @importFrom glmnet glmnet

# x = matrix(rnorm(100 * 20), 100, 20)
# y = rnorm(100)
# m.lm(x, y, .ctr = list())

m.enet <- function(x, y, lambda = NULL, alpha = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  if (is.null(lambda)) {
    lambda.max <- 0.25
    lambda <- seq(lambda.max, lambda.max * 0.0001, length.out = 100)
  }

  if (is.null(alpha)) alpha <- seq(0, 1, by = 0.2)

  mods <- seq_along(alpha) %>%
    map(function(i) {
      m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = alpha[i],
                                               lambda = lambda), args))

      coefs <- data.matrix(coef(m), rownames.force = T)
      colnames(coefs) <- paste(colnames(coefs), i, sep = "_")

      if (is.null(lambda)) {
        lambda <- m$lambda
      }

      grid <- tibble(
        alpha = alpha[i],
        lambda = lambda
      )

      return(list(coefs = coefs, grid = grid))
    })

  coefs <- map_dfc(mods, function(x) x$coefs)
  grid <- map_dfr(mods, function(x) x$grid)
  var_names <- rownames(mods[[1]]$coefs)

  out <- coefs %>%
    data.matrix %>%
    as_tibble %>%
    mutate(variable = var_names) %>%
    gather("grid_id", "beta", -variable) %>%
    mutate(alpha = grid$alpha[match(grid_id, colnames(coefs))]) %>%
    mutate(lambda = grid$lambda[match(grid_id, colnames(coefs))])

  return(out)

}
