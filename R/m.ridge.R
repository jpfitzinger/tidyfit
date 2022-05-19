#' @name m.ridge
#' @title Ridge regression for tidyfit
#' @description Fits a ridge regression and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param lambda lambda value or lambda sequence for shrinkage penalty.
#' @param ...  Additional arguments passed to \code{glmnet}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = m.ridge(x, y, kappa = 0.5)
#' fit
#'
#' @export
#'
#' @seealso \code{m.enet}, \code{m.lasso} and \code{m.adalasso} methods
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data

m.ridge <- function(x, y, lambda = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  if (is.null(lambda)) {
    lambda.max <- 0.25
    lambda <- seq(lambda.max, lambda.max * 0.0001, length.out = 100)
  }

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 0,
                                           lambda = lambda), args))

  coefs <- stats::coef(m)
  var_names <- rownames(coefs)

  if (is.null(lambda)) lambda <- m$lambda

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(lambda = lambda[match(.data$grid_id, colnames(coefs))])

  return(out)

}
