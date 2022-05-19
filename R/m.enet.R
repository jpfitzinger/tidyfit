#' @name m.enet
#' @title ElasticNet regression or classification for tidyfit
#' @description Fits an elasticnet regression or classification and returns the results as a tibble. The function can be used with \code{tidyfit}.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param lambda shrinkage parameter or vector of shrinkage parameters.
#' @param alpha The elasticnet mixing parameter. \code{alpha=1} is the lasso penalty, and \code{alpha=0} the ridge penalty.
#' @param ...  Additional arguments passed to \code{lm}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = rnorm(100)
#' fit = m.enet(x, y, kappa = 0.5)
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom purrr map_dfc map_dfr map
#' @importFrom stats coef
#' @importFrom rlang .data

m.enet <- function(x, y, lambda = NULL, alpha = NULL, ...) {

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  if (is.null(lambda)) {
    lambda.max <- 0.25
    lambda <- seq(lambda.max, lambda.max * 0.0001, length.out = 100)
  }

  if (is.null(alpha)) alpha <- seq(0, 1, by = 0.2)

  mods <- seq_along(alpha) %>%
    purrr::map(function(i) {
      m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = alpha[i],
                                               lambda = lambda), args))

      coefs <- data.matrix(stats::coef(m), rownames.force = T)
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

  coefs <- purrr::map_dfc(mods, function(x) x$coefs)
  grid <- purrr::map_dfr(mods, function(x) x$grid)
  var_names <- rownames(mods[[1]]$coefs)

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(alpha = grid$alpha[match(.data$grid_id, colnames(coefs))]) %>%
    dplyr::mutate(lambda = grid$lambda[match(.data$grid_id, colnames(coefs))])

  return(out)

}
