#' @name .model.enet
#' @title ElasticNet regression or classification for \code{tidyfit}
#' @description Fits an ElasticNet regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(penalty)*
#' - \code{alpha} *(L1-L2 mixing parameter)*
#'
#' The ElasticNet regression is estimated using \code{glmnet::glmnet}. For classification pass \code{family = "binomial"} to \code{control} or to the argument \code{...} in \code{\link{m}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)} and \code{is.null(control$alpha)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100 for \code{lambda} and 5 for \code{alpha}. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{glmnet::glmnet}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#' Regularization Paths for Generalized Linear Models via Coordinate Descent.
#' Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("enet", x, y, kappa = 0.5)
#' fit
#'
#' @seealso \code{\link{.model.lasso}}, \code{\link{.model.adalasso}}, \code{\link{.model.ridge}} and \code{\link{m}} methods
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom purrr map_dfc map_dfr map
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom dials grid_regular penalty mixture

.model.enet <- function(x = NULL,
                        y = NULL,
                        control = NULL,
                        ...
                        ) {

  control <- control[names(control) %in% names(formals(glmnet::glmnet))]

  f <- control$family
  control$family <- f$family

  if (is.null(control$lambda)) {
    control$lambda <- dials::grid_regular(dials::penalty(), levels = 100)$penalty
  }

  if (is.null(control$alpha)) {
    control$alpha <- dials::grid_regular(dials::mixture(), levels = 5)$mixture
  }

  alpha <- control$alpha
  control <- control[names(control) != "alpha"]

  mods <- seq_along(alpha) %>%
    purrr::map(function(i) {
      m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = alpha[i]), control))

      coefs <- data.matrix(stats::coef(m), rownames.force = T)
      colnames(coefs) <- paste(colnames(coefs), i, sep = "_")

      grid <- tibble(
        alpha = alpha[i],
        lambda = control$lambda
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
    dplyr::mutate(lambda = grid$lambda[match(.data$grid_id, colnames(coefs))]) %>%
    mutate(family = list(f))

  return(out)

}
