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
#' @importFrom methods formalArgs

.model.enet <- function(x = NULL,
                        y = NULL,
                        control = NULL,
                        ...
                        ) {

  control <- control[names(control) %in% methods::formalArgs(glmnet::glmnet)]

  f <- control$family
  control$family <- f$family

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y), control))

  coefs <- coef(m)
  var_names <- rownames(coefs)
  colnames(coefs) <- formatC(1:ncol(coefs), 2, flag = "0")

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(lambda = control$lambda[match(.data$grid_id, colnames(coefs))]) %>%
    mutate(family = list(f))
  control <- control[!names(control) %in% c("family", "lambda")]
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, as_tibble(func_to_list(control)))
  }

  return(out)

}
