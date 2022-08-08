#' @name .model.adalasso
#' @title Adaptive Lasso regression or classification for \code{tidyfit}
#' @description Fits an adaptive Lasso regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L1 penalty)*
#'
#' The adaptive Lasso is a weighted implementation of the Lasso algorithm, with covariate-specific weights obtained using an initial regression fit (in this case, a ridge regression with \code{lambda = 0.01}). The adaptive Lasso is computed using the \code{glmnet::glmnet} function. For classification pass \code{family = "binomial"}  to \code{control} or to the argument \code{...} in \code{\link{m}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param x input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y response variable.
#' @param control  Additional arguments passed to \code{glmnet::glmnet}.
#' @param ... Not used.
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
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("adalasso", x, y, lambda = 0.1)
#' fit
#'
#' @seealso \code{\link{.model.lasso}}, \code{\link{.model.enet}}, \code{\link{.model.ridge}} and \code{\link{m}} methods
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble bind_cols select
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.model.adalasso <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
    ) {

  control <- control[names(control) %in% methods::formalArgs(glmnet::glmnet)]
  control <- control[names(control) != "alpha"]

  f <- control$family
  control$family <- f$family

  control_ <- control[names(control) != "lambda"]
  penalty_mod <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 0,
                                                     lambda = 0.01), control_))
  penalty_factor <- abs(stats::coef(penalty_mod)[-1])^(-1)
  penalty_factor[is.infinite(penalty_factor) | is.na(penalty_factor)] <- 0

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 1,
                                           penalty.factor = penalty_factor), control))

  coefs <- coef(m)
  var_names <- rownames(coefs)
  colnames(coefs) <- formatC(1:ncol(coefs), 2, flag = "0")

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(lambda = control$lambda[match(.data$grid_id, colnames(coefs))]) %>%
    dplyr::mutate(family = list(f)) %>%
    dplyr::select(.data$variable, .data$beta, .data$grid_id, .data$family, .data$lambda)

  control <- control[!names(control) %in% c("family", "lambda")]
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, as_tibble(func_to_list(control)))
  }

  return(out)

}
