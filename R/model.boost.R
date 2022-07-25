#' @name .model.boost
#' @title Gradient boosting regression for \code{tidyfit}
#' @description Fits a gradient boosting regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{mstop} (*number of boosting iterations*)
#' - \code{nu} (*step size*)
#'
#' The gradient boosting regression is performed using \code{mboost::glmboost}. For classification pass \code{family = "binomial"} to \code{control} or to the argument \code{...} in \code{\link{m}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$mstop)} and \code{is.null(control$nu)}), the default grid is used with \code{mstop = c(100, 500, 1000)} and \code{nu = c(0.01, 0.05, 0.1, 0.15, 0.2)}.
#'
#' @param x input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y response variable.
#' @param control  Additional arguments passed to \code{mboost::glmboost}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' T. Hothorn, P. Buehlmann, T. Kneib, M. Schmid, and B. Hofner (2022).
#' mboost: Model-Based Boosting, R package version 2.9-7, https://CRAN.R-project.org/package=mboost.
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rbinom(100, 1, 0.5)
#' fit <- m("boost", x, y, mstop = 100, family = binomial())
#' fit
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom stats coef
#' @importFrom mboost glmboost Gaussian Binomial
#' @importFrom purrr map_dbl partial quietly
#' @importFrom dplyr mutate left_join n
#' @importFrom tidyr gather expand_grid

.model.boost <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
    ) {

  f <- control$family
  if (f$family == "gaussian") {
    family <- mboost::Gaussian()
  } else if (f$family == "binomial") {
    family <- mboost::Binomial()
    y <- as.factor(y)
  }

  mstop <- control$mstop
  nu <- control$nu
  control <- control[names(control) %in% names(formals(mboost::glmboost))]
  control <- control[!names(control) %in% c("control", "center", "family")]

  if (is.null(mstop)) mstop <- c(100, 500, 1000)
  if (is.null(nu)) nu <- c(0.01, 0.05, 0.1, 0.15, 0.2)

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  xs <- data.frame(`(Intercept)` = 1, xs, check.names = FALSE)

  quiet_coefs <- purrr::quietly(stats::coef)

  hyper_grid <- tidyr::expand_grid(mstop = mstop, nu = nu) %>%
    dplyr::mutate(grid_id = paste0("s", 1:dplyr::n()))

  coefs <- purrr::map2_dfr(hyper_grid$mstop, hyper_grid$nu,
                           function(mstop, nu) {
      ctr <- mboost::boost_control(mstop = mstop, nu = nu)
      m <- do.call(mboost::glmboost, append(list(x = data.matrix(xs), y = y,
                                                 control = ctr, center = F,
                                                 family = family), control))

      beta <- purrr::map_dbl(colnames(xs), function(x) quiet_coefs(m, which = x, off2int = F)$result[x])
      beta[1] <- beta[1] + m$offset
      names(beta) <- colnames(xs)
      if (f$family == "binomial") {
        beta <- beta * 2
      }
      beta[-1] <- beta[-1] / standard_sd
      beta[1] <- beta[1] - crossprod(beta[-1], standard_mean)
      out <- tibble(variable = names(beta), beta = beta, mstop = mstop, nu = nu)
      return(out)
    })

  out <- coefs %>%
    dplyr::left_join(hyper_grid, by = c("mstop", "nu")) %>%
    mutate(family = list(f))

  return(out)

}
