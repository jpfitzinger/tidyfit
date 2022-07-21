#' @name mod_boost
#' @title Gradient boosting regression for \code{tidyfit}
#' @description Fits a gradient boosting regression or classification and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details The gradient boosting regression is performed using 'mboost::glmboost'.
#'
#' The function can be used for classification or regression, covariates are standardized and an intercept is always included.
#'
#' If no hyperparameter grid is passed (\code{mstop = NULL}), the default grid is used with \code{mstop = c(100, 500, 1000, 2000)}.
#'
#' When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
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
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rbinom(100, 1, 0.5)
#' fit <- mod_boost(x, y, mstop = 100, family = "binomial")
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom mboost glmboost Gaussian Binomial
#' @importFrom purrr map_dbl partial quietly
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom dials grid_regular penalty mixture

mod_boost <- function(
    x = NULL,
    y = NULL,
    mstop = NULL,
    ...
    ) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_boost))
    return(do.call(purrr::partial, args))

  }

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

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  xs <- data.frame(intercept = 1, xs, check.names = FALSE)

  quiet_coefs <- purrr::quietly(mboost:::coef.glmboost)

  coefs <- sapply(mstop, function(mst) {
    ctr <- mboost::boost_control(mstop = mst, nu = 0.1)
    m <- do.call(mboost::glmboost, append(list(x = data.matrix(xs), y = y,
                                               control = ctr, center = F,
                                               family = family), args))

    beta <- purrr::map_dbl(colnames(xs), function(x) quiet_coefs(m, which = x, off2int = F)$result[x])
    beta[1] <- beta[1] + m$offset
    names(beta) <- colnames(xs)
    beta <- beta * 2
    beta[-1] <- beta[-1] / standard_sd
    beta[1] <- beta[1] - crossprod(beta[-1], standard_mean)
    return(beta)
  })
  colnames(coefs) <- as.character(mstop)

  var_names <- rownames(coefs)

  grid_ids <- paste0("s", seq_along(mstop))
  names(grid_ids) <- mstop

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble( ) %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(variable = ifelse(.data$variable == "intercept", "(Intercept)", .data$variable)) %>%
    dplyr::mutate(mstop = mstop[match(.data$grid_id, colnames(coefs))]) %>%
    mutate(grid_id = grid_ids[.data$grid_id])

  return(out)

}
