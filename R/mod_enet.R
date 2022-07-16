#' @name mod_enet
#' @title ElasticNet regression or classification for \code{tidyfit}
#' @description Fits an ElasticNet regression or classification and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details The ElasticNet regression is estimated using \code{glmnet::glmnet}. For classification pass \code{family = "binomial"} to \code{...}.
#'
#' If no hyperparameter grid is passed (\code{lambda = NULL} and \code{alpha = NULL}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100 for \code{lambda} and 5 for \code{alpha}.
#'
#' When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param lambda lambda value or lambda sequence for shrinkage penalty. See Details.
#' @param alpha The elasticnet mixing parameter. \code{alpha=1} is the lasso penalty, and \code{alpha=0} the ridge penalty. See Details.
#' @param ...  Additional arguments passed to \code{glmnet::glmnet}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- mod_enet(x, y, kappa = 0.5)
#' fit
#'
#' @export
#'
#' @seealso \code{mod_lasso} and \code{mod_ridge} methods
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom purrr map_dfc map_dfr map
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty mixture

mod_enet <- function(x = NULL,
                     y = NULL,
                     lambda = NULL,
                     alpha = NULL,
                     ...
                     ) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_enet))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  if (is.null(lambda)) {
    lambda <- dials::grid_regular(dials::penalty(), levels = 100)$penalty
  }

  if (is.null(alpha)) {
    alpha <- dials::grid_regular(dials::mixture(), levels = 5)$mixture
  }

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
