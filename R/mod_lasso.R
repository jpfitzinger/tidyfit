#' @name mod_lasso
#' @title Lasso regression and classification for \code{tidyfit}
#' @description Fits a linear regression or classification with L1 penalty and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details The Lasso regression is estimated using \code{glmnet::glmnet} with \code{alpha = 1}. For classification pass \code{family = "binomial"} to \code{...}.
#'
#' If no hyperparameter grid is passed (\code{lambda = NULL}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100.
#'
#' When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param lambda lambda value or lambda sequence for shrinkage penalty. See Details.
#' @param ...  Additional arguments passed to \code{glmnet::glmnet}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- mod_lasso(x, y)
#' fit
#'
#' # Partial execution
#' mod <- mod_lasso(lambda = 1)
#' fit <- mod(x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{m.enet}, \code{m.ridge} and \code{m.adalasso} methods
#'
#' @importFrom glmnet glmnet
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty

mod_lasso <- function(
    x = NULL,
    y = NULL,
    lambda = NULL,
    ...
    ) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_lasso))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  args <- args[names(args) %in% names(formals(glmnet::glmnet))]

  if (is.null(lambda)) {
    lambda <- dials::grid_regular(dials::penalty(), levels = 100)$penalty
  }

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 1,
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
