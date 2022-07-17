#' @name mod_pcr
#' @title Principal Components Regression for \code{tidyfit}
#' @description Fits a principal components regression and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details The principal components regression is fitted using \code{pls} package. Covariates are standardized, with coefficients back-transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{ncomps = NULL}), the default is \code{seq(1, nvars)}, where nvars is the number of features.
#'
#' When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ncomp number of components or sequence of components.
#' @param ...  Additional arguments passed to \code{pls::pcr}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- mod_pcr(x, y, ncomp = 4)
#' fit
#'
#' @export
#'
#' @seealso \code{mod_plsr} method
#'
#' @importFrom pls pcr
#' @importFrom stats coef sd
#' @importFrom dplyr as_tibble mutate
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty

mod_pcr <- function(
    x = NULL,
    y = NULL,
    ncomp = NULL,
    ...
    ) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_pcr))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  if (!is.null(args$family)) {
    if (args$family == "binomial")
      stop("pcr cannot be used for classification")
  }
  if ("weights" %in% names(args)) {
    warning("pcr cannot handle weights, weights are ignored")
  }
  args <- args[names(args) %in% names(formals(pls::pcr))]

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  if (is.null(ncomp)) {
    ncomps <- 1:ncol(xs)
  } else {
    ncomps <- ncomp
  }

  coefs <- sapply(ncomps, function(ncomp) {
    m <- do.call(pls::pcr, append(list(formula = y~xs, scale=F, center=T, ncomp=ncomp), args))
    beta <- drop(stats::coef(m, intercept = T))
    beta[-1] <- beta[-1] / standard_sd
    beta[1] <- beta[1] - crossprod(beta[-1], standard_mean)
    return(beta)
  })
  colnames(coefs) <- as.character(ncomps)

  var_names <- rownames(coefs)

  out <- coefs %>%
    data.matrix %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = var_names) %>%
    tidyr::gather("grid_id", "beta", -.data$variable) %>%
    dplyr::mutate(ncomp = ncomps[match(.data$grid_id, colnames(coefs))])

  return(out)

}
