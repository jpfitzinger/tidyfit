#' @name mod_hfr
#' @title Hierarchical feature regression for \code{tidyfit}
#' @description Fits a hierarchical feature regression and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param kappa shrinkage penalty or sequence of shrinkage penalties.
#' @param ...  Additional arguments passed to \code{hfr::cv.hfr}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Pfitzinger J (2022).
#' _hfr: Estimate Hierarchical Feature Regression Models_.
#' R package version 0.5.0, <https://CRAN.R-project.org/package=hfr>.
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- mod_hfr(x, y, kappa = 0.5)
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom hfr cv.hfr
#' @importFrom dplyr mutate as_tibble
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty mixture

mod_hfr <- function(
    x = NULL,
    y = NULL,
    kappa = NULL,
    ...
    ) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_hfr))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  args <- args[names(args) %in% names(formals(hfr::cv.hfr))]

  if (is.null(kappa)) kappa <- seq(0, 1, by = 0.1)

  m <- do.call(hfr::cv.hfr, append(list(x = x, y = y, nfolds = 1,
                                        kappa_grid = kappa), args))

  coefs <- stats::coef(m)

  grid_ids <- paste0("s", seq_along(kappa))
  names(grid_ids) <- kappa

  out <- coefs %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable = rownames(coefs)) %>%
    tidyr::gather("kappa", "beta", -.data$variable) %>%
    dplyr::mutate(grid_id = grid_ids[.data$kappa])

  return(out)

}
