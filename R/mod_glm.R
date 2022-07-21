#' @name mod_glm
#' @title Logistic regression for \code{tidyfit}
#' @description Fits a logistic regression and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @details
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param ...  Additional arguments passed to \code{glm}.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rbinom(100, 1, 0.5)
#' fit <- mod_glm(x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{mod_lm} method
#'
#' @importFrom stats glm coef binomial gaussian poisson
#' @importFrom dplyr tibble
#' @importFrom purrr partial
#' @importFrom dials grid_regular penalty

mod_glm <- function(x = NULL, y = NULL, ...) {

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_glm))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::glm))]

  family <- args$family
  if (!is.null(family)) {
    if (family == "binomial") f <- binomial
    if (family == "gaussian") f <- gaussian
    if (family == "poisson") f <- poisson
    args$family <- f
  }

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(stats::glm, append(list(formula = y~., data = dat), args))
  coefs <- stats::coef(m)

  out <- dplyr::tibble(
    variable = c("(Intercept)", colnames(dat)[-1]),
    grid_id = "X",
    beta = coefs,
    `s.e.` = summary(m)$coefficients[, 2],
    `t value` = summary(m)$coefficients[, 3],
    `p value` = summary(m)$coefficients[, 4],
    R.squared = summary(m)$adj.r.squared
  )

  return(out)

}
