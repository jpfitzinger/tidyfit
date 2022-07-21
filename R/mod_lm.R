#' @name mod_lm
#' @title Linear regression for \code{tidyfit}
#' @description Fits a linear regression and returns the results as a tibble. The function can be used with \code{regress}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param vcov_type Robust standard errors. \code{sandwich} is used to calculate the vcov using \code{sandwich::vcovHAC}, \code{sandwich::vcovBS} etc.
#' @param ...  Additional arguments passed to \code{lm}.
#' @return A 'tibble'.
#' @details When called without \code{x} and \code{y} arguments, the function returns a partialised version of the function that can be called with data to fit the model.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- mod_lm(x, y)
#' fit
#'
#' # Partial execution
#' mod <- mod_lm(vcov = "HAC")
#' fit <- mod(x, y)
#' fit
#'
#' @export
#'
#' @seealso \code{mod_lasso} method
#'
#' @importFrom stats lm coef
#' @importFrom dplyr tibble
#' @importFrom purrr partial
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovBS vcovHAC vcovHC vcovOPG

mod_lm <- function(
    x = NULL,
    y = NULL,
    vcov_type = c("default", "BS", "HAC", "HC", "OPG"),
    ...
    ) {

  vcov_type <- match.arg(vcov_type)

  # Return a partial if no data is provided
  if (is.null(x) & is.null(y)) {

    args <- c(as.list(environment()), list(...))
    args <- args[!names(args) %in% c("x", "y")]
    args <- append(args, list(.f = tidyfit::mod_lm))
    return(do.call(purrr::partial, args))

  }

  args <- list(...)
  args <- args[names(args) %in% names(formals(stats::lm))]

  dat <- data.frame(y = y, x, check.names = FALSE)
  m <- do.call(stats::lm, append(list(formula = y~., data = dat), args))
  coefs <- stats::coef(m)

  if (vcov_type == "default") coef_stats <- summary(m)$coefficients
  if (vcov_type == "BS") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovBS(m))
  }
  if (vcov_type == "HAC") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovHAC(m))
  }
  if (vcov_type == "HC") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m))
  }
  if (vcov_type == "OPG") {
    coef_stats <- lmtest::coeftest(m, vcov. = sandwich::vcovOPG(m))
  }

  out <- dplyr::tibble(
    variable = c("(Intercept)", colnames(dat)[-1]),
    grid_id = "default",
    beta = coefs,
    `s.e.` = coef_stats[, 2],
    `t value` = coef_stats[, 3],
    `p value` = coef_stats[, 4],
    `Adj. R-squared` = summary(m)$adj.r.squared
    )

  return(out)

}
