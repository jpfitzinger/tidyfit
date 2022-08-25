#' @name .model.robust
#' @title Robust regression for \code{tidyfit}
#' @description Fits a robust linear regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{MASS::rlm}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{MASS::rlm}.
#'
#' An argument \code{vcov.} can be passed in control or to \code{...} in \code{\link{m}} to estimate the model with robust standard errors. \code{vcov.} can be one of "BS", "HAC", "HC" and "OPG" and is passed to the \code{sandwich} package.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("robust", Return ~ ., data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("robust"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' # With robust standard errors
#' fit <- m("robust", Return ~ ., data, vcov. = "HAC")
#' tidyr::unnest(fit, settings)
#'
#' @seealso \code{\link{.model.lm}} and \code{\link{m}} methods
#'
#' @importFrom stats coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom purrr partial
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovBS vcovHAC vcovHC vcovOPG
#' @importFrom MASS rlm

.model.robust <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  vcov. <- control$vcov.
  f <- control$family
  # Available args are not accessible using formalArgs without importing MASS:::rlm.formula
  rlm.formula_args <- c("formula", "data", "weights", "subset", "na.action", "method",
                        "wt.method", "model", "x.ret", "y.ret", "contrasts")
  control <- control[names(control) %in% rlm.formula_args]

  m <- do.call(MASS::rlm, append(list(formula = formula, data = data.frame(data)), control))

  if (is.null(vcov.)) {
    adj_m <- m
  } else if (vcov. == "BS") {
    adj_m <- lmtest::coeftest(m, vcov. = sandwich::vcovBS(m))
  } else if (vcov. == "HAC") {
    adj_m <- lmtest::coeftest(m, vcov. = sandwich::vcovHAC(m))
  } else if (vcov. == "HC") {
    adj_m <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m))
  } else if (vcov. == "OPG") {
    adj_m <- lmtest::coeftest(m, vcov. = sandwich::vcovOPG(m))
  }

  model_handler <- purrr::partial(.handler.stats, object = m, formula = formula)

  control <- control[!names(control) %in% c("weights")]
  control$vcov. <- vcov.
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "MASS::rlm",
    handler = list(model_handler),
    settings
  )

  return(out)

}
