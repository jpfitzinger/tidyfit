#' @name .model.quantile
#' @title Quantile regression for \code{tidyfit}
#' @description Fits a linear quantile regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{quantreg::rq}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{quantreg::rq}. The argument \code{tau} is the chosen quantile (default \code{tau = 0.5}).
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("quantile", Return ~ ., data, tau = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ .,
#'                m("quantile", tau = c(0.1, 0.5, 0.9)),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lm}}, \code{\link{.model.bayes}} and \code{\link{m}} methods
#'
#' @importFrom stats coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom methods formalArgs

.model.quantile <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  f <- control$family
  control <- control[names(control) %in% methods::formalArgs(quantreg::rq)]

  m <- do.call(quantreg::rq, append(list(formula = formula, data = data), control))
  model_handler <- purrr::partial(.handler.stats, object = m, formula = formula)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "quantreg::rq",
    handler = list(model_handler),
    settings
  )

  return(out)

}
