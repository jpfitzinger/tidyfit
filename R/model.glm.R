#' @name .model.glm
#' @title Generalized linear regression for \code{tidyfit}
#' @description Fits a linear or logistic regression and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{stats::glm}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{glm}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data$Return <- ifelse(data$Return > 0, 1, 0)
#'
#' # Stand-alone function
#' fit <- m("glm", Return ~ ., data)
#' fit
#'
#' # Within 'classify' function
#' fit <- classify(data, Return ~ ., m("glm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lm}} and \code{\link{m}} methods
#'
#' @importFrom stats glm coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom methods formalArgs

.model.glm <- function(formula = NULL, data = NULL, control = NULL, ...) {

  control <- control[names(control) %in% methods::formalArgs(stats::glm)]

  m <- do.call(stats::glm, append(list(formula = formula, data = data), control))

  model_handler <- purrr::partial(.handler.stats, object = m, formula = formula)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "stats::glm",
    handler = list(model_handler),
    settings
  )

  return(out)

}
