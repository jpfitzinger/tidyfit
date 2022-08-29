#' @name .model.bayes
#' @title Bayesian generalized linear regression for \code{tidyfit}
#' @description Fits a Bayesian regression and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{stats::glm}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{bayesglm}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("bayes", Return ~ ., data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("bayes"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#'
#' @seealso \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom dplyr tibble as_tibble everything
#' @importFrom tidyr nest
#' @importFrom methods formalArgs
#' @importFrom purrr partial
#' @importFrom utils object.size

.model.bayes <- function(formula = NULL, data = NULL, control = NULL, ...) {

  f <- control$family
  control$x <- FALSE
  control$y <- FALSE
  control$model <- FALSE
  control <- control[names(control) %in% methods::formalArgs(arm::bayesglm)]

  m <- do.call(arm::bayesglm, append(list(formula = formula, data = data), control))

  model_handler <- purrr::partial(.handler.stats, object = m, formula = formula)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = everything())
  } else {
    settings <- NULL
  }

  out <- dplyr::tibble(
    estimator = "arm::bayesglm",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}
