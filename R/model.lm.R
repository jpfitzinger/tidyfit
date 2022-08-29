#' @name .model.lm
#' @title Linear regression for \code{tidyfit}
#' @description Fits a linear regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{lm}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{stats::lm}.
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
#' fit <- m("lm", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' # With robust standard errors
#' fit <- m("lm", Return ~ `Mkt-RF` + HML + SMB, data, vcov. = "HAC")
#' fit
#'
#' @seealso \code{\link{.model.robust}}, \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom stats lm
#' @importFrom dplyr tibble everything as_tibble
#' @importFrom tidyr nest
#' @importFrom purrr partial
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.lm <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
    ) {

  vcov. <- control$vcov.
  control$model <- FALSE
  control$x <- FALSE
  control$y <- FALSE
  control <- control[names(control) %in% methods::formalArgs(stats::lm)]

  m <- do.call(stats::lm, append(list(formula = formula, data = data), control))

  model_handler <- purrr::partial(.handler.stats, object = m, formula = formula, vcov. = vcov.)

  control <- control[!names(control) %in% c("weights")]
  control$vcov. <- vcov.
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "stats::lm",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.stats <- function(object, data, formula = NULL, names_map = NULL, vcov. = NULL, ..., .what = "model") {

  if (.what == "model") {
    return(object)
  }

  if (.what == "predict") {
    response_var <- all.vars(formula)[1]
    if (response_var %in% colnames(data)) {
      truth <- data[, response_var]
    } else {
      truth <- NULL
    }
    if (!is.null(names_map)) data <- data.frame(data)
    pred <- dplyr::tibble(
      prediction = stats::predict(object, data, type = "response"),
      truth = truth
    )
    return(pred)
  }

  if (.what == "estimates") {
    if (is.null(vcov.)) {
      adj_obj <- object
    } else if (vcov. == "BS") {
      adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovBS(object))
    } else if (vcov. == "HAC") {
      adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovHAC(object))
    } else if (vcov. == "HC") {
      adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovHC(object))
    } else if (vcov. == "OPG") {
      adj_obj <- lmtest::coeftest(object, vcov. = sandwich::vcovOPG(object))
    }
    estimates <- broom::tidy(adj_obj)
    if (!is.null(names_map)) estimates$term <- names_map[estimates$term]
    return(estimates)
  }

}
