#' @name .model.subset
#' @title Best subset regression and classification for \code{tidyfit}
#' @description Fits a best subset regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The best subset regression is estimated using \code{bestglm::bestglm} which is a wrapper around \code{leaps::regsubsets} for the regression case, and performs an exhaustive search for the classification case. For classification pass \code{family = "binomial"} to \code{control} or to the argument \code{...} in \code{\link{m}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' Forward or backward selection can be performed by passing \code{method = "forward"} or \code{method = "backward"} to \code{\link{m}}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{bestglm::bestglm}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' A.I. McLeod, Changjiang Xu and Yuanhao Lai (2020).
#' bestglm: Best Subset GLM and Regression Utilities.
#' R package version 0.37.3. URL https://CRAN.R-project.org/package=bestglm.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("subset", Return ~ ., data, method = c("forward", "backward"))
#' tidyr::unnest(fit, settings)
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("subset", method = "forward"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom bestglm bestglm
#' @importFrom purrr quietly partial
#' @importFrom methods formalArgs

.model.subset <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  control <- control[names(control) %in% methods::formalArgs(bestglm::bestglm)]
  if (is.null(control$family)) {
    f <- gaussian()
  } else {
    f <- control$family
  }

  control <- control[names(control) != "family"]

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  Xy <- data.frame(x, y, check.names = FALSE)
  var_names_map <- .names_map(colnames(Xy))

  quiet_bestglm <- purrr::quietly(bestglm::bestglm)

  if (f$family == "gaussian") {
    part_bestglm <- purrr::partial(quiet_bestglm, family = gaussian, intercept = incl_intercept)
  } else if (f$family == "binomial") {
    part_bestglm <- purrr::partial(quiet_bestglm, family = binomial, intercept = incl_intercept)
  }

  m <- do.call(part_bestglm, append(list(Xy = data.frame(Xy)), control))$result
  m <- m$BestModel

  model_handler <- purrr::partial(.handler.bestglm, object = m, formula = formula, names_map = var_names_map)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "bestglm::bestglm",
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.bestglm <- function(object, data, formula = NULL, names_map = NULL, ..., .what = "model") {

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
    data <- data.frame(stats::model.matrix(formula, data))
    pred <- dplyr::tibble(
      prediction = stats::predict(object, data, type = "response"),
      truth = truth
    )
    return(pred)
  }

  if (.what == "estimates") {
    estimates <- broom::tidy(object)
    estimates$term <- names_map[estimates$term]
    return(estimates)
  }

}
