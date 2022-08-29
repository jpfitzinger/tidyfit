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
#' fit <- m("robust", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("robust"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' # With robust standard errors
#' fit <- m("robust", Return ~ `Mkt-RF` + HML + SMB, data, vcov. = "HAC")
#' tidyr::unnest(fit, settings)
#'
#' @seealso \code{\link{.model.lm}} and \code{\link{m}} methods
#'
#' @importFrom stats coef
#' @importFrom dplyr tibble bind_cols
#' @importFrom purrr partial
#' @importFrom MASS rlm
#' @importFrom utils size

.model.robust <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  vcov. <- control$vcov.
  f <- control$family
  # Available args are not accessible using formalArgs without importing MASS:::rlm.formula
  rlm_args <- c("x", "y", "weights", "w", "init", "psi", "scale.est", "k2", "method",
                        "wt.method", "maxit", "acc", "test.vec", "lqs.control")
  control <- control[names(control) %in% rlm_args]

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, data)
  y <- stats::model.response(mf)
  #var_names_map <- .names_map(colnames(model_mat))

  m <- do.call(MASS::rlm, append(list(x = x, y = y), control))

  model_handler <- purrr::partial(.handler.MASS, object = m, formula = formula, vcov. = vcov.)

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
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.MASS <- function(object, data, formula = NULL, names_map = NULL, vcov. = NULL, ..., .what = "model") {

  if (.what == "model") {
    return(object)
  }

  if (.what == "predict") {
    response_var <- all.vars(formula)[1]
    if (response_var %in% colnames(data)) {
      truth <- data[, response_var]
    } else {
      data[, response_var] <- 1
      truth <- NULL
    }
    mf <- stats::model.frame(formula, data)
    x <- stats::model.matrix(formula, data)
    y <- stats::model.response(mf)
    pred <- dplyr::tibble(
      prediction = drop(crossprod(t(x), object$coefficients)),
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
