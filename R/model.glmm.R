#' @name .model.glmm
#' @title Generalized linear mixed-effects model for \code{tidyfit}
#' @description Fits a linear or logistic mixed-effects model (GLMM) and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{lme4::glmer}.
#'
#' Note that the function should only be used in conjunction with \code{regress} or \code{classify} and not as a stand-alone function. This is because of the reliance on the formula syntax in \code{glmer}, which requires some specialized arguments passed to 'control'.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{glmer}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data$Return <- ifelse(data$Return > 0, 1, 0)
#'
#' # Estimate model with random effects
#' fit <- classify(data, Return ~ CMA + (CMA | Industry), logit = m("glmm"), .mask = "Date")
#' fit
#'
#'
#' @seealso \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom dplyr tibble bind_cols mutate all_of
#' @importFrom tidyr expand_grid pivot_longer
#' @importFrom purrr map_dfr map2_dfr
#' @importFrom rlang :=
#' @importFrom methods formalArgs

.model.glmm <- function(formula = NULL, data = NULL, control = NULL, ...) {

  control <- control[names(control) %in% methods::formalArgs(lme4::glmer)]

  m <- do.call(lme4::glmer, append(list(formula = formula, data = data), control))

  model_handler <- purrr::partial(.handler.glmm, object = m, formula = formula)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "lme4::glmm",
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.glmm <- function(object, data, formula = NULL, ..., .what = "model") {

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
    pred <- dplyr::tibble(
      prediction = stats::predict(object, data),
      truth = truth
    )
    return(pred)
  }

  if (.what == "estimates") {
    coefs <- stats::coef(object)
    estimates <- coefs %>%
      purrr::map2_dfr(names(coefs), function(cf, nam) {
        coefs_ <- cf %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(!! nam := rownames(cf)) %>%
          tidyr::pivot_longer(-dplyr::all_of(nam),
                              names_to = "term",
                              values_to = "estimate")
      })
    return(estimates)
  }

}
