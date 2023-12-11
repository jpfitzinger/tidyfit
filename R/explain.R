#' @name explain
#' @title Explain details of a fitted tidyfit.models frame
#' @description A generic method for calculating XAI and variable importance methods for tidyfit.models frames.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param method the variable importance method used to create explanations. See 'Details' for possible options.
#' @param ... additional arguments passed to the importance method
#'
#' @return A 'tibble'.
#'
#' @details The function uses the 'model_object' column in a \code{tidyfit.model} frame to return variable importance measures for each model.
#'
#' **Possible methods include:**
#'
#' ### Linear regression ('lm'):
#'
#' * 'shapley_reg' for Shapley regression (default). The method uses the 'lmg' algorithm implemented in the `relaimpo`-package. Other algorithms can be applied by explicitly passing 'type' to `...`, which is passed to `relaimpo::calc.relimp`.
#' * 'rel_weights' for relative weights. The method uses the 'genizi' algorithm implemented in the `relaimpo`-package.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- dplyr::group_by(tidyfit::Factor_Industry_Returns, Industry)
#' fit <- regress(data, Return ~ ., m("lm"), .mask = "Date")
#' explain(fit, method = "rel_weights")
#'
#' @export
#'
#' @importFrom generics explain

explain <- function(object, method, ...) {
  UseMethod("explain")
}

.explain <- function(object, ...) {
  UseMethod(".explain")
}
