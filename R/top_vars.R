#' @name top_vars
#' @title Select the top \code{n} features from a \code{tidyfit.models} frame
#' @description The function returns the names of the top features from all models in a \code{tidyfit.models} frame in a tidy frame.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param n maximum number of top features to select. Default \code{n = 1}.
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
#'
#' @return A 'tibble'.
#'
#' @details The function selects \code{n} features for each method in a \code{tidyfit.models} frame that has an appropriate feature selection method.
#'
#' A warning will be issued for all methods that do not have a feature selection method.
#'
#' If a model contains fewer features than \code{n}, all features in the model will be selected.
#'
#' If a model contains more features than \code{n}, and a parameter is missing or set incorrectly that limits the number of features (e.g. \code{pmax} in glmnet or \code{nvmax} in bestglm), the function will attempt to refit the model with a limit on the number of features.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' top_vars(fit)
#'
#' @seealso \code{\link{explain.tidyfit.models}} and \code{\link{coef.tidyfit.models}}
#'
#' @export

top_vars <- function(object, n, .keep_grid_id) {
  UseMethod("top_vars")
}

.top_vars <- function(object, ...) {
  UseMethod(".top_vars")
}
