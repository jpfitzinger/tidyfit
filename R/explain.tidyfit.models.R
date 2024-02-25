#' @name explain.tidyfit.models
#' @title An interface for variable importance measures for a fitted tidyfit.models frames
#' @description A generic method for calculating XAI and variable importance methods for tidyfit.models frames.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param use_package the package to use to calculate variable importance. See 'Details' for possible options.
#' @param use_method the method from 'use_package' that should be used to calculate variable importance.
#' @param ... additional arguments passed to the importance method
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
#'
#' @return A 'tibble'.
#'
#' @details **WARNING**
#' This function is currently in an experimental stage.
#'
#' The function uses the 'model_object' column in a \code{tidyfit.model} frame to return variable importance measures for each model.
#'
#' **Possible packages and methods include:**
#'
#' ### \code{sensitivity} package:
#'
#' The package provides methods to assess variable importance in linear regressions ('lm') and classifications ('glm').
#'
#' *Usage:* \code{use_package="sensitivity"}
#' *Methods:*
#'
#' * "lmg" (Shapley regression),
#' * "pmvd" (Proportional marginal variance decomposition),
#' * "src" (standardized regression coefficients),
#' * "pcc" (partial correlation coefficients),
#' * "johnson" (Johnson indices)
#'
#' See \code{?sensitivity::lmg} for more information and additional arguments.
#'
#' ### \code{iml} package:
#'
#' Integration with iml is currently in progress. The methods can be used for 'nnet', 'rf', 'lasso', 'enet', 'ridge', 'adalasso', 'glm' and 'lm'.
#'
#' *Usage:* \code{use_package="iml"}
#' *Methods:*
#'
#' * "Shapley" (SHAP values)
#' * "LocalModel" (LIME)
#' * "FeatureImp" (Permutation-based feature importance)
#'
#' The argument 'which_rows' (vector of integer indexes) can be used to explain specific rows in the data set for Shapley and LocalModel methods.
#'
#' ### \code{randomForest} package:
#'
#' This uses the native importance method of the randomForest package and can be used with 'rf' and 'quantile_rf' regression and classification.
#'
#' *Usage:* \code{use_package="randomForest"}
#' *Methods:*
#'
#' * "mean_decrease_accuracy"
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- dplyr::group_by(tidyfit::Factor_Industry_Returns, Industry)
#' fit <- regress(data, Return ~ ., m("lm"), .mask = "Date")
#' explain(fit, use_package = "sensitivity", use_method = "src")
#'
#' data <- dplyr::filter(tidyfit::Factor_Industry_Returns, Industry == Industry[1])
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' explain(fit, use_package = "iml", use_method = "Shapley", which_rows = c(1))
#'
#' @references
#'
#' Molnar C, Bischl B, Casalicchio G (2018). “iml: An R package for Interpretable Machine Learning.” _JOSS_,
#' *3*(26), 786. <doi:10.21105/joss.00786>.
#'
#' Iooss B, Veiga SD, Janon A, Pujol G, Broto wcfB, Boumhaout K, Clouvel L, Delage T, Amri RE, Fruth J, Gilquin
#' L, Guillaume J, Herin M, Idrissi MI, Le Gratiet L, Lemaitre P, Marrel A, Meynaoui A, Nelson BL, Monari F,
#' Oomen R, Rakovec O, Ramos B, Rochet P, Roustant O, Sarazin G, Song E, Staum J, Sueur R, Touati T, Verges V,
#' Weber F (2024). _sensitivity: Global Sensitivity Analysis of Model Outputs and Importance Measures_. R
#' package version 1.30.0, <https://CRAN.R-project.org/package=sensitivity>.
#'
#' A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.
#'
#' @export

explain.tidyfit.models <- function(object,
                                   use_package = NULL,
                                   use_method = NULL,
                                   ...,
                                   .keep_grid_id = FALSE) {

  object <- .warn_and_remove_errors(object)
  additional_args <- list(...)

  get_explanation <- function(model) {
    model$explain(use_package = use_package, use_method = use_method, additional_args = additional_args)
  }

  sel_cols <- c("settings", "estimator_fct", "size (MB)", "errors", "warnings", "messages")
  gr_vars <- attr(object, "structure")$groups
  model_df <- object %>%
    dplyr::select(-dplyr::any_of(sel_cols)) %>%
    dplyr::rename(grid_id_ = "grid_id")
  explanation_df <- purrr::map(model_df$model_object, get_explanation)
  out <- model_df %>%
    dplyr::mutate(importance = explanation_df) %>%
    dplyr::select(-"model_object") %>%
    tidyr::unnest("importance")

  if ("grid_id" %in% colnames(out)) {
    out <- dplyr::select(out, -"grid_id_")
  } else {
    out <- dplyr::rename(out, grid_id = "grid_id_")
  }

  out <- out %>%
    dplyr::group_by(across(any_of(c(gr_vars, "model")))) %>%
    dplyr::mutate(nids = length(unique(.data$grid_id)))

  if (all(out$nids==1) & !.keep_grid_id) {
    out <- dplyr::select(out, - "grid_id")
  }
  out <- dplyr::select(out, - "nids")

  col_ord <- c(gr_vars, "model", "term", "class", "importance", "grid_id", "slice_id")
  out <- out %>%
    dplyr::relocate(any_of(col_ord))

  # Remove backticks from names
  out <- out %>%
    mutate(term = gsub("`", "", .data$term))

  return(out)

}

