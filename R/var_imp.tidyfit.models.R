#' @name var_imp.tidyfit.models
#' @title Variable importance measures for a \code{tidyfit.models} frame
#' @description The function generates explanations for all models in a \code{tidyfit.models} frame and outputs a tidy frame.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param method the variable importance method used to create explanations. See 'Details' for possible options.
#' @param ... additional arguments passed to the importance method
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
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
#' var_imp(fit, method = "rel_weights")
#'
#' @seealso \code{\link{coef.tidyfit.models}}, \code{\link{residuals.tidyfit.models}} and \code{\link{fitted.tidyfit.models}}
#'
#' @export

var_imp.tidyfit.models <- function(object,
                                   method = NULL,
                                   ...,
                                   .keep_grid_id = FALSE) {

  object <- .warn_and_remove_errors(object)
  additional_args <- list(...)

  sel_cols <- c("settings", "estimator_fct", "size (MB)", "errors", "warnings", "messages")
  gr_vars <- attr(object, "structure")$groups
  out <- object %>%
    dplyr::select(-dplyr::any_of(sel_cols)) %>%
    dplyr::rename(grid_id_ = "grid_id") %>%
    dplyr::mutate(importance = purrr::map(.data$model_object, ~.$var_imp(method = method, additional_args = additional_args))) %>%
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

