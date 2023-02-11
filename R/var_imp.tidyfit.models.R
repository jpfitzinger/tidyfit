#' @name var_imp.tidyfit.models
#' @title Estimate variable importance from a \code{tidyfit.models} frame
#' @description The function estimates relative feature importance from all models in a \code{tidyfit.models} frame and outputs a tidy frame. 
#' 
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param ... currently not used
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
#'
#' @return A 'tibble'.
#'
#' @details Relative feature importance is estimated via a model agnostic method from the \code{iml} package (evaluating model error increase for feature permutation). This wrapper uses
#' \code{compare="ratio"} of \code{loss="mae"} for regression models, and \code{compare="difference"} of \code{loss="ce"} for classification models.
#'  
#' 
#' @author Phil Holzmeister
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' var_imp(fit)
#'
#' @seealso \code{\link{predict.tidyfit.models}}, \code{\link{fitted.tidyfit.models}}, \code{\link{coef.tidyfit.models}} and \code{\link{residuals.tidyfit.models}}
#'
#' @export

var_imp.tidyfit.models <- function(
    object,
    ...,
    .keep_grid_id = FALSE) {

  object <- .warn_and_remove_errors(object)

  sel_cols <- c("settings", "estimator_fct", "size (MB)", "errors", "warnings", "messages")
  gr_vars <- attr(object, "structure")$groups
  out <- object |>
    dplyr::select(-dplyr::any_of(sel_cols)) |>
    dplyr::rename(grid_id_ = "grid_id") |>
    dplyr::mutate(varimp = purrr::map(.data$model_object, ~.$varimp())) |>
    dplyr::select(-"model_object") |>
    tidyr::unnest("varimp")

  if ("grid_id" %in% colnames(out)) {
    out <- dplyr::select(out, -"grid_id_")
  } else {
    out <- dplyr::rename(out, grid_id = "grid_id_")
  }

  out <- out |>
    dplyr::group_by(across(.cols = any_of(c(gr_vars, "model")))) |>
    dplyr::mutate(nids = length(unique(.data$grid_id)))

  if (all(out$nids==1) & !.keep_grid_id) {
    out <- dplyr::select(out, - "grid_id")
  }
  out <- dplyr::select(out, - "nids")

  col_ord <- c(gr_vars, "model", "term", "class", "estimate", "grid_id", "slice_id")
  out <- out |>
    tidyr::nest(model_info = -dplyr::any_of(col_ord)) |>
    dplyr::relocate(any_of(col_ord))

  # Remove backticks from names
  out <- out |>
    mutate(term = gsub("`", "", .data$term))

  return(out)

}

