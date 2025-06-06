#' @name fitted.tidyfit.models
#' @title Obtain fitted values from models in a \code{tidyfit.models} frame
#' @description The function generates fitted values for all models in a \code{tidyfit.models} frame and outputs a tidy frame.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param ... currently not used
#'
#' @return A 'tibble'.
#'
#' @details The function uses the 'model_object' column in a \code{tidyfit.model} frame to return fitted values for each model.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- dplyr::group_by(tidyfit::Factor_Industry_Returns, Industry)
#' fit <- regress(data, Return ~ ., m("lm"), .mask = "Date")
#' fitted(fit)
#'
#' @seealso \code{\link{coef.tidyfit.models}}, \code{\link{predict.tidyfit.models}} and \code{\link{residuals.tidyfit.models}}
#'
#' @export

fitted.tidyfit.models <- function(object, ...) {

  gr_vars <- attr(object, "structure")$groups
  .mask <- attr(object, "structure")$mask
  .weights <- attr(object, "structure")$weights

  object <- .warn_and_remove_errors(object)
  object <- .nest_settings(object)

  sel_cols <- c("settings", "estimator_fct", "size (MB)", "errors", "warnings", "messages")
  out <- object |>
    dplyr::select(-dplyr::any_of(sel_cols)) |>
    dplyr::mutate(fitted = purrr::map(.data$model_object, ~.$fitted())) |>
    dplyr::select(- "model_object") |>
    tidyr::unnest("fitted")

  col_ord <- c(gr_vars, "model", "grid_id", "slice_id", "class", "fitted")
  out <- out |>
    dplyr::relocate(any_of(col_ord))

  out <- out |>
    dplyr::group_by(across(any_of(c(gr_vars, "model")))) |>
    dplyr::mutate(nids = length(unique(.data$grid_id)))

  if (all(out$nids==1)) {
    out <- dplyr::select(out, - "grid_id")
  }
  out <- dplyr::select(out, - "nids")

  return(out)

}

