#' @name predict.tidyfit.models
#' @title Predict using a \code{tidyfit.models} frame
#' @description The function generates predictions for all models in a \code{tidyfit.models} frame and outputs a tidy frame.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param newdata New values at which predictions are to made
#' @param ... currently not used
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
#'
#' @return A 'tibble'.
#'
#' @details The function uses the 'model_object' column in a \code{tidyfit.model} frame to return predictions using the \code{newdata} argument for each model.
#'
#' When the response variable is found in \code{newdata}, it is automatically included as a 'truth' column.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- dplyr::group_by(tidyfit::Factor_Industry_Returns, Industry)
#' fit <- regress(data, Return ~ ., m("lm"), .mask = "Date")
#' predict(fit, data)
#'
#' @seealso \code{\link{coef.tidyfit.models}}, \code{\link{residuals.tidyfit.models}} and \code{\link{fitted.tidyfit.models}}
#'
#' @export

predict.tidyfit.models <- function(object,
                                   newdata,
                                   ...,
                                   .keep_grid_id = FALSE) {

  gr_vars <- attr(object, "structure")$groups
  .mask <- attr(object, "structure")$mask
  .weights <- attr(object, "structure")$weights

  object <- .warn_and_remove_errors(object)
  object <- .nest_settings(object)

  get_predictions <- function(model_row) {
    if (is.null(model_row$newdata)) return(NULL)
    out <- model_row$model_object$predict(as.data.frame(model_row$newdata))
    if (is.null(out)) return(NULL)
    out <- out |>
      dplyr::mutate(model = model_row[["model"]], grid_id_ = model_row[["grid_id"]])
    return(dplyr::bind_cols(model_row[gr_vars], out))
  }

  newdata <- newdata |>
    dplyr::select(-any_of(c(.mask, .weights))) |>
    tidyr::nest(newdata = -any_of(gr_vars))
  if (ncol(newdata) == 1) {
    object <- object |>
      dplyr::bind_cols(newdata)
  } else {
    object <- object |>
      dplyr::left_join(newdata, by = gr_vars)
  }

  sel_cols <- c("settings", "estimator_fct", "size (MB)", "errors", "warnings", "messages")
  model_df <- object |>
    dplyr::select(-dplyr::any_of(sel_cols)) |>
    purrr::transpose()
  out <- purrr::map_dfr(model_df, get_predictions)

  if ("grid_id" %in% colnames(out)) {
    out <- dplyr::select(out, - "grid_id_")
  } else {
    out <- dplyr::rename(out, grid_id = "grid_id_")
  }

  col_ord <- c(gr_vars, "model", "grid_id", "slice_id", "class", "prediction", "truth")
  out <- out |>
    dplyr::relocate(any_of(col_ord))

  out <- out |>
    dplyr::group_by(across(any_of(c(gr_vars, "model")))) |>
    dplyr::mutate(nids = length(unique(.data$grid_id)))

  if (all(out$nids==1) & !.keep_grid_id) {
    out <- dplyr::select(out, - "grid_id")
  }
  out <- dplyr::select(out, - "nids")

  return(out)

}

