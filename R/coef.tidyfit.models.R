#' @name coef.tidyfit.models
#' @title Extract coefficients from a \code{tidyfit.models} frame
#' @description The function extracts and prepares coefficients from all models in a \code{tidyfit.models} frame and outputs a tidy frame of estimates.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param ... currently not used
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
#'
#' @return A 'tibble'.
#'
#' @details The function uses the 'handler' column in a \code{tidyfit.model} frame to return a tibble of estimated coefficients.
#'
#' Results are 'tidied' using \code{broom::tidy} whenever possible.
#'
#' All coefficients are transformed to ensure statistical comparability. For instance, standardized coefficients are always transformed back to the original data scale, naming conventions are harmonized etc.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{predict.tidyfit.models}}
#'
#' @export

#' @importFrom purrr map2_dfr

coef.tidyfit.models <- function(object, ..., .keep_grid_id = FALSE) {

  sel_cols <- c("settings", "estimator")
  gr_vars <- attr(object, "structure")$groups
  out <- object %>%
    dplyr::select(-dplyr::any_of(sel_cols)) %>%
    dplyr::rename(grid_id_ = .data$grid_id) %>%
    dplyr::mutate(fit = purrr::map2(.data$handler, .data$grid_id_,
                                    function(handler, grid_id_) handler(.what = "estimates", selected_id = grid_id_))) %>%
    dplyr::select(-.data$handler) %>%
    tidyr::unnest(.data$fit)

  if ("grid_id" %in% colnames(out)) {
    out <- dplyr::select(out, -.data$grid_id_)
  } else {
    out <- dplyr::rename(out, grid_id = .data$grid_id_)
  }

  col_ord <- c(gr_vars, "model", "term", "class", "estimate", "grid_id")
  out <- out %>%
    tidyr::nest(model_info = -dplyr::any_of(col_ord)) %>%
    dplyr::relocate(any_of(col_ord))

  out <- out %>%
    dplyr::group_by(across(any_of(c(gr_vars, "model")))) %>%
    dplyr::mutate(nids = length(unique(grid_id)))

  if (all(out$nids==1) & !.keep_grid_id) {
    out <- dplyr::select(out, -.data$grid_id)
  }
  out <- dplyr::select(out, -.data$nids)

  return(out)

}

