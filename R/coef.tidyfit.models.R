#' @name coef.tidyfit.models
#' @title Extract coefficients from a \code{tidyfit.models} frame
#' @description The function extracts and prepares coefficients from all models in a \code{tidyfit.models} frame and outputs a tidy frame of estimates.
#'
#' @param object \code{model.frame} created using \code{\link{regress}}, \code{\link{classify}} or \code{\link{m}}
#' @param ... currently not used
#' @param .add_bootstrap_interval calculate bootstrap intervals for the parameters. See 'Details'.
#' @param .bootstrap_alpha confidence level used for the bootstrap interval. Default is \code{.bootstrap_alpha = 0.05}.
#' @param .keep_grid_id boolean. By default the grid ID column is dropped, if there is only one unique setting per model or group. \code{.keep_grid_id = TRUE} ensures that the column is never dropped.
#'
#' @return A 'tibble'.
#'
#' @details The function uses the 'model_object' column in a \code{tidyfit.model} frame to return a data frame of estimated coefficients.
#'
#' Results are 'tidied' using \code{broom::tidy} whenever possible.
#'
#' All coefficients are transformed to ensure statistical comparability. For instance, standardized coefficients are always transformed back to the original data scale, naming conventions are harmonized etc.
#'
#' ### Bootstrap intervals
#'
#' Bootstrap intervals can be calculated using \code{rsample::int_pctl}. Only set \code{.add_bootstrap_interval = TRUE} if you are using \code{.cv = "bootstraps"} in combination with \code{.return_slices = TRUE} to generate the model frame.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{predict.tidyfit.models}}, \code{\link{fitted.tidyfit.models}} and \code{\link{residuals.tidyfit.models}}
#'
#' @export

#' @importFrom purrr map2
#' @importFrom dplyr distinct summarise

coef.tidyfit.models <- function(
    object,
    ...,
    .add_bootstrap_interval = FALSE,
    .bootstrap_alpha = 0.05,
    .keep_grid_id = FALSE) {

  sel_cols <- c("settings", "estimator", "size (MB)", "errors", "warnings", "messages")
  gr_vars <- attr(object, "structure")$groups
  out <- object %>%
    dplyr::select(-dplyr::any_of(sel_cols)) %>%
    dplyr::rename(grid_id_ = .data$grid_id) %>%
    dplyr::mutate(coefs = purrr::map(.data$model_object, ~.$coef())) %>%
    dplyr::select(-.data$model_object) %>%
    tidyr::unnest(.data$coefs)

  if ("grid_id" %in% colnames(out)) {
    out <- dplyr::select(out, -.data$grid_id_)
  } else {
    out <- dplyr::rename(out, grid_id = .data$grid_id_)
  }

  out <- out %>%
    dplyr::group_by(across(any_of(c(gr_vars, "model")))) %>%
    dplyr::mutate(nids = length(unique(.data$grid_id)))

  if (.add_bootstrap_interval) {
    if (!"slice_id" %in% colnames(out))
      stop("only use '.add_bootstrap_interval = TRUE' if '.return_slices = TRUE'")
    if (!any(grepl("Bootstrap", out$slice_id)))
      stop("only use '.add_bootstrap_interval = TRUE' if '.cv = \"bootstraps\"'")
    intervals <- out %>%
      dplyr::group_by(.data$grid_id, .add = TRUE) %>%
      dplyr::do(interval = .make_rsample_bootstraps(.)) %>%
      tidyr::unnest(.data$interval)
    out <- out %>%
      dplyr::select(-.data$estimate, -.data$slice_id) %>%
      dplyr::distinct() %>%
      dplyr::left_join(intervals, by = c(gr_vars, "model", "grid_id", "term"))
  }

  if (all(out$nids==1) & !.keep_grid_id) {
    out <- dplyr::select(out, -.data$grid_id)
  }
  out <- dplyr::select(out, -.data$nids)

  col_ord <- c(gr_vars, "model", "term", "class", "estimate", "grid_id", "slice_id")
  out <- out %>%
    tidyr::nest(model_info = -dplyr::any_of(col_ord)) %>%
    dplyr::relocate(any_of(col_ord))

  # Remove backticks from names
  out <- out %>%
    mutate(term = gsub("`", "", .data$term))

  return(out)

}

