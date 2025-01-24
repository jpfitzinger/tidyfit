#' @name top_vars.tidyfit.models
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

#' @importFrom purrr map2
#' @importFrom dplyr distinct summarise

top_vars.tidyfit.models <- function(
    object,
    n = 1,
    .keep_grid_id = FALSE) {

  object <- .warn_and_remove_errors(object)

  n <- as.integer(n)
  if (n <= 0)
    stop("'n' must be larger than zero.", call. = FALSE)

  get_top_vars <- function(model) {
    model$top_vars(n = n)
  }

  sel_cols <- c("settings", "estimator_fct", "size (MB)", "errors", "warnings", "messages")
  gr_vars <- attr(object, "structure")$groups
  model_df <- object %>%
    dplyr::select(-dplyr::any_of(sel_cols)) %>%
    dplyr::rename(grid_id_ = "grid_id")
  top_vars_df <- purrr::map(model_df$model_object, get_top_vars)
  out <- model_df %>%
    dplyr::mutate(top_vars = top_vars_df) %>%
    dplyr::select(-"model_object") %>%
    tidyr::unnest("top_vars")

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

  col_ord <- c(gr_vars, "model", "term", "grid_id", "slice_id")
  out <- out %>%
    dplyr::relocate(any_of(col_ord))

  # Remove backticks from names
  out <- out %>%
    mutate(term = gsub("`", "", .data$term))

  return(out)

}

