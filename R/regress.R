#' @name regress
#' @title Linear regression on tidy data
#' @description This function is a wrapper to fit many different types of linear
#' regression models on a (grouped) \code{tibble}.
#'
#' @details \code{regress} fits all models passed in \code{...} using the \code{\link{m}} function. The models can be passed as name-function pairs (e.g. \code{ols = m("lm")}) or without including a name.
#'
#' Hyperparameters are tuned automatically using the '.cv' and '.cv_args' arguments, or can be passed to \code{m()} (e.g. \code{lasso = m("lasso", lambda = 0.5)}). See the individual model functions (\code{?m()}) for an overview of hyperparameters.
#'
#' Cross validation is performed using the 'rsample' package with possible methods including 'initial_split' (simple train-test split), 'initial_time_split' (train-test split with retained order),
#' 'vfold' (aka kfold) cross validation, 'loo', time series ('rolling_origin') and 'bootstraps' cross validation. \code{.cv = "rolling_origin"} implements either rolling or expanding window cross validation with 'rsample::rolling_origin'.
#' The mean squared error loss is used to validate performance in the cross validation.
#'
#' Note that arguments for weights are automatically passed to the functions by setting the '.weights' argument. Weights are also considered during cross validation by calculating weighted versions of the cross validation loss function.
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). The data frame can be grouped.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param ...  name-function pairs of models to be estimated. See 'Details'.
#' @param .cv type of 'rsample' cross validation procedure to use to determine optimal hyperparameter values. Default is \code{.cv = "none"}. See 'Details'.
#' @param .cv_args additional settings to pass to the 'rsample' cross validation function.
#' @param .weights optional name of column containing sample weights.
#' @param .mask optional vector of columns names to ignore. Can be useful when using 'y ~ .' formula setup.
#' @param .return_slices boolean. Should the output of individual cross validation slices be returned or only the final fit. Default is \code{.return_slices=FALSE}.
#' @param .tune_each_group boolean. Should optimal hyperparameters be selected for each group or once across all groups. Default is \code{.tune_each_group=TRUE}.
#' @param .force_cv boolean. Should models be evaluated across all cv slices, even if no hyperparameters are tuned. Default is \code{.force_cv=TRUE}.
#' @return A \code{tidyfit.models} frame containing model details for each group.
#'
#' The **models frame** consists of 3 different components:
#'
#'  1. A group of identifying columns (e.g. model name, data groups, grid IDs)
#'  2. A 'handler' column, which consists of partialized functions that contain all the necessary information to return (a) the model object itself with, (b) model coefficients or (c) predictions.
#'  3. A nested 'settings' column containing model arguments and hyperparameters
#'
#' Coefficients or predictions can be accessed using the built-in \code{coef} and \code{predict} methods. Note that all coefficients are transformed to ensure comparability across methods.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- regress(data, Return ~ ., m("lasso", lambda = c(0.001, 0.1)), .mask = c("Date", "Industry"))
#'
#' # Print the models frame
#' tidyr::unnest(fit, settings)
#'
#' # View coefficients
#' coef(fit)
#'
#' @export
#'
#' @seealso \code{\link{classify}}, \code{\link{coef.tidyfit.models}} and \code{\link{predict.tidyfit.models}} method
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest nest any_of
#' @importFrom tibble new_tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr group_vars group_by across all_of filter mutate ungroup select distinct left_join do select_if bind_rows coalesce
#' @importFrom rlang .data
#' @importFrom utils globalVariables

utils::globalVariables(c("."))

regress <- function(
  .data,
  formula,
  ...,
  .cv = c("none", "initial_split", "initial_time_split", "loo", "vfold",
          "rolling_origin", "sliding_index", "sliding_period", "sliding_window",
          "bootstraps"),
  .cv_args = NULL,
  .weights = NULL,
  .mask = NULL,
  .remove_dependent_features = FALSE,
  .return_slices = FALSE,
  .tune_each_group = TRUE,
  .force_cv = FALSE
) {

  model_list <- list(...)

  # Checks
  if (length(model_list)==0)
    stop("provide at least one method.")

  # Prepare model names
  model_names <- names(model_list)
  if (is.null(model_names)) model_names <- rep(NA, length(model_list))
  model_names <- sapply(model_names, function(nam) ifelse(nam == "", NA, nam))
  method_names <- sapply(model_list, function(m) m(.return_method_name = TRUE))
  model_names <- dplyr::coalesce(model_names, method_names)
  names(model_list) <- model_names

  if (.force_cv) {
    model_cv <- rep(T, length(model_list))
  } else {
    model_cv <- sapply(method_names, .check_method, "cv")
  }

  sapply(method_names, .check_method, "regress", message = TRUE)

  .cv <- match.arg(.cv)
  if (is.null(.cv_args)) .cv_args <- list()
  if (!inherits(.cv_args, "list")) stop("'.cv_args' must be a 'list'.")

  gr_vars <- dplyr::group_vars(.data)

  # Fit models
  df <- .data %>%
    do(result = .fit(., formula, model_list, .cv, .cv_args,
                     .weights, gr_vars, .mask, gaussian(),
                     .force_cv, .remove_dependent_features)) %>%
    tidyr::unnest(.data$result)

  if (!.return_slices & .cv == "none") {
    df <- df %>%
      dplyr::select(-.data$slice_id)
  }

  if (.cv != "none") {
    # Select optimal hyperparameter setting
    df_no_cv <- df %>%
      dplyr::filter(.data$model %in% model_names[!model_cv]) %>%
      dplyr::select(-.data$slice_id)

    df <- df %>%
      dplyr::filter(.data$model %in% model_names[model_cv])

    if (nrow(df) == 0) {
      df <- df_no_cv
    } else {
      if (.tune_each_group) {
        df <- df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(gr_vars)))
      }

      df_slices <- df %>%
        dplyr::filter(.data$slice_id != "FULL") %>%
        dplyr::group_by(.data$model, .data$grid_id, .add = TRUE) %>%
        dplyr::mutate(metric = mean(.data$metric)) %>%
        dplyr::ungroup(.data$grid_id) %>%
        dplyr::filter(.data$metric == min(.data$metric)) %>%
        dplyr::filter(.data$grid_id == unique(.data$grid_id)[1]) %>%
        dplyr::select(-.data$metric)

      if (.return_slices) {
        df <- df_slices %>%
          dplyr::bind_rows(df_no_cv)
      } else {
        df <- df_slices %>%
          dplyr::ungroup() %>%
          dplyr::select(!!gr_vars, .data$grid_id, .data$model) %>%
          dplyr::distinct() %>%
          dplyr::left_join(df %>% dplyr::ungroup() %>% dplyr::filter(.data$slice_id == "FULL"), by = c(gr_vars, "grid_id", "model")) %>%
          dplyr::bind_rows(df_no_cv) %>%
          dplyr::select(-.data$metric, -.data$slice_id)
      }
    }
  }

  col_ord <- c(gr_vars, "model", "estimator", "grid_id", "handler", "settings", "warnings", "messages")
  df <- dplyr::relocate(df, any_of(col_ord))

  df <- tibble::new_tibble(df, class = "tidyfit.models",
                           structure = list(groups = gr_vars,
                                            mask = .mask,
                                            weights = .weights))

  return(df)

}
