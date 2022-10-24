#' @name classify
#' @title Classification on tidy data
#' @description This function is a wrapper to fit many different types of linear
#' classification models on a (grouped) \code{tibble}.
#'
#' @details \code{classify} fits all models passed in \code{...} using the \code{\link{m}} function. The models can be passed as name-function pairs (e.g. \code{ols = m("lm")}) or without including a name.
#'
#' Hyperparameters are tuned automatically using the '.cv' and '.cv_args' arguments, or can be passed to \code{m()} (e.g. \code{lasso = m("lasso", lambda = 0.5)}). See the individual model functions (\code{?m()}) for an overview of hyperparameters.
#'
#' Cross validation is performed using the 'rsample' package with possible methods including
#'
#'  - 'initial_split' (simple train-test split)
#'  - 'initial_time_split' (train-test split with retained order)
#'  - 'vfold' (aka kfold cross validation)
#'  - 'loo' (leave-one-out)
#'  - 'rolling_origin' (generalized time series cross validation, e.g. rolling or expanding windows)
#'  - 'sliding_window', 'sliding_index', 'sliding_period' (specialized time series splits)
#'  - 'bootstraps'
#'
#' The negative log loss is used to validate performance in the cross validation.
#'
#' Note that arguments for weights are automatically passed to the functions by setting the '.weights' argument. Weights are also considered during cross validation by calculating weighted versions of the cross validation loss function.
#'
#' \code{classify} can handle both binomial and multinomial response distributions, however not all underlying methods are capable of handling a multinomial response.
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). The data frame can be grouped.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param ...  name-function pairs of models to be estimated. See 'Details'.
#' @param .cv type of 'rsample' cross validation procedure to use to determine optimal hyperparameter values. Default is \code{.cv = "none"}. See 'Details'.
#' @param .cv_args additional settings to pass to the 'rsample' cross validation function.
#' @param .weights optional name of column containing sample weights.
#' @param .mask optional vector of columns names to ignore. Can be useful when using 'y ~ .' formula syntax.
#' @param .return_slices logical. Should the output of individual cross validation slices be returned or only the final fit. Default is \code{.return_slices=FALSE}.
#' @param .tune_each_group logical. Should optimal hyperparameters be selected for each group or once across all groups. Default is \code{.tune_each_group=TRUE}.
#' @param .force_cv logical. Should models be evaluated across all cross validation slices, even if no hyperparameters are tuned. Default is \code{.force_cv=TRUE}.
#' @return A \code{tidyfit.models} frame containing model details for each group.
#'
#' The **'tidyfit.models' frame** consists of 4 different components:
#'
#'  1. A group of identifying columns (e.g. model name, data groups, grid IDs)
#'  2. A 'model_object' column, which contains the fitted model.
#'  3. A nested 'settings' column containing model arguments and hyperparameters
#'  4. Columns showing errors, warnings and messages (if applicable)
#'
#' Coefficients, predictions, fitted values or residuals can be accessed using the built-in \code{coef}, \code{predict}, \code{fitted} and \code{resid} methods. Note that all coefficients are transformed to ensure comparability across methods.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::mutate(data, Return = ifelse(Return > 0, 1, 0))
#' fit <- classify(data, Return ~ ., m("lasso", lambda = c(0.001, 0.1)), .mask = c("Date", "Industry"))
#'
#' # Print the models frame
#' tidyr::unnest(fit, settings)
#'
#' # View coefficients
#' coef(fit)
#'
#' @export
#'
#' @seealso \code{\link{regress}}, \code{\link{coef.tidyfit.models}} and \code{\link{predict.tidyfit.models}} method
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr expand_grid
#' @importFrom purrr map_dfr map transpose
#' @importFrom dplyr coalesce group_vars group_split
#' @importFrom progressr progressor
#' @importFrom rlang .data
#' @importFrom utils globalVariables

utils::globalVariables(c("."))

classify <- function(
    .data,
    formula,
    ...,
    .cv = c("none", "initial_split", "initial_time_split", "loo", "vfold",
            "rolling_origin", "sliding_index", "sliding_period", "sliding_window",
            "bootstraps"),
    .cv_args = NULL,
    .weights = NULL,
    .mask = NULL,
    .return_slices = FALSE,
    .tune_each_group = TRUE,
    .force_cv = FALSE
) {

  model_list <- list(...)
  .cv <- match.arg(.cv)
  if (is.null(.cv_args)) .cv_args <- list()
  if (!inherits(.cv_args, "list"))
    stop("'.cv_args' must be a 'list'.")

  # Checks
  if (length(model_list)==0)
    stop("provide at least one method.")

  # Prepare model names
  model_names <- names(model_list)
  if (is.null(model_names)) model_names <- rep(NA, length(model_list))
  model_names[model_names == ""] <- NA
  method_names <- sapply(model_list, function(mod) mod$model_object[[1]]$method)
  model_names <- dplyr::coalesce(model_names, method_names)
  names(model_list) <- model_names

  .check_method(method_names, "classify", message = TRUE)

  # Multinomial classification
  # Check if formula specified as two-column response
  multi_column <- grepl("cbind", deparse(formula))
  if (!multi_column) {
    response_var <- all.vars(formula)[1]
    response_lvls <- unique(.data[[response_var]])
    if (length(response_lvls) < 2)
      stop("response must contain at least 2 classes")
    if (length(response_lvls) > 2) {
      .check_method(method_names, "multinomial", message = TRUE)
    }
    .data[[response_var]] <- as.factor(.data[[response_var]])
  }

  model_df <- purrr::map_dfr(model_list, ~., .id = "model")
  model_df$model_object <- purrr::map(model_df$model_object, function(mod) {
    mod$formula <- formula
    mod$mode <- "classification"
    if (.force_cv) mod$cv <- TRUE
    mod
  })

  gr_vars <- dplyr::group_vars(.data)
  df_list <- dplyr::group_split(.data)
  df_list <- purrr::map(df_list,
                        ~.make_cross_val(
                          ., .cv, .cv_args,
                          gr_vars, .mask, .weights
                          ))
  eval_df <- tidyr::expand_grid(model_df, data = df_list)

  p <- progressr::progressor(nrow(eval_df))
  fit_progress <- function(row, ...) {
    out <- .fit_groups(row, ...)
    p()
    out
  }

  df <- eval_df %>%
    purrr::transpose() %>%
    map_dfr(function(row) fit_progress(row))

  df <- .post_process(df, .return_slices, .cv, .tune_each_group,
                      .mask, .weights, gr_vars)
  return(df)

}
