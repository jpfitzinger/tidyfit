#' @name tidyfit
#' @title Linear regression of classification on tidy data
#' @description This function is a wrapper to fit many different types of linear
#' regression or classification models on a (grouped) \code{tibble}.
#'
#' @details Cross validation is performed using the 'rsample' package with possible methods including 'vfold' (aka kfold) cross validation, 'loo' and time series ('ts') cross validation. \code{.cv = "ts"} implements either rolling or expanding window cross validation using 'rsample::rolling_sample'.
#'
#' Hyperparameter grids passed to \code{.control} are optimized using the MSE for regression problems, and Accuracy for classification problems.
#'
#' The method function passed to \code{...} must have a specific input and output format. The package includes a number of methods (see \code{m.lm}, \code{m.lasso}, \code{m.boost} for instance), but it is possible to pass custom functions here.
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). The data frame can be grouped.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param ...  name-function pairs of models to be estimated. See 'Details'.
#' @param .cv type of 'rsample' cross validation procedure to use to determine optimal hyperparameter values. Default is \code{.cv = "none"}. See 'Details'.
#' @param .cv_args additional settings to pass to the 'rsample' cross validation function.
#' @param .control named list of arguments passed to model functions. This includes the hyperparameter vectors. Default values will be used when \code{.control=NULL}.
#' @param .weights optional name of column containing sample weights.
#' @param .mask optional vector of columns names to ignore. Can be useful when using 'y ~ .' formula setup.
#' @param .return_slices boolean. Should the output of individual slices be returned or only the final fit. Default is \code{.return_slices=FALSE}.
#' @param .tune_each_group boolean. Should optimal hyperparameters be selected for each group or once across all groups. Default is \code{.tune_each_group=TRUE}.
#' @return A \code{tibble}.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#' data <- tidyfit::Factor_Industry_Returns
#' fit <- tidyfit(data, Return ~ ., lin_reg = m.lm, .mask = "Date")
#' fit
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest nest any_of
#' @importFrom purrr map_dfr
#' @importFrom dplyr group_vars group_by across all_of filter mutate ungroup select distinct left_join do select_if
#' @importFrom rlang .data
#' @importFrom utils globalVariables

utils::globalVariables(c("."))

tidyfit <- function(
  .data,
  formula,
  ...,
  .cv = c("none", "loo", "vfold", "ts"),
  .cv_args = list(v = 10),
  .control = NULL,
  .weights = NULL,
  .mask = NULL,
  .return_slices = FALSE,
  .tune_each_group = TRUE
) {

  model_list <- list(...)
  if (length(model_list)==0)
    stop("provide at least one method.")
  if (any(names(model_list)=="")) {
    warning("models should be name-function pairs. Names auto-assigned.")
    names(model_list)[names(model_list)==""] <- paste("MODEL", 1:sum(names(model_list)==""), sep = "_")
  }

  .cv <- match.arg(.cv)
  if (is.null(.cv_args)) .cv_args <- list()
  if (is.null(.control)) .control <- list()
  if (class(.control) != "list") stop("'.control' must be a 'list'.")
  if (class(.cv_args) != "list") stop("'.cv_args' must be a 'list'.")

  gr_vars <- dplyr::group_vars(.data)

  df <- .data %>%
    do(result = .fit(., formula, model_list, .cv, .cv_args,
                     .control, .weights, gr_vars, .mask)) %>%
    tidyr::unnest(.data$result)

  if (.cv == "none") {
    df <- df %>%
      dplyr::select(-.data$grid_id)

    if (!.return_slices) {
      df <- df %>%
        dplyr::select(-.data$slice_id)
    }
  } else {
    # Select optimal hyperparameter setting
    if (.tune_each_group) {
      df <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(gr_vars)))
    }

    df_slices <- df %>%
      dplyr::filter(.data$slice_id != "FULL") %>%
      dplyr::group_by(.data$model, .data$grid_id, .add = T) %>%
      dplyr::mutate(crit = mean(.data$crit)) %>%
      dplyr::ungroup(.data$grid_id) %>%
      dplyr::filter(.data$crit == min(.data$crit)) %>%
      dplyr::filter(.data$grid_id == unique(.data$grid_id)[1]) %>%
      dplyr::select(-.data$crit)

    if (.return_slices) {
      df <- df_slices %>%
        dplyr::select(-.data$grid_id)
    } else {
      df <- df_slices %>%
        dplyr::ungroup() %>%
        dplyr::select(!!gr_vars, .data$variable, .data$grid_id, .data$model) %>%
        dplyr::left_join(df %>% dplyr::ungroup() %>% dplyr::filter(.data$slice_id == "FULL"), by = c(gr_vars, "variable", "grid_id", "model")) %>%
        dplyr::select(-.data$grid_id, -.data$crit, -.data$slice_id)
    }
  }

  df <- df %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$model) %>%
    dplyr::do(temp = dplyr::select_if(., ~!all(is.na(.))))

  df <- df$temp %>%
    purrr::map_dfr(~tidyr::nest(., model_info = -tidyr::any_of(c(gr_vars, "variable", "beta", "model", "slice_id"))))

  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_vars)))

  attr(df, "formula") <- formula
  attr(df, "structure") <- list(mask = .mask, col_names = colnames(df))

  if (!is.null(.control$family)) {
    attr(df, "family") <- .control$family
  }

  return(df)

}
