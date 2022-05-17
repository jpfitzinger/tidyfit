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
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted..
#' @param ...  name-function pairs of models to be estimated. See 'Details'.
#' @param .cv type of 'rsample' cross validation procedure to use to determine optimal hyperparameter values. Default is \code{.cv = "none"}. See 'Details'.
#' @param .cv_args additional settings to pass to the 'rsample' cross validation function.
#' @param .control named list of arguments passed to model functions. This includes the hyperparameter vectors. Default values will be used when \code{.control=NULL}.
#' @param .weights optional name of column containing sample weights.
#' @param .mask optional vector of columns names to ignore. Can be useful when using 'y ~ .' formula setup.
#' @param .return_slices boolean. Should the output of individual slices be returned or only the final fit. Default is \code{.return_slices=FALSE}.
#' @return A \code{tibble}.
#' @author Johann Pfitzinger
#' @references
#'
#' @examples
#'
#' @export
#'
#' @seealso \code{tidypredict} method
#'
#' @importFrom magrittr `%>%`

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
  if (any(names(model_list)=="")) {
    warning("models should be name-function pairs. Names auto-assigned.")
    names(model_list)[names(model_list)==""] <- paste("MODEL", 1:sum(names(model_list)==""), sep = "_")
  }

  .cv <- match.arg(.cv)
  if (is.null(.cv_args)) .cv_args <- list()
  if (is.null(.control)) .control <- list()
  if (class(.control) != "list") stop("'.control' must be a 'list'.")
  if (class(.cv_args) != "list") stop("'.cv_args' must be a 'list'.")

  gr_vars <- group_vars(.data)

  df <- .data %>%
    do(result = .fit(., formula, model_list, .cv, .cv_args,
                     .control, .weights, gr_vars, .mask)) %>%
    unnest(result)

  # Select optimal hyperparameter setting
  if (.tune_each_group) {
    df <- df %>%
      group_by(across(all_of(gr_vars)))
  }

  df_slices <- df %>%
    filter(slice_id != "FULL") %>%
    group_by(model, grid_id, .add = T) %>%
    mutate(crit = mean(crit)) %>%
    ungroup(grid_id) %>%
    filter(crit == min(crit)) %>%
    select(-crit)

  if (.return_slices) {
    df <- df_slices %>%
      select(-grid_id)
  } else {
    df <- df_slices %>%
      ungroup %>%
      select(index, variable, grid_id, model) %>%
      left_join(df %>% ungroup %>% filter(slice_id == "FULL"), by = c("index", "variable", "grid_id", "model")) %>%
      select(-grid_id, -crit, -slice_id)
  }

  df <- df %>%
    group_by(model) %>%
    do(temp = select(., where(~!all(is.na(.)))))

  df <- df$temp %>%
    map_dfr(~nest(., model_info = -any_of(c(gr_vars, "variable", "beta", "model", "slice_id"))))

  df <- df %>%
    group_by(across(all_of(gr_vars)))

  attr(df, "formula") <- formula
  attr(df, "structure") <- list(mask = .mask, col_names = colnames(df))

  if (!is.null(.control$family)) {
    attr(df, "family") <- .control$family
  }

  return(df)

}
