#' @name tidypredict
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
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest nest all_of
#' @importFrom dplyr group_by across
#' @importFrom purrr map_dfr

tidypredict <- function(fit, data) {

  gr_vars <- group_vars(fit)
  data <- data %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(gr_vars)))

  df <- data %>%
    do(result = .predict(., fit, gr_vars)) %>%
    tidyr::unnest(result)

  df <- df %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(gr_vars)))

  return(df)

}
