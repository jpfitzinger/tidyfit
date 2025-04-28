#' @name get_model
#' @title Get a fitted model from a tidyfit.models frame
#' @description Returns a single fitted model object produced by the underlying fitting algorithm from a tidyfit.models frame based on a given row number.
#'
#' @details 
#'
#' This method is a utility to return the object fitted by the underlying algorithm. For instance, when \code{m("lm")} is used to create the tidyfit.models frame, the returned object is of class "lm".
#'
#' @param df a tidyfit.models frame created using m(), regress(), classify() and similar methods
#' @param row an integer index of the row number in 'df' for which the model should be returned
#' @return An object of the class associated with the underyling fitting algorithm
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("lm", Return ~ ., data)
#' summary(get_model(fit))
#'
#' @seealso \code{\link{get_tidyFit}} method
#' 
#' @export

get_model <- function(df, row = 1) {
  mod <- .get_selected_row(df, row)
  if (is.null(mod$object))
    stop("no model fitted for the selected row. check errors.")
  return(mod$object)
}

#' @name get_tidyFit
#' @title Get a tidyFit model from a tidyfit.models frame
#' @description Returns a single tidyFit object from a tidyfit.models frame based on a given row number.
#'
#' @details 
#'
#' This method is a utility to return the tidyFit object from a row index of the tidyfit.models frame. The tidyFit object contains the fitted model and several additional objects necessary to reproduce the analysis or refit the model on new data.
#'
#' @param df a tidyfit.models frame created using m(), regress(), classify() and similar methods
#' @param row an integer index of the row number in 'df' for which the model should be returned
#' @return An object of the class associated with the underyling fitting algorithm
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("lm", Return ~ ., data)
#' model <- get_tidyFit(fit)
#' 
#' # Refit
#' model$fit(data)
#'
#' @seealso \code{\link{get_model}} method
#' 
#' @export

get_tidyFit <- function(df, row = 1) {
  mod <- .get_selected_row(df, row)
  return(mod)
}

.get_selected_row <- function(df, row) {
  if (!is.numeric(row))
    stop("argument 'row' must be a number")
  if (!is(df, 'tidyfit.models'))
    stop("'df' must be a tidyfit.models frame generated using 'm()', 'regress()', 'classify()' etc.")
  if ((row > nrow(df)) | (row < 1L))
    stop("invalid value provided for 'row'")
  if (!"model_object" %in% colnames(df))
    stop("'model_object' column is missing in 'df'")
  mod <- df$model_object[[row]]
  return(mod)
}