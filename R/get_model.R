#' @name get_model
#' @title Get a fitted model from a tidyfit.models frame
#' @description Returns a single fitted model object produced by the underlying fitting algorithm from a tidyfit.models frame based on a given row number.
#'
#' @details 
#'
#' This method is a utility to return the object fitted by the underlying algorithm. For instance, when \code{m("lm")} is used to create the tidyfit.models frame, the returned object is of class "lm".
#'
#' @param df a tidyfit.models frame created using m(), regress(), classify() and similar methods
#' @param ... arguments passed to \code{dplyr::filter} to filter row in 'df' for which the model should be returned. filters can also include columns nested in \code{df$settings}.
#' @param .first_row should the first row be returned if the (filtered) df contains multiple rows
#' @return An object of the class associated with the underlying fitting algorithm
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data("mtcars")
#' 
#' # fit separate models for transmission types
#' mtcars <- dplyr::group_by(mtcars, am)
#'
#' fit <- regress(mtcars, mpg ~ ., m("lm"))
#' 
#' # get the model for single row
#' summary(get_model(fit, am == 0))
#' 
#' # get model by row number
#' summary(get_model(fit, dplyr::row_number() == 2))
#'
#' @seealso \code{\link{get_tidyFit}} method
#' 
#' @export

get_model <- function(df, ..., .first_row = TRUE) {
  model <- .get_selected_row(df, .first_row, ...)
  if (is.null(model$object))
    stop("no model fitted for the selected row. check errors.", call. = FALSE)
  return(model$object)
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
#' @param ... arguments passed to \code{dplyr::filter} to filter row in 'df' for which the model should be returned. filters can also include columns nested in \code{df$settings}.
#' @param .first_row should the first row be returned if the (filtered) df contains multiple rows
#' @return An object of the class associated with the underlying fitting algorithm
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data("mtcars")
#' 
#' # fit separate models for transmission types
#' mtcars <- dplyr::group_by(mtcars, am)
#'
#' fit <- regress(mtcars, mpg ~ ., m("lm"))
#' 
#' # get the model for single row
#' get_tidyFit(fit, am == 0)
#' 
#' # get model by row number
#' get_tidyFit(fit, dplyr::row_number() == 2)
#'
#' @seealso \code{\link{get_model}} method
#' 
#' @export

get_tidyFit <- function(df, ..., .first_row = TRUE) {
  mod <- .get_selected_row(df, .first_row, ...)
  return(mod)
}

.get_selected_row <- function(df, .first_row, ...) {
  if (!methods::is(df, 'tidyfit.models'))
    stop("'df' must be a tidyfit.models frame generated using 'm()', 'regress()', 'classify()' etc.", call. = FALSE)
  if (!"model_object" %in% colnames(df))
    stop("'model_object' column is missing in 'df'", call. = FALSE)
  if (length(dplyr::quos(...)) > 0) {
    df_subset <- dplyr::filter(tidyr::unnest(df, dplyr::any_of("settings")), ...)
    if ((nrow(df_subset) > 1) & .first_row) {
      warning("filters passed to '...' return more than 1 row. returning the first row.", call. = FALSE)
    } else if (nrow(df_subset) == 0) {
      stop("filters returned no rows.", call. = FALSE)
    }
  } else {
    df_subset <- df
  }
  if ((nrow(df_subset) > 1) & !.first_row) {
    stop("filters passed to '...' return more than 1 row.", call. = FALSE)
  }
  model <- df_subset$model_object[[1]]
  return(model)
}