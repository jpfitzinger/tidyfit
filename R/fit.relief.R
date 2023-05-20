#' @name .fit.relief
#' @title ReliefF and RReliefF feature selection algorithm for \code{tidyfit}
#' @description Selects features for continuous or factor data using ReliefF on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' - estimator (*selection algorithm to use (default is 'ReliefFequalK')*)
#'
#' The ReliefF algorithm is estimated using the \code{CORElearn::attrEval} function. See \code{?attrEval} for more details.
#'
#' **Implementation**
#'
#' Use with \code{\link{regress}} for regression problems and with \code{\link{classify}} for classification problems. \code{coef} returns the score for each feature. Select the required number of features with the largest scores.
#'
#' The Relief objects have no \code{predict} and related methods.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' Robnik-Sikonja M, Savicky P (2021).
#' _CORElearn: Classification, Regression and Feature Evaluation_.
#' R package version 1.56.0, <https://CRAN.R-project.org/package=CORElearn>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Date, -Industry)
#'
#' # Stand-alone function
#' fit <- m("relief", Return ~ ., data)
#' coef(fit)
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("relief"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.mrmr}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.relief <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("relief cannot handle weights, weights are ignored")
  }

  estimator <- ifelse(self$mode=="classification", "ReliefFequalK", "RReliefFequalK")
  self$set_args(estimator = estimator, overwrite = FALSE)

  ctr <- self$args[names(self$args) %in% methods::formalArgs(CORElearn::attrEval)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(CORElearn::attrEval, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "CORElearn::attrEval"
  invisible(self)
}

.coef.numeric <- function(object, self = NULL, ...) {

  estimates <- dplyr::tibble(
    term = names(object),
    estimate = object
  )
  return(estimates)

}

.predict.numeric <- function(object, data, self, ...) {
  warning(paste0("No prediction method for type '", self$method, "'."))
  return(NULL)
}

.resid.numeric <- function(object, self, ...) {
  warning(paste0("No residual method for type '", self$method, "'."))
  return(NULL)
}

.fitted.numeric <- function(object, self, ...) {
  warning(paste0("No fitted method for type '", self$method, "'."))
  return(NULL)
}
