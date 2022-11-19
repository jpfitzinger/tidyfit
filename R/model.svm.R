#' @name .model.svm
#' @title Support vector regression or classification for \code{tidyfit}
#' @description Fits a support vector regression or classification on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} or \code{\link{classify}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details  **Hyperparameters:**
#'
#' - cost (*cost of constraint violation*)
#' - epsilon (*epsilon in the insensitive-loss function*)
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{e1071::svm}. See \code{?svm} for more details.
#'
#' **Implementation**
#'
#' The default value for the \code{kernel} argument is set to 'linear'. If set to a different value, no coefficients will be returned.
#'
#' @author Johann Pfitzinger
#'
#' @references
#'  Meyer D, Dimitriadou E, Hornik K, Weingessel A, Leisch F (2022).
#'  _e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien_.
#'  R package version 1.7-12, <https://CRAN.R-project.org/package=e1071>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#'
#' # Stand-alone function
#' fit <- m("svm", Return ~ `Mkt-RF` + HML + SMB, data, cost = 0.1)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("svm", cost = 0.1),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.boost}}, \code{\link{.model.lasso}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.model.svm <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("svm cannot handle weights, weights are ignored")
  }

  self$set_args(kernel = "linear", overwrite = FALSE)
  self$set_args(probability = TRUE)

  formal_args <- c("scale", "type", "kernel", "degree", "gamma", "coef0", "cost",
                   "nu", "class.weights", "cachesize", "tolerance", "epsilon",
                   "shrinking", "cross", "probability", "fitted", "subset",
                   "na.action")
  ctr <- self$args[names(self$args) %in% formal_args]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(e1071::svm, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "e1071::svm"
  invisible(self)
}
