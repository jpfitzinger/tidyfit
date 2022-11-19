#' @name .model.cor
#' @title Pearson's correlation for \code{tidyfit}
#' @description Calculates Pearson's correlation coefficient on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{stats::cor.test}. See \code{?cor.test} for more details.
#'
#' **Implementation**
#'
#' Results can be viewed using \code{coef}.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("cor", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("cor"), .mask = c("Date", "Industry"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.model.chisq}} and \code{\link{m}} methods
#'
#' @importFrom stats cor.test
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.model.cor <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("cor cannot handle weights, weights are ignored")
  }

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]
  var_names <- colnames(x)
  eval_fun_ <- function(...) {
    args <- list(...)
    tests <- purrr::map(var_names, function(nam) cor.test(x[,nam], y))
    names(tests) <- var_names
    class(tests) <- "custom.test"
    tests
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- eval_fun()
  .store_on_self(self, res)
  self$estimator <- "stats::cor.test"
  invisible(self)
}

.coef.custom.test <- function(object, self = NULL, ...) {

  estimates <- purrr::map_dfr(object, broom::tidy, .id = "term")
  if (!"estimate" %in% colnames(estimates)) {
    estimates <- dplyr::mutate(estimates, estimate = NA)
  }

  return(estimates)

}

.predict.custom.test <- function(object, self, ...) {
  warning(paste0("No prediction method for type '", self$method, "'."))
  return(NULL)
}

.resid.custom.test <- function(object, self, ...) {
  warning(paste0("No residual method for type '", self$method, "'."))
  return(NULL)
}

.fitted.custom.test <- function(object, self, ...) {
  warning(paste0("No fitted method for type '", self$method, "'."))
  return(NULL)
}
