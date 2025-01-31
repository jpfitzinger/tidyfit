#' @name .fit.mrmr
#' @title Minimum redundancy, maximum relevance feature selection for \code{tidyfit}
#' @description Selects features for continuous or (ordered) factor data using MRMR on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' - feature_count (*number of features to select*)
#' - solution_count (*ensemble size*)
#'
#' The MRMR algorithm is estimated using the \code{mRMRe::mRMR.ensemble} function. See \code{?mRMR.ensemble} for more details.
#'
#' **Implementation**
#'
#' Use with \code{\link{regress}} for regression problems and with \code{\link{classify}} for classification problems. The selected features can be obtained using \code{coef}.
#'
#' The MRMR objects have no \code{predict} and related methods.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' De Jay N, Papillon-Cavanagh S, Olsen C, Bontempi G and Haibe-Kains B (2012).
#' _mRMRe: an R package for parallelized mRMR ensemble feature selection_.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, SMB, HML, RMW, CMA, Return)
#'
#' \dontrun{
#' fit <- m("mrmr", Return ~ ., data, feature_count = 2)
#'
#' # Retrieve selected features
#' coef(fit)
#' }
#'
#' @seealso \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.mrmr <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("mrmr cannot handle weights, weights are ignored", call. = FALSE)
  }

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  if (self$mode == "classification") {
    y <- factor(y, ordered = TRUE)
  }

  self$set_args(solution_count = 1, overwrite = FALSE)
  self$set_args(target_indices = 1, overwrite = TRUE)
  dat <- mRMRe::mRMR.data(data.frame(y, x, check.names = FALSE))

  ctr <- self$args[names(self$args) %in% c(methods::formalArgs(mRMRe::mRMR.ensemble),
                                           "target_indices", "method", "levels", "continuous_estimator", "bootstrap_count")]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(mRMRe::mRMR.ensemble, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(data = dat), ctr))
  .store_on_self(self, res)
  invisible(self)
}

.coef.mRMRe.Filter <- function(object, self = NULL, ...) {

  selected_features <- sort(table(object@filters[[1]]), decreasing = TRUE)[1:self$args$feature_count]
  estimates <- dplyr::tibble(
    term = object@feature_names[as.numeric(names(selected_features))],
    estimate = as.numeric(selected_features)
  )
  return(estimates)

}

.predict.mRMRe.Filter <- function(object, data, self, ...) {
  warning(paste0("No prediction method for type '", self$method, "'."), call. = FALSE)
  return(NULL)
}

.resid.mRMRe.Filter <- function(object, self, ...) {
  warning(paste0("No residual method for type '", self$method, "'."), call. = FALSE)
  return(NULL)
}

.fitted.mRMRe.Filter <- function(object, self, ...) {
  warning(paste0("No fitted method for type '", self$method, "'."), call. = FALSE)
  return(NULL)
}

