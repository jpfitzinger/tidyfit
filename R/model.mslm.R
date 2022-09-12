#' @name .model.mslm
#' @title Markov-Switching Regression for \code{tidyfit}
#' @description Fits a Markov-Switching regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param self a tidyFit R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted tidyFit class model.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{MSwM::msmFit}. Note that only the regression method with 'lm' is implemented at this stage.
#'
#' An argument \code{index_col} can be passed, which allows a custom index to be added to \code{coef(m("mslm"))} (e.g. a date index).
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Industry)
#'
#' ctr <- list(maxiter = 10, parallelization = FALSE)
#'
#' # Stand-alone function
#' fit <- m("mslm", Return ~ ., data, index_col = "Date", k = 2, control = ctr)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ .,
#'                m("mslm", index_col = "Date", k = 2, control = ctr))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.model.tvp}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats lm
#' @importFrom methods formalArgs

.model.mslm <- function(
    self,
    data = NULL
) {
  data <- data.frame(data, check.names = FALSE)
  idx_col <- self$args$index_col
  if (is.null(self$args$index_col)) {
    idx_var <- 1:nrow(data)
  } else {
    idx_var <- data[, self$args$index_col]
    data <- data[, colnames(data)!=self$args$index_col]
  }
  wts <- self$args$weights
  self$set_args(k = 2, overwrite = FALSE)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(MSwM::msmFit)]

  if (is.null(wts)) {
    m_raw <- stats::lm(self$formula, data)
  } else {
    m_raw <- stats::lm(self$formula, data, weights = wts)
  }
  if (is.null(ctr$sw)) {
    ctr$sw <- rep(TRUE, length(m_raw$coefficients) + 1)
  }

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(MSwM::msmFit, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(object = m_raw), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(index_var = idx_var)
  self$estimator <- "MSwM::msmFit"
  invisible(self)
}
