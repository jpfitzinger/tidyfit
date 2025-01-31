#' @name .fit.mslm
#' @title Markov-Switching Regression for \code{tidyfit}
#' @description Fits a Markov-Switching regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#'  - \code{k} (the number of regimes)
#'  - \code{sw} (logical vector indicating which coefficients switch)
#'  - \code{control} (additional fitting parameters)
#'
#' The function provides a wrapper for \code{MSwM::msmFit}. See \code{?msmFit} for more details.
#'
#' **Implementation**
#'
#' Note that only the regression method with 'lm' is implemented at this stage.
#'
#' An argument \code{index_col} can be passed, which allows a custom index to be added to \code{coef(m("mslm"))} (e.g. a date index).
#'
#' If no \code{sw} argument is passed, all coefficients are permitted to switch between regimes.``
#'
#' @author Johann Pfitzinger
#' @references
#' Sanchez-Espigares JA, Lopez-Moreno A (2021). _MSwM: Fitting Markov Switching Models_. R package version 1.5, <https://CRAN.R-project.org/package=MSwM>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec", Date >= 201801)
#' data <- dplyr::select(data, -Industry)
#'
#' ctr <- list(maxiter = 100, parallelization = FALSE)
#'
#' # Stand-alone function
#' fit <- m("mslm", Return ~ HML, data, index_col = "Date", k = 2, control = ctr)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ HML,
#'                m("mslm", index_col = "Date", k = 2, control = ctr))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.fit.tvp}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats lm
#' @importFrom methods formalArgs

.fit.mslm <- function(
    self,
    data = NULL
) {
  if (!is.null(self$args$weights)) {
    warning("mslm cannot handle weights, weights are ignored", call. = FALSE)
  }
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

  if (TRUE) {
    m_raw <- do.call(stats::lm, list(formula = self$formula, data = data))
  } else {
    # weights currently not implemented correctly
    # m_raw <- do.call(stats::lm, list(formula = self$formula, data = data, weights = wts))
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
  invisible(self)
}
