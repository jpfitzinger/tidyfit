#' @name .fit.ar
#' @title Bayesian Time-Varying Regression for \code{tidyfit}
#' @description Fits a Bayesian time-varying regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#'  - \code{mod_type}
#'  - \code{niter} (number of MCMC iterations)
#'
#' The function provides a wrapper for \code{shrinkTVP::shrinkTVP}. See \code{?shrinkTVP} for more details.
#'
#' **Implementation**
#'
#' An argument \code{index_col} can be passed, which allows a custom index to be added to \code{coef(m("tvp"))} (e.g. a date index, see Examples).
#'
#' @author Johann Pfitzinger
#' @references
#' Peter Knaus, Angela Bitto-Nemling, Annalisa Cadonna and Sylvia Frühwirth-Schnatter (2021).
#' \emph{Shrinkage in the Time-Varying Parameter Model Framework Using the {R} Package {shrinkTVP}.
#' Journal of Statistical Software 100(13), 1--32}.
#' \doi{10.18637/jss.v100.i13}.\cr
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Industry)
#'
#' # Within 'regress' function (using low niter for illustration)
#' fit <- regress(data, Return ~ ., m("tvp", niter = 50, index_col = "Date"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.fit.bayes}}, \code{\link{.fit.mslm}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.ar <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("star cannot handle weights, weights are ignored")
  }

  idx_col <- self$args$index_col
  if (is.null(self$args$index_col)) {
    idx_var <- 1:nrow(data)
  } else {
    idx_var <- data[, self$args$index_col]
    data <- data[, colnames(data)!=self$args$index_col]
  }
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  if (NCOL(x) > 1) stop("Too many columns supplied") else x <- drop(x)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(tsDyn::linear)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(tsDyn::linear, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(index_var = idx_var)
  self$estimator <- "tsDyn::linear"
  invisible(self)
}
