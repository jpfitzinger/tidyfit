#' @name .model.tvp
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
#' # Stand-alone function (using low niter for illustration)
#' fit <- m("tvp", Return ~ ., data, index_col = "Date", niter = 50)
#' fit
#'
#' # Within 'regress' function (using low niter for illustration)
#' fit <- regress(data, Return ~ ., m("tvp", niter = 50, index_col = "Date"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.model.bayes}}, \code{\link{.model.mslm}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.model.tvp <- function(
    self,
    data = NULL
) {

  self$set_args(display_progress = FALSE, overwrite = FALSE)

  if (!is.null(self$args$weights)) {
    warning("tvp cannot handle weights, weights are ignored")
  }

  idx_col <- self$args$index_col
  if (is.null(self$args$index_col)) {
    idx_var <- 1:nrow(data)
  } else {
    idx_var <- data[, self$args$index_col]
    data <- data[, colnames(data)!=self$args$index_col]
  }
  ctr <- self$args[names(self$args) %in% methods::formalArgs(shrinkTVP::shrinkTVP)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(shrinkTVP::shrinkTVP, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(index_var = idx_var)
  self$estimator <- "shrinkTVP::shrinkTVP"
  invisible(self)
}
