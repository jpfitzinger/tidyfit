#' @name .model.hfr
#' @title Hierarchical feature regression for \code{tidyfit}
#' @description Fits a hierarchical feature regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - kappa_grid (*proportional size of regression graph*)
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The hierarchical feature regression is estimated using the \code{hfr::cv.hfr} function. See \code{?cv.hfr} for more details.
#'
#' **Implementation**
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is provided (\code{is.null(control$kappa_grid)}), the default is \code{seq(0, 1, by = 0.1)}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' Pfitzinger J (2022).
#' _hfr: Estimate Hierarchical Feature Regression Models_.
#' R package version 0.5.0, <https://CRAN.R-project.org/package=hfr>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("hfr", Return ~ ., data, kappa_grid = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("hfr", kappa_grid = c(0.1, 0.5)),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.plsr}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.model.hfr <- function(
    self,
    data = NULL
) {
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]
  ctr <- self$args[names(self$args) %in% methods::formalArgs(hfr::cv.hfr)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(hfr::cv.hfr, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x, y = y, nfolds = 1, intercept = incl_intercept), ctr))
  .store_on_self(self, res)
  self$inner_grid <- data.frame(
    grid_id = paste(substring(self$grid_id, 1, 4), formatC(1:length(self$args$kappa_grid), 2, flag = "0"), sep = "|"),
    kappa_grid = self$args$kappa_grid
  )
  self$estimator <- "hfr::cv.hfr"
  invisible(self)
}
