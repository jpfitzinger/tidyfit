#' @name .model.plsr
#' @title Partial Least Squares Regression for \code{tidyfit}
#' @description Fits a partial least squares regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{ncomp} *(number of components)*
#' - \code{ncomp_pct} *(number of components, percentage of features)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The partial least squares regression is fitted using \code{pls} package. See \code{?plsr} for more details.
#'
#' **Implementation**
#'
#' Covariates are standardized, with coefficients back-transformed to the original scale. An intercept is always included.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$ncomp) & is.null(control$ncomp_pct)}), the default is \code{ncomp_pct = seq(0, 1, length.out = 20)}, where 0 results in one component and 1 results in the number of features.
#'
#' Note that at present \code{pls} does not offer weighted implementations or non-gaussian response. The method can therefore only be used with \code{\link{regress}}
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' Liland K, Mevik B, Wehrens R (2022). _pls: Partial Least Squares and Principal Component Regression_. R package version 2.8-1, <https://CRAN.R-project.org/package=pls>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Industry)
#'
#' # Stand-alone function
#' fit <- m("plsr", Return ~ ., data, ncomp = 3)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("plsr", ncomp_pct = c(0, 0.25, 0.5)),
#'                .mask = c("Date"), .cv = "vfold")
#' coef(fit)
#'
#' @seealso \code{\link{.model.pcr}} and \code{\link{m}} methods
#'
#' @importFrom stats coef sd
#' @importFrom rlang .data
#' @importFrom methods formalArgs


.model.plsr <- function(
    self,
    data = NULL
    ) {

  if (!is.null(self$args$weights)) {
    warning("plsr cannot handle weights, weights are ignored")
  }

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  standard_sd <- apply(x, 2, stats::sd)

  self$set_args(ncomp = 1 + round((NCOL(x) - 1) * self$args$ncomp_pct),
                overwrite = FALSE)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(pls::mvr)]
  ctr$model <- FALSE
  ctr$x <- FALSE
  ctr$y <- FALSE

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(pls::plsr, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data,
                             scale=standard_sd, center=T), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(standard_sd = standard_sd)
  self$estimator <- "pls::plsr"
  invisible(self)
}
