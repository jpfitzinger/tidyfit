#' @name .model.pcr
#' @title Principal Components Regression for \code{tidyfit}
#' @description Fits a principal components regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{ncomp} *(number of components)*
#'
#' The principal components regression is fitted using \code{pls} package. Covariates are standardized, with coefficients back-transformed to the original scale. An intercept is always included.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$ncomp)}), the default is \code{seq(1, nvars)}, where nvars is the number of features.
#'
#' Note that at present \code{pls} does not offer weighted implementations or non-gaussian response. The method can therefore only be used with \code{\link{regress}}
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{pls::pcr}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Kristian Hovde Liland, Bjørn-Helge Mevik, Ron Wehrens (2022).
#' pls: Partial Least Squares and Principal Component Regression.
#' R package version 2.8-1. URL https://CRAN.R-project.org/package=pls.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("pcr", Return ~ ., data, ncomp = 3)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("pcr"), .mask = c("Date", "Industry"), .cv = "vfold")
#' coef(fit)
#'
#' @seealso \code{\link{.model.plsr}} and \code{\link{m}} methods
#'
#' @importFrom pls pcr
#' @importFrom stats coef sd
#' @importFrom dplyr as_tibble mutate tibble bind_cols
#' @importFrom methods formalArgs

.model.pcr <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  if ("weights" %in% names(control)) {
    warning("pcr cannot handle weights, weights are ignored")
  }
  control <- control[names(control) %in% methods::formalArgs(pls::mvr)]

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]

  standard_sd <- apply(x, 2, stats::sd)

  m <- do.call(pls::pcr, append(list(formula = formula, data = data,
                                      scale=standard_sd, center=T), control))

  model_handler <- purrr::partial(.handler.pls, object = m, formula = formula, standard_sd = standard_sd)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "pls::plsr",
    handler = list(model_handler),
    settings
  )

  return(out)

}
