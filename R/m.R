#' @name m
#' @title Generic model wrapper for \code{tidyfit}
#' @description The function can fit various regression or classification models and returns the results as a tibble. \code{m()} can be used in conjunction with \code{\link{regress}} and \code{\link{classify}}, or as a stand-alone function.
#'
#' @details \code{model_method} specifies the model to fit to the data and can take one of several options:
#'
#' ### Linear (generalized) regression or classification
#'
#' \code{"lm"} performs an OLS regression using \code{stats::lm}. See \code{\link{.model.lm}} for details.
#'
#' \code{"glm"} performs a generalized regression or classification using \code{stats::glm}. See \code{\link{.model.glm}} for details.
#'
#' \code{"robust"} performs a robust regression using \code{MASS::rlm}. See \code{\link{.model.robust}} for details.
#'
#' \code{"quantile"} performs a quantile regression using \code{quantreg::rq}. See \code{\link{.model.quantile}} for details.
#'
#' ### Regression and classification with L1 and L2 penalties
#'
#' \code{"lasso"} performs a linear regression or classification with L1 penalty using \code{glmnet::glmnet}. See \code{\link{.model.lasso}} for details.
#'
#' \code{"ridge"} performs a linear regression or classification with L2 penalty using \code{glmnet::glmnet}. See \code{\link{.model.lasso}} for details.
#'
#' \code{"adalasso"} performs an Adaptive Lasso regression or classification using \code{glmnet::glmnet}. See \code{\link{.model.adalasso}} for details.
#'
#' \code{"enet"} performs a linear regression or classification with L1 and L2 penalties using \code{glmnet::glmnet}. See \code{\link{.model.enet}} for details.
#'
#' ### Gradient boosting
#'
#' \code{"boost"} performs gradient boosting regression or classification using \code{mboost::glmboost}. See \code{\link{.model.boost}} for details.
#'
#' ### Factor regressions
#'
#' \code{"pcr"} performs a principal components regression using \code{pls::pcr}. See \code{\link{.model.pcr}} for details.
#'
#' \code{"plsr"} performs a partial least squares regression using \code{pls::plsr}. See \code{\link{.model.plsr}} for details.
#'
#' \code{"hfr"} performs a hierarchical feature regression using \code{hfr::hfr}. See \code{\link{.model.hfr}} for details.
#'
#' ### Best subset selection
#'
#' \code{"subset"} performs a best subset regression or classification using \code{bestglm::bestglm} (wrapper for \code{leaps}). See \code{\link{.model.subset}} for details.
#'
#' ### Bayesian methods
#'
#' \code{"bayes"} performs a Bayesian generalized regression or classification using \code{arm::bayesglm}. See \code{\link{.model.bayes}} for details.
#'
#' \code{"tvp"} performs a Bayesian time-varying parameter regression using \code{shrinkTVP::shrinkTVP}. See \code{\link{.model.tvp}} for details.
#'
#' ### Mixed-effects modeling
#'
#' \code{"glmm"} performs a mixed-effects GLM using \code{lme4::glmer}. See \code{\link{.model.glmm}} for details.
#'
#' ### Specialized time series methods
#'
#' \code{"mslm"} performs a Markov-switching regression using \code{MSwM::msmFit}. See \code{\link{.model.mslm}} for details.
#'
#' When called without \code{formula} and \code{data} arguments, the function returns a 'tidyfit.models' data frame with unfitted models.
#'
#' @param model_method The name of the method to fit. See Details.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ...  Additional arguments passed to the underlying method function (e.g. \code{lm} or \code{glm}).
#' @return A 'tidyfit.models' data frame.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("lm", Return ~ ., data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("lm"), .mask = "Date")
#' fit
#'
#' @export
#'
#' @seealso \code{\link{regress}} and \code{\link{classify}} methods
#'
#' @importFrom purrr partial quietly
#' @importFrom tidyr complete expand_grid
#' @importFrom tibble new_tibble
#' @importFrom rlang .data
#' @importFrom dplyr mutate arrange relocate bind_rows bind_cols

m <- function(
    model_method,
    formula = NULL,
    data = NULL,
    ...
) {
  if (!is.null(data) & is.null(formula))
    stop("'formula' cannot be missing when 'data' is provided")
  args <- list(...)
  default_grids <- .default_hp_grid(model_method,
                                    args,
                                    formula,
                                    data)
  args <- append(args, default_grids)
  args_grid <- .args_to_grid(model_method, args)
  grd_ids <- paste0("#", formatC(1:length(args_grid), 2, flag = "0"), "0000")
  mods <- purrr::map2_dfr(args_grid, grd_ids, function(args_vec, grd_id) {
    tidyr::tibble(
      grid_id = grd_id,
      model_object = c(model_definition$new(model_method, formula, args_vec, grid_id = grd_id))
    )
  })

  if (!is.null(data)) {
    mods$model_object <- purrr::map(mods$model_object, function(mod) mod$fit(data))
  }

  mods <- .make_model_cols(mods)
  mods <- .unnest_args(mods)
  col_ord <- c("estimator", "size (MB)", "grid_id", "model_object", "settings", "errors", "warnings", "messages")
  mods <- dplyr::relocate(mods, any_of(col_ord)) %>%
    dplyr::arrange(.data$grid_id)
  mods <- tibble::new_tibble(mods, class = "tidyfit.models")
  return(mods)
}
