#' @name m
#' @title Generic model wrapper for \code{tidyfit}
#' @description The function can fit various regression or classification models and returns the results as a tibble. \code{m()} can be used in conjunction with \code{\link{regress}} and \code{\link{classify}}, or as a stand-alone function.
#'
#' @details \code{model_method} specifies the model used to regress \code{y} on \code{x} and can take one of several options:
#'
#' ### Linear (generalized) regression or classification
#'
#' \code{"lm"} performs an OLS regression using \code{stats::lm}. See \code{\link{.model.lm}} for details.
#'
#' \code{"glm"} performs a generalized regression using \code{stats::glm}. See \code{\link{.model.glm}} for details.
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
#' \code{"boost"} performs gradient boosting regression or classificaiton using \code{mboost::glmboost}. See \code{\link{.model.boost}} for details.
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
#' ### Bayesian regression
#'
#' \code{"bayes"} performs a Bayesian generalized regression or classification using \code{arm::bayesglm}. See \code{\link{.model.bayes}} for details.
#'
#' ### Mixed-effects modeling
#'
#' \code{"glmm"} performs a mixed-effects GLM using \code{lme4::glmer}. See \code{\link{.model.glmm}} for details.
#'
#' When called without \code{formula} and \code{data} arguments, the function returns a partialised version of itself that can be called with data to fit a model.
#'
#' @param model_method The name of the method to fit. See Details.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ...  Additional arguments passed to the underlying method function (e.g. \code{lm} or \code{glm}).
#' @param .return_method_name When \code{TRUE}, the function simply returns the 'method' argument.
#' @param .check_family When \code{TRUE}, the function returns a flag indicating whether a custom 'family' object has been passed to \code{...}.
#' @return A 'tibble'.
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

m <- function(model_method,
              formula = NULL,
              data = NULL,
              ...,
              .return_method_name = FALSE,
              .check_family = FALSE
              ) {

  # Checks
  .check_method(model_method, "exists")
  additional_args <- list(...)
  if (.return_method_name) return(model_method)
  if (.check_family) return("family" %in% names(additional_args))

  # Partialised function when no data is passed
  if (is.null(formula) & is.null(data)) {
    args <- c(list(model_method = model_method), additional_args)
    args <- args[!names(args) %in% c("formula", "data")]
    args <- append(args, list(.f = m))
    return(do.call(purrr::partial, args))
  }

  # Set default hyperparameter grids
  default_grids <- .default_hp_grid(model_method,
                                    additional_args,
                                    formula,
                                    data)
  additional_args <- append(additional_args, default_grids)

  # Used to define the class
  tmp_ <- structure("", class = model_method)
  qmodel <- purrr::quietly(function(args) do.call(.model, args))

  if (length(additional_args)==0) {
    args <- list(formula = formula, data = data, control = additional_args, identifier = tmp_)
    mod_list <- qmodel(args)
    mod <- mod_list$result
    mod <- dplyr::bind_cols(mod, purrr::compact(mod_list[-c(1:2)]))
    mod <- mod %>%
      dplyr::mutate(grid_id = "#0010000")
  } else {
    args_grid <- .args_to_grid(model_method, additional_args)
    names(args_grid) <- paste0("#", formatC(1:length(args_grid), 2, flag = "0"), "0000")
    mod <- args_grid %>%
      purrr::map_dfr(function(additional_args) {
        args <- list(formula = formula, data = data, control = additional_args, identifier = tmp_)
        mod_list <- qmodel(args)
        mod <- mod_list$result
        mod <- dplyr::bind_cols(mod, purrr::compact(mod_list[-c(1:2)]))
        return(mod)
      }, .id = "grid_id")
  }

  # Arrange output
  mod <- mod %>%
    dplyr::arrange(.data$grid_id) %>%
    dplyr::relocate(.data$grid_id)

  mod <- tibble::new_tibble(mod, class = "tidyfit.models")

  return(mod)

}
