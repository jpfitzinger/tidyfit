#' @name .model.tvp
#' @title Bayesian Time-Varying Regression for \code{tidyfit}
#' @description Fits a Bayesian time-varying regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{lm}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{shrinkTVP::shrinkTVP}.
#'
#' An argument \code{index_col} can be passed, which allows a custom index to be added to \code{coef(m("tvp"))} (e.g. a date index). This only works when the formula is specified as \code{y ~ .} (with a dot on the RHS), since otherwise the index_col is currently removed by \code{\link{regress}}.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data <- dplyr::filter(data, Industry == "HiTec")
#' data <- dplyr::select(data, -Industry)
#'
#' # Stand-alone function
#' fit <- m("tvp", Return ~ ., data, index_col = "Date", niter = 100)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("tvp", niter = 100, index_col = "Date"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.model.robust}}, \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom dplyr tibble everything as_tibble
#' @importFrom tidyr nest
#' @importFrom purrr partial
#' @importFrom stats quantile
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.tvp <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  idx_col <- control$index_col
  control <- control[names(control) %in% methods::formalArgs(shrinkTVP::shrinkTVP)]

  if (is.null(idx_col)) {
    idx_var <- 1:nrow(data)
  } else {
    idx_var <- data[, idx_col]
    data <- data[, colnames(data)!=idx_col]
  }

  m <- do.call(shrinkTVP::shrinkTVP, append(list(formula = formula, data = data), control))

  model_handler <- purrr::partial(.handler.shrinkTVP, object = m, formula = formula, index_var = idx_var)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "shrinkTVP::shrinkTVP",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.shrinkTVP <- function(object, data, formula = NULL, index_var = NULL, ..., .what = "model") {

  if (.what == "model") {
    return(object)
  }

  if (.what == "predict") {
    response_var <- all.vars(formula)[1]
    if (response_var %in% colnames(data)) {
      truth <- data[, response_var]
    } else {
      truth <- NULL
    }
    pred <- stats::predict(object)
    pred <- dplyr::tibble(
      prediction = colMeans(pred),
      truth = truth
    )
    return(pred)
  }

  if (.what == "estimates") {
    beta <- object$beta
    estimates <- map_dfr(beta, function(bet) {
      tibble(
        estimate = apply(bet, 2, mean)[-1],
        upper = apply(bet, 2, stats::quantile, 0.925)[-1],
        lower = apply(bet, 2, stats::quantile, 0.025)[-1],
        posterior.sd = apply(bet, 2, sd)[-1],
        index = index_var
      )
    }, .id = "term")
    estimates <- estimates %>%
      dplyr::mutate(term = gsub("beta_", "", .data$term)) %>%
      dplyr::mutate(term = ifelse(.data$term == "Intercept", "(Intercept)", .data$term))
    return(estimates)
  }

}
