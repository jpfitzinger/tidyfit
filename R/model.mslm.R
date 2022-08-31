#' @name .model.mslm
#' @title Markov-Switching Regression for \code{tidyfit}
#' @description Fits a Markov-Switching regression and returns the results as a tibble. The function can be used with \code{\link{regress}}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{MSwM::msmFit}.
#' @param ... Not used.
#' @return A 'tibble'.
#'
#' @details  **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{MSwM::msmFit}. Note that only the regression method with 'lm' is implemented at this stage.
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
#' fit <- m("mslm", Return ~ ., data, index_col = "Date", k = 2, sw = rep(TRUE, 8))
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ .,
#'                m("mslm", niter = 100, index_col = "Date", k = 2, sw = rep(TRUE, 8)))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.model.tvp}} and \code{\link{m}} methods
#'
#' @importFrom dplyr tibble everything as_tibble
#' @importFrom tidyr nest
#' @importFrom purrr partial
#' @importFrom stats lm
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.mslm <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  idx_col <- control$index_col
  control$family <- NULL
  wts <- control$weights
  control <- control[names(control) %in% methods::formalArgs(MSwM::msmFit)]

  if (is.null(idx_col)) {
    idx_var <- 1:nrow(data)
  } else {
    idx_var <- data[, idx_col]
    data <- data[, colnames(data)!=idx_col]
  }

  if (is.null(wts)) {
    m_raw <- stats::lm(formula, data)
  } else {
    m_raw <- stats::lm(formula, data, weights = wts)
  }

  m <- do.call(MSwM::msmFit, append(list(object = m_raw), control))

  model_handler <- purrr::partial(.handler.MSwM, object = m, formula = formula, index_var = idx_var)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "MSwM::msmFit",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.MSwM <- function(object, data, formula = NULL, index_var = NULL, ..., .what = "model") {

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
    pred <- object@Fit@CondMean * object@Fit@smoProb[-1,]
    pred <- dplyr::tibble(
      prediction = rowSums(pred),
      truth = truth
    )
    return(pred)
  }

  if (.what == "estimates") {
    beta <- data.matrix(object@Coef)
    probs <- data.matrix(object@Fit@smoProb[-1,])
    beta_probs <- probs %*% beta
    estimates <- beta_probs %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(index = index_var) %>%
      tidyr::gather("term", "estimate", -.data$index)
    colnames(probs) <- paste("Regime", 1:object@k, "Prob")
    probs_df <- dplyr::as_tibble(probs) %>%
      dplyr::mutate(index = index_var)
    beta_df <- t(beta)
    colnames(beta_df) <- paste("Regime", 1:object@k, "Beta")
    beta_df <- dplyr::as_tibble(beta_df) %>%
      dplyr::mutate(term = rownames(beta_df))
    estimates <- estimates %>%
      dplyr::left_join(probs_df, by = "index") %>%
      dplyr::left_join(beta_df, by = "term")
    return(estimates)
  }

}
