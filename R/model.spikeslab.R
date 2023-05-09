#' @name .model.spikeslab
#' @title Bayesian Spike and Slab regression for \code{tidyfit}
#' @description Fits a Bayesian Spike and Slab regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' - expected.r2
#' - prior.df
#' - expected.model.size
#' - niter
#'
#' The function provides a wrapper for \code{BoomSpikeSlab::lm.spike}. See \code{?lm.spike} for more details.
#'
#' **Implementation**
#'
#' Prior arguments are passed to BoomSpikeSlab::SpikeSlabPrior (the function automatically identifies which arguments are for the prior, and which for BoomSpikeSlab::lm.spike).
#'
#' @param self a tidyFit R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted tidyFit class model.
#' @author Johann Pfitzinger
#' @references
#' Scott SL (2022). _BoomSpikeSlab: MCMC for Spike and Slab Regression_. R package version 1.2.5, <https://CRAN.R-project.org/package=BoomSpikeSlab>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("spikeslab", Return ~ ., data, niter = 100)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("spikeslab", niter = 100),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lasso}}, \code{\link{.model.blasso}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.model.spikeslab <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  # Set default arguments
  self$set_args(niter = 1000, overwrite = FALSE)

  prior_args <- self$args[names(self$args) %in% methods::formalArgs(BoomSpikeSlab::SpikeSlabPrior)]
  fit_args <- self$args[names(self$args) %in% methods::formalArgs(BoomSpikeSlab::lm.spike)]

  fit_args[["prior"]] <- do.call(BoomSpikeSlab::SpikeSlabPrior, append(list(x = x, y = y), prior_args))

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(BoomSpikeSlab::lm.spike, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), fit_args))
  .store_on_self(self, res)
  self$estimator <- "BoomSpikeSlab::lm.spike"
  self$fit_info <- list(var_names = colnames(x))
  invisible(self)

}
.coef.lm.spike <- function(object, self = NULL, ...) {
  model_estimates <- summary(object)$coefficients
  estimates <- tidyr::tibble(
    term = row.names(model_estimates),
    estimate = model_estimates[, "mean"],
    sd = model_estimates[, "sd"],
    mean.inc = model_estimates[, "mean.inc"],
    sd.inc = model_estimates[, "sd.inc"],
    inc.prob = model_estimates[, "inc.prob"]
  )
  return(estimates)
}
.predict.lm.spike <- function(object, data, self, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 0
    truth <- NULL
  }
  pred <- dplyr::tibble(
    prediction = rowMeans(stats::predict(object, data)),
    truth = truth
  )
  return(pred)
}
.fitted.lm.spike <- function(object, self, ...) {
  fit <- dplyr::tibble(
    fitted = rowMeans(stats::predict(object))
  )
  return(fit)
}
.resid.lm.spike <- function(object, self, ...) {
  residual <- dplyr::tibble(
    residual = object$response - rowMeans(predict(object))
  )
  return(residual)
}
