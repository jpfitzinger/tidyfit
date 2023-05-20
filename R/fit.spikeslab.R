#' @name .fit.spikeslab
#' @title Bayesian Spike and Slab regression or classification for \code{tidyfit}
#' @description Fits a Bayesian Spike and Slab regression or classification on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' In the case of **regression**, arguments are passed to \code{BoomSpikeSlab::lm.spike} and \code{BoomSpikeSlab::SpikeSlabPrior}. Check those functions for details.
#'
#' \code{BoomSpikeSlab::SpikeSlabPrior}
#' - expected.r2
#' - prior.df
#' - expected.model.size
#'
#' \code{BoomSpikeSlab::lm.spike}
#' - niter
#'
#' In the case of **classification**, arguments are passed to \code{BoomSpikeSlab::logit.spike} and \code{BoomSpikeSlab::SpikeSlabGlmPrior}. Check those functions for details.
#'
#' \code{BoomSpikeSlab::logit.spike}
#' - niter
#'
#' I advise against the use of \code{BoomSpikeSlab::SpikeSlabGlmPrior} at the moment, since it appears to be buggy.
#'
#' The function provides wrappers for \code{BoomSpikeSlab::lm.spike} and \code{BoomSpikeSlab::logit.spike}. See \code{?lm.spike} and \code{?logit.spike} for more details.
#'
#' **Implementation**
#'
#' Prior arguments are passed to \code{BoomSpikeSlab::SpikeSlabPrior} and \code{BoomSpikeSlab::SpikeSlabGlmPrior} (the function automatically identifies which arguments are for the prior, and which for \code{BoomSpikeSlab::lm.spike} or \code{BoomSpikeSlab::logit.spike}).
#'
#' \code{BoomSpikeSlab::logit.spike} is automatically selected when using \code{classify}.
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
#' @seealso \code{\link{.fit.lasso}}, \code{\link{.fit.blasso}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.spikeslab <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  if (self$mode == "classification") {
    class_names_map <- levels(y)
    names(class_names_map) <- c(0, 1)
    data[, all.vars(self$formula)[1]] <- ifelse(y == class_names_map[1], 0, 1)
  }

  # Set default arguments
  self$set_args(niter = 1000, overwrite = FALSE)

  if (self$mode == "regression") {
    prior_args <- self$args[names(self$args) %in% methods::formalArgs(BoomSpikeSlab::SpikeSlabPrior)]
    fit_args <- self$args[names(self$args) %in% methods::formalArgs(BoomSpikeSlab::lm.spike)]
    if (length(prior_args) > 0)
      fit_args[["prior"]] <- do.call(BoomSpikeSlab::SpikeSlabPrior, append(list(x = x, y = y), prior_args))
    eval_fun_ <- function(...) {
      args <- list(...)
      do.call(BoomSpikeSlab::lm.spike, args)
    }
    estimator <- "BoomSpikeSlab::lm.spike"
  }
  if (self$mode == "classification") {
    prior_args <- self$args[names(self$args) %in% methods::formalArgs(BoomSpikeSlab::SpikeSlabGlmPrior)]
    fit_args <- self$args[names(self$args) %in% methods::formalArgs(BoomSpikeSlab::logit.spike)]
    if (length(prior_args) > 0)
      fit_args[["prior"]] <- do.call(BoomSpikeSlab::SpikeSlabGlmPrior, append(list(predictors = x), prior_args))
    eval_fun_ <- function(...) {
      args <- list(...)
      do.call(BoomSpikeSlab::logit.spike, args)
    }
    estimator <- "BoomSpikeSlab::logit.spike"
  }

  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), fit_args))
  .store_on_self(self, res)
  self$estimator <- estimator
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
    fitted = rowMeans(stats::predict(object, object$training.data))
  )
  return(fit)
}
.resid.lm.spike <- function(object, self, ...) {
  residual <- dplyr::tibble(
    residual = object$response - rowMeans(predict(object))
  )
  return(residual)
}
