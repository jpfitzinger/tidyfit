#' @name .fit.bma
#' @title Bayesian model averaging for \code{tidyfit}
#' @description Fits a Bayesian model averaging regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#'  - \code{iter} (*number of iteration draws*)
#'  - \code{mcmc} (*model sampler used (default 'bd')*)
#'
#' The function provides a wrapper for \code{BMS::bms}. See \code{?bms} for more details.
#'
#' **Implementation**
#'
#' The underlying function automatically generates plotting output, which is not suppressed.
#'
#' Use \code{coef(fit)} to obtain posterior mean, standard deviation as well as posterior inclusion probabilities for the features.
#'
#' @author Johann Pfitzinger
#'
#' @references
#' Feldkircher, M. and S. Zeugner (2015).
#' _Bayesian Model Averaging Employing Fixed and Flexible Priors: The BMS Package for R_,
#' Journal of Statistical Software 68(4).
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("bma", Return ~ `Mkt-RF` + HML + SMB + RMW + CMA, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("bma"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.bayes}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.bma <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("bma cannot handle weights, weights are ignored", call. = FALSE)
  }

  self$set_args(user.int = FALSE, overwrite = FALSE)

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  # keep only linearly independent columns
  var_names <- colnames(x)
  x <- x[, qr(x)$pivot[seq_len(qr(x)$rank)]]
  model_var_names <- colnames(x)
  if (length(var_names) > length(model_var_names))
    warning("some variables were dropped from BMA estimation due to linear dependence", call. = FALSE)

  y <- stats::model.response(mf)
  Xy <- cbind(y, x)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(BMS::bms)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(BMS::bms, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(X.data = Xy), ctr))
  .store_on_self(self, res)
  self$fit_info = list(all_var_names = var_names, model_var_names = model_var_names)
  invisible(self)
}

.coef.bma <- function(object, self = NULL, ...) {
  raw_estimates <- coef(object, include.constant = TRUE)
  estimates <- dplyr::tibble(
    term = rownames(raw_estimates),
    estimate = raw_estimates[, "Post Mean"],
    posterior_sd = raw_estimates[, "Post SD"],
    pip = raw_estimates[, "PIP"]
  )
  estimates <- tidyr::complete(estimates, term = self$fit_info$all_var_names)
  return(estimates)
}

.predict.bma <- function(object, data, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 0
    truth <- NULL
  }
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  x <- x[, colnames(x) %in% self$fit_info$model_var_names]
  pred <- dplyr::tibble(
    prediction = stats::predict(object, x, exact = TRUE),
    truth = truth
  )
  return(pred)
}
