#' @name .fit.nnet
#' @title Neural Network regression for \code{tidyfit}
#' @description Fits a single-hidden-layer neural network regression on a 'tidyFit' \code{R6} class.
#' The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @details **Hyperparameters:**
#'
#' - \code{size} *(number of units in the hidden layer)*
#' - \code{decay} *(parameter for weight decay)*
#' - \code{maxit} *(maximum number of iterations)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{nnet::nnet.formula}. See \code{?nnet} for more details.
#'
#' **Implementation**
#'
#' For \code{\link{regress}}, linear output units (\code{linout=True}) are used, while \code{\link{classify}} implements
#' the default logic of  \code{nnet} (\code{entropy=TRUE} for 2 target classes and \code{softmax=TRUE} for more classes).
#'
#' @author Phil Holzmeister
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("nnet", Return ~ ., data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("nnet", decay=0.5, size = 8),
#'                .mask = c("Date", "Industry"))
#'
#' # Within 'classify' function
#' fit <- classify(iris, Species ~ ., m("nnet", decay=0.5, size = 8))
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom methods formalArgs
#' @importFrom vctrs vec_as_names

.fit.nnet <- function(
    self,
    data = NULL
) {

  # create model matrix to provide defaults for weights and size args
  # ultimately nnet.formula is called rather than nnet.default as
  # it handles the factor encoding for classification

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  if (self$mode == "regression") {
    self$set_args(linout = TRUE, overwrite = FALSE)
  }

  # no default value for hidden layer size argument,
  # set to 2x input neurons if not provided
  self$set_args(size = 2 * NCOL(x), overwrite = FALSE)

  # if missing(weights), nnet defaults to 1; but not for is.null(weights)
  # so set default values here
  if (is.null(self$args$weights)) {
    self$set_args(weights = rep(1, NROW(x)), overwrite = TRUE)
  }

  ctr <- self$args[names(self$args) %in% methods::formalArgs(nnet::nnet.default)]

  # ignore duplicate arguments that nnet.formula passes for nnet.default
  # if these should be specified via the classify interface, switch to nnet.default
  # (but requires handing of categorical variable encoding here)
  ctr <- ctr[!(names(ctr) %in% c("softmax", "entropy"))]

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(nnet::nnet.formula, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "nnet::nnet"
  invisible(self)

}


.predict.nnet <- function(object, data, self = NULL, ...) {

  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth_vec <- data[, response_var]
  } else {
    truth_vec <- NULL
  }

  if (self$mode == "regression") {
    pred <- dplyr::tibble(
      prediction = drop(stats::predict(object, data)),
      truth = truth_vec
    )
  }

  if (self$mode == "classification") {
    pred <- stats::predict(object, data) %>%
      dplyr::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
      dplyr::mutate(truth = truth_vec) %>%
      tidyr::pivot_longer(-any_of("truth"), names_to = "class", values_to = "prediction") %>%
      dplyr::select(any_of(c("class", "prediction", "truth")))
  }

  return(pred)
}

.fitted.nnet <- function(object, self = NULL, ...) {
  if (self$mode == "regression"){
    fitted <- dplyr::tibble(
      fitted = drop(predict(object))
    )
  }
  if (self$mode == "classification") {
    fitted <- dplyr::tibble(
      fitted = predict(object, type="class")
    )
  }
  return(fitted)
}

.resid.nnet <- function(object, self = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = drop(object$residuals)
  )
  return(residuals)
}
