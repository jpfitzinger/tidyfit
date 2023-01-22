#' @name .model.nnet
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
#' \code{coef()} does not return model coefficients, but the relative feature importance as implemented in the \code{iml} package.
#' (\code{compare="ratio"} of \code{loss="mae"} for regression models, and \code{compare="difference"} of \code{loss="ce"}
#' for classification models reported for each class). *Calculation of feature importance only works for syntactic feature names.*
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
#' coef(fit)
#'
#' # Within 'classify' function
#' fit <- classify(iris, Species ~ ., m("nnet", decay=0.5, size = 8))
#' coef(fit)
#'
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom methods formalArgs

.model.nnet <- function(
  self,
  data = NULL
) {

  # sanitize names to ensure compatibility with iml
  var_names_map <- .names_map(colnames(data))
  data <- data.frame(data)
  self$formula <- .fix_names(self$formula, var_names_map)

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
  self$fit_info <- list(names_map = var_names_map)
  self$estimator <- "nnet::nnet"
  invisible(self)

}


.predict.nnet <- function(object, data, self = NULL, ...) {
  # sanitize names
  data <- data.frame(data)

  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    truth <- NULL
  }

  if (self$mode == "regression") {
    pred <- dplyr::tibble(
      prediction = drop(stats::predict(object, data)),
      truth = truth
    )
  }

  if (self$mode == "classification") {
    pred <- stats::predict(object, data) %>%  dplyr::as_tibble() %>%
      dplyr::mutate(truth = truth) %>%
      tidyr::pivot_longer(-truth, names_to = "class", values_to = "prediction") %>%
      dplyr::select(all_of(c("class", "prediction", "truth")))
  }

  return(pred)
}

.coef.nnet <- function(object, self = NULL, ...) {
  response_var <- all.vars(self$formula)[1]
  data <- object$call$data
  data <- data.frame(data)
  mf <- stats::model.frame(self$formula, data)

  if (self$mode == "regression") {
    mod <- iml::Predictor$new(object, data = mf %>% dplyr::select(-response_var),
                              y = mf[[response_var]])
    imp <- iml::FeatureImp$new(mod, loss = "mae")
    estimates <- imp$results %>%
      dplyr::select(term = "feature",
                    estimate = "importance") %>%
      dplyr::as_tibble()
  }

  if (self$mode == "classification") {
    # separately calculate permutation feature importance for each class
    estimates <- purrr::map_dfr(unique(mf[[response_var]]),
                                function(cls) {

      mf_class <- mf %>% dplyr::filter(!!dplyr::sym(response_var) == cls)
      mod <- iml::Predictor$new(object, data = mf_class %>% dplyr::select(-response_var),
                         y = mf_class[[response_var]], type = "raw")
      imp <- iml::FeatureImp$new(mod, loss = "ce", compare = "difference")
      imp$results %>%
        dplyr::select(term = "feature",
                      estimate = "importance") %>%
        dplyr::mutate(class = cls) %>%
        dplyr::as_tibble()
    })

  }

  estimates <- estimates %>%
    dplyr::mutate(term = self$fit_info$names_map[.data$term])

  return(estimates)
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
