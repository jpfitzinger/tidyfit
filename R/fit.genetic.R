#' @name .fit.genetic
#' @title Genetic algorithm with linear regression fitness evaluator for \code{tidyfit}
#' @description Fits a linear regression with variable selection using a genetic algorithm on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' - statistic
#' - populationSize
#' - numGenerations
#' - minVariables
#' - maxVariables
#'
#' The function provides a wrapper for \code{gaselect::genAlg}. See \code{?genAlg} for more details.
#'
#' **Implementation**
#'
#' Control arguments are passed to gaselect::genAlgControl (the function automatically identifies which arguments are for the control object, and which for gaselect::genAlg).
#'
#' gaselect::evaluatorLM is used as the evaluator with the relevant arguments automatically identified by the function.
#'
#' @param self a tidyFit R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted tidyFit class model.
#' @author Johann Pfitzinger
#' @references
#' Kepplinger D (2023). _gaselect: Genetic Algorithm (GA) for Variable Selection from High-Dimensional Data_. R package version 1.0.21, <https://CRAN.R-project.org/package=gaselect>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Generally used inside 'regress' function
#' fit <- regress(data, Return ~ ., m("genetic", statistic = "BIC"),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lm}}, \code{\link{.fit.bayes}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.genetic <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  # Set default arguments
  self$set_args(verbosity = 0, overwrite = TRUE)
  self$set_args(populationSize = 100,
                numGenerations = 10,
                minVariables = 1,
                maxVariables = ncol(x),
                seed = 123,
                numThreads = 1,
                overwrite = FALSE)

  incl_intercept <- "(Intercept)" %in% colnames(x)

  evaluator_args <- self$args[names(self$args) %in% methods::formalArgs(gaselect::evaluatorLM)]
  control_args <- self$args[names(self$args) %in% methods::formalArgs(gaselect::genAlgControl)]
  fit_args <- self$args[names(self$args) %in% methods::formalArgs(gaselect::genAlg)]

  fit_args[["control"]] <- do.call(gaselect::genAlgControl, control_args)
  fit_args[["evaluator"]] <- do.call(gaselect::evaluatorLM, evaluator_args)

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(gaselect::genAlg, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(X = x, y = y), fit_args))
  if (!is.null(res$result$result)) {
    x_subset <- cbind(
      dplyr::select(as.data.frame(x), all_of(gaselect::subsets(res$result$result, 1, names = TRUE)[[1]])),
      data.frame(target_var = y)
    )
    x_subset <- data.frame(x_subset)
    if (incl_intercept & (!"(Intercept)" %in% colnames(x_subset))) {
      res_mod <- stats::lm(target_var ~ 1 + ., data = x_subset, weights = self$args$weights)
    } else {
      res_mod <- stats::lm(target_var ~ 0 + ., data = x_subset, weights = self$args$weights)
    }

  } else {
    res_mod <- NULL
  }
  .store_on_self(self, res)
  self$estimator <- "gaselect::genAlg"
  self$fit_info <- list(var_names = colnames(x), fitted_regression = res_mod)
  self$force_syntactic_names <- TRUE
  invisible(self)

}
.coef.GenAlg <- function(object, self = NULL, ...) {
  estimates <- broom::tidy(self$fit_info$fitted_regression)
  return(estimates)
}
.predict.GenAlg <- function(object, data, self, ...) {
  response_var <- all.vars(self$formula)[1]
  if (response_var %in% colnames(data)) {
    truth <- data[, response_var]
  } else {
    data[, response_var] <- 0
    truth <- NULL
  }
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  pred <- dplyr::tibble(
    prediction = stats::predict(self$fit_info$fitted_regression, data.frame(x)),
    truth = truth
  )
  return(pred)
}
.fitted.GenAlg <- function(object, self, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(self$fit_info$fitted_regression)
  )
  return(fitted)
}
.resid.GenAlg <- function(object, self, ...) {
  residual <- dplyr::tibble(
    residual = resid(self$fit_info$fitted_regression)
  )
  return(residual)
}

