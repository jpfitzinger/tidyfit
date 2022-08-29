#' @name .model.boost
#' @title Gradient boosting regression for \code{tidyfit}
#' @description Fits a gradient boosting regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{mstop} (*number of boosting iterations*)
#' - \code{nu} (*step size*)
#'
#' The gradient boosting regression is performed using \code{mboost::glmboost}. For classification pass \code{family = "binomial"} to \code{control} or to the argument \code{...} in \code{\link{m}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$mstop)} and \code{is.null(control$nu)}), the default grid is used with \code{mstop = c(100, 500, 1000, 5000)} and \code{nu = c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25)}.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{mboost::glmboost}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' T. Hothorn, P. Buehlmann, T. Kneib, M. Schmid, and B. Hofner (2022).
#' mboost: Model-Based Boosting, R package version 2.9-7, https://CRAN.R-project.org/package=mboost.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("boost", Return ~ ., data, nu = 0.1)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("boost", nu = c(0.1, 0.05)), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom stats coef
#' @importFrom purrr map_dbl partial quietly
#' @importFrom dplyr mutate left_join n
#' @importFrom tidyr gather expand_grid
#' @importFrom methods formalArgs
#' @importFrom utils object.size

.model.boost <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
    ) {

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]

  f <- control$family
  if (is.null(f)) f <- gaussian()
  if (f$family == "gaussian") {
    family <- mboost::Gaussian()
  } else if (f$family == "binomial") {
    family <- mboost::Binomial()
    y <- as.factor(y)
  }

  mstop <- control$mstop
  nu <- control$nu
  # Available args are not accessible using formalArgs without importing mboost:::glmboost.matrix
  glmboost.matrix_args <- c("x", "y", "weights", "offset", "family", "na.action", "contrasts.arg", "center", "control", "oobweights")
  control <- control[names(control) %in% glmboost.matrix_args]
  control <- control[!names(control) %in% c("control", "center", "family", "x", "y")]

  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))

  xs <- data.frame(`(Intercept)` = 1, xs, check.names = FALSE)

  ctr <- mboost::boost_control(mstop = mstop, nu = nu)
  m <- do.call(mboost::glmboost, append(list(x = data.matrix(xs), y = y,
                                             control = ctr, center = F,
                                             family = family), control))

  model_handler <- purrr::partial(.handler.mboost, object = m, formula = formula,
                                  family = f, standard_sd = standard_sd,
                                  standard_mean = standard_mean,
                                  var_names = colnames(xs))

  control <- control[!names(control) %in% c("weights")]
  control$mstop <- mstop
  control$nu <- nu
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control))
    settings <- tidyr::nest(settings, settings = dplyr::everything())
  } else {
    settings <- NULL
  }

  out <- tibble(
    estimator = "mboost::glmboost",
    size = utils::object.size(m),
    handler = list(model_handler),
    settings
  )

  return(out)

}

.handler.mboost <- function(object, data, formula = NULL, family = NULL,
                            standard_sd = NULL, standard_mean = NULL, var_names = NULL,
                            ..., .what = "model") {

  if (.what == "model") {
    return(object)
  }

  if (.what == "predict") {
    response_var <- all.vars(formula)[1]
    if (response_var %in% colnames(data)) {
      truth <- data[, response_var]
    } else {
      data[, response_var] <- 1
      truth <- NULL
    }
    mf <- stats::model.frame(formula, data)
    x <- stats::model.matrix(formula, mf)
    y <- stats::model.response(mf)
    if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
    xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))
    xs <- data.frame(`(Intercept)` = 1, xs, check.names = FALSE)
    pred <- dplyr::tibble(
      prediction = drop(stats::predict(object, data.matrix(xs), type = "response")),
      truth = truth
    )
    return(pred)
  }

  if (.what == "estimates") {
    quiet_coefs <- purrr::quietly(stats::coef)
    beta <- purrr::map_dbl(var_names, function(x) quiet_coefs(object, which = x, off2int = F)$result[x])
    beta[1] <- beta[1] + object$offset
    names(beta) <- var_names
    if (family$family == "binomial") {
      beta <- beta * 2
    }
    beta[-1] <- beta[-1] / standard_sd
    beta[1] <- beta[1] - crossprod(beta[-1], standard_mean)

    estimates <- dplyr::tibble(
      term = var_names,
      estimate = beta
    )
    return(estimates)
  }

}
