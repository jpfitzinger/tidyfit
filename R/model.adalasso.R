#' @name .model.adalasso
#' @title Adaptive Lasso regression or classification for \code{tidyfit}
#' @description Fits an adaptive Lasso regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L1 penalty)*
#'
#' The adaptive Lasso is a weighted implementation of the Lasso algorithm, with covariate-specific weights obtained using an initial regression fit (in this case, a ridge regression with \code{lambda = 0.01}). The adaptive Lasso is computed using the \code{glmnet::glmnet} function. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param control  Additional arguments passed to \code{glmnet::glmnet}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Zou, H. (2006).
#' The Adaptive Lasso and Its Oracle Properties.
#' Journal of the American Statistical Association, 101(476), 1418-1429.
#'
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#' Regularization Paths for Generalized Linear Models via Coordinate Descent.
#' Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("adalasso", Return ~ ., data, lambda = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("adalasso", lambda = c(0.1, 0.5)), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lasso}}, \code{\link{.model.enet}}, \code{\link{.model.ridge}} and \code{\link{m}} methods
#'
#' @importFrom dplyr mutate as_tibble bind_cols select
#' @importFrom tidyr gather
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.model.adalasso <- function(
    formula = NULL,
    data = NULL,
    control = NULL,
    ...
) {

  control <- control[names(control) %in% methods::formalArgs(glmnet::glmnet)]
  control <- control[names(control) != "alpha"]
  control$lambda <- sort(control$lambda, TRUE)

  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)

  # Prepare 'family' arg
  if (is.null(control$family)) {
    control$family <- gaussian()
  }
  if (inherits(control$family, "character")) {
    f_name <- control$family
  } else {
    f_name <- control$family$family
  }
  if (!f_name %in% c("gaussian", "binomial", "multinomial"))
    stop("invalid 'family' argument")
  if (f_name == "gaussian") {
    control$family <- "gaussian"
    f <- gaussian()
  } else {
    control$family <- "multinomial"
    f <- binomial()
    y <- as.factor(y)
  }

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  control_ <- control[names(control) != "lambda"]
  penalty_mod <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 0,
                                                     lambda = 0.01, intercept = incl_intercept), control_))
  coefs <- stats::coef(penalty_mod)
  if (!inherits(coefs, "list")) coefs <- list(coefs)
  penalty_factor <- sapply(coefs, function(cf) {
    cf <- cf[-1]
    cf <- abs(cf)^(-1)
    cf[is.infinite(cf) | is.na(cf)] <- 0
    return(cf)
  })
  penalty_factor <- rowMeans(penalty_factor)

  m <- do.call(glmnet::glmnet, append(list(x = x, y = y, alpha = 1, intercept = incl_intercept, penalty.factor = penalty_factor), control))

  control$lambda <- m$lambda
  grid_ids <- formatC(1:length(control$lambda), 2, flag = "0")

  model_handler <- purrr::partial(.handler.glmnet, object = m, grid_ids = grid_ids,
                                  formula = formula, family = control$family)

  control <- control[!names(control) %in% c("weights")]
  if (length(control) > 0) {
    settings <- dplyr::as_tibble(.func_to_list(control)) %>%
      dplyr::mutate(grid_id = grid_ids)
  } else {
    settings <- NULL
  }
  settings <- tidyr::nest(settings, settings = dplyr::everything())

  out <- tibble(
    estimator = "glmnet::glmnet",
    handler = list(model_handler),
    settings
  )

  return(out)

}
