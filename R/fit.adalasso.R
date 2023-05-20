#' @name .fit.adalasso
#' @title Adaptive Lasso regression or classification for \code{tidyfit}
#' @description Fits an adaptive Lasso regression or classification on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L1 penalty)*
#' - \code{lambda_ridge} *(L2 penalty (default = 0.01) used in the first step to determine the penalty factor)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The adaptive Lasso is a weighted implementation of the Lasso algorithm, with covariate-specific weights obtained using an initial regression fit (in this case, a ridge regression with \code{lambda = lambda_ridge}, where \code{lambda_ridge} can be passed as an argument). The adaptive Lasso is computed using the \code{glmnet::glmnet} function. See \code{?glmnet} for more details. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
#'
#' **Implementation**
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#'
#' @author Johann Pfitzinger
#' @references
#' Zou, H. (2006).
#' The Adaptive Lasso and Its Oracle Properties.
#' Journal of the American Statistical Association, 101(476), 1418-1429.
#'
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
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
#' fit <- regress(data, Return ~ ., m("adalasso", lambda = c(0.1, 0.5)),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lasso}}, \code{\link{.fit.enet}}, \code{\link{.fit.ridge}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats coef
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.adalasso <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  self$set_args(alpha = 1, lambda = sort(self$args$lambda, TRUE))
  # Prepare 'family' arg
  if (self$mode == "regression") {
    self$set_args(family = "gaussian", overwrite = FALSE)
  }
  if (self$mode == "classification") {
    self$set_args(family = "multinomial", overwrite = FALSE)
    class_names_map <- levels(y)
    names(class_names_map) <- 1:length(levels(y))
    y <- as.numeric(as.factor(y))
  }

  ctr <- self$args[names(self$args) %in% methods::formalArgs(glmnet::glmnet)]
  if (is.null(self$args$lambda_ridge)) {
    lambda_ridge <- 0.01
  } else {
    lambda_ridge <- self$args$lambda_ridge
  }

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(glmnet::glmnet, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))

  control_ <- ctr[!names(ctr) %in% c("lambda", "alpha", "dfmax", "pmax")]
  penalty_mod <- do.call(eval_fun, append(list(x = x, y = y, alpha = 0,
                                               lambda = lambda_ridge, intercept = incl_intercept), control_))
  if (is.null(penalty_mod$error)) {
    coefs <- stats::coef(penalty_mod$result$result)
    if (!inherits(coefs, "list")) coefs <- list(coefs)
    penalty_factor <- sapply(coefs, function(cf) {
      cf <- cf[-1]
      cf <- abs(cf)^(-1)
      cf[is.infinite(cf) | is.na(cf)] <- 0
      cf <- pmin(cf, 1e4)
      cf <- cf + 1e-8
      return(cf)
    })
    penalty_factor <- rowMeans(penalty_factor)

    res <- do.call(eval_fun,
                   append(list(x = x, y = y, intercept = incl_intercept,
                               penalty.factor = penalty_factor), ctr))
  } else {
    res <- penalty_mod
  }

  .store_on_self(self, res)
  self$set_args(lambda = res$result$result$lambda)
  self$estimator <- "glmnet::glmnet"
  self$inner_grid <- data.frame(
    grid_id = paste(substring(self$grid_id, 1, 4), formatC(1:length(self$args$lambda), 2, flag = "0"), sep = "|"),
    lambda = self$args$lambda
  )
  if (self$mode == "classification") {
    self$fit_info <- list(class_names_map = class_names_map)
  }
  invisible(self)

}
