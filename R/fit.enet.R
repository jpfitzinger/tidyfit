#' @name .fit.enet
#' @title ElasticNet regression or classification for \code{tidyfit}
#' @description Fits an ElasticNet regression or classification on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(penalty)*
#' - \code{alpha} *(L1-L2 mixing parameter)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The ElasticNet regression is estimated using \code{glmnet::glmnet}. See \code{?glmnet} for more details. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
#'
#' **Implementation**
#'
#' If the response variable contains more than 2 classes, a multinomial response is used automatically.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)} and \code{is.null(control$alpha)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100 for \code{lambda} and 5 for \code{alpha}. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. URL https://www.jstatsoft.org/v33/i01/.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("enet", Return ~ ., data, lambda = c(0, 0.1), alpha = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("enet", alpha = c(0, 0.5), lambda = c(0.1)),
#'                .mask = c("Date", "Industry"), .cv = "vfold_cv")
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lasso}}, \code{\link{.fit.adalasso}}, \code{\link{.fit.ridge}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.enet <- function(
    self,
    data = NULL
) {

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  self$set_args(lambda = sort(self$args$lambda, TRUE))
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

  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(glmnet::glmnet, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = x, y = y, intercept = incl_intercept), ctr))
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
