#' @name .fit.lasso
#' @title Lasso regression and classification for \code{tidyfit}
#' @description Fits a linear regression or classification with L1 penalty on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{lambda} *(L1 penalty)*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The Lasso regression is estimated using \code{glmnet::glmnet} with \code{alpha = 1}. See \code{?glmnet} for more details. For classification pass \code{family = "binomial"} to \code{...} in \code{\link{m}} or use \code{\link{classify}}.
#'
#' **Implementation**
#'
#' If the response variable contains more than 2 classes, a multinomial response is used automatically.
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$lambda)}), \code{dials::grid_regular()} is used to determine a sensible default grid. The grid size is 100. Note that the grid selection tools provided by \code{glmnet::glmnet} cannot be used (e.g. \code{dfmax}). This is to guarantee identical grids across groups in the tibble.
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
#' fit <- m("lasso", Return ~ ., data, lambda = 0.5)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("lasso", lambda = c(0.1, 0.5)),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.enet}}, \code{\link{.fit.ridge}}, \code{\link{.fit.adalasso}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data
#' @importFrom methods formalArgs

.fit.lasso <- function(
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

.top_vars.glmnet <- function(object, self = NULL, n = NULL, ...) {
  coefs_df <- self$coef()
  # check if pmax is set to correct value
  refit <- F
  if (nrow(coefs_df) - 1 > n) {
    if(is.null(self$args$pmax)) {
      warning(sprintf("'%s' method: parameter 'pmax' is not set. will attempt to refit.", self$method), call. = F)
      refit <- T
    } else if(self$args$pmax > n) {
      warning(sprintf("'%s' method: parameter 'pmax' too large. will attempt to refit.", self$method), call. = FALSE)
      refit <- T
    }
  }
  if(refit) {
    refit_self <- self$clone()
    refit_self$set_args(lambda = NULL, pmax = n)
    refit_self$fit(refit_self$data)
    if (any(is.finite(refit_self$object$lambda))) {
      ix <- last(which(colSums(as.matrix(refit_self$object$beta) != 0) <= n))
      selected_grid <- refit_self$inner_grid[ix, 1]
      refit_coefs_df <- dplyr::filter(refit_self$coef(), .data$grid_id == selected_grid)
      refit_coefs_df$grid_id <- unique(coefs_df$grid_id)
      coefs_df <- refit_coefs_df
    } else {
      warning(sprintf("'%s' method: failed to refit. try a different value for 'n'.", self$method), call. = FALSE)
      return(tibble(term = character()))
    }
  }
  coefs_df <- coefs_df |>
    dplyr::filter(.data$term != "(Intercept)") |>
    dplyr::select("term")
  return(coefs_df)
}
