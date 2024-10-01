#' @name .fit.cor
#' @title Pearson's correlation for \code{tidyfit}
#' @description Calculates Pearson's correlation coefficient on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#' The function provides a wrapper for \code{stats::cor.test}. See \code{?cor.test} for more details.
#'
#' **Implementation**
#'
#' Results can be viewed using \code{coef}.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("cor", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("cor"), .mask = c("Date", "Industry"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.fit.chisq}} and \code{\link{m}} methods
#'
#' @importFrom stats cor.test
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.cor <- function(
    self,
    data = NULL
) {

  if (!is.null(self$args$weights)) {
    warning("cor cannot handle weights, weights are ignored")
  }

  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]
  var_names <- colnames(x)
  eval_fun_ <- function(...) {
    args <- list(...)
    tests <- purrr::map(var_names, function(nam) cor.test(x[,nam], y))
    names(tests) <- var_names
    class(tests) <- "custom.test"
    tests
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- eval_fun()
  .store_on_self(self, res)
  self$estimator <- "stats::cor.test"
  invisible(self)
}

.coef.custom.test <- function(object, self = NULL, ...) {

  estimates <- purrr::map_dfr(object, broom::tidy, .id = "term")
  if (!"estimate" %in% colnames(estimates)) {
    estimates <- dplyr::mutate(estimates, estimate = NA)
  }

  return(estimates)

}

.predict.custom.test <- function(object, data, self, ...) {
  if (is.null(self$args$predict_method)) {
    warning(paste0("No prediction method for type '", self$method, "'."))
    return(NULL)
  }
  estimates <- self$coef()
  n_features <- self$args$feature_count
  if (is.null(n_features)) n_features <- nrow(estimates)
  selected_variables <- estimates |>
    dplyr::filter(rank(-abs(.data$estimate)) <= n_features) |>
    pull("term")
  mf_fit <- stats::model.frame(self$original_formula, self$data)
  x_fit <- stats::model.matrix(self$formula, mf_fit)
  y_fit <- stats::model.response(mf_fit)
  new_data <- data.frame(dplyr::select(dplyr::as_tibble(x_fit), any_of(selected_variables)), check.names = FALSE)
  response_var <- all.vars(self$formula)[1]
  new_data[, response_var] <- y_fit
  new_formula <- paste(response_var, ".", sep = "~")
  pred_method_args <- self$args[names(self$args) != "predict_method"]
  pred_method_args <- append(pred_method_args,
                             list(model_method = self$args$predict_method,
                                  formula = as.formula(new_formula),
                                  data = new_data))
  pred_method <- do.call(m, pred_method_args)

  mf_pred <- stats::model.frame(self$original_formula, data)
  x_pred <- stats::model.matrix(self$formula, mf_pred)
  y_pred <- stats::model.response(mf_pred)
  pred_data <- data.frame(x_pred, check.names = FALSE)
  pred_data[, response_var] <- y_pred
  prediction <- predict(pred_method, pred_data)

  return(prediction)
}

.resid.custom.test <- function(object, self, ...) {
  warning(paste0("No residual method for type '", self$method, "'."))
  return(NULL)
}

.fitted.custom.test <- function(object, self, ...) {
  warning(paste0("No fitted method for type '", self$method, "'."))
  return(NULL)
}
