#' @name .fit.boost
#' @title Gradient boosting regression for \code{tidyfit}
#' @description Fits a gradient boosting regression or classification on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' - \code{mstop} (*number of boosting iterations*)
#' - \code{nu} (*step size*)
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The gradient boosting regression is performed using \code{mboost::glmboost}. See \code{?glmboost} for more details.
#'
#' **Implementation**
#'
#' Features are standardized by default with coefficients transformed to the original scale.
#'
#' If no hyperparameter grid is passed (\code{is.null(control$mstop)} and \code{is.null(control$nu)}), the default grid is used with \code{mstop = c(100, 500, 1000, 5000)} and \code{nu = c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25)}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' T. Hothorn, P. Buehlmann, T. Kneib, M. Schmid, and B. Hofner (2022). mboost: Model-Based Boosting, R package version 2.9-7,https://CRAN.R-project.org/package=mboost.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("boost", Return ~ ., data, nu = 0.1, mstop = 100)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("boost", nu = c(0.1, 0.05), mstop = 100),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom stats coef
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.boost <- function(
    self,
    data = NULL
) {
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  if ("(Intercept)" %in% colnames(x)) x <- x[, -1]
  if (self$mode == "regression") {
    self$set_args(family = mboost::Gaussian(), overwrite = FALSE)
  } else if (self$mode == "classification") {
    self$set_args(family = mboost::Binomial(), overwrite = FALSE)
    y <- as.factor(y)
  }
  glmboost.matrix_args <- c("weights", "offset", "family", "na.action",
                            "contrasts.arg", "oobweights",
                            "nu", "mstop")
  ctr <- self$args[names(self$args) %in% glmboost.matrix_args]
  standard_mean <- apply(x, 2, mean)
  standard_sd <- apply(x, 2, stats::sd)
  xs <- as.matrix(scale(x, center = standard_mean, scale = standard_sd))
  xs <- data.frame(`(Intercept)` = 1, xs, check.names = FALSE)

  control <- mboost::boost_control(mstop = ctr$mstop, nu = ctr$nu)
  ctr <- within(ctr, rm("mstop", "nu"))
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(mboost::glmboost, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(x = data.matrix(xs), y = y, control = control,
                             center = F), ctr))
  .store_on_self(self, res)
  self$fit_info <- list(standard_mean = standard_mean, standard_sd = standard_sd, var_names = colnames(xs))
  invisible(self)
}
