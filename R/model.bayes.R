#' @name .model.bayes
#' @title Bayesian generalized linear regression for \code{tidyfit}
#' @description Fits a Bayesian regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{arm::bayesglm}. See \code{?bayesglm} for more details.
#'
#' **Implementation**
#'
#' *No implementation notes*
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' Gelman A, Su Y (2021). _arm: Data Analysis Using Regression and Multilevel/Hierarchical Models_. R package version 1.12-2, <https://CRAN.R-project.org/package=arm>.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("bayes", Return ~ ., data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("bayes"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#'
#' @seealso \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.model.bayes <- function(
    self,
    data = NULL
) {

  ctr <- self$args[names(self$args) %in% methods::formalArgs(arm::bayesglm)]
  ctr$model <- FALSE
  ctr$x <- FALSE
  ctr$y <- FALSE
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(arm::bayesglm, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "arm::bayesglm"
  invisible(self)

}
