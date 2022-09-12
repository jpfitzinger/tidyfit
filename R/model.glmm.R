#' @name .model.glmm
#' @title Generalized linear mixed-effects model for \code{tidyfit}
#' @description Fits a linear or logistic mixed-effects model (GLMM) and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The function provides a wrapper for \code{lme4::glmer}.
#'
#' @param self a tidyFit R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted tidyFit class model.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data$Return <- ifelse(data$Return > 0, 1, 0)
#'
#' # Estimate model with random effects
#' fit <- classify(data, Return ~ CMA + (CMA | Industry), logit = m("glmm"), .mask = "Date")
#' fit
#'
#'
#' @seealso \code{\link{.model.glm}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.model.glmm <- function(
    self,
    data = NULL
) {
  ctr <- self$args[names(self$args) %in% methods::formalArgs(lme4::glmer)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(lme4::glmer, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "lme4::glmer"
  invisible(self)
}
