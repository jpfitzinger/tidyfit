#' @name .fit.glm
#' @title Generalized linear regression for \code{tidyfit}
#' @description Fits a linear or logistic regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{stats::glm}. See \code{?glm} for more details.
#'
#' **Implementation**
#'
#' *No implementation notes*
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data$Return <- ifelse(data$Return > 0, 1, 0)
#'
#' # Stand-alone function
#' fit <- m("glm", Return ~ ., data)
#' fit
#'
#' # Within 'classify' function
#' fit <- classify(data, Return ~ ., m("glm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.fit.lm}} and \code{\link{m}} methods
#'
#' @importFrom stats glm coef gaussian binomial
#' @importFrom methods formalArgs

.fit.glm <- function(
    self,
    data = NULL
) {

  if (self$mode == "regression")
    self$set_args(family = gaussian(), overwrite = FALSE)
  if (self$mode == "classification")
    self$set_args(family = binomial(), overwrite = FALSE)
  ctr <- self$args[names(self$args) %in% methods::formalArgs(stats::glm)]
  ctr$model <- FALSE
  ctr$x <- FALSE
  ctr$y <- FALSE
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(stats::glm, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "stats::glm"
  invisible(self)

}
