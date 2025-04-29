#' @name .fit.anova
#' @title ANOVA for \code{tidyfit}
#' @description Performs Analysis of Variance on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} or \code{\link{classify}}.
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
#' The function provides a wrapper for \code{stats::anova}. See \code{?anova} for more details.
#'
#' First a \code{glm} model is fitted which is passed to \code{anova}.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("anova", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("anova"), .mask = c("Date", "Industry"))
#' tidyr::unnest(coef(fit), model_info)
#'
#' @seealso \code{\link{.fit.lm}}, \code{\link{.fit.glm}} and \code{\link{m}} methods
#'
#' @importFrom stats anova
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.anova <- function(
    self,
    data = NULL
) {
  ctr <- self$args[names(self$args) %in% methods::formalArgs(stats::glm)]
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)

  if (self$mode == "regression")
    self$set_args(family = gaussian(), overwrite = FALSE)
  if (self$mode == "classification") {
    self$set_args(family = binomial(), overwrite = FALSE)
    class_names_map <- levels(y)
    names(class_names_map) <- 1:length(levels(y))
    mf[, 1] <- as.numeric(as.factor(y)) - 1
  }

  ctr <- self$args[names(self$args) %in% methods::formalArgs(stats::glm)]
  eval_fun_ <- function(...) {
    args <- list(...)
    mod <- do.call(stats::glm, args)
    stats::anova(mod)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = mf), ctr))
  .store_on_self(self, res)
  if (self$mode == "classification") {
    self$fit_info <- list(class_names_map = class_names_map)
  }
  invisible(self)
}

.coef.anova <- function(object, self = NULL, ...) {
  estimates <- broom::tidy(object)
  return(estimates)
}
