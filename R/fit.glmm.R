#' @name .fit.glmm
#' @title Generalized linear mixed-effects model for \code{tidyfit}
#' @description Fits a linear or logistic mixed-effects model (GLMM) on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#' The function provides a wrapper for \code{lme4::glmer}. See \code{?glmer} for more details.
#'
#' **Implementation**
#'
#' *No implementation notes*
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#' data$Return <- ifelse(data$Return > 0, 1, 0)
#'
#' # Estimate model with random effects
#' fit <- classify(data, Return ~ CMA + (CMA | Industry), logit = m("glmm"),
#'                 .mask = "Date")
#' fit
#'
#'
#' @seealso \code{\link{.fit.glm}} and \code{\link{m}} methods
#'
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.glmm <- function(
    self,
    data = NULL
) {
  if (self$mode == "classification") {
    self$set_args(family = binomial, overwrite = FALSE)
    response_var <- all.vars(self$formula)[1]
    class_names_map <- levels(data[,response_var])
    names(class_names_map) <- c(0, 1)
    data[,response_var] <- ifelse(data[,response_var]==class_names_map[1], 0, 1)
  }
  ctr <- self$args[names(self$args) %in% methods::formalArgs(lme4::glmer)]
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(lme4::glmer, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  if (self$mode == "classification") {
    self$fit_info <- list(class_names_map = class_names_map)
  }
  self$estimator <- "lme4::glmer"
  invisible(self)
}
