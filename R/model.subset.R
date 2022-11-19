#' @name .model.subset
#' @title Best subset regression and classification for \code{tidyfit}
#' @description Fits a best subset regression or classification on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' **Important method arguments (passed to \code{\link{m}})**
#'
#'  - \code{method} (e.g. 'forward', 'backward')
#'  - \code{IC} (information criterion, e.g. 'AIC')
#'
#' The best subset regression is estimated using \code{bestglm::bestglm} which is a wrapper around \code{leaps::regsubsets} for the regression case, and performs an exhaustive search for the classification case. See \code{?bestglm} for more details.
#'
#' **Implementation**
#'
#' Forward or backward selection can be performed by passing \code{method = "forward"} or \code{method = "backward"} to \code{\link{m}}.
#'
#' @param self a 'tidyFit' R6 class.
#' @param data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @return A fitted 'tidyFit' class model.
#' @author Johann Pfitzinger
#' @references
#' A.I. McLeod, Changjiang Xu and Yuanhao Lai (2020).
#' \emph{bestglm: Best Subset GLM and Regression Utilities.
#' R package version 0.37.3.} URL https://CRAN.R-project.org/package=bestglm.\cr
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("subset", Return ~ ., data, method = c("forward", "backward"))
#' tidyr::unnest(fit, settings)
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("subset", method = "forward"),
#'                .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' @seealso \code{\link{.model.lm}} and \code{\link{m}} methods
#'
#' @importFrom purrr quietly safely partial
#' @importFrom methods formalArgs

.model.subset <- function(
    self,
    data = NULL
) {
  if (is.null(self$mode)) self$mode <- "regression"
  ctr <- self$args[names(self$args) %in% methods::formalArgs(bestglm::bestglm)]
  mf <- stats::model.frame(self$formula, data)
  x <- stats::model.matrix(self$formula, mf)
  y <- stats::model.response(mf)
  incl_intercept <- "(Intercept)" %in% colnames(x)
  if (incl_intercept) x <- x[, -1]

  # TODO: Request 'bestglm' maintainers to fix this
  ctr$weights <- NULL
  if (!is.null(self$args$weights)) {
    warning("subset cannot handle weights, weights are ignored")
  }

  Xy <- data.frame(x, y, check.names = FALSE)
  var_names_map <- .names_map(colnames(Xy))
  eval_fun_ <- function(...) {
    m <- bestglm::bestglm(...)
    m$BestModel
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  if (self$mode == "regression") {
    eval_fun <- purrr::partial(eval_fun, family = gaussian)
  }
  if (self$mode == "classification") {
    eval_fun <- purrr::partial(eval_fun, family = binomial)
  }
  res <- do.call(eval_fun, append(
    list(Xy = data.frame(Xy), intercept = incl_intercept),
    ctr))
  .store_on_self(self, res)
  self$fit_info <- list(names_map = var_names_map)
  self$estimator <- "bestglm::bestglm"
  invisible(self)
}
