#' @name .fit.lm
#' @title Linear regression for \code{tidyfit}
#' @description Fits a linear regression on a 'tidyFit' \code{R6} class. The function can be used with \code{\link{regress}}.
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
#' The function provides a wrapper for \code{stats::lm}. See \code{?lm} for more details.
#'
#' **Implementation**
#'
#' An argument \code{vcov.} can be passed in control or to \code{...} in \code{\link{m}} to estimate the model with robust standard errors. \code{vcov.} can be one of "BS", "HAC", "HC" and "OPG" and is passed to the \code{sandwich} package.
#'
#' @author Johann Pfitzinger
#'
#' @examples
#' # Load data
#' data <- tidyfit::Factor_Industry_Returns
#'
#' # Stand-alone function
#' fit <- m("lm", Return ~ `Mkt-RF` + HML + SMB, data)
#' fit
#'
#' # Within 'regress' function
#' fit <- regress(data, Return ~ ., m("lm"), .mask = c("Date", "Industry"))
#' coef(fit)
#'
#' # With robust standard errors
#' fit <- m("lm", Return ~ `Mkt-RF` + HML + SMB, data, vcov. = "HAC")
#' fit
#'
#' @seealso \code{\link{.fit.robust}}, \code{\link{.fit.glm}} and \code{\link{m}} methods
#'
#' @importFrom stats lm
#' @importFrom purrr safely quietly
#' @importFrom methods formalArgs

.fit.lm <- function(
    self,
    data = NULL
) {
  ctr <- self$args[names(self$args) %in% methods::formalArgs(stats::lm)]
  ctr$model <- TRUE
  ctr$x <- FALSE
  ctr$y <- FALSE
  eval_fun_ <- function(...) {
    args <- list(...)
    do.call(stats::lm, args)
  }
  eval_fun <- purrr::safely(purrr::quietly(eval_fun_))
  res <- do.call(eval_fun,
                 append(list(formula = self$formula, data = data), ctr))
  .store_on_self(self, res)
  self$estimator <- "stats::lm"
  invisible(self)
}

.explain.lm <- function(
    object,
    self,
    method = c("lmg", "pmvd", "src", "pcc", "johnson", "partition_shap"),
    explainer_model = NULL,
    ...
) {
  # Check packages
  if (method %in% c("lmg", "pmvd", "src", "pcc", "johnson") &
      !"sensitivity" %in% utils::installed.packages())
    stop("Package 'sensitivity' is necessary to explain 'lm' models. Use install.packages('sensitivity') to install it.")

  if (method %in% c("partition_shap") &
      !"glmnet" %in% utils::installed.packages())
    stop("Package 'glmnet' is necessary to explain 'lm' models. Use install.packages('glmnet') to install it.")

  additional_args <- list(...)
  method <- match.arg(method)
  additional_args$logistic <- self$mode == "classification"
  if (method == "partition_shap" & is.null(explainer_model))
    stop("provide an explainer_model, e.g. m('ridge', lambda = 0)")

  mf <- stats::model.frame(self$formula, self$data)
  x <- stats::model.matrix(self$formula, mf)
  x <- as.data.frame(x[, colnames(x)!="(Intercept)"])
  y <- stats::model.response(mf)

  method_fx <- list(
    lmg = function(...) {
      args <- list(...)
      res <- do.call(sensitivity::lmg, args[names(args) %in% methods::formalArgs(sensitivity::lmg)])
      res$lmg[, "original"]
    },
    pmvd = function(...) {
      args <- list(...)
      res <- do.call(sensitivity::pmvd, args[names(args) %in% methods::formalArgs(sensitivity::pmvd)])
      res$pmvd[, "original"]
    },
    johnson = function(...) {
      args <- list(...)
      res <- do.call(sensitivity::johnson, args[names(args) %in% methods::formalArgs(sensitivity::johnson)])
      res$johnson[, "original"]
    },
    src = function(...) {
      args <- list(...)
      res <- do.call(sensitivity::src, args[names(args) %in% methods::formalArgs(sensitivity::src)])
      rank <- ifelse(!is.null(args$rank), args$rank, FALSE)
      res <- ifelse(rank, res$SRRC, res$SRC)[[1]]
      res
    },
    pcc = function(...) {
      args <- list(...)
      res <- do.call(sensitivity::pcc, args[names(args) %in% methods::formalArgs(sensitivity::pcc)])
      rank <- ifelse(!is.null(args$rank), args$rank, FALSE)
      semi <- ifelse(!is.null(args$semi), args$semi, FALSE)
      if (rank) {
        res <- ifelse(semi, res$SPRCC, res$PRCC)
      } else {
        res <- ifelse(semi, res$SPCC, res$PCC)
      }
      res <- res[[1]]
      res
    },
    partition_shap = function(...) {
      args <- list(...)
      partition_shap(self, explainer_model_object = args$explainer_model)
    }
  )

  args <- list(X = data.frame(x), y = y)
  args <- append(args, additional_args)
  result <- do.call(method_fx[[method]], args)
  result_df <- tibble(
    term = colnames(x),
    importance = result
  )
  return (result_df)

}
