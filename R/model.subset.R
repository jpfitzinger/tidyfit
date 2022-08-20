#' @name .model.subset
#' @title Best subset regression and classification for \code{tidyfit}
#' @description Fits a best subset regression or classification and returns the results as a tibble. The function can be used with \code{\link{regress}} and \code{\link{classify}}.
#'
#' @details **Hyperparameters:**
#'
#' *None. Cross validation not applicable.*
#'
#' The best subset regression is estimated using \code{bestglm::bestglm} which is a wrapper around \code{leaps::regsubsets} for the regression case, and performs an exhaustive search for the classification case. For classification pass \code{family = "binomial"} to \code{control} or to the argument \code{...} in \code{\link{m}}.
#'
#' An intercept is always included and features are standardized with coefficients transformed to the original scale.
#'
#' Forward or backward selection can be performed by passing \code{method = "forward"} or \code{method = "backward"} to \code{\link{m}}.
#'
#' @param x Input matrix or data.frame, of dimension \eqn{(N\times p)}{(N x p)}; each row is an observation vector.
#' @param y Response variable.
#' @param control  Additional arguments passed to \code{bestglm::bestglm}.
#' @param ... Not used.
#' @return A 'tibble'.
#' @author Johann Pfitzinger
#' @references
#' A.I. McLeod, Changjiang Xu and Yuanhao Lai (2020).
#' bestglm: Best Subset GLM and Regression Utilities.
#' R package version 0.37.3. URL https://CRAN.R-project.org/package=bestglm.
#'
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' fit <- m("subset", x, y)
#' fit
#'
#' @seealso \code{\link{m}} method
#'
#' @importFrom bestglm bestglm
#' @importFrom purrr quietly partial
#' @importFrom methods formalArgs

.model.subset <- function(
    x = NULL,
    y = NULL,
    control = NULL,
    ...
) {

  control <- control[names(control) %in% methods::formalArgs(bestglm::bestglm)]
  if (is.null(control$family)) {
    f <- gaussian()
  } else {
    f <- control$family
  }

  control <- control[names(control) != "family"]

  Xy <- data.frame(x, y, check.names = FALSE)

  quiet_bestglm <- purrr::quietly(bestglm::bestglm)

  if (f$family == "gaussian") {
    part_bestglm <- purrr::partial(quiet_bestglm, family = gaussian)
  } else if (f$family == "binomial") {
    part_bestglm <- purrr::partial(quiet_bestglm, family = binomial)
  }

  m <- do.call(part_bestglm, append(list(Xy = Xy), control))$result

  var_names <- colnames(x)
  coef_stats <- summary(m$BestModel)$coefficients
  rownames(coef_stats) <- c("(Intercept)", var_names[as.logical(m$BestModels[1,-ncol(m$BestModels)])])
  cols_omit <- var_names[!as.logical(m$BestModels[1,-ncol(m$BestModels)])]
  var_names <- c("(Intercept)", var_names)
  if (length(cols_omit)>0) {
    coef_stats_omit <- data.frame(matrix(NA, nrow = length(cols_omit), ncol = 4), row.names = cols_omit)
    coef_stats_omit[, 1] <- 0
    colnames(coef_stats_omit) <- colnames(coef_stats)
    coef_stats <- rbind(coef_stats, coef_stats_omit)[var_names,]
  }


  out <- tibble(
    variable = var_names,
    beta = coef_stats[, 1],
    family = list(f),
    `s.e.` = coef_stats[, 2],
    `t value` = coef_stats[, 3],
    `p value` = coef_stats[, 4],
    `Adj. R-squared` = summary(m$BestModel)$adj.r.squared
  )
  if (length(control) > 0) {
    out <- dplyr::bind_cols(out, dplyr::as_tibble(.func_to_list(control)))
  }

  return(out)

}
