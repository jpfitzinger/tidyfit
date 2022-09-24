# Generic methods to generate tidy residual frames

#' @importFrom dplyr tibble

.resid.glmnet <- function(object, lambda = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = object$call$y - drop(predict(object, object$call$x, s = lambda))
  )
  return(residuals)
}

.resid.lm <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.glm <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.rq <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.rlm <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.mvr <- function(object, standard_sd = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.glmboost <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = drop(resid(object))
  )
  return(residuals)
}

.resid.merMod <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.shrinkTVP <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = colMeans(t(resid(object)))
  )
  return(residuals)
}

.resid.MSM.lm <- function(object, index_var = NULL, ...) {
  condMean <- object@Fit@CondMean
  probs <- object@Fit@smoProb[-1,]
  residuals <- dplyr::tibble(
    residual = object@model$model[,1] - rowSums(condMean * probs)
  )
  return(residuals)
}

.resid.cv.hfr <- function(object, kappa_grid = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = object$y - drop(predict(object, kappa = kappa_grid))
  )
  return(residuals)

}
