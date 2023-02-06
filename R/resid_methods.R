# Generic methods to generate tidy residual frames

#' @importFrom dplyr tibble

.resid.default <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = resid(object)
  )
  return(residuals)
}

.resid.glmnet <- function(object, self = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = object$call$y - drop(predict(object, object$call$x, s = self$args$lambda))
  )
  return(residuals)
}

.resid.bma <- function(object, self = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = object$arguments$X.data[,1] - predict(object)
  )
  return(residuals)
}

.resid.mvr <- function(object, self = NULL, ...) {
  residuals <- dplyr::tibble(
    residual = drop(resid(object)[,,self$args$ncomp])
  )
  return(residuals)
}

.resid.glmboost <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = drop(resid(object))
  )
  return(residuals)
}

.resid.shrinkTVP <- function(object, ...) {
  residuals <- dplyr::tibble(
    residual = colMeans(t(resid(object)))
  )
  return(residuals)
}

.resid.MSM.lm <- function(object, self = NULL, ...) {
  condMean <- object@Fit@CondMean
  probs <- object@Fit@smoProb[-1,]
  residuals <- dplyr::tibble(
    residual = object@model$model[,1] - rowSums(condMean * probs)
  )
  return(residuals)
}
