# Generic methods to generate tidy fitted frames

#' @importFrom dplyr tibble

.fitted.glmnet <- function(object, lambda = NULL, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(predict(object, object$call$x, s = lambda))
  )
  return(fitted)
}

.fitted.lm <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(object)
  )
  return(fitted)
}

.fitted.glm <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(object)
  )
  return(fitted)
}

.fitted.rq <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(object)
  )
  return(fitted)
}

.fitted.rlm <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(object)
  )
  return(fitted)
}

.fitted.mvr <- function(object, standard_sd = NULL, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(fitted(object))
  )
  return(fitted)
}

.fitted.glmboost <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(fitted(object))
  )
  return(fitted)
}

.fitted.merMod <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = fitted(object)
  )
  return(fitted)
}

.fitted.shrinkTVP <- function(object, ...) {
  fitted <- dplyr::tibble(
    fitted = colMeans(t(fitted(object)))
  )
  return(fitted)
}

.fitted.MSM.lm <- function(object, index_var = NULL, ...) {
  condMean <- object@Fit@CondMean
  probs <- object@Fit@smoProb[-1,]
  fitted <- dplyr::tibble(
    fitted = rowSums(condMean * probs)
  )
  return(fitted)
}

.fitted.cv.hfr <- function(object, kappa_grid = NULL, ...) {
  fitted <- dplyr::tibble(
    fitted = drop(predict(object, kappa = kappa_grid))
  )
  return(fitted)

}
