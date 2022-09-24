# Generic methods to generate tidy fitted frames

#' @importFrom dplyr tibble

.fitted.glmnet <- function(object, lambda = NULL, family = NULL, ...) {
  x <- object$call$x
  pred_mat <- stats::predict(object, x, type = "response", s = lambda)

  if (length(dim(pred_mat))==3) {
    class_vals <- dimnames(pred_mat)[[2]]
  } else {
    class_vals <- NULL
  }
  pred <- pred_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(row_n = dplyr::row_number())
  if (family == "multinomial") {
    pred <- pred %>%
      tidyr::pivot_longer(-dplyr::any_of(c("row_n")),
                          names_to = c("class", "grid_id"),
                          values_to = "fitted",
                          names_sep = "\\.")
  } else {
    pred <- pred %>%
      tidyr::gather("grid_id", "fitted", -dplyr::any_of(c("row_n")))
  }
  pred <- pred %>%
    dplyr::select(-.data$row_n, -.data$grid_id)
  if (length(class_vals)==2) {
    pred <- pred %>%
      dplyr::filter(.data$class == sort(class_vals)[2]) %>%
      dplyr::select(-.data$class)
  }
  return(pred)
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
