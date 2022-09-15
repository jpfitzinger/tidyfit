# tidyFit generics

#' @importFrom stats coef predict resid fitted
#' @importFrom broom tidy glance

#' @export
coef.tidyFit <- function(object, ...) {
  stats::coef(object$object, ...)
}

#' @export
predict.tidyFit <- function(object, ...) {
  stats::predict(object$object, ...)
}

#' @export
plot.tidyFit <- function(x, ...) {
  plot(x$object, ...)
}

#' @export
fitted.tidyFit <- function(object, ...) {
  stats::fitted(object$object, ...)
}

#' @export
resid.tidyFit <- function(object, ...) {
  stats::resid(object$object, ...)
}

#' @export
summary.tidyFit <- function(object, ...) {
  summary(object$object, ...)
}

#' @export
tidy.tidyFit <- function(object, ...) {
  broom::tidy(object$object, ...)
}

#' @export
glance.tidyFit <- function(object, ...) {
  broom::glance(object$object, ...)
}
