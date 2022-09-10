# tidyFit generics

#' @importFrom stats coef predict resid fitted
#' @importFrom broom tidy glance

coef.tidyFit <- function(object, ...) {
  stats::coef(object$object, ...)
}

predict.tidyFit <- function(object, ...) {
  stats::predict(object$object, ...)
}

plot.tidyFit <- function(object, ...) {
  plot(object$object, ...)
}

fitted.tidyFit <- function(object, ...) {
  stats::fitted(object$object, ...)
}

resid.tidyFit <- function(object, ...) {
  stats::resid(object$object, ...)
}

summary.tidyFit <- function(object, ...) {
  summary(object$object, ...)
}

tidy.tidyFit <- function(object, ...) {
  broom::tidy(object$object, ...)
}

glance.tidyFit <- function(object, ...) {
  broom::glance(object$object, ...)
}
