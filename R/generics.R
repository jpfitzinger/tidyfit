# tidyFit generics

#' @importFrom stats coef predict resid fitted
#' @importFrom broom tidy glance
#' @importFrom generics var_imp
#' @export var_imp

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
tidy.tidyFit <- function(x, ...) {
  broom::tidy(x$object, ...)
}

#' @export
glance.tidyFit <- function(x, ...) {
  broom::glance(x$object, ...)
}

#' @export
var_imp.tidyFit <- function(object, ...) {
  generics::var_imp(object$object, ...)
}
