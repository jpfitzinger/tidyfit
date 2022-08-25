.model <- function(formula, data, control, identifier) {
  UseMethod(".model", identifier)
}
