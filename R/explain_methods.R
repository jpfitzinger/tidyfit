#' @importFrom stats rnorm

.explain.default <- function(
    object,
    self,
    method,
    ...) {
  additional_args <- list(...)
  if (!is.null(method)) {
    possible_methods <- c("shapley_reg", "rel_weights")
    if (!method %in% possible_methods) {
      stop(sprintf("available 'explain' methods for '%s' objects are: %s", self$method, paste(possible_methods, collapse=", ")))
    }
  } else {
    method = "shapley_reg"
    if (!"type" %in% names(additional_args))
      warning(sprintf("the default importance metric for method '%s' is 'shapley_reg' using `type='lmg'` in `relaimpo` package", self$method))
  }
  if (!"type" %in% names(additional_args)) {
    additional_args["type"] = ifelse(method == "shapley_reg", "lmg", "genizi")
  }
  mf <- stats::model.frame(self$formula, self$data)
  x <- stats::model.matrix(self$formula, mf)

  target_var <- all.vars(self$formula)[1]
  fitted_values <- self$fitted()$fitted
  resid_values <- self$resid()$residual
  y <- mf[[target_var]]
  R2 <- 1 - sum(resid_values^2)/sum((y - mean(y))^2)

  # keep only columns with non-zero coefficients
  selected_vars <- self$coef()$term
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]
  if (length(selected_vars) == 0) return(tibble(term = character(), importance = numeric()))
  if (length(selected_vars) == 1) {
    result_df <- tibble(
      term = colnames(x[, -1])[selected_vars],
      importance = R2
    ) |>
      tidyr::complete(term = colnames(x)[-1], fill = list(importance = 0))
    return(result_df)
  }
  data <- as.data.frame(x[, -1][, selected_vars])

  # Add small epsilon
  data[target_var] <- fitted_values + stats::rnorm(length(fitted_values), sd = sd(fitted_values) * 0.001)
  args <- list(formula = self$formula, data = data)
  args <- append(args, additional_args)
  result <- do.call(relaimpo::calc.relimp.formula, args)
  result <- attr(result, additional_args$type)

  result_df <- tibble(
    term = names(result),
    importance = result * R2
  ) |>
    tidyr::complete(term = colnames(x)[-1], fill = list(importance = 0))
  return (result_df)
}
