# Partition explainer

.partimp_explainer <- function(object, ...) {
  args <- list(...)
  args[["formula"]] <- object$formula
  if (is.null(args$fExplain)) fExplain_obj <- object else fExplain_obj <- args$fExplain$model_object[[1]]
  if (!is.null(args$fEstimate))
    warning("fModel will be ignored, will use the fitted object instead", call. = FALSE)
  fExplain <- function(x, y) {
    object <- fExplain_obj$clone()$clear()
    data <- data.frame(y = y, x)
    object$formula <- y~.
    object$fit(data)
    return(list(fitted.values = object$fitted()$fitted))
  }
  fEstimate <- function(x, y) {
    object <- object$clone()$clear()
    data <- data.frame(y = y, x)
    object$formula <- y~.
    object$fit(data)
    return(list(fitted.values = object$fitted()$fitted))
  }
  args[["fExplain"]] <- fExplain
  args[["fEstimate"]] <- fEstimate
  args[["data"]] <- object$data
  args <- args[c("formula", "data", "fExplain", "fEstimate", "method")]
  res <- do.call(partim::partim, args)
  result_df <- tibble(
    term = names(res),
    importance = res
  )
  return(result_df)
}
