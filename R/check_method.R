# Internal function to check valid usage of methods
.check_method <- function(
    method,
    what = c("exists", "cv", "has_predict_method", "has_importance_method", "uses_index",
             "regress", "classify", "multinomial", "nonstandard_formula", "has_coef_method"),
    message = FALSE
    ) {

  what <- match.arg(what)
  for (meth in method) {
    if (what == "exists") {
      chk <- meth %in% names(METHOD_REGISTER)
    } else {
      chk <- METHOD_REGISTER[[meth]]$check(what)
    }

    if (message & !chk)
      stop(sprintf(.checks[[what]]$message, meth))
    if (!message) return(chk)
  }

}

.checks <- list(
  exists = list(
    message = "'%s' is not a valid method. See '?m' for details"
  ),
  cv = list(
    message = "'%s' has no hyperparameters. Use '.force_cv = TRUE' to perform a cross validation"
  ),
  has_predict_method = list(
    message = "'%s' has no predict method."
  ),
  uses_index = list(
    message = "'%s' use no index columns. Parameters are estimated once for the entire sample"
  ),
  regress = list(
    message = "'%s' cannot be used for regression"
  ),
  classify = list(
    message = "'%s' cannot be used for classification"
  ),
  multinomial = list(
    message = "'%s' cannot be used for multinomial classification"
  ),
  nonstandard_formula = list(
    message = "'%s' uses non-standard formula syntax"
  ),
  has_importance_method = list(
    message = "'%s' has no variable importance method"
  ),
  has_coef_method = list(
    message = "'%s' has not coef method"
  )
)
