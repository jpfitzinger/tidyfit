# Internal function to check valid usage of methods
.check_method <- function(
    method,
    what = c("exists", "cv", "uses_index", "regress", "classify", "multinomial"),
    message = FALSE
    ) {

  what <- match.arg(what)
  for (meth in method) {
    chk <- meth %in% .checks[[what]]$methods
    if (message & !chk)
      stop(sprintf(.checks[[what]]$message, meth))
    if (!message) return(chk)
  }

}

.checks <- list(
  exists = list(
    methods = c("lm", "glm", "robust", "lasso", "ridge", "enet", "adalasso",
                "pcr", "plsr", "hfr", "boost", "subset", "cor", "bayes", "quantile",
                "glmm", "tvp", "mslm", "bma", "gets", "svm", "mrmr", "relief", "cor",
                "chisq", "rf"),
    message = "'%s' is not a valid method. See '?m' for details"
  ),
  cv = list(
    methods = c("lasso", "ridge", "enet", "adalasso", "pcr", "plsr", "hfr", "boost", "svm", "rf"),
    message = "'%s' has no hyperparameters. Use '.force_cv = TRUE' to perform a cross validation"
  ),
  uses_index = list(
    methods = c("glmm"),
    message = "'%s' use no index columns. Parameters are estimated once for the entire sample"
  ),
  regress = list(
    methods = c("lm", "glm", "robust", "lasso", "ridge", "enet", "adalasso",
                "pcr", "plsr", "hfr", "boost", "subset", "cor", "bayes", "quantile",
                "glmm", "tvp", "mslm", "bma", "gets", "svm", "mrmr", "relief", "cor", "rf"),
    message = "'%s' cannot be used for regression"
  ),
  classify = list(
    methods = c("glm", "lasso", "ridge", "enet", "adalasso", "boost", "subset", "bayes", "glmm", "svm", "mrmr", "relief", "chisq", "rf"),
    message = "'%s' cannot be used for classification"
  ),
  multinomial = list(
    methods = c("ridge", "lasso", "enet", "adalasso", "svm", "mrmr", "relief", "chisq", "rf"),
    message = "'%s' cannot be used for multinomial classification"
  )
)
