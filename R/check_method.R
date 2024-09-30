# Internal function to check valid usage of methods
.check_method <- function(
    method,
    what = c("exists", "cv", "has_predict_method", "has_importance_method", "uses_index",
             "regress", "classify", "multinomial", "nonstandard_formula", "has_coef_method"),
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
                "chisq", "rf", "bridge", "blasso", "spikeslab", "genetic",
                "quantile_rf", "anova", "nnet", "group_lasso"),
    message = "'%s' is not a valid method. See '?m' for details"
  ),
  cv = list(
    methods = c("lasso", "ridge", "enet", "adalasso", "pcr", "plsr", "hfr", "boost",
                "svm", "rf", "quantile_rf", "nnet", "group_lasso"),
    message = "'%s' has no hyperparameters. Use '.force_cv = TRUE' to perform a cross validation"
  ),
  has_predict_method = list(
    methods = c("lm", "glm", "robust", "lasso", "ridge", "enet", "adalasso",
                "pcr", "plsr", "hfr", "boost", "subset", "bayes", "quantile",
                "glmm", "tvp", "mslm", "bma", "gets", "svm",
                "rf", "bridge", "blasso", "spikeslab", "genetic",
                "quantile_rf", "nnet", "group_lasso"),
    message = "'%s' has no predict method."
  ),
  uses_index = list(
    methods = c("glmm"),
    message = "'%s' use no index columns. Parameters are estimated once for the entire sample"
  ),
  regress = list(
    methods = c("lm", "glm", "robust", "lasso", "ridge", "enet", "adalasso",
                "pcr", "plsr", "hfr", "boost", "subset", "cor", "bayes", "quantile",
                "glmm", "tvp", "mslm", "bma", "gets", "svm", "mrmr", "relief", "cor", "rf", "bridge", "blasso",
                "spikeslab", "genetic", "quantile_rf", "anova", "nnet", "group_lasso"),
    message = "'%s' cannot be used for regression"
  ),
  classify = list(
    methods = c("glm", "lasso", "ridge", "enet", "adalasso", "boost", "subset", "bayes", "glmm", "svm", "mrmr",
                "relief", "chisq", "rf", "spikeslab", "anova", "nnet", "group_lasso"),
    message = "'%s' cannot be used for classification"
  ),
  multinomial = list(
    methods = c("ridge", "lasso", "enet", "adalasso", "svm", "mrmr", "relief", "chisq", "rf", "nnet"),
    message = "'%s' cannot be used for multinomial classification"
  ),
  nonstandard_formula = list(
    methods = c("glmm"),
    message = "'%s' uses non-standard formula syntax"
  ),
  has_importance_method = list(
    methods = c("lm", "glm", "lasso", "ridge", "enet", "adalasso", "rf", "quantile_rf",
                "subset", "gets", "nnet", "hfr", "pcr", "plsr", "bayes"),
    message = "'%s' has no variable importance method"
  ),
  has_coef_method = list(
    methods = c("lm", "glm", "robust", "lasso", "ridge", "enet", "adalasso",
                "pcr", "plsr", "hfr", "boost", "subset", "cor", "bayes", "quantile",
                "glmm", "tvp", "mslm", "bma", "gets", "svm", "mrmr", "relief", "cor",
                "chisq", "bridge", "blasso", "spikeslab", "genetic", "anova", "group_lasso"),
    message = "'%s' has not coef method"
  )
)
