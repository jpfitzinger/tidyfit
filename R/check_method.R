.check_method <- function(method, what = c("exists", "cv", "regress", "classify")) {

  what <- match.arg(what)
  if (what == "exists") {
    if(!method %in% .valid_methods)
      stop(sprintf("'%s' is not a valid method. See '?m' for details", method))
  }
  if (what == "cv") {
    return(method %in% .valid_cv)
  }
  if (what == "regress") {
    if (!method %in% .valid_regress)
      stop(sprintf("'%s' cannot be used for regression", method))
  }
  if (what == "classify") {
    if (!method %in% .valid_classify)
      stop(sprintf("'%s' cannot be used for classification", method))
  }

}

.valid_methods <- c("lm", "glm", "lasso", "ridge", "enet", "adalasso", "pcr", "plsr", "hfr", "boost", "subset", "cor", "bayes")
.valid_cv <- c("lasso", "ridge", "enet", "adalasso", "pcr", "plsr", "hfr", "boost")
.valid_regress <- c("lm", "glm", "lasso", "ridge", "enet", "adalasso", "pcr", "plsr", "hfr", "boost", "subset", "cor", "bayes")
.valid_classify <- c("glm", "lasso", "ridge", "enet", "adalasso", "boost", "subset", "bayes")
