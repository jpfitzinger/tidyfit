.check_explain_method <- function(object, package) {
  if(object$mode == "regression") {
    explain_methods <- regression_explain_methods
  } else {
    explain_methods <- classification_explain_methods
  }
  if (object$method %in% names(regression_explain_methods)) {
    available_packages <- regression_explain_methods[[object$method]]
    if (package %in% available_packages) {
      if (package %in% utils::installed.packages()) {
        return(NULL)
      } else {
        if (package == "partimp") {
          stop(sprintf("Package '%s' is necessary to explain '%s' models. Use devtools::install_github('jpfitzinger/partimp') to install it.", package, object$method, package))
        } else {
          stop(sprintf("Package '%s' is necessary to explain '%s' models. Use install.packages('%s') to install it.", package, object$method, package))
        }
      }
    } else {
      stop(sprintf("Package '%s' cannot be used with '%s' %s", package, object$method, object$mode))
    }
  } else {
    stop(sprintf("No explain methods available for '%s' %s", object$method, object$mode))
  }
}

.get_default_explain_method <- function(package, method) {
  valid_methods <- valid_explain_methods[[package]]
  if (is.null(method)) method <- valid_methods[1]
  if (!method %in% valid_methods)
    stop(sprintf("'%s' is not a valid explain method for '%s' package", method, package))
  return(method)
}

.get_default_explain_package <- function(object) {
  if (object$mode == "regression") {
    package = regression_explain_methods[[object$method]]
  } else {
    package = classification_explain_methods[[object$method]]
  }
  if (is.null(package))
    stop(sprintf("No explain methods available for '%s' %s", object$method, object$mode))
  return (package[1])
}

regression_explain_methods <- list(
  lm = c("sensitivity", "partim", "iml"),
  glm = c("sensitivity", "partim", "iml"),
  lasso = c("iml", "partim"),
  ridge = c("iml", "partim"),
  enet = c("iml", "partim"),
  adalasso = c("iml", "partim"),
  hfr = c("iml", "partim"),
  nnet = c("iml", "partim"),
  rf = c("randomForest"),
  quantile_rf = c("randomForest")
)
classification_explain_methods <- list(
  glm = c("sensitivity"),
  nnet = c("iml"),
  rf = c("randomForest"),
  quantile_rf = c("randomForest")
)
valid_explain_methods <- list(
  sensitivity = c("lmg", "pmvd", "johnson", "src", "pcc"),
  iml = c("Shapley", "LocalModel", "FeatureImp"),
  partim = c("tree_lmg", "tree_pmvd", "tree_entropy"),
  randomForest = c("mean_decrease_accuracy")
)
