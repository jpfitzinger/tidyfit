#' @importFrom utils installed.packages

.check_package_name <- function(method) {
  return(.get_package_name(method) %in% utils::installed.packages())
}

.get_package_name <- function(method) {
  return(.package_names[[method]])
}

.package_names <- list(
  adalasso = "glmnet",
  anova = "stats",
  bayes = "arm",
  blasso = "monomvn",
  bma = "BMS",
  boost = "mboost",
  bridge = "monomvn",
  chisq = "stats",
  cor = "stats",
  enet = "glmnet",
  genetic = "gaselect",
  gets = "gets",
  glm = "stats",
  glmm = "lme4",
  hfr = "hfr",
  lasso = "glmnet",
  lm = "stats",
  mrmr = "mRMRe",
  mslm = "MSwM",
  nnet = "nnet",
  pcr = "pls",
  plsr = "pls",
  quantile_rf = "quantregForest",
  quantile = "quantreg",
  relief = "CORElearn",
  rf = "randomForest",
  ridge = "glmnet",
  robust = "MASS",
  spikeslab = "BoomSpikeSlab",
  subset = "bestglm",
  svm = "e1071",
  tvp = "shrinkTVP"
)
