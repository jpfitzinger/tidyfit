# Methods Register

METHOD <- R6::R6Class(
  "tidyFit.method",
  public = list(
    method = NULL,
    estimator = NULL,
    vector_args = NULL,
    tunable_vector_args = NULL,
    cv = NULL,
    regress = NULL,
    classify = NULL,
    has_predict_method = NULL,
    uses_index = NULL,
    multinomial = NULL,
    nonstandard_formula = NULL,
    has_importance_method = NULL,
    has_coef_method = NULL,

    initialize = function(method, estimator, regress, classify, multinomial,
                          cv, uses_index, nonstandard_formula,
                          vector_args, tunable_vector_args,
                          has_predict_method, has_coef_method, has_importance_method) {
      self$method <- method
      self$estimator <- estimator
      self$regress <- regress
      self$classify <- classify
      self$vector_args <- vector_args
      self$tunable_vector_args <- tunable_vector_args
      self$multinomial <- multinomial
      self$cv <- cv
      self$uses_index <- uses_index
      self$nonstandard_formula <- nonstandard_formula
      self$has_predict_method <- has_predict_method
      self$has_coef_method <- has_coef_method
      self$has_importance_method <- has_importance_method
    }
  )
)

METHOD_REGISTER <- list(
  adalasso = METHOD$new(
    method = "adalasso", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"), tunable_vector_args = c("lambda", "lower.limits", "upper.limits"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  anova = METHOD$new(
    method = "anova", estimator = "stats::anova", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "subset", "offset", "contrasts", "control"), tunable_vector_args = c("subset", "control"),
    has_predict_method = FALSE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  bayes = METHOD$new(
    method = "bayes", estimator = "arm::bayesglm", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "contrasts", "offset", "start", "etastart", "mustart", "control", "prior.mean", "prior.scale", "prior.df"),
    tunable_vector_args = c("subset", "start", "etastart", "mustart", "control", "prior.mean", "prior.scale", "prior.df"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  blasso = METHOD$new(
    method = "blasso", estimator = "monomvn::blasso", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("beta", "rd", "ab"),
    tunable_vector_args = c("beta", "rd", "ab"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  bma = METHOD$new(
    method = "bma", estimator = "BMS::bms", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("start.value"),
    tunable_vector_args = c("start.value"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  bridge = METHOD$new(
    method = "bridge", estimator = "monomvn::bridge", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("beta", "rd", "ab"),
    tunable_vector_args = c("beta", "rd", "ab"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  boost = METHOD$new(
    method = "boost", estimator = "mboost::glmboost", regress = TRUE, classify = TRUE,
    multinomial = FALSE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "contrasts.arg", "offset", "control", "oobweights"),
    tunable_vector_args = c("control"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = FALSE
  ),
  enet = METHOD$new(
    method = "enet", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"), tunable_vector_args = c("lambda", "lower.limits", "upper.limits"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  lasso = METHOD$new(
    method = "lasso", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"), tunable_vector_args = c("lambda", "lower.limits", "upper.limits"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  lm = METHOD$new(
    method = "lm", estimator = "stats::lm", regress = TRUE, classify = FALSE,
    multinomial = FALSE, cv = FALSE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("subset", "weights", "contrasts", "offset"), tunable_vector_args = c("subset"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  ),
  ridge = METHOD$new(
    method = "ridge", estimator = "glmnet::glmnet", regress = TRUE, classify = TRUE,
    multinomial = TRUE, cv = TRUE, uses_index = FALSE, nonstandard_formula = FALSE,
    vector_args = c("weights", "offset", "lambda", "exclude", "lower.limits", "upper.limits"), tunable_vector_args = c("lambda", "lower.limits", "upper.limits"),
    has_predict_method = TRUE, has_coef_method = TRUE, has_importance_method = TRUE
  )
)
